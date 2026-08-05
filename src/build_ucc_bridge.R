##############################################################################
# build_ucc_bridge.R
#
# One-time script that parses the BLS CE PUMD hierarchical groupings (stubs)
# file and generates a UCC -> PCE bridge CSV.
#
# Input:  resources/cex_ucc/stubs/CE-HG-Integ-2023.txt
# Output: resources/ucc_pce_bridge.csv
##############################################################################

library(data.table)
library(stringr)

# ---- Paths ----------------------------------------------------------------
project_root <- here::here()
stubs_path   <- file.path(project_root,
                          "resources/cex_ucc/stubs/CE-HG-Integ-2023.txt")
output_path  <- file.path(project_root, "resources/ucc_pce_bridge.csv")

# Clean up erroneously downloaded file if it exists
bad_xlsx <- file.path(project_root, "resources/bls_ce_stubs.xlsx")
if (file.exists(bad_xlsx)) {
  message("Removing invalid file: ", bad_xlsx)
  file.remove(bad_xlsx)
}

# ---- Read stubs file -------------------------------------------------------
raw_lines <- readLines(stubs_path, warn = FALSE)
message(sprintf("Read %d lines from stubs file", length(raw_lines)))

# ---- Parse lines -----------------------------------------------------------
# Each line starts with "1" (data) or "2" (continuation).
# We'll build a list of parsed data lines, then handle continuations.

parsed <- vector("list", length(raw_lines))
n_parsed <- 0L

for (i in seq_along(raw_lines)) {
  line <- raw_lines[i]
  line_type <- substr(line, 1, 1)

  if (line_type == "1") {
    # Data line: extract indent, description, code, flag, section
    # Format: position 1 = "1", positions 4-5 = indent level (right-justified
    # in a 2-char field), position 7+ = description ... code flag num section

    indent <- as.integer(trimws(substr(line, 2, 5)))

    # Everything from position 7 onward
    rest <- substring(line, 7)

    # The tail of the line has: CODE FLAG NUM SECTION (each separated by
    # whitespace). We need to parse from the right. Split by whitespace from
    # the right.
    # Strategy: find the last few whitespace-delimited tokens
    tokens <- str_match(
      rest,
      "^(.*?)\\s{2,}(\\S+)\\s+(\\S)\\s+(\\S+)\\s+(\\S+)\\s*$"
    )

    if (!is.na(tokens[1, 1])) {
      description <- trimws(tokens[1, 2])
      code        <- tokens[1, 3]
      flag        <- tokens[1, 4]
      # tokens[1,5] is a number (weight multiplier), tokens[1,6] is section
      section     <- tokens[1, 6]
    } else {
      # Fallback: try to parse with less strict pattern
      description <- trimws(rest)
      code        <- NA_character_
      flag        <- NA_character_
      section     <- NA_character_
    }

    n_parsed <- n_parsed + 1L
    parsed[[n_parsed]] <- list(
      line_num    = i,
      indent      = indent,
      description = description,
      code        = code,
      flag        = flag,
      section     = section,
      line_type   = "1"
    )

  } else if (line_type == "2") {
    # Continuation line: append text to prior data line's description
    if (n_parsed > 0L) {
      cont_text <- trimws(substring(line, 2))
      parsed[[n_parsed]]$description <- paste(
        parsed[[n_parsed]]$description, cont_text
      )
    }
  }
}

parsed <- parsed[seq_len(n_parsed)]
message(sprintf("Parsed %d data lines", n_parsed))

# Convert to data.table
dt <- rbindlist(lapply(parsed, as.data.table))

# ---- Identify UCCs (leaf-level expenditure items) ---------------------------
# UCCs are lines where:
#   1. code is a 6-digit number
#   2. flag is D (Diary) or I (Interview)
#   3. They appear in the EXPEND or FOOD section (under TOTALEXP)
#
# We only want the EXPEND section, which starts at "Average annual
# expenditures" (TOTALEXP).

# Find the TOTALEXP row and the next level-1 header after it
totalexp_idx <- which(dt$code == "TOTALEXP")
if (length(totalexp_idx) == 0) stop("Cannot find TOTALEXP in stubs file")
totalexp_idx <- totalexp_idx[1]

# Find where the expenditure section ends (next indent==1 row after TOTALEXP)
level1_rows <- which(dt$indent == 1)
next_section <- level1_rows[level1_rows > totalexp_idx]
if (length(next_section) > 0) {
  expend_end <- next_section[1] - 1L
} else {
  expend_end <- nrow(dt)
}

# Subset to expenditure section
exp_dt <- dt[totalexp_idx:expend_end]
message(sprintf("Expenditure section: rows %d-%d (%d lines)",
                totalexp_idx, expend_end, nrow(exp_dt)))

# ---- Build hierarchy for each row ------------------------------------------
# Track ancestor codes at each indent level using a named stack.
# For each row, record the ancestor code at each level up to its parent.

hierarchy <- vector("list", nrow(exp_dt))
ancestor_stack <- list()  # list of (indent, code) pairs

for (i in seq_len(nrow(exp_dt))) {
  row <- exp_dt[i]
  ind <- row$indent

  # Pop any stack entries at or above current indent level
  ancestor_stack <- ancestor_stack[
    sapply(ancestor_stack, function(x) x$indent) < ind
  ]

  # Record hierarchy for this row
  hierarchy[[i]] <- list(
    ancestors = ancestor_stack,
    indent    = ind,
    code      = row$code
  )

  # Push this row onto the stack (it becomes an ancestor for deeper rows)
  ancestor_stack <- c(ancestor_stack, list(list(indent = ind, code = row$code)))
}

# ---- Extract UCC rows with their ancestors ----------------------------------
is_ucc <- grepl("^\\d{6}$", exp_dt$code) & exp_dt$flag %in% c("D", "I")
ucc_indices <- which(is_ucc)
message(sprintf("Found %d UCC items in expenditure section", length(ucc_indices)))

ucc_list <- vector("list", length(ucc_indices))
for (j in seq_along(ucc_indices)) {
  idx <- ucc_indices[j]
  row <- exp_dt[idx]
  h   <- hierarchy[[idx]]

  # Extract ancestor codes at each level
  anc_codes <- sapply(h$ancestors, function(x) x$code)
  anc_levels <- sapply(h$ancestors, function(x) x$indent)

  # Level 2 ancestor = major category (FOODTOTL, HOUSING, etc.)
  level2 <- if (any(anc_levels == 2)) anc_codes[anc_levels == 2][1] else NA
  level3 <- if (any(anc_levels == 3)) anc_codes[anc_levels == 3][1] else NA
  level4 <- if (any(anc_levels == 4)) anc_codes[anc_levels == 4][1] else NA

  ucc_list[[j]] <- data.table(
    ucc         = row$code,
    description = row$description,
    survey      = row$flag,
    level2      = level2,
    level3      = level3,
    level4      = level4,
    all_ancestors = paste(anc_codes, collapse = "/")
  )
}

uccs <- rbindlist(ucc_list)

# ---- Apply PCE mapping rules -----------------------------------------------
# Initialize
uccs[, pce_major := NA_character_]
uccs[, notes := ""]

# Helper: check if an ancestor code appears anywhere in the ancestor chain
has_ancestor <- function(dt, anc_code) {
  grepl(anc_code, dt$all_ancestors, fixed = TRUE)
}

# --- Food (FOODTOTL) ---
uccs[level2 == "FOODTOTL" & has_ancestor(uccs, "FOODHOME"),
     pce_major := "food_off_premises"]
uccs[level2 == "FOODTOTL" & has_ancestor(uccs, "FOODAWAY"),
     pce_major := "food_accommodations"]

# --- Alcoholic beverages (ALCBEVG) ---
uccs[level2 == "ALCBEVG", pce_major := "food_off_premises"]

# --- Housing (HOUSING) ---
# Shelter > Owned dwellings
uccs[level2 == "HOUSING" & level3 == "SHELTER" &
       has_ancestor(uccs, "OWNDWELL"),
     pce_major := "housing"]
# Shelter > Rented dwellings
uccs[level2 == "HOUSING" & level3 == "SHELTER" &
       has_ancestor(uccs, "RNTDWELL"),
     pce_major := "housing"]
# Shelter > Other lodging
uccs[level2 == "HOUSING" & level3 == "SHELTER" &
       has_ancestor(uccs, "OTHLODGE"),
     pce_major := "food_accommodations"]

# Utilities > Telephone services
uccs[level2 == "HOUSING" & level3 == "UTILS" &
       has_ancestor(uccs, "PHONE"),
     pce_major := "communication"]
# Utilities > Everything else (natural gas, electricity, fuel oil, water, etc.)
uccs[level2 == "HOUSING" & level3 == "UTILS" & is.na(pce_major),
     pce_major := "utilities"]

# Household operations
# Special case: 690114 (Computer information services/internet)
uccs[level2 == "HOUSING" & level3 == "HHOPER" & ucc == "690114",
     `:=`(pce_major = "communication",
          notes = "PCE: classified as communication")]
# Everything else in HHOPER
uccs[level2 == "HOUSING" & level3 == "HHOPER" & is.na(pce_major),
     pce_major := "other_services"]

# Housekeeping supplies
uccs[level2 == "HOUSING" & level3 == "HKPGSUPP",
     pce_major := "other_nondurables"]

# Household furnishings and equipment
uccs[level2 == "HOUSING" & level3 == "HHFURNSH",
     pce_major := "furnishings"]

# --- Apparel and services (APPAREL) ---
uccs[level2 == "APPAREL", pce_major := "clothing"]
# Exceptions: Watches and Jewelry → other_durables
uccs[level2 == "APPAREL" & ucc == "430110",
     `:=`(pce_major = "other_durables",
          notes = "PCE: classified as other durables, not apparel")]
uccs[level2 == "APPAREL" & ucc == "430120",
     `:=`(pce_major = "other_durables",
          notes = "PCE: classified as other durables, not apparel")]

# --- Transportation (TRANS) ---
# Vehicle purchases
uccs[level2 == "TRANS" & has_ancestor(uccs, "VEHPURCH"),
     pce_major := "motor_vehicles"]
# Gasoline and other fuels (GASOIL or GASFUEL)
uccs[level2 == "TRANS" &
       (has_ancestor(uccs, "GASOIL") | has_ancestor(uccs, "GASFUEL")),
     pce_major := "gasoline"]
# Other vehicle expenses
#   Vehicle finance charges
uccs[level2 == "TRANS" & has_ancestor(uccs, "VEHFINCH"),
     pce_major := "financial_insurance"]
#   Maintenance and repairs
uccs[level2 == "TRANS" & has_ancestor(uccs, "CAREPAIR"),
     pce_major := "transport_services"]
#   Vehicle rental, leases, licenses
uccs[level2 == "TRANS" & has_ancestor(uccs, "VEHRNTLC"),
     pce_major := "transport_services"]
#   Vehicle insurance (500110)
uccs[level2 == "TRANS" & ucc == "500110",
     `:=`(pce_major = "financial_insurance",
          notes = "PCE: net motor vehicle insurance")]
# Public and other transportation
uccs[level2 == "TRANS" & has_ancestor(uccs, "PUBTRANS"),
     pce_major := "transport_services"]

# --- Healthcare (HEALTH) ---
# Health insurance
uccs[level2 == "HEALTH" & has_ancestor(uccs, "HLTHINSR"),
     `:=`(pce_major = "financial_insurance",
          notes = "PCE: net health insurance -> financial services")]
# Medical services
uccs[level2 == "HEALTH" & has_ancestor(uccs, "MEDSERVS"),
     pce_major := "health_care"]
# Drugs
uccs[level2 == "HEALTH" & has_ancestor(uccs, "DRUGS"),
     `:=`(pce_major = "other_nondurables",
          notes = "PCE: classified as goods, not health care")]
# Medical supplies
uccs[level2 == "HEALTH" & has_ancestor(uccs, "MEDSUPPL"),
     `:=`(pce_major = "other_durables",
          notes = "PCE: classified as durables, not health care")]

# --- Entertainment (ENTRTAIN) ---
# Fees and admissions
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "FEESADM"),
     pce_major := "rec_services"]

# Audio and visual equipment and services (TVAUDIO)
# Cable/satellite TV → communication
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "TVAUDIO") &
       ucc %in% c("270310", "270311"),
     `:=`(pce_major = "communication",
          notes = "PCE: classified as communication")]
# Streaming services → rec_services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "TVAUDIO") &
       ucc %in% c("310243", "310350", "620930"),
     pce_major := "rec_services"]
# Repair and rental of equipment → rec_services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "TVAUDIO") &
       ucc %in% c("340610", "340902", "340905", "620904",
                   "620917", "620918", "690320", "690330",
                   "690340", "690350"),
     pce_major := "rec_services"]
# Rental and repair of musical instruments → rec_services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "TVAUDIO") &
       ucc == "620904",
     pce_major := "rec_services"]
# Musical instruments → rec_goods
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "TVAUDIO") &
       ucc == "610130",
     pce_major := "rec_goods"]
# All remaining TVAUDIO items (hardware: TVs, stereos, video players,
# game consoles, video media, apps, CDs, etc.) → rec_goods
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "TVAUDIO") &
       is.na(pce_major),
     pce_major := "rec_goods"]

# Pets, toys, hobbies, and playground equipment
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "PETSPLAY"),
     pce_major := "other_durables"]

# Other entertainment supplies, equipment, and services (ENTEROTH)
# Motorized and non-motorized recreational vehicles → rec_goods (durable goods)
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       (has_ancestor(uccs, "UNMTRBOT") | has_ancestor(uccs, "PWRSPVEH")),
     pce_major := "rec_goods"]
# Outboard motors → rec_goods
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       ucc == "600110",
     pce_major := "rec_goods"]
# Sports, recreation, and exercise equipment → rec_goods
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "RECEQUIP") &
       is.na(pce_major),
     pce_major := "rec_goods"]
# Rental of recreational vehicles → rec_services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "RNTSPVEH"),
     pce_major := "rec_services"]
# Docking and landing fees → rec_services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       ucc == "520901",
     pce_major := "rec_services"]
# Photographic equipment and supplies → mostly goods, services items are
# services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "PHOTOEQ") &
       ucc %in% c("620330", "620905", "620320"),
     pce_major := "rec_services"]
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "PHOTOEQ") &
       is.na(pce_major),
     pce_major := "rec_goods"]
# Fireworks, souvenirs, visual goods → rec_goods
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       ucc %in% c("610901", "610902", "610903"),
     pce_major := "rec_goods"]
# Pinball and electronic video games → rec_services (arcade admissions)
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       ucc == "620913",
     pce_major := "rec_services"]
# Live entertainment for catered affairs → rec_services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       ucc == "680310",
     pce_major := "rec_services"]
# Rental of party supplies for catered affairs → rec_services
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       ucc == "680320",
     pce_major := "rec_services"]
# Catch-all for remaining ENTEROTH: repair/rental of sports equipment
uccs[level2 == "ENTRTAIN" & has_ancestor(uccs, "ENTEROTH") &
       ucc == "620908",
     pce_major := "rec_services"]

# Catch-all: any remaining entertainment items
uccs[level2 == "ENTRTAIN" & is.na(pce_major),
     pce_major := "rec_goods"]

# --- Personal care (PERSCARE) ---
uccs[level2 == "PERSCARE" & has_ancestor(uccs, "PERSPROD"),
     pce_major := "other_nondurables"]
uccs[level2 == "PERSCARE" & has_ancestor(uccs, "PERSSERV"),
     pce_major := "other_services"]

# --- Reading (READING) ---
uccs[level2 == "READING", pce_major := "other_nondurables"]

# --- Education (EDUCATN) ---
uccs[level2 == "EDUCATN", pce_major := "education"]

# --- Tobacco (TOBACCO) ---
uccs[level2 == "TOBACCO", pce_major := "other_nondurables"]

# --- Miscellaneous (MISC) ---
uccs[level2 == "MISC", pce_major := "other_services"]

# --- Cash contributions (CASHCONT) ---
uccs[level2 == "CASHCONT",
     `:=`(pce_major = "exclude",
          notes = "Not consumption")]

# --- Personal insurance and pensions (INSPENSN) ---
uccs[level2 == "INSPENSN" & has_ancestor(uccs, "LIFEINSR"),
     `:=`(pce_major = "financial_insurance",
          notes = "PCE: net life insurance cost")]
uccs[level2 == "INSPENSN" & has_ancestor(uccs, "PENSIONS"),
     `:=`(pce_major = "exclude",
          notes = "Savings/transfers, not consumption")]

# ---- Check for unmapped UCCs -----------------------------------------------
unmapped <- uccs[is.na(pce_major)]
if (nrow(unmapped) > 0) {
  message("\nWARNING: ", nrow(unmapped), " UCCs could not be mapped:")
  print(unmapped[, .(ucc, description, level2, level3, all_ancestors)])
} else {
  message("\nAll UCCs successfully mapped.")
}

# ---- Prepare output ---------------------------------------------------------
bridge <- uccs[, .(ucc, ucc_description = description, pce_major, survey, notes)]

# Sort by UCC
setorder(bridge, ucc)

# ---- Write CSV --------------------------------------------------------------
fwrite(bridge, output_path)
message(sprintf("\nWrote %d rows to %s", nrow(bridge), output_path))

# ---- Summary ----------------------------------------------------------------
message("\n=== PCE Category Summary ===")
summary_dt <- bridge[, .N, by = pce_major][order(-N)]
for (i in seq_len(nrow(summary_dt))) {
  message(sprintf("  %-25s %3d UCCs", summary_dt$pce_major[i], summary_dt$N[i]))
}
message(sprintf("  %-25s %3d UCCs", "TOTAL", nrow(bridge)))
