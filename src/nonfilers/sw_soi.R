# =============================================================================
# sw_soi.R  --  split out of state_weights.R (F4, 2026-09-23)
#
# IRS SOI readers: Historic Table 2 (state x AGI stub), Pub 1304 Tables 1.6 and
# 1.7, the IRA age split, the filing threshold, and (2026-09-27) the IRS-Ind
# aligned panels of the Pub 1304 by-size tables.
#
# Code moved verbatim; see state_weights.R for the module map.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(stringr)
})

# -----------------------------------------------------------------------------
# SOI HT2 AGI_STUB → [lower, upper) AGI bracket, in nominal dollars.
# 10-class scheme, verified from the data (2017–2023 all carry stubs 1..10, with
# stub 1 = "under $1": it holds negative AGI). A record's stub is found by
# lower ≤ agi < upper. Pre-2017 HT2 used a coarser scheme; extend here if needed.
# -----------------------------------------------------------------------------
ht2_stub_breaks <- function(year) {
  data.table(
    agi_stub = 1:10,
    lower = c(-Inf,       1,   10e3, 25e3, 50e3,  75e3, 100e3, 200e3, 500e3, 1e6),
    upper = c(   1,   10e3,   25e3, 50e3, 75e3, 100e3, 200e3, 500e3,   1e6, Inf))
}

# -----------------------------------------------------------------------------
# read_ht2(): read one HT2 state × AGI CSV into a tidy long table of filer targets.
# Amounts (A*) are in $thousands in the file and converted to dollars. Counts (N*)
# are numbers of returns. Comma-formatted numeric strings are parsed.
#
# Returns a data.table: state, agi_stub, variable, value  (state totals, stub 0,
# and the US row are dropped; only the 51 + PR + OA jurisdictions × stubs 1..K).
# -----------------------------------------------------------------------------
# SOI code -> target-series name. Counts: N1 total returns; MARS1/2/4
# single/joint/HoH returns; N2 is "number of exemptions" through TY2017 and
# "number of individuals" from TY2018 (TCJA; SOI kept the column name and
# repointed the concept -- both count people represented on returns, and
# calibration is within-year, so the relabel never crosses a target cell).
# Amounts (A*) and their return counts (N*) follow the plan §2.1 target set:
# AGI, wages, interest, dividends, capital gains, SALT income/sales tax,
# real-estate tax, mortgage interest, EITC. Itemized-deduction items (18425/
# 18500/19300) reflect post-TCJA itemizer collapse from 2018 -- within-year
# targets, so levels are comparable to same-year PUF Schedule A amounts.
HT2_TARGET_MAP <- c(
  N1     = "n_returns", MARS1 = "n_single", MARS2 = "n_joint",
  MARS4  = "n_hoh",     N2    = "n_indiv",
  A00100 = "agi_amt",        N00200 = "n_wages",      A00200 = "wages_amt",
  N00300 = "n_int",          A00300 = "int_amt",
  N00600 = "n_div",          A00600 = "div_amt",
  N01000 = "n_kg",           A01000 = "kg_amt",
  N18425 = "n_salt_inc",     A18425 = "salt_inc_amt",
  N18450 = "n_salt_sales",   A18450 = "salt_sales_amt",
  N18500 = "n_re_tax",       A18500 = "re_tax_amt",
  N19300 = "n_mort_int",     A19300 = "mort_int_amt",
  N59660 = "n_eitc",         A59660 = "eitc_amt")

read_ht2 <- function(path, year) {

  # Files in the IRS-Ind mirror are gzipped; quoted comma-formatted numbers
  # are handled by fread + the comma-stripping parse below
  d <- if (grepl("\\.gz$", path)) {
    fread(cmd = paste("zcat", shQuote(path)), colClasses = "character")
  } else {
    fread(path, colClasses = "character")
  }
  setnames(d, toupper(names(d)))

  parse_num <- function(x) as.numeric(str_replace_all(x, ",", ""))

  # Tolerate the pre-2014 AGI column name
  if (!("A00100" %in% names(d)) && "A001" %in% names(d)) {
    setnames(d, "A001", "A00100")
  }
  keep <- intersect(names(HT2_TARGET_MAP), names(d))
  keep_amounts <- keep[startsWith(keep, "A")]

  d[, STATE := toupper(trimws(STATE))]
  d[, AGI_STUB := as.integer(AGI_STUB)]
  d <- d[!(STATE %in% c("US","")) & AGI_STUB != 0]              # drop US total & stub-0 total

  d[, (keep) := lapply(.SD, parse_num), .SDcols = keep]

  long <- melt(d[, c("STATE","AGI_STUB", keep), with = FALSE],
               id.vars = c("STATE","AGI_STUB"),
               variable.name = "variable", value.name = "value")
  # amounts ($ thousands in HT2) -> dollars
  long[variable %in% keep_amounts, value := value * 1000]
  long[, variable := HT2_TARGET_MAP[as.character(variable)]]
  setnames(long, c("STATE","AGI_STUB"), c("state","agi_stub"))
  long[, year := year][]
}

# -----------------------------------------------------------------------------
# SOI "Adjusted Gross Income Percentiles by State": one row per state with, for
# the state's OWN top 1 / 5 / 10 / 25 / 50 / 75 percent of returns, the AGI
# cutoff to enter the group, the return count, aggregate AGI, eight income
# items (count and amount) and tax after credits. Universe: returns filed the
# next calendar year EXCLUDING dependents' returns and negative-AGI returns --
# so totals are smaller than HT2's and not additive with them. Groups are
# cumulative (top 5 includes top 1). Documented, with its year-to-year quirks,
# in IRS-Ind notes/percentile.md; the quirks handled here:
#   * amounts are $ thousands in 2013, 2015, 2018-2023 but WHOLE DOLLARS in
#     2014, 2016, 2017 (with ~6-digit scientific-notation truncation on large
#     cells); cutoffs are dollars in every year
#   * 2018 alone merges the IRA and pension blocks into `ira_pension`
#   * an OA (statefips 57) row appears in 2014, 2016, 2017 only
#   * statefips is zero-padded from 2018; read as character
# Returns long: state, top_pct (1, 5, 10, 25, 50, 75; 100 = the state total,
# cutoff NA), cutoff_agi (dollars), series, measure (count / amount), value
# (dollars for amounts), year.
# -----------------------------------------------------------------------------
PERCENTILE_GROUPS <- c("01" = 1, "05" = 5, "10" = 10, "25" = 25, "50" = 50, "75" = 75)
PERCENTILE_DOLLAR_YEARS <- c(2014L, 2016L, 2017L)   # amounts already in dollars

# keep_us = TRUE keeps the national row (state "US"), whose cutoffs are the
# national top groups' -- the level the state-percentile targets are scaled to.
read_state_percentile <- function(path, year, keep_us = FALSE) {
  d <- if (grepl("\\.gz$", path)) {
    fread(cmd = paste("zcat", shQuote(path)), colClasses = "character")
  } else {
    fread(path, colClasses = "character")
  }
  d[, state := toupper(trimws(state))]
  d <- d[!(state %in% c(if (!keep_us) "US", "OA", ""))]
  stopifnot(nrow(d) == 51 + keep_us)
  num <- function(x) as.numeric(x)              # handles "1.11477E+13"
  amt_scale <- if (year %in% PERCENTILE_DOLLAR_YEARS) 1 else 1000

  items <- unique(sub("^total_(.*)_num$", "\\1", grep("^total_.*_num$", names(d), value = TRUE)))
  out <- list()
  for (g in names(PERCENTILE_GROUPS)) {
    pct <- PERCENTILE_GROUPS[[g]]
    cut <- num(d[[paste0("agi_", g)]])
    rows <- list(
      data.table(state = d$state, series = "returns", measure = "count",  value = num(d[[paste0("top_", g)]])),
      data.table(state = d$state, series = "agi",     measure = "amount", value = num(d[[paste0("sum_agi_", g)]]) * amt_scale),
      data.table(state = d$state, series = "tax",     measure = "amount", value = num(d[[paste0("sum_tax_", g)]]) * amt_scale))
    for (it in items) rows <- c(rows, list(
      data.table(state = d$state, series = it, measure = "count",  value = num(d[[paste0("num_", it, "_", g)]])),
      data.table(state = d$state, series = it, measure = "amount", value = num(d[[paste0("sum_", it, "_", g)]]) * amt_scale)))
    out[[g]] <- rbindlist(rows)[, `:=`(top_pct = pct, cutoff_agi = rep(cut, length(rows)))]
  }
  tot <- list(
    data.table(state = d$state, series = "returns", measure = "count",  value = num(d$total)),
    data.table(state = d$state, series = "agi",     measure = "amount", value = num(d$total_agi) * amt_scale),
    data.table(state = d$state, series = "tax",     measure = "amount", value = num(d$total_tax) * amt_scale))
  for (it in items) tot <- c(tot, list(
    data.table(state = d$state, series = it, measure = "count",  value = num(d[[paste0("total_", it, "_num")]])),
    data.table(state = d$state, series = it, measure = "amount", value = num(d[[paste0("total_", it, "_amt")]]) * amt_scale)))
  out[["100"]] <- rbindlist(tot)[, `:=`(top_pct = 100, cutoff_agi = NA_real_)]
  long <- rbindlist(out)
  stopifnot(!anyNA(long$value))
  long[, year := year]
  setcolorder(long, c("state", "top_pct", "cutoff_agi", "series", "measure", "value", "year"))
  long[]
}

# Standard-deduction filing thresholds by year × filing status (nominal $).
filing_threshold <- function(year) {
  # single, joint, hoh — standard deduction (the dominant filing-requirement floor)
  tbl <- list(
    `2017` = c(single = 10400, joint = 20800, hoh = 13400),
    `2018` = c(single = 12000, joint = 24000, hoh = 18000),
    `2019` = c(single = 12200, joint = 24400, hoh = 18350),
    `2020` = c(single = 12400, joint = 24800, hoh = 18650),
    `2021` = c(single = 12550, joint = 25100, hoh = 18800),
    `2022` = c(single = 12950, joint = 25900, hoh = 19400),
    `2023` = c(single = 13850, joint = 27700, hoh = 20800))
  tbl[[as.character(year)]]
}

#' Pub 1304 Table 1.6 return counts by marital status, for one tax year.
#'
#' Lived in 09_asec_tax_unit_diagnostics.R and was duplicated as hardcoded
#' constants in 01_build_units.R (`SOI_RETURNS`, `SOI_HOH`, 2017 and 2022
#' only). Reading the published table instead means every build year works
#' without a new literal, which is what group D needs -- 2014-2016 to
#' calibrate on the Pub 5785 years, 2018 and 2021 to test the aging.
#'
#' The published "married filing jointly" block also carries surviving
#' spouses, and the four blocks partition all returns -- exactly from TY2015
#' on, but the TY2014 table is short by 75,255 (0.05%), which is the IRS's
#' arithmetic and is reported rather than corrected.
#'
#' @return list(returns = data.table(filing_status, soi_returns), total)
soi_filing_status <- function(year) {
  f <- file.path(raw_data_root(), "IRS-Ind/national/by_size",
                 sprintf("returns_marital_age_%d.xls", year))
  if (!file.exists(f)) {
    stop(sprintf("soi_filing_status(): no Pub 1304 T1.6 workbook for %d (%s)",
                 year, basename(f)), call. = FALSE)
  }
  x   <- suppressMessages(readxl::read_excel(f, sheet = 1, col_names = FALSE))
  lab <- str_squish(as.character(x[[1]]))
  n   <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x[[2]]))))
  pick <- function(pat) n[which(str_detect(lab, regex(pat, ignore_case = TRUE)))[1]]

  out <- data.table(
    filing_status = c("joint_and_qss", "married_separate",
                      "head_of_household", "single"),
    soi_returns   = c(pick("married persons filing jointly"),
                      pick("married persons filing separately"),
                      pick("heads of household"),
                      pick("single persons")))
  total <- pick("^All returns, total")
  stopifnot(!anyNA(out$soi_returns), !is.na(total))

  gap <- sum(out$soi_returns) - total
  if (abs(gap) > 1000) {
    message(sprintf(paste("  NOTE: Pub 1304 T1.6 TY%d marital blocks miss the",
                          "published total by %s returns (%.3f%%) -- a defect",
                          "in the source table, carried not corrected"),
                    year, format(gap, big.mark = ","), 100 * gap / total))
  }
  stopifnot(abs(gap) < 0.001 * total)
  list(returns = out, total = total)
}


# -----------------------------------------------------------------------------
# HT2 filing-status identities: convert return counts into counts of PEOPLE on
# filed returns, by state. One definition per computation -- the reconciliation
# diagnostic below, the residual non-filer anchors (research/state_weights/nonfiler_residual_design.md
# §3.1), and any external consumer all call this.
#
# Identities and documented approximations:
#   married filing adults = 2 * MARS2 + (N1 - MARS1 - MARS2 - MARS4)
#                           [residual is MFS-dominated; QSS (~1M unmarried
#                            returns) is misclassified as married]
#   single filing adults  = MARS1 + MARS4
#   dependents            = N2 - (N1 + MARS2)
#                           [includes adult dependents; a dependent who files
#                            their own return is double-counted (own return +
#                            parent's N2); N2 = exemptions pre-2018]
# PR/OA are excluded to match ACS/PEP coverage.
# -----------------------------------------------------------------------------
#' @param states jurisdictions to tabulate. Defaults to the 51 modeled ones.
#'        Pass NONTAX_BUCKETS to measure the out-of-state buckets (OA, and PR
#'        from 2018) that the default drops -- those returns ARE in Pub 1304's
#'        national totals, so the two universes differ by exactly this amount
#'        and 12_anchor_basis_comparison.R needs it named rather than implied.


ht2_filing_persons <- function(ht2, states = NULL) {
  ht2 <- as.data.table(ht2)
  if (is.null(states)) states <- setdiff(unique(ht2$state), NONTAX_BUCKETS)
  irs <- dcast(ht2[state %in% states &
                                  variable %in% c("n_returns","n_single","n_joint","n_hoh","n_indiv"),
                                  .(value = sum(value)), by = .(state, variable)],
               state ~ variable, value.var = "value")
  irs[, .(state,
          married_filing_adults = 2 * n_joint + (n_returns - n_single - n_joint - n_hoh),
          single_filing_adults  = n_single + n_hoh,
          dependents            = n_indiv - (n_returns + n_joint),
          # HT2 publishes no MFS series, so the status residual is MFS PLUS
          # qualifying surviving spouse -- 1 adult either way, which is why the
          # adult identity above is unaffected by the mix. Named here because a
          # filing-STATUS target is not: 12_anchor_basis_comparison.R nets the
          # nationally published MFS count off it to estimate QSS.
          mfs_qss_returns       = n_returns - n_single - n_joint - n_hoh)]
}

#' OASDI beneficiaries aged 65 or older, by state, December of `year`.
#'
#' The flat series is the source of record -- every year in one file, with a
#' labelled schema -- and the same year's workbook is read back as an
#' assertion, so the agreement documented in NOTES.md §2 stays enforced rather
#' than assumed. Universe is `beneficiary`: a person-level administrative
#' universe that is neither `resident` nor `household` (design memo §7.3).
#' SOI IRA study Table 4, column (1): Form 1040 filers by five-year age band.
#'
#' The ONLY published source that splits filers inside 65+, which Pub 1304
#' Table 1.6 cannot (its top band is "65 and over") and no state-level source
#' can either (SSA OASDI-SC publishes 65+ by sex only). So this supplies the
#' `65_74` / `75p` SHARE and never a level: its own 65+ total runs 4.5-5.7%
#' below T1.6's, consistently across years, because it assigns each taxpayer
#' THEIR OWN age where T1.6 assigns a joint return the PRIMARY's. Applying the
#' share to T1.6's level keeps T1.6's convention and borrows only the shape.
#'
#' @return list(share_65_74, share_75p, filers_65_74, filers_75p) -- shares of
#'         the 65+ total, summing to 1.
read_soi_ira_age_split <- function(year) {

  # SOI switched this workbook from .xls to .xlsx at TY2017; both are on disk
  # (the series runs 2000-2023), so try each rather than assuming the newer
  # one and failing on every pre-2017 year.
  dir <- file.path(raw_data_root(), "IRS-Ind/national/ira")
  cand <- file.path(dir, sprintf("ira_t04_%d.%s", year, c("xlsx", "xls")))
  f <- cand[file.exists(cand)][1]
  if (is.na(f))
    stop("SOI IRA study Table 4 not found for ", year, ": tried ",
         paste(basename(cand), collapse = " and "),
         if (year == 2003) "  (TY2003 is not published)" else "")

  raw <- readxl::read_excel(f, sheet = 1, col_names = FALSE,
                            .name_repair = "minimal")
  lab <- trimws(as.character(raw[[1]]))

  # Column (1) is "Number of taxpayers who filed Form 1040". Locate it by the
  # published column-number row rather than by position, so an edition that
  # inserts a column fails loudly instead of silently reading contributions.
  hdr <- which(vapply(seq_len(nrow(raw)),
                      function(i) identical(trimws(as.character(raw[i, 2])), "(1)"),
                      logical(1)))
  stopifnot(length(hdr) == 1)
  col1 <- suppressWarnings(as.numeric(raw[[2]]))

  # The five-year bands aggregate exactly onto our two: 65-70 + 70-75 = 65_74,
  # 75-80 + 80+ = 75p. Matched on the published row labels, asserted present.
  want <- c("65 under 70", "70 under 75", "75 under 80", "80 and over")
  idx  <- match(want, lab)
  if (anyNA(idx))
    stop("SOI IRA Table 4 ", year, ": missing age row(s) ",
         paste(want[is.na(idx)], collapse = ", "))
  v <- col1[idx]
  stopifnot(!anyNA(v), all(v > 0))

  f_65_74 <- v[1] + v[2]
  f_75p   <- v[3] + v[4]
  tot     <- f_65_74 + f_75p

  # Sanity: the two bands must be a real split of a plausible 65+ filer count.
  # 20-60M brackets every published year in the 2000-2023 series generously.
  stopifnot(tot > 20e6, tot < 60e6, f_65_74 > f_75p)

  list(share_65_74 = f_65_74 / tot, share_75p = f_75p / tot,
       filers_65_74 = f_65_74, filers_75p = f_75p)
}

# -----------------------------------------------------------------------------
# Pub 1304 national by-size tables (IRS-Ind/national/by_size).
#
# HT2 is state x AGI stub and carries NO age and NO married-filing-separately
# series, so the national quantities those would define come from the Pub 1304
# by-size tables instead. The division of labour is deliberate and is the rule
# for this pair of sources: Pub 1304 owns national LEVELS, HT2 owns state
# SHARES, and the two are never differenced (they are different estimands --
# Pub 1304 is a sample-based national estimate, HT2 is population-based
# administrative tabulation).
#
# Promoted out of nonfiler_residual/02_build_residual_anchors.R on 2026-08-27
# (one definition per computation) so that 12_anchor_basis_comparison.R reads
# the same national level the anchors are built on.
# -----------------------------------------------------------------------------
#---------------------------------------------------------------
# Pub 1304 Table 1.6: returns by filing status block x age band
#---------------------------------------------------------------

# Row-label -> (block | age band) parse of the published sheet. Blocks carry
# 1 or 2 adults per return; MFJ ages are the PRIMARY taxpayer's (documented
# approximation). "Under 26" rows in status blocks are treated as 18-25 --
# under-18 married/HoH filers are negligible; the single block's "Under 18"
# row (dependent-age filers) is excluded from the adult bands and reported.
read_pub1304_t16 <- function(year) {
  f <- file.path(raw_data_root(), 'IRS-Ind/national/by_size',
                 sprintf('returns_marital_age_%d.xls', year))
  x <- suppressMessages(read_excel(f, sheet = 1, col_names = FALSE))
  lab <- str_squish(as.character(x[[1]]))
  n   <- suppressWarnings(as.numeric(gsub('[^0-9.-]', '', as.character(x[[2]]))))

  block_map <- c('^All returns, total'                        = 'all',
                 'married persons filing jointly'             = 'mfj',
                 'married persons filing separately'          = 'mfs',
                 'heads of household'                         = 'hoh',
                 'surviving spouse'                           = 'qss',
                 'single persons'                             = 'single')
  age_map <- c('^Under 18$' = 'u18', '^Under 26$' = '18_25',
               '^18 under 26$' = '18_25', '^26 under 35$' = '26_34',
               '^35 under 45$' = '35_44', '^45 under 55$' = '45_54',
               '^55 under 65$' = '55_64', '^65 and over$'  = '65p')

  cur_block <- NA_character_
  out <- list()
  for (i in seq_along(lab)) {
    if (is.na(lab[i]) || lab[i] == '') next
    b <- block_map[str_detect(lab[i], regex(names(block_map), ignore_case = TRUE))]
    b <- b[!is.na(b)]
    if (length(b)) { cur_block <- b[1]; next }
    a <- age_map[str_detect(lab[i], regex(names(age_map), ignore_case = TRUE))]
    a <- a[!is.na(a)]
    if (length(a) && !is.na(cur_block) && !is.na(n[i])) {
      out[[length(out) + 1]] <- data.table(block = cur_block, band = a[1],
                                           n_returns = n[i])
    }
  }
  t16 <- rbindlist(out)
  # Duplicate (block, band) rows would mean the label parse broke on a vintage
  stopifnot(nrow(t16) == uniqueN(t16[, .(block, band)]))
  t16
}

read_pub1304_t17_total <- function(year) {
  f <- file.path(raw_data_root(), 'IRS-Ind/national/by_size',
                 sprintf('dependent_returns_%d.xls', year))
  x <- suppressMessages(read_excel(f, sheet = 1, col_names = FALSE))
  i <- which(str_squish(as.character(x[[1]])) == 'All returns')[1]
  as.numeric(gsub('[^0-9.-]', '', as.character(x[[2]][i])))
}

#---------------------------------------------------------------
# IRS-Ind aligned panels (added 2026-09-27, Phase 3b / S40)
#---------------------------------------------------------------

# The IRS-Ind store publishes the Pub 1304 by-size tables already parsed and
# checked (its align_bysize.R, harmonize_bysize.R and build_t16_targets.R;
# see IRS-Ind notes/national_bysize.md). Two layers are read here:
#   panel_{family}.csv  each table harmonized across years: `series` is the
#       consistent concept (Table 1.4's "wages" continues "Salaries and wages"
#       into TY2022's "Total wages"), agi_lo/agi_hi the class in dollars,
#       [lo, hi); the consistent classes are agi_change != 'expand_component'
#   targets_returns_marital_age.csv  Table 1.6 as targets, combined (**) cells
#       folded into blocks whose published sum is the target
# The files are regenerated in place, with no vintage, so pipeline code never
# reads them directly: a research script turns them into a committed file in
# resources/ (research/state_weights/filer_refit/01_refit_targets.R).
irs_ind_aligned_path <- function(file) {
  p <- file.path(raw_data_root(), 'IRS-Ind/aligned', file)
  if (!file.exists(p)) stop('IRS-Ind aligned file not found: ', p,
                            ' -- build it with the IRS-Ind scripts (README, "Usage")')
  p
}

# One harmonized series by AGI class: the consistent classes of `panel`, one
# row per (tax_year, agi_lo, agi_hi), plus the panel total as agi_lo = NA.
read_bysize_series <- function(family, series_name, measure_name,
                               panel_name = 'all returns', years) {
  p <- fread(irs_ind_aligned_path(sprintf('panel_%s.csv', family)), na.strings = '')
  d <- p[series == series_name & measure == measure_name & panel == panel_name &
           tax_year %in% years & !(agi_change %in% 'expand_component') &
           (is.na(row) | row == 'total')]
  if (!setequal(unique(d$tax_year), years)) {
    stop(family, ' / ', series_name, ': years missing: ',
         paste(setdiff(years, d$tax_year), collapse = ' '))
  }
  if (any(duplicated(d[, .(tax_year, agi_lo, agi_hi)]))) {
    stop(family, ' / ', series_name, ': more than one value per class')
  }
  d[, .(tax_year, agi_lo, agi_hi, value, flag, derived)]
}

# Table 1.6 target cells, one row per (tax_year, panel, age, AGI class), with
# block_id / block_value for combined cells. Tax-Data codes attached:
# filing_status (1 single, 2 joint incl. surviving spouses, 3 MFS, 4 HoH; NA
# for the all-returns panel) and age_group (1 = under 26, ..., 6 = 65+; the
# all-returns panel's 'under 18' and '18 under 26' both map to 1, as in
# project_puf's age1 < 26).
read_t16_targets <- function(years) {
  t <- fread(irs_ind_aligned_path('targets_returns_marital_age.csv'), na.strings = '')
  t <- t[tax_year %in% years]
  panel_status <- c('all returns' = NA_integer_,
                    'returns of single persons' = 1L,
                    'returns of married persons filing jointly and returns of surviving spouses' = 2L,
                    'returns of married persons filing jointly' = 2L,
                    'returns of surviving spouses' = 2L,
                    'returns of married persons filing separately' = 3L,
                    'returns of heads of households' = 4L)
  age_group <- c('under 18' = 1L, '18 under 26' = 1L, 'under 26' = 1L,
                 '26 under 35' = 2L, '35 under 45' = 3L, '45 under 55' = 4L,
                 '55 under 65' = 5L, '65 and over' = 6L)
  if (!all(t$panel %in% names(panel_status))) {
    stop('unmapped Table 1.6 panel: ', paste(setdiff(t$panel, names(panel_status)), collapse = '; '))
  }
  if (!all(t$age %in% names(age_group))) {
    stop('unmapped Table 1.6 age band: ', paste(setdiff(t$age, names(age_group)), collapse = '; '))
  }
  t[, `:=`(filing_status = unname(panel_status[panel]), age_group = unname(age_group[age]))]
  t[]
}
