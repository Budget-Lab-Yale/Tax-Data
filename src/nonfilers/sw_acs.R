# =============================================================================
# sw_acs.R  --  split out of state_weights.R (F4, 2026-09-23)
#
# ACS readers and the state x age x income population margins that the
# # non-filer partition targets, plus the ACS-vs-IRS comparisons.
#
# Code moved verbatim; see state_weights.R for the module map.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(stringr)
})

#' Read the needed variables from a shared IPUMS USA fixed-width extract
#' (us{year}a/: usa_{year}a.dat.gz + variables.csv giving column positions).
#' Applies implied-decimal scaling (PERWT carries 2) and recodes the IPUMS
#' INCTOT N/A sentinel (9999999, the 7-digit max) to NA -- without which every
#' child inflates unit income by ~$10M and everyone looks like a filer.
read_acs_extract <- function(acs_year,
                             cols = c("YEAR","STATEFIP","PERWT","PERNUM","SERIAL","SAMPLE",
                                      "AGE","MARST","SPLOC","MOMLOC","POPLOC","INCTOT",
                                      "GQ","SCHOOL")) {

  dir <- acs_extract_dir(acs_year)
  v   <- fread(file.path(dir, "variables.csv"))
  v   <- v[var_name %in% cols]
  stopifnot(all(cols %in% v$var_name))

  a <- readr::read_fwf(
    file      = file.path(dir, sprintf("usa_%da.dat.gz", acs_year)),
    col_positions = readr::fwf_positions(start = v$start, end = v$end,
                                         col_names = v$var_name),
    col_types = paste(rep("d", nrow(v)), collapse = ""),
    progress  = FALSE
  ) |> as.data.table()

  # Implied decimals
  for (i in which(v$imp_decim > 0)) {
    nm <- v$var_name[i]
    a[, (nm) := get(nm) / 10^v$imp_decim[i]]
  }
  # IPUMS N/A sentinel for INCTOT
  if ("INCTOT" %in% cols) {
    a[INCTOT >= 9999998, INCTOT := NA_real_]
  }
  a[]
}

#' Build ACS non-filer margins + state population totals from an IPUMS extract.
#' @param acs data.table from read_acs_extract() (or any source with the same
#'        columns); `year` is the TAX year the margins will target
#' @param gq_treatment "differentiated" (D4, the production rule -- see the GQ
#'        block below) or "v0" for the pre-B1 non-treatment, kept ONLY so the
#'        Stage D diagnostic can still reproduce its frozen v0 row.
#' @return list(nonfiler_margins = dt[state, age_band, income_tier, n_units, n_adults],
#'              filer_units      = dt[state, n_units],
#'              state_pop        = dt[state, pop],
#'              gq_composition   = dt[state, gq_type, band, persons],
#'              gq_reclassified  = dt[state, persons])  all weighted to population.
build_acs_margins <- function(acs, year, acs_year = NULL,
                              gq_treatment = c("differentiated", "v0")) {

  gq_treatment <- match.arg(gq_treatment)
  a <- as.data.table(acs)
  # De-pool: the extract may stack multiple ACS 1-year samples (summing PERWT across
  # them double-counts population). Keep exactly one survey YEAR.
  yrs <- sort(unique(a$YEAR))
  if (is.null(acs_year)) {
    acs_year <- max(yrs)
    if (length(yrs) > 1)
      message("  ACS extract pools years ", paste(yrs, collapse=","),
              "; using YEAR=", acs_year, " (pass acs_year= to override)")
  }
  a <- a[YEAR == acs_year]
  a[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  a <- a[!is.na(state)]                                       # drop PR/other non-mapped
  a[, hh_id := paste(SAMPLE, SERIAL, sep = "-")]

  # --- state population, BEFORE any group-quarters reclassification ---------
  # Load-bearing ordering. The anchors are PEP RESIDENT adults with no group-
  # quarters subtraction (design memo §3.0), so the population margin must count
  # every resident regardless of how the unit builder treats them. Reclassifying
  # a dorm student changes who heads a tax unit; it must not change how many
  # people live in the state.
  state_pop <- a[, .(pop = sum(PERWT)), by = state][order(state)]

  # --- differentiated group quarters (D4 / task B1) --------------------------
  # Three types, three treatments:
  #   institutional (GQ==3) -- retained as own-state non-filer units unless
  #     income makes them filers. No code needed: they carry no family pointers,
  #     so they head their own unit and the income test below decides. Verified
  #     rather than assumed (the role check after unit construction).
  #   dorm students (GQ==4, in school, 18-24) -- NOT unit heads. They are
  #     dependents on a parent's return, generally in another state, and HT2
  #     already counts them in N2, so a dorm student heading an income-less
  #     non-filer unit in the college's state is double-counted mass in the
  #     wrong place.
  #   other GQ (GQ==4, everyone else: military barracks, shelters, group homes)
  #     -- left to the income test, which classifies most barracks residents as
  #     filers because they have wages.
  #
  # SCOPE BOUNDARY (2026-08-23): this stops at the removal. The ACS has no
  # cross-household parent link, so a reclassified student cannot be placed in
  # the parent's state, and it is not yet decided that they should be -- the PEP
  # anchor puts them in the institution state while the HT2 dependents identity
  # puts the slot in the parent's state, so §3.0's universe invariant breaks
  # whichever way a reallocation goes. notes/student_cross_state_linkage.md owns
  # that question. Do not widen this block to chase it; report the mass instead.
  gq_composition <- gq_reclassified <- NULL
  if (gq_treatment == "differentiated") {

    miss <- setdiff(c("GQ", "SCHOOL"), names(a))
    if (length(miss))
      stop("build_acs_margins(gq_treatment = 'differentiated') needs ",
           paste(miss, collapse = ", "),
           " -- add them to read_acs_extract()'s `cols`")
    unknown <- setdiff(unique(a$GQ), c(GQ_HOUSEHOLD, GQ_INSTITUTIONAL, GQ_OTHER))
    if (length(unknown))
      stop("unexpected IPUMS GQ code(s): ", paste(sort(unknown), collapse = ", "),
           " -- classify_gq() covers 1/2/5 (household), 3, 4")

    a[, gq_type := classify_gq(GQ, SCHOOL, AGE)]

    # Composition report, by state x type x age band, on the FULL frame. Band
    # convention is the anchors' (a16_band), with under-18 called out, so this
    # is directly comparable to the Stage D T7 table.
    gq_composition <- a[gq_type != "household",
                        .(persons = sum(PERWT)),
                        by = .(state, gq_type,
                               band = fifelse(AGE < 18, "u18",
                                              as.character(a16_band(AGE))))
                       ][order(state, gq_type, band)]

    # A dorm student whose OWN income clears the single filing threshold is left
    # alone: the income test then makes them a filer. Same "unless income makes
    # them filers" rule the institutional population gets, and it keeps the
    # support test honest -- someone earning above the standard deduction is not
    # a plausible dependent. INCTOT NA is read as zero, matching the unit-income
    # convention below (`na.rm = TRUE`).
    thr_single <- filing_threshold(year)[["single"]]
    a[, own_inc := fifelse(is.na(INCTOT), 0, pmax(INCTOT, 0))]
    a[, dorm_dependent := gq_type == "dorm_student" & own_inc < thr_single &
                          MOMLOC == 0 & POPLOC == 0 & SPLOC == 0]

    # IPUMS zeroes family pointers inside group quarters, so the pointer clause
    # above should never bind. Report it rather than trust it; a nonzero count
    # means the extract's pointer convention changed and these records fall
    # through to the ordinary unit logic.
    n_ptr <- a[gq_type == "dorm_student" &
               (MOMLOC > 0 | POPLOC > 0 | SPLOC > 0), .N]
    if (n_ptr > 0)
      message("  ", n_ptr, " dorm-student records carry a family pointer; ",
              "left to the ordinary unit logic")

    gq_reclassified <- a[dorm_dependent == TRUE,
                         .(persons = sum(PERWT)), by = state][order(state)]
    message(sprintf(paste("  GQ persons %.2fM (institutional %.2fM, dorm students",
                          "%.2fM, other %.2fM); %.2fM students reclassified as",
                          "dependents"),
                    gq_composition[, sum(persons)] / 1e6,
                    gq_composition[gq_type == "institutional", sum(persons)] / 1e6,
                    gq_composition[gq_type == "dorm_student", sum(persons)] / 1e6,
                    gq_composition[gq_type == "other_noninstitutional",
                                   sum(persons)] / 1e6,
                    sum(gq_reclassified$persons) / 1e6))

    # Nobody points at a reclassified student (pointers are zeroed in GQ and a
    # household child is a different SERIAL), so dropping them here orphans no
    # dependent.
    a <- a[dorm_dependent == FALSE]
  }

  # --- v0 tax units from pointers -------------------------------------------
  a[, married_present := MARST == 1 & SPLOC > 0]
  a[, couple_head := fifelse(married_present, pmin(PERNUM, SPLOC), NA_integer_)]
  a[, parent_target := fifelse(MOMLOC > 0, MOMLOC, fifelse(POPLOC > 0, POPLOC, NA_integer_))]
  a[, is_child := AGE < 19 & !married_present & !is.na(parent_target)]
  a[, self_head := fifelse(married_present, couple_head, PERNUM)]

  # map a child to its parent's self-head; else self
  heads <- a[, .(hh_id, PERNUM, self_head)]
  a <- merge(a, heads[, .(hh_id, parent_target = PERNUM, parent_head = self_head)],
             by = c("hh_id","parent_target"), all.x = TRUE, sort = FALSE)
  a[, head_pernum := fifelse(is_child & !is.na(parent_head), parent_head, self_head)]
  a[, tu_id := paste(hh_id, head_pernum, sep = "-")]
  a[, role := fifelse(head_pernum == PERNUM, "head",
              fifelse(married_present & SPLOC == head_pernum, "spouse", "dependent"))]

  # The institutional treatment is "retain as own-state units": check it rather
  # than assume it. Anything other than "head" here means group quarters carry
  # family pointers after all, and the income test is being applied to a pooled
  # unit instead of the resident.
  if (gq_treatment == "differentiated") {
    n_inst_nonhead <- a[gq_type == "institutional" & role != "head", .N]
    if (n_inst_nonhead > 0)
      message("  ", n_inst_nonhead, " institutional records are not unit heads ",
              "(family pointers inside group quarters)")
  }

  # --- unit-level: income, filing status, filer flag ------------------------
  units <- a[, .(
    state       = first(state),
    weight      = PERWT[role == "head"][1],            # head's person weight = unit weight
    head_age    = AGE[role == "head"][1],
    has_spouse  = any(role == "spouse"),
    n_dep       = sum(role == "dependent"),
    gross_inc   = sum(pmax(INCTOT, 0), na.rm = TRUE)   # unit gross income (loss-floored)
  ), by = tu_id]

  # Drop malformed units (no resolvable head row -> NA weight/age; ~331 of
  # 198M units in the 2022 extract, from edge cases in the pointer logic)
  n_bad <- units[is.na(weight) | is.na(head_age), .N]
  if (n_bad > 0) {
    message("  dropping ", n_bad, " units with unresolved head (NA weight/age)")
    units <- units[!is.na(weight) & !is.na(head_age)]
  }

  units[, filing_status := fifelse(has_spouse, "joint",
                           fifelse(n_dep > 0, "hoh", "single"))]
  thr <- filing_threshold(year)
  units[, threshold := thr[filing_status]]
  units[, is_filer := gross_inc >= threshold]

  units[, `:=`(age_band = age_band(head_age), income_tier = income_tier(gross_inc))]

  # Both denominations are emitted. `n_units` counts tax units (one per head);
  # `n_adults` counts ADULTS, matching the residual anchors and the PUF-side
  # `n_adults` x-vector (D5). The ACS adult concept is 1 + has_spouse, which is
  # the same rule as the PUF's 1 + (filing_status == 2) -- a joint unit is two
  # adults on both sides. Keep them side by side rather than replacing: the
  # unit count is what places a RECORD (a record is a unit), the adult count is
  # what the target constrains.
  nonfiler_margins <- units[is_filer == FALSE,
                            .(n_units  = sum(weight),
                              n_adults = sum(weight * (1 + has_spouse))),
                            by = .(state, age_band, income_tier)][order(state, age_band, income_tier)]
  filer_units <- units[is_filer == TRUE,
                       .(n_units = sum(weight)), by = state][order(state)]
  # state_pop was computed above, before the GQ reclassification, deliberately.

  list(nonfiler_margins = nonfiler_margins[, year := year],
       filer_units      = filer_units[, year := year],
       state_pop        = state_pop[, year := year],
       unit_summary     = units[, .(n_units = sum(weight, na.rm = TRUE),
                                     n_filers = sum(weight * is_filer, na.rm = TRUE),
                                     n_nonfilers = sum(weight * !is_filer, na.rm = TRUE),
                                     n_units_na_wt = sum(is.na(weight)))],
       gq_composition   = if (is.null(gq_composition)) NULL
                          else gq_composition[, year := year],
       gq_reclassified  = if (is.null(gq_reclassified)) NULL
                          else gq_reclassified[, year := year],
       gq_treatment     = gq_treatment,
       acs_year         = acs_year)
}

# -----------------------------------------------------------------------------
# Individual-level IRS vs ACS comparison (reconciliation diagnostic).
#
# Compares weighted counts of PEOPLE -- single adults, married adults, and
# children/dependents -- between HT2 (who appears on filed returns) and the
# ACS (everyone), by state. Unlike the filing-unit comparison, this does not
# depend on the v0 ACS tax-unit/filing model at all: the ACS side is a direct
# person-level tabulation, and the difference ACS - IRS estimates the
# non-filer population by group and state.
#
# Construction and documented approximations:
#   IRS side: ht2_filing_persons() above (identities + caveats documented there)
#   ACS (person-level PERWT sums):
#     married adults = AGE >= 18 & MARST in (1,2)   [separated -> single,
#                      matching filing rules]
#     single adults  = AGE >= 18 & !married
#     children       = AGE < 18   [vs IRS dependents, which include 18+
#                      students/relatives -- expect ACS < IRS here for that
#                      reason and the reverse from never-claimed children]
# -----------------------------------------------------------------------------
compare_individuals_acs_irs <- function(ht2, acs) {

  irs <- ht2_filing_persons(ht2)[
    , .(state, married_filing_adults, single_filing_adults, dependents)]
  setnames(irs, c("married_filing_adults","single_filing_adults","dependents"),
                c("irs_married_adults","irs_single_adults","irs_children"))

  a <- as.data.table(acs)
  a[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  a <- a[!is.na(state)]
  acs_tab <- a[, .(
    acs_married_adults = sum(PERWT * (AGE >= 18 & MARST %in% 1:2)),
    acs_single_adults  = sum(PERWT * (AGE >= 18 & !(MARST %in% 1:2))),
    acs_children       = sum(PERWT * (AGE < 18))
  ), by = state]

  out <- merge(irs, acs_tab, by = "state")
  for (g in c("married_adults", "single_adults", "children")) {
    out[[paste0("nonfiler_", g)]] <- out[[paste0("acs_", g)]] - out[[paste0("irs_", g)]]
    out[[paste0("irs_share_", g)]] <- out[[paste0("irs_", g)]] / out[[paste0("acs_", g)]]
  }
  out[order(state)]
}

# -----------------------------------------------------------------------------
# Wage-based IRS vs ACS/QWI comparison (reconciliation diagnostic, JI
# 2026-07-13): wage earners and total wage dollars by state, IRS records vs
# two independent sources.
#
# DOLLARS are the clean comparison: HT2 A00200 (wages on filed returns) vs
# ACS INCWAGE person sums vs QWI total payroll. COUNTS carry documented
# concept gaps: HT2 N00200 counts RETURNS with wages (a two-earner joint
# return is one), ACS counts PERSONS with wage income, QWI counts JOBS
# (multi-jobholders duplicated).
#
# Source caveats:
#   ACS: INCWAGE reference period is the 12 months before interview, so the
#        2022 1-year sample spans calendar 2021-2022 income; residence-based.
#   QWI: UI-covered employment only (excludes most self-employment, some
#        federal/agricultural work) and WORKPLACE-based -- state comparisons
#        against residence-based IRS/ACS diverge sharply in commuter
#        geographies (DC especially). Requires CENSUS_API_KEY (free:
#        https://api.census.gov/data/key_signup.html).
# -----------------------------------------------------------------------------
compare_wages_acs_irs <- function(ht2, acs) {

  irs <- dcast(as.data.table(ht2)[!(state %in% NONTAX_BUCKETS) &
                                  variable %in% c("n_wages","wages_amt"),
                                  .(value = sum(value)), by = .(state, variable)],
               state ~ variable, value.var = "value")

  a <- as.data.table(acs)
  stopifnot("INCWAGE" %in% names(a))
  a[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
  a <- a[!is.na(state)]
  a[INCWAGE >= 999998, INCWAGE := NA_real_]          # IPUMS N/A / missing codes
  acs_tab <- a[, .(
    acs_wage_earners = sum(PERWT * (!is.na(INCWAGE) & INCWAGE > 0)),
    acs_wages        = sum(PERWT * fifelse(is.na(INCWAGE), 0, INCWAGE))
  ), by = state]

  out <- merge(irs, acs_tab, by = "state")
  out[, `:=`(irs_share_earners = n_wages / acs_wage_earners,
             irs_share_wages   = wages_amt / acs_wages)]
  setnames(out, c("n_wages","wages_amt"), c("irs_returns_wages","irs_wages"))
  out[order(state)]
}
