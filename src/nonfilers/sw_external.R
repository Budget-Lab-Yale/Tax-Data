# =============================================================================
# sw_external.R  --  split out of state_weights.R (F4, 2026-09-23)
#
# External readers: SSA (OASDI 65+, HI covered earnings), Census QWI and LODES.
#
# Code moved verbatim; see state_weights.R for the module map.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(readr)
  library(stringr)
})

#' State-level annual employment and payroll from Census QWI (LEHD sex-age
#' endpoint). API facts learned empirically (2026-07-13): the 'for' clause
#' does not accept state:* (loop states); the Payroll variable is NULL at
#' state tabulations, so annual payroll is proxied as
#'   Σ_quarters 3 * EmpS * EarnS
#' (full-quarter stable jobs x average monthly earnings x 3 months) --
#' understates total payroll by unstable-job earnings; document when
#' comparing levels. Jobs are not persons (multi-jobholders duplicated) and
#' geography is WORKPLACE-based.
#'
#' The sex x age cross IS fully published: pass sex ('1','2') and agegrp
#' ('A01'..'A08', WIA bands) vectors to pull demographic cells -- the basis
#' for candidate person-level wage/employment targets matched to the PUF's
#' individual earners (wages1/2 x male1/2 x age1/2).
fetch_qwi <- function(year, sex = "0", agegrp = "A00",
                      states = names(FIPS_TO_STATE),
                      key = Sys.getenv("CENSUS_API_KEY")) {

  if (!nzchar(key)) {
    stop("fetch_qwi(): set CENSUS_API_KEY ",
         "(free signup: https://api.census.gov/data/key_signup.html)")
  }
  grid <- CJ(fips = states, q = 1:4, sx = sex, ag = agegrp)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i]
    url <- sprintf(paste0("https://api.census.gov/data/timeseries/qwi/sa",
                          "?get=Emp,EmpS,EarnS&for=state:%s&time=%d-Q%d",
                          "&sex=%s&agegrp=%s&key=%s"),
                   g$fips, year, g$q, g$sx, g$ag, key)
    j <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    if (is.null(j) || nrow(j) < 2) return(NULL)
    dt <- as.data.table(j[-1, , drop = FALSE])
    setnames(dt, as.character(j[1, ]))
    dt[, quarter := g$q]
    dt
  })
  out <- rbindlist(rows, fill = TRUE)
  out[, c("Emp","EmpS","EarnS") := lapply(.SD, as.numeric),
      .SDcols = c("Emp","EmpS","EarnS")]
  out[, .(qwi_jobs_avg      = mean(Emp,  na.rm = TRUE),
          qwi_stable_avg    = mean(EmpS, na.rm = TRUE),
          qwi_payroll_proxy = sum(3 * EmpS * EarnS, na.rm = TRUE)),
      by = .(statefips = state, sex, agegrp)][
    , state := FIPS_TO_STATE[as.character(as.integer(statefips))]][
    !is.na(state)][order(state, sex, agegrp)]
}

# -----------------------------------------------------------------------------
# SSA statcomps administrative margins (nonfiler_residual_design.md §4.2).
# NOTES.md in each store is the authority on the data; what follows encodes the
# traps rather than restating them.
#
#   SSA-OASDI-SC   OASDI beneficiaries in current-payment status, a DECEMBER
#                  STOCK from the Master Beneficiary Record (100% data). Table
#                  2's last two measures -- beneficiaries aged 65 or older, by
#                  sex -- are the elderly age margin (D6). The flat series is
#                  the source of record; the workbook is the cross-check.
#   SSA-EEDATA-SC  Persons with covered earnings, an ANNUAL FLOW from the
#                  Continuous Work History Sample (1% sample, so state counts
#                  carry sampling error). Per-year workbooks only.
#
# The coverage concept is HI/Medicare, not OASDI (JI 2026-08-19): HI includes
# the state and local government employment outside OASDI coverage (~4.1M
# persons) and its earnings are uncapped, agreeing with QCEW to ~1% where
# OASDI's capped earnings sit ~17% low. So Table 4 for persons and dollars,
# Table 5 for the state x age detail; Tables 1/2 are the cross-check only.
#
# Traps the readers below handle:
#   - Sum the 51 jurisdictions; never read `All areas`, which carries the
#     territories and (OASDI) beneficiaries abroad -- 2.6% too many 65+ people.
#   - `Total` != `Wage and salary` + `Self-employed`: a person with both kinds
#     of earnings is counted in both components but once in the total. The
#     covered-worker margin is `Wage and salary`.
#   - Table 5's age detail is published on the TOTAL covered universe, not on
#     wage-and-salary alone. It is a SHAPE, on a wider universe than Table 4's
#     level; its `Total, all ages` is not a wage-and-salary count.
#   - The flat series skips 2010 and labels the U.S. Virgin Islands
#     `Virgin Islands` through 2006.
# -----------------------------------------------------------------------------

# SSA publishes state names; base R's state.name/state.abb omit DC.
SSA_STATE_ABB <- c(setNames(state.abb, state.name),
                   `District of Columbia` = "DC")

# Both publications lay geography out across several label columns -- states in
# the first, territories indented into the second, `All areas` into the third --
# with sex sub-rows indented one further. A row is an AREA TOTAL when exactly
# one non-sex label appears among the label columns and no sex label does.
SSA_SEX_LABELS <- c("Men", "Women")

# Footnote markers ride on the column headers ("Total c") and vary by edition;
# strip them so the layout assertion checks structure, not footnote letters.
.ssa_norm_key <- function(k) {
  k <- gsub("\\s+[a-z]\\b", "", k)
  k <- gsub("Self-\\s*employed", "Self-employed", k)
  str_squish(k)
}

# One header row as character, whatever type readxl gave each column.
.ssa_header_row <- function(x, r, first_data_col) {
  v <- vapply(first_data_col:ncol(x), function(j) as.character(x[[j]][r]),
              character(1))
  v[is.na(v)] <- ""
  str_squish(v)
}

#' Read one SSA state table into `state` x published-column form.
#'
#' @param first_data_col first column holding numbers (label columns precede it)
#' @param header_rows    rows carrying the column headers; all but the last are
#'                       merged group labels and are forward-filled
#' @return data.table of the 51 modeled jurisdictions, one column per published
#'         column, plus attribute `all_areas` -- the published national row,
#'         kept for a file-identity check, never used as the anchor.
.ssa_area_table <- function(path, sheet, first_data_col, header_rows) {
  x <- suppressMessages(readxl::read_excel(path, sheet = sheet,
                                           col_names = FALSE,
                                           .name_repair = "minimal"))
  lab <- as.matrix(x[, seq_len(first_data_col - 1), drop = FALSE])
  lab[is.na(lab)] <- ""
  lab[] <- str_squish(lab)
  val <- vapply(first_data_col:ncol(x),
                function(j) suppressWarnings(as.numeric(x[[j]])),
                numeric(nrow(x)))

  keys <- lapply(header_rows, .ssa_header_row, x = x, first_data_col = first_data_col)
  for (i in seq_len(length(keys) - 1)) {          # forward-fill merged groups
    v <- keys[[i]]
    for (j in seq_along(v)) if (v[j] == "" && j > 1) v[j] <- v[j - 1]
    keys[[i]] <- v
  }
  keys <- .ssa_norm_key(do.call(paste, keys))

  area <- apply(lab, 1, function(r) {
    r <- r[r != "" & !(r %in% SSA_SEX_LABELS)]
    if (length(r) == 1) r else NA_character_
  })
  keep <- !is.na(area) & !apply(lab, 1, function(r) any(r %in% SSA_SEX_LABELS)) &
          !is.na(val[, 1])

  out <- as.data.table(val[keep, , drop = FALSE])
  setnames(out, keys)
  out[, area := area[keep]]

  national <- out[area == "All areas"]
  stopifnot(nrow(national) == 1)
  out[, state := SSA_STATE_ABB[area]]
  out <- out[!is.na(state)]
  stopifnot(nrow(out) == 51, setequal(out$state, STATE_JURISDICTIONS))
  out <- out[, area := NULL][order(state)]
  # The 51 jurisdictions are a strict subset of the published `All areas` row,
  # and a large majority of it -- the rest is territories, and for OASDI
  # beneficiaries abroad. A parse that dropped, duplicated or misaligned rows
  # breaks this here rather than in a caller's arithmetic.
  s51 <- sum(out[[1]])
  stopifnot(s51 <= national[[1]], s51 >= 0.9 * national[[1]])
  setattr(out, "all_areas", national)
  out[]
}

read_ssa_oasdi_65p <- function(year) {
  fs <- file.path(raw_data_root(), "SSA-OASDI-SC",
                  "oasdi_sc_flatseries_table2_beneficiaries.json")
  d <- as.data.table(jsonlite::fromJSON(fs)$data)
  d <- d[month == sprintf("%d-12", year)]
  if (nrow(d) == 0) {
    stop("read_ssa_oasdi_65p(): no ", year, "-12 in the flat series, which ",
         "spans 1999-2025 and SKIPS 2010.")
  }
  d[state_or_area == "Virgin Islands", state_or_area := "U.S. Virgin Islands"]
  d[, state := SSA_STATE_ABB[state_or_area]]
  out <- d[!is.na(state), .(state, beneficiaries_65p = persons_oasdi_65_older_men +
                                                       persons_oasdi_65_older_women)]
  stopifnot(nrow(out) == 51, setequal(out$state, STATE_JURISDICTIONS))

  wb <- ssa_workbook("SSA-OASDI-SC", year)
  if (file.exists(wb)) {
    t2 <- .ssa_area_table(wb, "Table 2", first_data_col = 4, header_rows = 2:3)
    stopifnot(identical(names(t2), c("Total",
      "Retirement Retired workers", "Retirement Spouses", "Retirement Children",
      "Survivors Widow(er)s and parents", "Survivors Children",
      "Disability Disabled workers", "Disability Spouses", "Disability Children",
      "Aged 65 or older Men", "Aged 65 or older Women", "state")))
    chk <- merge(out, t2[, .(state, wb_65p = `Aged 65 or older Men` +
                                             `Aged 65 or older Women`)], by = "state")
    stopifnot(nrow(chk) == 51, all(chk$beneficiaries_65p == chk$wb_65p))
  } else {
    message("read_ssa_oasdi_65p(): no workbook for ", year,
            "; flat series read without its cross-check")
  }
  out[, universe := "beneficiary"][order(state)]
}


#' HI (Medicare)-covered workers by state, `year`: level from Table 4, age
#' shape from Table 5.
#'
#' @return list of two data.tables, both tagged `covered_worker_hi`:
#'   `persons` state x {total / wage-and-salary / self-employed persons,
#'             wage-and-salary taxable earnings in DOLLARS}
#'   `age`     state x SSA age band x persons, on the TOTAL covered universe
#'             (see the section header -- Table 5 has no wage-and-salary split)
read_ssa_eedata_hi <- function(year) {
  wb <- ssa_workbook("SSA-EEDATA-SC", year)
  if (!file.exists(wb)) {
    stop("read_ssa_eedata_hi(): no workbook for ", year, " (", basename(wb),
         "). The series runs 2017-2023 and has no flat-series alternative.")
  }
  t4 <- .ssa_area_table(wb, "Table 4", first_data_col = 5, header_rows = 2:3)
  t5 <- .ssa_area_table(wb, "Table 5", first_data_col = 5, header_rows = 2)

  stopifnot(identical(names(t4), c(
    "Number Total", "Number Wage and salary", "Number Self-employed",
    "Taxable earnings (thousands of dollars) Total",
    "Taxable earnings (thousands of dollars) Wage and salary",
    "Taxable earnings (thousands of dollars) Self-employed",
    "HI contributions (thousands of dollars) Total",
    "HI contributions (thousands of dollars) Wage and salary",
    "HI contributions (thousands of dollars) Self-employed", "state")))
  ssa_age_bands <- c("Under 20", "20–29", "30–39", "40–49",
                     "50–59", "60–61", "62–64", "65–69",
                     "70 or older")
  stopifnot(identical(names(t5), c("Total, all ages", ssa_age_bands, "state")))

  persons <- t4[, .(state,
                    hi_persons_total       = `Number Total`,
                    hi_persons_wage_salary = `Number Wage and salary`,
                    hi_persons_self_emp    = `Number Self-employed`,
                    hi_wage_salary_earnings =
                      `Taxable earnings (thousands of dollars) Wage and salary` * 1000)]

  # Table 5's all-ages column IS Table 4's `Number Total` -- the check that
  # pins down which universe the age shape is published on.
  stopifnot(all(t5[order(state), `Total, all ages`] ==
                t4[order(state), `Number Total`]))
  age <- melt(t5, id.vars = "state", measure.vars = ssa_age_bands,
              variable.name = "ssa_age_band", value.name = "hi_persons_total")
  # Bands sum to the all-ages column up to independent rounding of each cell in
  # the 1% sample (0 persons in 2017, 7 in 2022, on a base of 183M).
  stopifnot(abs(age[, sum(hi_persons_total)] -
                t5[, sum(`Total, all ages`)]) < 1000)

  list(persons = persons[order(state)][, universe := "covered_worker_hi"][],
       age     = age[order(state, ssa_age_band)][, universe := "covered_worker_hi"][])
}

# -----------------------------------------------------------------------------
# LODES (LEHD Origin-Destination Employment Statistics): the residence-basis
# answer to QWI's workplace-basis problem (JI 2026-07-13). Same underlying
# LEHD job frame as QWI, published both ways:
#   RAC (Residence Area Characteristics): jobs by HOME census block --
#     aggregated to state, this is residence-based employment directly,
#     with age bands (CA01 <=29 / CA02 30-54 / CA03 55+) and monthly
#     earnings bands (CE01 <=$1,250 / CE02 $1,251-3,333 / CE03 >$3,333).
#   OD (main = in-state residents, aux = out-of-state residents): home x
#     work block flows -- aggregated, the state-to-state commuter matrix
#     that converts workplace payroll to residence basis. Verified 2022:
#     only 31.0% of DC-workplace primary jobs are held by DC residents
#     (MD 39.0%, VA 25.9%) -- the entire QWI DC/MD/VA anomaly, quantified.
#
# Concept notes: JT01 = primary jobs (one per worker, closest to persons);
# point-in-time job counts sit below ACS/IRS any-wage-in-year earner counts
# by construction. Same UI-covered universe as QWI. LODES8 covers through
# 2022 on 2020 blocks. Block geocodes must be read as CHARACTER (15 digits;
# integer64 breaks substr-based state extraction).
# -----------------------------------------------------------------------------
LODES_BASE <- "https://lehd.ces.census.gov/data/lodes/LODES8"

#' Residence-based state employment from LODES RAC, with age/earnings bands.
fetch_lodes_rac <- function(year, states = tolower(STATE_JURISDICTIONS),
                            jt = "JT01") {
  rbindlist(lapply(states, function(st) {
    d <- tryCatch(
      fread(sprintf("%s/%s/rac/%s_rac_S000_%s_%d.csv.gz",
                    LODES_BASE, tolower(st), tolower(st), jt, year)),
      error = function(e) NULL)
    if (is.null(d)) {
      message("  LODES RAC unavailable: ", st, " ", year)
      return(NULL)
    }
    data.table(state = toupper(st),
               workers_res  = sum(d$C000),
               age_u30      = sum(d$CA01), age_30_54 = sum(d$CA02),
               age_55p      = sum(d$CA03),
               earn_low     = sum(d$CE01), earn_mid  = sum(d$CE02),
               earn_high    = sum(d$CE03))
  }))
}

#' State-to-state commuter matrix from LODES OD (jobs by work state x
#' residence state). Downloads main + aux per WORK state requested.
fetch_lodes_od_matrix <- function(year, work_states, jt = "JT01") {
  rbindlist(lapply(work_states, function(st) {
    od <- rbindlist(lapply(c("main", "aux"), function(part) {
      tryCatch(
        fread(sprintf("%s/%s/od/%s_od_%s_%s_%d.csv.gz",
                      LODES_BASE, tolower(st), tolower(st), part, jt, year),
              colClasses = list(character = c("w_geocode", "h_geocode")))[
          , .(h_geocode, S000)],
        error = function(e) NULL)
    }))
    if (nrow(od) == 0) return(NULL)
    od[, res_state := FIPS_TO_STATE[as.character(as.integer(substr(h_geocode, 1, 2)))]]
    od[, .(jobs = sum(S000)), by = res_state][
      , `:=`(work_state = toupper(st), share = jobs / sum(jobs))][]
  }))
}
