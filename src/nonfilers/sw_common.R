# =============================================================================
# sw_common.R  --  split out of state_weights.R (F4, 2026-09-23)
#
# Shared constants and helpers: the jurisdiction set, FIPS map, path roots,
# # age bands, income tiers and group-quarters classes. The light base -- sourceable
# # on its own, e.g. by filing_model.R or Affordability-Index, without the fit engine.
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
# Jurisdiction set. 50 states + DC are modeled; PR and OA (SOI "Other Areas") are
# carried as no-tax buckets so weights still sum to the national total (plan §5.4).
# -----------------------------------------------------------------------------
STATE_JURISDICTIONS <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
  "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
  "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
  "WV","WI","WY")                       # 51 modeled
NONTAX_BUCKETS <- c("PR","OA")          # carried, no state income tax calc

# -----------------------------------------------------------------------------
# Shared raw-data stores. Derived from the production output root in
# config/interfaces/output_roots.yaml (raw_data is its sibling directory), so
# no additional machine paths are hardcoded here.
#   IRS-Ind: SOI individual income tax mirror -- by-geography (HT2 here) plus
#            national Pub 1304 by-size tables (github.com/johniselin-budget-lab/
#            IRS-Ind, renamed from IRS-GEO 2026-08; see NOTES.md at the store
#            root for data caveats)
#   ACS:     IPUMS USA 1-year fixed-width extracts, us{year}a/ vintages
# -----------------------------------------------------------------------------
raw_data_root <- function() {
  file.path(yaml::read_yaml("./config/interfaces/output_roots.yaml")$production,
            "raw_data")
}
#' Root of the shared model_data store, the sibling of raw_data_root().
#'
#' Added 2026-08-30: group D's diagnostics read Tax-Data's ledgers and the
#' Macro-Projections series, and had the absolute server path plus a pinned
#' vintage typed into the tracked script. No server path belongs in a tracked
#' file, and a collaborator on another machine could not have run them.
#' Vintages stay explicit -- they pin what a result was computed against -- but
#' as arguments, not as embedded paths.
model_data_root <- function() {
  file.path(yaml::read_yaml("./config/interfaces/output_roots.yaml")$production,
            "model_data")
}

#' Root for large, regenerable research intermediates (fits, sweeps, PDFs).
#'
#' Added 2026-09-11 on the move to Tax-Data: four scripts carried the same
#' absolute scratch path, which had already been reclaimed once (2026-08-27)
#' and which no tracked file should name. Machine-specific values live in
#' ~/.Renviron (TAXDATA_SCRATCH=/some/scratch/dir) and are read here; there is
#' deliberately no default, because a default would be another server path.
#' Accepted artifacts belong in-repo (results/), not under this root.
scratch_root <- function() {
  p <- Sys.getenv("TAXDATA_SCRATCH", unset = "")
  if (!nzchar(p)) {
    stop("TAXDATA_SCRATCH is not set. Add TAXDATA_SCRATCH=<scratch dir> to ",
         "~/.Renviron; research scripts write regenerable intermediates there.",
         call. = FALSE)
  }
  p
}

#' Path into one model's published vintage, e.g.
#' model_data_path("Tax-Data", "v1", "2026081216", "baseline").
model_data_path <- function(model, version, vintage, ...) {
  p <- file.path(model_data_root(), model, version, vintage, ...)
  if (!dir.exists(p) && !file.exists(p)) {
    stop(sprintf("model_data_path(): %s not found. Vintages are pinned on ",
                 p), "purpose -- check the interface version before editing.",
         call. = FALSE)
  }
  p
}

ht2_path <- function(year) {
  file.path(raw_data_root(), "IRS-Ind/state/HT2", sprintf("ht2_%d.csv.gz", year))
}
acs_extract_dir <- function(acs_year) {
  file.path(raw_data_root(), "ACS/acs_common_v2", sprintf("us%da", acs_year))
}
ssa_workbook <- function(family, year) {
  stem <- c(`SSA-OASDI-SC` = "oasdi_sc", `SSA-EEDATA-SC` = "eedata_sc")[family]
  stopifnot(!is.na(stem))
  file.path(raw_data_root(), family, sprintf("%s%02d.xlsx", stem, year %% 100))
}

# -----------------------------------------------------------------------------
# ACS side: non-filer population margins.
#
# HT2 covers filers only, so the non-filer PUF partition is targeted to ACS/Census
# state × age × income margins instead (plan §2.1). This builds those margins from
# the local IPUMS USA extract; shared cluster copies live at
# {raw_data_root()}/ACS/acs_common_v2 (us{year}a/
# vintages 2006-2024: usa_{year}a.dat.gz + DDI xml + variables.csv). Re-pointed
# from acs_common 2026-09-04; record counts are identical sample-for-sample, so
# the state x age x income margins this builds are unchanged. The authority for WHO is a non-filer is the Tax-Data
# `filer` flag on the PUF; ACS supplies only the geographic MARGINS across which the
# non-filer partition is spread — so the ACS filing model here need only be a
# reasonable v0, refined against the reconciliation total (plan risk item).
#
# v0 tax-unit + filing model (documented approximations, to refine later):
#   - Couples (MARST==1 & SPLOC>0) file jointly, head = lower PERNUM.
#   - Children (AGE < 19, parent pointer present, not married) are dependents.
#   - All other adults head their own unit (single, or HoH if they have dependents).
#   - Qualifying-relative dependency NOT modeled (minor); HOUSEHOLD students aged
#     19-24 not separated (SCHOOL is wired below for dormitory residents only --
#     keeping household students dependent is task C1, design memo §3.2.1).
#   - Filer if unit gross income ≥ the filing threshold (standard-deduction proxy by
#     filing status, year-specific); else non-filer. Ignores the $400 SE rule,
#     dependents' own filing, and elderly/blind bumps (approximation).
#
# Group quarters ARE differentiated (task B1, decision D4). The v0 non-treatment
# put the ~8M group-quarters persons into the unit builder as income-less
# single-person units, which is right in principle -- the PUF/DINA universe
# INCLUDES group-quarters residents, so a blanket exclusion would rake genuine
# non-filer mass out of exactly the prison/college/nursing-home states (design
# memo §3.0, and F6 measured the overshoot: AZ falls to 0.77x its anchor under
# exclusion) -- but wrong in composition, because dorm students are dependents
# on a parent's return, generally in another state. See build_acs_margins().
# -----------------------------------------------------------------------------
FIPS_TO_STATE <- c(
  "1"="AL","2"="AK","4"="AZ","5"="AR","6"="CA","8"="CO","9"="CT","10"="DE",
  "11"="DC","12"="FL","13"="GA","15"="HI","16"="ID","17"="IL","18"="IN","19"="IA",
  "20"="KS","21"="KY","22"="LA","23"="ME","24"="MD","25"="MA","26"="MI","27"="MN",
  "28"="MS","29"="MO","30"="MT","31"="NE","32"="NV","33"="NH","34"="NJ","35"="NM",
  "36"="NY","37"="NC","38"="ND","39"="OH","40"="OK","41"="OR","42"="PA","44"="RI",
  "45"="SC","46"="SD","47"="TN","48"="TX","49"="UT","50"="VT","51"="VA","53"="WA",
  "54"="WV","55"="WI","56"="WY")

# Age bands. ONE definition, three consumers: the ACS non-filer margins, the PUF
# non-filer cell assignment, and the residual anchors (research/state_weights/nonfiler_residual/02, /03).
#
# The boundaries are Pub 1304 Table 1.6's, not our own choosing, and the reason is
# that T1.6 is the ONLY published source of filing adults by age -- so it sets the
# resolution of the residual (PEP adults - filing adults), no matter how much age
# detail PEP or the ACS carry. Two consequences, both decided 2026-08-19:
#
#   * The bottom cut is 26, NOT 25. The pre-2026-08-19 age_band() cut at 25 while
#     the anchors cut at 26, so the 4.33M people aged exactly 25 sat in a different
#     cell on the target side than on the record side -- more than the entire
#     non-filer residual of all but the largest states, and silent.
#   * 65_74 / 75p survive as CELLS but not as state TARGETS. T1.6 stops at "65 and
#     over" and SSA OASDI-SC publishes 65+ by sex only, so no state-level source can
#     split it. Nationally it IS splittable -- SOI's IRA study Table 4 column (1)
#     gives Form 1040 filers by 5-year age band to 80+ -- so the national age shape
#     (which is what the Tax-Data age draw needs) keeps the split, while state
#     targets collapse to 65p. Hence two functions.
#
# age_band()        -- the CELL space: full resolution, what records are keyed on
#                      and what the ACS prior shapes.
# target_age_band() -- the TARGET space: collapsed to what the anchors can source.
#
# Same discipline as the income tiers: the anchors own the level, the ACS owns the
# within-cell shape.

AGE_BANDS        <- c("18_25","26_34","35_44","45_54","55_64","65_74","75p")
TARGET_AGE_BANDS <- c("18_25","26_34","35_44","45_54","55_64","65p")

age_band <- function(age) {
  cut(age, breaks = c(-Inf, 26, 35, 45, 55, 65, 75, Inf),
      labels = AGE_BANDS, right = FALSE)
}

# Collapse the cell space onto the anchor's target space.
target_age_band <- function(band) {
  b <- as.character(band)
  factor(fifelse(b %in% c("65_74", "75p"), "65p", b), levels = TARGET_AGE_BANDS)
}

# The anchors' band function, on PERSON age, adults only (18+). Identical
# boundaries to target_age_band(); kept separate because it takes an age, not a
# band, and because it is undefined below 18 where the anchors have no universe.
a16_band <- function(age) {
  cut(age, breaks = c(18, 26, 35, 45, 55, 65, Inf),
      labels = TARGET_AGE_BANDS, right = FALSE)
}
income_tier <- function(inc) {
  cut(pmax(inc, 0), breaks = c(-Inf, 1, 10e3, 25e3, 50e3, Inf),
      labels = c("neg_zero","1_10k","10_25k","25_50k","50k_plus"), right = FALSE)
}

# Group-quarters classification (IPUMS USA `GQ`). Household codes are 1/2/5;
# 3 is institutions (correctional facilities, nursing homes, psychiatric
# hospitals); 4 is "other group quarters" -- college dormitories, military
# barracks, shelters, group homes, bundled together because the extract carries
# no dormitory or barracks flag and EMPSTAT cannot separate them cleanly.
#
# College residence is therefore identified as GQ==4 & in school & aged 18-24.
# That predicate is Stage D's (research/state_weights/nonfiler_residual/03,
# which produced the T7 composition table and F6's sizing); it lives here so the
# production margins and the diagnostic cannot drift apart -- the same rule as
# age_band()/a16_band().
GQ_HOUSEHOLD      <- c(1L, 2L, 5L)
GQ_INSTITUTIONAL  <- 3L
GQ_OTHER          <- 4L
DORM_STUDENT_AGES <- 18:24

classify_gq <- function(GQ, SCHOOL, AGE) {
  fifelse(GQ == GQ_INSTITUTIONAL, "institutional",
  fifelse(GQ == GQ_OTHER & SCHOOL == 2 & AGE %in% DORM_STUDENT_AGES, "dorm_student",
  fifelse(GQ == GQ_OTHER, "other_noninstitutional", "household")))
}
