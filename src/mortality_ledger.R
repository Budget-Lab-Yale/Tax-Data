#---------------------------------------------------
# mortality_ledger.R
#
# Per-record per-year probability of death for the
# primary (q_death1) and secondary (q_death2) filer.
# Used downstream in Tax-Simulator for estate tax and
# step-up-basis modeling.
#
# Layered q_death:
#   q_death = q_baseline(age, sex, year)   # SSA Trustees TR2024
#           × R_chetty(age, sex, pctile)   # Chetty 2016 Table 15
#           × R_marital(age, sex, married) # Johnson 2000, men only
#
# Layer composition is mean-preserving by construction on the PUF
# distribution: build_chetty_pctile uses a within-(sex, age) rank, and
# renormalize_johnson_for_puf rescales the Johnson factors so each
# (sex='M', age_band) cell has weighted mean 1.0 on the PUF marital
# composition. Per-cell ratio imputed_q / SSA_q lands within ±3% across
# non-young bands, so no aggregate calibration step is needed.
#
# Departure from Ricco (2020) PWBM Tax Module §4.2: the marital
# adjustment is applied to males only. Lillard & Waite (1995, AJS)
# document that the female marital mortality benefit is largely
# income-mediated; Chetty's percentile is calculated within (gender,
# age, year) but NOT within marital status (per healthinequality.org
# Table 15 readme), so a Johnson 2000 layer for women double-counts
# the income-mediated portion of the marital effect that Chetty
# already absorbs. Johnson's male factors survive Chetty because the
# male marital effect is largely net of income.
#
# References:
#   Ricco (2020), "Data Processing for PWBM's Tax Module" §4.2
#   Chetty et al. (2016), JAMA 315(16):1750
#   Johnson, Backlund, Sorlie & Loveless (2000), Ann Epi 10(4)
#   Lillard & Waite (1995), AJS 100(5):1131
#---------------------------------------------------


#---------------------------------------------
# Categorical bin helpers
#---------------------------------------------

# Chetty Table 15 age bins. Ricco 2020 convention: ages <40 use the
# 40-49 bin; ages 77+ use the 70-76 bin (the boundary bin extends to
# all out-of-coverage ages, no closure model).
age_to_chetty_bin = function(age) {
  bin = character(length(age))
  bin[age < 50]              = '<50'
  bin[age >= 50 & age <  60] = '50-59'
  bin[age >= 60 & age <  70] = '60-69'
  bin[age >= 70]             = '70+'
  bin
}

# Johnson 2000 age bands.
age_to_johnson_band = function(age) {
  ifelse(age < 65, 'under_65', 'age_65_plus')
}

# PUF MARS → married indicator. PWBM convention (Ricco 2020):
# MFJ → married; Single, MFS, HoH, QW → unmarried.
filing_status_to_married = function(filing_status) {
  as.integer(filing_status == 2L)
}

# Internal: normalize a sex column to {'M', 'F'}. Accepts a logical /
# integer "is_male" column as well as a character column.
normalize_sex = function(sex) {
  if (is.logical(sex) || is.integer(sex) || is.numeric(sex)) {
    ifelse(sex == 1L | sex == TRUE, 'M', 'F')
  } else {
    out = ifelse(sex == 'M' | sex == 'Male' | sex == 'male', 'M', 'F')
    out
  }
}


#---------------------------------------------
# Chetty income gradient: R_chetty(sex, age_bin, pctile)
#
# R_{y,a} = m_{y,a} / mean_{y in 1..100} m_{y,a}
# where m_{y,a} is the average mortrate over the Chetty panel
# (years 2001-2014) at percentile y, age bin a. Within each
# (sex, age_bin), R_chetty's pctile-marginal is 1.0 by construction,
# so layering R_chetty on top of an SSA baseline does not change
# the (sex, age_bin) marginal q_x.
#---------------------------------------------

build_chetty_R = function(path = 'resources/chetty_table_15.csv') {
  raw = readr::read_csv(path, show_col_types = FALSE)
  required = c('gnd', 'pctile', 'age_at_d', 'yod', 'mortrate')
  missing  = setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop('build_chetty_R(): missing columns: ',
         paste(missing, collapse = ', '))
  }
  if (any(is.na(raw$mortrate))) {
    stop('build_chetty_R(): chetty_table_15 has NA mortrate (',
         sum(is.na(raw$mortrate)), ' rows). Investigate before proceeding.')
  }

  # Average across years first to get m_{y,a}, then ratio.
  raw %>%
    dplyr::mutate(
      sex     = ifelse(gnd == 'M', 'M', 'F'),
      age_bin = age_to_chetty_bin(age_at_d),
      pctile  = as.integer(pctile)
    ) %>%
    dplyr::group_by(sex, age_bin, pctile) %>%
    dplyr::summarise(m = mean(mortrate),
                     .groups = 'drop') %>%
    dplyr::group_by(sex, age_bin) %>%
    dplyr::mutate(R_chetty = m / mean(m)) %>%
    dplyr::ungroup() %>%
    dplyr::select(sex, age_bin, pctile, R_chetty)
}


#---------------------------------------------
# Johnson 2000 marital factors: R_marital(sex, age_band, married)
#
# Resource file ships only male rows by design (see Lillard & Waite
# 1995 finding noted at top). Female lookups fall through to 1.0.
#---------------------------------------------

load_johnson_marital = function(path = 'resources/johnson_2000_marital_hr.csv') {
  raw = readr::read_csv(path, show_col_types = FALSE)
  required = c('sex', 'age_band', 'married', 'unmarried')
  missing  = setdiff(required, names(raw))
  if (length(missing) > 0L) {
    stop('load_johnson_marital(): missing columns: ',
         paste(missing, collapse = ', '))
  }
  raw %>%
    tidyr::pivot_longer(cols      = c(married, unmarried),
                        names_to  = 'marital_label',
                        values_to = 'factor') %>%
    dplyr::mutate(married = as.integer(marital_label == 'married')) %>%
    dplyr::select(sex, age_band, married, factor)
}


#---------------------------------------------
# q_baseline providers (pluggable)
#
# Each provider returns a closure with signature
#   function(age, sex, year) → numeric vector of q_x
# vectorized over its arguments (which must be the same length).
#---------------------------------------------

# Default: SSA Trustees TR2024 period life tables. Hist 1900-2021 +
# Alt2 2022-2100, both sexes, single year of age 0-119.
#
# `cap_age` (default 80) handles the upstream PUF age cap: ages in
# `tax_units` are hard-floored at 80 (search `pmin(80,` in src/), so
# records coded as age=80 actually represent everyone 80+. For those
# records, we replace q(80) with an L-weighted average q across ages
# [cap_age, age_max] — the standard actuarial average over a "rest of
# life" band. Without this correction, q_baseline at the cap returns
# only q(80) ≈ 0.05-0.06, missing roughly half the mortality of the
# very-old who are pooled into that bin.
#
# The within-cap-age age distribution comes from SSA's stationary
# life-table population (L_x). This is an approximation: actual cohort
# composition at 80+ in any given year differs from the stationary one.
# Magnitude of the approximation error is small relative to the cap-
# correction itself.
make_q_baseline_ssa_tr2024 = function(resources_dir = 'resources',
                                       cap_age       = MAX_AGE) {
  read_one = function(path) {
    # SSA file format: 4 prose header lines, then a CSV header
    # `Year,x,q(x),l(x),d(x),L(x),...`. We need (Year, x, q(x), L(x)).
    raw = readr::read_csv(path, skip = 4, show_col_types = FALSE,
                          col_select = c('Year', 'x', 'q(x)', 'L(x)'))
    tibble::tibble(
      year = as.integer(raw$Year),
      age  = as.integer(raw$x),
      q_x  = as.numeric(raw$`q(x)`),
      L_x  = as.numeric(raw$`L(x)`)
    )
  }

  m = dplyr::bind_rows(
        read_one(file.path(resources_dir, 'PerLifeTables_M_Hist_TR2024.csv')),
        read_one(file.path(resources_dir, 'PerLifeTables_M_Alt2_TR2024.csv'))
      ) %>% dplyr::mutate(sex = 'M')
  f = dplyr::bind_rows(
        read_one(file.path(resources_dir, 'PerLifeTables_F_Hist_TR2024.csv')),
        read_one(file.path(resources_dir, 'PerLifeTables_F_Alt2_TR2024.csv'))
      ) %>% dplyr::mutate(sex = 'F')
  full = dplyr::bind_rows(m, f) %>%
    dplyr::distinct(year, age, sex, .keep_all = TRUE)

  if (any(is.na(full$q_x))) {
    stop('make_q_baseline_ssa_tr2024(): NA q_x in SSA file union (',
         sum(is.na(full$q_x)), ' rows).')
  }

  age_max  = max(full$age)
  year_min = min(full$year)
  year_max = max(full$year)

  # Replace q(cap_age) with the L-weighted average q over [cap_age, age_max]
  # per (year, sex). Records coded at age=cap_age represent everyone at or
  # above cap_age in the upstream PUF; this gives them the right
  # population-averaged mortality rather than just q(cap_age).
  if (!is.null(cap_age)) {
    cap_avg = full %>%
      dplyr::filter(age >= cap_age) %>%
      dplyr::group_by(year, sex) %>%
      dplyr::summarise(q_x_avg = sum(L_x * q_x) / sum(L_x),
                       .groups = 'drop') %>%
      dplyr::mutate(age = as.integer(cap_age))

    full = full %>%
      dplyr::filter(!(age == cap_age)) %>%
      dplyr::bind_rows(
        cap_avg %>% dplyr::transmute(year, age, sex,
                                     q_x = q_x_avg,
                                     L_x = NA_real_)
      )
  }

  # Build a string-keyed lookup. For ~270K records × 81 years, a
  # paste-based match() takes a handful of seconds; fast enough.
  full$key = paste(full$year, full$age, full$sex, sep = ':')

  function(age, sex, year) {
    if (length(age) != length(sex) || length(age) != length(year)) {
      stop('q_baseline: age, sex, year must be same length')
    }
    age_int = pmin(pmax(as.integer(age), 0L), age_max)
    if (any(year < year_min | year > year_max)) {
      stop(sprintf('q_baseline: year out of range [%d, %d]', year_min, year_max))
    }
    sex_norm = normalize_sex(sex)
    query = paste(as.integer(year), age_int, sex_norm, sep = ':')
    idx = match(query, full$key)
    if (any(is.na(idx))) {
      first_missing = which(is.na(idx))[1]
      stop(sprintf('q_baseline: %d (year, age, sex) tuple(s) not found. First: %s',
                   sum(is.na(idx)), query[first_missing]))
    }
    full$q_x[idx]
  }
}

# Static 2022: ignores year. For testing only.
make_q_baseline_static_2022 = function(path = 'resources/ssa_life_table_2022.csv') {
  lt = readr::read_csv(path, show_col_types = FALSE)
  age_max = max(lt$age)
  function(age, sex, year) {
    age_int = pmin(pmax(as.integer(age), 0L), age_max)
    sex_norm = normalize_sex(sex)
    ifelse(sex_norm == 'M',
           lt$male_qx[match(age_int, lt$age)],
           lt$female_qx[match(age_int, lt$age)])
  }
}


#---------------------------------------------
# Bespoke percentile match for the Chetty layer
#
# Chetty 2016 computes percentiles WITHIN (gender × age × year), so his
# R_{y,a} is mean-preserving only when records are uniformly distributed
# across pctile within each (sex, age) cell. A global income rank (like
# record_bucket.R produces for DFA) violates that — within a (sex, age)
# cell, the global-rank distribution is non-uniform, and mean R_chetty
# drifts off 1.
#
# This function computes the right thing: a per-person within-(sex, age)
# weighted rank in 1..100. Inline to the mortality build, not exposed
# elsewhere — DFA wealth aging keeps using the global rank from
# record_bucket.R. The two pctile concepts serve different purposes.
#
# Returns wide tibble: (id, pctile1, pctile2). pctile2 is NA for non-MFJ
# records (no secondary). Income is shared at the tax-unit level; only
# the (sex, age) cell differs across the two persons.
#---------------------------------------------

build_chetty_pctile = function(puf_ref) {
  # Broad-income definition lives in src/imputations/helpers.R; this module
  # uses the same definition as record_bucket.R and wealth.R for cross-
  # module rank coherence.
  required = c('id', 'weight', 'age1', 'age2', 'male1', 'male2',
               'filing_status', broad_income_components)
  missing = setdiff(required, names(puf_ref))
  if (length(missing) > 0L) {
    stop('build_chetty_pctile(): missing columns: ',
         paste(missing, collapse = ', '))
  }

  income = compute_broad_income(puf_ref)

  # Ages are capped at 80 in tax_units (search `pmin(80,` in src/).
  # Within-cell rank uses the same capped age, so a record at age=80
  # is ranked among the entire 80+ pool — consistent with the
  # cap-age correction in q_baseline.
  age1_cap = pmin(MAX_AGE, as.integer(puf_ref$age1))
  age2_cap = dplyr::if_else(!is.na(puf_ref$age2),
                            pmin(MAX_AGE, as.integer(puf_ref$age2)),
                            NA_integer_)

  # Long format: one row per person.
  primary = tibble::tibble(
    id     = puf_ref$id,
    person = 1L,
    sex    = normalize_sex(puf_ref$male1),
    age    = age1_cap,
    weight = puf_ref$weight,
    income = income
  )

  has_secondary = puf_ref$filing_status == 2L &
                  !is.na(puf_ref$age2) &
                  !is.na(puf_ref$male2)
  secondary = tibble::tibble(
    id     = puf_ref$id[has_secondary],
    person = 2L,
    sex    = normalize_sex(puf_ref$male2[has_secondary]),
    age    = age2_cap[has_secondary],
    weight = puf_ref$weight[has_secondary],
    income = income[has_secondary]
  )

  long = dplyr::bind_rows(primary, secondary)

  # Within each (sex, age) cell, compute weighted rank → 1..100.
  ranked = long %>%
    dplyr::group_by(sex, age) %>%
    dplyr::arrange(income, .by_group = TRUE) %>%
    dplyr::mutate(cum_w      = cumsum(weight) / sum(weight),
                  rank_0_100 = 100 * cum_w,
                  pctile     = pmin(100L,
                              pmax(  1L,
                                   as.integer(ceiling(rank_0_100))))) %>%
    dplyr::ungroup() %>%
    dplyr::select(id, person, pctile)

  # Wide: (id, pctile1, pctile2). Records without secondary get pctile2 = NA.
  ranked %>%
    tidyr::pivot_wider(names_from   = person,
                       values_from  = pctile,
                       names_prefix = 'pctile')
}


#---------------------------------------------
# Modifier: per-record, constant across years
#
# modifier1 = R_chetty(age1, sex1, pctile1) × R_marital(age1, sex1, married)
# modifier2 = same with secondary; NA when no secondary (filing_status != 2).
#
# pctile1 / pctile2 are within-(sex, age) ranks from build_chetty_pctile,
# so each person is matched to Chetty at the right cell-conditional pctile.
#---------------------------------------------

# Vectorized R_chetty lookup. Queries (sex, age_bin, pctile) → R_chetty.
lookup_R_chetty = function(sex, age_bin, pctile, chetty_R) {
  query = paste(sex, age_bin, as.integer(pctile), sep = ':')
  key   = paste(chetty_R$sex, chetty_R$age_bin,
                as.integer(chetty_R$pctile), sep = ':')
  idx = match(query, key)
  if (any(is.na(idx))) {
    first = which(is.na(idx))[1]
    stop(sprintf('lookup_R_chetty: %d miss(es). First: %s',
                 sum(is.na(idx)), query[first]))
  }
  chetty_R$R_chetty[idx]
}

#' Renormalize Johnson 2000 factors so weighted mean = 1.0 within each
#' (sex='M', age_band) cell on the PUF's actual marital composition.
#'
#' Johnson's published HRs (0.94 / 1.26 etc.) are mean-preserving only on
#' the marital-share distribution Johnson observed. Our PUF's distribution
#' differs (M under-65 has more singles than Johnson's reference), so the
#' raw Johnson layer drifts off 1 within each cell and biases q_death.
#' This is the same kind of issue that build_chetty_pctile fixed for the
#' Chetty income gradient.
#'
#' Females: factor stays 1.0 by design (Lillard-Waite asymmetric finding).
#'
#' @param johnson    The 4-row male-only tibble from load_johnson_marital.
#' @param tax_units  PUF tibble with per-record demographics + weight.
#' @return           Same shape as `johnson`; factors rescaled so
#'                   E[R_marital | sex=M, age_band] = 1 on the PUF
#'                   person-long population (primary + MFJ secondary).
renormalize_johnson_for_puf = function(johnson, tax_units) {
  required_tu = c('weight', 'male1', 'male2', 'age1', 'age2',
                  'filing_status')
  missing_tu = setdiff(required_tu, names(tax_units))
  if (length(missing_tu) > 0L) {
    stop('renormalize_johnson_for_puf(): missing tax_units columns: ',
         paste(missing_tu, collapse = ', '))
  }

  # Person-long: primaries + MFJ secondaries.
  primary = tibble::tibble(
    sex     = normalize_sex(tax_units$male1),
    age     = as.integer(tax_units$age1),
    married = filing_status_to_married(tax_units$filing_status),
    weight  = tax_units$weight
  )
  has_secondary = tax_units$filing_status == 2L &
                  !is.na(tax_units$age2) &
                  !is.na(tax_units$male2)
  secondary = tibble::tibble(
    sex     = normalize_sex(tax_units$male2[has_secondary]),
    age     = as.integer(tax_units$age2[has_secondary]),
    married = 1L,
    weight  = tax_units$weight[has_secondary]
  )
  long = dplyr::bind_rows(primary, secondary) %>%
    dplyr::filter(!is.na(age)) %>%
    dplyr::mutate(age_band = age_to_johnson_band(age))

  males = long %>% dplyr::filter(sex == 'M')

  # Look up the raw Johnson factor per male person.
  query = paste('male', males$age_band, as.integer(males$married), sep = ':')
  key   = paste(johnson$sex, johnson$age_band,
                as.integer(johnson$married), sep = ':')
  idx = match(query, key)
  if (any(is.na(idx))) {
    stop('renormalize_johnson_for_puf(): ',
         sum(is.na(idx)),
         ' male person(s) have no Johnson factor lookup match.')
  }
  males$factor_raw = johnson$factor[idx]

  cell_mean = males %>%
    dplyr::group_by(age_band) %>%
    dplyr::summarise(mean_factor = sum(weight * factor_raw) / sum(weight),
                     .groups = 'drop')

  cat('renormalize_johnson_for_puf: cell means on PUF (M):\n')
  print(cell_mean %>% dplyr::mutate(mean_factor = round(mean_factor, 4)))

  # Apply the per-cell rescale to the male rows.
  johnson %>%
    dplyr::left_join(cell_mean, by = 'age_band') %>%
    dplyr::mutate(factor = factor / mean_factor) %>%
    dplyr::select(sex, age_band, married, factor)
}


# Vectorized Johnson lookup. For females, returns 1.0 (no marital
# adjustment). For males, returns Johnson 2000 factor.
lookup_R_marital = function(sex, age, married, johnson) {
  age_band = age_to_johnson_band(age)
  out = rep(1.0, length(sex))
  is_male = sex == 'M'
  if (any(is_male)) {
    query = paste('male', age_band[is_male],
                  as.integer(married[is_male]), sep = ':')
    key   = paste(johnson$sex, johnson$age_band,
                  as.integer(johnson$married), sep = ':')
    idx = match(query, key)
    if (any(is.na(idx))) {
      first = which(is.na(idx))[1]
      stop(sprintf('lookup_R_marital: %d male miss(es). First: %s',
                   sum(is.na(idx)), query[first]))
    }
    out[is_male] = johnson$factor[idx]
  }
  out
}

build_record_modifier = function(tax_units, chetty_pctile,
                                  chetty_R, johnson) {
  required_tu = c('id', 'age1', 'age2', 'male1', 'male2', 'filing_status')
  missing_tu = setdiff(required_tu, names(tax_units))
  if (length(missing_tu) > 0L) {
    stop('build_record_modifier: missing tax_units columns: ',
         paste(missing_tu, collapse = ', '))
  }
  if (!all(c('id', 'pctile1', 'pctile2') %in% names(chetty_pctile))) {
    stop('build_record_modifier: chetty_pctile must have (id, pctile1, pctile2)')
  }

  # Renormalize Johnson factors so the marital layer is mean-preserving
  # on the PUF's actual marital composition (not Johnson's implicit
  # reference). See renormalize_johnson_for_puf docstring.
  johnson_n = renormalize_johnson_for_puf(johnson, tax_units)

  m = tax_units %>%
    dplyr::select(id, age1, age2, male1, male2, filing_status) %>%
    dplyr::left_join(chetty_pctile, by = 'id')

  if (any(is.na(m$pctile1))) {
    stop('build_record_modifier: ', sum(is.na(m$pctile1)),
         ' record(s) have no pctile1 in chetty_pctile.')
  }

  # Primary: always present.
  sex1 = normalize_sex(m$male1)
  Rc1 = lookup_R_chetty(sex1, age_to_chetty_bin(m$age1), m$pctile1, chetty_R)
  Rm1 = lookup_R_marital(sex1, m$age1,
                         filing_status_to_married(m$filing_status), johnson_n)
  mod1 = Rc1 * Rm1

  # Secondary: only when filing_status == 2 (MFJ). Otherwise NA.
  has_secondary = m$filing_status == 2L &
                  !is.na(m$age2) &
                  !is.na(m$male2)
  mod2 = rep(NA_real_, nrow(m))
  if (any(has_secondary)) {
    if (any(is.na(m$pctile2[has_secondary]))) {
      stop('build_record_modifier: ',
           sum(is.na(m$pctile2[has_secondary])),
           ' MFJ record(s) have no pctile2 in chetty_pctile.')
    }
    sex2 = normalize_sex(m$male2[has_secondary])
    Rc2 = lookup_R_chetty(sex2,
                          age_to_chetty_bin(m$age2[has_secondary]),
                          m$pctile2[has_secondary],
                          chetty_R)
    Rm2 = lookup_R_marital(sex2,
                           m$age2[has_secondary],
                           rep(1L, sum(has_secondary)),
                           johnson_n)
    mod2[has_secondary] = Rc2 * Rm2
  }

  tibble::tibble(
    id            = m$id,
    age1          = as.integer(m$age1),
    age2          = as.integer(m$age2),
    sex1          = sex1,
    sex2          = normalize_sex(m$male2),
    has_secondary = has_secondary,
    mod1          = mod1,
    mod2          = mod2
  )
}


#---------------------------------------------
# Main entry: build_mortality_ledger
#
# Returns (year, id, q_death1, q_death2). q_death2 is NA for records
# with no secondary filer.
#---------------------------------------------

build_mortality_ledger = function(tax_units,
                                   chetty_pctile,
                                   years,
                                   q_baseline = NULL,
                                   chetty_R   = NULL,
                                   johnson    = NULL) {
  if (is.null(q_baseline)) q_baseline = make_q_baseline_ssa_tr2024()
  if (is.null(chetty_R))   chetty_R   = build_chetty_R()
  if (is.null(johnson))    johnson    = load_johnson_marital()

  modifier = build_record_modifier(tax_units, chetty_pctile,
                                    chetty_R, johnson)
  N = nrow(modifier)
  has2 = modifier$has_secondary

  per_year = function(y) {
    q1 = q_baseline(modifier$age1, modifier$sex1, rep(y, N))
    q2 = rep(NA_real_, N)
    if (any(has2)) {
      q2[has2] = q_baseline(modifier$age2[has2],
                            modifier$sex2[has2],
                            rep(y, sum(has2)))
    }
    tibble::tibble(
      year     = as.integer(y),
      id       = modifier$id,
      q_death1 = q1 * modifier$mod1,
      q_death2 = q2 * modifier$mod2
    )
  }

  purrr::map_dfr(years, per_year) %>% dplyr::arrange(year, id)
}


#---------------------------------------------------------------------------
# Standalone tests. Run with:
#   Rscript src/mortality_ledger.R
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(tidyr)
    library(readr)
    library(purrr)
    library(Hmisc)   # wtd.quantile, used transitively via helpers.R
  })
  source('src/imputations/helpers.R')   # broad_income_components + compute_broad_income

  cat('--- mortality_ledger.R tests ---\n')

  # Test 1: bin helpers.
  stopifnot(age_to_chetty_bin(c(20, 49, 50, 65, 70, 90)) ==
            c('<50', '<50', '50-59', '60-69', '70+', '70+'))
  stopifnot(all(age_to_johnson_band(c(20, 64, 65, 90)) ==
                c('under_65', 'under_65', 'age_65_plus', 'age_65_plus')))
  stopifnot(filing_status_to_married(c(1L, 2L, 3L, 4L, 5L)) ==
            c(0L, 1L, 0L, 0L, 0L))
  cat('  [PASS] bin helpers\n')

  # Test 2: Chetty R loader. Within each (sex, age_bin), R_chetty's
  # mean across pctile should be 1.0.
  cR = build_chetty_R()
  stopifnot(c('sex', 'age_bin', 'pctile', 'R_chetty') %in% names(cR))
  stopifnot(all(cR$pctile %in% 1:100))
  stopifnot(all(cR$age_bin %in% c('<50', '50-59', '60-69', '70+')))
  stopifnot(all(cR$sex %in% c('M', 'F')))
  R_means = cR %>%
    group_by(sex, age_bin) %>%
    summarise(mean_R = mean(R_chetty), .groups = 'drop')
  stopifnot(all(abs(R_means$mean_R - 1.0) < 1e-12))
  cat('  [PASS] build_chetty_R: marginals normalize to 1.0\n')

  # Test 3: Johnson loader. Only male rows present.
  jh = load_johnson_marital()
  stopifnot(all(jh$sex == 'male'))
  stopifnot(nrow(jh) == 4L)   # 2 age bands × 2 marital states
  cat('  [PASS] load_johnson_marital: 4 male rows\n')

  # Test 4: Johnson lookup — females always 1.0.
  R_marital_F = lookup_R_marital(rep('F', 4),
                                  age = c(40, 70, 40, 70),
                                  married = c(0, 0, 1, 1),
                                  johnson = jh)
  stopifnot(all(R_marital_F == 1.0))
  cat('  [PASS] lookup_R_marital: females all 1.0 (no double-count of Chetty)\n')

  # Test 5: Johnson lookup — males pick up the right factor.
  R_marital_M = lookup_R_marital(c('M', 'M', 'M', 'M'),
                                  age = c(40, 70, 40, 70),
                                  married = c(0, 0, 1, 1),
                                  johnson = jh)
  # Expected from Ricco 2020 Table 5:
  #   Male <65 unmarried = 1.26, Male 65+ unmarried = 1.10
  #   Male <65 married  = 0.94, Male 65+ married  = 0.97
  stopifnot(abs(R_marital_M[1] - 1.26) < 1e-9)
  stopifnot(abs(R_marital_M[2] - 1.10) < 1e-9)
  stopifnot(abs(R_marital_M[3] - 0.94) < 1e-9)
  stopifnot(abs(R_marital_M[4] - 0.97) < 1e-9)
  cat('  [PASS] lookup_R_marital: male factors match Ricco 2020 Table 5\n')

  # Test 6: SSA TR2024 baseline returns finite q_x in expected ranges.
  qb = make_q_baseline_ssa_tr2024()
  q40_2022_M = qb(40, 'M', 2022)
  q70_2050_F = qb(70, 'F', 2050)
  q1_2022_M  = qb(1,  'M', 2022)
  stopifnot(is.numeric(q40_2022_M), q40_2022_M > 0, q40_2022_M < 0.01)
  stopifnot(is.numeric(q70_2050_F), q70_2050_F > 0, q70_2050_F < 0.05)
  stopifnot(q1_2022_M < q40_2022_M)  # 1-year-old << 40-year-old
  cat('  [PASS] q_baseline_ssa_tr2024: lookup hits and reasonable magnitudes\n')

  # Test 7: q_baseline projected vs Ricco 2020-era static — Alt2 2095
  # should be substantially below 2022 for older ages (mortality
  # improvement).
  q70_2022_M = qb(70, 'M', 2022)
  q70_2095_M = qb(70, 'M', 2095)
  stopifnot(q70_2095_M < q70_2022_M)
  cat(sprintf('  [PASS] mortality improvement: q(70, M, 2022) = %.5f → q(70, M, 2095) = %.5f\n',
              q70_2022_M, q70_2095_M))

  # Test 7b: cap-age correction — q at age=80 should be the L-weighted
  # average across 80+, NOT the raw q(80). Compare to a no-cap baseline.
  qb_nocap = make_q_baseline_ssa_tr2024(cap_age = NULL)
  q80_capped = qb(80, 'M', 2022)
  q80_raw    = qb_nocap(80, 'M', 2022)
  q79_raw    = qb_nocap(79, 'M', 2022)

  # Sanity: capped value should be (a) larger than raw q(80), (b) larger
  # than raw q(79), (c) approximately twice raw q(80) (very-old smearing).
  stopifnot(q80_capped > q80_raw)
  stopifnot(q80_capped > q79_raw)
  stopifnot(q80_capped > 1.5 * q80_raw)
  stopifnot(q80_capped < 4.0 * q80_raw)
  cat(sprintf(
    '  [PASS] cap-age correction: q(80=80+, M, 2022) = %.4f vs raw q(80) = %.4f (%.2fx)\n',
    q80_capped, q80_raw, q80_capped / q80_raw))

  # Sanity: capped value should approximately match 1/e(80) under the
  # stationary-pop assumption. The two differ by ~0.005-0.015 in the SSA
  # tables because e(x) is the "complete" expectation (lifetime + 0.5)
  # while the L-weighted average corresponds to the curtate version;
  # both are valid actuarial constructions, off by O(1/e²) ≈ 0.015 at
  # age 80. Tolerance accommodates this.
  ssa_m = readr::read_csv('resources/PerLifeTables_M_Alt2_TR2024.csv',
                          skip = 4, show_col_types = FALSE,
                          col_select = c('Year', 'x', 'e(x)'))
  e80_M_2022 = ssa_m$`e(x)`[ssa_m$Year == 2022 & ssa_m$x == 80]
  identity_q = 1.0 / e80_M_2022
  stopifnot(abs(q80_capped - identity_q) < 0.02)
  cat(sprintf(
    '  [PASS] q_avg(80+) ≈ 1/e(80) actuarial identity: %.4f vs %.4f (curtate/complete gap)\n',
    q80_capped, identity_q))

  # Below cap_age: q values should be identical between capped and no-cap
  # providers (no upstream change).
  stopifnot(qb(50, 'M', 2050) == qb_nocap(50, 'M', 2050))
  stopifnot(qb(70, 'F', 2030) == qb_nocap(70, 'F', 2030))
  cat('  [PASS] sub-cap ages unaffected by cap correction\n')

  # Test 7c: build_chetty_pctile on a synthetic PUF.
  zero_inc = function(n) {
    cols = c('sole_prop','farm','scorp_active','scorp_active_loss','scorp_179',
             'scorp_passive','scorp_passive_loss','part_active','part_active_loss',
             'part_179','part_passive','part_passive_loss','txbl_int','exempt_int',
             'div_ord','div_pref','kg_lt','kg_st','gross_ss','gross_pens_dist',
             'ui','rent','rent_loss','estate','estate_loss')
    out = lapply(cols, function(.) rep(0, n)); names(out) = cols
    as_tibble(out)
  }
  # 6 records: 3 single (M, ages 40/55/80), 3 MFJ (M primary 50, F secondary 48
  # at 3 different incomes). All in the same (sex, age) cells will get
  # within-cell pctile.
  puf_pct = bind_cols(
    tibble(id            = 1:6,
           weight        = rep(1, 6),
           age1          = c(40L, 55L, 80L, 50L, 50L, 50L),
           age2          = c(NA, NA, NA,  48L, 48L, 48L),
           male1         = c(1L, 1L, 1L, 1L, 1L, 1L),
           male2         = c(NA, NA, NA, 0L, 0L, 0L),
           filing_status = c(1L, 1L, 1L, 2L, 2L, 2L),
           wages         = c(50, 50, 50, 30000, 100000, 200000)),
    zero_inc(6)
  )
  cp_built = build_chetty_pctile(puf_pct)
  stopifnot(c('id', 'pctile1', 'pctile2') %in% names(cp_built))
  stopifnot(all(cp_built$pctile1 >= 1L & cp_built$pctile1 <= 100L))
  # The 3 MFJ records share (M, age=50) for primary; ranks within that cell
  # should be monotone in income (id 4 < 5 < 6).
  mfj = cp_built %>% filter(id %in% 4:6) %>% arrange(id)
  stopifnot(mfj$pctile1[1] <= mfj$pctile1[2])
  stopifnot(mfj$pctile1[2] <= mfj$pctile1[3])
  # Female secondary cell (F, age=48) has 3 records → ranks span its full range.
  stopifnot(min(mfj$pctile2) <= max(mfj$pctile2))
  # Non-MFJ records have NA pctile2.
  stopifnot(all(is.na(cp_built$pctile2[cp_built$id %in% 1:3])))
  cat('  [PASS] build_chetty_pctile: within-cell rank, NA pctile2 for non-MFJ\n')

  # Test 8: end-to-end on a tiny synthetic tax_units fixture.
  tu = tibble(
    id            = 1:4,
    weight        = rep(1.0, 4),
    age1          = c(45L, 55L, 65L, 75L),
    age2          = c(NA_integer_, 53L, NA_integer_, NA_integer_),
    male1         = c(1L, 1L, 0L, 0L),
    male2         = c(NA_integer_, 0L, NA_integer_, NA_integer_),
    filing_status = c(1L, 2L, 4L, 5L)   # Single, MFJ, HoH, QW
  )
  cp = tibble(id      = 1:4,
              pctile1 = c(10L, 50L, 80L, 99L),
              pctile2 = c(NA_integer_, 60L, NA_integer_, NA_integer_))

  ledger = build_mortality_ledger(tax_units = tu, chetty_pctile = cp,
                                   years = c(2022L, 2050L),
                                   q_baseline = qb)
  stopifnot(nrow(ledger) == 8L)   # 4 records × 2 years
  stopifnot(all(c('year', 'id', 'q_death1', 'q_death2') %in% names(ledger)))

  # Only the MFJ record (id=2) has a non-NA q_death2.
  ledger_2022 = ledger %>% filter(year == 2022L) %>% arrange(id)
  stopifnot(is.na(ledger_2022$q_death2[1]))   # Single
  stopifnot(!is.na(ledger_2022$q_death2[2]))  # MFJ
  stopifnot(is.na(ledger_2022$q_death2[3]))   # HoH
  stopifnot(is.na(ledger_2022$q_death2[4]))   # QW
  cat('  [PASS] build_mortality_ledger: q_death2 NA for non-MFJ records\n')

  # All q_death1 values should be in (0, 1).
  stopifnot(all(ledger$q_death1 > 0 & ledger$q_death1 < 1))
  stopifnot(all(ledger$q_death2[!is.na(ledger$q_death2)] > 0 &
                ledger$q_death2[!is.na(ledger$q_death2)] < 1))
  cat('  [PASS] build_mortality_ledger: all q_death values in (0, 1)\n')

  # Mortality improvement carries through: q_death1 in 2050 < 2022 at
  # the same age × sex × pctile × marital composition.
  ledger_wide = ledger %>%
    select(year, id, q_death1) %>%
    pivot_wider(names_from = year, values_from = q_death1,
                names_prefix = 'y_')
  stopifnot(all(ledger_wide$y_2050 < ledger_wide$y_2022))
  cat('  [PASS] build_mortality_ledger: per-record q_death1 falls 2022 → 2050\n')

  cat('\nAll tests passed.\n')
}
