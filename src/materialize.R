#--------------------------------------
# materialize.R
#
# Reconstructs a year-indexed PUF tibble
# from three persistent objects:
#   (1) base       — the 2017 tibble
#   (2) factor_ledger — per-(year, variable)
#                     multiplicative growth factors
#   (3) module_deltas — per-module base-year
#                     imputed columns
# optionally (4) weight_ledger — per-(year, id)
#                     record weights.
#
# Pure function, no side effects. Called once
# per target year in main.R's Phase 4, and once
# per module's base_year in Phase 3 (to hand
# the module a year-appropriate PUF).
#--------------------------------------


#' Materialize the PUF tibble at a given year.
#'
#' Semantics:
#'   1. Start with `base`. All module-imputed columns are expected to be
#'      present in `base` as NA placeholders.
#'   2. For each variable appearing in `factor_ledger` and present in `base`,
#'      multiply by the cumulative factor from 2017+1..target_year.
#'   3. If `weight_ledger` is provided, overwrite `weight` with that year's
#'      record weights.
#'   4. For each module in `module_deltas` whose base_year ≤ target_year:
#'        - Age the module's column values from its base_year to target_year
#'          via factor_ledger.
#'        - Overwrite (not left_join) the module's columns in `out`.
#'      Modules whose base_year > target_year are skipped; those columns
#'      remain NA from the base initialization.
#'
#' @param target_year    Integer; the year to materialize.
#' @param base           Tibble with `id` column + 2017 values + NA
#'                       placeholders for all module-imputed variables.
#' @param factor_ledger  Tibble with columns (year, variable, factor,
#'                       source). `factor` is CUMULATIVE from each
#'                       variable's base_year to `year` (not year-over-year).
#'                       Absence of (year, variable) means factor = 1.
#' @param weight_ledger  Optional tibble with (year, id, weight). NULL skips
#'                       weight override.
#' @param module_deltas  Named list of the form
#'                       list(<name> = list(base_year, values)), where
#'                       `values` is a tibble with `id` + that module's
#'                       imputed columns at base_year.
#' @return               Tibble of the same row shape as `base` with values
#'                       at `target_year`.
materialize = function(target_year,
                       base,
                       factor_ledger,
                       weight_ledger = NULL,
                       module_deltas = list()) {

  stopifnot(is.data.frame(base))
  stopifnot('id' %in% names(base))
  stopifnot(is.data.frame(factor_ledger))
  stopifnot(all(c('year', 'variable', 'factor') %in% names(factor_ledger)))
  stopifnot(is.list(module_deltas))

  out = base

  # ---------------------------------------------------------------------------
  # (1) Apply cumulative factors to base-native variables.
  # Base-native means: present in `base` with non-NA values. Module-imputed
  # columns are NA here and handled in step (4). Factor is already cumulative
  # from 2017 — just look up and multiply.
  # ---------------------------------------------------------------------------

  fl_year = factor_ledger[factor_ledger$year == target_year, ]
  base_vars_with_factors = intersect(fl_year$variable, names(base))
  for (v in base_vars_with_factors) {
    cf = fl_year$factor[match(v, fl_year$variable)]
    out[[v]] = out[[v]] * cf
  }

  # ---------------------------------------------------------------------------
  # (2) Apply weight_ledger if provided.
  # ---------------------------------------------------------------------------

  if (!is.null(weight_ledger)) {
    stopifnot(is.data.frame(weight_ledger))
    stopifnot(all(c('year', 'id', 'weight') %in% names(weight_ledger)))
    wl = weight_ledger[weight_ledger$year == target_year, c('id', 'weight')]
    if (nrow(wl) > 0L) {
      out$weight = wl$weight[match(out$id, wl$id)]
    }
  }

  # ---------------------------------------------------------------------------
  # (3) Attach + age module deltas.
  # ---------------------------------------------------------------------------

  for (mname in names(module_deltas)) {
    m = module_deltas[[mname]]
    stopifnot(is.list(m), all(c('base_year', 'values') %in% names(m)))
    stopifnot(is.data.frame(m$values), 'id' %in% names(m$values))

    # Module not yet active — base NA placeholders persist.
    if (target_year < m$base_year) next

    m_vars = setdiff(names(m$values), 'id')
    m_values = m$values

    # Apply cumulative factor (from m$base_year to target_year) per module var.
    # Factor is already cumulative — just look up (year == target_year).
    if (target_year > m$base_year) {
      fl_m_year = factor_ledger[
        factor_ledger$year == target_year &
        factor_ledger$variable %in% m_vars,
      ]
      for (i in seq_len(nrow(fl_m_year))) {
        v  = fl_m_year$variable[i]
        cf = fl_m_year$factor[i]
        m_values[[v]] = m_values[[v]] * cf
      }
    }

    row_match = match(out$id, m_values$id)
    for (v in m_vars) {
      out[[v]] = m_values[[v]][row_match]
    }
  }

  out
}


#---------------------------------------------------------------------------
# Standalone tests. Run with:
#   Rscript src/materialize.R
# Exits 0 on success, non-zero on test failure. Not part of the pipeline.
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  # Fixtures.
  base_puf = data.frame(
    id     = 1:3,
    wages  = c(100, 200, 300),
    weight = c(10,   20,  30),
    # module-imputed placeholder:
    cash   = NA_real_
  )

  # Factor ledger stores CUMULATIVE factors from each variable's base_year.
  # wages: base 2017, grows 5%/year → factor at year Y = 1.05^(Y-2017).
  # cash: base 2022, grows 10%/year → factor at year Y = 1.10^(Y-2022).
  years_post_2017 = 2018:2024
  years_post_2022 = 2023:2024
  factor_ledger = data.frame(
    year     = c(years_post_2017, years_post_2022),
    variable = c(rep('wages', length(years_post_2017)),
                 rep('cash',  length(years_post_2022))),
    factor   = c(1.05 ^ (years_post_2017 - 2017L),
                 1.10 ^ (years_post_2022 - 2022L)),
    source   = c(rep('macro:wages', length(years_post_2017)),
                 rep('DFA:cash',    length(years_post_2022))),
    stringsAsFactors = FALSE
  )

  weight_ledger = data.frame(
    year   = rep(2017:2022, each = 3),
    id     = rep(1:3, 6),
    weight = c(c(10, 20, 30),                  # 2017
               c(11, 21, 31),                  # 2018
               c(12, 22, 32),                  # 2019
               c(13, 23, 33),                  # 2020
               c(14, 24, 34),                  # 2021
               c(15, 25, 35))                  # 2022
  )

  wealth_delta = data.frame(
    id   = 1:3,
    cash = c(1000, 2000, 3000)
  )
  module_deltas = list(wealth = list(base_year = 2022, values = wealth_delta))

  cat('--- materialize.R tests ---\n')

  # Test 1: materialize(2017) preserves base on non-module cols, NA on module
  # cols; weight overridden by weight_ledger (here equal to base).
  r17 = materialize(2017L, base_puf, factor_ledger, weight_ledger,
                    module_deltas)
  stopifnot(identical(r17$wages, c(100, 200, 300)))
  stopifnot(all(is.na(r17$cash)))
  stopifnot(identical(r17$weight, c(10, 20, 30)))
  cat('  [PASS] materialize(2017) identity on base, NA on module cols\n')

  # Test 2: materialize(2020) ages wages by 1.05^3.
  r20 = materialize(2020L, base_puf, factor_ledger, weight_ledger,
                    module_deltas)
  expected_wages = c(100, 200, 300) * 1.05^3
  stopifnot(max(abs(r20$wages - expected_wages)) < 1e-10)
  stopifnot(all(is.na(r20$cash)))
  stopifnot(identical(r20$weight, c(13, 23, 33)))
  cat('  [PASS] materialize(2020) ages wages 3 years; cash still NA\n')

  # Test 3: materialize(2022) is the module's base year; cash values attached
  # unaged; wages aged 5 years from 2017.
  r22 = materialize(2022L, base_puf, factor_ledger, weight_ledger,
                    module_deltas)
  stopifnot(max(abs(r22$wages - c(100,200,300) * 1.05^5)) < 1e-10)
  stopifnot(identical(r22$cash, c(1000, 2000, 3000)))
  cat('  [PASS] materialize(2022) attaches cash unaged; wages aged 5yrs\n')

  # Test 4: materialize(2024) ages wages 7yrs total, cash 2yrs from 2022.
  r24 = materialize(2024L, base_puf, factor_ledger, NULL, module_deltas)
  expected_wages_24 = c(100, 200, 300) * 1.05^7
  expected_cash_24  = c(1000, 2000, 3000) * 1.10^2
  stopifnot(max(abs(r24$wages - expected_wages_24)) < 1e-10)
  stopifnot(max(abs(r24$cash  - expected_cash_24))  < 1e-10)
  cat('  [PASS] materialize(2024) cumulative factors applied correctly\n')

  # Test 5: empty module_deltas — cash stays NA at every year.
  r24b = materialize(2024L, base_puf, factor_ledger, NULL, list())
  stopifnot(all(is.na(r24b$cash)))
  cat('  [PASS] empty module_deltas leaves module cols NA\n')

  # Test 6: idempotence — calling twice returns the same thing.
  r22a = materialize(2022L, base_puf, factor_ledger, weight_ledger,
                     module_deltas)
  r22b = materialize(2022L, base_puf, factor_ledger, weight_ledger,
                     module_deltas)
  stopifnot(identical(r22a, r22b))
  cat('  [PASS] idempotence\n')

  # Test 7: module_deltas order invariance on disjoint cols.
  base_puf2 = cbind(base_puf, other = NA_real_)
  d_other = data.frame(id = 1:3, other = c(7, 8, 9))
  md1 = list(wealth = list(base_year = 2022, values = wealth_delta),
             extra  = list(base_year = 2022, values = d_other))
  md2 = list(extra  = list(base_year = 2022, values = d_other),
             wealth = list(base_year = 2022, values = wealth_delta))
  r_a = materialize(2023L, base_puf2, factor_ledger, NULL, md1)
  r_b = materialize(2023L, base_puf2, factor_ledger, NULL, md2)
  stopifnot(identical(r_a, r_b))
  cat('  [PASS] module_deltas order invariance on disjoint cols\n')

  # Test 8: variable with no factor_ledger entry at year Y stays at base.
  base_no_grow = cbind(base_puf, flag = c(1L, 0L, 1L))
  r_ng = materialize(2024L, base_no_grow, factor_ledger, NULL, list())
  stopifnot(identical(r_ng$flag, c(1L, 0L, 1L)))
  cat('  [PASS] variables without factor_ledger entries untouched\n')

  cat('\nAll tests passed.\n')
}
