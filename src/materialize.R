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
# optionally (5) bucketed_factors + record_bucket —
#                     per-(year, variable, bucket)
#                     growth factors for variables
#                     whose growth varies by a
#                     per-record bucket (e.g. DFA
#                     income percentile for wealth).
#
# Pure function, no side effects. Called once
# per target year in main.R's Phase 4, and once
# per module's base_year in Phase 3 (to hand
# the module a year-appropriate PUF).
#--------------------------------------


# Fail loudly when a bucketed lookup yields NA. Distinguishes the two
# failure modes (record has no bucket vs. record's bucket is not in the
# ledger) so the caller can fix the right object.
.report_bucket_na = function(year, variable, rec_buckets, ledger_buckets) {
  unassigned     = is.na(rec_buckets)
  missing_bucket = !unassigned & !(rec_buckets %in% ledger_buckets)
  msg = sprintf(
    'materialize(): bucketed factor lookup returned NA for (year=%d, variable=%s). ',
    year, variable)
  if (any(unassigned)) {
    msg = paste0(msg, sprintf(
      '%d record(s) have no bucket assignment in record_bucket. ',
      sum(unassigned)))
  }
  if (any(missing_bucket)) {
    bad = unique(rec_buckets[missing_bucket])
    msg = paste0(msg, sprintf(
      '%d record(s) have bucket(s) not present in bucketed_factors: %s.',
      sum(missing_bucket), paste(bad, collapse = ', ')))
  }
  stop(msg, call. = FALSE)
}


#' Materialize the PUF tibble at a given year.
#'
#' Semantics:
#'   1. Start with `base`. All module-imputed columns are expected to be
#'      present in `base` as NA placeholders.
#'   2. For each variable appearing in `factor_ledger` and present in `base`,
#'      multiply by the cumulative factor from 2017+1..target_year.
#'   3. For each variable appearing in `bucketed_factors` (optional sibling
#'      ledger with a per-record-bucket dimension), multiply by the
#'      record-specific cumulative factor. Bucket is resolved via
#'      `record_bucket`. A (year, variable) pair must appear in exactly ONE
#'      of factor_ledger / bucketed_factors — overlap errors.
#'   4. If `weight_ledger` is provided, overwrite `weight` with that year's
#'      record weights.
#'   5. For each module in `module_deltas` whose base_year ≤ target_year:
#'        - Age the module's column values from its base_year to target_year
#'          via factor_ledger AND/OR bucketed_factors.
#'        - Overwrite (not left_join) the module's columns in `out`.
#'      Modules whose base_year > target_year are skipped; those columns
#'      remain NA from the base initialization.
#'
#' @param target_year      Integer; the year to materialize.
#' @param base             Tibble with `id` column + 2017 values + NA
#'                         placeholders for all module-imputed variables.
#' @param factor_ledger    Tibble with columns (year, variable, factor,
#'                         source). `factor` is CUMULATIVE from each
#'                         variable's base_year to `year`. Absence of
#'                         (year, variable) means factor = 1.
#' @param weight_ledger    Optional tibble with (year, id, weight). NULL
#'                         skips weight override.
#' @param module_deltas    Named list of the form
#'                         list(<name> = list(base_year, values)), where
#'                         `values` is a tibble with `id` + that module's
#'                         imputed columns at base_year.
#' @param bucketed_factors Optional tibble with (year, variable, bucket,
#'                         factor, source). For variables whose growth
#'                         varies by a per-record bucket (e.g. DFA income
#'                         percentile). Each row's factor is CUMULATIVE
#'                         from the variable's base_year. Strict: when a
#'                         variable appears at year Y, every bucket
#'                         present in `record_bucket` must have a
#'                         corresponding row at (Y, variable, bucket);
#'                         an unmatched lookup errors rather than
#'                         silently defaulting. Variables absent at year
#'                         Y are untouched — "absence = no-op" applies
#'                         at the variable level, not the bucket level.
#' @param record_bucket    Required iff `bucketed_factors` non-NULL.
#'                         Tibble with (id, bucket), one row per record
#'                         in `base`. Bucket labels must match the
#'                         `bucket` column in `bucketed_factors`.
#' @return                 Tibble of the same row shape as `base` with
#'                         values at `target_year`.
materialize = function(target_year,
                       base,
                       factor_ledger,
                       weight_ledger = NULL,
                       module_deltas = list(),
                       bucketed_factors = NULL,
                       record_bucket = NULL) {

  stopifnot(is.data.frame(base))
  stopifnot('id' %in% names(base))
  stopifnot(is.data.frame(factor_ledger))
  stopifnot(all(c('year', 'variable', 'factor') %in% names(factor_ledger)))
  stopifnot(is.list(module_deltas))

  if (!is.null(bucketed_factors)) {
    stopifnot(is.data.frame(bucketed_factors))
    stopifnot(all(c('year', 'variable', 'bucket', 'factor') %in%
                  names(bucketed_factors)))
    stopifnot(!is.null(record_bucket))
    stopifnot(is.data.frame(record_bucket))
    stopifnot(all(c('id', 'bucket') %in% names(record_bucket)))

    # Invariant: a (year, variable) pair must live in at most one ledger.
    fl_key = paste(factor_ledger$year, factor_ledger$variable)
    bf_key = paste(bucketed_factors$year, bucketed_factors$variable)
    overlap = intersect(fl_key, bf_key)
    if (length(overlap) > 0L) {
      stop('materialize(): (year, variable) pairs appear in both ',
           'factor_ledger and bucketed_factors: ',
           paste(head(unique(overlap), 5), collapse = '; '))
    }
  }

  out = base

  # Per-record bucket vector (NULL if no bucketed_factors supplied).
  rec_buckets = if (!is.null(bucketed_factors)) {
    record_bucket$bucket[match(out$id, record_bucket$id)]
  } else NULL

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
  # (1b) Apply per-record bucketed factors to base-native variables.
  # Strict: every record must have a bucket, and every assigned bucket must
  # have a factor row at this (year, variable). NAs are a schema bug, not a
  # no-op — surface them rather than silently coalescing to 1.
  # ---------------------------------------------------------------------------

  if (!is.null(bucketed_factors)) {
    bf_year = bucketed_factors[bucketed_factors$year == target_year, ]
    for (v in intersect(unique(bf_year$variable), names(base))) {
      v_rows  = bf_year[bf_year$variable == v, ]
      factors = v_rows$factor[match(rec_buckets, v_rows$bucket)]
      if (any(is.na(factors))) {
        .report_bucket_na(target_year, v, rec_buckets, v_rows$bucket)
      }
      out[[v]] = out[[v]] * factors
    }
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

      # Per-record bucketed path for module vars. Strict on NAs (see 1b).
      if (!is.null(bucketed_factors)) {
        bf_m_year = bucketed_factors[
          bucketed_factors$year == target_year &
          bucketed_factors$variable %in% m_vars,
        ]
        if (nrow(bf_m_year) > 0L) {
          m_rec_buckets = record_bucket$bucket[match(m_values$id,
                                                     record_bucket$id)]
          for (v in unique(bf_m_year$variable)) {
            v_rows  = bf_m_year[bf_m_year$variable == v, ]
            factors = v_rows$factor[match(m_rec_buckets, v_rows$bucket)]
            if (any(is.na(factors))) {
              .report_bucket_na(target_year, v, m_rec_buckets, v_rows$bucket)
            }
            m_values[[v]] = m_values[[v]] * factors
          }
        }
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

  # Bucketed-factor fixtures. 3 records × 3 buckets. `cash` moved out of the
  # uniform ledger into the bucketed one (no-overlap invariant).
  record_bucket = data.frame(
    id     = 1:3,
    bucket = c('low', 'mid', 'high'),
    stringsAsFactors = FALSE
  )
  bucketed_factors = data.frame(
    year     = rep(2023L:2024L, each = 3L),
    variable = 'cash',
    bucket   = rep(c('low', 'mid', 'high'), times = 2L),
    factor   = c(1.05, 1.08, 1.12,       # 2023 cumulative from 2022
                 1.10, 1.17, 1.25),      # 2024 cumulative from 2022
    source   = 'DFA:cash',
    stringsAsFactors = FALSE
  )
  factor_ledger_no_cash = factor_ledger[factor_ledger$variable != 'cash', ]

  # Test 9: bucketed factor applied per-record to a MODULE col.
  r23b = materialize(2023L, base_puf, factor_ledger_no_cash, NULL,
                     module_deltas, bucketed_factors, record_bucket)
  # cash at 2022 = c(1000, 2000, 3000); per-bucket multiplied:
  stopifnot(max(abs(r23b$cash - c(1000*1.05, 2000*1.08, 3000*1.12))) < 1e-10)
  cat('  [PASS] bucketed factor applied per-record to module col\n')

  # Test 10: bucketed factor applied per-record to a BASE-NATIVE col.
  base_bn = data.frame(id = 1:3, wp = c(1000, 2000, 3000), cash = NA_real_)
  bf_bn = data.frame(
    year = 2023L, variable = 'wp',
    bucket = c('low', 'mid', 'high'),
    factor = c(1.05, 1.08, 1.12),
    source = 'DFA:wp',
    stringsAsFactors = FALSE
  )
  r_bn = materialize(2023L, base_bn, factor_ledger_no_cash, NULL,
                     list(), bf_bn, record_bucket)
  stopifnot(max(abs(r_bn$wp - c(1050, 2160, 3360))) < 1e-10)
  cat('  [PASS] bucketed factor applied per-record to base-native col\n')

  # Test 11: (year, variable) in both ledgers → error.
  err_msg = tryCatch(
    materialize(2023L, base_puf, factor_ledger, NULL,
                module_deltas, bucketed_factors, record_bucket),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl('appear in both', err_msg))
  cat('  [PASS] overlap between factor_ledger and bucketed_factors errors\n')

  # Test 12a: record with a bucket not present in the ledger → error.
  record_bucket_unknown = data.frame(
    id     = 1:3,
    bucket = c('low', 'mid', 'unknown'),
    stringsAsFactors = FALSE
  )
  err_unk = tryCatch(
    materialize(2023L, base_puf, factor_ledger_no_cash, NULL,
                module_deltas, bucketed_factors, record_bucket_unknown),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl('not present in bucketed_factors', err_unk))
  stopifnot(grepl('unknown', err_unk))
  cat('  [PASS] bucket not in ledger errors with useful message\n')

  # Test 12b: record missing from record_bucket entirely → error.
  record_bucket_partial = data.frame(
    id     = 1:2,
    bucket = c('low', 'mid'),
    stringsAsFactors = FALSE
  )
  err_part = tryCatch(
    materialize(2023L, base_puf, factor_ledger_no_cash, NULL,
                module_deltas, bucketed_factors, record_bucket_partial),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl('no bucket assignment', err_part))
  cat('  [PASS] record missing from record_bucket errors\n')

  # Test 12c: NA in the factor column → error (not silently skipped).
  bf_with_na = bucketed_factors
  bf_with_na$factor[1] = NA_real_
  err_nafac = tryCatch(
    materialize(2023L, base_puf, factor_ledger_no_cash, NULL,
                module_deltas, bf_with_na, record_bucket),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl('NA', err_nafac))
  cat('  [PASS] NA in factor column errors\n')

  # Test 13: bucketed_factors = NULL is byte-identical to the old 5-arg call.
  r24_old = materialize(2024L, base_puf, factor_ledger, NULL, module_deltas)
  r24_new = materialize(2024L, base_puf, factor_ledger, NULL, module_deltas,
                        bucketed_factors = NULL, record_bucket = NULL)
  stopifnot(identical(r24_old, r24_new))
  cat('  [PASS] bucketed_factors=NULL preserves prior behavior\n')

  # Test 14: cumulative compose — materialize(2024) with bucketed cash ages
  # the module col by the 2024 per-bucket factor (not a y/y cumprod).
  r24b = materialize(2024L, base_puf, factor_ledger_no_cash, NULL,
                     module_deltas, bucketed_factors, record_bucket)
  stopifnot(max(abs(r24b$cash - c(1000*1.10, 2000*1.17, 3000*1.25))) < 1e-10)
  cat('  [PASS] bucketed factor at 2024 is cumulative, not y/y\n')

  cat('\nAll tests passed.\n')
}
