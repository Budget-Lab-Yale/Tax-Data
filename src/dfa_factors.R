#---------------------------------------------
# dfa_factors.R
#
# Builds bucketed_factor_ledger for the 23
# wealth Y-vars, covering 2023..2097.
#
#   2023..last_dfa_year  — per-bucket growth in
#                          per-household average
#                          wealth, derived from
#                          DFA detail categories.
#   next_year..2097      — each bucket's final-
#                          DFA cumulative extended
#                          uniformly by per-household
#                          GDP growth.
#
# Per-household = (bucket dollars / bucket weight).
# Strips population growth so there's no double
# count with weight_ledger in materialize().
#
# Schema-compatible with materialize()'s
# bucketed_factors arg: (year, variable, bucket,
# factor, source). `factor` is CUMULATIVE from
# 2022 to `year`.
#---------------------------------------------


source('src/imputations/wealth_schema.R')   # wealth_y_vars etc.


# DFA bucket labels as they appear in dfa_totals.csv (income slice).
# Must match dfa_income_buckets in src/record_bucket.R.
DFA_INCOME_BUCKETS = c('pct00to20', 'pct20to40', 'pct40to60',
                       'pct60to80', 'pct80to99', 'pct99to100')


#' Resolve the list of DFA detail categories for each y_var.
#'
#' Parses crosswalk$dfa_variable by splitting on ' + ', stripping
#' '(residual after ...)' annotations, and returning a per-y_var list of
#' categories. Derived (kg_*) and none (trusts) return NULL — those are
#' filled via inheritance after the main factor computation.
#'
#' @return Named list: element name is y_var, value is character vector
#'         of DFA category names (or NULL for inherited y_vars).
resolve_dfa_categories = function(crosswalk) {
  clean = function(s) sub(' \\(residual after[^)]*\\)$', '', s)
  out = lapply(crosswalk$dfa_variable, function(s) {
    if (is.na(s) || s == '') return(NULL)
    parts = strsplit(s, ' + ', fixed = TRUE)[[1]]
    vapply(parts, clean, character(1), USE.NAMES = FALSE)
  })
  names(out) = crosswalk$y_var
  out
}


#' Per-household growth factor (cumulative from 2022) for one y_var,
#' one bucket, across DFA-covered years.
#'
#' Mechanic:
#'   factor(b, Y) = [D(b, Y) / W(b, Y)] / [D(b, 2022) / W(b, 2022)]
#' where D is the sum of `categories` dollars at (bucket b, year Y) and
#' W is the total weight_ledger weight at (bucket b, year Y).
#'
#' @param categories  Character vector; DFA category names to sum.
#' @param dfa_long    Tibble (bucket, year, category, dollars), pre-
#'                    filtered to detail / income / Q4.
#' @param W_by_by     Tibble (bucket, year, W); total weights by bucket.
#' @param years       Integer vector of target years (2023..last_dfa_year).
#' @param base_year   Integer, reference year (2022).
#' @return            Tibble (year, bucket, factor).
dfa_factor_one_var = function(categories, dfa_long, W_by_by, years, base_year) {
  D = dfa_long %>%
    filter(category %in% categories) %>%
    group_by(bucket, year) %>%
    summarise(D = sum(dollars), .groups = 'drop')

  D_base = D %>% filter(year == base_year) %>% select(bucket, D_base = D)
  W_base = W_by_by %>% filter(year == base_year) %>% select(bucket, W_base = W)

  D %>%
    filter(year %in% years) %>%
    inner_join(W_by_by, by = c('bucket', 'year')) %>%
    inner_join(D_base,  by = 'bucket') %>%
    inner_join(W_base,  by = 'bucket') %>%
    mutate(factor = (D / W) / (D_base / W_base)) %>%
    select(year, bucket, factor)
}


#' Build the full bucketed factor ledger for wealth.
#'
#' @param weight_ledger     (year, id, weight), 2017..2097.
#' @param record_bucket     (id, bucket), frozen at 2022.
#' @param macro_projections Tibble with `year` and `gdp` columns through 2097.
#' @param dfa_path          CSV path for dfa_totals.
#' @param crosswalk_path    CSV path for dfa_schema_crosswalk.
#' @param base_year         Integer, 2022.
#' @return                  Tibble (year, variable, bucket, factor, source).
build_wealth_bucketed_factors = function(weight_ledger,
                                         record_bucket,
                                         macro_projections,
                                         dfa_path       = 'resources/dfa/dfa_totals.csv',
                                         crosswalk_path = 'resources/dfa/dfa_schema_crosswalk.csv',
                                         base_year      = 2022L) {

  stopifnot(all(c('year', 'id', 'weight')     %in% names(weight_ledger)))
  stopifnot(all(c('id', 'bucket')             %in% names(record_bucket)))
  stopifnot(all(c('year', 'gdp')              %in% names(macro_projections)))

  # ---------- DFA data ----------
  dfa = read_csv(dfa_path,       show_col_types = FALSE) %>%
    filter(variant == 'detail', slice == 'income', quarter == 4L) %>%
    select(year, bucket, category, dollars)

  stopifnot(setequal(unique(dfa$bucket), DFA_INCOME_BUCKETS))

  last_dfa_year = max(dfa$year)
  dfa_years_post_base = (base_year + 1L):last_dfa_year

  # ---------- Crosswalk ----------
  crosswalk = read_csv(crosswalk_path, show_col_types = FALSE)
  stopifnot(all(wealth_y_vars %in% crosswalk$y_var))
  dfa_cats = resolve_dfa_categories(crosswalk)

  # ---------- Bucket-total weights ----------
  # W(b, Y) = sum of weight_ledger weights for records whose frozen
  # bucket is b, evaluated at year Y.
  W_by_by = weight_ledger %>%
    inner_join(record_bucket, by = 'id') %>%
    group_by(year, bucket) %>%
    summarise(W = sum(weight), .groups = 'drop')

  stopifnot(nrow(W_by_by %>% filter(year == base_year)) ==
            length(DFA_INCOME_BUCKETS))

  # ---------- Direct/sum/share_of/residual y_vars ----------
  # Everything whose dfa_cats entry is non-NULL gets its factor from DFA
  # directly. kg_* / trusts inherit after this block.
  dfa_derivable = names(dfa_cats)[!vapply(dfa_cats, is.null, logical(1))]

  dfa_factors_list = lapply(dfa_derivable, function(v) {
    dfa_factor_one_var(dfa_cats[[v]], dfa, W_by_by,
                       dfa_years_post_base, base_year) %>%
      mutate(variable = v,
             source   = paste0('dfa:', paste(dfa_cats[[v]], collapse = '+')))
  })
  dfa_factor_tbl = bind_rows(dfa_factors_list)

  # ---------- Inheritance: kg_* + trusts ----------
  inherit_map = list(
    kg_primary_home  = 'primary_home',
    kg_other_re      = 'other_home',       # re_fund has same factor
    kg_pass_throughs = 'pass_throughs',
    kg_other         = 'equities',
    trusts           = 'equities'
  )
  stopifnot(all(names(inherit_map) %in% wealth_y_vars))
  stopifnot(all(unlist(inherit_map) %in% dfa_factor_tbl$variable))

  inherited_tbl = bind_rows(lapply(names(inherit_map), function(v) {
    parent = inherit_map[[v]]
    dfa_factor_tbl %>%
      filter(variable == parent) %>%
      mutate(variable = v,
             source   = paste0('inherit:', parent))
  }))

  ledger_dfa = bind_rows(dfa_factor_tbl, inherited_tbl)

  stopifnot(setequal(unique(ledger_dfa$variable), wealth_y_vars))

  # ---------- 2026+ macro extension ----------
  # Per-household GDP = gdp / W_total, uniform across buckets. Factor at
  # year Y > last_dfa is: factor_at_last_dfa(bucket) × ratio(Y, last_dfa).
  W_total = W_by_by %>%
    group_by(year) %>%
    summarise(W_total = sum(W), .groups = 'drop')

  macro = macro_projections %>%
    select(year, gdp) %>%
    inner_join(W_total, by = 'year') %>%
    mutate(gdp_per_hh = gdp / W_total)

  gdp_last = macro$gdp_per_hh[macro$year == last_dfa_year]
  stopifnot(length(gdp_last) == 1L, !is.na(gdp_last))

  macro_post = macro %>%
    filter(year > last_dfa_year) %>%
    mutate(ratio = gdp_per_hh / gdp_last) %>%
    select(year, ratio)

  ledger_last = ledger_dfa %>%
    filter(year == last_dfa_year) %>%
    select(variable, bucket, factor_last = factor, source_last = source)

  ledger_macro = tidyr::crossing(
    ledger_last %>% select(variable, bucket, factor_last, source_last),
    macro_post
  ) %>%
    mutate(factor = factor_last * ratio,
           source = paste0(source_last, '+gdp_per_hh')) %>%
    select(year, variable, bucket, factor, source)

  # ---------- Combine + sort ----------
  out = bind_rows(
    ledger_dfa %>% select(year, variable, bucket, factor, source),
    ledger_macro
  ) %>%
    arrange(year, variable, bucket)

  # Invariants.
  stopifnot(!any(is.na(out$factor)))
  stopifnot(setequal(unique(out$variable), wealth_y_vars))
  stopifnot(setequal(unique(out$bucket), DFA_INCOME_BUCKETS))
  expected_years = (base_year + 1L):max(macro_projections$year)
  stopifnot(all(expected_years %in% unique(out$year)))

  out
}


#---------------------------------------------------------------------------
# Standalone tests. Targeted unit checks against a small synthetic fixture.
# Run with:
#   Rscript src/dfa_factors.R
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(readr); library(tibble)
  })

  cat('--- dfa_factors.R tests ---\n')

  # Test A: resolve_dfa_categories on a hand-built crosswalk.
  cw = tibble(
    y_var = c('cash', 'equities', 'other_fin', 'kg_primary_home'),
    dfa_variable = c('Deposits + Money market fund shares',
                     'Corporate equities and mutual fund shares',
                     'Miscellaneous assets + Loans (Assets) + Miscellaneous other equity (residual after trusts+pass_throughs)',
                     ''),
    aggregation = c('sum', 'direct', 'residual', 'derived'))
  cats = resolve_dfa_categories(cw)
  stopifnot(identical(cats$cash,
                      c('Deposits', 'Money market fund shares')))
  stopifnot(identical(cats$equities,
                      'Corporate equities and mutual fund shares'))
  stopifnot(identical(cats$other_fin,
                      c('Miscellaneous assets', 'Loans (Assets)',
                        'Miscellaneous other equity')))
  stopifnot(is.null(cats$kg_primary_home))
  cat('  [PASS] crosswalk parser handles sum / direct / residual / derived\n')

  # Test B: dfa_factor_one_var on a tiny fixture where the answer is
  # computable by hand.
  #
  # 2 buckets × 3 years. One category "X":
  #   (low, 2022): $100, (low, 2023): $120, (low, 2024): $140
  #   (high, 2022): $1000, (high, 2023): $1200, (high, 2024): $1500
  # Weights:
  #   (low, 2022): 10, (low, 2023): 11, (low, 2024): 12
  #   (high, 2022): 20, (high, 2023): 21, (high, 2024): 22
  #
  # Per-household growth at (low, 2023) = (120/11) / (100/10) = 10.909/10 = 1.0909
  # Per-household growth at (high, 2024) = (1500/22) / (1000/20) = 68.18/50 = 1.3636
  dfa_long = tibble(
    bucket   = rep(c('low', 'high'), each = 3),
    year     = rep(2022:2024, times = 2),
    category = 'X',
    dollars  = c(100, 120, 140, 1000, 1200, 1500)
  )
  W = tibble(
    bucket = rep(c('low', 'high'), each = 3),
    year   = rep(2022:2024, times = 2),
    W      = c(10, 11, 12, 20, 21, 22)
  )
  f = dfa_factor_one_var('X', dfa_long, W, 2023:2024, 2022L)
  stopifnot(abs(f$factor[f$bucket=='low'  & f$year==2023] -
                (120/11)/(100/10)) < 1e-10)
  stopifnot(abs(f$factor[f$bucket=='high' & f$year==2024] -
                (1500/22)/(1000/20)) < 1e-10)
  cat('  [PASS] dfa_factor_one_var: per-household growth on fixture\n')

  # Test C: sum of two categories collapses correctly (linearity).
  dfa_long2 = bind_rows(
    dfa_long,
    dfa_long %>% mutate(category = 'Y', dollars = dollars * 2)
  )
  # Summing X + Y at each (bucket, year) triples the dollars at that cell,
  # so the resulting factor is IDENTICAL to factor for X alone
  # ((3*num)/(3*denom) cancels).
  f2 = dfa_factor_one_var(c('X', 'Y'), dfa_long2, W, 2023:2024, 2022L)
  stopifnot(max(abs(f2$factor - f$factor)) < 1e-10)
  cat('  [PASS] sum aggregation: proportional scaling cancels\n')

  # Test D: end-to-end on real files (if available). Light checks only.
  skip_real = !file.exists('resources/dfa/dfa_totals.csv') ||
              !file.exists('resources/dfa/dfa_schema_crosswalk.csv')
  if (!skip_real) {
    # Tiny synthetic weight_ledger spanning exactly the DFA years + a few
    # macro years, and a tiny record_bucket.
    record_bucket_fx = tibble(
      id     = 1:6,
      bucket = DFA_INCOME_BUCKETS
    )
    years_fx = 2022L:2030L
    weight_ledger_fx = tidyr::crossing(
      id = 1:6, year = years_fx
    ) %>% mutate(weight = 1 + 0.01 * (year - 2022))   # trivially growing
    macro_fx = tibble(
      year = years_fx,
      gdp  = 25000 * (1.03 ^ (year - 2022))
    )

    out = build_wealth_bucketed_factors(
      weight_ledger_fx, record_bucket_fx, macro_fx,
      base_year = 2022L
    )

    stopifnot(setequal(unique(out$variable), wealth_y_vars))
    stopifnot(setequal(unique(out$bucket),   DFA_INCOME_BUCKETS))
    # Expected years = 2023..max(macro_fx)
    stopifnot(setequal(unique(out$year), 2023L:2030L))
    stopifnot(!any(is.na(out$factor)))

    # Inherited factors must equal their parent at every (year, bucket).
    inherited = out %>%
      filter(variable %in% c('kg_primary_home','kg_other_re',
                             'kg_pass_throughs','kg_other','trusts')) %>%
      mutate(parent = case_when(
        variable == 'kg_primary_home'  ~ 'primary_home',
        variable == 'kg_other_re'      ~ 'other_home',
        variable == 'kg_pass_throughs' ~ 'pass_throughs',
        variable == 'kg_other'         ~ 'equities',
        variable == 'trusts'           ~ 'equities'))
    parent_tbl = out %>%
      select(year, bucket, parent_var = variable,
             parent_factor = factor)
    check = inherited %>%
      inner_join(parent_tbl,
                 by = c('year', 'bucket', 'parent' = 'parent_var'))
    stopifnot(nrow(check) == nrow(inherited))
    stopifnot(max(abs(check$factor - check$parent_factor)) < 1e-12)
    cat('  [PASS] end-to-end: all y_vars present, inheritance exact\n')

    # Macro extension: for any fixed variable and bucket, 2026+ factors
    # should grow proportional to gdp_per_hh only.
    # Check equities on the lowest bucket.
    last_dfa = max(read_csv('resources/dfa/dfa_totals.csv',
                            show_col_types = FALSE)$year[
                      read_csv('resources/dfa/dfa_totals.csv',
                               show_col_types = FALSE)$slice == 'income'])
    # (cheap; done in-test to avoid plumbing more state.)
    eq_low = out %>%
      filter(variable == 'equities', bucket == 'pct00to20') %>%
      arrange(year)
    f_last   = eq_low$factor[eq_low$year == last_dfa]
    f_after  = eq_low$factor[eq_low$year > last_dfa]
    W_total = weight_ledger_fx %>%
      group_by(year) %>% summarise(W = sum(weight), .groups = 'drop')
    gdp_per_hh = macro_fx$gdp / (W_total$W[match(macro_fx$year, W_total$year)])
    names(gdp_per_hh) = macro_fx$year
    ratio_expected = gdp_per_hh[as.character(eq_low$year[eq_low$year > last_dfa])] /
                     gdp_per_hh[as.character(last_dfa)]
    factor_expected = f_last * ratio_expected
    stopifnot(max(abs(f_after - factor_expected)) < 1e-10)
    cat('  [PASS] macro extension matches per-household gdp ratio\n')
  } else {
    cat('  [SKIP] end-to-end tests (DFA files not present)\n')
  }

  cat('\nAll tests passed.\n')
}
