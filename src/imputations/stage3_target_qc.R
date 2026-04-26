#---------------------------------------------
# stage3_target_qc.R
#
# Target specification + automatic viability
# check for Stage 3 wealth calibration.
#
# Caller supplies a DESIRED target set (cells ×
# categories × margins). Viability is assessed
# from the SCF side (sample-size thin; extensive
# fraction degenerate near 0 or 1) and the PUF-
# leaf side (donor support for extensive targets).
# Targets that don't pass are reported and
# dropped from the solver.
#---------------------------------------------

source('src/imputations/wealth_schema.R')


# User-facing bucket grid (cells × categories × margins).
CALIB_INCOME_BUCKETS = c('pct00to20', 'pct20to40', 'pct40to60',
                         'pct60to80', 'pct80to90', 'pct90to99',
                         'pct99to99.9', 'pct99.9to100')
CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)

CALIB_AGE_BUCKETS = c('nonsenior', 'senior')
SENIOR_AGE        = 65L

# 2026-04-26: split "other" into 3 sub-categories (retirement, business,
# residual other) to give Step A direct amount control over the two largest
# aggregate-driver components. Previously cat_other lumped 10 wealth y-vars
# together, including retirement (~$30T aggregate) and pass_throughs/
# business (~$15T, very heavy-tailed) — the joint solver had only one knob
# for an aggregate determined by 10 underlying values, so within-other
# composition could be wrong even when the aggregate matched. After the
# split: retirement and business have their own targets; "other" becomes
# the residual (cash, life_ins, annuities, trusts, other_fin, other_home,
# re_fund, other_nonfin) — 8 vars instead of 10, more homogeneous.
CALIB_CATEGORIES = c('nw', 'equities', 'bonds', 'homes',
                     'retirement', 'business', 'other', 'debt')
CALIB_MARGINS    = c('intensive', 'extensive')


#' Return the default user-requested target spec (7 × 2 × 6 × 2 = 168 rows).
default_wealth_target_spec = function() {
  tidyr::crossing(
    cell_income = CALIB_INCOME_BUCKETS,
    cell_age    = CALIB_AGE_BUCKETS,
    category    = CALIB_CATEGORIES,
    margin      = CALIB_MARGINS
  )
}


#' Build the 6 category columns (cat_nw, cat_equities, cat_bonds,
#' cat_homes, cat_other, cat_debt) from a 23-var wealth tibble.
#' The `cat_` prefix avoids clobbering the 23-var raw names (the input
#' tibble already has columns named `equities`, `bonds`, etc.).
#'
#' Definitions (last revised 2026-04-26):
#'   cat_nw         = sum(13 assets) - sum(6 debts)  (kg_* NOT included)
#'   cat_equities   = equities                       (single var)
#'   cat_bonds      = bonds                          (single var)
#'   cat_homes      = primary_home                   (primary residence)
#'   cat_retirement = retirement                     (IRA + 401k + pensions)
#'   cat_business   = pass_throughs                  (private business)
#'   cat_other      = residual: cash + life_ins + annuities + trusts +
#'                    other_fin + other_home + re_fund + other_nonfin
#'   cat_debt       = sum(6 debt vars)
compute_category_values = function(df) {
  residual_other_vars = setdiff(
    wealth_asset_vars,
    c('equities', 'bonds', 'primary_home', 'retirement', 'pass_throughs'))
  stopifnot(all(wealth_asset_vars %in% names(df)))
  stopifnot(all(wealth_debt_vars  %in% names(df)))
  total_debt = rowSums(df[, wealth_debt_vars, drop = FALSE])
  data.frame(
    cat_nw         = rowSums(df[, wealth_asset_vars, drop = FALSE]) - total_debt,
    cat_equities   = df$equities,
    cat_bonds      = df$bonds,
    cat_homes      = df$primary_home,
    cat_retirement = df$retirement,
    cat_business   = df$pass_throughs,
    cat_other      = rowSums(df[, residual_other_vars, drop = FALSE]),
    cat_debt       = total_debt
  )
}

# Column names produced by compute_category_values(), keyed by category
# label used in target specs.
CAT_COL = setNames(paste0('cat_', CALIB_CATEGORIES), CALIB_CATEGORIES)


#' Assign calibration cell labels (cell_income, cell_age) to a tibble.
#'
#' @param df   Tibble with weight + income + age_older columns.
#' @return     df with two new columns added: cell_income, cell_age.
assign_calibration_cells = function(df, income, age_older, weight) {
  stopifnot(length(income) == nrow(df))
  # Continuous 0..100 rank from weighted income (not quantized to integer
  # percentiles). Needed because CALIB_INCOME_EDGES now includes a 99.9
  # break, which compute_percentile's integer output can't resolve.
  ord        = order(income)
  cum_w      = cumsum(weight[ord]) / sum(weight)
  rank_0_100 = numeric(length(income))
  rank_0_100[ord] = 100 * cum_w
  idx = findInterval(rank_0_100, CALIB_INCOME_EDGES,
                     rightmost.closed = TRUE, all.inside = TRUE)
  df$cell_income = CALIB_INCOME_BUCKETS[idx]
  df$cell_age    = if_else(age_older >= SENIOR_AGE, 'senior', 'nonsenior')
  df
}


#' Run the SCF-anchor viability check on a requested target set.
#'
#' For each requested (cell × category × margin), compute:
#'   - n_unwt : SCF unweighted count in the cell
#'   - n_wt   : SCF weighted count in the cell
#'   - frac_pos: weighted frac with category > 0 in the cell
#'   - target_value : the target the solver will try to match
#'                    (intensive = weighted $ total; extensive = weighted count>0)
#'
#' and flag viability:
#'   thin_unwt_n : cell unwtd n < this → drop (SCF too thin to anchor)
#'   degen_lo    : extensive frac_pos < this → drop (near-zero, uninformative)
#'   degen_hi    : extensive frac_pos > this → drop (near-one, uninformative)
#'
#' @return Tibble with all requested columns + n_unwt, n_wt, frac_pos,
#'         target_value, status, status_reason.
assess_target_viability = function(requested, scf_cells,
                                    thin_unwt_n = 50L,
                                    degen_lo    = 0.02,
                                    degen_hi    = 0.98) {
  stopifnot(all(c('cell_income', 'cell_age', 'category', 'margin') %in%
                names(requested)))
  stopifnot(all(c('cell_income', 'cell_age', 'weight') %in% names(scf_cells)))
  stopifnot(all(CAT_COL %in% names(scf_cells)))

  # Precompute per-cell × per-category SCF stats.
  stats = scf_cells %>%
    pivot_longer(cols = all_of(unname(CAT_COL)),
                 names_to  = 'cat_col',
                 values_to = 'y_value') %>%
    mutate(category = sub('^cat_', '', cat_col)) %>%
    group_by(cell_income, cell_age, category) %>%
    summarise(
      n_unwt         = dplyr::n(),
      n_wt           = sum(weight),
      frac_pos       = weighted.mean(y_value > 0, weight),
      total_dollars  = sum(weight * y_value),
      pos_count_wt   = sum(weight * (y_value > 0)),
      .groups = 'drop'
    )

  requested %>%
    left_join(stats,
              by = c('cell_income', 'cell_age', 'category')) %>%
    mutate(
      target_value = if_else(margin == 'intensive',
                             total_dollars,
                             pos_count_wt),
      status_reason = case_when(
        is.na(n_unwt)                                   ~ 'drop_no_data',
        n_unwt < thin_unwt_n                            ~ 'drop_thin',
        margin == 'extensive' & frac_pos < degen_lo     ~ 'drop_near_zero',
        margin == 'extensive' & frac_pos > degen_hi     ~ 'drop_near_one',
        TRUE                                            ~ 'keep'
      ),
      status = if_else(status_reason == 'keep', 'keep', 'drop')
    ) %>%
    select(cell_income, cell_age, category, margin,
           n_unwt, n_wt, frac_pos, target_value,
           status, status_reason)
}


#' PUF-leaf donor-support check for extensive targets.
#'
#' For each PUF record's leaf, is there AT LEAST ONE donor with the
#' category value > 0? If too many records have dead leaves for a
#' target, the swap can't reach it. Flag cells where the supported
#' fraction falls below `min_supported`.
#'
#' Returns a tibble (cell_income, cell_age, category, frac_supported).
#' Caller merges with QC table to augment status.
assess_puf_donor_support = function(puf_cells, leaf_donors_list,
                                    donor_categories,
                                    min_supported = 0.80) {
  stopifnot(nrow(puf_cells) == length(leaf_donors_list))
  stopifnot(all(CALIB_CATEGORIES %in% names(donor_categories)))

  support_per_record = function(cat_vec) {
    vapply(leaf_donors_list,
           function(idx) any(cat_vec[idx] > 0),
           logical(1))
  }

  results = lapply(CALIB_CATEGORIES, function(cat_name) {
    supported = support_per_record(donor_categories[[cat_name]])
    tibble(
      cell_income = puf_cells$cell_income,
      cell_age    = puf_cells$cell_age,
      category    = cat_name,
      supported   = supported
    )
  })
  bind_rows(results) %>%
    group_by(cell_income, cell_age, category) %>%
    summarise(frac_supported = mean(supported), .groups = 'drop') %>%
    mutate(donor_support_ok = frac_supported >= min_supported)
}


#' Printable summary of a QC table.
summarize_qc = function(qc) {
  total   = nrow(qc)
  kept    = sum(qc$status == 'keep')
  dropped = total - kept
  cat(sprintf(
    'Stage 3 target QC: %d requested, %d kept, %d dropped.\n',
    total, kept, dropped))
  if (dropped > 0) {
    reason_tbl = qc %>% filter(status != 'keep') %>%
      count(status_reason, sort = TRUE)
    for (r in seq_len(nrow(reason_tbl))) {
      cat(sprintf('  %-18s  %3d\n',
                  reason_tbl$status_reason[r],
                  reason_tbl$n[r]))
    }
  }
  invisible(qc)
}


#---------------------------------------------------------------------------
# Standalone tests. Run with:
#   Rscript src/imputations/stage3_target_qc.R
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(tibble); library(Hmisc)
  })
  source('src/imputations/helpers.R')

  cat('--- stage3_target_qc.R tests ---\n')

  # Fixture: small synthetic SCF with known cell composition.
  set.seed(1)
  n = 5000L
  scf_fx = tibble(
    weight    = runif(n, 50, 500),
    income    = rlnorm(n, log(60000), 1.5) *
                  sign(runif(n, -0.05, 1)),   # 5% negative
    age_older = sample(25:85, n, replace = TRUE),
    # all 23 wealth y_vars; populate with plausible zeros + magnitudes
    cash             = rexp(n, 1/5000) * rbinom(n, 1, 0.7),
    equities         = rexp(n, 1/20000) * rbinom(n, 1, 0.35),
    bonds            = rexp(n, 1/10000) * rbinom(n, 1, 0.10),
    retirement       = rexp(n, 1/40000) * rbinom(n, 1, 0.6),
    life_ins         = rexp(n, 1/5000)  * rbinom(n, 1, 0.2),
    annuities        = rexp(n, 1/10000) * rbinom(n, 1, 0.05),
    trusts           = rexp(n, 1/50000) * rbinom(n, 1, 0.02),
    other_fin        = rexp(n, 1/3000)  * rbinom(n, 1, 0.3),
    pass_throughs    = rexp(n, 1/100000)* rbinom(n, 1, 0.1),
    primary_home     = rexp(n, 1/150000)* rbinom(n, 1, 0.65),
    other_home       = rexp(n, 1/50000) * rbinom(n, 1, 0.1),
    re_fund          = rexp(n, 1/20000) * rbinom(n, 1, 0.05),
    other_nonfin     = rexp(n, 1/15000) * rbinom(n, 1, 0.95),
    primary_mortgage = rexp(n, 1/100000)* rbinom(n, 1, 0.5),
    other_mortgage   = rexp(n, 1/50000) * rbinom(n, 1, 0.1),
    credit_lines     = rexp(n, 1/5000)  * rbinom(n, 1, 0.1),
    credit_cards     = rexp(n, 1/3000)  * rbinom(n, 1, 0.4),
    installment_debt = rexp(n, 1/8000)  * rbinom(n, 1, 0.3),
    other_debt       = rexp(n, 1/4000)  * rbinom(n, 1, 0.1),
    kg_primary_home  = 0, kg_other_re  = 0,
    kg_pass_throughs = 0, kg_other     = 0
  )
  scf_fx = assign_calibration_cells(scf_fx, scf_fx$income, scf_fx$age_older, scf_fx$weight)
  scf_fx = cbind(scf_fx, compute_category_values(scf_fx))

  # Test 1: default spec size.
  spec = default_wealth_target_spec()
  expected_rows = length(CALIB_INCOME_BUCKETS) *
                  length(CALIB_AGE_BUCKETS) *
                  length(CALIB_CATEGORIES) *
                  length(CALIB_MARGINS)
  stopifnot(nrow(spec) == expected_rows)
  cat(sprintf('  [PASS] default spec has %d rows\n', expected_rows))

  # Test 2: assess_target_viability returns one row per requested target.
  qc = assess_target_viability(spec, scf_fx)
  stopifnot(nrow(qc) == nrow(spec))
  stopifnot(all(qc$status %in% c('keep', 'drop')))
  cat(sprintf('  [PASS] assess produced %d rows, %d kept, %d dropped\n',
              nrow(qc), sum(qc$status == 'keep'), sum(qc$status == 'drop')))

  # Test 3: intensive targets should never be dropped for degeneracy
  #         (only thin or no_data can drop intensive).
  intensive_dropped = qc %>%
    filter(margin == 'intensive', status == 'drop')
  stopifnot(all(intensive_dropped$status_reason %in%
                c('drop_thin', 'drop_no_data')))
  cat('  [PASS] intensive-margin targets not dropped for degeneracy\n')

  # Test 4: A near-zero extensive target should drop. Build one manually by
  #         shrinking bonds to near-zero in pct00to20 nonsenior.
  scf_zero = scf_fx
  scf_zero$bonds[scf_zero$cell_income == 'pct00to20' &
                 scf_zero$cell_age == 'nonsenior'] = 0
  scf_zero[, names(CAT_COL) %>% paste0('cat_', .)] = NULL
  scf_zero = cbind(scf_zero, compute_category_values(scf_zero))
  qc_zero = assess_target_viability(spec, scf_zero)
  target_row = qc_zero %>%
    filter(cell_income == 'pct00to20', cell_age == 'nonsenior',
           category == 'bonds', margin == 'extensive')
  stopifnot(target_row$status == 'drop')
  stopifnot(target_row$status_reason == 'drop_near_zero')
  cat('  [PASS] near-zero extensive target dropped with correct reason\n')

  # Test 5: summarize_qc runs without error.
  summarize_qc(qc)
  cat('  [PASS] summarize_qc runs\n')

  # Test 6: compute_category_values arithmetic.
  catvals = compute_category_values(scf_fx)
  # nw = sum(assets) - sum(debts); spot-check rowwise.
  assets_sum = rowSums(scf_fx[, wealth_asset_vars])
  debts_sum  = rowSums(scf_fx[, wealth_debt_vars])
  stopifnot(max(abs(catvals$cat_nw - (assets_sum - debts_sum))) < 1e-10)
  stopifnot(max(abs(catvals$cat_equities - scf_fx$equities)) < 1e-10)
  stopifnot(max(abs(catvals$cat_homes - scf_fx$primary_home)) < 1e-10)
  cat('  [PASS] category value arithmetic\n')

  cat('\nAll tests passed.\n')
}
