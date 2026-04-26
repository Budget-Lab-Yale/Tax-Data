#---------------------------------------------
# wealth_compositional_decomp.R
#
# For each (cell × feature group), build a
# counterfactual PUF where THAT group's features
# are replaced with values drawn from SCF records
# in the same cell (all other features stay
# PUF-native). Apply the cell forest to the
# counterfactual. Compare aggregate NW to
# baseline PUF. Delta = how much of the per-cell
# PUF-SCF gap is attributable to that group's
# within-cell X distribution difference.
#
# Uses the cached per-cell forests from
# wealth_percell_diagnostic.R. Pre-tilt (no
# Stage 3) so we're measuring forest-routing
# bias directly.
#
# Usage:
#   Rscript src/eda/wealth_compositional_decomp.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(drf);   library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/wealth.R')    # for scf_to_y

features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'age_younger', 'n_dep_hh',
             'pctile_wages', 'has_wages',
             'pctile_business', 'has_business_act', 'has_business_pos',
             'pctile_int_div', 'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act', 'has_capital_gains_pos',
             'pctile_ss_pens', 'has_ss_pens')

FEATURE_GROUPS = list(
  income    = c('has_income_act', 'has_income_pos', 'has_negative_income',
                'pctile_income'),
  wages     = c('pctile_wages', 'has_wages'),
  business  = c('pctile_business', 'has_business_act', 'has_business_pos'),
  int_div   = c('pctile_int_div', 'has_int_div'),
  cap_gains = c('pctile_capital_gains', 'has_capital_gains_act',
                'has_capital_gains_pos'),
  ss_pens   = c('pctile_ss_pens', 'has_ss_pens'),
  age       = c('age_older', 'age_younger'),
  family    = c('married', 'n_dep_hh')
)

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')

make_has = function(x) case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)

set.seed(42)


#--- Load + prep SCF -----------------------------------------------------

scf = read_rds('resources/cache/scf_tax_units.rds') %>%
  mutate(
    n_dep_hh    = n_dep,
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    age_younger = if_else(!is.na(age2_capped), pmin(age1_capped, age2_capped), 0L),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    has_income_act        = as.integer(income != 0),
    has_income_pos        = as.integer(income >  0),
    has_negative_income   = as.integer(income <  0),
    pctile_income         = compute_percentile(income, weight, FINE_PCTILE_PROBS),
    has_wages             = make_has(wages_scf),
    pctile_wages          = compute_percentile(wages_scf, weight, FINE_PCTILE_PROBS),
    has_business_act      = as.integer(business_scf != 0),
    has_business_pos      = as.integer(business_scf >  0),
    pctile_business       = compute_percentile(business_scf, weight, FINE_PCTILE_PROBS),
    has_int_div           = make_has(int_div_scf),
    pctile_int_div        = compute_percentile(int_div_scf, weight, FINE_PCTILE_PROBS),
    has_capital_gains_act = as.integer(capital_gains_scf != 0),
    has_capital_gains_pos = as.integer(capital_gains_scf >  0),
    pctile_capital_gains  = compute_percentile(capital_gains_scf, weight, FINE_PCTILE_PROBS),
    has_ss_pens           = make_has(ss_pens_scf),
    pctile_ss_pens        = compute_percentile(ss_pens_scf, weight, FINE_PCTILE_PROBS)
  )

scf_y = scf_to_y(scf)
X_scf = as.matrix(scf[features])
Y_scf = as.matrix(scf_y[wealth_y_vars])
W_scf = scf$weight
N_scf = nrow(X_scf)
scf_nw_truth = rowSums(Y_scf[, wealth_asset_vars]) - rowSums(Y_scf[, wealth_debt_vars])

ord = order(scf$income); cum_w = cumsum(W_scf[ord]) / sum(W_scf)
rank_scf = numeric(N_scf); rank_scf[ord] = 100 * cum_w
scf_cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]


#--- Load + prep PUF -----------------------------------------------------

puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                show_col_types = FALSE) %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    age_younger = if_else(!is.na(age2_capped), pmin(age1_capped, age2_capped), 0L),
    married     = as.integer(!is.na(male2)),
    n_dep_hh    = (!is.na(dep_age1) & dep_age1 < 18) +
                  (!is.na(dep_age2) & dep_age2 < 18) +
                  (!is.na(dep_age3) & dep_age3 < 18),
    income = wages + sole_prop + farm +
             scorp_active  - scorp_active_loss  - scorp_179 +
             scorp_passive - scorp_passive_loss +
             part_active   - part_active_loss   - part_179 +
             part_passive  - part_passive_loss +
             txbl_int + exempt_int + div_ord + div_pref +
             kg_lt + kg_st +
             gross_ss + gross_pens_dist + ui +
             rent - rent_loss + estate - estate_loss,
    wages_puf         = wages,
    business_puf      = sole_prop + farm +
                        scorp_active  - scorp_active_loss  - scorp_179 +
                        scorp_passive - scorp_passive_loss +
                        part_active   - part_active_loss   - part_179 +
                        part_passive  - part_passive_loss,
    int_div_puf       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains_puf = kg_lt + kg_st,
    ss_pens_puf       = gross_ss + gross_pens_dist,
    has_income_act        = as.integer(income != 0),
    has_income_pos        = as.integer(income >  0),
    has_negative_income   = as.integer(income <  0),
    pctile_income         = compute_percentile(income, weight, FINE_PCTILE_PROBS),
    has_wages             = make_has(wages_puf),
    pctile_wages          = compute_percentile(wages_puf, weight, FINE_PCTILE_PROBS),
    has_business_act      = as.integer(business_puf != 0),
    has_business_pos      = as.integer(business_puf >  0),
    pctile_business       = compute_percentile(business_puf, weight, FINE_PCTILE_PROBS),
    has_int_div           = make_has(int_div_puf),
    pctile_int_div        = compute_percentile(int_div_puf, weight, FINE_PCTILE_PROBS),
    has_capital_gains_act = as.integer(capital_gains_puf != 0),
    has_capital_gains_pos = as.integer(capital_gains_puf >  0),
    pctile_capital_gains  = compute_percentile(capital_gains_puf, weight, FINE_PCTILE_PROBS),
    has_ss_pens           = make_has(ss_pens_puf),
    pctile_ss_pens        = compute_percentile(ss_pens_puf, weight, FINE_PCTILE_PROBS)
  )

X_puf = as.matrix(puf[features])
W_puf = puf$weight
N_puf = nrow(X_puf)

puf_ord = order(puf$income)
puf_cum_w = cumsum(W_puf[puf_ord]) / sum(W_puf)
rank_puf = numeric(N_puf); rank_puf[puf_ord] = 100 * puf_cum_w
puf_cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]


#--- Forest utilities ----------------------------------------------------

extract_forest = function(m) list(
  n_trees = m[['_num_trees']],
  root    = sapply(m[['_root_nodes']], identity),
  child_L = lapply(m[['_child_nodes']], `[[`, 1L),
  child_R = lapply(m[['_child_nodes']], `[[`, 2L),
  split_v = m[['_split_vars']],
  split_s = m[['_split_values']],
  leaves  = m[['_leaf_samples']]
)

walk_to_leaf_one = function(f, t, x_row) {
  L = f$child_L[[t]]; R = f$child_R[[t]]
  v = f$split_v[[t]]; s = f$split_s[[t]]
  ll = f$leaves[[t]]
  node = f$root[t] + 1L
  repeat {
    if (length(ll[[node]]) > 0L) return(node)
    xv = x_row[v[node] + 1L]; sv = s[node]
    node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L else R[node] + 1L
  }
}

walk_rows = function(f, X_block, seed) {
  set.seed(seed)
  n = nrow(X_block)
  tree_pick = sample.int(f$n_trees, size = n, replace = TRUE)
  donor_idx = integer(n)
  for (i in seq_len(n)) {
    t  = tree_pick[i]
    nd = walk_to_leaf_one(f, t, X_block[i, ])
    lr = f$leaves[[t]][[nd]] + 1L
    donor_idx[i] = lr[sample.int(length(lr), 1L)]
  }
  donor_idx  # indices into cell's bootstrap
}


#--- Per-cell decomposition ---------------------------------------------

cat('Per-cell compositional decomposition of PUF Y overshoot.\n')
cat('Baseline: PUF forest applied with native X. Counterfactuals: each\n')
cat('feature group replaced with SCF values (same cell donor).\n\n')

all_results = list()
grp_deltas_by_cell = list()

for (cell_name in CALIB_INCOME_BUCKETS) {
  cat(sprintf('\n=============================================\n'))
  cat(sprintf('Cell %s\n', cell_name))
  cat(sprintf('=============================================\n'))
  seed_offset = which(CALIB_INCOME_BUCKETS == cell_name)

  # Reconstruct training bootstrap (matches wealth_percell_diagnostic.R)
  scf_rows_in_cell = which(scf_cell == cell_name)
  set.seed(100 + seed_offset)
  n_boot = if (cell_name == 'pct99.9to100') 150000L else 100000L
  boot_in_cell = sample.int(length(scf_rows_in_cell), size = n_boot,
                             replace = TRUE,
                             prob = W_scf[scf_rows_in_cell])
  boot_scf_row = scf_rows_in_cell[boot_in_cell]

  # Load cached forest
  drf_cell = read_rds(paste0('resources/cache/qrf/wealth_percell_',
                              cell_name, '.rds'))
  f_cell = extract_forest(drf_cell)

  # Baseline: apply forest with native PUF X
  puf_rows = which(puf_cell == cell_name)
  scf_rows_cell = which(scf_cell == cell_name)
  if (length(puf_rows) == 0L) next

  donor_base = walk_rows(f_cell, X_puf[puf_rows, , drop = FALSE],
                          seed = 1000 + seed_offset)
  puf_nw_base = scf_nw_truth[boot_scf_row[donor_base]]
  baseline_agg = sum(W_puf[puf_rows] * puf_nw_base) / 1e12

  # SCF truth in this cell
  scf_mask = scf_cell == cell_name
  scf_truth_agg = sum(W_scf[scf_mask] * scf_nw_truth[scf_mask]) / 1e12

  cat(sprintf('  SCF truth:          $%.3fT\n', scf_truth_agg))
  cat(sprintf('  PUF baseline:       $%.3fT  (gap +$%.3fT, +%.1f%%)\n',
              baseline_agg, baseline_agg - scf_truth_agg,
              100 * (baseline_agg - scf_truth_agg) / scf_truth_agg))

  # For each feature group, build counterfactual
  cell_deltas = list()
  for (g_name in names(FEATURE_GROUPS)) {
    grp_features = FEATURE_GROUPS[[g_name]]

    # For each PUF record, draw an SCF donor within the cell (weighted)
    set.seed(2000 + seed_offset * 100 + which(names(FEATURE_GROUPS) == g_name))
    scf_donor_for_swap = scf_rows_cell[
      sample.int(length(scf_rows_cell), size = length(puf_rows),
                 replace = TRUE, prob = W_scf[scf_rows_cell])
    ]

    # Build counterfactual X: PUF except group features from SCF donor
    X_cf = X_puf[puf_rows, , drop = FALSE]
    X_cf[, grp_features] = X_scf[scf_donor_for_swap, grp_features, drop = FALSE]

    # Walk forest
    donor_cf = walk_rows(f_cell, X_cf, seed = 3000 + seed_offset * 100 +
                                       which(names(FEATURE_GROUPS) == g_name))
    puf_nw_cf = scf_nw_truth[boot_scf_row[donor_cf]]
    cf_agg = sum(W_puf[puf_rows] * puf_nw_cf) / 1e12

    delta = cf_agg - baseline_agg
    cell_deltas[[g_name]] = delta
    cat(sprintf('  swap %-10s: $%.3fT  (Δ vs baseline %+.3fT, %.1f%% of gap)\n',
                g_name, cf_agg, delta,
                if (abs(baseline_agg - scf_truth_agg) > 1e-9)
                  100 * (-delta) / (baseline_agg - scf_truth_agg)
                else NA))
  }

  # Also: full swap (all features from SCF donor) as a sanity check
  set.seed(9000 + seed_offset)
  scf_donor_full = scf_rows_cell[
    sample.int(length(scf_rows_cell), size = length(puf_rows),
               replace = TRUE, prob = W_scf[scf_rows_cell])
  ]
  X_full_swap = X_scf[scf_donor_full, , drop = FALSE]
  donor_full = walk_rows(f_cell, X_full_swap, seed = 9500 + seed_offset)
  puf_nw_full = scf_nw_truth[boot_scf_row[donor_full]]
  full_agg = sum(W_puf[puf_rows] * puf_nw_full) / 1e12
  cat(sprintf('  FULL swap (SCF X): $%.3fT  (Δ vs baseline %+.3fT)\n',
              full_agg, full_agg - baseline_agg))
  cat(sprintf('    [theoretical: should ≈ SCF truth × PUF/SCF pop ratio = $%.3fT]\n',
              scf_truth_agg * sum(W_puf[puf_rows]) / sum(W_scf[scf_mask])))

  grp_deltas_by_cell[[cell_name]] = tibble(
    cell          = cell_name,
    scf_truth_T   = scf_truth_agg,
    puf_base_T    = baseline_agg,
    gap_T         = baseline_agg - scf_truth_agg,
    full_swap_T   = full_agg,
    !!!setNames(as.list(unlist(cell_deltas)), paste0('delta_', names(cell_deltas)))
  )
}


#--- Consolidated report --------------------------------------------------

cat('\n\n=============================================\n')
cat('Summary: per-cell, per-group deltas ($T, pre-tilt)\n')
cat('=============================================\n\n')

summary_tbl = bind_rows(grp_deltas_by_cell)
print(as.data.frame(summary_tbl %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))),
  row.names = FALSE)

cat('\n\n=============================================\n')
cat('Ranked contributions per cell (swap → reduces gap)\n')
cat('=============================================\n\n')

for (cn in CALIB_INCOME_BUCKETS) {
  row = summary_tbl %>% filter(cell == cn)
  if (nrow(row) == 0) next
  deltas = unlist(row %>% select(starts_with('delta_')))
  names(deltas) = sub('^delta_', '', names(deltas))
  # Negative delta = swap reduces PUF aggregate → "closes" the gap
  closing = -deltas
  gap = row$gap_T
  pct = if (abs(gap) > 1e-9) 100 * closing / gap else rep(NA, length(closing))
  ord_idx = order(closing, decreasing = TRUE)
  cat(sprintf('%s (gap $%.2fT):\n', cn, gap))
  for (k in ord_idx) {
    cat(sprintf('  %-10s  Δ %+7.2fT  (closes %5.1f%% of gap)\n',
                names(closing)[k], closing[k], pct[k]))
  }
  cat(sprintf('  SUM        Δ %+7.2fT  (closes %5.1f%% of gap)\n\n',
              sum(closing), 100 * sum(closing) / gap))
}

cat('Done.\n')
