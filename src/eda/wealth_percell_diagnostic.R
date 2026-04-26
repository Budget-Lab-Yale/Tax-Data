#---------------------------------------------
# wealth_percell_diagnostic.R
#
# Per-cell option (a) architecture test.
#
# 8 income cells (weighted percentile edges
# 0/20/40/60/80/90/99/99.9/100). For each cell:
#   - weighted bootstrap to n_boot rows
#     (100k for bottom 7, 150k for pct99.9to100)
#   - unweighted DRF, min.node.size=20, mtry=10,
#     num.features=50
#   - uniform leaf draw at inference
#
# Apply to SCF X, aggregate with SCF weights,
# compare to SCF truth. Mirror of
# wealth_dim2_diagnostic.R but per-cell.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(drf);   library(Hmisc)
})

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/wealth.R')   # for scf_to_y

set.seed(42)

#--- Load + prepare SCF ----------------------------------------------------

cat('Loading SCF tax units...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'age_younger', 'n_dep_hh',
             'pctile_wages', 'has_wages',
             'pctile_business', 'has_business_act', 'has_business_pos',
             'pctile_int_div', 'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act', 'has_capital_gains_pos',
             'pctile_ss_pens', 'has_ss_pens')

make_has = function(x) case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)

scf_tax_units = scf_tax_units %>%
  mutate(
    n_dep_hh    = n_dep,
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                          NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
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

scf_y_df = scf_to_y(scf_tax_units)

X_scf = as.matrix(scf_tax_units[features])
Y_scf = as.matrix(scf_y_df[wealth_y_vars])
W_scf = scf_tax_units$weight
N_scf = nrow(X_scf)

scf_nw_truth = rowSums(Y_scf[, wealth_asset_vars]) -
               rowSums(Y_scf[, wealth_debt_vars])

# --- Assign 8 cells on SCF's own income rank
ord = order(scf_tax_units$income)
cum_w = cumsum(W_scf[ord]) / sum(W_scf)
rank_0_100 = numeric(N_scf)
rank_0_100[ord] = 100 * cum_w

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')
cell_idx = findInterval(rank_0_100, CALIB_INCOME_EDGES,
                        rightmost.closed = TRUE, all.inside = TRUE)
scf_cell = CALIB_INCOME_BUCKETS[cell_idx]

# Cell-specific bootstrap sizes
n_boot_cell = c(
  'pct00to20'    = 100000L,
  'pct20to40'    = 100000L,
  'pct40to60'    = 100000L,
  'pct60to80'    = 100000L,
  'pct80to90'    = 100000L,
  'pct90to99'    = 100000L,
  'pct99to99.9'  = 100000L,
  'pct99.9to100' = 150000L
)

# Coarser bin for per-quintile reporting (matches wealth_dim2_diagnostic.R).
to_bin = function(pct) {
  cut(pct, breaks = c(-1, 19, 39, 59, 79, 89, 98, 100),
      labels = c('p0-20','p20-40','p40-60','p60-80','p80-90','p90-99','top1'),
      include.lowest = TRUE, right = TRUE)
}
scf_pct = compute_percentile(scf_tax_units$income, W_scf, seq(0.01, 0.99, 0.01))
scf_bin_reporting = to_bin(scf_pct)

cat(sprintf('  %d SCF rows, total pop weight %.1fM, total NW truth $%.2fT\n',
            N_scf, sum(W_scf)/1e6, sum(W_scf * scf_nw_truth) / 1e12))


#--- Forest walk utilities -------------------------------------------------

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


#--- Per-cell train + walk -------------------------------------------------

cat('\n\n================================================\n')
cat('Per-cell training + walking\n')
cat('================================================\n')

estimate_models = 1L

picks_scf_row = integer(N_scf)
t_total_start = Sys.time()

for (cell_name in CALIB_INCOME_BUCKETS) {
  cat(sprintf('\n--- cell %s ---\n', cell_name))

  cell_rows = which(scf_cell == cell_name)
  cat(sprintf('  SCF rows in cell: %d\n', length(cell_rows)))

  n_boot = n_boot_cell[cell_name]
  cell_weights = W_scf[cell_rows]

  seed_offset = which(CALIB_INCOME_BUCKETS == cell_name)

  # Weighted bootstrap within this cell.
  set.seed(100 + seed_offset)
  boot_in_cell = sample.int(length(cell_rows), size = n_boot,
                             replace = TRUE, prob = cell_weights)
  boot_scf_row = cell_rows[boot_in_cell]

  unique_rows  = length(unique(boot_scf_row))
  cat(sprintf('  Bootstrap: %d rows; unique SCF rows sampled: %d of %d (%d missing)\n',
              n_boot, unique_rows, length(cell_rows),
              length(cell_rows) - unique_rows))

  X_boot = X_scf[boot_scf_row, , drop = FALSE]
  Y_boot = Y_scf[boot_scf_row, , drop = FALSE]

  cat('  Training DRF ...\n')
  t0 = Sys.time()
  drf_cell = train_or_load_drf(
    name          = paste0('wealth_percell_', cell_name),
    X             = X_boot,
    Y             = Y_boot,
    num.features  = 50,
    min.node.size = 20,
    mtry          = 10
  )
  cat(sprintf('    train+cache %.1fs\n',
              as.numeric(Sys.time() - t0, units = 'secs')))

  f_cell = extract_forest(drf_cell)
  med_leaf = median(unlist(lapply(f_cell$leaves, function(ll)
               lengths(ll)[lengths(ll) > 0L])))
  cat(sprintf('  %d trees; median leaf size = %d\n', f_cell$n_trees, med_leaf))

  # Walk forest for this cell's SCF rows.
  cell_X_scf = X_scf[cell_rows, , drop = FALSE]
  set.seed(200 + seed_offset)
  tree_pick = sample.int(f_cell$n_trees, size = length(cell_rows),
                          replace = TRUE)
  t0 = Sys.time()
  for (i in seq_along(cell_rows)) {
    t  = tree_pick[i]
    nd = walk_to_leaf_one(f_cell, t, cell_X_scf[i, ])
    lr = f_cell$leaves[[t]][[nd]] + 1L
    pick_boot = lr[sample.int(length(lr), 1L)]   # uniform within leaf
    picks_scf_row[cell_rows[i]] = boot_scf_row[pick_boot]
  }
  cat(sprintf('  Walking %d rows: %.1fs\n',
              length(cell_rows),
              as.numeric(Sys.time() - t0, units = 'secs')))
}

cat(sprintf('\nTotal train+walk time: %.1fs\n',
            as.numeric(Sys.time() - t_total_start, units = 'secs')))


#--- Aggregate and compare -------------------------------------------------

Y_hat = Y_scf[picks_scf_row, , drop = FALSE]

aggregate_report = function(y_pred_mat, w, bin_vec, label) {
  nw = rowSums(y_pred_mat[, wealth_asset_vars]) -
       rowSums(y_pred_mat[, wealth_debt_vars])
  total = sum(w * nw) / 1e12
  cat_totals = sapply(wealth_y_vars, function(v) sum(w * y_pred_mat[, v]) / 1e12)
  q_nw_t = tapply(w * nw, bin_vec, sum) / 1e12
  ord = order(nw, decreasing = TRUE)
  cw = cumsum(w[ord])
  top1_mask  = cw <= 0.01 * sum(w)
  top10_mask = cw <= 0.10 * sum(w)
  top1_share  = sum(w[ord][top1_mask]  * nw[ord][top1_mask])  / sum(w * nw)
  top10_share = sum(w[ord][top10_mask] * nw[ord][top10_mask]) / sum(w * nw)
  list(label = label, total_nw = total, cat_totals = cat_totals,
       q_nw_t = q_nw_t, top1_share = top1_share, top10_share = top10_share)
}

truth  = aggregate_report(Y_scf, W_scf, scf_bin_reporting, 'SCF truth')
report = aggregate_report(Y_hat, W_scf, scf_bin_reporting, '8-cell per-cell')

cat('\n\n================================================\n')
cat('SIDE-BY-SIDE\n')
cat('================================================\n\n')

pctd = function(num, ref) if (ref == 0) NA_real_ else 100 * (num - ref) / ref

cat('Total NW ($T):\n')
cat(sprintf('  SCF truth:    %7.2f\n', truth$total_nw))
cat(sprintf('  per-cell:     %7.2f  (%+.1f%%)\n',
            report$total_nw, pctd(report$total_nw, truth$total_nw)))

cat('\nNW ($T) by SCF income rank bin:\n')
q_tbl = tibble(
  bin     = names(truth$q_nw_t),
  scf     = as.numeric(truth$q_nw_t),
  percell = as.numeric(report$q_nw_t)
) %>% mutate(
  diff_pct = round(100 * (percell - scf) / pmax(abs(scf), 1e-6), 1),
  scf      = round(scf, 2),
  percell  = round(percell, 2)
)
print(q_tbl, n = Inf)

cat('\nPer-category totals ($T):\n')
cat_tbl = tibble(
  category = wealth_y_vars,
  scf      = as.numeric(truth$cat_totals),
  percell  = as.numeric(report$cat_totals)
) %>% mutate(
  diff_pct = round(100 * (percell - scf) / pmax(abs(scf), 1e-6), 1),
  scf      = round(scf, 3),
  percell  = round(percell, 3)
)
print(cat_tbl, n = Inf)

cat('\nConcentration:\n')
cat(sprintf('  SCF truth:    top1 %5.3f   top10 %5.3f\n',
            truth$top1_share, truth$top10_share))
cat(sprintf('  per-cell:     top1 %5.3f   top10 %5.3f\n',
            report$top1_share, report$top10_share))


#--- Top-15 donor reachability --------------------------------------------

cat('\n\n================================================\n')
cat('Top-15 highest-NW SCF donors: picks under per-cell scheme\n')
cat('================================================\n\n')

top15 = order(scf_nw_truth, decreasing = TRUE)[1:15]
n_picks = sapply(top15, function(r) sum(picks_scf_row == r))
reach = tibble(
  scf_row = top15,
  cell    = scf_cell[top15],
  nw_B    = round(scf_nw_truth[top15] / 1e9, 2),
  weight  = round(W_scf[top15]),
  n_picks = n_picks
)
print(reach, n = Inf)

cat('\nDone.\n')
