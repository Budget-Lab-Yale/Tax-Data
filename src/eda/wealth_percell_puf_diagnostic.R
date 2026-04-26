#---------------------------------------------
# wealth_percell_puf_diagnostic.R
#
# Apply the cached per-cell DRFs to PUF X and
# aggregate with PUF weights. Compare the PUF-
# imputed wealth distribution to SCF truth.
#
# This is the real validation: the per-cell
# architecture cleared the SCF self-consistency
# test at +0.4% aggregate. Now we check whether
# it holds up under PUF X-shift.
#
# Usage:
#   Rscript src/eda/wealth_percell_puf_diagnostic.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(drf);   library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript ... <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/wealth.R')   # for scf_to_y

set.seed(42)

features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'age_younger', 'n_dep_hh',
             'pctile_wages', 'has_wages',
             'pctile_business', 'has_business_act', 'has_business_pos',
             'pctile_int_div', 'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act', 'has_capital_gains_pos',
             'pctile_ss_pens', 'has_ss_pens')

make_has = function(x) case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')
n_boot_cell = c(
  'pct00to20'    = 100000L, 'pct20to40'    = 100000L,
  'pct40to60'    = 100000L, 'pct60to80'    = 100000L,
  'pct80to90'    = 100000L, 'pct90to99'    = 100000L,
  'pct99to99.9'  = 100000L, 'pct99.9to100' = 150000L
)


#--- Load + prepare SCF (same feature engineering as training) -------------

cat('Loading SCF tax units...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

scf_tax_units = scf_tax_units %>%
  mutate(
    n_dep_hh    = n_dep,
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
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

# SCF cell assignment for reproducing the training bootstraps.
ord = order(scf_tax_units$income)
cum_w = cumsum(W_scf[ord]) / sum(W_scf)
rank_0_100 = numeric(N_scf)
rank_0_100[ord] = 100 * cum_w
scf_cell_idx = findInterval(rank_0_100, CALIB_INCOME_EDGES,
                             rightmost.closed = TRUE, all.inside = TRUE)
scf_cell = CALIB_INCOME_BUCKETS[scf_cell_idx]

scf_nw_truth = rowSums(Y_scf[, wealth_asset_vars]) - rowSums(Y_scf[, wealth_debt_vars])


#--- Load + prepare PUF ----------------------------------------------------

cat('Loading PUF 2022 from ', output_dir, '...\n', sep = '')
puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
               show_col_types = FALSE)

cat(sprintf('  %d PUF rows, total pop weight %.1fM\n',
            nrow(puf), sum(puf$weight) / 1e6))

# Feature engineering — verbatim from wealth.R.
puf = puf %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                          NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    married     = as.integer(!is.na(male2)),
    n_dep_hh    = (!is.na(dep_age1) & dep_age1 < 18) +
                  (!is.na(dep_age2) & dep_age2 < 18) +
                  (!is.na(dep_age3) & dep_age3 < 18),
    income      = wages +
                  sole_prop + farm +
                  scorp_active  - scorp_active_loss  - scorp_179 +
                  scorp_passive - scorp_passive_loss +
                  part_active   - part_active_loss   - part_179 +
                  part_passive  - part_passive_loss +
                  txbl_int + exempt_int + div_ord + div_pref +
                  kg_lt + kg_st +
                  gross_ss + gross_pens_dist +
                  ui +
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

# PUF cell assignment on PUF's own weighted income rank.
puf_ord = order(puf$income)
puf_cum_w = cumsum(W_puf[puf_ord]) / sum(W_puf)
puf_rank = numeric(N_puf)
puf_rank[puf_ord] = 100 * puf_cum_w
puf_cell_idx = findInterval(puf_rank, CALIB_INCOME_EDGES,
                             rightmost.closed = TRUE, all.inside = TRUE)
puf_cell = CALIB_INCOME_BUCKETS[puf_cell_idx]

cat('\nPUF cell populations:\n')
puf_cell_pop = tapply(W_puf, puf_cell, sum) / 1e6
print(round(puf_cell_pop[CALIB_INCOME_BUCKETS], 2))


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


#--- Per-cell: load forest, recover boot index map, walk PUF --------------

cat('\n\n================================================\n')
cat('Per-cell walking on PUF X\n')
cat('================================================\n')

picks_scf_row = integer(N_puf)
t_total_start = Sys.time()

for (cell_name in CALIB_INCOME_BUCKETS) {
  cat(sprintf('\n--- cell %s ---\n', cell_name))

  # Recover the exact training bootstrap by re-running the same RNG sequence
  # used in wealth_percell_diagnostic.R. Same seed, same cell mask, same
  # prob-vector → identical boot_scf_row. This lets us map forest leaf
  # indices back to SCF row indices for Y lookup.
  seed_offset = which(CALIB_INCOME_BUCKETS == cell_name)
  cell_rows_scf = which(scf_cell == cell_name)
  cell_weights_scf = W_scf[cell_rows_scf]
  set.seed(100 + seed_offset)
  boot_in_cell = sample.int(length(cell_rows_scf),
                             size = n_boot_cell[cell_name],
                             replace = TRUE, prob = cell_weights_scf)
  boot_scf_row = cell_rows_scf[boot_in_cell]

  # Load the cached forest.
  drf_cell = read_rds(paste0('resources/cache/qrf/wealth_percell_',
                              cell_name, '.rds'))
  f_cell = extract_forest(drf_cell)

  # PUF rows in this cell.
  puf_rows_cell = which(puf_cell == cell_name)
  cat(sprintf('  PUF rows in cell: %d (pop %.2fM)\n',
              length(puf_rows_cell),
              sum(W_puf[puf_rows_cell]) / 1e6))

  if (length(puf_rows_cell) == 0L) next

  cell_X_puf = X_puf[puf_rows_cell, , drop = FALSE]
  set.seed(300 + seed_offset)
  tree_pick = sample.int(f_cell$n_trees, size = length(puf_rows_cell),
                          replace = TRUE)

  t0 = Sys.time()
  for (i in seq_along(puf_rows_cell)) {
    t  = tree_pick[i]
    nd = walk_to_leaf_one(f_cell, t, cell_X_puf[i, ])
    lr = f_cell$leaves[[t]][[nd]] + 1L       # indices into boot
    pick_boot = lr[sample.int(length(lr), 1L)]
    picks_scf_row[puf_rows_cell[i]] = boot_scf_row[pick_boot]
  }
  cat(sprintf('  walk %.1fs\n', as.numeric(Sys.time() - t0, units = 'secs')))
}
cat(sprintf('\nTotal walk time: %.1fs\n',
            as.numeric(Sys.time() - t_total_start, units = 'secs')))


#--- Aggregate and compare to SCF truth -----------------------------------

Y_hat_puf = Y_scf[picks_scf_row, , drop = FALSE]

# Reporting bins for PUF side — use PUF's own income distribution.
to_bin = function(pct) {
  cut(pct, breaks = c(-1, 19, 39, 59, 79, 89, 98, 100),
      labels = c('p0-20','p20-40','p40-60','p60-80','p80-90','p90-99','top1'),
      include.lowest = TRUE, right = TRUE)
}
scf_pct  = compute_percentile(scf_tax_units$income, W_scf, seq(0.01, 0.99, 0.01))
puf_pct  = compute_percentile(puf$income, W_puf, seq(0.01, 0.99, 0.01))
scf_bin  = to_bin(scf_pct)
puf_bin  = to_bin(puf_pct)

aggregate_report = function(y_pred_mat, w, bin_vec) {
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
  list(total_nw = total, cat_totals = cat_totals,
       q_nw_t = q_nw_t, top1_share = top1_share, top10_share = top10_share)
}

truth  = aggregate_report(Y_scf, W_scf, scf_bin)
report = aggregate_report(Y_hat_puf, W_puf, puf_bin)

cat('\n\n================================================\n')
cat('PUF-IMPUTED vs SCF TRUTH\n')
cat('================================================\n\n')

pctd = function(num, ref) if (ref == 0) NA_real_ else 100 * (num - ref) / ref

cat('Total NW ($T):\n')
cat(sprintf('  SCF truth (pop %.1fM):    %7.2f\n',
            sum(W_scf)/1e6, truth$total_nw))
cat(sprintf('  PUF imputed (pop %.1fM):  %7.2f  (%+.1f%% vs SCF)\n',
            sum(W_puf)/1e6, report$total_nw,
            pctd(report$total_nw, truth$total_nw)))

cat('\nNW ($T) by income rank bin (each on its own dataset\'s rank):\n')
q_tbl = tibble(
  bin = names(truth$q_nw_t),
  scf = as.numeric(truth$q_nw_t),
  puf = as.numeric(report$q_nw_t)
) %>% mutate(
  diff_pct = round(100 * (puf - scf) / pmax(abs(scf), 1e-6), 1),
  scf      = round(scf, 2),
  puf      = round(puf, 2)
)
print(q_tbl, n = Inf)

cat('\nPer-category totals ($T):\n')
cat_tbl = tibble(
  category = wealth_y_vars,
  scf = as.numeric(truth$cat_totals),
  puf = as.numeric(report$cat_totals)
) %>% mutate(
  diff_pct = round(100 * (puf - scf) / pmax(abs(scf), 1e-6), 1),
  scf      = round(scf, 3),
  puf      = round(puf, 3)
)
print(cat_tbl, n = Inf)

cat('\nConcentration:\n')
cat(sprintf('  SCF truth:   top1 %5.3f   top10 %5.3f\n',
            truth$top1_share, truth$top10_share))
cat(sprintf('  PUF imputed: top1 %5.3f   top10 %5.3f\n',
            report$top1_share, report$top10_share))


#--- Top-NW-donor reachability on PUF side --------------------------------

cat('\n\n================================================\n')
cat('Top-15 highest-NW SCF donors: picks across PUF records\n')
cat('================================================\n\n')

top15 = order(scf_nw_truth, decreasing = TRUE)[1:15]
n_picks = sapply(top15, function(r) sum(picks_scf_row == r))
wt_picks = sapply(top15, function(r) sum(W_puf[picks_scf_row == r]))
reach = tibble(
  scf_row     = top15,
  cell        = scf_cell[top15],
  nw_B        = round(scf_nw_truth[top15] / 1e9, 2),
  scf_weight  = round(W_scf[top15]),
  n_puf_picks = n_picks,
  puf_wt_repped_M = round(wt_picks / 1e6, 2)
)
print(reach, n = Inf)

cat('\nDone.\n')
