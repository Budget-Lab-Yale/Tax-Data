#---------------------------------------------
# wealth_percell_decomp.R
#
# Decomposition of the PUF +44% aggregate-NW
# overshoot under the per-cell architecture.
#
# Three diagnostics:
#   D1. Per-cell per-capita NW. Compare SCF
#       truth, forest-on-SCF (self-test), and
#       forest-on-PUF. Separates forest self-bias
#       from PUF X-shift per cell.
#   D2. Per-cell within-cell X-distribution.
#       For each (cell × key X feature), weighted
#       mean on SCF side vs PUF side. Flag
#       features with largest PUF/SCF gap.
#   D3. Global option-a counterfactual. Apply
#       the cached GLOBAL weighted-bootstrap
#       forest (wealth_drf_dim2_optionA) to PUF
#       and compare vs per-cell. Direct test of
#       whether stratification is what's
#       degrading PUF aggregate.
#
# Usage:
#   Rscript src/eda/wealth_percell_decomp.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(drf); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/wealth.R')

features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'age_younger', 'n_dep_hh',
             'pctile_wages', 'has_wages',
             'pctile_business', 'has_business_act', 'has_business_pos',
             'pctile_int_div', 'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act', 'has_capital_gains_pos',
             'pctile_ss_pens', 'has_ss_pens')

key_features_d2 = c('pctile_income', 'has_negative_income',
                    'pctile_wages', 'has_wages',
                    'pctile_business', 'has_business_pos', 'has_business_act',
                    'pctile_int_div', 'has_int_div',
                    'pctile_capital_gains', 'has_capital_gains_pos', 'has_capital_gains_act',
                    'pctile_ss_pens', 'has_ss_pens',
                    'age_older', 'age_younger', 'married', 'n_dep_hh')

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


#--- SCF -------------------------------------------------------------------

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
scf_nw_truth = rowSums(Y_scf[, wealth_asset_vars]) - rowSums(Y_scf[, wealth_debt_vars])

# SCF cells (own income rank)
ord = order(scf_tax_units$income)
cum_w = cumsum(W_scf[ord]) / sum(W_scf)
rank_scf = numeric(N_scf); rank_scf[ord] = 100 * cum_w
scf_cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]


#--- PUF -------------------------------------------------------------------

cat('Loading PUF 2022...\n')
puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'), show_col_types = FALSE) %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
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


#--- Forest walkers --------------------------------------------------------

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

walk_and_map = function(f, X_query, query_rows, boot_to_orig, seed) {
  set.seed(seed)
  out = integer(length(query_rows))
  tree_pick = sample.int(f$n_trees, size = length(query_rows), replace = TRUE)
  for (i in seq_along(query_rows)) {
    t  = tree_pick[i]
    nd = walk_to_leaf_one(f, t, X_query[query_rows[i], ])
    lr = f$leaves[[t]][[nd]] + 1L
    pick_boot = lr[sample.int(length(lr), 1L)]
    out[i] = boot_to_orig[pick_boot]
  }
  out
}


#--- Per-cell walks: SCF-self and PUF under the 8 per-cell forests --------

cat('\nWalking per-cell forests on both SCF and PUF...\n')

picks_scf_percell = integer(N_scf)
picks_puf_percell = integer(N_puf)

for (cell_name in CALIB_INCOME_BUCKETS) {
  seed_offset = which(CALIB_INCOME_BUCKETS == cell_name)

  # Reproduce training bootstrap exactly.
  cell_rows_training = which(scf_cell == cell_name)
  set.seed(100 + seed_offset)
  boot_in_cell = sample.int(length(cell_rows_training),
                             size = n_boot_cell[cell_name],
                             replace = TRUE,
                             prob = W_scf[cell_rows_training])
  boot_scf_row = cell_rows_training[boot_in_cell]

  drf_cell = read_rds(paste0('resources/cache/qrf/wealth_percell_',
                              cell_name, '.rds'))
  f_cell = extract_forest(drf_cell)

  scf_q = cell_rows_training
  picks_scf_percell[scf_q] = walk_and_map(f_cell, X_scf, scf_q,
                                           boot_scf_row,
                                           seed = 200 + seed_offset)

  puf_q = which(puf_cell == cell_name)
  if (length(puf_q) > 0L)
    picks_puf_percell[puf_q] = walk_and_map(f_cell, X_puf, puf_q,
                                             boot_scf_row,
                                             seed = 300 + seed_offset)

  cat(sprintf('  %s: SCF %d rows, PUF %d rows\n',
              cell_name, length(scf_q), length(puf_q)))
}

scf_nw_forest_percell = scf_nw_truth[picks_scf_percell]
puf_nw_forest_percell = scf_nw_truth[picks_puf_percell]


#--- D1: Per-cell per-capita NW --------------------------------------------

cat('\n\n================================================\n')
cat('D1. Per-cell per-capita NW ($K/capita)\n')
cat('================================================\n\n')

d1 = purrr::map_dfr(CALIB_INCOME_BUCKETS, function(cn) {
  sm = scf_cell == cn; pm = puf_cell == cn
  scf_pop = sum(W_scf[sm])
  puf_pop = sum(W_puf[pm])
  scf_truth_pc  = sum(W_scf[sm] * scf_nw_truth[sm]) / scf_pop
  scf_forest_pc = sum(W_scf[sm] * scf_nw_forest_percell[sm]) / scf_pop
  puf_forest_pc = sum(W_puf[pm] * puf_nw_forest_percell[pm]) / puf_pop
  tibble(
    cell             = cn,
    scf_pop_M        = round(scf_pop / 1e6, 1),
    puf_pop_M        = round(puf_pop / 1e6, 1),
    scf_truth_pc_K   = round(scf_truth_pc / 1e3, 1),
    scf_forest_pc_K  = round(scf_forest_pc / 1e3, 1),
    puf_forest_pc_K  = round(puf_forest_pc / 1e3, 1),
    forest_self_pct  = round(100 * (scf_forest_pc - scf_truth_pc) / scf_truth_pc, 1),
    puf_vs_scf_pct   = round(100 * (puf_forest_pc - scf_forest_pc) / scf_forest_pc, 1)
  )
})
print(as.data.frame(d1), row.names = FALSE)


#--- D2: Per-cell within-cell X-distribution -------------------------------

cat('\n\n================================================\n')
cat('D2. Within-cell X-feature distribution (SCF vs PUF)\n')
cat('    Weighted means.  Flag puf_minus_scf.\n')
cat('================================================\n\n')

for (cn in CALIB_INCOME_BUCKETS) {
  sm = scf_cell == cn; pm = puf_cell == cn
  sw = W_scf[sm];       pw = W_puf[pm]

  rows = purrr::map_dfr(key_features_d2, function(f) {
    sv = X_scf[sm, f];  pv = X_puf[pm, f]
    s_wmean = sum(sw * sv) / sum(sw)
    p_wmean = sum(pw * pv) / sum(pw)
    tibble(
      feature   = f,
      scf_mean  = round(s_wmean, 3),
      puf_mean  = round(p_wmean, 3),
      diff      = round(p_wmean - s_wmean, 3),
      pct_diff  = round(100 * (p_wmean - s_wmean) / pmax(abs(s_wmean), 1e-6), 1)
    )
  })

  cat(sprintf('\n--- %s ---\n', cn))
  print(as.data.frame(rows), row.names = FALSE)
}


#--- D3: Global option-a counterfactual ------------------------------------

cat('\n\n================================================\n')
cat('D3. Global option-a forest on PUF (counterfactual)\n')
cat('    Forest trained on 250k weighted SCF bootstrap (no stratification).\n')
cat('================================================\n\n')

# Recover the exact global bootstrap from dim2 run (set.seed(200), n=250000).
set.seed(200)
boot_idx_global = sample.int(N_scf, size = 250000L, replace = TRUE, prob = W_scf)

drf_global = read_rds('resources/cache/qrf/wealth_drf_dim2_optionA.rds')
f_global   = extract_forest(drf_global)
cat(sprintf('Global forest: %d trees\n', f_global$n_trees))

cat('Walking global forest on SCF (self-test, sanity)...\n')
picks_scf_global = walk_and_map(f_global, X_scf, seq_len(N_scf),
                                 boot_idx_global, seed = 500)

cat('Walking global forest on PUF...\n')
picks_puf_global = walk_and_map(f_global, X_puf, seq_len(N_puf),
                                 boot_idx_global, seed = 501)

scf_nw_global = scf_nw_truth[picks_scf_global]
puf_nw_global = scf_nw_truth[picks_puf_global]

scf_truth_total = sum(W_scf * scf_nw_truth) / 1e12
scf_global_total = sum(W_scf * scf_nw_global) / 1e12
puf_global_total = sum(W_puf * puf_nw_global) / 1e12
puf_percell_total = sum(W_puf * puf_nw_forest_percell) / 1e12

cat('\nTotal NW ($T):\n')
cat(sprintf('  SCF truth:                  %7.2f\n', scf_truth_total))
cat(sprintf('  Global-a forest on SCF:     %7.2f  (%+.1f%%)\n',
            scf_global_total, 100 * (scf_global_total - scf_truth_total) / scf_truth_total))
cat(sprintf('  Global-a forest on PUF:     %7.2f  (%+.1f%% vs SCF truth)\n',
            puf_global_total, 100 * (puf_global_total - scf_truth_total) / scf_truth_total))
cat(sprintf('  Per-cell forest on PUF:     %7.2f  (%+.1f%% vs SCF truth)\n',
            puf_percell_total, 100 * (puf_percell_total - scf_truth_total) / scf_truth_total))

# Per-cell comparison of the two PUF runs.
cat('\nPer-cell PUF NW ($T): global-a vs per-cell\n')
d3_tbl = purrr::map_dfr(CALIB_INCOME_BUCKETS, function(cn) {
  pm = puf_cell == cn
  sm = scf_cell == cn
  tibble(
    cell        = cn,
    scf_truth_T = round(sum(W_scf[sm] * scf_nw_truth[sm]) / 1e12, 2),
    puf_global_T  = round(sum(W_puf[pm] * puf_nw_global[pm]) / 1e12, 2),
    puf_percell_T = round(sum(W_puf[pm] * puf_nw_forest_percell[pm]) / 1e12, 2)
  ) %>% mutate(
    global_pct  = round(100 * (puf_global_T - scf_truth_T) / pmax(abs(scf_truth_T), 1e-6), 1),
    percell_pct = round(100 * (puf_percell_T - scf_truth_T) / pmax(abs(scf_truth_T), 1e-6), 1),
    stratif_cost_pp = round(percell_pct - global_pct, 1)  # how much stratification added
  )
})
print(as.data.frame(d3_tbl), row.names = FALSE)

cat('\nDone.\n')
