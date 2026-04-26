#---------------------------------------------
# wealth_percell_minimal.R
#
# Minimal-features per-cell test. Strip the
# forest down to just pctile_income + age_older
# + age_younger. Within a cell, pctile_income
# barely varies, so the forest effectively
# routes on age only. Mechanical test of
# whether eliminating the features that differ
# between PUF and SCF (business, cap_gains,
# ss_pens) brings PUF aggregate back toward SCF.
#
# Usage:
#   Rscript src/eda/wealth_percell_minimal.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(drf);   library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/wealth.R')

features = c('pctile_income', 'age_older', 'age_younger')   # minimal

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')
n_boot_cell = c(
  'pct00to20'    = 100000L, 'pct20to40'    = 100000L,
  'pct40to60'    = 100000L, 'pct60to80'    = 100000L,
  'pct80to90'    = 100000L, 'pct90to99'    = 100000L,
  'pct99to99.9'  = 100000L, 'pct99.9to100' = 150000L
)


#--- SCF prep --------------------------------------------------------------

cat('Loading SCF tax units...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds') %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                          NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    pctile_income = compute_percentile(income, weight, FINE_PCTILE_PROBS)
  )

scf_y_df = scf_to_y(scf_tax_units)
X_scf = as.matrix(scf_tax_units[features])
Y_scf = as.matrix(scf_y_df[wealth_y_vars])
W_scf = scf_tax_units$weight
N_scf = nrow(X_scf)
scf_nw_truth = rowSums(Y_scf[, wealth_asset_vars]) - rowSums(Y_scf[, wealth_debt_vars])

ord = order(scf_tax_units$income)
cum_w = cumsum(W_scf[ord]) / sum(W_scf)
rank_scf = numeric(N_scf); rank_scf[ord] = 100 * cum_w
scf_cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]


#--- PUF prep --------------------------------------------------------------

cat('Loading PUF 2022...\n')
puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                show_col_types = FALSE) %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                          NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    income = wages + sole_prop + farm +
             scorp_active  - scorp_active_loss  - scorp_179 +
             scorp_passive - scorp_passive_loss +
             part_active   - part_active_loss   - part_179 +
             part_passive  - part_passive_loss +
             txbl_int + exempt_int + div_ord + div_pref +
             kg_lt + kg_st +
             gross_ss + gross_pens_dist + ui +
             rent - rent_loss + estate - estate_loss,
    pctile_income = compute_percentile(income, weight, FINE_PCTILE_PROBS)
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

cat(sprintf('  PUF: %d rows, pop %.1fM\n', N_puf, sum(W_puf)/1e6))


#--- Forest utilities ------------------------------------------------------

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


#--- Train + walk per cell ------------------------------------------------

estimate_models = 1L
picks_scf       = integer(N_scf)
picks_puf       = integer(N_puf)
picks_scf_boot  = integer(N_scf)   # uniform draw from cell bootstrap (no forest)
picks_puf_boot  = integer(N_puf)   # uniform draw from cell bootstrap (no forest)
t_all = Sys.time()

for (cell_name in CALIB_INCOME_BUCKETS) {
  cat(sprintf('\n--- cell %s ---\n', cell_name))
  seed_offset = which(CALIB_INCOME_BUCKETS == cell_name)

  cell_rows_training = which(scf_cell == cell_name)
  set.seed(100 + seed_offset)
  boot_in_cell = sample.int(length(cell_rows_training),
                             size = n_boot_cell[cell_name],
                             replace = TRUE,
                             prob = W_scf[cell_rows_training])
  boot_scf_row = cell_rows_training[boot_in_cell]
  X_boot = X_scf[boot_scf_row, , drop = FALSE]
  Y_boot = Y_scf[boot_scf_row, , drop = FALSE]

  cat('  Training DRF (3-feature minimal)...\n')
  t0 = Sys.time()
  drf_cell = train_or_load_drf(
    name          = paste0('wealth_percell_minimal_', cell_name),
    X             = X_boot,
    Y             = Y_boot,
    num.features  = 50,
    min.node.size = 20,
    mtry          = 3         # all three features at every split
  )
  cat(sprintf('    %.1fs\n', as.numeric(Sys.time() - t0, units = 'secs')))
  f_cell = extract_forest(drf_cell)

  # Walk SCF (self-test).
  scf_q = cell_rows_training
  picks_scf[scf_q] = walk_and_map(f_cell, X_scf, scf_q, boot_scf_row,
                                   seed = 200 + seed_offset)

  # Walk PUF.
  puf_q = which(puf_cell == cell_name)
  if (length(puf_q) > 0L)
    picks_puf[puf_q] = walk_and_map(f_cell, X_puf, puf_q, boot_scf_row,
                                     seed = 300 + seed_offset)

  # Bootstrap-only baseline: uniform draw from the cell's bootstrap (no
  # forest, no X-routing). Isolates the pure sampling-noise floor — any
  # difference between this and SCF truth is Monte Carlo / bootstrap
  # variance, not forest error.
  set.seed(400 + seed_offset)
  picks_scf_boot[scf_q] = boot_scf_row[
    sample.int(length(boot_scf_row), length(scf_q), replace = TRUE)]
  if (length(puf_q) > 0L) {
    set.seed(500 + seed_offset)
    picks_puf_boot[puf_q] = boot_scf_row[
      sample.int(length(boot_scf_row), length(puf_q), replace = TRUE)]
  }
}
cat(sprintf('\nTotal time: %.1fs\n', as.numeric(Sys.time() - t_all, units = 'secs')))


#--- Aggregate + compare --------------------------------------------------

scf_nw_forest    = scf_nw_truth[picks_scf]
puf_nw_forest    = scf_nw_truth[picks_puf]
scf_nw_boot_only = scf_nw_truth[picks_scf_boot]
puf_nw_boot_only = scf_nw_truth[picks_puf_boot]

cat('\n\n================================================\n')
cat('MINIMAL PER-CELL RESULTS\n')
cat('================================================\n\n')

cat('Total NW ($T):\n')
t_scf_truth   = sum(W_scf * scf_nw_truth)    / 1e12
t_scf_boot    = sum(W_scf * scf_nw_boot_only) / 1e12
t_scf_forest  = sum(W_scf * scf_nw_forest)   / 1e12
t_puf_boot    = sum(W_puf * puf_nw_boot_only) / 1e12
t_puf_forest  = sum(W_puf * puf_nw_forest)   / 1e12

pct = function(x, ref) round(100 * (x - ref) / ref, 1)

cat(sprintf('  SCF truth:                     %7.2f\n', t_scf_truth))
cat(sprintf('  SCF bootstrap (no forest):     %7.2f  (%+.1f%% vs truth)\n',
            t_scf_boot, pct(t_scf_boot, t_scf_truth)))
cat(sprintf('  SCF forest (minimal):          %7.2f  (%+.1f%% vs truth)\n',
            t_scf_forest, pct(t_scf_forest, t_scf_truth)))
cat(sprintf('  PUF bootstrap (no forest):     %7.2f  (%+.1f%% vs truth)\n',
            t_puf_boot, pct(t_puf_boot, t_scf_truth)))
cat(sprintf('  PUF forest (minimal):          %7.2f  (%+.1f%% vs truth)\n',
            t_puf_forest, pct(t_puf_forest, t_scf_truth)))
cat(sprintf('  [interpretation:\n'))
cat(sprintf('     SCF truth   → SCF boot:     sampling-noise floor\n'))
cat(sprintf('     SCF boot    → SCF forest:   forest routing on SCF X\n'))
cat(sprintf('     SCF boot    → PUF boot:     PUF pop-size / pop-composition effect\n'))
cat(sprintf('     PUF boot    → PUF forest:   forest routing on PUF X = X-shift ]\n'))

cat('\nPer-cell ($T):\n')
tbl = purrr::map_dfr(CALIB_INCOME_BUCKETS, function(cn) {
  sm = scf_cell == cn; pm = puf_cell == cn
  tibble(
    cell       = cn,
    scf_truth  = round(sum(W_scf[sm] * scf_nw_truth[sm]) / 1e12, 2),
    scf_boot   = round(sum(W_scf[sm] * scf_nw_boot_only[sm]) / 1e12, 2),
    scf_forest = round(sum(W_scf[sm] * scf_nw_forest[sm]) / 1e12, 2),
    puf_boot   = round(sum(W_puf[pm] * puf_nw_boot_only[pm]) / 1e12, 2),
    puf_forest = round(sum(W_puf[pm] * puf_nw_forest[pm]) / 1e12, 2)
  )
})
print(as.data.frame(tbl), row.names = FALSE)

# Concentration
concen = function(nw, w) {
  ord = order(nw, decreasing = TRUE)
  cw = cumsum(w[ord])
  top1  = sum(w[ord][cw <= 0.01 * sum(w)] * nw[ord][cw <= 0.01 * sum(w)]) / sum(w * nw)
  top10 = sum(w[ord][cw <= 0.10 * sum(w)] * nw[ord][cw <= 0.10 * sum(w)]) / sum(w * nw)
  c(top1 = top1, top10 = top10)
}
c_truth   = concen(scf_nw_truth,    W_scf)
c_scfbt   = concen(scf_nw_boot_only, W_scf)
c_scf     = concen(scf_nw_forest,    W_scf)
c_pufbt   = concen(puf_nw_boot_only, W_puf)
c_puf     = concen(puf_nw_forest,    W_puf)

cat('\nConcentration:\n')
cat(sprintf('  SCF truth:             top1 %5.3f  top10 %5.3f\n', c_truth[1], c_truth[2]))
cat(sprintf('  SCF bootstrap:         top1 %5.3f  top10 %5.3f\n', c_scfbt[1], c_scfbt[2]))
cat(sprintf('  SCF forest (minimal):  top1 %5.3f  top10 %5.3f\n', c_scf[1], c_scf[2]))
cat(sprintf('  PUF bootstrap:         top1 %5.3f  top10 %5.3f\n', c_pufbt[1], c_pufbt[2]))
cat(sprintf('  PUF forest (minimal):  top1 %5.3f  top10 %5.3f\n', c_puf[1], c_puf[2]))

cat('\nDone.\n')
