#---------------------------------------------
# mns50_warmstart_test.R
#
# Tests the recommended combination from
# docs/swap_vs_tilt_experiment.md: bigger DRF
# leaves (mns=50) + swap warm-started from
# tilt's output, on the same forest. The two
# tweaks were the only single-axis wins in the
# previous sweep, but they were tested separately.
# This script runs them together and compares
# against the existing baselines.
#
# Arms:
#   tilt_mns20      — production baseline, mns=20 (existing cache)
#   tilt_mns50      — tilt on bigger leaves
#   swap_mns50      — swap on bigger leaves (uniform init)
#   swap_mns50_ws   — swap on bigger leaves, warm-started from tilt_mns50
#
# Expected: swap_mns50_ws is the new champion on count match without
# losing top shares or joint correlation. If it doesn't beat both
# swap_mns50 and the prior best (swap_mns20_warmstart at 44.8% within
# ±5%), we have hit a real ceiling and the right next step is a hybrid
# solver or coarsened cell grid.
#
# Usage:
#   Rscript src/eda/mns50_warmstart_test.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript src/eda/mns50_warmstart_test.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

# mns50 forest already cached from the prior Run C, so estimate_models = 0L.
estimate_models = 0L
do_lp           = 0L

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')


#--- Load inputs -----------------------------------------------------------

cat('Loading inputs...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')
snap_path = file.path(output_dir, 'puf_2022_snapshot.rds')
if (file.exists(snap_path)) {
  puf_2022 = read_rds(snap_path)
} else {
  puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                      show_col_types = FALSE)
  for (v in wealth_y_vars) puf_2022[[v]] = NA_real_
}

source('src/imputations/wealth.R')


#--- Run arms --------------------------------------------------------------

run_arm = function(label, ...) {
  cat(sprintf('\n========= ARM: %s =========\n', label))
  set.seed(76)
  t0 = Sys.time()
  result = run_wealth_imputation(puf_2022, scf_tax_units, ...)
  result$arm = label
  result$elapsed_secs = as.numeric(Sys.time() - t0, units = 'secs')
  cat(sprintf('Arm %s: total %.1fs\n', label, result$elapsed_secs))
  result
}

# tilt @ mns20 — existing production cache, also our reference baseline.
res_tilt_20 = run_arm('tilt_mns20',
                       stage3_method = 'tilt', min_node_size = 20L)

# tilt @ mns50 — needed both as its own arm AND to provide the warm-start
# init for swap_mns50_ws. The tilt loop hasn't been run on mns50 leaves
# before this experiment.
res_tilt_50 = run_arm('tilt_mns50',
                       stage3_method = 'tilt', min_node_size = 50L)

# swap @ mns50, uniform init — already known from Run C, rerun for clean
# side-by-side numbers.
res_swap_50 = run_arm('swap_mns50',
                       stage3_method = 'swap', min_node_size = 50L)

# swap @ mns50, warm-started from tilt @ mns50 — the new combination.
# init donors come from tilt's per-cell solve on the same forest leaves,
# so every init donor is guaranteed to be in the corresponding mns50 leaf.
res_swap_50_ws = run_arm('swap_mns50_ws',
                          stage3_method = 'swap', min_node_size = 50L,
                          init_donors_override = res_tilt_50$step_a_donors)

arms = list(res_tilt_20, res_tilt_50, res_swap_50, res_swap_50_ws)
names(arms) = sapply(arms, `[[`, 'arm')


#--- Build cell-keyed dataframes -------------------------------------------

cat('\n========= Building cell-keyed dataframes =========\n')

puf_w = puf_2022$weight
puf_age2 = ifelse(is.na(puf_2022$age2), 0L, puf_2022$age2)
puf_age_older = pmax(pmin(80L, puf_2022$age1), pmin(80L, puf_age2))
puf_income_vec = with(puf_2022,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)
puf_meta = data.frame(id = puf_2022$id, weight = puf_w,
                      age_older = puf_age_older, income = puf_income_vec)
puf_meta = assign_calibration_cells(puf_meta, puf_meta$income,
                                    puf_meta$age_older, puf_meta$weight)

attach_meta = function(df_y) {
  df_y %>% inner_join(puf_meta %>% select(id, weight, cell_income, cell_age),
                       by = 'id') %>%
    bind_cols(compute_category_values(.))
}

scf_y    = scf_to_y(scf_tax_units)
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_ao   = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_meta = data.frame(weight = scf_y$weight, age_older = scf_ao, income = scf_inc)
scf_meta = cbind(scf_meta, compute_category_values(scf_y))
scf_meta = assign_calibration_cells(scf_meta, scf_meta$income,
                                    scf_meta$age_older, scf_meta$weight)


#--- Per-arm per-(cell × age × cat) stats ----------------------------------

CATS = c('nw','equities','bonds','homes','other','debt')

cell_cat_stats = function(df, source_label) {
  out = list()
  for (cc in CATS) {
    col = paste0('cat_', cc)
    s = df %>%
      group_by(cell_income, cell_age) %>%
      summarise(count_wt = sum(weight * (.data[[col]] > 0)),
                total    = sum(weight * .data[[col]]),
                .groups  = 'drop') %>%
      mutate(category = cc, source = source_label)
    out[[cc]] = s
  }
  bind_rows(out)
}

scf_stats = cell_cat_stats(scf_meta, 'SCF')

err_for = function(res) {
  s = cell_cat_stats(attach_meta(res$y), res$arm)
  s %>% inner_join(scf_stats %>% select(cell_income, cell_age, category,
                                         count_scf = count_wt, total_scf = total),
                    by = c('cell_income','cell_age','category')) %>%
    mutate(count_err = abs(count_wt - count_scf) / pmax(count_scf, 1),
           total_err = abs(total - total_scf)    / pmax(abs(total_scf), 1))
}

errs = lapply(arms, err_for)


#--- Match-quality summary -------------------------------------------------

cat('\n========= Match-quality summary (count error) =========\n\n')

quality = function(err) {
  vapply(c(0.01, 0.05, 0.10, 0.25, 0.50),
         function(t) 100 * mean(err$count_err <= t, na.rm = TRUE),
         numeric(1))
}

q_tbl = data.frame(
  arm = names(arms),
  pct_within_1pct  = round(sapply(errs, function(e) quality(e)[1]), 1),
  pct_within_5pct  = round(sapply(errs, function(e) quality(e)[2]), 1),
  pct_within_10pct = round(sapply(errs, function(e) quality(e)[3]), 1),
  pct_within_25pct = round(sapply(errs, function(e) quality(e)[4]), 1),
  pct_within_50pct = round(sapply(errs, function(e) quality(e)[5]), 1),
  elapsed_secs     = round(sapply(arms, `[[`, 'elapsed_secs'), 1),
  stringsAsFactors = FALSE
)
print(q_tbl, row.names = FALSE)


#--- Top NW shares ---------------------------------------------------------

cat('\n========= Top NW shares + Gini (Δ vs SCF in pp) =========\n\n')

weighted_quantile = function(x, w, probs) {
  o = order(x); x = x[o]; w = w[o]
  p = cumsum(w) / sum(w); approx(p, x, xout = probs, rule = 2)$y
}
top_share = function(x, w, top_frac) {
  cutoff = weighted_quantile(x, w, 1 - top_frac)
  above  = x >= cutoff
  sum(w[above] * x[above]) / sum(w * x)
}
weighted_gini = function(x, w) {
  o = order(x); x = x[o]; w = w[o]; W = sum(w); mu = sum(w*x)/W
  cw = cumsum(w) - 0.5*w
  sum(w*x*(2*cw-W)) / (W^2*mu)
}

topshare_tbl = function(df) c(
  top_001 = top_share(df$cat_nw, df$weight, 0.001),
  top_01  = top_share(df$cat_nw, df$weight, 0.01),
  top_1   = top_share(df$cat_nw, df$weight, 0.05),
  top_10  = top_share(df$cat_nw, df$weight, 0.10),
  gini    = weighted_gini(df$cat_nw, df$weight)
)
scf_vals = topshare_tbl(scf_meta)
cat(sprintf('SCF actuals: top0.1=%.3f  top1=%.3f  top10=%.3f  gini=%.3f\n',
            scf_vals['top_001'], scf_vals['top_01'],
            scf_vals['top_10'], scf_vals['gini']))

ts_rows = lapply(arms, function(res) {
  v = topshare_tbl(attach_meta(res$y))
  data.frame(
    arm = res$arm,
    delta_top_001_pp = round(100 * (v['top_001'] - scf_vals['top_001']), 2),
    delta_top_01_pp  = round(100 * (v['top_01']  - scf_vals['top_01']),  2),
    delta_top_1_pp   = round(100 * (v['top_1']   - scf_vals['top_1']),   2),
    delta_top_10_pp  = round(100 * (v['top_10']  - scf_vals['top_10']),  2),
    delta_gini_pp    = round(100 * (v['gini']    - scf_vals['gini']),    2),
    stringsAsFactors = FALSE
  )
})
print(do.call(rbind, ts_rows), row.names = FALSE)


#--- Joint correlation matrix L1 distance ----------------------------------

cat('\n========= Joint correlation matrix vs SCF (L1 distance) =========\n')

cor_cats = c('cat_equities','cat_bonds','cat_homes','cat_other','cat_debt')
weighted_cor = function(df, vars) {
  m = as.matrix(df[, vars])
  w = df$weight
  cov.wt(m, wt = w, cor = TRUE)$cor
}

cor_scf = weighted_cor(scf_meta, cor_cats)
l1 = function(a, b) sum(abs(a - b))

l1_tbl = data.frame(
  arm = names(arms),
  l1_dist_to_scf = round(sapply(arms, function(res) {
    l1(weighted_cor(attach_meta(res$y), cor_cats), cor_scf)
  }), 3)
)
print(l1_tbl, row.names = FALSE)


#--- Convergence status (swap arms only) -----------------------------------

cat('\n========= Convergence status (swap arms) =========\n')

`%||%` = function(a, b) if (!is.null(a)) a else b
status_table = function(res) {
  d = res$step_a_diagnostics
  st = sapply(d, function(x) x$status %||% 'tilt')
  if (all(st == 'tilt')) return(NULL)
  tibble(
    arm        = res$arm,
    converged  = sum(st == 'converged'),
    stuck      = sum(st == 'stuck'),
    cap        = sum(st == 'cap'),
    n_buckets  = length(d),
    median_max_rel = round(median(sapply(d, function(x)
                              x$final_max_rel %||% NA_real_), na.rm = TRUE), 4)
  )
}
print(as.data.frame(bind_rows(lapply(arms, status_table))),
      row.names = FALSE)


#--- Worst-bucket comparison -----------------------------------------------

cat('\n========= Worst buckets per arm (count error) =========\n\n')

err_wide = errs[[1]] %>%
  select(cell_income, cell_age, category, count_scf)
for (a in names(errs)) {
  err_wide[[paste0('err_', a)]] = round(errs[[a]]$count_err, 3)
}
worst = err_wide %>%
  rowwise() %>%
  mutate(max_err = max(c_across(starts_with('err_')), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(max_err > 0.25) %>%
  arrange(desc(err_tilt_mns20)) %>%
  head(15)
print(as.data.frame(worst), row.names = FALSE)


#--- Save ------------------------------------------------------------------

results = list(
  output_dir   = output_dir,
  quality_tbl  = q_tbl,
  topshare_tbl = do.call(rbind, ts_rows),
  l1_tbl       = l1_tbl,
  errs         = errs,
  worst        = worst,
  per_arm_diag = setNames(lapply(arms, `[[`, 'step_a_diagnostics'),
                          names(arms)),
  scf_top_shares = scf_vals
)
out_path = file.path(output_dir, 'mns50_warmstart_results.rds')
write_rds(results, out_path)
cat(sprintf('\nWrote %s\n', out_path))

cat('\nDone.\n')
