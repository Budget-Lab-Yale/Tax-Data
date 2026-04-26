#---------------------------------------------
# swap_vs_tilt_compare.R
#
# Side-by-side comparison harness for Stage 3
# Step A: legacy exponential-tilt vs new
# randomized-swap solver. Runs the wealth pipeline
# twice (loading the same forest cache for both)
# and reports per-bucket count match, top shares,
# joint correlations, and per-bucket convergence
# status.
#
# Usage:
#   Rscript src/eda/swap_vs_tilt_compare.R <output_dir>
#
# Side effects: writes the structured summary to
# `<output_dir>/swap_vs_tilt_results.rds` so the
# md report can be generated downstream.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop('Usage: Rscript src/eda/swap_vs_tilt_compare.R <output_dir> [min_node_size]')
output_dir    = args[1]
mns_arg       = if (length(args) >= 2L) as.integer(args[2]) else 20L
stopifnot(dir.exists(output_dir), mns_arg >= 1L)

# When mns != 20 the per-cell forest cache doesn't yet exist; let
# train_or_load_drf train and persist it. For mns == 20 reuse the
# production cache. The Run C trigger (mns=50) lives in slurm_swap_compare_mns50.sh.
estimate_models = if (mns_arg != 20L) 1L else 0L
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
  cat('No puf_2022_snapshot.rds; reconstructing from tax_units_2022.csv ',
      'by nulling wealth columns.\n', sep = '')
  puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                      show_col_types = FALSE)
  for (v in wealth_y_vars) puf_2022[[v]] = NA_real_
}

source('src/imputations/wealth.R')


#--- Run both methods ------------------------------------------------------

run_one = function(method, mns) {
  cat(sprintf('\n========= ARM: method=%s  min_node_size=%d =========\n',
              method, mns))
  set.seed(76)
  t0 = Sys.time()
  result = run_wealth_imputation(puf_2022, scf_tax_units,
                                  stage3_method = method,
                                  min_node_size = mns)
  cat(sprintf('Total time %.1fs\n',
              as.numeric(Sys.time() - t0, units = 'secs')))
  result
}

# Tilt arm always uses the production-default forest (mns=20). Swap arm
# uses mns_arg — same forest as tilt under default invocation, or a bigger-
# leaf forest under Run C.
res_tilt = run_one('tilt', 20L)
res_swap = run_one('swap', mns_arg)


#--- Build per-row PUF + SCF frames with cells + cat_* columns -------------

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
post_tilt = attach_meta(res_tilt$y)
post_swap = attach_meta(res_swap$y)
pre       = attach_meta(res_tilt$y_pre_tilt)   # same uniform draw under both

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


#--- Per (cell × age × cat) count + total stats ----------------------------

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

scf_stats  = cell_cat_stats(scf_meta,  'SCF')
pre_stats  = cell_cat_stats(pre,       'pre')
tilt_stats = cell_cat_stats(post_tilt, 'tilt')
swap_stats = cell_cat_stats(post_swap, 'swap')


#--- Match-quality summary --------------------------------------------------

merge_stats = function(arm_stats, label) {
  arm_stats %>%
    inner_join(scf_stats %>% select(cell_income, cell_age, category,
                                     count_scf = count_wt, total_scf = total),
                by = c('cell_income','cell_age','category')) %>%
    mutate(count_err = abs(count_wt - count_scf) / pmax(count_scf, 1),
           total_err = abs(total - total_scf)    / pmax(abs(total_scf), 1),
           arm = label)
}
err_tilt = merge_stats(tilt_stats, 'tilt')
err_swap = merge_stats(swap_stats, 'swap')

quality = function(err) {
  tibble(
    tol_pct = c(1, 5, 10, 25, 50),
    count_within = vapply(c(0.01, 0.05, 0.10, 0.25, 0.50),
                          function(t) 100 * mean(err$count_err <= t, na.rm = TRUE),
                          numeric(1)),
    total_within = vapply(c(0.01, 0.05, 0.10, 0.25, 0.50),
                          function(t) 100 * mean(err$total_err <= t, na.rm = TRUE),
                          numeric(1))
  )
}

qual_tilt = quality(err_tilt)
qual_swap = quality(err_swap)

cat('\n========= Match-quality summary =========\n')
cat('\n--- TILT (legacy) ---\n')
for (i in seq_len(nrow(qual_tilt))) {
  cat(sprintf('  ±%4.0f%% : count=%4.1f%%  total=%4.1f%%\n',
              qual_tilt$tol_pct[i],
              qual_tilt$count_within[i], qual_tilt$total_within[i]))
}
cat('\n--- SWAP (new) ---\n')
for (i in seq_len(nrow(qual_swap))) {
  cat(sprintf('  ±%4.0f%% : count=%4.1f%%  total=%4.1f%%\n',
              qual_swap$tol_pct[i],
              qual_swap$count_within[i], qual_swap$total_within[i]))
}

cat('\n--- Δ (swap − tilt), positive = swap better ---\n')
for (i in seq_len(nrow(qual_swap))) {
  cat(sprintf('  ±%4.0f%% : count=%+5.1fpp  total=%+5.1fpp\n',
              qual_swap$tol_pct[i],
              qual_swap$count_within[i] - qual_tilt$count_within[i],
              qual_swap$total_within[i] - qual_tilt$total_within[i]))
}


#--- Worst buckets, side by side -------------------------------------------

cat('\n========= Worst (cell × age × cat) buckets by count error =========\n\n')
worst = err_tilt %>% select(cell_income, cell_age, category, count_scf,
                             tilt_count = count_wt, tilt_err = count_err) %>%
  inner_join(err_swap %>% select(cell_income, cell_age, category,
                                  swap_count = count_wt, swap_err = count_err),
              by = c('cell_income','cell_age','category')) %>%
  arrange(desc(pmax(tilt_err, swap_err))) %>%
  slice_head(n = 15) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(worst), row.names = FALSE)


#--- Top shares + Gini -----------------------------------------------------

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

cat('\n========= Top NW shares + Gini =========\n')
top_tbl = function(df, lab) tibble(
  source = lab,
  top_001 = top_share(df$cat_nw, df$weight, 0.001),
  top_01  = top_share(df$cat_nw, df$weight, 0.01),
  top_1   = top_share(df$cat_nw, df$weight, 0.05),
  top_10  = top_share(df$cat_nw, df$weight, 0.10),
  gini    = weighted_gini(df$cat_nw, df$weight)
)
top_summary = bind_rows(
  top_tbl(scf_meta,  'SCF'),
  top_tbl(pre,       'pre'),
  top_tbl(post_tilt, 'tilt'),
  top_tbl(post_swap, 'swap')
) %>% mutate(across(-source, ~ round(.x, 4)))
print(top_summary)

cat('\n--- Δ vs SCF in pp ---\n')
scf_vals = top_summary %>% filter(source == 'SCF') %>%
  select(-source) %>% unlist()
delta_tbl = top_summary %>%
  mutate(across(-source,
                ~ round(100 * (.x - scf_vals[dplyr::cur_column()]), 2)))
print(delta_tbl)


#--- Joint correlation matrix L1 distance ----------------------------------

cat('\n========= Joint correlation matrix vs SCF (L1 distance) =========\n')

cor_cats = c('cat_equities','cat_bonds','cat_homes','cat_other','cat_debt')
weighted_cor = function(df, vars) {
  m = as.matrix(df[, vars])
  w = df$weight
  cov.wt(m, wt = w, cor = TRUE)$cor
}

cor_scf  = weighted_cor(scf_meta,  cor_cats)
cor_pre  = weighted_cor(pre,       cor_cats)
cor_tilt = weighted_cor(post_tilt, cor_cats)
cor_swap = weighted_cor(post_swap, cor_cats)

l1 = function(a, b) sum(abs(a - b))
cat(sprintf('L1(pre  − SCF)  = %.3f\n', l1(cor_pre,  cor_scf)))
cat(sprintf('L1(tilt − SCF)  = %.3f\n', l1(cor_tilt, cor_scf)))
cat(sprintf('L1(swap − SCF)  = %.3f\n', l1(cor_swap, cor_scf)))

cat('\nSCF correlation matrix:\n');  print(round(cor_scf, 3))
cat('\nTilt − SCF (entrywise):\n');  print(round(cor_tilt - cor_scf, 3))
cat('\nSwap − SCF (entrywise):\n');  print(round(cor_swap - cor_scf, 3))


#--- Per-bucket convergence status (swap arm only) -------------------------

cat('\n========= Swap solver: per-bucket status =========\n')
diag_swap = res_swap$step_a_diagnostics
diag_summary = lapply(names(diag_swap), function(k) {
  d = diag_swap[[k]]
  parts = strsplit(k, ':')[[1]]
  tibble(cell_income = parts[1], cell_age = parts[2],
         n = d$n, k_targets = d$k, status = d$status,
         swaps_accepted = d$swaps_accepted,
         total_proposals = d$total_proposals,
         final_max_rel = round(d$final_max_rel, 4))
}) %>% bind_rows()
print(as.data.frame(diag_summary), row.names = FALSE)

n_stuck = sum(diag_summary$status == 'stuck')
n_cap   = sum(diag_summary$status == 'cap')
n_conv  = sum(diag_summary$status == 'converged')
cat(sprintf('\nStatus: converged=%d  stuck=%d  cap=%d (of %d buckets)\n',
            n_conv, n_stuck, n_cap, nrow(diag_summary)))


#--- Save structured results -----------------------------------------------

results = list(
  output_dir       = output_dir,
  qual_tilt        = qual_tilt,
  qual_swap        = qual_swap,
  err_tilt         = err_tilt,
  err_swap         = err_swap,
  worst_buckets    = worst,
  top_summary      = top_summary,
  cor_scf          = cor_scf,
  cor_pre          = cor_pre,
  cor_tilt         = cor_tilt,
  cor_swap         = cor_swap,
  l1_pre_scf       = l1(cor_pre,  cor_scf),
  l1_tilt_scf      = l1(cor_tilt, cor_scf),
  l1_swap_scf      = l1(cor_swap, cor_scf),
  swap_diagnostics = diag_summary,
  tilt_diagnostics = res_tilt$step_a_diagnostics,
  rescale_factors_tilt = res_tilt$rescale_factors,
  rescale_factors_swap = res_swap$rescale_factors,
  qc_report        = res_swap$qc_report,
  min_node_size    = mns_arg
)
out_suffix = if (mns_arg != 20L) sprintf('_mns%d', mns_arg) else ''
out_path = file.path(output_dir,
                     paste0('swap_vs_tilt_results', out_suffix, '.rds'))
write_rds(results, out_path)
cat(sprintf('\nWrote %s\n', out_path))

cat('\nDone.\n')
