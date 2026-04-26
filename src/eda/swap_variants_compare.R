#---------------------------------------------
# swap_variants_compare.R
#
# Sweep over algorithmic variants of the
# swap-based Stage 3 Step A. The point is to
# tell whether the swap idea has a fundamental
# ceiling (donor pool exhausted) or whether
# specific tweaks unlock substantially better
# count match. Variants are listed in the table
# below with the hypothesis each one tests.
#
#   variant            tests
#   ─────────────────  ─────────────────────────
#   tilt               legacy baseline
#   swap_default       reproduces Run B (mns20)
#   swap_warmstart     init from tilt's output
#   swap_anneal        light annealing escapes
#                      L1 plateaus
#   swap_l2            squared-error objective —
#                      smoother landscape
#   swap_guided        target-driven proposal —
#                      bias toward error-reducing
#                      records
#   swap_long          5M iter cap + 50k stuck
#                      (just give it more rope)
#   swap_multistart    5 random restarts — local
#                      minima vs feasibility
#   swap_combo         warmstart + guided + anneal
#                      (best per-axis tweaks)
#
# Usage:
#   Rscript src/eda/swap_variants_compare.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript src/eda/swap_variants_compare.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

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


#--- Variant definitions ---------------------------------------------------

run_variant = function(label, ...) {
  cat(sprintf('\n========= VARIANT: %s =========\n', label))
  set.seed(76)
  t0 = Sys.time()
  result = run_wealth_imputation(puf_2022, scf_tax_units, ...)
  result$variant = label
  result$elapsed_secs = as.numeric(Sys.time() - t0, units = 'secs')
  cat(sprintf('Variant %s: total %.1fs\n', label, result$elapsed_secs))
  result
}


#--- Tilt baseline (provides warm-start init_donors_override) --------------

res_tilt = run_variant('tilt',
                        stage3_method = 'tilt',
                        min_node_size = 20L)
tilt_donors = res_tilt$step_a_donors


#--- Swap variants ---------------------------------------------------------

res_swap_default = run_variant('swap_default',
                                stage3_method = 'swap',
                                min_node_size = 20L)

res_swap_warmstart = run_variant('swap_warmstart',
                                  stage3_method = 'swap',
                                  min_node_size = 20L,
                                  init_donors_override = tilt_donors)

# Annealing: T0 sized so a typical "barely worse" Δerr (≈ 1/n_records of
# initial error) accepts with prob ~0.5. n_records is bucket-dependent
# but ~1k–30k; T0 = 0.1 gives a reasonable mid-range. anneal_iters chosen
# so cooling completes in the first ~20% of the proposal budget.
res_swap_anneal = run_variant('swap_anneal',
                               stage3_method = 'swap',
                               min_node_size = 20L,
                               swap_options = list(
                                 anneal_T0    = 0.1,
                                 anneal_iters = 100000L
                               ))

res_swap_l2 = run_variant('swap_l2',
                           stage3_method = 'swap',
                           min_node_size = 20L,
                           swap_options = list(objective = 'l2'))

res_swap_guided = run_variant('swap_guided',
                               stage3_method = 'swap',
                               min_node_size = 20L,
                               swap_options = list(proposal_strategy = 'guided'))

res_swap_long = run_variant('swap_long',
                             stage3_method = 'swap',
                             min_node_size = 20L,
                             swap_options = list(
                               max_iters      = 5000000L,
                               n_trials_stuck = 50000L
                             ))

res_swap_multistart = run_variant('swap_multistart',
                                   stage3_method = 'swap',
                                   min_node_size = 20L,
                                   n_restarts = 5L)

res_swap_combo = run_variant('swap_combo',
                              stage3_method = 'swap',
                              min_node_size = 20L,
                              init_donors_override = tilt_donors,
                              swap_options = list(
                                proposal_strategy = 'guided',
                                anneal_T0         = 0.1,
                                anneal_iters      = 100000L,
                                max_iters         = 2000000L,
                                n_trials_stuck    = 20000L
                              ))


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


#--- Per-variant per-(cell×age×cat) stats ----------------------------------

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

stats_for = function(res) cell_cat_stats(attach_meta(res$y), res$variant)
err_for   = function(res) {
  s = stats_for(res)
  s %>% inner_join(scf_stats %>% select(cell_income, cell_age, category,
                                         count_scf = count_wt, total_scf = total),
                    by = c('cell_income','cell_age','category')) %>%
    mutate(count_err = abs(count_wt - count_scf) / pmax(count_scf, 1),
           total_err = abs(total - total_scf)    / pmax(abs(total_scf), 1))
}

variants = list(res_tilt, res_swap_default, res_swap_warmstart,
                res_swap_anneal, res_swap_l2, res_swap_guided,
                res_swap_long, res_swap_multistart, res_swap_combo)

errs = lapply(variants, err_for)
names(errs) = sapply(variants, `[[`, 'variant')


#--- Match-quality summary table -------------------------------------------

cat('\n========= Match-quality summary (count error) =========\n\n')

quality = function(err) {
  vapply(c(0.01, 0.05, 0.10, 0.25, 0.50),
         function(t) 100 * mean(err$count_err <= t, na.rm = TRUE),
         numeric(1))
}

q_tbl = data.frame(
  variant = names(errs),
  pct_within_1pct  = round(sapply(errs, function(e) quality(e)[1]), 1),
  pct_within_5pct  = round(sapply(errs, function(e) quality(e)[2]), 1),
  pct_within_10pct = round(sapply(errs, function(e) quality(e)[3]), 1),
  pct_within_25pct = round(sapply(errs, function(e) quality(e)[4]), 1),
  pct_within_50pct = round(sapply(errs, function(e) quality(e)[5]), 1),
  elapsed_secs     = round(sapply(variants, `[[`, 'elapsed_secs'), 1),
  stringsAsFactors = FALSE
)
print(q_tbl, row.names = FALSE)


#--- Top NW shares per variant --------------------------------------------

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

ts_rows = lapply(variants, function(res) {
  v = topshare_tbl(attach_meta(res$y))
  data.frame(
    variant = res$variant,
    delta_top_001_pp = round(100 * (v['top_001'] - scf_vals['top_001']), 2),
    delta_top_01_pp  = round(100 * (v['top_01']  - scf_vals['top_01']),  2),
    delta_top_1_pp   = round(100 * (v['top_1']   - scf_vals['top_1']),   2),
    delta_top_10_pp  = round(100 * (v['top_10']  - scf_vals['top_10']),  2),
    delta_gini_pp    = round(100 * (v['gini']    - scf_vals['gini']),    2),
    stringsAsFactors = FALSE
  )
})
ts_tbl = do.call(rbind, ts_rows)
rownames(ts_tbl) = NULL
cat(sprintf('SCF actuals: top0.1=%.3f  top1=%.3f  top10=%.3f  gini=%.3f\n',
            scf_vals['top_001'], scf_vals['top_01'],
            scf_vals['top_10'], scf_vals['gini']))
print(ts_tbl, row.names = FALSE)


#--- Convergence status per variant ----------------------------------------

cat('\n========= Convergence status (swap variants) =========\n\n')

status_table = function(res) {
  if (res$variant == 'tilt') return(NULL)
  d = res$step_a_diagnostics
  st = sapply(d, function(x) x$status %||% 'tilt')
  tibble(
    variant       = res$variant,
    converged     = sum(st == 'converged'),
    stuck         = sum(st == 'stuck'),
    cap           = sum(st == 'cap'),
    n_buckets     = length(d),
    median_max_rel = round(median(sapply(d, function(x)
                              x$final_max_rel %||% NA_real_), na.rm = TRUE), 4)
  )
}
`%||%` = function(a, b) if (!is.null(a)) a else b
st_tbl = bind_rows(lapply(variants[-1], status_table))
print(as.data.frame(st_tbl), row.names = FALSE)


#--- Per-bucket detail (worst 10 buckets across all variants by SCF count) -

cat('\n========= Worst buckets: count error per variant =========\n\n')

bucket_id = function(x) paste(x$cell_income, x$cell_age, x$category, sep = '/')

err_wide = errs[[1]] %>%
  select(cell_income, cell_age, category, count_scf) %>%
  mutate(bid = paste(cell_income, cell_age, category, sep = '/'))
for (v in names(errs)) {
  err_wide[[paste0('err_', v)]] = round(errs[[v]]$count_err, 3)
}

# Buckets where SOME variant differs by >25% from SCF, sorted by tilt error.
worst_bids = err_wide %>%
  rowwise() %>%
  mutate(max_err = max(c_across(starts_with('err_')), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(max_err > 0.25) %>%
  arrange(desc(err_tilt)) %>%
  head(15)
print(as.data.frame(worst_bids %>% select(-bid)), row.names = FALSE)


#--- Save structured results -----------------------------------------------

results = list(
  output_dir       = output_dir,
  quality_table    = q_tbl,
  topshare_table   = ts_tbl,
  scf_top_shares   = scf_vals,
  status_table     = st_tbl,
  worst_buckets    = worst_bids,
  per_variant_diag = setNames(lapply(variants, `[[`, 'step_a_diagnostics'),
                              sapply(variants, `[[`, 'variant')),
  per_variant_elapsed = setNames(sapply(variants, `[[`, 'elapsed_secs'),
                                  sapply(variants, `[[`, 'variant')),
  per_variant_qc      = setNames(lapply(variants, `[[`, 'qc_report'),
                                  sapply(variants, `[[`, 'variant'))[1L],
  errs              = errs
)
out_path = file.path(output_dir, 'swap_variants_results.rds')
write_rds(results, out_path)
cat(sprintf('\nWrote %s\n', out_path))

cat('\nDone.\n')
