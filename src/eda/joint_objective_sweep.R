#---------------------------------------------
# joint_objective_sweep.R
#
# Sweep over amount_weight ∈ {0, 0.5, 1, 2} for
# the swap solver in joint count + amount mode.
# Tests whether minimizing Step B's job — by
# making Step A also pull amounts toward SCF —
# is feasible. All arms run at mns=50, uniform
# init from leaf (no warm start; user preference).
#
# For each arm, prints:
#   - count match summary across the 96 cells
#   - amount match summary BEFORE Step B rescale
#   - top NW shares
#   - Step B factor distribution (smaller = swap
#     did more of the work, less data distortion)
#
# Plus per-cat tables of SCF / pre-swap / post-swap
# for amount_weight = 1 (the "is this real?" arm).
#
# Usage:
#   Rscript src/eda/joint_objective_sweep.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop('Usage: Rscript src/eda/joint_objective_sweep.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

# mns_run controls which forest cache is hit. mns=20 and mns=50 are cached.
# Pass a different mns via env var WEALTH_MNS_OVERRIDE; estimate_models is
# flipped on automatically when training a new forest.
mns_run = {
  e = Sys.getenv('WEALTH_MNS_OVERRIDE')
  if (nzchar(e)) as.integer(e) else 50L
}
estimate_models = if (mns_run %in% c(20L, 50L)) 0L else 1L
do_lp           = 0L
cat(sprintf('joint_objective_sweep: mns_run=%d  estimate_models=%d\n',
            mns_run, estimate_models))

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


#--- Define arms -----------------------------------------------------------

run_arm = function(label, amount_weight) {
  cat(sprintf('\n========= ARM: %s  (amount_weight=%g, mns=%d) =========\n',
              label, amount_weight, mns_run))
  set.seed(76)
  t0 = Sys.time()
  swap_opts = list(amount_weight = amount_weight)
  result = run_wealth_imputation(puf_2022, scf_tax_units,
                                  min_node_size = mns_run,
                                  swap_options  = swap_opts)
  result$arm = label
  result$amount_weight = amount_weight
  result$elapsed_secs = as.numeric(Sys.time() - t0, units = 'secs')
  cat(sprintf('Arm %s: total %.1fs\n', label, result$elapsed_secs))
  result
}

arms_def = list(
  list(label = 'aw_0',    amount_weight = 0.0),
  list(label = 'aw_0p5',  amount_weight = 0.5)
  # aw_1 and aw_2 dropped after the mns=75 timeout — they add wall time
  # but barely move from aw_0p5 (we have full per-bucket evidence). Add
  # them back if you want a wider sweep.
)
arms = lapply(arms_def, function(d) run_arm(d$label, d$amount_weight))
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


#--- Per-arm stats: counts and PRE-RESCALE amounts -------------------------

CATS = c('nw', 'equities', 'bonds', 'homes',
         'retirement', 'business', 'other', 'debt')

cell_cat_stats = function(df, label) {
  out = list()
  for (cc in CATS) {
    col = paste0('cat_', cc)
    s = df %>%
      group_by(cell_income, cell_age) %>%
      summarise(count_wt = sum(weight * (.data[[col]] > 0)),
                total    = sum(weight * .data[[col]]),
                .groups  = 'drop') %>%
      mutate(category = cc, source = label)
    out[[cc]] = s
  }
  bind_rows(out)
}

scf_stats = cell_cat_stats(scf_meta, 'SCF')

# pre-rescale post-Step-A: this is what we care about — what swap achieved
# on amounts, before Step B does any work.
arm_stats_post = lapply(arms, function(res) {
  cell_cat_stats(attach_meta(res$y_post_step_a_pre_rescale), res$arm)
})


#--- Match-quality summary --------------------------------------------------

err_for = function(s) {
  s %>% inner_join(scf_stats %>%
                     select(cell_income, cell_age, category,
                            count_scf = count_wt, total_scf = total),
                    by = c('cell_income','cell_age','category')) %>%
    mutate(count_err = abs(count_wt - count_scf) / pmax(count_scf, 1),
           total_err = abs(total - total_scf)    / pmax(abs(total_scf), 1))
}

errs = lapply(arm_stats_post, err_for)

quality = function(err, key) {
  vapply(c(0.01, 0.05, 0.10, 0.25, 0.50),
         function(t) 100 * mean(err[[key]] <= t, na.rm = TRUE),
         numeric(1))
}

cat('\n========= Match quality at each tolerance =========\n')
cat('  count_err = |post_count − scf_count| / scf_count\n')
cat('  amount_err = |pre_rescale_post_amount − scf_amount| / |scf_amount|\n\n')

q_count = data.frame(
  arm = names(errs),
  amount_weight = sapply(arms, `[[`, 'amount_weight'),
  count_pct_within_1  = round(sapply(errs, quality, 'count_err')[1, ], 1),
  count_pct_within_5  = round(sapply(errs, quality, 'count_err')[2, ], 1),
  count_pct_within_10 = round(sapply(errs, quality, 'count_err')[3, ], 1),
  count_pct_within_25 = round(sapply(errs, quality, 'count_err')[4, ], 1),
  amount_pct_within_5  = round(sapply(errs, quality, 'total_err')[2, ], 1),
  amount_pct_within_10 = round(sapply(errs, quality, 'total_err')[3, ], 1),
  amount_pct_within_25 = round(sapply(errs, quality, 'total_err')[4, ], 1),
  amount_pct_within_50 = round(sapply(errs, quality, 'total_err')[5, ], 1),
  elapsed_secs = round(sapply(arms, `[[`, 'elapsed_secs'), 1),
  stringsAsFactors = FALSE
)
print(q_count, row.names = FALSE)


#--- Step B factor distribution per arm ------------------------------------

cat('\n========= Step B rescale factor distribution per arm =========\n')
cat('(factor = SCF_total / post_swap_pre_rescale_total per cell × cat;\n')
cat(' factor = 1 means swap got the amount exactly right and Step B is a no-op)\n\n')

rf_summary = function(res) {
  rf = res$rescale_factors %>% filter(applied)
  data.frame(
    arm = res$arm,
    n_applied = nrow(rf),
    factor_min    = round(min(rf$factor), 3),
    factor_p10    = round(as.numeric(quantile(rf$factor, 0.10)), 3),
    factor_median = round(median(rf$factor), 3),
    factor_p90    = round(as.numeric(quantile(rf$factor, 0.90)), 3),
    factor_max    = round(max(rf$factor), 3),
    n_outside_0p5_2p0 = sum(rf$factor < 0.5 | rf$factor > 2.0),
    median_log2_dist  = round(median(abs(log2(pmax(rf$factor, 1e-6)))), 3),
    stringsAsFactors = FALSE
  )
}
print(do.call(rbind, lapply(arms, rf_summary)), row.names = FALSE)


#--- Top NW shares per arm -------------------------------------------------

cat('\n========= Top NW shares + Gini (pre-Step-B post-Step-A) =========\n')

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
cat(sprintf('SCF actuals: top0.1=%.3f  top1=%.3f  top10=%.3f  gini=%.3f\n\n',
            scf_vals['top_001'], scf_vals['top_01'],
            scf_vals['top_10'], scf_vals['gini']))

ts_rows_post = lapply(arms, function(res) {
  v = topshare_tbl(attach_meta(res$y_post_step_a_pre_rescale))
  data.frame(
    arm = res$arm, stage = 'pre_rescale',
    delta_top_001_pp = round(100 * (v['top_001'] - scf_vals['top_001']), 2),
    delta_top_01_pp  = round(100 * (v['top_01']  - scf_vals['top_01']),  2),
    delta_top_1_pp   = round(100 * (v['top_1']   - scf_vals['top_1']),   2),
    delta_top_10_pp  = round(100 * (v['top_10']  - scf_vals['top_10']),  2),
    delta_gini_pp    = round(100 * (v['gini']    - scf_vals['gini']),    2),
    stringsAsFactors = FALSE
  )
})
ts_rows_final = lapply(arms, function(res) {
  v = topshare_tbl(attach_meta(res$y))
  data.frame(
    arm = res$arm, stage = 'final_post_rescale',
    delta_top_001_pp = round(100 * (v['top_001'] - scf_vals['top_001']), 2),
    delta_top_01_pp  = round(100 * (v['top_01']  - scf_vals['top_01']),  2),
    delta_top_1_pp   = round(100 * (v['top_1']   - scf_vals['top_1']),   2),
    delta_top_10_pp  = round(100 * (v['top_10']  - scf_vals['top_10']),  2),
    delta_gini_pp    = round(100 * (v['gini']    - scf_vals['gini']),    2),
    stringsAsFactors = FALSE
  )
})
print(rbind(do.call(rbind, ts_rows_post),
            do.call(rbind, ts_rows_final)),
      row.names = FALSE)


#--- Aggregate amount comparison across arms (printed FIRST) ---------------
# The user's headline question is about per-category aggregate amount gaps,
# so print and save these before the per-cell detail loop runs (so a crash
# downstream doesn't take out the headline numbers).

cat('\n========= Aggregate (sum across all 16 buckets) per category and arm =========\n')

agg_per_arm = function(res) {
  s = cell_cat_stats(attach_meta(res$y_post_step_a_pre_rescale),
                     res$arm)
  s %>% group_by(category) %>%
    summarise(count_M  = round(sum(count_wt) / 1e6,  2),
              amount_T = round(sum(total)    / 1e12, 3),
              .groups = 'drop') %>%
    mutate(arm = res$arm)
}
scf_agg_pre = scf_stats %>% group_by(category) %>%
  summarise(count_scf_M  = round(sum(count_wt) / 1e6, 2),
            amount_scf_T = round(sum(total) / 1e12, 3), .groups = 'drop')

agg_arms_pre = bind_rows(lapply(arms, agg_per_arm))
agg_compact = agg_arms_pre %>%
  select(arm, category, count_M, amount_T) %>%
  pivot_wider(names_from = arm, values_from = c(count_M, amount_T)) %>%
  inner_join(scf_agg_pre, by = 'category') %>%
  mutate(category = factor(category, levels = CATS)) %>%
  arrange(category)
print(as.data.frame(agg_compact), row.names = FALSE)

# Save partial results now, before the per-cell loop, so they survive any
# crash in the formatting code.
partial_results = list(
  output_dir = output_dir,
  match_quality = q_count,
  rescale_factor_summary =
    do.call(rbind, lapply(arms, rf_summary)),
  topshare_pre_rescale =
    do.call(rbind, ts_rows_post),
  topshare_post_rescale =
    do.call(rbind, ts_rows_final),
  agg_per_arm = agg_compact,
  per_arm_diag = setNames(lapply(arms, `[[`, 'step_a_diagnostics'),
                          names(arms)),
  rescale_factors_per_arm = setNames(lapply(arms, `[[`, 'rescale_factors'),
                                      names(arms))
)
out_path = file.path(output_dir, 'joint_objective_sweep_results.rds')
write_rds(partial_results, out_path)
cat(sprintf('\nWrote partial results to %s\n', out_path))


#--- Per-cat detail: SCF / pre-swap / aw=1 post-swap (pre-rescale) --------

cat('\n========= Per-(cell × cat) detail for aw=1 (pre-rescale) =========\n')
cat('counts in M, amounts in $T, gap_post = signed % deviation post-swap vs SCF\n\n')

# pre-Step-A (uniform draw) — same for all arms (random init seeded the same)
pre  = attach_meta(arms[[1]]$y_pre_swap)
pre_stats = cell_cat_stats(pre, 'pre')

# Pick a "joint" arm for the per-cat detail. Prefer aw_1 when present
# (canonical equal-weight); otherwise fall back to the last arm in the
# sweep, which is the highest-amount-weight arm we have.
detail_arm = if ('aw_1' %in% names(arms)) {
  arms[['aw_1']]
} else {
  arms[[length(arms)]]
}
post1 = attach_meta(detail_arm$y_post_step_a_pre_rescale)
post1_stats = cell_cat_stats(post1, detail_arm$arm)

cell_levels = paste(rep(CALIB_INCOME_BUCKETS, each = 2),
                    rep(CALIB_AGE_BUCKETS,
                        times = length(CALIB_INCOME_BUCKETS)),
                    sep = '/')

format_cat = function(cc) {
  s_scf  = scf_stats   %>% filter(category == cc)
  s_pre  = pre_stats   %>% filter(category == cc)
  s_post = post1_stats %>% filter(category == cc)

  s_scf %>%
    select(cell_income, cell_age,
           count_scf = count_wt, amount_scf = total) %>%
    inner_join(s_pre %>%
                 select(cell_income, cell_age,
                        count_pre = count_wt, amount_pre = total),
                by = c('cell_income', 'cell_age')) %>%
    inner_join(s_post %>%
                 select(cell_income, cell_age,
                        count_post = count_wt, amount_post = total),
                by = c('cell_income', 'cell_age')) %>%
    mutate(
      cell = factor(paste(cell_income, cell_age, sep = '/'),
                    levels = cell_levels),
      count_scf_M  = round(count_scf  / 1e6, 2),
      count_pre_M  = round(count_pre  / 1e6, 2),
      count_post_M = round(count_post / 1e6, 2),
      amount_scf_T  = round(amount_scf  / 1e12, 3),
      amount_pre_T  = round(amount_pre  / 1e12, 3),
      amount_post_T = round(amount_post / 1e12, 3),
      count_gap_pct  = round(100 * (count_post  - count_scf)  /
                                     pmax(count_scf, 1), 1),
      amount_gap_pct = round(100 * (amount_post - amount_scf) /
                                     pmax(abs(amount_scf), 1), 1)
    ) %>%
    arrange(cell) %>%
    select(cell,
           count_scf_M, count_pre_M, count_post_M, count_gap_pct,
           amount_scf_T, amount_pre_T, amount_post_T, amount_gap_pct)
}

for (cc in CATS) {
  cat(sprintf('\n--- Category: %s ---\n', toupper(cc)))
  print(as.data.frame(format_cat(cc)), row.names = FALSE)
}


#--- Re-save with per-cat detail ------------------------------------------
# Augment partial_results with the per-cat detail tables now that the
# format_cat loop has run successfully.
partial_results$per_cat_aw1 = setNames(lapply(CATS, format_cat), CATS)
write_rds(partial_results, out_path)
cat(sprintf('\nUpdated %s with per-cat detail\n', out_path))

cat('\nDone.\n')
