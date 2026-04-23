#---------------------------------------------
# wealth_harness.R
#
# Fast iteration harness for wealth imputation.
# Loads puf_2022 + scf_tax_units from cache,
# runs run_wealth_imputation, prints compact
# SCF-vs-PUF metrics. ~1-2 min per iteration
# (no Phase 1 or Phase 2 rerun).
#
# Requires a completed pipeline run's output
# directory for the inputs (pipeline writes
# puf_2022_snapshot.rds; if not present, this
# script reconstructs from tax_units_2022.csv
# by nulling the wealth columns).
#
# Usage:
#   Rscript src/eda/wealth_harness.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop('Usage: Rscript src/eda/wealth_harness.R <output_dir> [retrain]')
}
output_dir   = args[1]
do_retrain   = length(args) >= 2 && tolower(args[2]) == 'retrain'

# estimate_models = 1 triggers DRF retrain (writes cache). Use when the
# forest features or training params change (the old cache is invalid).
# Otherwise 0 loads cache; iteration cost stays ~30s.
estimate_models = as.integer(do_retrain)
do_lp           = 0

if (do_retrain) cat('wealth_harness: RETRAIN mode (forest will be re-fit)\n')

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
stopifnot(dir.exists(output_dir))


#--- Load inputs ------------------------------------------------------------

cat('Loading scf_tax_units from cache...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

snap_path = file.path(output_dir, 'puf_2022_snapshot.rds')
if (file.exists(snap_path)) {
  cat(sprintf('Loading puf_2022 snapshot: %s\n', snap_path))
  puf_2022 = read_rds(snap_path)
} else {
  cat('No puf_2022_snapshot.rds; reconstructing from tax_units_2022.csv ',
      'by nulling wealth columns (pre-wealth equivalent).\n', sep = '')
  puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                      show_col_types = FALSE)
  for (v in wealth_y_vars) puf_2022[[v]] = NA_real_
}


#--- Run wealth imputation --------------------------------------------------

# NOTE: source wealth.R AFTER estimate_models is set above.
source('src/imputations/wealth.R')

cat('\n=== Running run_wealth_imputation ===\n')
t0 = Sys.time()
set.seed(76)   # reproducible across iterations; flip to explore stochasticity
result = run_wealth_imputation(puf_2022, scf_tax_units)
cat(sprintf('wealth_harness: total time %.1fs\n',
            as.numeric(Sys.time() - t0, units = 'secs')))


#--- Compact diagnostic ----------------------------------------------------

y_post = result$y
y_pre  = result$y_pre_tilt
rf     = result$rescale_factors

# Join to get weights + cell assignments on post-tilt + pre-tilt.
puf_base = puf_2022 %>%
  select(id, weight, age1, age2,
         wages, sole_prop, farm,
         scorp_active, scorp_active_loss, scorp_179,
         scorp_passive, scorp_passive_loss,
         part_active, part_active_loss, part_179,
         part_passive, part_passive_loss,
         txbl_int, exempt_int, div_ord, div_pref,
         kg_lt, kg_st,
         gross_ss, gross_pens_dist, ui,
         rent, rent_loss, estate, estate_loss)
puf_age2  = ifelse(is.na(puf_base$age2), 0L, puf_base$age2)
puf_base$age_older = pmax(pmin(80L, puf_base$age1), pmin(80L, puf_age2))
puf_base$income    = with(puf_base,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st +
  gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)
puf_base = assign_calibration_cells(puf_base, puf_base$income,
                                    puf_base$age_older, puf_base$weight)

attach_cells = function(df_y) {
  df_y %>% inner_join(puf_base %>% select(id, weight, age_older, income,
                                          cell_income, cell_age),
                      by = 'id') %>%
    bind_cols(compute_category_values(.))
}
post = attach_cells(y_post)
pre  = attach_cells(y_pre)

# SCF with category values.
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_ao   = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_y    = scf_to_y(scf_tax_units)
scf = data.frame(weight = scf_y$weight, age_older = scf_ao, income = scf_inc)
scf = cbind(scf, compute_category_values(scf_y))
scf = assign_calibration_cells(scf, scf$income, scf$age_older, scf$weight)


#--- Aggregate table -------------------------------------------------------

cat('\n=== Aggregate totals per category (SCF / pre / post) ===\n')
totals = function(df, lab) tibble(
  source = lab,
  category = CALIB_CATEGORIES,
  total = vapply(unname(CAT_COL),
                 function(c) sum(df$weight * df[[c]]), numeric(1))
)
agg = bind_rows(totals(scf,'SCF'), totals(pre,'pre'), totals(post,'post')) %>%
  pivot_wider(names_from = source, values_from = total) %>%
  mutate(pre_gap  = 100 * (pre  - SCF) / SCF,
         post_gap = 100 * (post - SCF) / SCF,
         across(c(SCF, pre, post),
                ~ paste0('$', round(.x / 1e12, 2), 'T')))
print(agg, n = Inf)


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

cat('\n=== Top NW shares + Gini ===\n')
tb = function(df, lab) tibble(
  source = lab,
  top_01 = top_share(df$cat_nw, df$weight, 0.001),
  top_1  = top_share(df$cat_nw, df$weight, 0.01),
  top_5  = top_share(df$cat_nw, df$weight, 0.05),
  top_10 = top_share(df$cat_nw, df$weight, 0.10),
  gini   = weighted_gini(df$cat_nw, df$weight)
)
print(bind_rows(tb(scf,'SCF'), tb(pre,'pre'), tb(post,'post')) %>%
      mutate(across(-source, ~ round(.x, 4))))


#--- Share of aggregate NW by INCOME percentile bin ------------------------

income_bins   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
income_labels = c('p0-20','p20-40','p40-60','p60-80',
                  'p80-90','p90-99','p99-99.9','top0.1%')

inc_shares = function(df, lab) {
  pctile = compute_percentile(df$income, df$weight)
  bin    = cut(pctile, breaks = income_bins, labels = income_labels,
               include.lowest = TRUE, right = FALSE)
  df %>% mutate(bin = bin) %>%
    group_by(bin) %>%
    summarise(nw_total = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(share = nw_total / sum(nw_total), source = lab)
}

cat('\n=== DEBUG: direct p0-20 consistency check ===\n')
debug_p020 = function(df, lab) {
  pp = compute_percentile(df$income, df$weight)
  s  = pp < 20
  cat(sprintf('%s p0-20: n_unwt=%d n_wt=%.1fM mean_NW=$%.0f total_NW=$%.2fT share_of_total_NW=%.4f\n',
              lab, sum(s), sum(df$weight[s])/1e6,
              sum(df$weight[s] * df$cat_nw[s]) / sum(df$weight[s]),
              sum(df$weight[s] * df$cat_nw[s]) / 1e12,
              sum(df$weight[s] * df$cat_nw[s]) / sum(df$weight * df$cat_nw)))
}
debug_p020(scf, 'SCF')
debug_p020(pre, 'pre')
debug_p020(post, 'post')

cat('\n=== Share of aggregate NW by INCOME percentile ===\n')
inc_share_tbl = bind_rows(
  inc_shares(scf,  'SCF'),
  inc_shares(pre,  'pre'),
  inc_shares(post, 'post')
) %>%
  select(bin, source, share) %>%
  pivot_wider(names_from = source, values_from = share) %>%
  mutate(across(c(SCF, pre, post), ~ round(.x, 3))) %>%
  mutate(pre_minus_scf  = round(pre  - SCF, 3),
         post_minus_scf = round(post - SCF, 3))
print(inc_share_tbl, n = Inf)


#--- Share of aggregate NW by WEALTH percentile bin ------------------------

wealth_bins   = c(0, 50, 90, 99, 99.9, 100)
wealth_labels = c('bot50','next40','next9','top1%','top0.1%')

w_shares = function(df, lab) {
  pctile = compute_percentile(df$cat_nw, df$weight)
  # compute_percentile assigns 0 to non-positive; attach to bot50 bin.
  bin = cut(pctile, breaks = wealth_bins, labels = wealth_labels,
            include.lowest = TRUE, right = FALSE)
  # Non-positive NW gets its own share bucket implicitly via cut on 0.
  df %>% mutate(bin = bin) %>%
    group_by(bin) %>%
    summarise(nw_total = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(share = nw_total / sum(nw_total), source = lab)
}

cat('\n=== Share of aggregate NW by WEALTH percentile ===\n')
w_share_tbl = bind_rows(
  w_shares(scf,  'SCF'),
  w_shares(pre,  'pre'),
  w_shares(post, 'post')
) %>%
  select(bin, source, share) %>%
  pivot_wider(names_from = source, values_from = share) %>%
  mutate(across(c(SCF, pre, post), ~ round(.x, 3))) %>%
  mutate(pre_minus_scf  = round(pre  - SCF, 3),
         post_minus_scf = round(post - SCF, 3))
print(w_share_tbl, n = Inf)


#--- Rescale factor report -------------------------------------------------

cat('\n=== Step B: rescale factor distribution (applied only) ===\n')
f_app = rf %>% filter(applied)
cat(sprintf('n_applied = %d / %d\n', sum(rf$applied), nrow(rf)))
cat(sprintf('factor: min=%.3f  p10=%.3f  median=%.3f  mean=%.3f  p90=%.3f  max=%.3f\n',
            min(f_app$factor), quantile(f_app$factor, 0.10),
            median(f_app$factor), mean(f_app$factor),
            quantile(f_app$factor, 0.90), max(f_app$factor)))

cat('\n=== Rescale factors by category (all cells) ===\n')
cat_summary = f_app %>%
  group_by(category) %>%
  summarise(n = n(),
            min_factor    = min(factor),
            median_factor = median(factor),
            max_factor    = max(factor),
            .groups = 'drop')
print(cat_summary)

cat('\n=== Extreme rescale factors (|log2| > 1 → outside [0.5, 2.0]) ===\n')
extreme = f_app %>% filter(factor < 0.5 | factor > 2.0) %>%
  arrange(desc(abs(log2(pmax(factor, 1e-6)))))
if (nrow(extreme) > 0) {
  print(extreme %>% select(cell_income, cell_age, category,
                           scf_total, puf_pre_rescale_total, factor),
        n = Inf)
} else {
  cat('  (none — all rescales within 2×)\n')
}


#--- Skipped cells ---------------------------------------------------------

cat('\n=== Cells where rescale was skipped (near-zero aggregate) ===\n')
skipped = rf %>% filter(!applied)
if (nrow(skipped) > 0) {
  print(skipped %>% select(cell_income, cell_age, category,
                           scf_total, puf_pre_rescale_total),
        n = Inf)
} else {
  cat('  (none)\n')
}

cat('\nDone.\n')
