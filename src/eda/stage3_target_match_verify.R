#---------------------------------------------
# stage3_target_match_verify.R
#
# Verify Stage 3 actually matches what it
# claims to match: weighted positive count
# (extensive) and weighted aggregate (intensive)
# per (cell × age × category) vs SCF.
#
# Output: 6 tables (one per category), rows
# are (cell × age), columns are:
#   - SCF count_M, SCF total_T
#   - pre count_M, pre total_T
#   - post count_M, post total_T
#   - count_err post (relative)
#   - total_err post (relative)
#
# Plus a final summary of how many targets are
# matched within 5% / 10% / 25%.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

estimate_models = 0L
do_lp           = 0L

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')


#--- Load + run wealth ---------------------------------------------------

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
cat('=== Running run_wealth_imputation ===\n')
t0 = Sys.time()
set.seed(76)
result = run_wealth_imputation(puf_2022, scf_tax_units)
cat(sprintf('Total time %.1fs\n\n', as.numeric(Sys.time() - t0, units = 'secs')))

y_post = result$y
y_pre  = result$y_pre_tilt


#--- Build per-row PUF + SCF frames with cells + cat_* columns ----------

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
post = attach_meta(y_post)
pre  = attach_meta(y_pre)

# SCF
scf_y    = scf_to_y(scf_tax_units)
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_ao   = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_meta = data.frame(weight = scf_y$weight,
                      age_older = scf_ao, income = scf_inc)
scf_meta = cbind(scf_meta, compute_category_values(scf_y))
scf_meta = assign_calibration_cells(scf_meta, scf_meta$income,
                                    scf_meta$age_older, scf_meta$weight)


#--- Compute count + aggregate per (cell × age × category) --------------

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

scf_stats  = cell_cat_stats(scf_meta, 'SCF')
pre_stats  = cell_cat_stats(pre,      'pre')
post_stats = cell_cat_stats(post,     'post')

all_stats = bind_rows(scf_stats, pre_stats, post_stats)


#--- Wide-format table per category ------------------------------------

cell_levels = paste(rep(CALIB_INCOME_BUCKETS, each = 2),
                    rep(CALIB_AGE_BUCKETS, times = length(CALIB_INCOME_BUCKETS)),
                    sep = '/')

format_one_category = function(cc) {
  s = all_stats %>% filter(category == cc) %>%
    mutate(cell = paste(cell_income, cell_age, sep = '/'))
  wide = s %>% select(cell, source, count_wt, total) %>%
    pivot_wider(names_from = source,
                values_from = c(count_wt, total))

  # Rounded display columns + relative errors for post
  wide %>%
    mutate(
      cell = factor(cell, levels = cell_levels),
      SCF_count_M  = round(count_wt_SCF  / 1e6, 2),
      pre_count_M  = round(count_wt_pre  / 1e6, 2),
      post_count_M = round(count_wt_post / 1e6, 2),
      SCF_total_T  = round(total_SCF  / 1e12, 3),
      pre_total_T  = round(total_pre  / 1e12, 3),
      post_total_T = round(total_post / 1e12, 3),
      count_err_pre  = round((count_wt_pre  - count_wt_SCF) / pmax(count_wt_SCF, 1), 3),
      count_err_post = round((count_wt_post - count_wt_SCF) / pmax(count_wt_SCF, 1), 3),
      total_err_pre  = round((total_pre  - total_SCF) / pmax(abs(total_SCF), 1), 3),
      total_err_post = round((total_post - total_SCF) / pmax(abs(total_SCF), 1), 3)
    ) %>%
    arrange(cell) %>%
    select(cell,
           SCF_count_M, pre_count_M, post_count_M, count_err_post,
           SCF_total_T, pre_total_T, post_total_T, total_err_post)
}

for (cc in CATS) {
  cat(sprintf('\n=== Category: %s ===\n', toupper(cc)))
  print(as.data.frame(format_one_category(cc)), row.names = FALSE)
}


#--- Summary: how many of 96 (cell × age × category) targets are matched

cat('\n\n=== Match-quality summary ===\n')
err_summary = all_stats %>%
  filter(source == 'post') %>%
  inner_join(scf_stats %>% select(cell_income, cell_age, category,
                                   count_scf = count_wt, total_scf = total),
              by = c('cell_income','cell_age','category')) %>%
  mutate(count_err = abs(count_wt - count_scf) / pmax(count_scf, 1),
         total_err = abs(total - total_scf)    / pmax(abs(total_scf), 1))

cat('\nFraction of 96 (cell × age × cat) buckets where post-tilt is within...\n')
for (tol in c(0.01, 0.05, 0.10, 0.25, 0.50)) {
  cat(sprintf('  ±%4.0f%% : count match=%4.1f%%  total match=%4.1f%%\n',
              tol * 100,
              100 * mean(err_summary$count_err <= tol, na.rm = TRUE),
              100 * mean(err_summary$total_err <= tol, na.rm = TRUE)))
}

cat('\nWorst-matched (cell × age × cat) buckets by post-tilt count error:\n')
worst_count = err_summary %>% arrange(desc(count_err)) %>%
  slice_head(n = 10) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(cell_income, cell_age, category,
         count_scf, count_wt, count_err)
print(as.data.frame(worst_count), row.names = FALSE)

cat('\nWorst-matched (cell × age × cat) buckets by post-tilt total error:\n')
worst_total = err_summary %>% arrange(desc(total_err)) %>%
  slice_head(n = 10) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(cell_income, cell_age, category,
         total_scf, total, total_err)
print(as.data.frame(worst_total), row.names = FALSE)

cat('\nDone.\n')
