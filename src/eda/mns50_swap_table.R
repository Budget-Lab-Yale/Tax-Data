#---------------------------------------------
# mns50_swap_table.R
#
# Show what swap@mns50 (uniform random init —
# no warm start) does on its own, BEFORE the
# Step B intensive rescale fixes amounts to
# SCF aggregates by construction.
#
# For each of the 6 categories (nw, equities,
# bonds, homes, other, debt) print one wide
# table whose rows are the 16 (income × age)
# buckets and columns are:
#
#   count_scf       SCF weighted count of records with cat > 0
#   count_pre       PUF count under uniform leaf draw
#   count_post      PUF count after swap solver
#   amount_scf      SCF weighted dollar total
#   amount_pre      PUF $ total under uniform draw
#   amount_post     PUF $ total after swap, BEFORE Step B rescale
#
# Counts are reported in millions of households,
# amounts in trillions of dollars. Relative gap
# columns are the post/SCF gaps (signed pp for
# counts, signed pct for amounts).
#
# Usage:
#   Rscript src/eda/mns50_swap_table.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript src/eda/mns50_swap_table.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

estimate_models = 0L     # mns50 forest is already cached from Run C
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


#--- Run swap @ mns50, uniform init ----------------------------------------

cat('\n========= Running swap @ mns50, uniform init =========\n')
set.seed(76)
t0 = Sys.time()
res = run_wealth_imputation(puf_2022, scf_tax_units,
                             stage3_method = 'swap',
                             min_node_size = 50L)
cat(sprintf('Total time %.1fs\n',
            as.numeric(Sys.time() - t0, units = 'secs')))


#--- Build cell-keyed dataframes -------------------------------------------

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

# Pre-Step-A: uniform random draw from each leaf.
pre  = attach_meta(res$y_pre_tilt)
# Post-Step-A, BEFORE Step B intensive rescale: this is what swap accomplished
# on its own. Counts are identical to post-Step-B (Step B is multiplicative).
post = attach_meta(res$y_post_step_a_pre_rescale)

# SCF actuals
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


#--- Compute per (cell × age × cat) stats ----------------------------------

CATS = c('nw','equities','bonds','homes','other','debt')

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

scf_stats  = cell_cat_stats(scf_meta, 'SCF')
pre_stats  = cell_cat_stats(pre,      'pre')
post_stats = cell_cat_stats(post,     'post')


#--- Side-by-side table per category ---------------------------------------

cell_levels = paste(rep(CALIB_INCOME_BUCKETS, each = 2),
                    rep(CALIB_AGE_BUCKETS,
                        times = length(CALIB_INCOME_BUCKETS)),
                    sep = '/')

format_cat = function(cc) {
  s_scf  = scf_stats  %>% filter(category == cc)
  s_pre  = pre_stats  %>% filter(category == cc)
  s_post = post_stats %>% filter(category == cc)

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
      # Counts in millions of households.
      count_scf_M  = round(count_scf  / 1e6, 2),
      count_pre_M  = round(count_pre  / 1e6, 2),
      count_post_M = round(count_post / 1e6, 2),
      # Amounts in trillions of dollars.
      amount_scf_T  = round(amount_scf  / 1e12, 3),
      amount_pre_T  = round(amount_pre  / 1e12, 3),
      amount_post_T = round(amount_post / 1e12, 3),
      # Signed relative gaps (post − scf) / |scf|, as %.
      count_gap_post_pct  = round(100 * (count_post  - count_scf)  /
                                          pmax(count_scf, 1),       1),
      amount_gap_post_pct = round(100 * (amount_post - amount_scf) /
                                          pmax(abs(amount_scf), 1), 1)
    ) %>%
    arrange(cell) %>%
    select(cell,
           count_scf_M, count_pre_M, count_post_M, count_gap_post_pct,
           amount_scf_T, amount_pre_T, amount_post_T, amount_gap_post_pct)
}

for (cc in CATS) {
  cat(sprintf('\n========= Category: %s =========\n', toupper(cc)))
  cat('  counts in millions of households, amounts in $T\n')
  cat('  count_gap_post_pct = signed % deviation post-swap vs SCF (counts)\n')
  cat('  amount_gap_post_pct = signed % deviation post-swap (pre-rescale) vs SCF (amounts)\n\n')
  print(as.data.frame(format_cat(cc)), row.names = FALSE)
}


#--- Aggregate row per category --------------------------------------------

cat('\n========= Aggregate (sum across all 16 buckets) =========\n')

agg = bind_rows(
  scf_stats  %>% group_by(category) %>%
    summarise(count_scf_M  = sum(count_wt) / 1e6,
              amount_scf_T = sum(total)    / 1e12, .groups = 'drop'),
  pre_stats  %>% group_by(category) %>%
    summarise(count_pre_M  = sum(count_wt) / 1e6,
              amount_pre_T = sum(total)    / 1e12, .groups = 'drop'),
  post_stats %>% group_by(category) %>%
    summarise(count_post_M  = sum(count_wt) / 1e6,
              amount_post_T = sum(total)    / 1e12, .groups = 'drop')
)

agg_wide = scf_stats %>% group_by(category) %>%
  summarise(count_scf_M  = round(sum(count_wt) / 1e6, 2),
            amount_scf_T = round(sum(total)    / 1e12, 3),
            .groups = 'drop') %>%
  inner_join(pre_stats %>% group_by(category) %>%
               summarise(count_pre_M  = round(sum(count_wt) / 1e6, 2),
                         amount_pre_T = round(sum(total)    / 1e12, 3),
                         .groups = 'drop'),
              by = 'category') %>%
  inner_join(post_stats %>% group_by(category) %>%
               summarise(count_post_M  = round(sum(count_wt) / 1e6, 2),
                         amount_post_T = round(sum(total)    / 1e12, 3),
                         .groups = 'drop'),
              by = 'category') %>%
  mutate(category = factor(category, levels = CATS),
         count_gap_post_pct  = round(100 * (count_post_M - count_scf_M)  /
                                            pmax(count_scf_M, 1e-3), 1),
         amount_gap_post_pct = round(100 * (amount_post_T - amount_scf_T) /
                                            pmax(abs(amount_scf_T), 1e-6), 1)) %>%
  arrange(category)

print(as.data.frame(agg_wide), row.names = FALSE)


#--- Save structured ------------------------------------------------------

per_cat = lapply(CATS, format_cat)
names(per_cat) = CATS

results = list(
  output_dir = output_dir,
  per_cat    = per_cat,
  agg_wide   = agg_wide,
  scf_stats  = scf_stats,
  pre_stats  = pre_stats,
  post_stats = post_stats,
  step_a_diagnostics = res$step_a_diagnostics
)
out_path = file.path(output_dir, 'mns50_swap_table.rds')
write_rds(results, out_path)
cat(sprintf('\nWrote %s\n', out_path))

cat('\nDone.\n')
