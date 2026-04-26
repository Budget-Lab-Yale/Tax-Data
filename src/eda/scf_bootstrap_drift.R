#---------------------------------------------------------------
# scf_bootstrap_drift.R
#
# Quick check: how much does the per-cell weighted bootstrap
# (used for forest training in wealth.R) drift in aggregate
# from raw SCF? Per cell, compares:
#
#   true_cell_agg   = sum(w_i * NW_i)
#   boot_cell_agg   = (cell_W / N_boot) * sum(NW_b)   # equal-weight
#                                                      # bootstrap rows
#
# Reproduces wealth.R's bootstrap exactly: same seed
# (100 + cell_idx), same n_boot per cell.
#---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')

scf = read_rds('resources/cache/scf_tax_units.rds')

# NW from asset/debt/kg vars (matches harness)
scf$nw = rowSums(scf[, wealth_asset_vars]) - rowSums(scf[, wealth_debt_vars])

# Income (same construction as wealth.R)
scf$income = with(scf, wages_scf + business_scf + int_div_scf +
                  capital_gains_scf + rent_scf + ss_pens_scf + ui_other_scf)

scf$age1_capped = pmin(as.integer(scf$age1), 80L)
scf$age2_capped = ifelse(!is.na(scf$age2), pmin(as.integer(scf$age2), 80L), NA)
scf$age_older   = ifelse(!is.na(scf$age2_capped),
                         pmax(scf$age1_capped, scf$age2_capped),
                         scf$age1_capped)

scf_cells = assign_calibration_cells(
  data.frame(weight = scf$weight),
  scf$income, scf$age_older, scf$weight)$cell_income

n_boot_cell = c(
  'pct00to20'=100000L, 'pct20to40'=100000L, 'pct40to60'=100000L,
  'pct60to80'=100000L, 'pct80to90'=100000L, 'pct90to99'=100000L,
  'pct99to99.9'=100000L, 'pct99.9to100'=150000L
)

cat('Per-cell bootstrap drift (8 cells, seed 100+cell_idx, exactly matching wealth.R)\n\n')

results = tibble(cell = CALIB_INCOME_BUCKETS,
                 scf_rows = NA_integer_,
                 cell_pop_M = NA_real_,
                 true_agg_T = NA_real_,
                 boot_agg_T = NA_real_,
                 drift_T = NA_real_,
                 drift_pct = NA_real_,
                 max_w_in_cell = NA_real_,
                 min_w_in_cell = NA_real_)

for (ci_idx in seq_along(CALIB_INCOME_BUCKETS)) {
  ci = CALIB_INCOME_BUCKETS[ci_idx]
  rows = which(scf_cells == ci)
  cell_w = scf$weight[rows]
  cell_nw = scf$nw[rows]
  W_c = sum(cell_w)
  true_agg = sum(cell_w * cell_nw)

  N_boot = n_boot_cell[[ci]]
  set.seed(100 + ci_idx)
  boot_in_cell = sample.int(length(rows), size = N_boot, replace = TRUE, prob = cell_w)
  boot_nw = cell_nw[boot_in_cell]
  boot_agg = (W_c / N_boot) * sum(boot_nw)

  results$scf_rows[ci_idx]      = length(rows)
  results$cell_pop_M[ci_idx]    = W_c / 1e6
  results$true_agg_T[ci_idx]    = true_agg / 1e12
  results$boot_agg_T[ci_idx]    = boot_agg / 1e12
  results$drift_T[ci_idx]       = (boot_agg - true_agg) / 1e12
  results$drift_pct[ci_idx]     = 100 * (boot_agg - true_agg) / true_agg
  results$max_w_in_cell[ci_idx] = max(cell_w)
  results$min_w_in_cell[ci_idx] = min(cell_w)
}

print(as.data.frame(results %>%
  mutate(cell_pop_M = round(cell_pop_M, 2),
         true_agg_T = round(true_agg_T, 2),
         boot_agg_T = round(boot_agg_T, 2),
         drift_T    = round(drift_T, 3),
         drift_pct  = round(drift_pct, 2),
         max_w_in_cell = round(max_w_in_cell, 0),
         min_w_in_cell = round(min_w_in_cell, 0))),
  row.names = FALSE)

cat(sprintf('\nTotal:  raw SCF agg = $%.2fT,  bootstrap agg = $%.2fT,  drift = $%+.2fT (%+.2f%%)\n',
            sum(results$true_agg_T),
            sum(results$boot_agg_T),
            sum(results$boot_agg_T) - sum(results$true_agg_T),
            100 * (sum(results$boot_agg_T) - sum(results$true_agg_T)) /
                  sum(results$true_agg_T)))

cat('\nDone.\n')
