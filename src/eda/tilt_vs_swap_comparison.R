#---------------------------------------------
# tilt_vs_swap_comparison.R
#
# Side-by-side comparison of the new tilt-based
# Stage 3 wealth imputation against the prior
# swap-solver baseline. Reads the harness output
# diagnostic dump (wealth_harness_tilt_diag.rds)
# and the existing swap baseline tax_units_2022.csv
# from the same output_dir.
#
# Outputs:
#   - Aggregate match table per category
#   - Per-bucket tilt diagnostics (max_rel,
#     ||lambda||, ESS, KL)
#   - Step B factor distribution (tilt vs swap)
#   - Top wealth shares + Gini comparison
#   - Stale-target / out-of-range cell flags
#
# Usage (sbatch):
#   Rscript src/eda/tilt_vs_swap_comparison.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript tilt_vs_swap_comparison.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
source('src/imputations/wealth.R')   # for scf_to_y

cat('=== tilt vs swap comparison ===\n')
cat(sprintf('Output dir: %s\n', output_dir))

#--- Load diagnostic dumps -------------------------------------------------
diag_path = file.path(output_dir, 'wealth_harness_tilt_diag.rds')
if (!file.exists(diag_path)) {
  stop('Tilt diagnostic dump not found at: ', diag_path,
       '\nRun src/eda/wealth_harness.R first.')
}
tilt = readRDS(diag_path)
cat(sprintf('Loaded tilt diagnostics: %d records, %d cells_present\n',
            tilt$cell_info$n_records, length(tilt$cell_info$cells_present)))


#--- Per-bucket tilt diagnostics ------------------------------------------
cat('\n=== Per-bucket tilt convergence ===\n')
diag_rows = list()
for (b in names(tilt$tilt_diagnostics)) {
  d = tilt$tilt_diagnostics[[b]]
  diag_rows[[length(diag_rows) + 1L]] = tibble(
    bucket          = b,
    n_puf           = d$n,
    M               = d$n_kept_targets,
    max_rel         = d$max_rel,
    lambda_norm     = d$lambda_norm,
    n_iters         = d$n_iters,
    ESS_solver      = d$eff_donors_solver_mean,
    ESS_sample      = d$eff_donors_sampled_mean,
    KL_to_uniform   = d$kl_to_uniform_mean,
    out_of_range_n  = sum(d$out_of_range),
    status          = d$status
  )
}
diag_tbl = bind_rows(diag_rows) %>% arrange(desc(max_rel))
print(diag_tbl, n = Inf)
cat(sprintf('\nConverged: %d / %d buckets (max_rel < tolerance)\n',
            sum(diag_tbl$status == 'converged'), nrow(diag_tbl)))
cat(sprintf('Buckets with infeasible targets (target outside attainable): %d\n',
            sum(diag_tbl$out_of_range_n > 0)))


#--- Step B rescale factors -----------------------------------------------
cat('\n=== Step B rescale factors (tilt run) ===\n')
rf = tilt$rescale_factors %>% filter(applied)
cat(sprintf('factor: min=%.3f  p10=%.3f  median=%.3f  mean=%.3f  p90=%.3f  max=%.3f\n',
            min(rf$factor), quantile(rf$factor, 0.10),
            median(rf$factor), mean(rf$factor),
            quantile(rf$factor, 0.90), max(rf$factor)))
cat(sprintf('factors outside [0.95, 1.05]: %d / %d (%.1f%%)\n',
            sum(rf$factor < 0.95 | rf$factor > 1.05), nrow(rf),
            100 * mean(rf$factor < 0.95 | rf$factor > 1.05)))
cat(sprintf('factors outside [0.5, 2.0]:   %d / %d (%.1f%%)\n',
            sum(rf$factor < 0.5  | rf$factor > 2.0), nrow(rf),
            100 * mean(rf$factor < 0.5  | rf$factor > 2.0)))


#--- Aggregate / shares already in tilt diagnostic ------------------------
cat('\n=== Aggregate totals (SCF / pre-tilt / post-tilt+rescale) ===\n')
print(tilt$agg_table, n = Inf)

cat('\n=== Share of NW by income percentile ===\n')
print(tilt$inc_share_tbl, n = Inf)

cat('\n=== Share of NW by wealth percentile ===\n')
print(tilt$w_share_tbl, n = Inf)

cat('\n=== Step B per-category factor summary ===\n')
print(tilt$cat_summary)


#--- Compare against existing swap baseline if available ------------------
swap_2022 = file.path(output_dir, 'tax_units_2022.csv')
if (file.exists(swap_2022)) {
  cat(sprintf('\n=== Loading swap-baseline tax_units_2022.csv from %s ===\n',
              output_dir))
  swap_df = read_csv(swap_2022,
                     col_select = c('id','weight','dep_status', wealth_y_vars),
                     show_col_types = FALSE)
  swap_df = swap_df %>% filter(dep_status == 0L)

  # Categories on swap side.
  swap_cat = compute_category_values(swap_df)
  swap_df  = bind_cols(swap_df, swap_cat)

  cat('\n=== SWAP BASELINE: aggregate totals (sanity check) ===\n')
  swap_agg = tibble(category = CALIB_CATEGORIES,
                    swap_total_T = vapply(unname(CAT_COL),
                       function(c) sum(swap_df$weight * swap_df[[c]]) / 1e12,
                       numeric(1)))
  print(swap_agg)
} else {
  cat('\n[SKIP] No tax_units_2022.csv in output_dir; cannot compare against swap baseline.\n')
}

cat('\nDone.\n')
