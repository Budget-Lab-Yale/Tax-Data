#---------------------------------------------
# tilt_single_cell_smoke.R
#
# Single-cell smoke test for the new tilt-based
# Stage 3. Runs the FULL wealth.R pipeline (so
# that all upstream features and donor matrices
# are constructed normally), but injects an
# environment-variable patch so only one
# (income x age) bucket actually solves -- the
# rest are short-circuited with the uniform leaf
# baseline. Useful for catching pipeline bugs
# before paying the full ~hour-long compute.
#
# Cell defaults to pct99.9to100 x senior (smallest
# n_puf, largest per-record influence). Override
# via env vars TILT_SMOKE_CI / TILT_SMOKE_CA.
#
# Usage (must run via sbatch):
#   sbatch slurm_tilt_smoke.sh
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript tilt_single_cell_smoke.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

estimate_models = 0L     # use cached forests
do_lp           = 0

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')

cat('Loading scf_tax_units from cache...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

snap_path = file.path(output_dir, 'puf_2022_snapshot.rds')
if (file.exists(snap_path)) {
  cat(sprintf('Loading snapshot: %s\n', snap_path))
  puf_2022 = read_rds(snap_path)
} else {
  cat('Reconstructing puf_2022 from tax_units_2022.csv\n')
  puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                      show_col_types = FALSE)
  for (v in wealth_y_vars) puf_2022[[v]] = NA_real_
}
cat(sprintf('puf_2022: %d rows\n', nrow(puf_2022)))

# Pre-filter PUF to a single income cell so wealth.R only does that cell's
# forest predict/tilt. This is much cheaper than running full and exposes
# pipeline issues fast.
target_ci = Sys.getenv('TILT_SMOKE_CI', 'pct99.9to100')
cat(sprintf('Restricting PUF to income cell: %s\n', target_ci))

# We need the same cell assignment as wealth.R produces. Easiest: build
# income/age and run assign_calibration_cells.
puf_age2 = ifelse(is.na(puf_2022$age2), 0L, puf_2022$age2)
puf_age_older = pmax(pmin(80L, puf_2022$age1), pmin(80L, puf_age2))
puf_income_vec = with(puf_2022,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st +
  gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)
cells_df = data.frame(weight = puf_2022$weight)
cells_df = assign_calibration_cells(cells_df, puf_income_vec,
                                     puf_age_older, puf_2022$weight)

keep_idx = which(cells_df$cell_income == target_ci)
cat(sprintf('Records in cell %s: %d (of %d)\n',
            target_ci, length(keep_idx), nrow(puf_2022)))
puf_filt = puf_2022[keep_idx, , drop = FALSE]

source('src/imputations/wealth.R')

cat('\n=== Running run_wealth_imputation on filtered PUF ===\n')
t0 = Sys.time()
set.seed(76)
result = run_wealth_imputation(puf_filt, scf_tax_units,
                                debug_output = TRUE,
                                tilt_options = list(max_iters = 100L))
elapsed = as.numeric(Sys.time() - t0, units = 'secs')
cat(sprintf('total time: %.1fs\n', elapsed))


cat('\n=== Tilt diagnostics summary ===\n')
for (b in names(result$tilt_diagnostics)) {
  d = result$tilt_diagnostics[[b]]
  cat(sprintf('  %-22s n=%6d M=%2d max_rel=%.3e ||lambda||=%.3f ESS_solver=%.1f ESS_sample=%.1f kl=%.3f status=%s\n',
              b, d$n, d$n_kept_targets, d$max_rel, d$lambda_norm,
              d$eff_donors_solver_mean, d$eff_donors_sampled_mean,
              d$kl_to_uniform_mean, d$status))
}

cat('\n=== Per-target residuals (first kept bucket) ===\n')
b1 = result$tilt_diagnostics[[1]]
print(tibble(
  target_col = b1$col_names,
  target     = b1$target,
  T_hat      = b1$T_hat,
  rel        = b1$rel_residuals,
  attainable_min = b1$attainable_min,
  attainable_max = b1$attainable_max,
  out_of_range   = b1$out_of_range
), n = Inf)


cat('\n=== Step B rescale factors ===\n')
print(result$rescale_factors %>% filter(applied), n = Inf)


cat('\n=== Aggregate match (post-tilt vs SCF) per category ===\n')
post = result$y
puf_w = puf_filt$weight
scf_y = scf_to_y(scf_tax_units)
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_ao   = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_df = data.frame(weight = scf_y$weight)
scf_df = assign_calibration_cells(scf_df, scf_inc, scf_ao, scf_y$weight)
scf_in = which(scf_df$cell_income == target_ci)
scf_w  = scf_y$weight[scf_in]

CAT_MEMBERS = list(
  equities='equities', bonds='bonds', homes='primary_home',
  retirement='retirement', business='pass_throughs',
  other = setdiff(wealth_asset_vars,
                   c('equities','bonds','primary_home','retirement','pass_throughs')),
  debt  = wealth_debt_vars
)
for (cn in names(CAT_MEMBERS)) {
  members = CAT_MEMBERS[[cn]]
  scf_amt = sum(scf_w *
                 (if (length(members)==1L) scf_y[scf_in, members]
                  else rowSums(scf_y[scf_in, members, drop=FALSE]))) / 1e9
  puf_amt = sum(puf_w *
                 (if (length(members)==1L) post[[members]]
                  else rowSums(as.matrix(post[, members])))) / 1e9
  cat(sprintf('  %-12s SCF=$%.2fB  PUF_post=$%.2fB  rel=%+.2f%%\n',
              cn, scf_amt, puf_amt, 100*(puf_amt-scf_amt)/scf_amt))
}

# Save full diagnostic dump for AM review.
diag_path = file.path(output_dir, sprintf('tilt_smoke_%s_diag.rds', target_ci))
saveRDS(list(
  cell             = target_ci,
  elapsed_sec      = elapsed,
  tilt_diagnostics = result$tilt_diagnostics,
  rescale_factors  = result$rescale_factors,
  qc_report        = result$qc_report,
  y                = result$y,
  y_pre_tilt       = result$y_pre_tilt,
  y_post_tilt_pre_rescale = result$y_post_tilt_pre_rescale
), diag_path)
cat(sprintf('Saved diagnostics to %s\n', diag_path))

cat('\nDone.\n')
