#---------------------------------------------
# main_accruals_validate.R
#
# Wrapper around main.R for the accruals integration validation run
# (per /home/jar335/.claude/plans/glistening-churning-moonbeam.md §B/§C).
# Loads upstream caches but runs the full wealth imputation so the new
# accruals.* columns are computed against real Stage-3-calibrated wealth.
#
# Knob differences from main.R:
#   - estimate_models = 0  (load cached imputation models incl. wealth DRFs)
#   - do_lp           = 0  (load cached LP reweight)
#   - skip_tilt       = FALSE in the wealth call (i.e. real production wealth)
#
# Pre-requisites:
#   - Per-cell DRF caches at resources/cache/wealth_percell_*_mns50.rds.
#     The earlier comparison job (10155346) wrote these.
#   - Stage 1 cache at resources/cache/scf_tax_units.rds either deleted or
#     older than src/imputations/stage1_scf_tax_units.R (so the new
#     dc_equity_share_scf column is produced). The SLURM wrapper deletes
#     the stale cache before invoking this script.
#---------------------------------------------


lapply(readLines('requirements.txt'), library, character.only = T)
source('./src/configure.R')

# Load caches: don't retrain DRFs or re-solve LP.
estimate_models = 0L
do_lp           = 0L
cat(sprintf('main_accruals_validate: estimate_models=%d, do_lp=%d\n',
            estimate_models, do_lp))

set.seed(76)


#-----------------------
# Phase 1: base construction
#-----------------------

source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
source('./src/impute_nonfilers.R')
source('./src/impute_variables.R')


#-----------------------
# Phase 2: projection ledger
#-----------------------

source('./src/project_puf.R')


#-----------------------
# Phase 3: donor-year imputations (real tilt + Step B)
#-----------------------

source('./src/materialize.R')
source('./src/record_bucket.R')
source('./src/dfa_factors.R')
module_deltas = list()

source('./src/imputations/stage1_scf_tax_units.R')
source('./src/imputations/wealth.R')

puf_2022 = materialize(2022L, tax_units, factor_ledger, weight_ledger,
                       module_deltas)
write_rds(puf_2022, file.path(output_path, 'puf_2022_snapshot.rds'))

record_bucket = build_record_bucket(puf_2022)
write_rds(record_bucket, file.path(output_path, 'record_bucket.rds'))

bucketed_factor_ledger = build_wealth_bucketed_factors(
  weight_ledger, record_bucket, macro_projections)
write_rds(bucketed_factor_ledger,
          file.path(output_path, 'bucketed_factor_ledger.rds'))

wealth_result = run_wealth_imputation(puf_2022, scf_tax_units)
module_deltas[['wealth']] = list(base_year = 2022L,
                                  values = wealth_result$y)

write_rds(wealth_result$y_pre_tilt,
          file.path(output_path, 'wealth_pre_tilt.rds'))
write_rds(wealth_result$y_post_tilt_pre_rescale,
          file.path(output_path, 'wealth_post_tilt_pre_rescale.rds'))
write_rds(wealth_result$qc_report,
          file.path(output_path, 'stage3_qc_report.rds'))
write_rds(wealth_result$rescale_factors,
          file.path(output_path, 'rescale_factors.rds'))
write_rds(wealth_result$tilt_diagnostics,
          file.path(output_path, 'tilt_diagnostics.rds'))

rm(puf_2022, wealth_result)


#-----------------------
# Phase 4: materialize + write
#-----------------------

source('./src/write_outputs.R')

cat('\nmain_accruals_validate: DONE.\n')
cat(sprintf('output_path = %s\n', output_path))
