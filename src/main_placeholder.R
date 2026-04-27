#---------------------------------------------
# main_placeholder.R
#
# Wrapper around main.R for producing a fast
# placeholder Tax-Data interface (all columns
# present, data may be coarse). Differs from
# main.R only in three knobs:
#   - estimate_models = 0 (load cached imputation models)
#   - do_lp           = 0 (load cached LP reweight)
#   - skip_tilt       = TRUE in the wealth call
#                       (Stage 2 raw-DRF leaf draw, no Stage 3 tilt,
#                        no Step B rescale)
#
# Used to produce a downstream-contract output where
# the column shape is what matters; not for production
# wealth values.
#---------------------------------------------


#-----------------------
# Bootstrap
#-----------------------

lapply(readLines('requirements.txt'), library, character.only = T)
source('./src/configure.R')

# OVERRIDE: load caches everywhere we can.
# The fresh LP cache from the just-completed Phase 1+2 run is at
# resources/cache/lp/weight_deltas.rds; do_lp=0 picks that up.
estimate_models = 0L
do_lp           = 0L
cat(sprintf('main_placeholder: estimate_models=%d, do_lp=%d\n',
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
# Phase 3: donor-year imputations (skip_tilt)
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

# Wealth — skip_tilt = TRUE means raw DRF leaf draw, no calibration.
# Wealth values are coherent (real SCF donor records) but not aggregate-
# matched. Fine for placeholder column contracts.
wealth_result = run_wealth_imputation(puf_2022, scf_tax_units,
                                       skip_tilt = TRUE)
module_deltas[['wealth']] = list(base_year = 2022L,
                                  values = wealth_result$y)

write_rds(wealth_result$y_pre_tilt,
          file.path(output_path, 'wealth_pre_tilt.rds'))
write_rds(wealth_result$qc_report,
          file.path(output_path, 'stage3_qc_report.rds'))
write_rds(wealth_result$rescale_factors,
          file.path(output_path, 'rescale_factors.rds'))

rm(puf_2022, wealth_result)


#-----------------------
# Phase 4: materialize + write
#-----------------------

source('./src/write_outputs.R')

cat('\nmain_placeholder: DONE.\n')
cat(sprintf('output_path = %s\n', output_path))
