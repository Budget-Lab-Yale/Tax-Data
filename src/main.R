#---------------------------------------------
# main.R
#
# Entry point into Tax-Data processing module.
# Four explicit phases:
#   1. Base construction  — 2017-native PUF + imputations.
#   2. Projection ledger  — project_puf.R produces factor_ledger +
#                            weight_ledger, no per-year tibble mutation.
#   3. Donor-year imputations — modules with base_year > 2017 run at
#                            their native year, producing module_deltas.
#                            (Empty until wealth migration lands.)
#   4. Materialize + write — for each year 2017..2097, materialize via
#                            (base, factor_ledger, weight_ledger,
#                            module_deltas) and write tax_units_{year}.csv.
#---------------------------------------------


#-----------------------
# Bootstrap
#-----------------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Read runtime configuration params and set filepaths
source('./src/configure.R')

# Set random seed
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
# Phase 3: donor-year imputations
#-----------------------

# Modules with base_year > 2017 attach their outputs to module_deltas.
# Each module: (i) materializes the PUF at its base_year using the ledger
# built in Phase 2, (ii) imputes its variables against that state, (iii)
# stores the base-year values in module_deltas.
#
source('./src/materialize.R')
source('./src/record_bucket.R')
source('./src/dfa_factors.R')
module_deltas = list()

# Wealth — SCF 2022 donor, runs at 2022 base.
source('./src/imputations/stage1_scf_tax_units.R')
source('./src/imputations/wealth.R')

puf_2022 = materialize(2022L, tax_units, factor_ledger, weight_ledger,
                       module_deltas)

# Freeze per-record income bucket at 2022. Used by Phase 4 together with
# bucketed_factor_ledger to age wealth Y-vars by DFA income percentile.
record_bucket = build_record_bucket(puf_2022)
write_rds(record_bucket, file.path(output_path, 'record_bucket.rds'))

# Bucketed growth factors for wealth Y-vars: DFA 2023..last_dfa_year, then
# per-household GDP compounding 2026+ on top of each bucket's final DFA
# cumulative. All 23 wealth Y-vars live in this ledger (none in
# factor_ledger) — the invariant in materialize() guarantees no double-
# multiplication.
bucketed_factor_ledger = build_wealth_bucketed_factors(
  weight_ledger, record_bucket, macro_projections)
write_rds(bucketed_factor_ledger,
          file.path(output_path, 'bucketed_factor_ledger.rds'))
cat(sprintf('main.R: bucketed_factor_ledger built (%d rows)\n',
            nrow(bucketed_factor_ledger)))

wealth_result = run_wealth_imputation(puf_2022, scf_tax_units)
module_deltas[['wealth']] = list(base_year = 2022L, values = wealth_result$y)

# Diagnostic artifacts for src/eda/wealth_summary_diagnostics.R: the
# pre-tilt (Stage 2 only, uniform leaf draw) donors and the QC report
# describing which requested Stage 3 targets were kept/dropped.
write_rds(wealth_result$y_pre_tilt,
          file.path(output_path, 'wealth_pre_tilt.rds'))
write_rds(wealth_result$qc_report,
          file.path(output_path, 'stage3_qc_report.rds'))

rm(puf_2022, wealth_result)


#-----------------------
# Phase 4: materialize + write
#-----------------------

source('./src/write_outputs.R')
