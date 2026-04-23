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
# Wealth aging factors post-2022 (DFA 2023–25, macro 2026+) are NOT yet
# added to factor_ledger — follow-up task. Wealth values therefore stay
# frozen at 2022 levels through 2097 for this PR.
source('./src/materialize.R')
module_deltas = list()

# Wealth — SCF 2022 donor, runs at 2022 base.
source('./src/imputations/stage1_scf_tax_units.R')
source('./src/imputations/wealth.R')

puf_2022 = materialize(2022L, tax_units, factor_ledger, weight_ledger,
                       module_deltas)
wealth_delta = run_wealth_imputation(puf_2022, scf_tax_units)
module_deltas[['wealth']] = list(base_year = 2022L, values = wealth_delta)
rm(puf_2022, wealth_delta)


#-----------------------
# Phase 4: materialize + write
#-----------------------

source('./src/write_outputs.R')
