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

# Modules with base_year > 2017 attach their outputs to module_deltas and
# extend factor_ledger with their post-base growth series. Empty for now —
# wealth migration (task 14) will populate this block.
module_deltas = list()


#-----------------------
# Phase 4: materialize + write
#-----------------------

source('./src/write_outputs.R')
