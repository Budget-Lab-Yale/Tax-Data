#---------------------------------------------
# main_phase12_snapshot.R
#
# Stripped main.R: runs Phase 1 + Phase 2 of the
# pipeline, materializes puf_2022, saves snapshot,
# and stops. Skips Phase 3 (wealth imputation)
# and Phase 4 (per-year CSV writing) — neither is
# needed for the SCF-vs-PUF age/income diagnostic.
#
# Output: puf_2022_snapshot.rds in the standard
# output_path (timestamped vintage dir).
#---------------------------------------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Read runtime configuration params and set filepaths
source('./src/configure.R')

# Override: load cached imputation models instead of retraining. The
# user's upstream fix is in the LP age imputation, so we DO need do_lp=1
# (that re-solves and overwrites resources/cache/lp/weight_deltas.rds),
# but other QRFs/DRFs are unchanged and can be loaded.
estimate_models = 0L
cat(sprintf('main_phase12_snapshot: estimate_models=%d, do_lp=%d\n',
            estimate_models, do_lp))

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
# Materialize puf_2022 + save snapshot
#-----------------------

source('./src/materialize.R')

module_deltas = list()  # empty: no Phase 3 yet
puf_2022 = materialize(2022L, tax_units, factor_ledger, weight_ledger,
                       module_deltas)

snap_path = file.path(output_path, 'puf_2022_snapshot.rds')
write_rds(puf_2022, snap_path)
cat(sprintf('Wrote snapshot: %s (%d rows)\n', snap_path, nrow(puf_2022)))
cat(sprintf('output_path = %s\n', output_path))

# Persist the output_path so the chained diagnostic script can find it
# without needing to be told.
writeLines(output_path, '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/.last_phase12_output_path')

cat('\nDone — Phase 1+2 + snapshot saved.\n')
