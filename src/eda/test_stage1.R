#--------------------------------------
# test_stage1.R
#
# Smoke test for stage1_scf_tax_units.R.
# Loads configure.R + stage1, runs the
# port against SCF 2022, prints sanity
# diagnostics and caches the output.
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')

set.seed(76)

cat('========== Stage 1: SCF 2022 → tax units ==========\n\n')
source('./src/imputations/stage1_scf_tax_units.R')
cat('\n========== Stage 1 complete ==========\n')
