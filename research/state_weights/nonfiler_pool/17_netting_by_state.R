#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 17_netting_by_state.R  (decision S37)
#
# The claimed adult dependents who do not file, by the dependent's ASEC state:
# the netting the split-weight fit subtracts from each state's residual anchor
# before sharing the non-filer adult total across states. It is 04's own
# netting -- same join, same calibrated filing probabilities -- summed by state
# instead of by age band, and it ties out to 04's national figure exactly.
#
# Committed because production (sw_fit.R, read_nonfiler_state_anchor()) cannot
# read the pool's .rds intermediates, which are gitignored.
#
# Inputs: results/units_{year}.rds (01), results/calibrated_units_{year}.rds
#         and results/netting_sensitivity_{year}.csv (04)
# Writes: results/netting_by_state_{year}.csv
#
# Login-node safe.
#   Rscript research/state_weights/nonfiler_pool/17_netting_by_state.R [year ...]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table) })
source('src/nonfilers/state_weights.R')

args  <- commandArgs(trailingOnly = TRUE)
YEARS <- if (length(args)) as.integer(args) else 2017:2023

for (yr in YEARS) {
  net <- netting_by_state(asec_adult_dependents(yr))
  stopifnot(nrow(net) == 51)
  fwrite(net, file.path(NONFILER_POOL_RESULTS, sprintf('netting_by_state_%d.csv', yr)))
  message(sprintf(paste('TY%d: %.2fM netted adult dependents (ties to 04), %.2fM',
                        'full-time students 18-24; sample %d-%d per state'),
                  yr, sum(net$asec_netting) / 1e6, sum(net$asec_ft_student) / 1e6,
                  min(net$asec_netting_n), max(net$asec_netting_n)))
}
