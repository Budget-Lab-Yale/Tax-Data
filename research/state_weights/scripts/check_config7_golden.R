#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/check_config7_golden.R
#
# Regression check for the split-weight engine: rebuild the TY2022 inputs from
# scratch with the current code, run the PRODUCTION entry point
# build_split_weights(method = "gradient") with no hyperparameters, and assert
# the results are identical() to the golden config-7 set written 2026-09-23 --
# the run that reproduced the Phase 1 bake-off exactly (95.3% / 0.43%).
#
# Three assertions, each exact rather than approximate:
#   1. inputs      build_weight_inputs() output == golden hardened_fit_2022.rds
#   2. prior       the counts-backbone IPF prior == golden prior_counts_2022.rds
#   3. production  build_split_weights()'s filer split == golden fit_7.rds,
#                  which is what shows the entry point now IS config 7
#
# Run after any change to src/nonfilers/sw_*.R. Needs TAXDATA_SCRATCH, and the
# golden set under {TAXDATA_SCRATCH}/state_weights_tmp/golden_ty2022_cfg7/.
# Memory: ~8 GB at peak; run under sbatch, not on a login node.
#
#   Rscript research/state_weights/scripts/check_config7_golden.R <baseline_detail_dir>
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr); library(stringr); library(yaml)
})
source('src/nonfilers/state_weights.R')

detail_dir <- commandArgs(trailingOnly = TRUE)[1]
stopifnot(!is.na(detail_dir))
YEAR <- 2022L; VINTAGE <- '2026030513'
GOLD <- file.path(scratch_root(), 'state_weights_tmp', 'golden_ty2022_cfg7')

INPUT_COLS <- c('id','weight','filer','filing_status','n_dep','age1',
                'wages','txbl_int','exempt_int','div_ord','div_pref',
                'kg_st','kg_lt','salt_inc_sales','salt_prop',
                'first_mort_int','second_mort_int','txbl_pens_dist',
                'txbl_ira_dist','gross_ss','sole_prop')
tu <- fread(file.path(model_data_root(), 'Tax-Data/v1', VINTAGE, 'baseline',
                      sprintf('tax_units_%d.csv', YEAR)), select = INPUT_COLS)
tu <- merge(tu, fread(file.path(detail_dir, sprintf('%d.csv', YEAR)), select = c('id','agi','eitc')),
            by = 'id', all.x = TRUE)
stopifnot(!anyNA(tu$agi), !anyNA(tu$eitc))

report <- function(label, ok) { cat(sprintf('  %-11s %s\n', label, if (ok) 'IDENTICAL' else '*** DIFFERS ***')); ok }

cat('1. inputs\n')
inputs <- build_weight_inputs(tu, YEAR)
ok1 <- report('inputs', identical(inputs, readRDS(file.path(GOLD, 'hardened_fit_2022.rds'))$inputs))

cat('2. counts-backbone prior\n')
p <- inputs$filers
counts <- Filter(function(t) t$series == CONFIG_7$prior_series, p$targets)
prior  <- fit_calibration(p$w, p$P0, counts, n_iter = CONFIG_7$prior_n_iter)$P
ok2 <- report('prior', identical(prior, readRDS(file.path(GOLD, 'prior_counts_2022.rds'))))

cat('3. production entry point, no hyperparameters supplied\n')
got <- build_split_weights(tu, YEAR, method = 'gradient', inputs = inputs)
gold_P <- readRDS(file.path(GOLD, 'fit_7.rds'))$P
W <- p$w * gold_P
want <- data.table(id = rep(tu$id[p$idx], ncol(W)), state = rep(inputs$jurisdictions, each = nrow(W)),
                   weight = as.vector(W))[weight > 0]
got_filers <- got[id %in% tu$id[p$idx]]
# Compare columns, not the tables: identical() on two data.tables can report a
# difference from their internal self-reference pointers alone.
ok3 <- report('production', nrow(got_filers) == nrow(want) &&
                identical(got_filers$id, want$id) && identical(got_filers$state, want$state) &&
                identical(got_filers$weight, want$weight))
cat(sprintf('\n%s\n', if (ok1 && ok2 && ok3) 'CHECK_CONFIG7_GOLDEN_PASS' else 'CHECK_CONFIG7_GOLDEN_FAIL'))
if (!(ok1 && ok2 && ok3)) quit(status = 1)
