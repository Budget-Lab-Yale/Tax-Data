#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/prep_fit_inputs.R
#
# Rebuild the cached engine inputs the sweep harness reads,
# {TAXDATA_SCRATCH}/state_weights_tmp/hardened_fit_{year}.rds, from a pinned
# Tax-Data vintage plus a Tax-Simulator baseline that supplies each record's
# calculated `agi` and `eitc` (S30: the fit is two-pass, because assign_ht2_stub
# stratifies every filer target by calculated AGI).
#
# Written 2026-09-23 because the original file was lost when scratch was
# reclaimed on 2026-08-27 (state_weights_phase1_summary.md section 9), and
# nothing recorded how to regenerate it.
#
# Usage (from the repo root):
#   Rscript research/state_weights/scripts/prep_fit_inputs.R \
#     <year> <tax_data_vintage> <baseline_detail_dir>
#
# The detail dir must hold {year}.csv with id, agi, eitc for the same records.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr); library(stringr); library(yaml)
})
source('src/nonfilers/state_weights.R')

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3)
year       <- as.integer(args[1])
td_vintage <- args[2]
detail_dir <- args[3]

INPUT_COLS <- c('id','weight','filer','filing_status','n_dep','age1',
                'wages','txbl_int','exempt_int','div_ord','div_pref',
                'kg_st','kg_lt','salt_inc_sales','salt_prop',
                'first_mort_int','second_mort_int','txbl_pens_dist',
                'txbl_ira_dist','gross_ss','sole_prop')

td_file <- file.path(model_data_root(), 'Tax-Data/v1', td_vintage, 'baseline',
                     sprintf('tax_units_%d.csv', year))
tu     <- fread(td_file, select = INPUT_COLS)
detail <- fread(file.path(detail_dir, sprintf('%d.csv', year)), select = c('id','agi','eitc'))

# Every Tax-Data record must have a calculated AGI: a missing one would drop
# the record out of its HT2 stub silently.
tu <- merge(tu, detail, by = 'id', all.x = TRUE)
stopifnot(nrow(tu) == nrow(fread(td_file, select = 'id')), !anyNA(tu$agi), !anyNA(tu$eitc))
message(sprintf('%d: %s records from Tax-Data %s, %.2fM weighted, AGI and EITC joined for all',
                year, format(nrow(tu), big.mark = ','), td_vintage, sum(tu$weight) / 1e6))

inputs <- build_weight_inputs(tu, year)
message(sprintf('filer targets: %d   non-filer targets: %d',
                length(inputs$filers$targets), length(inputs$nonfilers$targets)))

out_dir <- file.path(scratch_root(), 'state_weights_tmp')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out <- file.path(out_dir, sprintf('hardened_fit_%d.rds', year))
saveRDS(list(inputs = inputs, tax_data_vintage = td_vintage, detail_dir = detail_dir), out)
message('wrote ', out)
