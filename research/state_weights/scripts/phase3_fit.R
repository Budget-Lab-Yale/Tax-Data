#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/phase3_fit.R
#
# Phase 3 (plan.md §5a, decision S29): the per-year FILER fit, three ways.
# Non-filers are fitted per year by exact IPF under every arm (S37) -- their
# records are new each year -- so the architecture question is the filers',
# whose 207,692 PUF records persist 2017-2023 under the same ids (plus
# 717-749 Forbes records from 2022, with new ids each year).
#
#   A <year>   independent: config 7 cold, one year
#   B          warm-started chain: 2017 is A's fit; each later year starts
#              from the previous year's logits, matched by id. A record new
#              to the year (Forbes) starts at its own counts prior -- the
#              cold-start rule. Objective unchanged, so B's answer differs
#              from A's only by how unconverged 3,000 steps leave either
#   C <mult>   amortized: one joint fit of 2018-2023, plus
#              gamma * KL(P_y || P_{y-1}) on every persistent record, with
#              gamma = mult x config 7's beta (fit_gradient_panel(); gamma = 0
#              would be arm A exactly). 2018 on, because HT2 splits Puerto
#              Rico out of Other Areas that year: a 2017 record legitimately
#              moves OA -> PR, which the penalty would fight. C's 2017 is A's.
#   C <mult> B  the same, warm-started from arm B's fit of each year: tests
#              whether cold C's weaker targeted fit is the objective or the
#              3,000-step budget (B showed a cold start leaves it unconverged)
#   B pct, C <mult> B pct
#              the percentile arm (P6 re-run): the same chains with each
#              state's top-1%/5% return count and AGI added to the targets
#              (state_percentile_targets(), stored by prep_fit_inputs.R as
#              pct_targets). Written to B_pct/ and C_{mult}_warmB_pct/; the
#              warm start reads B_pct/
#
# Every arm uses config 7's counts prior as both start and KL anchor, and its
# beta, lr, schedule and 3,000 steps. Inputs are prep_fit_inputs.R's
# hardened_fit_{year}.rds under {TAXDATA_SCRATCH}/state_weights_tmp/.
# Writes {TAXDATA_SCRATCH}/{PHASE3_OUT:-phase3}/{arm}/fit_{year}.rds =
# list(P, theta, ids, jurisdictions).
#
# Since 2026-09-29 (Phase 4 P3) the chains live in src/nonfilers/sw_fit.R --
# warm_start_theta(), fit_filers_chain_B(), fit_filers_chain_C() -- and this
# script only loads inputs and saves fits. One consequence: arm B now fits
# 2017 itself rather than copying arm A's saved fit; the two are identical
# (the fit is deterministic), which phase3_compare_fits.R certified against
# the S39 fits. Set PHASE3_OUT to write somewhere other than phase3/ (the
# certification runs did).
#
# Usage (repo root, under sbatch):
#   Rscript research/state_weights/scripts/phase3_fit.R A 2019
#   Rscript research/state_weights/scripts/phase3_fit.R B
#   Rscript research/state_weights/scripts/phase3_fit.R C 10
#   Rscript research/state_weights/scripts/phase3_fit.R C 10 B
#   Rscript research/state_weights/scripts/phase3_fit.R B pct
#   Rscript research/state_weights/scripts/phase3_fit.R C 10 B pct
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(readxl) })
source('src/nonfilers/state_weights.R')

YEARS <- 2017:2023
IN  <- file.path(scratch_root(), 'state_weights_tmp')
OUT <- file.path(scratch_root(), Sys.getenv('PHASE3_OUT', unset = 'phase3'))

args <- commandArgs(trailingOnly = TRUE)
pct  <- tail(args, 1) == 'pct'          # the percentile arm
if (pct) args <- head(args, -1)
arm  <- args[1]
stopifnot(arm %in% c('A', 'B', 'C'), !(pct && arm == 'A'))
SUFFIX <- if (pct) '_pct' else ''

#' The filer partition for one year, with its record ids in partition order.
#' prep_fit_inputs.R built the inputs from Tax-Data merged with the baseline
#' by id, so its rows are id-sorted: re-read id and weight the same way and
#' assert the weights line up before trusting the ids.
load_filers <- function(year) {
  h <- readRDS(file.path(IN, sprintf('hardened_fit_%d.rds', year)))
  p <- h$inputs$filers
  tu <- fread(file.path(model_data_root(), 'Tax-Data/v1', h$tax_data_vintage, 'baseline',
                        sprintf('tax_units_%d.csv', year)), select = c('id', 'filer', 'weight'))
  setorder(tu, id)
  stopifnot(identical(tu$weight[p$idx], p$w), all(tu$filer[p$idx] == 1))
  p$ids <- tu$id[p$idx]
  p$jurisdictions <- h$inputs$jurisdictions
  if (pct) {
    stopifnot(length(h$pct_targets) > 0)
    p$targets <- c(p$targets, h$pct_targets)
  }
  p
}

save_fit <- function(dir, year, fit) {
  assert_split_shares(fit$P)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(fit[c('P', 'theta', 'ids', 'jurisdictions')], file.path(dir, sprintf('fit_%d.rds', year)))
}

if (arm == 'A') {
  year <- as.integer(args[2]); stopifnot(year %in% YEARS)
  p <- load_filers(year)
  f <- fit_filers_config7(p, verbose = TRUE)
  save_fit(file.path(OUT, 'A'), year, list(P = f$P, theta = f$theta, ids = p$ids, jurisdictions = p$jurisdictions))
}

if (arm == 'B') {
  fits <- fit_filers_chain_B(lapply(YEARS, load_filers), verbose = TRUE)
  for (k in seq_along(YEARS)) save_fit(file.path(OUT, paste0('B', SUFFIX)), YEARS[k], fits[[k]])
}

if (arm == 'C') {
  mult <- as.numeric(args[2]); stopifnot(is.finite(mult), mult > 0)
  warm <- length(args) >= 3
  if (warm) stopifnot(args[3] == 'B')
  C_YEARS <- YEARS[YEARS >= 2018]
  ps <- lapply(C_YEARS, load_filers)
  warm_fits <- if (warm) lapply(C_YEARS, function(y) readRDS(file.path(OUT, paste0('B', SUFFIX), sprintf('fit_%d.rds', y)))) else NULL
  c_dir <- sprintf('C_%g%s%s', mult, if (warm) '_warmB' else '', SUFFIX)
  res <- fit_filers_chain_C(ps, gamma_mult = mult, warm = warm_fits, verbose = TRUE)
  for (k in seq_along(C_YEARS)) save_fit(file.path(OUT, c_dir), C_YEARS[k], res$fits[[k]])
  saveRDS(res$loss_hist, file.path(OUT, c_dir, 'loss_hist.rds'))
}

message('PHASE3_FIT_DONE ', paste(args, collapse = ' '))
