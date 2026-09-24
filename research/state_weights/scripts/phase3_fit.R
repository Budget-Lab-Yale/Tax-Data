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
#
# Every arm uses config 7's counts prior as both start and KL anchor, and its
# beta, lr, schedule and 3,000 steps. Inputs are prep_fit_inputs.R's
# hardened_fit_{year}.rds under {TAXDATA_SCRATCH}/state_weights_tmp/.
# Writes {TAXDATA_SCRATCH}/phase3/{arm}/fit_{year}.rds = list(P, theta, ids).
#
# Usage (repo root, under sbatch):
#   Rscript research/state_weights/scripts/phase3_fit.R A 2019
#   Rscript research/state_weights/scripts/phase3_fit.R B
#   Rscript research/state_weights/scripts/phase3_fit.R C 10
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(readxl) })
source('src/nonfilers/state_weights.R')

YEARS <- 2017:2023
IN  <- file.path(scratch_root(), 'state_weights_tmp')
OUT <- file.path(scratch_root(), 'phase3')

args <- commandArgs(trailingOnly = TRUE)
arm  <- args[1]
stopifnot(arm %in% c('A', 'B', 'C'))

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
  p
}

save_fit <- function(dir, year, P, theta, ids, jurisdictions) {
  stopifnot(all(P >= 0), max(abs(rowSums(P) - 1)) < 1e-8)   # the split invariant
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(P = P, theta = theta, ids = ids, jurisdictions = jurisdictions),
          file.path(dir, sprintf('fit_%d.rds', year)))
}

if (arm == 'A') {
  year <- as.integer(args[2]); stopifnot(year %in% YEARS)
  p <- load_filers(year)
  f <- fit_filers_config7(p, verbose = TRUE)
  save_fit(file.path(OUT, 'A'), year, f$P, f$theta, p$ids, p$jurisdictions)
}

if (arm == 'B') {
  prev <- readRDS(file.path(OUT, 'A', sprintf('fit_%d.rds', YEARS[1])))
  save_fit(file.path(OUT, 'B'), YEARS[1], prev$P, prev$theta, prev$ids, prev$jurisdictions)
  for (year in YEARS[-1]) {
    p <- load_filers(year)
    # Start each carried record at last year's split, by jurisdiction name.
    # Jurisdictions can grow (PR enters HT2 in 2018): a column new to the
    # year gets the record's prior mass there, and the carried split is
    # scaled to make room. A new record starts at its prior.
    prior <- config7_prior(p)
    theta0 <- log(pmax(prior, 1e-12))
    rows <- match(p$ids, prev$ids)
    keep <- which(!is.na(rows))
    cols <- match(prev$jurisdictions, p$jurisdictions)
    stopifnot(!anyNA(cols))
    new_cols <- setdiff(seq_along(p$jurisdictions), cols)
    q <- if (length(new_cols)) rowSums(prior[keep, new_cols, drop = FALSE]) else 0
    theta0[keep, cols] <- log(pmax(prev$P[rows[keep], ] * (1 - q), 1e-12))
    message(sprintf('TY%d: %d records warm-started, %d cold (new ids)',
                    year, length(keep), length(p$ids) - length(keep)))
    f <- fit_filers_config7(p, verbose = TRUE, theta0 = theta0)
    save_fit(file.path(OUT, 'B'), year, f$P, f$theta, p$ids, p$jurisdictions)
    prev <- list(P = f$P, ids = p$ids, jurisdictions = p$jurisdictions)
  }
}

if (arm == 'C') {
  mult <- as.numeric(args[2]); stopifnot(is.finite(mult), mult > 0)
  C_YEARS <- YEARS[YEARS >= 2018]
  ps <- lapply(C_YEARS, load_filers)
  J  <- ps[[1]]$jurisdictions
  stopifnot(all(vapply(ps, function(p) identical(p$jurisdictions, J), logical(1))))
  years <- lapply(ps, function(p) list(w = p$w, P0 = config7_prior(p),
                                       targets = p$targets, ids = p$ids))
  rm(ps); invisible(gc())
  gamma <- mult * CONFIG_7$beta
  message(sprintf('arm C: gamma = %g (%g x beta), %d years', gamma, mult, length(YEARS)))
  f <- fit_gradient_panel(years, gamma = gamma, beta = CONFIG_7$beta, lr = CONFIG_7$lr,
                          n_steps = CONFIG_7$n_steps, lr_schedule = CONFIG_7$lr_schedule,
                          verbose = TRUE)
  for (k in seq_along(C_YEARS)) {
    save_fit(file.path(OUT, sprintf('C_%g', mult)), C_YEARS[k], f$P[[k]], f$theta[[k]],
             years[[k]]$ids, J)
  }
  saveRDS(f$loss_hist, file.path(OUT, sprintf('C_%g', mult), 'loss_hist.rds'))
}

message('PHASE3_FIT_DONE ', paste(args, collapse = ' '))
