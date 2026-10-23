#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/phase3_score.R
#
# Scores the Phase 3 arms (phase3_fit.R) on the Phase 1 battery, per year,
# plus the metric single-year work never needed -- year-over-year stability
# (S29):
#
#   per (arm, year)   targeted within-2% and MARD; held-out HT2 within-2% and
#                     MARD (heldout_ht2.R); near-degenerate rows (max share
#                     > 0.99); min-state Kish effective sample size
#   per (arm, y-1->y) churn: weighted mean total-variation distance between a
#                     persistent record's split in consecutive years; and the
#                     held-out SHARE-CHANGE error: per (series, stub, state),
#                     |fitted share growth - HT2 share growth|. Stability is
#                     scored against HT2's own year-over-year movement, never
#                     as smoothness alone, which arm C would win by construction
#
# Arm C fits 2018-2023 only, so its 2017 is A's, and the 2017->2018
# transition is scored for A and B alone. Pilot-state liability (Phase 1
# §5.3) is not scored: it needs Tax-Simulator's state calculator on the
# current build's records, whose cross-model cache predates Block E.
#
# Usage (repo root, under sbatch; reads {TAXDATA_SCRATCH}/phase3/):
#   Rscript research/state_weights/scripts/phase3_score.R [arm ...]   (default: all five)
# Writes research/state_weights/scripts/results/phase3_{metrics,heldout_by_series,yoy}.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(readxl); library(stringr)
})
source('src/nonfilers/state_weights.R')
source('research/state_weights/scripts/heldout_ht2.R')

YEARS <- 2017:2023
args  <- commandArgs(trailingOnly = TRUE)
ARMS  <- if (length(args)) args else c('A', 'B', 'C_1', 'C_10', 'C_100')
IN    <- file.path(scratch_root(), 'state_weights_tmp')
FITS  <- file.path(scratch_root(), 'phase3')
OUT   <- 'research/state_weights/scripts/results'
TAG   <- Sys.getenv('PHASE3_TAG')   # e.g. '_p6': results beside, not over, the S39 run's
DEGENERATE <- 0.99      # a row this concentrated on one state is near-degenerate

# Arm C fits 2018 on; its 2017 is the fit it was warm-started from (B and A
# are identical in 2017, the fit being deterministic; Phase 4 P3), else A's
fit_path <- function(arm, year) {
  a <- if (startsWith(arm, 'C_') && year < 2018) {
    if (grepl('_warmB', arm)) sub('^C_[0-9.]+_warm', '', arm) else 'A'
  } else arm
  file.path(FITS, a, sprintf('fit_%d.rds', year))
}

#' The filer partition and its records, id-sorted as prep_fit_inputs.R built them
load_year <- function(year) {
  h <- readRDS(file.path(IN, sprintf('hardened_fit_%d.rds', year)))
  tu <- fread(file.path(model_data_root(), 'Tax-Data/v1', h$tax_data_vintage, 'baseline',
                        sprintf('tax_units_%d.csv', year)),
              select = c('id', 'filer', 'weight', 'sole_prop', 'txbl_pens_dist',
                         'txbl_ira_dist', 'kg_st', 'kg_lt'))
  det <- fread(file.path(h$detail_dir, sprintf('%d.csv', year)),
               select = c('id', 'agi', 'txbl_ss', 'liab_iit'))
  n <- nrow(tu)
  tu <- merge(tu, det, by = 'id')
  stopifnot(nrow(tu) == n, !anyNA(tu))
  p <- h$inputs$filers
  tu_f <- tu[p$idx]
  stopifnot(identical(tu_f$weight, p$w))
  list(p = p, jur = h$inputs$jurisdictions, tu_f = tu_f)
}

#' Targeted errors, all series x states at once, via the engine's own grouping
targeted_err <- function(p, P) {
  groups <- gradient_groups(p$w, p$targets, ncol(P))
  unlist(lapply(groups, function(g) {
    That <- crossprod(g$X, P[g$rows, , drop = FALSE])
    abs(That[g$has] / g$Tm[g$has] - 1)
  }))
}

shares_of <- function(h) {
  h[, `:=`(f_share = that / sum(that), h_share = target / sum(target)), by = .(series, stub)]
  h[, .(series, stub, state, f_share, h_share, err)]
}

metrics <- list(); by_series <- list(); yoy <- list()
prev <- NULL
for (year in YEARS) {
  d <- load_year(year)
  cells <- heldout_cells(year, d$p, d$tu_f, d$jur)
  cur <- list()
  for (arm in ARMS) {
    f <- readRDS(fit_path(arm, year))
    stopifnot(identical(f$ids, d$tu_f$id), identical(f$jurisdictions, d$jur),
              max(abs(rowSums(f$P) - 1)) < 1e-8)
    te <- targeted_err(d$p, f$P)
    ho <- score_heldout(cells, d$p$w, f$P)
    W  <- d$p$w * f$P
    ess <- colSums(W)^2 / colSums(W^2)
    metrics[[length(metrics) + 1]] <- data.table(
      arm = arm, year = year,
      fit_source = basename(dirname(fit_path(arm, year))),
      targeted_within2 = mean(te <= 0.02), targeted_mard = mean(te),
      heldout_within2 = mean(ho$err <= 0.02), heldout_mard = mean(ho$err),
      degenerate_share = mean(apply(f$P, 1, max) > DEGENERATE),
      kish_min = min(ess[ess > 0]), kish_min_state = d$jur[ess > 0][which.min(ess[ess > 0])])
    by_series[[length(by_series) + 1]] <-
      ho[, .(heldout_mard = mean(err), heldout_within2 = mean(err <= 0.02)), by = series][
        , `:=`(arm = arm, year = year)]
    cur[[arm]] <- list(P = f$P, ids = f$ids, w = d$p$w, jur = d$jur, sh = shares_of(ho))

    if (!is.null(prev)) {
      q <- prev[[arm]]
      # C's first own year is 2018: its 2017->2018 step would be A->C
      if (!(startsWith(arm, 'C_') && year == 2018)) {
        rows <- match(cur[[arm]]$ids, q$ids); keep <- which(!is.na(rows))
        cols <- match(q$jur, d$jur)
        Pc <- f$P[keep, cols, drop = FALSE]
        tv <- 0.5 * rowSums(abs(Pc - q$P[rows[keep], , drop = FALSE])) +
              0.5 * rowSums(f$P[keep, -cols, drop = FALSE])      # mass in new columns
        m <- merge(cur[[arm]]$sh, q$sh, by = c('series', 'stub', 'state'),
                   suffixes = c('', '_prev'))
        m <- m[f_share_prev > 0 & h_share_prev > 0]
        m[, `:=`(f_growth = f_share / f_share_prev, h_growth = h_share / h_share_prev)]
        yoy[[length(yoy) + 1]] <- data.table(
          arm = arm, from = year - 1L, to = year,
          churn_tv = sum(d$p$w[keep] * tv) / sum(d$p$w[keep]),
          heldout_share_change_err = mean(abs(m$f_growth - m$h_growth)),
          heldout_fit_volatility = mean(abs(m$f_growth - 1)),
          heldout_ht2_volatility = mean(abs(m$h_growth - 1)),
          n_cells = nrow(m))
      }
    }
    message(sprintf('  %-5s %d  targeted %.1f%% / %.2f%%   held-out MARD %.2f%%',
                    arm, year, 100 * mean(te <= 0.02), 100 * mean(te), 100 * mean(ho$err)))
  }
  prev <- cur
  rm(d, cells); invisible(gc())
}

metrics   <- rbindlist(metrics)
by_series <- rbindlist(by_series)
yoy       <- rbindlist(yoy)
fwrite(metrics,   file.path(OUT, paste0('phase3', TAG, '_metrics.csv')))
fwrite(by_series, file.path(OUT, paste0('phase3', TAG, '_heldout_by_series.csv')))
fwrite(yoy,       file.path(OUT, paste0('phase3', TAG, '_yoy.csv')))

cat('\n=== per arm, 2018-2023 means (years every arm fits itself) ===\n')
print(metrics[year >= 2018, lapply(.SD, mean),
              .SDcols = c('targeted_within2', 'targeted_mard', 'heldout_mard',
                          'degenerate_share', 'kish_min'), by = arm])
cat('\n=== year-over-year, 2018->2023 transitions ===\n')
print(yoy[from >= 2018, lapply(.SD, mean),
          .SDcols = c('churn_tv', 'heldout_share_change_err',
                      'heldout_fit_volatility', 'heldout_ht2_volatility'), by = arm])
message('PHASE3_SCORE_DONE')
