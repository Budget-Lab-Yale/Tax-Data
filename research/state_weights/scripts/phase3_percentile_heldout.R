#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/phase3_percentile_heldout.R
#
# Phase 4 step P5b (notes/phase4_state_weights_interface.md 2.3): a held-out
# test of the Phase 3 arms against SOI's "AGI Percentiles by State" -- the
# one published source on the top of each STATE's own distribution, which
# HT2's fixed national stubs cannot see and the fit never targeted.
#
# For each arm, year and state s, and each of the IRS's top groups g (1, 5,
# 10, 25, 50, 75 percent of the state's returns) with its published AGI
# cutoff c_{s,g}: the fitted mass above the cutoff,
#     sum_i W[i,s] * x_i * 1[agi_i >= c_{s,g}],
# against the IRS figure, for returns, AGI and the eight income items (count
# and amount). No within-state ranking is needed -- the cutoff is given -- so
# this is linear in the split. Also the IMPLIED cutoff: the AGI at which the
# fitted state's cumulative weight reaches g% of its total, against the IRS's.
#
# Universe as the source tabulates it: filers, non-dependent returns, AGI > 0
# (the baseline's calculated AGI). Items map: sal = wages; int = txbl_int;
# div = div_ord + div_pref; businc = sole_prop (net); cpgain = kg_st + kg_lt
# with the $3,000 loss limit; iradist = txbl_ira_dist; pension =
# txbl_pens_dist; scorp = partnership + S corporation net (as the docguide
# says despite the name); 2018 merges iradist and pension into ira_pension.
#
# Usage (repo root, under sbatch; reads {TAXDATA_SCRATCH}/phase3/):
#   Rscript research/state_weights/scripts/phase3_percentile_heldout.R [arm ...]
# Writes research/state_weights/scripts/results/phase3_percentile_heldout.csv
# (summary per arm x year x group x series x measure) and
# phase3_percentile_heldout_by_state.csv for DETAIL_ARMS.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table) })
source('src/nonfilers/state_weights.R')

YEARS <- 2017:2023
args  <- commandArgs(trailingOnly = TRUE)
ARMS  <- if (length(args)) args else c('A', 'B', 'C_1', 'C_10', 'C_100', 'C_10_warmB')
DETAIL_ARMS <- c('B', 'C_10_warmB')
IN    <- file.path(scratch_root(), 'state_weights_tmp')
FITS  <- file.path(scratch_root(), 'phase3')
OUT   <- 'research/state_weights/scripts/results'
KG_LOSS_LIMIT <- 3000

fit_path <- function(arm, year) {
  a <- if (startsWith(arm, 'C_') && year < 2018) 'A' else arm
  file.path(FITS, a, sprintf('fit_%d.rds', year))
}

# The filer partition's records with the item columns, id-sorted as
# prep_fit_inputs.R built the partition (the same recovery phase3_score.R uses)
load_year <- function(year) {
  h <- readRDS(file.path(IN, sprintf('hardened_fit_%d.rds', year)))
  tu <- fread(file.path(model_data_root(), 'Tax-Data/v1', h$tax_data_vintage, 'baseline',
                        sprintf('tax_units_%d.csv', year)),
              select = c('id', 'filer', 'weight', 'dep_status', 'wages', 'txbl_int', 'div_ord', 'div_pref',
                         'sole_prop', 'kg_st', 'kg_lt', 'txbl_ira_dist', 'txbl_pens_dist',
                         'part_active', 'part_passive', 'part_active_loss', 'part_passive_loss', 'part_179',
                         'scorp_active', 'scorp_passive', 'scorp_active_loss', 'scorp_passive_loss', 'scorp_179'))
  det <- fread(file.path(h$detail_dir, sprintf('%d.csv', year)), select = c('id', 'agi'))
  n <- nrow(tu)
  tu <- merge(tu, det, by = 'id')
  stopifnot(nrow(tu) == n, !anyNA(tu))
  p <- h$inputs$filers
  tu_f <- tu[p$idx]
  stopifnot(identical(tu_f$weight, p$w), all(tu_f$filer == 1))
  # the source's items, on the file's columns
  tu_f[, `:=`(
    sal     = wages,
    int     = txbl_int,
    div     = div_ord + div_pref,
    businc  = sole_prop,
    cpgain  = pmax(kg_st + kg_lt, -KG_LOSS_LIMIT),
    iradist = txbl_ira_dist,
    pension = txbl_pens_dist,
    scorp   = part_active + part_passive - part_active_loss - part_passive_loss - part_179 +
              scorp_active + scorp_passive - scorp_active_loss - scorp_passive_loss - scorp_179)]
  tu_f[, ira_pension := iradist + pension]
  list(p = p, jur = h$inputs$jurisdictions, tu_f = tu_f)
}

ITEMS <- c('sal', 'int', 'div', 'businc', 'cpgain', 'iradist', 'pension', 'scorp', 'ira_pension')

# Fitted mass above each (state, group) cutoff, all series at once. Records
# are sorted by AGI descending once; every series is a cumulative sum in that
# order, read off at the cutoff's position.
fitted_above_cutoffs <- function(W, tu_f, jur, irs) {
  o   <- order(-tu_f$agi)
  agi <- tu_f$agi[o]
  n_above <- function(cut) sum(agi >= cut)          # position of the last record at or above the cutoff
  series_x <- c(list(returns = rep(1, nrow(tu_f)), agi = tu_f$agi),
                setNames(lapply(ITEMS, function(it) tu_f[[it]]), paste0(ITEMS, '_amount')),
                setNames(lapply(ITEMS, function(it) as.numeric(tu_f[[it]] != 0)), paste0(ITEMS, '_count')))
  out <- list()
  groups <- irs[top_pct < 100, unique(.SD), .SDcols = c('state', 'top_pct', 'cutoff_agi')]
  for (s in intersect(unique(groups$state), jur)) {
    ws <- W[o, match(s, jur)]
    cums <- lapply(series_x, function(x) cumsum(ws * x[o]))
    total_s <- sum(ws)
    for (k in which(groups$state == s)) {
      g <- groups[k]
      pos <- n_above(g$cutoff_agi)
      vals <- vapply(cums, function(cs) if (pos == 0) 0 else cs[pos], numeric(1))
      # the implied cutoff: AGI where the state's cumulative fitted weight reaches g%
      pos_implied <- which(cums$returns >= total_s * g$top_pct / 100)[1]
      out[[length(out) + 1]] <- data.table(
        state = s, top_pct = g$top_pct, cutoff_agi = g$cutoff_agi,
        implied_cutoff = if (is.na(pos_implied)) NA_real_ else agi[pos_implied],
        series = c('returns', 'agi', rep(ITEMS, 2)),
        measure = c('count', 'amount', rep('amount', length(ITEMS)), rep('count', length(ITEMS))),
        fitted = unname(vals))
    }
  }
  rbindlist(out)
}

summary_rows <- list(); detail_rows <- list()
for (year in YEARS) {
  d   <- load_year(year)
  irs <- read_state_percentile(state_percentile_path(year), year)
  keep <- d$tu_f$dep_status == 0 & d$tu_f$agi > 0     # the source's universe
  tu_u <- d$tu_f[keep]
  for (arm in ARMS) {
    fp <- fit_path(arm, year)
    if (!file.exists(fp)) { message(sprintf('  %-10s %d  (no fit)', arm, year)); next }
    f <- readRDS(fp)
    stopifnot(identical(f$ids, d$tu_f$id), identical(f$jurisdictions, d$jur))
    W <- (d$p$w * f$P)[keep, , drop = FALSE]
    fit <- fitted_above_cutoffs(W, tu_u, d$jur, irs)
    cmp <- merge(fit, irs[top_pct < 100, .(state, top_pct, series, measure, irs = value)],
                 by = c('state', 'top_pct', 'series', 'measure'))
    cmp <- cmp[irs != 0][, `:=`(arm = arm, year = year, err = abs(fitted / irs - 1),
                                cutoff_ratio = implied_cutoff / cutoff_agi)]
    summary_rows[[length(summary_rows) + 1]] <- cmp[, .(
      n_states = .N, mard = mean(err), within2 = mean(err <= 0.02), within10 = mean(err <= 0.10),
      fitted_over_irs = sum(fitted) / sum(irs),
      cutoff_ratio_median = median(cutoff_ratio)), by = .(arm, year, top_pct, series, measure)]
    if (arm %in% DETAIL_ARMS) detail_rows[[length(detail_rows) + 1]] <- cmp
    top1 <- cmp[top_pct == 1 & measure %in% c('count', 'amount') & series %in% c('returns', 'agi')]
    message(sprintf('  %-10s %d  top 1%%: returns MARD %.1f%%, AGI MARD %.1f%% (fitted/IRS %.3f), implied cutoff / IRS median %.3f',
                    arm, year, 100 * top1[series == 'returns', mean(err)], 100 * top1[series == 'agi', mean(err)],
                    top1[series == 'agi', sum(fitted) / sum(irs)], median(cmp[top_pct == 1, cutoff_ratio])))
  }
  rm(d, tu_u); invisible(gc())
}

summary <- rbindlist(summary_rows); detail <- rbindlist(detail_rows)
fwrite(summary, file.path(OUT, 'phase3_percentile_heldout.csv'))
fwrite(detail,  file.path(OUT, 'phase3_percentile_heldout_by_state.csv'))

options(width = 200)
cat('\n=== top 1% and 5%: returns and AGI, MARD over states, 2018-2023 means by arm ===\n')
print(dcast(summary[year >= 2018 & series %in% c('returns', 'agi') & top_pct %in% c(1, 5),
                    .(mard = round(100 * mean(mard), 1)), by = .(arm, top_pct, series)],
            arm ~ top_pct + series, value.var = 'mard'))
cat('\n=== fitted / IRS national ratio by group, C_10_warmB, 2018-2023 means (AGI) ===\n')
print(summary[arm == 'C_10_warmB' & year >= 2018 & series == 'agi', .(fitted_over_irs = round(mean(fitted_over_irs), 3),
                                                                       cutoff_ratio = round(mean(cutoff_ratio_median), 3)), by = top_pct])
cat('\n=== items at the top 1%, C_10_warmB, 2019-2023 means (amount MARD, %) ===\n')
print(summary[arm == 'C_10_warmB' & year >= 2019 & top_pct == 1 & measure == 'amount' & series %in% ITEMS,
              .(mard = round(100 * mean(mard), 1), fitted_over_irs = round(mean(fitted_over_irs), 3)), by = series])
message('PHASE3_PERCENTILE_DONE')
