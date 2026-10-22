#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 10_review_fixes_diff.R -- before / after the 2026-10-01 code-review fixes
#
# BEFORE 2026092720_filer_refit_v2, AFTER 2026100117_review_fixes
# (~/td_review_fixes.sbatch): same model caches, loaded
# (TAXDATA_ESTIMATE_MODELS=0), so the fixes are the change. Expected movers:
#   - TY2017 stage-2 refit now runs before tips / QBI / overtime / consumption,
#     so the filer weights should be unchanged and those columns move;
#   - TY2021 classing drops the below-the-line charitable deduction, so 2021
#     filer weights move (and everything aged from them).
# Anything else that moves is unexplained.
#
#   tripwire    per column and year: share of records changed, weighted-total
#               change (filers and non-filers, ids matched)
#   benchmarks  TY2017 tipped earners and average tips against the IRS W-2
#               figures in tips.R (by marital status: the scaling factor is
#               set per group, so both groups' realized/target ratios should
#               equal the one covid_factor); QBI weighted wage bill by form
#               against resources/qbi.csv; overtime receipt share
#   classes     filer returns by AGI class (agi_proxy, current code) against
#               Pub 1304 Table 1.4, TY2017 / 2021 / 2023
#
#   sbatch --job-name=review_fixes_diff --mem=64G --time=02:00:00 \
#     --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_rscript.sbatch research/state_weights/filer_refit/10_review_fixes_diff.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(yaml); library(readr) })
source('src/nonfilers/state_weights.R')
source('src/agi_proxy.R')

BEFORE <- '2026092720_filer_refit_v2'
AFTER  <- '2026100117_review_fixes'
RES    <- file.path('research/state_weights/filer_refit/results', AFTER)
YEARS_TRIPWIRE <- c(2017L, 2020L, 2021L, 2022L, 2023L, 2030L)
YEARS_CLASSES  <- c(2017L, 2021L, 2023L)
# IRS W-2 tipped-earner benchmarks, as set in src/imputations/tips.R
TIP_TARGETS <- data.table(married = c(TRUE, FALSE), n_target = c(1164973, 4800747),
                          avg_target = c(7072, 5686))
QBI_FORMS <- c('sole_prop', 'part', 'scorp')

tu_path <- function(v, y) file.path(model_data_root(), 'Tax-Data/v1', v, 'baseline',
                                    sprintf('tax_units_%d.csv', y))
both_year <- function(y) lapply(c(before = BEFORE, after = AFTER), function(v) fread(tu_path(v, y)))

# --- tripwire ----------------------------------------------------------------
tripwire <- rbindlist(lapply(YEARS_TRIPWIRE, function(y) {
  b <- both_year(y)
  a <- b$before; z <- b$after
  stopifnot(setequal(a$id, z$id), !anyDuplicated(a$id))
  setkey(a, id); setkey(z, id)
  num <- intersect(names(a)[vapply(a, is.numeric, logical(1))], names(z))
  rbindlist(lapply(num, function(v) {
    same <- (abs(a[[v]] - z[[v]]) <= 1e-9 * pmax(1, abs(a[[v]]))) | (is.na(a[[v]]) & is.na(z[[v]]))
    same[is.na(same)] <- FALSE                       # NA on one side only: changed
    tot <- function(d) if (v == 'weight') sum(d$weight) else sum(d$weight * d[[v]])
    data.table(year = y, column = v,
               share_changed_filers    = mean(!same[a$filer == 1]),
               share_changed_nonfilers = mean(!same[a$filer == 0]),
               wtotal_before = tot(a), wtotal_after = tot(z))
  }))
}))
tripwire[, wtotal_rel := wtotal_after / wtotal_before - 1]

# --- TY2017 benchmarks ---------------------------------------------------------
b17 <- both_year(2017L)
tips_bench <- rbindlist(lapply(names(b17), function(k) {
  d <- b17[[k]][filer == 1 | filer == 0]
  d[, married := filing_status == 2]
  d[, .(n_tipped = sum(weight * ((tips1 > 0) + (tips2 > 0))),
        tips_total = sum(weight * tips)), by = married][, vintage := k]
}))
tips_bench <- merge(tips_bench, TIP_TARGETS, by = 'married')
tips_bench[, `:=`(n_ratio = n_tipped / n_target, avg = tips_total / n_tipped)]
tips_bench[, avg_ratio := avg / avg_target]

qbi_target <- read_csv('./resources/qbi.csv', show_col_types = FALSE) %>%
  group_by(form) %>% summarise(target = sum(wages) * 1000, .groups = 'drop') %>% as.data.table()
qbi_bench <- rbindlist(lapply(names(b17), function(k) {
  d <- b17[[k]]
  data.table(vintage = k, form = QBI_FORMS,
             wagebill = vapply(QBI_FORMS, function(f) {
               x <- d[[paste0('wagebill_', f)]]
               sum(d$weight[!is.na(x)] * x[!is.na(x)])        # NA = no net income from the form (qbi.R)
             }, numeric(1)))
}))
qbi_bench <- merge(qbi_bench, qbi_target, by = 'form', all.x = TRUE)[, ratio := wagebill / target]

ot_bench <- rbindlist(lapply(names(b17), function(k) {
  d <- b17[[k]]
  data.table(vintage = k,
             earners = sum(d$weight * ((d$wages1 > 0) + (d$wages2 > 0))),
             ot_receivers = sum(d$weight * ((d$ot1 > 0) + (d$ot2 > 0))),
             ot_total = sum(d$weight * d$ot))
}))[, share := ot_receivers / earners]
rm(b17)

# --- classes against Table 1.4 ---------------------------------------------------
soi <- fread(irs_ind_aligned_path('panel_income_sources.csv'), na.strings = '')[
  panel == 'all returns' & !is.na(agi_lo) & !(agi_change %in% 'expand_component')]
breaks <- sort(unique(soi$agi_lo))
cls <- function(x) breaks[findInterval(x, breaks)]
classes <- rbindlist(lapply(YEARS_CLASSES, function(y) rbindlist(lapply(c(before = BEFORE, after = AFTER), function(v) {
  d <- fread(tu_path(v, y))[filer == 1]
  d <- as.data.table(agi_proxy(as_tibble(d), y))
  d[, .(returns = sum(weight)), by = .(agi_lo = cls(agi_proxy))][, year := y]
}), idcol = 'vintage')))
classes <- merge(dcast(classes, year + agi_lo ~ vintage, value.var = 'returns'),
                 soi[series == 'all' & measure == 'returns', .(year = tax_year, agi_lo, soi = value)],
                 by = c('year', 'agi_lo'), all.x = TRUE)
classes[, `:=`(before_rel = before / soi - 1, after_rel = after / soi - 1)]

dir.create(RES, showWarnings = FALSE, recursive = TRUE)
fwrite(tripwire,   file.path(RES, 'review_diff_tripwire.csv'))
fwrite(tips_bench, file.path(RES, 'review_diff_tips.csv'))
fwrite(qbi_bench,  file.path(RES, 'review_diff_qbi.csv'))
fwrite(ot_bench,   file.path(RES, 'review_diff_overtime.csv'))
fwrite(classes,    file.path(RES, 'review_diff_classes.csv'))

options(width = 220)
cat('\n== tripwire: columns that moved ==\n')
print(tripwire[share_changed_filers > 0 | share_changed_nonfilers > 0][order(year, -share_changed_filers)],
      nrows = 500, digits = 4)
cat('\n== TY2017 tips vs IRS W-2 (ratios by group should match each other) ==\n'); print(tips_bench, digits = 5)
cat('\n== TY2017 QBI wage bill vs resources/qbi.csv ==\n');                         print(qbi_bench, digits = 5)
cat('\n== TY2017 overtime ==\n');                                                    print(ot_bench, digits = 5)
cat('\n== filer returns by AGI class vs Table 1.4 ==\n')
print(classes[, .(before_mard = sum(abs(before - soi)) / sum(soi), after_mard = sum(abs(after - soi)) / sum(soi),
                  max_abs_before = max(abs(before_rel)), max_abs_after = max(abs(after_rel))), by = year], digits = 4)
print(classes, nrows = 200, digits = 4)
cat('REVIEW_DIFF_SUCCESS\n')
