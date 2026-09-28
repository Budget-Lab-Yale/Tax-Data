#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 05_vintage_diagnostics.R  (Phase 3b step 4, S40) -- before / after the refit
#
# Compares two Tax-Data vintages built from the same code and model caches but
# for the S40 filer refit: BEFORE 2026092220_s33_scratch, AFTER
# 2026092712_filer_refit (~/td_filer_refit.sbatch: its caches copied from the
# BEFORE build and loaded, TAXDATA_ESTIMATE_MODELS=0).
#
#   classes   filer returns (all, with wages) by AGI class, src/agi_proxy.R,
#             against Pub 1304 Table 1.4, TY2017 / 2019 / 2023
#   amounts   income amounts by AGI class against Table 1.4, TY2017 (the 2017
#             value rescaling was computed on the LP's weights)
#   headline  the low-income table of notes/low_income_filer_aging.md
#   tripwire  per column, the share of filer records whose value changed,
#             TY2017 and TY2023 (only weight-dependent columns should move)
#   totals    weighted filers and non-filers by year, 2017-2035
#
# Writes results/vintage_diag_{classes,amounts,headline,tripwire,totals}.csv.
#   sbatch --job-name=refit_vintage_diag --mem=48G --time=02:00:00 \
#     --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_rscript.sbatch research/state_weights/filer_refit/05_vintage_diagnostics.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(yaml) })
source('src/nonfilers/state_weights.R')
source('src/agi_proxy.R')

BEFORE <- '2026092220_s33_scratch'
AFTER  <- Sys.getenv('REFIT_DIAG_AFTER', unset = '2026092712_filer_refit')   # the vintage under review
RES    <- file.path('research/state_weights/filer_refit/results',
                  if (AFTER == '2026092712_filer_refit') '' else AFTER)
YEARS_CLASSES <- c(2017L, 2019L, 2023L)
YEARS_TOTALS  <- 2017:2035
# Table 1.4 amount series and the file columns that carry them (TY2017)
AMOUNTS <- list(
  'wages'                               = function(d) d$wages,
  'taxable interest'                    = function(d) d$txbl_int,
  'ordinary dividends'                  = function(d) d$div_ord + d$div_pref,
  'business or profession > net income' = function(d) pmax(d$sole_prop, 0),
  'pensions and annuities > taxable'    = function(d) d$txbl_pens_dist,
  'social security benefits > total'    = function(d) d$gross_ss)

tu_path <- function(v, y) file.path(model_data_root(), 'Tax-Data/v1', v, 'baseline',
                                    sprintf('tax_units_%d.csv', y))
read_year <- function(v, y) fread(tu_path(v, y))

soi <- fread(irs_ind_aligned_path('panel_income_sources.csv'), na.strings = '')[
  panel == 'all returns' & !is.na(agi_lo) & !(agi_change %in% 'expand_component')]
breaks <- sort(unique(soi$agi_lo))
cls <- function(x) breaks[findInterval(x, breaks)]

classes <- list(); amounts <- list(); headline <- list(); tripwire <- list()
for (y in YEARS_CLASSES) {
  both <- lapply(c(before = BEFORE, after = AFTER), function(v) {
    d <- read_year(v, y)[filer == 1 & weight > 0]
    as.data.table(agi_proxy(as_tibble(d), y))
  })
  for (k in names(both)) {
    d <- both[[k]]
    d[, agi_lo := cls(agi_proxy)]
    classes[[length(classes) + 1]] <- d[, .(returns = sum(weight), wage_returns = sum(weight * (wages > 0))),
                                        by = agi_lo][, `:=`(year = y, vintage = k)]
    low <- d[agi_proxy >= 1 & agi_proxy < 1e4 & wages > 0]
    headline[[length(headline) + 1]] <- data.table(year = y, vintage = k,
      wage_filers_1_10k = sum(low$weight), dep_wage_filers_1_10k = low[dep_status == 1, sum(weight)],
      wage_filers_18_25_1_10k = low[age1 >= 18 & age1 < 26, sum(weight)],
      filers = sum(d$weight))
    if (y == 2017L) for (s in names(AMOUNTS)) {
      amounts[[length(amounts) + 1]] <- data.table(agi_lo = d$agi_lo, v = d$weight * AMOUNTS[[s]](d))[
        , .(amount = sum(v) / 1e3), by = agi_lo][, `:=`(series = s, vintage = k)]   # $ thousands, as SOI
    }
  }
  # tripwire: same records, column by column
  a <- both$before; b <- both$after
  stopifnot(identical(sort(a$id), sort(b$id)))
  setkey(a, id); setkey(b, id)
  num <- intersect(names(a)[vapply(a, is.numeric, logical(1))], names(b))
  tripwire[[length(tripwire) + 1]] <- rbindlist(lapply(num, function(v) data.table(
    year = y, column = v,
    share_changed = mean(!(abs(a[[v]] - b[[v]]) <= 1e-9 * pmax(1, abs(a[[v]])) |
                           (is.na(a[[v]]) & is.na(b[[v]])))))))
}
classes <- rbindlist(classes)
soi_counts <- merge(soi[series == 'all' & measure == 'returns', .(year = tax_year, agi_lo, soi_returns = value)],
                    soi[series == 'wages' & measure == 'returns', .(year = tax_year, agi_lo, soi_wage_returns = value)],
                    by = c('year', 'agi_lo'))
classes <- merge(dcast(classes, year + agi_lo ~ vintage, value.var = c('returns', 'wage_returns')),
                 soi_counts, by = c('year', 'agi_lo'), all.x = TRUE)
amounts <- merge(dcast(rbindlist(amounts), series + agi_lo ~ vintage, value.var = 'amount'),
                 soi[tax_year == 2017L & measure == 'amount' & series %in% names(AMOUNTS),
                     .(series, agi_lo, soi = value)], by = c('series', 'agi_lo'), all.x = TRUE)

totals <- rbindlist(lapply(YEARS_TOTALS, function(y) rbindlist(lapply(c(before = BEFORE, after = AFTER), function(v) {
  d <- fread(tu_path(v, y), select = c('weight', 'filer'))
  data.table(year = y, filers = d[filer == 1, sum(weight)], nonfilers = d[filer == 0, sum(weight)])
}), idcol = 'vintage')))

dir.create(RES, showWarnings = FALSE, recursive = TRUE)
fwrite(classes, file.path(RES, 'vintage_diag_classes.csv'))
fwrite(amounts, file.path(RES, 'vintage_diag_amounts.csv'))
fwrite(rbindlist(headline), file.path(RES, 'vintage_diag_headline.csv'))
fwrite(rbindlist(tripwire), file.path(RES, 'vintage_diag_tripwire.csv'))
fwrite(totals, file.path(RES, 'vintage_diag_totals.csv'))

options(width = 200)
print(rbindlist(headline), digits = 4)
print(classes[agi_lo < 1e5], digits = 3)
print(amounts[, .(before_err = sum(abs(before - soi)) / sum(soi), after_err = sum(abs(after - soi)) / sum(soi)),
              by = series], digits = 3)
print(rbindlist(tripwire)[share_changed > 0][order(year, -share_changed)], nrows = 200, digits = 3)
print(dcast(totals, year ~ vintage, value.var = c('filers', 'nonfilers')), digits = 4)
cat('VINTAGE_DIAG_SUCCESS\n')
