#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 04_refit_score.R  (Phase 3b step 3b, S40) -- scores the filer refit bake-off
#
# Arms, per year (TY2017, TY2023):
#   current  the vintage's weights (the 2017 LP, then status x age scaling)
#   prior    the weights before the 2017 LP; in TY2023 times the status x age
#            factors -- what the refit starts from
#   cal1, cal100, lp   03_refit_fit.R's fits
# Metrics (research/state_weights/notes/filer_refit_design.md, F-A):
#   targeted   per source: share of targets within band, MARD
#   held-out   Table 1.4 returns by AGI class for untargeted items (interest,
#              dividends, business income, pensions, Social Security,
#              unemployment, IRA distributions): MARD over class x item cells
#              of at least HELDOUT_MIN returns
#   quality    Kish effective sample size of the refit records, ratio to prior
#              (quantiles, share at a bound), lumpiness: mean |log| step in
#              weight per record between adjacent $1k AGI bins, $1-100k
#   low income wage filers and dependent wage filers at AGI $1-10k against
#              Tables 1.4 / 1.7 (the Affordability-Index note's headline)
# In TY2023 incomes are the vintage's: the refit's effect on the extensive
# income factors (F-C) is not fed back here.
#
# Writes results/refit_bakeoff_metrics.csv (year, arm, metric, value) and
# results/refit_bakeoff_heldout.csv.
#   Rscript research/state_weights/filer_refit/04_refit_score.R   (under sbatch)
#------------------------------------------------------------------------------

source('research/state_weights/filer_refit/refit_common.R')

YEARS <- c(2017L, 2023L)
FIT_ARMS <- c('cal1', 'cal100', 'lp')
RES <- 'research/state_weights/filer_refit/results'
HELDOUT_MIN <- 50000
LUMP_BINS   <- 1:99       # $1k AGI bins, $1k-$100k
HELDOUT <- list(
  'taxable interest'                                              = function(d) d$txbl_int > 0,
  'ordinary dividends'                                            = function(d) d$div_ord + d$div_pref > 0,
  'business or profession > net income'                           = function(d) d$sole_prop > 0,
  'pensions and annuities > total'                                = function(d) d$gross_pens_dist > 0,
  'social security benefits > total'                              = function(d) d$gross_ss > 0,
  'unemployment compensation'                                     = function(d) d$ui > 0,
  'taxable individual retirement arrangement (ira) distributions' = function(d) d$txbl_ira_dist > 0)

soi <- fread(irs_ind_aligned_path('panel_income_sources.csv'), na.strings = '')[
  panel == 'all returns' & measure == 'returns' & series %in% names(HELDOUT) &
    !is.na(agi_lo) & !(agi_change %in% 'expand_component')]
targets_all <- fread(TARGETS)

metrics <- list(); heldout <- list()
put <- function(y, arm, metric, value) metrics[[length(metrics) + 1]] <<- data.table(year = y, arm, metric, value)

for (y in YEARS) {
  d  <- load_filers(y)
  free <- which(!d$fixed)
  tm <- target_matrix(d, targets_all[year == y])
  bnd <- band(tm$target$t)

  pre <- d$w_current
  pre17 <- NULL
  if (y == 2017L) {
    pre[free] <- prior_2017(d)
  } else {
    d17 <- load_filers(2017L)
    p17 <- setNames(prior_2017(d17), d17$id)
    pre[free] <- p17[as.character(d$id[free])] * status_age_factor(d$id[free], y)
  }
  arms <- list(current = d$w_current, prior = pre)
  for (a in FIT_ARMS) {
    f <- file.path(fit_dir(), sprintf('%s_%d.rds', a, y))
    if (!file.exists(f)) { cat('missing fit:', f, '\n'); next }
    fit <- readRDS(f)
    stopifnot(identical(fit$id, d$id))
    arms[[a]] <- fit$w
    arms[[paste0(a, '_prior')]] <- fit$prior      # each arm's own starting point
  }

  for (a in names(arms)) {
    if (grepl('_prior$', a)) next
    w <- arms[[a]]
    fitted <- as.numeric(crossprod(tm$X, w))
    # an LP solution sits exactly on its band edge: allow float error there
    miss <- abs(fitted - tm$target$t) / (1 + 1e-6)
    for (s in unique(tm$target$source)) {
      k <- tm$target$source == s
      put(y, a, sprintf('within_band:%s', s), mean(miss[k] <= bnd[k]))
      k <- k & tm$target$t > 0                       # a zero target has no relative miss
      put(y, a, sprintf('mard:%s', s), mean(miss[k] / tm$target$t[k]))
    }
    put(y, a, 'within_band:all', mean(miss <= bnd))

    # held-out Table 1.4 counts
    cls <- findInterval(d$agi_proxy, sort(unique(soi$agi_lo)))
    lo  <- sort(unique(soi$agi_lo))[cls]
    ho <- rbindlist(lapply(names(HELDOUT), function(s) {
      has <- HELDOUT[[s]](d)
      data.table(series = s, agi_lo = lo, w = w * has)[, .(file = sum(w)), by = .(series, agi_lo)]
    }))
    ho <- merge(ho, soi[tax_year == y, .(series, agi_lo, soi = value)], by = c('series', 'agi_lo'))
    ho[, `:=`(year = y, arm = a)]
    heldout[[length(heldout) + 1]] <- ho
    put(y, a, 'heldout_mard', ho[soi >= HELDOUT_MIN, mean(abs(file - soi) / soi)])

    # weight quality over the refit records
    wf <- w[free]; r <- wf / pre[free]
    put(y, a, 'kish_ess', sum(wf)^2 / sum(wf^2))
    for (q in c(0.01, 0.5, 0.99)) put(y, a, sprintf('ratio_to_prior_q%02d', 100 * q), quantile(r, q))
    put(y, a, 'share_at_ratio_bound', mean(r <= RATIO_MIN + 1e-6 | r >= RATIO_MAX - 1e-6))
    put(y, a, 'share_unchanged', mean(abs(r - 1) < 1e-9))
    bin <- floor(d$agi_proxy[free] / 1000)
    wpr <- data.table(bin, wf)[bin %in% LUMP_BINS, .(wpr = mean(wf)), by = bin][order(bin)]
    put(y, a, 'lumpiness', mean(abs(diff(log(wpr$wpr)))))

    # the low-income headline
    low <- d$agi_proxy >= 1 & d$agi_proxy < 1e4 & d$wages > 0
    put(y, a, 'wage_filers_1_10k_M', sum(w[low]) / 1e6)
    put(y, a, 'dep_wage_filers_1_10k_M', sum(w[low & d$dep_status == 1]) / 1e6)
  }
  put(y, 'soi', 'wage_filers_1_10k_M',
      targets_all[year == y & source == 't14_wages' & agi_lo >= 1 & agi_hi <= 1e4, sum(target_value)] / 1e6)
  put(y, 'soi', 'dep_wage_filers_1_10k_M',
      targets_all[year == y & source == 't17_wages' & agi_lo >= 1 & agi_hi <= 1e4, sum(target_value)] / 1e6)
}

metrics <- rbindlist(metrics); heldout <- rbindlist(heldout)
dir.create(RES, showWarnings = FALSE, recursive = TRUE)
fwrite(metrics, file.path(RES, 'refit_bakeoff_metrics.csv'))
fwrite(heldout, file.path(RES, 'refit_bakeoff_heldout.csv'))
print(dcast(metrics, metric ~ year + arm, value.var = 'value'), digits = 3)
cat('REFIT_SCORE_SUCCESS\n')
