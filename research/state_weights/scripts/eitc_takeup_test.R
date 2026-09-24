#!/usr/bin/env Rscript
# Test: do the joint fit's EITC target residuals correlate with IRS state
# EITC participation rates (TY2022, ACS-linked)? If claims-based targets are
# inconsistent with the eligibility-based x-vector, signed error should be
# NEGATIVELY correlated with take-up (overshoot where take-up is low).
suppressPackageStartupMessages({ library(data.table) })
source('src/nonfilers/state_weights.R')   # scratch_root()
SCRATCH <- file.path(scratch_root(), 'state_weights_tmp')

h5  <- readRDS(file.path(SCRATCH, 'hardened_fit_2022.rds'))
p   <- h5$inputs$filers
rm(h5); invisible(gc())
fit <- readRDS(file.path(SCRATCH, 'gradient_fit_2022.rds'))

# IRS EITC participation rates by state, TY2022 (ACS-Census linkage;
# irs.gov/tax-professionals/eitc-central/eitc-participation-rate-by-state)
pr <- data.table(state = names(read_eitc_participation()), takeup = read_eitc_participation())

cat('names(p):', paste(names(p), collapse = ' '), '\n')
state_names <- colnames(fit$P)
if (is.null(state_names)) state_names <- colnames(p$P0)
if (is.null(state_names)) state_names <- p$jurisdictions
if (is.null(state_names)) {
  # engine order of record: sort(unique(ht2$state)) = alphabetical incl OA/PR
  state_names <- sort(c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA',
    'HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO',
    'MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
    'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY','OA','PR'))
}
stopifnot(length(state_names) == ncol(fit$P))

errs <- rbindlist(lapply(p$targets, function(t) data.table(
  series = t$series,
  state  = if (is.character(t$state)) t$state else state_names[t$state],
  target = t$target,
  that   = sum(p$w[t$rows] * fit$P[t$rows, t$state] * t$x)
)))
errs[, sgn_err := that / target - 1]

for (ser in c('eitc_amt', 'n_eitc', 'wages_amt', 'n_returns')) {
  e <- errs[series == ser]
  # state-level: target-weighted mean signed error across stubs
  st <- e[, .(sgn_err = weighted.mean(sgn_err, abs(target)),
              target = sum(abs(target))), by = state]
  st <- merge(st, pr, by = 'state')
  ct <- cor.test(st$sgn_err, st$takeup)
  ctw <- cov.wt(cbind(st$sgn_err, st$takeup), wt = st$target, cor = TRUE)$cor[1,2]
  cat(sprintf('%-10s cor(signed err, takeup) = %+.3f (p = %.4f) | target-weighted cor = %+.3f\n',
              ser, ct$estimate, ct$p.value, ctw))
  if (ser == 'eitc_amt') {
    cat('\n  eitc_amt: 8 most-negative-takeup-deviation states vs signed error:\n')
    print(st[order(takeup)][1:8, .(state, takeup, sgn_err = round(sgn_err, 4))])
    cat('\n  8 highest:\n')
    print(st[order(-takeup)][1:8, .(state, takeup, sgn_err = round(sgn_err, 4))])
    # regression: how much of the state-level variance does takeup explain?
    fit_lm <- lm(sgn_err ~ I(takeup/100), data = st, weights = st$target)
    cat(sprintf('\n  WLS slope on takeup share: %+.3f (i.e. 1pp takeup -> %+.3fpp signed err); R2 = %.3f\n',
                coef(fit_lm)[2], coef(fit_lm)[2]/100*100, summary(fit_lm)$r.squared))
  }
}

# Counterfactual check: re-share the eitc targets by claims/takeup and ask how
# far the CURRENT fit is from those eligibility-based targets (no refit)
e <- errs[series %in% c('eitc_amt', 'n_eitc')]
e <- merge(e, pr, by = 'state')
e[, elig_share_adj := (target / takeup)]
e[, adj_target := elig_share_adj / sum(elig_share_adj) * sum(target), by = series]
e[, adj_err := abs(that / adj_target - 1)]
cat('\nCurrent fit vs CLAIMS targets vs TAKE-UP-ADJUSTED targets (no refit):\n')
print(e[, .(within2_claims = round(100*mean(abs(sgn_err) <= .02), 1),
            mard_claims    = round(100*mean(abs(sgn_err)), 2),
            within2_adj    = round(100*mean(adj_err <= .02), 1),
            mard_adj       = round(100*mean(adj_err), 2)), by = series])
cat('DONE\n')
