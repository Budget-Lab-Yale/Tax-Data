#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/nonfiler_anchor_bakeoff.R
#
# Phase 2 (plan.md §5a) harness for the NON-FILER half of the state-weight
# fit: the F1b + F2 bake-off that chose its targets (S37), and the F5
# validation additions scored on the same footing. F3's firm OASDI 65+
# non-filer target was built and REJECTED (S38): the filer fit misplaces 65+
# beneficiaries by more than the non-filers hold, so the target goes negative
# in four states. OASDI 65+ stays here as a held-out metric.
#
#   ref   production today: ACS state x age-band x income-tier cells are both
#         the prior and the targets, so the fit reproduces its prior
#   A     F2 + F1b by SHARES: the ACS cells are the prior only; one target per
#         state, the national non-filer adult total split by each state's
#         share of the dorm-netted residual anchor (Census PEP adults - IRS
#         filing adults - ACS dorm students)
#   B     F2 + F1b by NET LEVELS: as A, but the anchor first loses each
#         state's claimed adult dependents who do not file, estimated from the
#         CPS ASEC the non-filer pool is built from. The residual counts them;
#         the pool does not contain them (they are dependents on filers'
#         returns), so A assumes they are spread in proportion to the residual.
#
# A and B call the production target builder (sw_fit.R), so the harness
# scores what production fits. B's netting is placed where the ASEC places people, i.e. dorm students at
# their parents' address, while PEP (and so the anchor) counts them at school.
# The anchor's own dorm netting already removes them at school, so B adds back
# the ASEC's dorm-student share at the parents' state: the national ACS dorm
# count, allocated by the ASEC's non-filing full-time students 18-24 claimed as
# dependents.
#
# All three arms fit by exact IPF (the production non-filer engine) against
# the same filer fit (config 7), so every difference below is the non-filer
# target set. The held-out metrics are targeted by no arm:
#   oasdi65   state shares of persons 65+ in Social Security-receiving units
#             (filers + non-filers) against SSA OASDI 65+ beneficiaries
#   age       within-state adult age-band shares (filer adults + non-filer
#             adults + filers' claimed adult dependents) against Census PEP
# F5 adds, per arm:
#   pop identity  filer adults + non-filer adults + the netting (dorm students
#             at school, as PEP counts them) against PEP adults, by state,
#             scored against residual_tolerance_{year}.csv
#   hi2064    state shares of wage earners 20-64 against SSA's HI covered
#             workers 20-64 (EEDATA Table 5, a 1% sample), beside the sampling
#             noise floor -- the evidence for F3's call on that margin (S38)
# plus the distance each arm moves from the ACS cells, weight quality (KL
# from prior, Kish effective sample size), and checks on B's input (the ASEC's
# claimed dependents against HT2's dependent counts, its adult netting against
# the filer fit's own adult dependents) and the EITC take-up correlation of
# the filer fit's residuals (F5; TY2022, the year the IRS rates are for).
#
# Usage (from the repo root; heavy -- ACS read plus a config-7 filer fit):
#   Rscript research/state_weights/scripts/nonfiler_anchor_bakeoff.R \
#     <year> <tax_data_vintage> <baseline_detail_dir>
# Caches inputs and the filer fit under {TAXDATA_SCRATCH}/phase2_bakeoff/.
# Writes research/state_weights/scripts/results/nonfiler_anchor_bakeoff_{
#   metrics,checks,states,bootstrap}_{year}.csv
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(readr); library(stringr)
  library(yaml); library(readxl)
})
source('src/nonfilers/state_weights.R')

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3)
year       <- as.integer(args[1])
td_vintage <- args[2]
detail_dir <- args[3]

TOL  <- 'research/state_weights/nonfiler_residual/results/residual_tolerance_%d.csv'
OUT  <- 'research/state_weights/scripts/results'
CACHE <- file.path(scratch_root(), 'phase2_bakeoff')
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

IPF_N_ITER <- 500L     # state-total rakes converge in tens of passes
IPF_TOL    <- 1e-8
HI_WORKING_BANDS <- c('20–29', '30–39', '40–49', '50–59', '60–61', '62–64')
HI_SAMPLING_RATE <- 0.01   # EEDATA is the 1% Continuous Work History Sample
BOOT_REPS  <- 200L     # household-cluster bootstrap replicates of B's netting
BOOT_SEED  <- 20260923L

# Same columns as prep_fit_inputs.R, plus what the held-out metrics read
INPUT_COLS <- c('id','weight','filer','filing_status','n_dep','age1',
                'wages','txbl_int','exempt_int','div_ord','div_pref',
                'kg_st','kg_lt','salt_inc_sales','salt_prop',
                'first_mort_int','second_mort_int','txbl_pens_dist',
                'txbl_ira_dist','gross_ss','sole_prop')
METRIC_COLS <- c('age2', 'dep_age1', 'dep_age2', 'dep_age3', 'wages1', 'wages2')

mard <- function(got, want) mean(abs(got / want - 1))
shares <- function(x) x / sum(x)

#------------------------------------------------------------------------------
# 1. Records and fit inputs (cached)
#------------------------------------------------------------------------------

td_file <- file.path(model_data_root(), 'Tax-Data/v1', td_vintage, 'baseline',
                     sprintf('tax_units_%d.csv', year))
tu     <- fread(td_file, select = c(INPUT_COLS, METRIC_COLS))
detail <- fread(file.path(detail_dir, sprintf('%d.csv', year)), select = c('id','agi','eitc'))
n_td <- nrow(tu)
tu <- merge(tu, detail, by = 'id', all.x = TRUE)
stopifnot(nrow(tu) == n_td, !anyNA(tu$agi), !anyNA(tu$eitc))

inputs_f <- file.path(CACHE, sprintf('inputs_%d_%s.rds', year, td_vintage))
if (file.exists(inputs_f)) {
  inputs <- readRDS(inputs_f)
} else {
  inputs <- build_weight_inputs(tu[, c(INPUT_COLS, 'agi', 'eitc'), with = FALSE], year)
  saveRDS(inputs, inputs_f)
}
J <- inputs$jurisdictions
stopifnot(length(inputs$filers$w) == sum(tu$filer == 1),
          all.equal(inputs$filers$w, tu$weight[inputs$filers$idx]),
          all.equal(inputs$nonfilers$w, tu$weight[inputs$nonfilers$idx]))
message(sprintf('TY%d, Tax-Data %s: %s filer and %s non-filer records',
                year, td_vintage, format(length(inputs$filers$idx), big.mark = ','),
                format(length(inputs$nonfilers$idx), big.mark = ',')))

#------------------------------------------------------------------------------
# 2. Filer fit, config 7 (cached; identical for every arm)
#------------------------------------------------------------------------------

filer_f <- file.path(CACHE, sprintf('filer_fit_%d_%s.rds', year, td_vintage))
if (file.exists(filer_f)) {
  P_f <- readRDS(filer_f)
} else {
  P_f <- fit_filers_config7(inputs$filers, verbose = TRUE)$P
  saveRDS(P_f, filer_f)
}
stopifnot(all(P_f >= 0), max(abs(rowSums(P_f) - 1)) < 1e-8)

#------------------------------------------------------------------------------
# 3. The ASEC netting: claimed adult dependents who do not file, by state
#------------------------------------------------------------------------------

deps <- asec_adult_dependents(year)        # ties to the pool's national netting
anch <- fread(sprintf(RESIDUAL_ANCHORS_PATH, year))
stopifnot(nrow(anch) == 51, !anyNA(anch$residual_nonfiling_adults_net_dorm))
net_st <- netting_by_state(deps)
anchor_B <- nonfiler_state_anchor(anch, net_st)

# All-age claimed dependents, for the geography check against HT2
persons <- readRDS(file.path(NONFILER_POOL_RESULTS, sprintf('units_%d.rds', year)))$persons
all_deps <- persons[is_dependent == TRUE, .(asec_dependents = sum(ASECWT)),
                    by = .(state = FIPS_TO_STATE[as.character(STATEFIP)])]

sts <- Reduce(function(a, b) merge(a, b, by = 'state', all = TRUE),
              list(anch[, .(state, pep_adults_18p, dorm_dependents,
                            anchor_net_dorm = residual_nonfiling_adults_net_dorm,
                            ht2_dependents = dependents, oasdi_65p = beneficiaries_65p)],
                   net_st, anchor_B[, .(state, dorm_at_parents, anchor_b = anchor)],
                   all_deps))
stopifnot(nrow(sts) == 51, !anyNA(sts))
message(sprintf(paste('ASEC netting %.2fM (ties to the pool), of which %.2fM',
                      'full-time students 18-24; ACS dorm students %.2fM'),
                sum(sts$asec_netting) / 1e6, sum(sts$asec_ft_student) / 1e6,
                sum(sts$dorm_dependents) / 1e6))

#------------------------------------------------------------------------------
# 4. The non-filer arms
#------------------------------------------------------------------------------

p   <- inputs$nonfilers
tu_n <- tu[p$idx]
x_adults <- puf_series_x(tu_n, 'n_adults')
nf_total <- sum(p$w * x_adults)
# Non-filer priors come from the 51-state ACS: no mass in PR or Other Areas,
# which is why 51 state targets can close the partition.
extra_j <- setdiff(J, sts$state)
stopifnot(all(extra_j %in% c('OA', 'PR')), all(p$P0[, J %in% extra_j] == 0))

state_targets <- function(level) {
  nonfiler_state_targets(p$w, x_adults, data.table(state = sts$state, anchor = level), J)
}
# `ref` is production before S37, as cached with the inputs
stopifnot(all(vapply(p$targets, `[[`, character(1), 'series') == 'n_adults'))
arms <- list(ref     = p$targets,
             A       = state_targets(sts$anchor_net_dorm),
             B       = state_targets(sts$anchor_b))

fits <- lapply(names(arms), function(a) {
  f <- fit_calibration(p$w, p$P0, arms[[a]], n_iter = IPF_N_ITER, tol = IPF_TOL)
  stopifnot(all(f$P >= 0), max(abs(rowSums(f$P) - 1)) < 1e-8,
            length(f$unfittable) == 0)
  message(sprintf('  arm %-3s %4d targets: %3d IPF passes, max |rel err| %.1e',
                  a, length(arms[[a]]), f$iters, f$maxrel))
  f
})
names(fits) <- names(arms)

#------------------------------------------------------------------------------
# 5. Metrics
#------------------------------------------------------------------------------

predict_t <- function(P, t) sum(p$w[t$rows] * P[t$rows, t$state] * t$x)

# Persons per record, by what each held-out metric counts. age2 is NA exactly
# when there is no spouse (filing_status != 2); dep_age{k} is NA when the
# record has fewer than k dependents -- both are not-in-universe, asserted.
stopifnot(all(is.na(tu$age2) == (tu$filing_status != 2)))
tu[, n65_ss := fifelse(gross_ss > 0,
                       (age1 >= 65) + fifelse(filing_status == 2, as.numeric(age2 >= 65), 0), 0)]
is_adult_dep <- function(a) !is.na(a) & a >= 18
tu[, n_adult_dep := is_adult_dep(dep_age1) + is_adult_dep(dep_age2) + is_adult_dep(dep_age3)]
stopifnot(all(tu[, n_dep >= rowSums(!is.na(cbind(dep_age1, dep_age2, dep_age3)))]))

by_state <- function(P, idx, x) setNames(colSums(tu$weight[idx] * x * P), J)[sts$state]
filer_oasdi <- by_state(P_f, inputs$filers$idx, tu$n65_ss[inputs$filers$idx])
filer_adult_dep <- by_state(P_f, inputs$filers$idx, tu$n_adult_dep[inputs$filers$idx])
sts[, puf_adult_dependents := filer_adult_dep]
filer_adults_st <- by_state(P_f, inputs$filers$idx,
                            puf_series_x(tu[inputs$filers$idx], 'n_adults'))

# F5 population identity: PEP adults = filing adults + non-filing pool adults
# + the netted dependents, whom PEP counts at school when they are dorm
# students (the ASEC netting counts them at their parents').
tol_f <- sprintf(TOL, year)
tol <- if (file.exists(tol_f)) fread(tol_f)[state != 'US', .(state, tolerance_adults_net)] else NULL
if (is.null(tol)) message('  no residual_tolerance_', year, '.csv: identity scored without tolerance')
netted_at_pep <- sts$asec_netting - sts$dorm_at_parents + sts$dorm_dependents
pop_identity <- function(nf_adults) {
  gap <- filer_adults_st + nf_adults + netted_at_pep - sts$pep_adults_18p
  c(pop_identity_mard = mean(abs(gap) / sts$pep_adults_18p),
    pop_identity_within_tol = if (is.null(tol)) NA_real_ else
      mean(abs(gap) <= tol$tolerance_adults_net[match(sts$state, tol$state)]))
}

# F3 evidence on the HI covered-worker margin: wage earners 20-64 by state,
# counted per person from wages1/wages2 (the head carries Schedule C, which
# Table 5's total covered universe includes), against the SSA shares.
hi <- fread(sprintf('research/state_weights/nonfiler_residual/results/ssa_age_margin_%d.csv', year))
stopifnot(all(HI_WORKING_BANDS %in% hi$ssa_age_band))
hi <- hi[ssa_age_band %in% HI_WORKING_BANDS, .(hi_2064 = sum(hi_persons_total)), by = state]
stopifnot(nrow(hi) == 51)
sts <- merge(sts, hi, by = 'state')
in_2064 <- function(a) !is.na(a) & a >= 20 & a <= 64
tu[, n_earn_2064 := in_2064(age1) * (wages1 > 0 | sole_prop != 0) +
                    in_2064(age2) * (fcoalesce(wages2 > 0, FALSE))]
filer_earn <- by_state(P_f, inputs$filers$idx, tu$n_earn_2064[inputs$filers$idx])
# Expected share MARD from the 1% sample alone: |error| ~ 0.8 x CV per state
hi_noise_mard <- mean(0.8 * sqrt((1 - HI_SAMPLING_RATE) / (HI_SAMPLING_RATE * sts$hi_2064)))
hi_share <- function(nf_earn) mard(shares(filer_earn + nf_earn), shares(sts$hi_2064))

# Within-state adult age shares against PEP (6 anchor bands, 18+). Every
# person slot on a record counts -- head, spouse, up to three dependents -- on
# filer and non-filer records alike. An adult dependent who files their own
# return appears twice (their record, and a slot on the claimer's), which
# inflates 18_25 identically in every arm: a level bias, not a ranking one.
pep <- read_pep(year)[AGE >= 18][, band := as.character(a16_band(AGE))]
pep_sb <- pep[, .(pep = sum(pop)), by = .(state, band)]
adults_long <- function(P, idx) {
  d <- tu[idx]
  slots <- list(d$age1, d$age2, d$dep_age1, d$dep_age2, d$dep_age3)
  rbindlist(lapply(TARGET_AGE_BANDS, function(b) {
    n <- Reduce(`+`, lapply(slots, function(a) !is.na(a) & a >= 18 &
                                                 as.character(a16_band(pmax(a, 18))) == b))
    data.table(state = J, band = b, n = as.vector(crossprod(P, d$weight * n)))
  }))
}
filer_adults <- adults_long(P_f, inputs$filers$idx)

kish <- function(P) {
  W <- p$w * P[, match(sts$state, J)]
  colSums(W)^2 / colSums(W^2)
}
kl <- function(P) {
  pos <- P > 0
  sum((p$w * P * log(P / p$P0))[pos]) / sum(p$w)
}

held_out <- function(P) {
  oasdi <- filer_oasdi + by_state(P, p$idx, tu$n65_ss[p$idx])
  ages <- rbind(filer_adults, adults_long(P, p$idx))[state %in% sts$state,
                                                     .(n = sum(n)), by = .(state, band)]
  ages <- merge(ages, pep_sb, by = c('state', 'band'))
  stopifnot(nrow(ages) == 51 * length(TARGET_AGE_BANDS))
  ages[, `:=`(got = n / sum(n), want = pep / sum(pep)), by = state]
  c(oasdi65_share_mard = mard(shares(oasdi), shares(sts$oasdi_65p)),
    age_within_state_mard = ages[, mard(got, want)])
}

metrics <- rbindlist(lapply(names(fits), function(a) {
  P <- fits[[a]]$P
  nf_adults <- by_state(P, p$idx, x_adults)
  sts[, (paste0('nf_adults_', a)) := nf_adults]
  ho <- held_out(P)
  pid <- pop_identity(nf_adults)
  ref_cells <- vapply(p$targets, function(t) predict_t(P, t) / t$target - 1, numeric(1))
  ess <- kish(P)
  data.table(arm = a, n_targets = length(arms[[a]]), ipf_passes = fits[[a]]$iters,
             own_max_rel_err = fits[[a]]$maxrel,
             acs_cell_mard = mean(abs(ref_cells)),
             acs_cell_within2 = mean(abs(ref_cells) < 0.02),
             state_share_mard_vs_anchor = mard(shares(nf_adults), shares(sts$anchor_net_dorm)),
             oasdi65_share_mard = ho[['oasdi65_share_mard']],
             age_within_state_mard = ho[['age_within_state_mard']],
             pop_identity_mard = pid[['pop_identity_mard']],
             pop_identity_within_tol = pid[['pop_identity_within_tol']],
             hi2064_share_mard = hi_share(by_state(P, p$idx, tu$n_earn_2064[p$idx])),
             kl_from_prior = kl(P),
             kish_min = min(ess), kish_min_state = sts$state[which.min(ess)],
             kish_median = median(ess))
}))

# F5: the filer fit's signed residuals on n_returns and eitc_amt against the
# IRS EITC participation rates (TY2022 rates, so TY2022 only). Filers are fit
# on their own partition, so no non-filer arm can move this.
takeup_cor <- function(series) {
  if (year != 2022L) return(NA_real_)
  tk <- read_eitc_participation()
  e <- rbindlist(lapply(Filter(function(t) t$series == series, inputs$filers$targets), function(t)
    data.table(state = J[t$state], target = t$target,
               sgn = sum(inputs$filers$w[t$rows] * P_f[t$rows, t$state] * t$x) / t$target - 1)))
  e <- e[state %in% names(tk), .(sgn = weighted.mean(sgn, abs(target)), tw = sum(abs(target))), by = state]
  cov.wt(cbind(e$sgn, tk[e$state]), wt = e$tw, cor = TRUE)$cor[1, 2]
}

checks <- data.table(
  check = c('A vs B target shares (the signal B adds)',
            'ASEC all-age dependents vs HT2 dependents (shares)',
            'ASEC adult netting vs filer-fit adult dependents (shares)',
            'ASEC netted adult dependents, unweighted sample (min)',
            'ASEC netted adult dependents, unweighted sample (median)',
            'HI 20-64 share MARD expected from the 1% sample alone',
            'non-filer share of model wage earners 20-64',
            'EITC take-up cor: filer n_returns residuals (TY2022)',
            'EITC take-up cor: filer eitc_amt residuals (TY2022)'),
  value = c(mard(shares(sts$anchor_b), shares(sts$anchor_net_dorm)),
            mard(shares(sts$asec_dependents), shares(sts$ht2_dependents)),
            mard(shares(sts$asec_netting), shares(sts$puf_adult_dependents)),
            min(sts$asec_netting_n), median(sts$asec_netting_n),
            hi_noise_mard,
            sum(tu[p$idx, weight * n_earn_2064]) / sum(tu[, weight * n_earn_2064]),
            takeup_cor('n_returns'), takeup_cor('eitc_amt')))

fwrite(metrics, file.path(OUT, sprintf('nonfiler_anchor_bakeoff_metrics_%d.csv', year)))
fwrite(checks,  file.path(OUT, sprintf('nonfiler_anchor_bakeoff_checks_%d.csv', year)))
fwrite(sts[order(state)], file.path(OUT, sprintf('nonfiler_anchor_bakeoff_states_%d.csv', year)))
print(metrics); print(checks)

#------------------------------------------------------------------------------
# 6. Is B's lead over A signal? Household-cluster bootstrap of its netting
#------------------------------------------------------------------------------
# B's netting rests on 30-100 ASEC adults per state. Resample ASEC households
# (SERIAL) with replacement within state, rebuild B's targets, refit, and
# score: if B beats A only on some draws, its lead is sampling noise.

set.seed(BOOT_SEED)
# All ASEC households, not only those holding a netted dependent: how many
# such households a state's sample contains is itself part of the noise.
hh <- unique(persons[, .(state = FIPS_TO_STATE[as.character(STATEFIP)], SERIAL)])
ho_A <- held_out(fits$A$P)
boot <- rbindlist(lapply(seq_len(BOOT_REPS), function(r) {
  pick <- hh[, .(SERIAL = SERIAL[sample.int(.N, .N, replace = TRUE)]), by = state]
  d <- deps[pick, on = .(state, SERIAL), nomatch = NULL, allow.cartesian = TRUE]
  b <- nonfiler_state_anchor(anch, netting_by_state(d))
  b <- b[match(sts$state, state)]
  f <- fit_calibration(p$w, p$P0, state_targets(b$anchor),
                       n_iter = IPF_N_ITER, tol = IPF_TOL)
  stopifnot(length(f$unfittable) == 0)
  as.data.table(as.list(c(rep = r, held_out(f$P))))
}))
boot_summary <- data.table(
  metric = c('oasdi65_share_mard', 'age_within_state_mard'),
  A = ho_A,
  B_point = unlist(metrics[arm == 'B', .(oasdi65_share_mard, age_within_state_mard)]),
  B_boot_p05 = c(quantile(boot$oasdi65_share_mard, 0.05), quantile(boot$age_within_state_mard, 0.05)),
  B_boot_median = c(median(boot$oasdi65_share_mard), median(boot$age_within_state_mard)),
  B_boot_p95 = c(quantile(boot$oasdi65_share_mard, 0.95), quantile(boot$age_within_state_mard, 0.95)),
  share_B_beats_A = c(mean(boot$oasdi65_share_mard < ho_A[[1]]),
                      mean(boot$age_within_state_mard < ho_A[[2]])))
fwrite(boot_summary, file.path(OUT, sprintf('nonfiler_anchor_bakeoff_bootstrap_%d.csv', year)))
print(boot_summary)
message('NONFILER_ANCHOR_BAKEOFF_DONE ', year)
