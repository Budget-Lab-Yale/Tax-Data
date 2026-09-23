#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/nonfiler_anchor_bakeoff.R
#
# Phase 2 (plan.md §5a), F1b + F2 as a bake-off: what should the NON-FILER
# half of the state-weight fit target?
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
# B's netting is placed where the ASEC places people, i.e. dorm students at
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
# plus the distance each arm moves from the ACS cells, weight quality (KL
# from prior, Kish effective sample size), and two checks on B's input: the
# ASEC's claimed dependents (all ages) by state against HT2's dependent
# counts, and its adult netting against the filer fit's own adult dependents.
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

POOL <- 'research/state_weights/nonfiler_pool/results'
ANCH <- 'research/state_weights/nonfiler_residual/results'
OUT  <- 'research/state_weights/scripts/results'
CACHE <- file.path(scratch_root(), 'phase2_bakeoff')
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

IPF_N_ITER <- 500L     # state-total rakes converge in tens of passes
IPF_TOL    <- 1e-8
DORM_AGES  <- 18:24    # the ASEC full-time students taken as the dorm pool
BOOT_REPS  <- 200L     # household-cluster bootstrap replicates of B's netting
BOOT_SEED  <- 20260923L

# Same columns as prep_fit_inputs.R, plus what the held-out metrics read
INPUT_COLS <- c('id','weight','filer','filing_status','n_dep','age1',
                'wages','txbl_int','exempt_int','div_ord','div_pref',
                'kg_st','kg_lt','salt_inc_sales','salt_prop',
                'first_mort_int','second_mort_int','txbl_pens_dist',
                'txbl_ira_dist','gross_ss','sole_prop')
METRIC_COLS <- c('age2', 'dep_age1', 'dep_age2', 'dep_age3')

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

st <- readRDS(file.path(POOL, sprintf('units_%d.rds', year)))
cu <- readRDS(file.path(POOL, sprintf('calibrated_units_%d.rds', year)))
# The same join 04_calibrate.R makes: each 18+ dependent to their own scoring
# unit, whose calibrated filing probability decides whether they net out.
deps <- st$persons[is_dependent == TRUE & AGE >= 18,
                   .(SERIAL, PERNUM, AGE, ASECWT, STATEFIP, is_full_time_student,
                     unit_id = as.numeric(SERIAL) * 100 + PERNUM + 1e9)]
deps <- merge(deps, cu[unit_type == 'dependent', .(unit_id, p_file_cal)],
              by = 'unit_id', all.x = TRUE)
stopifnot(!anyNA(deps$p_file_cal), !anyNA(deps$is_full_time_student))
deps[, state := FIPS_TO_STATE[as.character(STATEFIP)]]
stopifnot(!anyNA(deps$state))
deps[, w_net := ASECWT * (1 - p_file_cal)]

# Tie-out: the national netting must be the one the pool calibrated against
sens <- fread(file.path(POOL, sprintf('netting_sensitivity_%d.csv', year)))
net_pool <- sens[scenario == 'central', sum(dep_nonfiling)]
stopifnot(abs(deps[, sum(w_net)] / net_pool - 1) < 1e-9)

anch <- fread(file.path(ANCH, sprintf('residual_anchors_%d.csv', year)))
stopifnot(nrow(anch) == 51, !anyNA(anch$residual_nonfiling_adults_net_dorm))
dorm_total <- anch[, sum(dorm_dependents)]

netting_by_state <- function(d) {
  n <- d[, .(asec_netting = sum(w_net), asec_netting_n = .N,
             asec_ft_student = sum(w_net * (is_full_time_student & AGE %in% DORM_AGES))),
         by = state]
  n[, dorm_at_parents := dorm_total * asec_ft_student / sum(asec_ft_student)]
  n[]
}
net_st <- netting_by_state(deps)

# All-age claimed dependents, for the geography check against HT2
all_deps <- st$persons[is_dependent == TRUE, .(asec_dependents = sum(ASECWT)),
                       by = .(state = FIPS_TO_STATE[as.character(STATEFIP)])]

sts <- Reduce(function(a, b) merge(a, b, by = 'state', all = TRUE),
              list(anch[, .(state, anchor_net_dorm = residual_nonfiling_adults_net_dorm,
                            ht2_dependents = dependents, oasdi_65p = beneficiaries_65p)],
                   net_st, all_deps))
stopifnot(nrow(sts) == 51, !anyNA(sts))
sts[, anchor_b := anchor_net_dorm - asec_netting + dorm_at_parents]
stopifnot(all(sts$anchor_b > 0))
message(sprintf(paste('ASEC netting %.2fM (ties to the pool), of which %.2fM',
                      'full-time students 18-24; ACS dorm students %.2fM'),
                sum(sts$asec_netting) / 1e6, sum(sts$asec_ft_student) / 1e6,
                dorm_total / 1e6))

#------------------------------------------------------------------------------
# 4. The three non-filer arms
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
  sh <- shares(level)
  lapply(seq_len(nrow(sts)), function(k) list(
    rows = seq_along(p$w), state = match(sts$state[k], J), x = x_adults,
    target = nf_total * sh[k], lambda = 1, series = 'n_adults_state',
    state_code = sts$state[k]))
}
arms <- list(ref = p$targets,
             A   = state_targets(sts$anchor_net_dorm),
             B   = state_targets(sts$anchor_b))

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
  ref_cells <- vapply(p$targets, function(t) predict_t(P, t) / t$target - 1, numeric(1))
  ess <- kish(P)
  data.table(arm = a, n_targets = length(arms[[a]]), ipf_passes = fits[[a]]$iters,
             own_max_rel_err = fits[[a]]$maxrel,
             acs_cell_mard = mean(abs(ref_cells)),
             acs_cell_within2 = mean(abs(ref_cells) < 0.02),
             state_share_mard_vs_anchor = mard(shares(nf_adults), shares(sts$anchor_net_dorm)),
             oasdi65_share_mard = ho[['oasdi65_share_mard']],
             age_within_state_mard = ho[['age_within_state_mard']],
             kl_from_prior = kl(P),
             kish_min = min(ess), kish_min_state = sts$state[which.min(ess)],
             kish_median = median(ess))
}))

checks <- data.table(
  check = c('A vs B target shares (the signal B adds)',
            'ASEC all-age dependents vs HT2 dependents (shares)',
            'ASEC adult netting vs filer-fit adult dependents (shares)',
            'ASEC netted adult dependents, unweighted sample (min)',
            'ASEC netted adult dependents, unweighted sample (median)'),
  value = c(mard(shares(sts$anchor_b), shares(sts$anchor_net_dorm)),
            mard(shares(sts$asec_dependents), shares(sts$ht2_dependents)),
            mard(shares(sts$asec_netting), shares(sts$puf_adult_dependents)),
            min(sts$asec_netting_n), median(sts$asec_netting_n)))

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
hh <- unique(st$persons[, .(state = FIPS_TO_STATE[as.character(STATEFIP)], SERIAL)])
ho_A <- held_out(fits$A$P)
boot <- rbindlist(lapply(seq_len(BOOT_REPS), function(r) {
  pick <- hh[, .(SERIAL = SERIAL[sample.int(.N, .N, replace = TRUE)]), by = state]
  d <- deps[pick, on = .(state, SERIAL), nomatch = NULL, allow.cartesian = TRUE]
  n <- netting_by_state(d)
  b <- merge(sts[, .(state, anchor_net_dorm)], n, by = 'state')
  stopifnot(nrow(b) == 51)
  b[, anchor_b := anchor_net_dorm - asec_netting + dorm_at_parents]
  stopifnot(all(b$anchor_b > 0))
  setkey(b, NULL); b <- b[match(sts$state, state)]
  f <- fit_calibration(p$w, p$P0, state_targets(b$anchor_b),
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
