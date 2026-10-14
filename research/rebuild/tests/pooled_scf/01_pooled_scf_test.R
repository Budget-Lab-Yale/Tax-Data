# 01_pooled_scf_test.R
#
# Real-SCF version of the D21 toy test (research/rebuild/design_decisions.md):
# can soft entropy calibration to one wave's published-style tables recover
# that wave's joint distribution of balance sheets when the imputation draws
# from (a) a single other wave, (b) a pool of other waves, (c) a pool that
# includes the other half of the target wave, (d) the other half alone?
#
# Usage: Rscript 01_pooled_scf_test.R <target_year> <split_id>
# Runs under Slurm only (run_test.sh). Writes one RDS per (target, split) to
# /nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/results/.
#
# Unit of analysis: SCF households from the summary extract (SCFP), NOT tax
# units. Each household appears five times (one row per implicate); every row
# is treated as a record with weight WGT. WGT = X42001 / 5, so the five
# implicates together sum to the household count (checked in 00_explore.R).
# A household's five rows always go to the same side of the split.
#
# Dollars: all four SCFP extracts are already in 2022 dollars (00b checks the
# factors against raw nominal values). "levels" therefore means the Fed's own
# price adjustment; "nominal" undoes it; "ratio" imputes amounts as ratios to
# income and rescales by the recipient's income.

suppressPackageStartupMessages(library(data.table))

args   = commandArgs(trailingOnly = TRUE)
TARGET = as.integer(args[1])
SPLIT  = as.integer(args[2])
stopifnot(TARGET %in% c(2013, 2016, 2019, 2022), !is.na(SPLIT))

ROOT  = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'
OUT   = '/nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/results'
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
WAVES = c(2013, 2016, 2019, 2022)

# Price factors the Fed applied to put each extract in 2022 dollars, measured
# in 00b_dollar_basis.R (STOCKS / raw X3915 for balance sheets; INCOME / raw
# X5729 for income, which refers to the prior calendar year).
FA = c(`2013` = 1.2728, `2016` = 1.2334, `2019` = 1.1592, `2022` = 1.0)
FI = c(`2013` = 1.2921, `2016` = 1.2493, `2019` = 1.1802, `2022` = 1.0809)

COMP = c('home', 'sf', 'ret', 'bus', 'debt')        # components with holding flags
AMT  = c(COMP, 'nw')                                  # all imputed amounts

#--------------------------------------------------------------------------
# Data
#--------------------------------------------------------------------------
load_wave = function(y) {
  d = fread(file.path(ROOT, y, 'historical', sprintf('SCFP%d.csv', y)),
            select = c('YY1', 'Y1', 'WGT', 'AGECL', 'MARRIED', 'KIDS', 'INCOME',
                       'HOUSES', 'HHOUSES', 'STOCKS', 'HSTOCKS', 'NMMF', 'HNMMF',
                       'RETQLIQ', 'HRETQLIQ', 'BUS', 'HBUS', 'DEBT', 'HDEBT', 'NETWORTH'))
  # No missing values are expected in any of these (00_explore.R found none).
  na = colSums(is.na(d)); if (any(na > 0)) stop('Unexpected NA in SCFP ', y, ': ', paste(names(na)[na > 0], collapse = ', '))
  data.table(wave = y, hh = d$YY1, w = d$WGT, agecl = d$AGECL, married = as.integer(d$MARRIED == 1),
             kids = d$KIDS, income = d$INCOME,
             home = d$HOUSES, sf = d$STOCKS + d$NMMF, ret = d$RETQLIQ, bus = d$BUS,
             debt = d$DEBT, nw = d$NETWORTH,
             # Holding flags as the Fed defines them (the SCFP H* variables).
             h_home = d$HHOUSES, h_sf = as.integer(d$HSTOCKS == 1 | d$HNMMF == 1),
             h_ret = d$HRETQLIQ, h_bus = d$HBUS, h_debt = d$HDEBT)
}
ALL = setNames(lapply(WAVES, load_wave), WAVES)

# Weighted income rank in (0, 1) within a set of rows; ties broken at random.
wrank = function(x, w) {
  o = order(x, runif(length(x))); cw = cumsum(w[o]); r = numeric(length(x))
  r[o] = (cw - w[o] / 2) / sum(w); r
}
wquant = function(x, w, p) { o = order(x); cw = cumsum(w[o]) / sum(w); x[o][which(cw >= p)[1]] }

# Income floor for the ratio method: the weighted 5th percentile of income in
# the row set (a rank, so it needs no price index). Ratios use
# max(income, floor) as the denominator so that near-zero incomes do not
# produce huge ratios.
add_rank = function(d) {
  d = copy(d)
  d[, rank := wrank(income, w)]
  d[, floor_y := wquant(income, w, 0.05)]
  d[, cell := (agecl - 1L) * 2L + married]
  d
}

#--------------------------------------------------------------------------
# Split the target wave by household
#--------------------------------------------------------------------------
set.seed(1e5 * TARGET + SPLIT)
tw  = ALL[[as.character(TARGET)]]
hhs = unique(tw$hh)
rec_hh = sample(hhs, floor(length(hhs) / 2))
R  = add_rank(tw[hh %in% rec_hh])            # recipients; their true balance sheets are the truth
DH = add_rank(tw[!hh %in% rec_hh])           # donor half of the target wave
stopifnot(length(intersect(R$hh, DH$hh)) == 0)
OTHERS = lapply(setdiff(WAVES, TARGET), function(y) add_rank(ALL[[as.character(y)]]))
names(OTHERS) = setdiff(WAVES, TARGET)

# Normalise recipient weights to mean 1 per record (row). The entropy
# distance is then measured in records, which fixes how hard a target with a
# given precision pulls (as in the toy test).
R[, b := w / mean(w)]
R[, id := .I]
NREC = nrow(R)

# Income groups for targets (quintiles 1-4, 80-90, 90-99, top 1%) and deciles
# for evaluation, by weighted income rank among recipients.
R[, grp := cut(rank, c(0, .2, .4, .6, .8, .9, .99, 1), labels = FALSE, include.lowest = TRUE)]
R[, dec := pmin(10L, floor(rank * 10) + 1L)]
stopifnot(!anyNA(R$grp), !anyNA(R$dec))
NG = 7

#--------------------------------------------------------------------------
# Donor sets
#--------------------------------------------------------------------------
nearest_older = function(t) { o = WAVES[WAVES < t]; if (length(o)) max(o) else min(WAVES[WAVES > t]) }
older2 = function(t) { o = WAVES[WAVES < t]; if (length(o) >= 2) sort(o, decreasing = TRUE)[2] else NA }

pool = function(parts) {
  # Each wave's weights sum to the same total (1) so that no wave dominates
  # the pooled draw because of its population or sample size.
  rbindlist(lapply(parts, function(p) { p = copy(p); p[, wd := w / sum(w)]; p }))
}
target_half = function() { p = copy(DH); p[, wave := TARGET]; p }

donor_sets = list()
a1 = nearest_older(TARGET)
donor_sets[[sprintf('a: single wave %d', a1)]] = list(pool(list(OTHERS[[as.character(a1)]])))
a2 = older2(TARGET)
if (!is.na(a2)) donor_sets[[sprintf('a: single wave %d', a2)]] = list(pool(list(OTHERS[[as.character(a2)]])))
donor_sets[['b: pooled, excluding target']] = list(pool(OTHERS))
donor_sets[['c: pooled, including target half']] = list(pool(c(OTHERS, list(target_half()))))
donor_sets[['d: target half only (benchmark)']] = list(pool(list(target_half())))

# Per-wave variants: copies drawn within each wave; starting weight leans
# toward the wave nearest the target year (60%), the rest split evenly.
lean_weights = function(waves, nearest, share = 0.6) {
  v = ifelse(waves == nearest, share, (1 - share) / (length(waves) - 1)); setNames(v, waves)
}
other_waves = setdiff(WAVES, TARGET)
nearest_other = other_waves[which.min(abs(other_waves - TARGET) + 0.1 * (other_waves > TARGET))]

#--------------------------------------------------------------------------
# Hot deck
#--------------------------------------------------------------------------
# For each recipient, the k donor rows nearest in income rank within its
# cell (age band x married); one donor drawn per copy with probability
# proportional to the donor weight wd. k is 3% of the cell's donor rows
# (at least 30 rows, about six households), so the neighbourhood covers a
# similar span of ranks whatever the pool size.
hot_deck = function(D, M) {
  out = vector('list', 12)
  for (cl in 1:12) {
    ri = which(R$cell == cl); if (!length(ri)) next
    dc = D[cell == cl][order(rank)]
    nd = nrow(dc); stopifnot(nd >= 30)
    k  = min(nd, max(30L, round(0.03 * nd)))
    cw = c(0, cumsum(dc$wd))
    pos = findInterval(R$rank[ri], dc$rank)
    lo  = pmin(pmax(pos - k %/% 2 + 1L, 1L), nd - k + 1L)
    hi  = lo + k - 1L
    out[[cl]] = rbindlist(lapply(seq_len(M), function(m) {
      u = cw[lo] + runif(length(ri)) * (cw[hi + 1L] - cw[lo])
      j = pmin(pmax(findInterval(u, cw), lo), hi)
      data.table(id = R$id[ri], copy = m, didx = j, cl = cl)
    }))
    out[[cl]][, `:=`(dwave = dc$wave[didx], dinc = dc$income[didx], dfloor = dc$floor_y[didx])]
    for (v in c(AMT, paste0('h_', COMP))) set(out[[cl]], j = v, value = dc[[v]][out[[cl]]$didx])
  }
  rbindlist(out)
}

# Put a donor's amounts on the recipient: three footings.
apply_footing = function(X, footing) {
  X = copy(X)
  if (footing == 'levels') return(X)           # extracts already in 2022 dollars
  if (footing == 'nominal') {                  # undo the Fed's price adjustment
    f = FA[as.character(TARGET)] / FA[as.character(X$dwave)]
    for (v in AMT) set(X, j = v, value = X[[v]] * f)
    return(X)
  }
  if (footing == 'ratio') {
    rinc = pmax(R$income[X$id], R$floor_y[X$id])
    f = rinc / pmax(X$dinc, X$dfloor)
    for (v in AMT) set(X, j = v, value = X[[v]] * f)
    return(X)
  }
  stop('unknown footing')
}

impute = function(spec) {
  if (is.null(spec$lean)) {
    X = hot_deck(spec$D, spec$M)
    X[, b := R$b[id] / spec$M]
  } else {
    X = rbindlist(lapply(names(spec$lean), function(y) {
      D = if (as.integer(y) == TARGET) pool(list(target_half())) else pool(list(OTHERS[[y]]))
      x = hot_deck(D, spec$M); x[, b := R$b[id] * spec$lean[[y]] / spec$M]; x
    }))
  }
  X = apply_footing(X, spec$footing)
  X[, `:=`(grp = R$grp[id], dec = R$dec[id], agecl = R$agecl[id], income = R$income[id])]
  X
}

#--------------------------------------------------------------------------
# Targets and calibration
#--------------------------------------------------------------------------
# Feature columns for one income group: households, then per component the
# number holding and the total amount, then total net worth.
feat_names = function(set) switch(set,
  rich   = c('n', paste0('hold_', COMP), paste0('tot_', COMP), 'tot_nw'),
  totals = c('n', paste0('tot_', COMP), 'tot_nw'),
  nw     = c('n', 'tot_nw'))
feat = function(X, set) {
  f = feat_names(set)
  sapply(f, function(nm) {
    if (nm == 'n') return(rep(1, nrow(X)))
    if (startsWith(nm, 'hold_')) return(as.numeric(X[[paste0('h_', sub('hold_', '', nm))]]))
    X[[sub('tot_', '', nm)]]
  })
}
truth_targets = function(set) {
  Fm = feat(R, set)
  t(sapply(1:NG, function(g) colSums(Fm[R$grp == g, , drop = FALSE] * R$b[R$grp == g])))
}

# Sampling standard error of every rich target, as a published table would
# report it: a bootstrap over the recipient half's households (all five
# implicates of a household move together; 200 replicates; income groups
# held fixed). Returned as a coefficient of variation (SE / |target|).
boot_cv = function(B = 200) {
  Fm = feat(R, 'rich'); nf = ncol(Fm)
  hi = match(R$hh, unique(R$hh)); H = max(hi)
  contrib = matrix(0, H, NG * nf)          # each household's contribution to each target
  for (g in 1:NG)                          # rows outside group g contribute zero to it
    contrib[, (g - 1) * nf + 1:nf] = rowsum(Fm * R$b * (R$grp == g), hi, reorder = TRUE)
  T0 = colSums(contrib)
  bt = t(sapply(1:B, function(r) colSums(contrib * tabulate(sample.int(H, H, replace = TRUE), H))))
  cv = apply(bt, 2, sd) / abs(T0)
  matrix(cv, NG, nf, byrow = TRUE, dimnames = list(NULL, colnames(Fm)))
}
CV = boot_cv()

# Precision (relative standard deviation of each target). Household counts
# 0.2% always. Otherwise either a flat value (e.g. 0.01) or 'se': the
# bootstrap coefficient of variation, floored at 1%.
prec_mat = function(set, PREC) {
  f = feat_names(set)
  P = if (identical(PREC, 'se')) pmax(CV[, f, drop = FALSE], 0.01) else matrix(as.numeric(PREC), NG, length(f))
  P[, f == 'n'] = 0.002
  P
}

# Soft entropy calibration (as in the toy test): minimise
#   sum_i w_i log(w_i/b_i) - w_i + b_i + sum_j (a_j'w / T_j - 1)^2 / (2 s_j^2)
# by Newton's method on the dual. Every target here lies within one income
# group and the groups are disjoint, so the problem splits by group.
calibrate_block = function(A, b, Tg, s) {
  stopifnot(all(abs(Tg) > 0))
  A = sweep(A, 2, Tg, '/'); s2 = s^2; lam = rep(0, ncol(A)); conv = FALSE
  obj = function(l) { e = drop(A %*% l); sum(b * exp(e)) - sum(l) + 0.5 * sum(s2 * l^2) }
  for (it in 1:300) {
    w = b * exp(drop(A %*% lam))
    g = drop(crossprod(A, w)) - 1 + s2 * lam
    if (max(abs(g)) < 1e-9) { conv = TRUE; break }
    H = crossprod(A, A * w) + diag(s2, ncol(A))
    d = -solve(H, g); f0 = obj(lam); step = 1
    while (!is.finite(obj(lam + step * d)) || obj(lam + step * d) > f0 + 1e-4 * step * sum(g * d)) {
      step = step / 2; if (step < 1e-12) break
    }
    lam = lam + step * d
  }
  w = b * exp(drop(A %*% lam)); g = drop(crossprod(A, w)) - 1 + s2 * lam
  # Converged if the first-order condition holds to 1e-6 in units of the
  # target (a millionth of each target), or the strict test above passed.
  list(w = w, conv = conv || max(abs(g)) < 1e-6, it = it, gmax = max(abs(g)))
}
calibrate = function(X, set, PREC) {
  Tm = truth_targets(set); w = numeric(nrow(X)); conv = TRUE; gmax = 0
  P  = prec_mat(set, PREC)
  for (g in 1:NG) {
    ix = which(X$grp == g)
    r  = calibrate_block(feat(X[ix], set), X$b[ix], Tm[g, ], P[g, ])
    w[ix] = r$w; conv = conv && r$conv; gmax = max(gmax, r$gmax)
  }
  list(w = w, conv = conv, gmax = gmax)
}

#--------------------------------------------------------------------------
# Evaluation
#--------------------------------------------------------------------------
TR_RICH = truth_targets('rich')
top_share = function(nw, w, p) {
  o = order(-nw); cw = cumsum(w[o]) / sum(w); prev = c(0, head(cw, -1))
  frac = pmin(1, pmax(0, (p - prev) / (cw - prev)))
  sum(w[o] * nw[o] * frac) / sum(w * nw)
}
tab_by = function(x, w, g, G) {
  # totals of each amount and holder counts by group g in 1..G
  tot  = sapply(AMT,  function(v) sapply(1:G, function(k) sum((w * x[[v]])[g == k])))
  hold = sapply(COMP, function(v) sapply(1:G, function(k) sum((w * x[[paste0('h_', v)]])[g == k])))
  n    = sapply(1:G, function(k) sum(w[g == k]))
  list(tot = tot, rate = hold / n)
}
ratio_q = function(x, w, g) {
  # Net worth to income ratio at the median and 90th percentile, by target
  # income group. Universe: rows with income > 0 (the ratio is undefined at
  # zero income; about 0.8% of rows).
  ok = x$income > 0
  t(sapply(1:NG, function(k) { s = ok & g == k; r = x$nw[s] / x$income[s]
    c(wquant(r, w[s], 0.5), wquant(r, w[s], 0.9)) }))
}
ED_N = 6000
emb = function(x) as.matrix(asinh(as.data.frame(x)[, c('income', AMT)] / 1e4))
mean_dist = function(P, Q) {
  tot = 0; qn = rowSums(Q^2)
  for (s in split(seq_len(nrow(P)), ceiling(seq_len(nrow(P)) / 1000))) {
    D2 = outer(rowSums(P[s, , drop = FALSE]^2), qn, '+') - 2 * P[s, , drop = FALSE] %*% t(Q)
    tot = tot + sum(sqrt(pmax(D2, 0)))
  }
  tot / (nrow(P) * nrow(Q))
}
TRUTH_EMB = emb(R[sample.int(NREC, ED_N, replace = TRUE, prob = R$b)])
TT_DIST   = mean_dist(TRUTH_EMB, TRUTH_EMB)
# Noise floor of the energy distance: truth against an independent resample
# of itself (same resample size as every comparison).
TRUTH_EMB2 = emb(R[sample.int(NREC, ED_N, replace = TRUE, prob = R$b)])
ED_FLOOR   = 2 * mean_dist(TRUTH_EMB, TRUTH_EMB2) - TT_DIST - mean_dist(TRUTH_EMB2, TRUTH_EMB2)
energy = function(X, w) {
  E = emb(X[sample.int(nrow(X), ED_N, replace = TRUE, prob = w)])
  2 * mean_dist(TRUTH_EMB, E) - TT_DIST - mean_dist(E, E)
}

TRUTH = list(dec = tab_by(R, R$b, R$dec, 10), age = tab_by(R, R$b, R$agecl, 6),
             top1 = top_share(R$nw, R$b, 0.01), top10 = top_share(R$nw, R$b, 0.10),
             own_stock = sum((R$b * R$h_home)[R$h_sf == 1]) / sum(R$b[R$h_sf == 1]),
             stock_bus = sum((R$b * R$h_sf)[R$h_bus == 1]) / sum(R$b[R$h_bus == 1]),
             ratio = ratio_q(R, R$b, R$grp))

evaluate = function(X, w) {
  Fm = feat(X, 'rich')
  ach = t(sapply(1:NG, function(g) colSums(Fm[X$grp == g, , drop = FALSE] * w[X$grp == g])))
  miss = abs(ach / TR_RICH - 1)
  zmiss = miss / prec_mat('rich', 'se')         # miss in units of the target's precision
  dec = tab_by(X, w, X$dec, 10); age = tab_by(X, w, X$agecl, 6)
  rel = function(a, b) abs(a / b - 1)
  rq  = ratio_q(X, w, X$grp)
  recw = tapply(w, X$id, sum); recb = tapply(X$b, X$id, sum); rr = recw / recb
  ess  = function(v) sum(v)^2 / sum(v^2)
  X[, w_ := w]
  cw   = X[, .(e = sum(w_)^2 / sum(w_^2) / .N), by = id]   # within-record copy ESS
  wave_sh = X[, .(s = sum(w_)), by = dwave][, s := s / sum(s)]
  c(miss_max = max(miss), miss_mean = mean(miss),
    miss_hold = max(miss[, grep('hold_', colnames(miss))]),
    miss_tot = max(miss[, grep('tot_', colnames(miss))]),
    miss_med = median(miss[, -1]), z_max = max(zmiss[, -1]), z_mean = mean(zmiss[, -1]),
    dec_tot_err = mean(rel(dec$tot[1:8, ], TRUTH$dec$tot[1:8, ])),
    dec_nw_err = mean(rel(dec$tot[1:8, 'nw'], TRUTH$dec$tot[1:8, 'nw'])),
    dec_rate_err = mean(abs(dec$rate[1:8, ] - TRUTH$dec$rate[1:8, ])),
    setNames(colMeans(rel(dec$tot[1:8, ], TRUTH$dec$tot[1:8, ])), paste0('dec_err_', AMT)),
    setNames(colMeans(abs(dec$rate[1:8, ] - TRUTH$dec$rate[1:8, ])), paste0('dec_rate_', COMP)),
    age_tot_err = mean(rel(age$tot, TRUTH$age$tot)),
    age_nw_err = mean(rel(age$tot[, 'nw'], TRUTH$age$tot[, 'nw'])),
    age_rate_err = mean(abs(age$rate - TRUTH$age$rate)),
    top1 = top_share(X$nw, w, 0.01), top10 = top_share(X$nw, w, 0.10),
    own_stock = sum((w * X$h_home)[X$h_sf == 1]) / sum(w[X$h_sf == 1]),
    stock_bus = sum((w * X$h_sf)[X$h_bus == 1]) / sum(w[X$h_bus == 1]),
    ratio_med_err = mean(rel(rq[, 1], TRUTH$ratio[, 1])),
    ratio_p90_err = mean(rel(rq[, 2], TRUTH$ratio[, 2])),
    energy = energy(X, w),
    rec_max = max(rr), rec_min = min(rr), rec_out = mean(rr < 0.5 | rr > 2),
    ess_rec = ess(recw) / ess(recb), copy_ess = mean(cw$e),
    target_wave_share = sum(wave_sh[dwave == TARGET, s]),
    nearest_wave_share = sum(wave_sh[dwave == nearest_other, s]))
}

#--------------------------------------------------------------------------
# Run
#--------------------------------------------------------------------------
# Two precision regimes for every imputation: 'se' (each target trusted to
# its bootstrap standard error, as a published table would be) and a flat 1%
# (targets treated as near exact, which they are in this test because they
# are the recipients' own values).
CAL_MAIN = list(list(set = 'rich', prec = 'se'), list(set = 'rich', prec = 0.01))
specs = list()
add = function(...) specs[[length(specs) + 1]] <<- list(...)
main_footings = c('levels', 'ratio')
for (ds in names(donor_sets)) for (M in c(1, 5, 20)) for (fo in main_footings)
  add(donors = ds, M = M, footing = fo, D = donor_sets[[ds]][[1]],
      cal = CAL_MAIN)
# Per-wave copies leaning to the nearest wave, 5 copies per wave.
for (fo in main_footings) {
  add(donors = sprintf('b: per wave, 60%% on %d', nearest_other), M = 5, footing = fo,
      lean = as.list(lean_weights(other_waves, nearest_other)), cal = CAL_MAIN)
  add(donors = sprintf('c: per wave, 60%% on %d half', TARGET), M = 5, footing = fo,
      lean = as.list(lean_weights(WAVES, TARGET)), cal = CAL_MAIN)
}
# Nominal footing (no price adjustment), single older wave and pooled.
for (ds in names(donor_sets)[c(1, which(startsWith(names(donor_sets), 'b:')))])
  add(donors = ds, M = 5, footing = 'nominal', D = donor_sets[[ds]][[1]],
      cal = CAL_MAIN)
# Target richness and precision, at 5 copies: attach extra calibrations to
# the matching 5-copy imputations (same imputed file, different targets).
extra_cal = list(list(set = 'totals', prec = 'se'), list(set = 'nw', prec = 'se'),
                 list(set = 'totals', prec = 0.01), list(set = 'nw', prec = 0.01))
for (i in seq_along(specs)) {
  s = specs[[i]]
  if (s$M == 5 && is.null(s$lean) && s$footing %in% main_footings &&
      substr(s$donors, 1, 1) %in% c('a', 'b', 'c', 'd'))
    specs[[i]]$cal = c(s$cal, extra_cal)
}

res = list(); t0 = Sys.time()
for (i in seq_along(specs)) {
  s = specs[[i]]
  set.seed(1e6 * TARGET + 1e3 * SPLIT + i)
  X = impute(s)
  base = evaluate(X, X$b)
  res[[length(res) + 1]] = data.table(target = TARGET, split = SPLIT, donors = s$donors, M = s$M,
                                      footing = s$footing, targets = 'none', prec = NA_character_,
                                      stage = 'uncalibrated', conv = TRUE, t(base))
  for (cs in s$cal) {
    cb = calibrate(X, cs$set, cs$prec)
    ev = evaluate(X, cb$w)
    res[[length(res) + 1]] = data.table(target = TARGET, split = SPLIT, donors = s$donors, M = s$M,
                                        footing = s$footing, targets = cs$set, prec = as.character(cs$prec),
                                        stage = 'calibrated', conv = cb$conv, gmax = cb$gmax, t(ev))
  }
  cat(sprintf('[%5.1f min] %d/%d %s M=%d %s\n', as.numeric(difftime(Sys.time(), t0, units = 'mins')),
              i, length(specs), s$donors, s$M, s$footing))
}
out = rbindlist(res, fill = TRUE)
truth = data.table(target = TARGET, split = SPLIT, top1 = TRUTH$top1, top10 = TRUTH$top10,
                   own_stock = TRUTH$own_stock, stock_bus = TRUTH$stock_bus,
                   n_rec = NREC, n_rec_hh = uniqueN(R$hh), n_donor_half_hh = uniqueN(DH$hh),
                   tt_dist = TT_DIST, ed_floor = ED_FLOOR)
saveRDS(list(res = out, truth = truth, truth_dec = TRUTH$dec, truth_age = TRUTH$age,
             truth_ratio = TRUTH$ratio, targets = TR_RICH, cv = CV),
        file.path(OUT, sprintf('res_%d_%02d.rds', TARGET, SPLIT)))
cat('done\n')
