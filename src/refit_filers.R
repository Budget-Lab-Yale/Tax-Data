#------------------------------------------------------------------------------
# refit_filers.R  (S40: the per-year national filer refit)
#
# Calibrates filer weights, one tax year at a time, to the Pub 1304 targets in
# resources/filer_refit_targets.csv (built and gated by
# research/state_weights/filer_refit/01_refit_targets.R): returns by filing
# status x age x AGI class (Table 1.6, combined cells as blocks), returns with
# wages by AGI (Table 1.4), dependent returns, all and with wages, by AGI
# (Table 1.7). Records are classed by src/agi_proxy.R.
#
# Method: entropy calibration (the "cal1" arm of the bake-off,
# research/state_weights/notes/filer_refit_design.md): minimize
#   KL(w || prior) / sum(prior) + (lambda / 2) * sum_j r_j^2,
# r_j the miss on target j in bands of max(0.5%, 500 returns), over
# log(w / prior) within [0.2, 5], by L-BFGS-B. It replaced the 2017 LP, whose
# objective (the sum of the multipliers) left every record at a bound.
#
# Functions only. src/refit_filers_2017.R (sourced by main.R after the
# imputations) refits TY2017 from the weights create_2017_puf.R held before
# its LP; project_puf.R calls refit_filer_year() for 2018-2023; the research
# scripts in research/state_weights/filer_refit/ reuse the target matrix.
#------------------------------------------------------------------------------

source('./src/agi_proxy.R')

REFIT_TARGETS    = './resources/filer_refit_targets.csv'
REFIT_TOL        = 0.005   # target band: +-0.5% ...
REFIT_TOL_FLOOR  = 500     # ... but never tighter than 500 returns
REFIT_LAMBDA     = 1       # penalty on a miss, per band, squared
REFIT_RATIO_MIN  = 0.2     # a record's weight relative to its prior
REFIT_RATIO_MAX  = 5
REFIT_MAXIT      = 5000

refit_targets = read_csv(REFIT_TARGETS, show_col_types = FALSE)

refit_band = function(t) pmax(REFIT_TOL * t, REFIT_TOL_FLOOR)

# Sparse records x targets indicator matrix: a target is the union of its
# member cells; NA in a member dimension means "all". `d` needs agi_proxy,
# filing_status, age1, dep_status and wages.
refit_target_matrix = function(d, tg) {
  cols = lapply(split(tg, tg$target_id), function(m) {
    hit = rep(FALSE, nrow(d))
    for (k in seq_len(nrow(m))) {
      r = m[k, ]
      h = d$agi_proxy >= r$agi_lo & d$agi_proxy < r$agi_hi
      if (!is.na(r$filing_status)) h = h & d$filing_status == r$filing_status
      if (!is.na(r$age_lo))        h = h & d$age1 >= r$age_lo & d$age1 < r$age_hi
      if (!is.na(r$dependent))     h = h & d$dep_status == r$dependent
      if (!is.na(r$wages))         h = h & (d$wages > 0) == (r$wages == 1)
      hit = hit | h
    }
    which(hit)
  })
  X = Matrix::sparseMatrix(i = unlist(cols), j = rep(seq_along(cols), lengths(cols)),
                           x = 1, dims = c(nrow(d), length(cols)))
  first = tg[!duplicated(tg$target_id), ]
  list(X = X, target_id = as.integer(names(cols)),
       t = first$target_value[match(as.integer(names(cols)), first$target_id)],
       source = first$source[match(as.integer(names(cols)), first$target_id)])
}

# Calibrate prior weights w0 to targets t over indicator matrix X. Targets no
# record reaches are dropped (the under-18 returns: the file has no filer under
# 18). `w_start`, if given, warm-starts the solve (the weight-income loop in
# project_puf.R passes the previous round's fit); the prior w0 is unchanged.
# Returns list(w, report).
calibrate_weights = function(w0, X, t, w_start = NULL) {
  reach = Matrix::colSums(X) > 0
  Xr = X[, reach, drop = FALSE]
  tr = t[reach]
  b  = refit_band(tr)
  W0 = sum(w0)
  fn_gr = function(u) {
    w = w0 * exp(u)
    r = (as.numeric(Matrix::crossprod(Xr, w)) - tr) / b
    list(f = sum(w0 * (exp(u) * u - exp(u) + 1)) / W0 + REFIT_LAMBDA / 2 * sum(r^2),
         g = w * u / W0 + REFIT_LAMBDA * w * as.numeric(Xr %*% (r / b)))
  }
  cache = new.env()
  f  = function(u) { cache$v = fn_gr(u); cache$u = u; cache$v$f }
  gr = function(u) { if (!identical(u, cache$u)) f(u); cache$v$g }
  u0 = if (is.null(w_start)) rep(0, length(w0)) else
         pmin(pmax(log(w_start / w0), log(REFIT_RATIO_MIN)), log(REFIT_RATIO_MAX))
  opt = optim(u0, f, gr, method = 'L-BFGS-B',
              lower = log(REFIT_RATIO_MIN), upper = log(REFIT_RATIO_MAX),
              control = list(maxit = REFIT_MAXIT, factr = 1e5))
  w = w0 * exp(opt$par)
  fitted = as.numeric(Matrix::crossprod(X, w))
  list(w = w,
       report = tibble(n_targets = length(t), n_unreached = sum(!reach),
                       within_band = mean(abs(fitted[reach] - tr) <= b * (1 + 1e-6)),
                       ratio_q01 = quantile(w / w0, 0.01), ratio_q99 = quantile(w / w0, 0.99),
                       convergence = opt$convergence, iterations = opt$counts[[1]]))
}

# Refit one year: `d` is that year's filers (materialized, one row per record,
# with a `weight` column holding the prior; `w_start`, a tibble(id, weight),
# optionally warm-starts it). Returns tibble(id, weight) and prints a one-line
# report.
refit_filer_year = function(d, year, w_start = NULL) {
  stopifnot(!any(duplicated(d$id)), all(d$weight > 0))
  d = agi_proxy(d, year)
  tg = refit_targets[refit_targets$year == year, ]
  if (nrow(tg) == 0) stop('refit_filer_year(): no targets for ', year)
  tm = refit_target_matrix(d, tg)
  ws = if (is.null(w_start)) NULL else w_start$weight[match(d$id, w_start$id)]
  if (!is.null(ws)) stopifnot(!anyNA(ws))
  fit = calibrate_weights(d$weight, tm$X, tm$t, ws)
  with(fit$report, cat(sprintf(
    'refit_filers: TY%d %d filers, %d targets (%d unreached), %.1f%% of the rest within band, ratio q01-q99 %.2f-%.2f, L-BFGS %d (%d)\n',
    year, nrow(d), n_targets, n_unreached, 100 * within_band, ratio_q01, ratio_q99,
    iterations, convergence)))
  tibble(id = d$id, weight = fit$w)
}
