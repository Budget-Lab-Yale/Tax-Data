#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 03_refit_fit.R  (Phase 3b step 3b, S40) -- one arm of the filer refit bake-off
#
# Fits one year's filer weights to resources/filer_refit_targets.csv from a
# prior, by one of three arms (research/state_weights/notes/filer_refit_design.md):
#   cal1, cal100  entropy calibration: minimize KL(w || prior) / total prior
#                 weight + (lambda/2) sum_j r_j^2, r_j the target miss in bands
#                 (lambda = 1 or 100), by L-BFGS-B on log(w / prior) within
#                 [RATIO_MIN, RATIO_MAX]
#   lp            linear programming with the objective the 2017 LP lacked:
#                 minimize sum_i (prior_i / total prior) |w_i / prior_i - 1|
#                 subject to every target within its band, same ratio bounds;
#                 elastic (a band slack costs 10 per band width), so a target
#                 the records cannot reach reports a miss instead of failing
# Priors: TY2017, the weights before the 2017 LP (refit_common.R); TY2023, the
# arm's own TY2017 fit times the vintage's status x age factor per record.
# Forbes records (S25) keep their weights and are netted out of the targets.
#
# Writes {TAXDATA_SCRATCH}/filer_refit/{arm}_{year}.rds =
#   list(id, w, prior, target_id, fitted, target, band, info)
#
# Usage (repo root, under sbatch -- see run_refit_bakeoff.sbatch):
#   Rscript research/state_weights/filer_refit/03_refit_fit.R cal1 2017
#------------------------------------------------------------------------------

source('research/state_weights/filer_refit/refit_common.R')
suppressPackageStartupMessages(library(Rglpk))

args <- commandArgs(trailingOnly = TRUE)
arm  <- args[1]
ty   <- as.integer(args[2])
stopifnot(arm %in% c("cal1", "cal100", "lp"), ty %in% c(2017L, 2023L))
LAMBDA   <- c(cal1 = 1, cal100 = 100)
LP_SLACK_COST <- 10
t0 <- Sys.time()

d  <- load_filers(ty)
tg <- fread(TARGETS)[year == ty]
tm <- target_matrix(d, tg)
X  <- tm$X
t_all <- tm$target$t
free <- which(!d$fixed)

prior <- d$w_current
if (ty == 2017L) {
  prior[free] <- prior_2017(d)
} else {
  f17 <- readRDS(file.path(fit_dir(), sprintf('%s_2017.rds', arm)))
  fac <- status_age_factor(d$id[free], ty)
  w17 <- f17$w[match(d$id[free], f17$id)]
  if (anyNA(w17) || anyNA(fac)) stop('TY', ty, ': ', sum(is.na(w17)), ' records missing a 2017 fit, ',
                                     sum(is.na(fac)), ' missing a status x age factor')
  prior[free] <- w17 * fac
}

# net the fixed records out of the targets; drop targets no free record reaches
Xf <- X[free, , drop = FALSE]
t_adj <- t_all - as.numeric(crossprod(X[-free, , drop = FALSE], prior[-free]))
reach <- colSums(Xf) > 0
if (any(!reach)) cat(sprintf('%d targets reached by no free record, dropped: %s\n', sum(!reach),
                             paste(tm$target_id[!reach], collapse = ' ')))
Xf <- Xf[, reach, drop = FALSE]; t_adj <- t_adj[reach]
b  <- band(t_all[reach])
w0 <- prior[free]; W0 <- sum(w0)
cat(sprintf('TY%d %s: %d free records, %d targets, prior misses %.1f bands on average\n', ty, arm,
            length(free), ncol(Xf), mean(abs(as.numeric(crossprod(Xf, w0)) - t_adj) / b)))

if (arm %in% names(LAMBDA)) {
  lambda <- LAMBDA[[arm]]
  fn_gr <- function(u) {
    w <- w0 * exp(u)
    r <- (as.numeric(crossprod(Xf, w)) - t_adj) / b
    list(f = sum(w0 * (exp(u) * u - exp(u) + 1)) / W0 + lambda / 2 * sum(r^2),
         g = w * u / W0 + lambda * w * as.numeric(Xf %*% (r / b)))
  }
  cache <- new.env()
  f  <- function(u) { cache$v <- fn_gr(u); cache$u <- u; cache$v$f }
  gr <- function(u) { if (!identical(u, cache$u)) f(u); cache$v$g }
  opt <- optim(rep(0, length(w0)), f, gr, method = 'L-BFGS-B',
               lower = log(RATIO_MIN), upper = log(RATIO_MAX),
               control = list(maxit = 5000, factr = 1e5))
  w_free <- w0 * exp(opt$par)
  info <- list(convergence = opt$convergence, message = opt$message, counts = opt$counts,
               value = opt$value)
} else {
  n <- length(w0); J <- ncol(Xf)
  A  <- t(Xf * w0)                                # J x n: target j's weight on record i's ratio
  base <- as.numeric(A %*% rep(1, n))             # fitted totals at the prior
  # variables: p (n), q (n), s_up (J), s_lo (J); ratio = 1 + p - q
  I_J <- Diagonal(J)
  M <- rbind(cbind(A, -A, -I_J, 0 * I_J),         # A(p - q) - s_up <= t + b - base
             cbind(A, -A, 0 * I_J, I_J))          # A(p - q) + s_lo >= t - b - base
  M <- as(M, 'TsparseMatrix')
  mat <- slam::simple_triplet_matrix(M@i + 1L, M@j + 1L, M@x, nrow(M), ncol(M))
  obj <- c(w0 / W0, w0 / W0, rep(LP_SLACK_COST, J) / b, rep(LP_SLACK_COST, J) / b)
  sol <- Rglpk_solve_LP(obj, mat, dir = c(rep('<=', J), rep('>=', J)),
                        rhs = c(t_adj + b - base, t_adj - b - base),
                        bounds = list(upper = list(ind = seq_len(2 * n),
                                                   val = c(rep(RATIO_MAX - 1, n), rep(1 - RATIO_MIN, n)))),
                        max = FALSE, control = list(verbose = TRUE, presolve = TRUE))
  if (sol$status != 0) stop('GLPK status ', sol$status)
  x <- sol$solution
  w_free <- w0 * (1 + x[seq_len(n)] - x[n + seq_len(n)])
  info <- list(status = sol$status, optimum = sol$optimum,
               slack_bands = sum(x[2 * n + seq_len(2 * J)] > 1e-9))
}

w <- d$w_current; w[free] <- w_free
fitted <- as.numeric(crossprod(X, w))
dir.create(fit_dir(), showWarnings = FALSE, recursive = TRUE)
saveRDS(list(id = d$id, w = w, prior = prior, target_id = tm$target_id, fitted = fitted,
             target = t_all, band = band(t_all), info = info),
        file.path(fit_dir(), sprintf('%s_%d.rds', arm, ty)))
within <- abs(fitted - t_all) <= band(t_all)
cat(sprintf('TY%d %s: %.1f%% of targets within band; ratio to prior %s; %.1f min\n', ty, arm,
            100 * mean(within), paste(round(quantile(w_free / w0, c(0, .05, .5, .95, 1)), 3), collapse = '/'),
            as.numeric(Sys.time() - t0, units = 'mins')))
str(info)
cat('REFIT_FIT_SUCCESS\n')
