#---------------------------------------------
# tilt_solver.R
#
# Stage 3 calibrated DRF donor-tilt solver.
#
# Replaces the Step-A swap solver. For each
# PUF record i in a (cell_income x cell_age)
# bucket, the DRF gives a vector of donor
# probabilities p_ij over its cell's bootstrap
# (drf::get_sample_weights). We tilt these
# probabilities by an exponential factor in a
# small set of donor target features Z_j:
#
#   q_ij(lambda) = p_ij * exp(Z_j' lambda)
#                  / sum_l p_il * exp(Z_l' lambda)
#
# and pick lambda to minimize a scaled-residual
# quadratic against SCF cell aggregate targets,
# plus a small ridge penalty:
#
#   L(lambda) = sum_m ((T_hat_m - target_m)/scale_m)^2
#             + eta * ||lambda||^2
#
# T_hat_m(lambda) = sum_i puf_w[i] * sum_j q_ij * Z_jm.
#
# Block structure: per-cell DRFs mean q_ij has
# zero cross-cell support, so the gradient
# decomposes block-diagonally by cell. We still
# present a single global lambda + single optim()
# call -- the per-cell decomposition is just an
# implementation detail of the gradient pass.
#
# Per-record sampling at the converged lambda is
# done in solve_tilt_global (after fit), with
# top-K + threshold sparsification per record.
#
# Two entry points:
#   solve_tilt_block(...)  -- one cell, with W supplied
#                              directly. Used by tests
#                              and by the global wrapper.
#   solve_tilt_global(...) -- multi-cell with chunked
#                              drf::get_sample_weights
#                              prediction.
#---------------------------------------------


#' Single-cell tilt solver (W supplied directly).
#'
#' @param W            [n_puf x n_donor] DRF row-normalized probabilities.
#'                      Each row sums to 1 (or near 1 -- we normalize anyway).
#' @param Z            [n_donor x M] donor target features. Columns are
#'                      named (cat amounts and indicators).
#' @param puf_w        [n_puf] PUF weights for records in this cell.
#' @param target       [M] SCF cell aggregate targets, named matching colnames(Z).
#' @param scale        [M] denominators for the quadratic objective.
#'                      amounts: max(|target|, amount_denom_floor) (default 1e9).
#'                      counts:  max(target, 1).
#' @param ridge_eta    Numeric. Ridge penalty on lambda. Default 1e-4.
#' @param max_iters    Integer. L-BFGS-B max iterations. Default 200.
#' @param tol_rel      Numeric. Per-target relative-miss tolerance for
#'                      'converged' status. Default 0.01.
#' @param init_lambda  Numeric [M], starting point. Default zeros.
#' @param init_seed    Integer. Reserved for parity with swap_solver shape;
#'                      tilt is deterministic given inputs (no RNG inside
#'                      optim).
#'
#' @return list(lambda, T_hat, residuals, max_rel, lambda_norm, status,
#'              effective_donors_mean, kl_to_uniform_mean, n_iters,
#'              converged)
solve_tilt_block = function(W, Z, puf_w, target, scale,
                             ridge_eta   = 1e-4,
                             max_iters   = 200L,
                             tol_rel     = 0.01,
                             init_lambda = NULL,
                             init_seed   = 42L,
                             lambda_max  = NULL) {

  # lambda_max: if non-NULL, switch to L-BFGS-B with box constraints
  # [-lambda_max, +lambda_max] on every component of lambda. Belt-and-
  # suspenders against runaway tilt in feasibility-limited cells. With
  # Z_n in [-1, 1], lambda_max=5 gives tilt factors exp(±5) ~ [0.007, 148]
  # — plenty of dynamic range without the corner-collapse that uncapped
  # BFGS exhibited in v1.
  stopifnot(is.matrix(W), is.matrix(Z),
            ncol(W) == nrow(Z),
            nrow(W) == length(puf_w),
            ncol(Z) == length(target),
            length(target) == length(scale),
            !is.null(colnames(Z)))

  M = ncol(Z)
  if (is.null(init_lambda)) init_lambda = rep(0, M)
  stopifnot(length(init_lambda) == M)

  # ---- Two-stage rescaling for L-BFGS-B conditioning ----------------------
  # Raw Z columns span 24 orders of magnitude (count cols in {0, 1};
  # amount cols up to ~1e8 dollars per donor). The natural lambda scale
  # is ~ 1 / range(Z[, m]), so an unscaled L-BFGS-B sees Hessian
  # eigenvalues spanning ~24 orders -- the line search trips at the
  # initial point.
  #
  # Fix: normalize Z columns by their abs-max so Z_n in [-1, 1] for all
  # cols. The natural lambda scale in the normalized basis is then O(1)
  # uniformly, and L-BFGS-B's quadratic approximation works.
  #
  # The residual scale (proposal sec 7) stays as a separate factor on
  # the objective; in the normalized basis it becomes scale_n =
  # scale_safe / z_norm, so the objective value
  # sum(((T_hat_n - target_n) / scale_n)^2) equals
  # sum(((T_hat   - target  ) / scale  )^2) -- same problem, well-
  # conditioned coordinates.
  z_norm = pmax(apply(abs(Z), 2, max), 1)         # never divide by 0
  Z_n    = sweep(Z, 2, z_norm, '/')
  scale_safe = pmax(abs(scale), 1)
  target_n   = target     / z_norm
  scale_n    = scale_safe / z_norm

  # Forward + gradient pass over the block. Closed-form gradient via the
  # donor-aggregated weight identity:
  #
  #   QW_j = sum_i puf_w[i] * Q[i,j]                    [n_donor]
  #   T_hat_m = sum_j QW_j * Z[j,m]                     [M]
  #   QZ      = Q %*% Z                                  [n_puf x M]
  #   d T_m / d lambda_p
  #     = sum_i puf_w[i] * ( E_q[Z_m Z_p | i] - E_q[Z_m|i] E_q[Z_p|i] )
  #     = (Z' diag(QW) Z)_{mp} - (QZ' diag(puf_w) QZ)_{mp}
  #
  # We avoid materializing Q during optim's iterations -- Q is a
  # [n_puf x n_donor] matrix that doubles peak memory and adds
  # allocation/copy work per iteration. The identities
  #
  #   s[i]  = sum_j W[i,j] * tilt[j]            (row-norm)
  #   QZ    = (W %*% (tilt * Z)) / s            (broadcast / row)
  #   QW    = tilt * (W' %*% (puf_w / s))
  #
  # let us compute T_hat and dTdL via mat-vec / small mat-mat products
  # only, with peak memory = W + a few small matrices. Q itself is
  # rebuilt by `materialize_Q` only at the final pass for sampling and
  # KL/ESS diagnostics.
  # All optim work happens on the NORMALIZED basis (Z_n, target_n, scale_n).
  # T_hat_n is recovered to original scale by multiplication by z_norm.
  forward_grad = function(lambda_in, want_Q = FALSE) {
    tilt = exp(as.numeric(Z_n %*% lambda_in))           # [n_donor]
    tilt[!is.finite(tilt)] = .Machine$double.xmax / 1e10

    s = as.numeric(W %*% tilt)                          # [n_puf]
    s[s <= 0] = 1

    Z_til = sweep(Z_n, 1, tilt, '*')                    # [n_donor x M]
    WT    = as.matrix(W %*% Z_til)                      # [n_puf x M]
    QZ    = WT / s                                      # [n_puf x M]
    T_hat_n = as.numeric(crossprod(QZ, puf_w))          # [M], normalized basis

    u   = puf_w / s
    WTu = as.numeric(crossprod(W, u))                   # [n_donor]
    QW  = tilt * WTu                                    # [n_donor]

    Zw    = sweep(Z_n, 1, QW, '*')                      # [n_donor x M]
    term1 = crossprod(Z_n, Zw)                          # [M x M]
    QZw   = sweep(QZ, 1, puf_w, '*')                    # [n_puf x M]
    term2 = crossprod(QZ, QZw)                          # [M x M]
    dTdL_n = term1 - term2

    out = list(T_hat_n = T_hat_n, dTdL_n = dTdL_n,
               T_hat   = T_hat_n * z_norm,             # back to original scale
               tilt    = tilt, s = s,
               eff_donor_mean = NA_real_, eff_donor_p10 = NA_real_,
               kl_mean        = NA_real_, Q = NULL)

    if (want_Q) {
      # Materialize Q only at the final pass. Memory cost: one big alloc.
      num = sweep(W, 2, tilt, '*')              # [n_puf x n_donor]
      Q   = num / s                             # tilted q_ij
      rm(num)
      eff_donor   = 1 / rowSums(Q^2)
      safe_W      = pmax(W, 1e-30)
      safe_Q      = pmax(Q, 1e-30)
      kl_per_row  = rowSums(safe_Q * (log(safe_Q) - log(safe_W)))
      out$eff_donor_mean = mean(eff_donor)
      out$eff_donor_p10  = quantile(eff_donor, 0.10)
      out$kl_mean        = mean(kl_per_row)
      out$Q              = Q
    }
    out
  }

  # fn / gr functions (gr re-uses last fn evaluation through a closure).
  cache = list(lambda = NULL, fwd = NULL)
  ensure_fwd = function(lambda_in) {
    if (!is.null(cache$lambda) && all(cache$lambda == lambda_in)) return(cache$fwd)
    fwd = forward_grad(lambda_in)
    cache$lambda <<- lambda_in
    cache$fwd    <<- fwd
    fwd
  }

  # In the normalized basis the objective is
  #   fn(lambda) = sum(((T_hat_n - target_n) / scale_n)^2) + ridge ||lambda||^2
  # which equals sum(((T_hat - target) / scale)^2) + ridge ||lambda||^2 in
  # original units. The Hessian here is well-conditioned because all Z_n
  # columns are O(1).
  fn = function(lambda_in) {
    fwd = ensure_fwd(lambda_in)
    res = (fwd$T_hat_n - target_n) / scale_n
    val = sum(res^2) + ridge_eta * sum(lambda_in^2)
    if (!is.finite(val)) return(.Machine$double.xmax / 1e10)
    val
  }
  gr = function(lambda_in) {
    fwd = ensure_fwd(lambda_in)
    v   = 2 * (fwd$T_hat_n - target_n) / scale_n^2   # [M]
    g   = as.numeric(fwd$dTdL_n %*% v) + 2 * ridge_eta * lambda_in
    g[!is.finite(g)] = 0
    g
  }

  # Optimizer choice: BFGS (unconstrained) by default, L-BFGS-B with box
  # constraints when lambda_max is set. L-BFGS-B has slightly more line-
  # search overhead but won't blow up.
  opt = tryCatch(
    if (!is.null(lambda_max) && is.finite(lambda_max) && lambda_max > 0) {
      optim(par = init_lambda, fn = fn, gr = gr,
            method = "L-BFGS-B",
            lower = -lambda_max, upper = +lambda_max,
            control = list(maxit = max_iters, factr = 1e9))
    } else {
      optim(par = init_lambda, fn = fn, gr = gr,
            method = "BFGS",
            control = list(maxit = max_iters, reltol = 1e-4))
    },
    error = function(e) list(par = init_lambda, value = NA, convergence = -1L,
                              message = conditionMessage(e))
  )

  # Final pass at converged lambda materializes Q for sampling + KL/ESS
  # diagnostics. Cache lookup is bypassed (cache only stored no-Q passes).
  fwd_final = forward_grad(opt$par, want_Q = TRUE)
  # T_hat is in the original (unscaled) basis. Compare against the
  # original-scale target.
  residuals = fwd_final$T_hat - target
  rel       = abs(residuals) / scale_safe
  max_rel   = max(rel)

  # optim convergence codes:
  #   0  = success
  #   1  = max iterations reached (BFGS / L-BFGS-B / others)
  #   51 = warning from the C side (L-BFGS-B specific)
  #   52 = error from the C side (L-BFGS-B specific)
  #   -1 = our tryCatch error fallback
  # 0 with max_rel < tol_rel is fully "converged"; 0 with max_rel >= tol
  # or 1 (max-iter) are both "cap" — solver ran fine but didn't reach
  # our user-facing tolerance. Anything else is an actual error.
  status = if (!is.null(opt$convergence) &&
               opt$convergence %in% c(0L, 1L) &&
               max_rel < tol_rel) {
    'converged'
  } else if (!is.null(opt$convergence) &&
             opt$convergence %in% c(0L, 1L)) {
    'cap'
  } else {
    'optim_error'
  }

  list(
    lambda                = opt$par,
    T_hat                 = fwd_final$T_hat,
    residuals             = residuals,
    rel_residuals         = rel,
    max_rel               = max_rel,
    lambda_norm           = sqrt(sum(opt$par^2)),
    objective             = opt$value,
    status                = status,
    optim_convergence     = if (is.null(opt$convergence)) NA else opt$convergence,
    optim_message         = if (is.null(opt$message)) NA else opt$message,
    n_iters               = if (!is.null(opt$counts)) opt$counts['function'] else NA,
    effective_donors_mean = fwd_final$eff_donor_mean,
    effective_donors_p10  = fwd_final$eff_donor_p10,
    kl_to_uniform_mean    = fwd_final$kl_mean,
    Q_at_optimum          = fwd_final$Q
  )
}


#' Per-target attainable bounds on a cell. For each target column m, the
#' min (max) attainable T_hat is achieved by putting all probability on the
#' donor with smallest (largest) Z_jm value, weighted by puf_w. This
#' replaces the q_ij row with a delta on argmin/argmax j; the implied
#' aggregate becomes sum_i puf_w[i] * min_j Z_jm = sum(puf_w) * min_j Z_jm
#' (since the bound holds independent of i). For a feasibility check we
#' care whether target_m is inside this interval.
attainable_bounds = function(Z, puf_w) {
  W_total = sum(puf_w)
  apply(Z, 2, function(z) c(min = W_total * min(z),
                             max = W_total * max(z)))
}


#' Multi-cell tilt solver with chunked drf::get_sample_weights prediction.
#'
#' @param cell_payload  Named list of length n_cells. For each cell:
#'   X_puf    : matrix [n_puf_cell x n_features]
#'   puf_w    : vector [n_puf_cell]
#'   drf_obj  : a drf model object whose training data has length matching nrow(donor_Z)
#'   donor_Z  : matrix [n_donor_cell x M_c] with named columns
#'   target   : vector [M_c] with names matching colnames(donor_Z)
#'   scale    : vector [M_c]
#' @param chunk_size    Integer. PUF rows per get_sample_weights call.
#' @param ridge_eta     Numeric. Ridge penalty on lambda. Default 1e-4.
#' @param max_iters     Integer. L-BFGS-B max iters per cell. Default 200.
#' @param tol_rel       Numeric. Per-target relative-miss tolerance.
#' @param verbose       Logical.
#'
#' @return list(per_cell, lambda_global, status_summary, total_iters).
#'         per_cell[[ci]] is a solve_tilt_block result + cell metadata.
solve_tilt_global = function(cell_payload,
                              chunk_size  = 2000L,
                              ridge_eta   = 1e-4,
                              max_iters   = 200L,
                              tol_rel     = 0.01,
                              verbose     = TRUE) {
  cell_names = names(cell_payload)
  stopifnot(length(cell_names) > 0L, !any(is.na(cell_names)))

  per_cell = vector('list', length(cell_names))
  names(per_cell) = cell_names

  for (ci in cell_names) {
    payload = cell_payload[[ci]]
    n_puf   = nrow(payload$X_puf)
    n_donor = nrow(payload$donor_Z)
    M_c     = ncol(payload$donor_Z)

    if (verbose) {
      cat(sprintf('tilt: cell %-12s  n_puf=%6d  n_donor=%6d  M=%2d\n',
                  ci, n_puf, n_donor, M_c))
    }

    # Build the dense W for this cell by chunking get_sample_weights.
    # For now we materialize W per cell (kept across many optim iterations
    # of solve_tilt_block to avoid recomputing). This trades RAM for speed.
    # Largest bootstrap is 150k donors; with int64 indices a [n_puf x n_donor]
    # double matrix can exceed 30 GB on the largest cells. If memory is an
    # issue we can swap in a recompute-each-iter path.
    chunks = split(seq_len(n_puf),
                   ceiling(seq_len(n_puf) / chunk_size))
    W_chunks = vector('list', length(chunks))
    t0 = Sys.time()
    for (k in seq_along(chunks)) {
      idx = chunks[[k]]
      W_k = drf::get_sample_weights(payload$drf_obj,
                                     newdata = payload$X_puf[idx, , drop = FALSE])
      stopifnot(ncol(W_k) == n_donor)
      W_chunks[[k]] = W_k
    }
    W = do.call(rbind, W_chunks)
    rm(W_chunks); gc(verbose = FALSE)
    if (verbose) {
      cat(sprintf('  predict W [%d x %d]: %.1fs (%.2f GB)\n',
                  n_puf, n_donor,
                  as.numeric(Sys.time() - t0, units = 'secs'),
                  as.numeric(object.size(W)) / 1024^3))
    }

    res = solve_tilt_block(
      W           = W,
      Z           = payload$donor_Z,
      puf_w       = payload$puf_w,
      target      = payload$target,
      scale       = payload$scale,
      ridge_eta   = ridge_eta,
      max_iters   = max_iters,
      tol_rel     = tol_rel,
      init_lambda = NULL
    )

    bounds = attainable_bounds(payload$donor_Z, payload$puf_w)
    out_of_range = (payload$target < bounds['min', ]) |
                   (payload$target > bounds['max', ])
    res$attainable_min = bounds['min', ]
    res$attainable_max = bounds['max', ]
    res$out_of_range   = out_of_range
    if (any(out_of_range) && res$status == 'cap') {
      res$status = 'infeasible'
    }
    res$cell_name = ci

    if (verbose) {
      cat(sprintf('  status=%-12s  max_rel=%.3e  ||lambda||=%.3f  ESS_mean=%.1f  KL_mean=%.3f  iters=%s\n',
                  res$status, res$max_rel, res$lambda_norm,
                  res$effective_donors_mean, res$kl_to_uniform_mean,
                  as.character(res$n_iters)))
    }

    # Don't keep Q at the cell level; the caller does sampling against W
    # directly. Drop heavy fields from per_cell return.
    res$Q_at_optimum = NULL
    per_cell[[ci]] = res
    rm(W); gc(verbose = FALSE)
  }

  status_summary = sapply(per_cell, function(r) r$status)
  total_iters    = sum(sapply(per_cell, function(r)
                              if (is.na(r$n_iters)) 0 else as.integer(r$n_iters)))

  lambda_global = unlist(lapply(per_cell, function(r) r$lambda))

  list(
    per_cell       = per_cell,
    lambda_global  = lambda_global,
    status_summary = status_summary,
    total_iters    = total_iters
  )
}


#' Sample one donor per record at the converged tilt. Top-K + threshold
#' sparsification per record (proposal section 10) -- after tilting, restrict
#' the sampling support to the top K donors (or those above q_threshold)
#' and renormalize, then draw.
#'
#' @param tilt_result   Output of solve_tilt_global.
#' @param cell_payload  Same as for solve_tilt_global (we re-predict W).
#' @param q_threshold   Drop donors with q < this BEFORE renormalize+sample.
#' @param top_k         If > 0, additionally cap to top_k donors per record.
#' @param chunk_size    Integer. PUF rows per get_sample_weights call.
#' @param init_seed     Integer. Per-cell seed = init_seed + cell_idx.
#' @param verbose       Logical.
#'
#' @return list(donor_assignment, eff_donor_mean, top_k_used_mean,
#'              q_kept_mass_mean) per cell + flat assignment vector keyed
#'              to global record index across cells.
sample_tilt_donors = function(tilt_result, cell_payload,
                               q_threshold = 1e-6,
                               top_k       = 300L,
                               chunk_size  = 2000L,
                               init_seed   = 1000L,
                               verbose     = TRUE) {
  cell_names = names(cell_payload)
  per_cell   = vector('list', length(cell_names))
  names(per_cell) = cell_names

  for (ci_idx in seq_along(cell_names)) {
    ci      = cell_names[ci_idx]
    payload = cell_payload[[ci]]
    lambda  = tilt_result$per_cell[[ci]]$lambda
    n_puf   = nrow(payload$X_puf)
    n_donor = nrow(payload$donor_Z)

    set.seed(init_seed + ci_idx)
    tilt   = exp(as.numeric(payload$donor_Z %*% lambda))
    tilt[!is.finite(tilt)] = .Machine$double.xmax / 1e10

    donor_assignment = integer(n_puf)
    eff_donor_vec = numeric(n_puf)
    k_used_vec    = integer(n_puf)
    qkm_vec       = numeric(n_puf)

    chunks = split(seq_len(n_puf),
                   ceiling(seq_len(n_puf) / chunk_size))
    for (idx in chunks) {
      W_k = drf::get_sample_weights(payload$drf_obj,
                                     newdata = payload$X_puf[idx, , drop = FALSE])
      num = sweep(W_k, 2, tilt, '*')
      s   = rowSums(num)
      s[s <= 0] = 1
      Q   = num / s
      rm(num)

      for (j in seq_along(idx)) {
        i = idx[j]
        qrow = Q[j, ]
        # Threshold drop
        keep = which(qrow >= q_threshold)
        # Top-K cap
        if (top_k > 0L && length(keep) > top_k) {
          ord  = order(qrow[keep], decreasing = TRUE)
          keep = keep[ord[seq_len(top_k)]]
        }
        if (length(keep) == 0L) {
          # Degenerate row: fall back to argmax of qrow.
          keep = which.max(qrow)
        }
        qi = qrow[keep] / sum(qrow[keep])
        donor_assignment[i] = keep[sample.int(length(keep), 1L, prob = qi)]
        eff_donor_vec[i] = 1 / sum(qi^2)
        k_used_vec[i]    = length(keep)
        qkm_vec[i]       = sum(qrow[keep])  # mass kept (pre-renorm)
      }
      rm(W_k, Q)
    }
    gc(verbose = FALSE)

    if (verbose) {
      cat(sprintf('  sample %-12s n=%6d  ESS_mean=%.1f  k_used_mean=%.0f  q_kept_mean=%.3f\n',
                  ci, n_puf, mean(eff_donor_vec),
                  mean(k_used_vec), mean(qkm_vec)))
    }

    per_cell[[ci]] = list(
      donor_assignment    = donor_assignment,
      eff_donor_mean      = mean(eff_donor_vec),
      eff_donor_p10       = quantile(eff_donor_vec, 0.10),
      k_used_mean         = mean(k_used_vec),
      q_kept_mass_mean    = mean(qkm_vec)
    )
  }

  per_cell
}


#---------------------------------------------------------------------------
# Standalone tests. Run with:
#   Rscript src/imputations/tilt_solver.R
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  cat('--- tilt_solver.R tests ---\n')

  # ------ Test 1: lambda=0 sanity -----------------------------------------
  set.seed(7)
  n_puf = 50L; n_donor = 30L; M = 3L
  W_raw = matrix(runif(n_puf * n_donor), n_puf, n_donor)
  W     = W_raw / rowSums(W_raw)
  Z     = matrix(runif(n_donor * M, -1, 1), n_donor, M)
  colnames(Z) = paste0('m', 1:M)
  puf_w = runif(n_puf, 0.5, 1.5)

  # Compute the lambda=0 implied aggregate -- this is target.
  T0 = as.numeric(crossprod(W %*% Z, puf_w))
  res0 = solve_tilt_block(W, Z, puf_w,
                           target = T0, scale = abs(T0) + 1,
                           init_lambda = rep(0, M))
  stopifnot(res0$max_rel < 1e-10)
  stopifnot(max(abs(res0$lambda)) < 1e-6)
  cat('  [PASS] lambda=0 already at target (residual=', res0$max_rel, ')\n', sep='')

  # ------ Test 2: small attainable target ---------------------------------
  # Move target away from T0 by a small amount in a feasible direction.
  set.seed(8)
  shift = T0 * 0.10        # 10% away
  target = T0 + shift
  scale  = abs(target) + 1
  res = solve_tilt_block(W, Z, puf_w, target, scale,
                         tol_rel = 1e-4, max_iters = 500L)
  stopifnot(res$status == 'converged')
  stopifnot(res$max_rel < 1e-3)
  cat(sprintf('  [PASS] feasible target hit (max_rel=%.2e, ||lambda||=%.3f, iters=%s)\n',
              res$max_rel, res$lambda_norm, as.character(res$n_iters)))

  # ------ Test 3: Closed-form gradient vs finite difference ---------------
  # Verify the gradient identity in the RAW Z basis (independent of the
  # solver's internal normalized basis). Use a fixed small lambda so
  # gradients are nontrivial but tilt is mild.
  set.seed(31)
  lambda_test = rnorm(M, sd = 0.05)
  fwd = (function() {
    tilt = exp(as.numeric(Z %*% lambda_test))
    num  = sweep(W, 2, tilt, '*')
    s    = rowSums(num)
    Q    = num / s
    QW   = as.numeric(crossprod(Q, puf_w))
    QZ   = Q %*% Z
    T_hat = as.numeric(crossprod(QZ, puf_w))
    Zw   = sweep(Z, 1, QW, '*')
    term1 = crossprod(Z, Zw)
    QZw  = sweep(QZ, 1, puf_w, '*')
    term2 = crossprod(QZ, QZw)
    list(T_hat = T_hat, dTdL = term1 - term2)
  })()
  L_at = sum(((fwd$T_hat - target) / scale)^2)  # no ridge for FD check
  v_at = 2 * (fwd$T_hat - target) / scale^2
  grad_closed = as.numeric(fwd$dTdL %*% v_at)

  fd_eps = 1e-5
  grad_fd = numeric(M)
  for (p in 1:M) {
    lp = lambda_test; lm = lambda_test
    lp[p] = lp[p] + fd_eps
    lm[p] = lm[p] - fd_eps
    fp = (function(L) {
      tt = exp(as.numeric(Z %*% L))
      nn = sweep(W, 2, tt, '*')
      ss = rowSums(nn)
      QQ = nn / ss
      Th = as.numeric(crossprod(QQ %*% Z, puf_w))
      sum(((Th - target) / scale)^2)
    })(lp)
    fm = (function(L) {
      tt = exp(as.numeric(Z %*% L))
      nn = sweep(W, 2, tt, '*')
      ss = rowSums(nn)
      QQ = nn / ss
      Th = as.numeric(crossprod(QQ %*% Z, puf_w))
      sum(((Th - target) / scale)^2)
    })(lm)
    grad_fd[p] = (fp - fm) / (2 * fd_eps)
  }
  rel_err = max(abs(grad_closed - grad_fd) / (abs(grad_fd) + 1e-12))
  stopifnot(rel_err < 1e-3)
  cat(sprintf('  [PASS] closed-form vs FD gradient agrees (max rel err=%.2e)\n', rel_err))

  # ------ Test 4: Reproducibility (same inputs -> same lambda) -------------
  res_a = solve_tilt_block(W, Z, puf_w, target, scale)
  res_b = solve_tilt_block(W, Z, puf_w, target, scale)
  stopifnot(max(abs(res_a$lambda - res_b$lambda)) < 1e-12)
  cat('  [PASS] reproducible (deterministic given inputs)\n')

  # ------ Test 5: Infeasible target detected via attainable_bounds --------
  # All donor Z values are in [-1, 1]. Maximum implied aggregate per col is
  # sum(puf_w) * 1 = ~50. Set target = 1000 (way out of range).
  W_total = sum(puf_w)
  target_inf = c(1000, 1000, 1000)
  bounds = attainable_bounds(Z, puf_w)
  stopifnot(all(target_inf > bounds['max', ]))
  cat(sprintf('  [PASS] attainable_bounds flags infeasible target (max=%.2f, target=%.0f)\n',
              max(bounds['max',]), target_inf[1]))

  # ------ Test 6: multistart shape on the same problem --------------------
  # We don't expose a multistart wrapper at the moment because optim is
  # deterministic on the same init -- start with a random non-zero lambda
  # and verify it converges back to a comparable optimum.
  set.seed(11)
  L_starts = list(rep(0, M),
                   rnorm(M, sd = 0.5),
                   rnorm(M, sd = 1.0))
  vals = sapply(L_starts, function(L0) {
    r = solve_tilt_block(W, Z, puf_w, target, scale, init_lambda = L0,
                         max_iters = 500L, tol_rel = 1e-4)
    r$objective
  })
  # All starts should reach a similar objective (local minima in this
  # tiny problem are unlikely; problem is convex enough).
  stopifnot(diff(range(vals)) < 1e-4)
  cat(sprintf('  [PASS] multistart objectives close (range=%.2e)\n',
              diff(range(vals))))

  # ------ Test 7: Larger 2-cell synthetic, mimicking solve_tilt_global ----
  # We still feed W directly (no DRF) by stubbing get_sample_weights. Skip
  # this -- the global wrapper relies on drf::get_sample_weights. Tested
  # in the wealth.R end-to-end harness instead.
  cat('  [SKIP] solve_tilt_global integration test (requires drf object)\n')

  cat('\nAll tilt_solver.R block-level tests passed.\n')
}
