#---------------------------------------------
# swap_solver.R
#
# Stage 3 Step A as a swap-based donor
# reassignment problem.
#
# For one (cell_income × cell_age) bucket of
# PUF records, each record starts with a uniform
# random donor from its DRF leaf. We then
# repeatedly propose (record, new_donor) swaps
# and accept any swap that reduces the bucket-
# global count error
#
#   global_err(count) = Σ_k |count_k − target_k|
#                       / max(target_k, 1)
#
# where count_k = Σ_i puf_w[i] * (cat_k(donor_i) > 0).
#
# Exits with one of three statuses:
#   - "converged" : every category within tol_rel
#   - "stuck"     : n_trials_stuck consecutive
#                   failed proposals (leaf donor
#                   pool is the binding constraint)
#   - "cap"       : max_iters proposals tried
#
# Why swap (not tilt):
#   - Hits per-category counts EXACTLY when
#     feasible. Tilt's exp-tilt over leaf donors
#     hits a soft target and collapses ESS when λ
#     has to be extreme.
#   - Preserves record-level joint Y consistency
#     by always assigning a whole donor row (no
#     Frankenstein records, unlike trim).
#   - Decomposes by (cell × age) — leaves contain
#     only same-cell donors (per-cell forest), so
#     a swap of record i changes counts only in
#     (cell_i, age_i, *).
#
# Background: src/eda/training_reweight_1d_test.R
# established that Y-conditional reweighting at
# training time moves the intensive aggregate but
# not the extensive count, because tree splits
# in the top cell don't cleanly stratify by age.
# Swap-based reassignment IS Y-conditional but
# operates on hard assignments, not smooth
# probabilities, so it's not constrained by ESS.
#---------------------------------------------


#' Solve Stage 3 Step A by random improving swaps within a single
#' (cell_income × cell_age) bucket.
#'
#' @param puf_weights              Numeric, length n. Weight per record.
#' @param leaf_donors_per_record   List of length n. Each element is an integer
#'                                  vector of donor indices (into the rows of
#'                                  cat_pos_matrix) eligible for that record.
#'                                  Always equals the record's per-cell DRF
#'                                  leaf, so all candidates are same-cell.
#' @param cat_pos_matrix           Integer matrix, donors_total × K, with
#'                                  cat_pos_matrix[d, k] = 1 if donor d has
#'                                  category k value > 0 else 0. Precomputed
#'                                  once at the wealth.R level. Column names
#'                                  are category names (e.g., 'equities').
#' @param target_counts            Named numeric, length K. SCF weighted count
#'                                  targets for this bucket. Names must match
#'                                  colnames(cat_pos_matrix).
#' @param init_donors              Integer, length n. Starting donor assignment
#'                                  (uniform random pick from each record's
#'                                  leaf is the default initializer). Required:
#'                                  must already be drawn before calling.
#' @param max_iters                Integer. Cap on proposal attempts. Default
#'                                  500000 (~10s for K=6, n=30000).
#' @param n_trials_stuck           Integer. Declare "stuck" after this many
#'                                  consecutive non-improving proposals.
#'                                  Default 5000.
#' @param tol_rel                  Numeric. Per-category relative-miss tolerance
#'                                  for "converged" status. Default 0.05.
#' @param init_seed                Integer. RNG seed for proposal generation.
#'                                  Per-bucket seeds make the run bit-identical
#'                                  even when buckets run in parallel.
#'
#' @return list(
#'   donor_assignment, final_counts, final_residuals, final_max_rel,
#'   final_global_err, swaps_accepted, total_proposals, iters_used, status
#' )
solve_swap_bucket = function(puf_weights,
                              leaf_donors_per_record,
                              cat_pos_matrix,
                              target_counts,
                              init_donors,
                              max_iters         = 500000L,
                              n_trials_stuck    = 5000L,
                              tol_rel           = 0.05,
                              init_seed         = 42L,
                              # ---- variant knobs ----
                              objective         = 'l1',     # 'l1' or 'l2'
                              anneal_T0         = 0,        # 0 = no anneal
                              anneal_iters      = 0L,       # decay window
                              proposal_strategy = 'uniform' # 'uniform' or 'guided'
                              ) {

  n = length(puf_weights)
  stopifnot(length(leaf_donors_per_record) == n,
            length(init_donors) == n,
            is.matrix(cat_pos_matrix))

  K = ncol(cat_pos_matrix)
  cat_names = colnames(cat_pos_matrix)
  stopifnot(!is.null(cat_names),
            length(target_counts) == K,
            identical(names(target_counts), cat_names),
            objective %in% c('l1', 'l2'),
            proposal_strategy %in% c('uniform', 'guided'),
            anneal_T0 >= 0, anneal_iters >= 0L)

  # ---------- per-bucket RNG state ----------
  # Don't touch the global RNG: capture, set, restore on exit. This makes the
  # solver bit-reproducible regardless of caller's RNG state and safe under
  # mclapply-style parallelism.
  if (exists('.Random.seed', envir = .GlobalEnv)) {
    saved_seed = .GlobalEnv$.Random.seed
    on.exit(assign('.Random.seed', saved_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(list = '.Random.seed', envir = .GlobalEnv), add = TRUE)
  }
  set.seed(init_seed)

  # ---------- initial state ----------
  current = as.integer(init_donors)
  pos_at_current = cat_pos_matrix[current, , drop = FALSE]
  count = as.numeric(crossprod(pos_at_current, puf_weights))
  names(count) = cat_names

  denom = pmax(target_counts, 1)

  # Objective: 'l1' = Σ |count_k − target_k| / denom_k (current default,
  # piecewise-linear, can plateau). 'l2' = Σ ((count_k − target_k)/denom_k)^2
  # (smooth gradient everywhere, cleaner descent on stochastic moves).
  err_fn = if (objective == 'l1') {
    function(cnt) sum(abs(cnt - target_counts) / denom)
  } else {
    function(cnt) sum(((cnt - target_counts) / denom)^2)
  }

  global_err = err_fn(count)
  max_rel    = max(abs(count - target_counts) / denom)

  # Pre-screen records that cannot move (single-donor leaves). Drawing from
  # these wastes proposal budget, so we sample from "movable" records only.
  ll = lengths(leaf_donors_per_record)
  movable_records = which(ll >= 2L)
  if (length(movable_records) == 0L) {
    return(list(
      donor_assignment   = current,
      final_counts       = count,
      final_residuals    = count - target_counts,
      final_max_rel      = max_rel,
      final_global_err   = global_err,
      swaps_accepted     = 0L,
      total_proposals    = 0L,
      iters_used         = 0L,
      status             = if (max_rel < tol_rel) 'converged' else 'stuck'
    ))
  }

  # Quick out: already converged from initial uniform draw.
  if (max_rel < tol_rel) {
    return(list(
      donor_assignment   = current,
      final_counts       = count,
      final_residuals    = count - target_counts,
      final_max_rel      = max_rel,
      final_global_err   = global_err,
      swaps_accepted     = 0L,
      total_proposals    = 0L,
      iters_used         = 0L,
      status             = 'converged'
    ))
  }

  # ---------- main proposal loop ----------
  trial_streak    = 0L
  swaps_accepted  = 0L
  swaps_anneal    = 0L     # uphill swaps accepted under annealing
  status          = 'cap'
  iters_used      = 0L

  # Best-seen state, in case annealing ever wanders to a worse final state.
  # Without this, anneal can degrade results when T cools too slowly.
  best_donors    = current
  best_count     = count
  best_global    = global_err
  best_max_rel   = max_rel

  # Helpers to support guided record-selection: when proposal_strategy
  # is 'guided', we want to sample a record currently positive in the
  # most-over-targeted category (or negative in the most-under-targeted).
  # Maintaining a record-by-cat positivity index would be expensive to
  # update on every swap; simpler: precompute movable indices and at
  # each iter, pick the worst-miss category, then linearly find candidates.
  use_guided = (proposal_strategy == 'guided')

  for (iter in seq_len(max_iters)) {
    iters_used = iter

    # ---------- propose (i, new_d) ----------
    if (use_guided) {
      # Find the category with the largest signed relative miss.
      rel = (count - target_counts) / denom
      kw  = which.max(abs(rel))
      over = rel[kw] > 0   # over-target → need to drop this cat from a record
                            # under-target → need to add this cat to a record

      # Sample movable records currently of the WRONG polarity in cat kw.
      # (over: pos at current donor; under: neg at current donor.) Then
      # propose a leaf donor of the OPPOSITE polarity.
      cur_pos = cat_pos_matrix[cbind(current[movable_records], kw)]
      target_polarity_at_record = if (over) 1L else 0L
      eligible_loc = which(cur_pos == target_polarity_at_record)
      if (length(eligible_loc) == 0L) {
        # Fallback: uniform proposal.
        i_loc = sample.int(length(movable_records), 1L)
      } else {
        i_loc = eligible_loc[sample.int(length(eligible_loc), 1L)]
      }
      i = movable_records[i_loc]
      cands = leaf_donors_per_record[[i]]
      cand_pos_kw = cat_pos_matrix[cands, kw]
      desired = if (over) 0L else 1L
      desired_cands = cands[cand_pos_kw == desired]
      if (length(desired_cands) == 0L) {
        # Leaf has no donors of the helpful polarity — uniform fallback.
        new_d = cands[sample.int(length(cands), 1L)]
      } else {
        new_d = desired_cands[sample.int(length(desired_cands), 1L)]
      }
    } else {
      i_loc = sample.int(length(movable_records), 1L)
      i     = movable_records[i_loc]
      cands = leaf_donors_per_record[[i]]
      new_d = cands[sample.int(length(cands), 1L)]
    }

    old_d = current[i]
    if (new_d == old_d) {
      trial_streak = trial_streak + 1L
      if (trial_streak >= n_trials_stuck) { status = 'stuck'; break }
      next
    }

    diff_pos = cat_pos_matrix[new_d, ] - cat_pos_matrix[old_d, ]
    if (all(diff_pos == 0L)) {
      trial_streak = trial_streak + 1L
      if (trial_streak >= n_trials_stuck) { status = 'stuck'; break }
      next
    }

    new_count = count + puf_weights[i] * diff_pos
    new_err   = err_fn(new_count)

    # ---------- accept / reject ----------
    accept = FALSE
    if (new_err < global_err) {
      accept = TRUE
    } else if (anneal_T0 > 0 && iter <= anneal_iters) {
      # Linear cooldown to T=0 at iter==anneal_iters. Accept worsening
      # swaps with prob exp(−Δerr / T). Δerr is small for L1 (≈ row-error
      # contribution); using the same denom-normalized scale as the
      # objective so T0 has consistent meaning across buckets.
      T_now = anneal_T0 * (1 - iter / anneal_iters)
      if (T_now > 0) {
        prob = exp(-(new_err - global_err) / T_now)
        if (runif(1) < prob) {
          accept = TRUE
          swaps_anneal = swaps_anneal + 1L
        }
      }
    }

    if (accept) {
      current[i]      = new_d
      count           = new_count
      global_err      = new_err
      swaps_accepted  = swaps_accepted + 1L
      trial_streak    = 0L
      max_rel         = max(abs(count - target_counts) / denom)

      if (global_err < best_global) {
        best_donors  = current
        best_count   = count
        best_global  = global_err
        best_max_rel = max_rel
      }

      if (max_rel < tol_rel) { status = 'converged'; break }
    } else {
      trial_streak = trial_streak + 1L
      if (trial_streak >= n_trials_stuck) { status = 'stuck'; break }
    }
  }

  # Restore best-seen state (anneal can otherwise leave us at a worse
  # state than we passed through).
  if (best_global < global_err) {
    current    = best_donors
    count      = best_count
    global_err = best_global
    max_rel    = best_max_rel
  }

  list(
    donor_assignment   = current,
    final_counts       = count,
    final_residuals    = count - target_counts,
    final_max_rel      = max_rel,
    final_global_err   = global_err,
    swaps_accepted     = swaps_accepted,
    swaps_anneal       = swaps_anneal,
    total_proposals    = iters_used,
    iters_used         = iters_used,
    status             = status
  )
}


#' Run solve_swap_bucket from N independent random seeds and return the
#' assignment with the lowest final_global_err. Tests whether the solver
#' is getting stuck in local minima vs hitting a true feasibility wall.
#'
#' @param n_restarts  Integer. Number of independent runs.
#' @param ...         Forwarded to solve_swap_bucket. `init_seed` is
#'                    overridden — the function uses init_seed,
#'                    init_seed+1, ..., init_seed+n_restarts-1.
solve_swap_bucket_multistart = function(n_restarts = 5L,
                                         init_seed   = 42L,
                                         ...) {
  best = NULL
  per_run_global_err = numeric(n_restarts)
  for (r in seq_len(n_restarts)) {
    res = solve_swap_bucket(init_seed = init_seed + r - 1L, ...)
    per_run_global_err[r] = res$final_global_err
    if (is.null(best) || res$final_global_err < best$final_global_err) {
      best = res
    }
  }
  best$multistart_n_runs       = n_restarts
  best$multistart_per_run_err  = per_run_global_err
  best
}


#---------------------------------------------------------------------------
# Standalone tests. Run with:
#   Rscript src/imputations/swap_solver.R
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  cat('--- swap_solver.R tests ---\n')

  # Synthetic 3-record × 5-donor × 2-category problem with a known optimum.
  # cat_pos_matrix:
  #   donor 1: (0, 0)    donor 2: (1, 0)
  #   donor 3: (0, 1)    donor 4: (1, 1)    donor 5: (0, 0)
  # All 3 records have full leaf {1..5}; weights = 1 each.
  # Target: count_a = 2, count_b = 1
  # Optimal: pick {2, 3, 1} or {4, 1, 1} or any combo summing to (2, 1).
  cat_pos = matrix(c(
    0L, 0L,
    1L, 0L,
    0L, 1L,
    1L, 1L,
    0L, 0L
  ), ncol = 2, byrow = TRUE)
  colnames(cat_pos) = c('a', 'b')

  res = solve_swap_bucket(
    puf_weights            = rep(1.0, 3),
    leaf_donors_per_record = list(1:5, 1:5, 1:5),
    cat_pos_matrix         = cat_pos,
    target_counts          = c(a = 2, b = 1),
    init_donors            = c(5L, 5L, 5L),       # all (0,0) — far from target
    max_iters              = 1000L,
    n_trials_stuck         = 200L,
    tol_rel                = 1e-6,
    init_seed              = 1L
  )
  stopifnot(res$status == 'converged')
  stopifnot(res$final_counts['a'] == 2)
  stopifnot(res$final_counts['b'] == 1)
  cat('  [PASS] toy 3×5×2 converges to exact target\n')

  # Stuck test: leaves don't contain any donor with category 'b' — infeasible.
  # Each record's leaf is restricted to donors {1, 2, 5} (none have b > 0).
  res_stuck = solve_swap_bucket(
    puf_weights            = rep(1.0, 3),
    leaf_donors_per_record = list(c(1L, 2L, 5L), c(1L, 2L, 5L), c(1L, 2L, 5L)),
    cat_pos_matrix         = cat_pos,
    target_counts          = c(a = 2, b = 1),
    init_donors            = c(5L, 5L, 5L),
    max_iters              = 1000L,
    n_trials_stuck         = 100L,
    tol_rel                = 1e-6,
    init_seed              = 2L
  )
  stopifnot(res_stuck$status == 'stuck')
  stopifnot(res_stuck$final_counts['b'] == 0)   # can't reach b
  cat('  [PASS] infeasible problem reports stuck status\n')

  # Reproducibility: two calls with the same seed must produce identical
  # assignments even when the global RNG was advanced between them.
  set.seed(99)
  r1 = solve_swap_bucket(rep(1, 3), list(1:5, 1:5, 1:5), cat_pos,
                         c(a = 2, b = 1), c(5L, 5L, 5L), init_seed = 7L)
  runif(100)   # advance global RNG
  r2 = solve_swap_bucket(rep(1, 3), list(1:5, 1:5, 1:5), cat_pos,
                         c(a = 2, b = 1), c(5L, 5L, 5L), init_seed = 7L)
  stopifnot(identical(r1$donor_assignment, r2$donor_assignment))
  cat('  [PASS] per-bucket seed gives bit-identical result\n')

  # Already-converged shortcut: init that already matches target should
  # return immediately.
  init_match = c(2L, 4L, 1L)   # (1,0)+(1,1)+(0,0) = (2,1) — exact
  r3 = solve_swap_bucket(rep(1, 3), list(1:5, 1:5, 1:5), cat_pos,
                         c(a = 2, b = 1), init_match, init_seed = 3L)
  stopifnot(r3$status == 'converged')
  stopifnot(r3$swaps_accepted == 0L)
  stopifnot(identical(r3$donor_assignment, init_match))
  cat('  [PASS] already-converged init exits with 0 swaps\n')

  # L2 objective converges on the same toy problem.
  res_l2 = solve_swap_bucket(
    puf_weights            = rep(1.0, 3),
    leaf_donors_per_record = list(1:5, 1:5, 1:5),
    cat_pos_matrix         = cat_pos,
    target_counts          = c(a = 2, b = 1),
    init_donors            = c(5L, 5L, 5L),
    objective              = 'l2',
    max_iters              = 1000L, n_trials_stuck = 200L,
    tol_rel = 1e-6, init_seed = 11L
  )
  stopifnot(res_l2$status == 'converged',
            res_l2$final_counts['a'] == 2,
            res_l2$final_counts['b'] == 1)
  cat('  [PASS] L2 objective converges on toy problem\n')

  # Guided proposals converge on the same toy problem.
  res_g = solve_swap_bucket(
    puf_weights            = rep(1.0, 3),
    leaf_donors_per_record = list(1:5, 1:5, 1:5),
    cat_pos_matrix         = cat_pos,
    target_counts          = c(a = 2, b = 1),
    init_donors            = c(5L, 5L, 5L),
    proposal_strategy      = 'guided',
    max_iters              = 1000L, n_trials_stuck = 200L,
    tol_rel = 1e-6, init_seed = 12L
  )
  stopifnot(res_g$status == 'converged')
  cat('  [PASS] guided proposals converge on toy problem\n')

  # Multistart returns no worse than single-shot best.
  res_ms = solve_swap_bucket_multistart(
    n_restarts             = 3L,
    puf_weights            = rep(1.0, 3),
    leaf_donors_per_record = list(1:5, 1:5, 1:5),
    cat_pos_matrix         = cat_pos,
    target_counts          = c(a = 2, b = 1),
    init_donors            = c(5L, 5L, 5L),
    max_iters              = 1000L, n_trials_stuck = 200L,
    tol_rel = 1e-6, init_seed = 13L
  )
  stopifnot(res_ms$multistart_n_runs == 3L,
            res_ms$final_global_err == min(res_ms$multistart_per_run_err))
  cat('  [PASS] multistart returns best of N runs\n')

  cat('\nAll swap_solver.R tests passed.\n')
}
