#---------------------------------------------------------------
# Unit test: tune_epsilon binary search correctness
#
# Tests that tune_epsilon properly narrows the search interval.
# The bug (l = test instead of low = test) prevents the lower
# bound from advancing on failure. This causes the search to
# converge more slowly because it always bisects from 0.
#
# With few iterations (as used in production, e_runs=10), this
# can cause tune_epsilon to find a suboptimal (too-loose) epsilon,
# meaning weights deviate more than necessary from observed values.
#---------------------------------------------------------------

library(lpSolveAPI)

# Source the reweighting code
source("src/reweight.R")

n_passed = 0
n_failed = 0

assert = function(desc, condition) {
  if (isTRUE(condition)) {
    cat(sprintf("  PASS: %s\n", desc))
    n_passed <<- n_passed + 1
  } else {
    cat(sprintf("  FAIL: %s\n", desc))
    n_failed <<- n_failed + 1
  }
}


# =============================================================
# Test: basic convergence
# =============================================================

cat("\nTest: tune_epsilon converges to correct value\n")

# x1 + x2 = 3, starting from 1 each -> optimal epsilon = 0.5
lp = make.lp(0, 2)
set.objfn(lp, c(1, 1))
add.constraint(lp, c(1, 1), "<=", 3)
add.constraint(lp, c(1, 1), ">=", 3)

result = tune_epsilon(lp, 2, 20)

assert("returns numeric", is.numeric(result) && length(result) == 1)
assert("epsilon in [0, 1]", result >= 0 && result <= 1)
assert(
  sprintf("epsilon ~0.5 with 20 iters (got %.6f)", result),
  abs(result - 0.5) < 0.001
)


# =============================================================
# Test: precision with limited iterations (production setting)
# =============================================================

cat("\nTest: precision with e_runs=10 (production default)\n")

# Optimal epsilon = 0.75. With only 10 iterations:
# - Correct binary search: precision ~1/2^10 = 0.001
# - Buggy search (low stuck at 0): bisects [0, high] each time,
#   converges but with lower precision
lp2 = make.lp(0, 2)
set.objfn(lp2, c(1, 1))
add.constraint(lp2, c(1, 1), "<=", 3.5)
add.constraint(lp2, c(1, 1), ">=", 3.5)

result_10 = tune_epsilon(lp2, 2, 10)

assert(
  sprintf("epsilon ~0.75 with 10 iters (got %.6f, within 0.005)", result_10),
  abs(result_10 - 0.75) < 0.005
)


# =============================================================
# Test: high optimal epsilon (where bug has most impact)
# =============================================================

cat("\nTest: high optimal epsilon (~0.9)\n")

# Optimal epsilon = 0.9. The bug hurts most here because the
# initial test (0.5) fails, and with low stuck at 0, the search
# keeps testing points between 0 and the current upper bound,
# wasting iterations re-exploring the infeasible region.
lp3 = make.lp(0, 2)
set.objfn(lp3, c(1, 1))
add.constraint(lp3, c(1, 1), "<=", 3.8)
add.constraint(lp3, c(1, 1), ">=", 3.8)

result_high = tune_epsilon(lp3, 2, 10)

assert(
  sprintf("epsilon ~0.9 with 10 iters (got %.6f, within 0.005)", result_high),
  abs(result_high - 0.9) < 0.005
)


# =============================================================
# Test: trivially feasible
# =============================================================

cat("\nTest: trivially feasible gives near-zero epsilon\n")

lp4 = make.lp(0, 2)
set.objfn(lp4, c(1, 1))
add.constraint(lp4, c(1, 1), "<=", 2)
add.constraint(lp4, c(1, 1), ">=", 2)

result_trivial = tune_epsilon(lp4, 2, 10)

assert(
  sprintf("epsilon ~0 (got %.6f)", result_trivial),
  result_trivial < 0.002
)


# =============================================================
# Summary
# =============================================================

cat(sprintf("\n%d passed, %d failed\n", n_passed, n_failed))
if (n_failed > 0) {
  stop(sprintf("%d test(s) failed", n_failed))
}
