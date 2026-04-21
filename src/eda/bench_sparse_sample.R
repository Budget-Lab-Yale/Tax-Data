#--------------------------------------
# bench_sparse_sample.R
#
# Benchmark and equivalence test for the
# proposed "pick one tree, walk, sample
# uniformly from the leaf" donor-sampling
# method vs the current
# apply(W, 1, sample.int(prob=...)).
#
# Uses the cached wealth_drf and the
# Y_mat / X_mat written by the last
# test_wealth.R run.
#
# Outputs:
#   - timing for each method on a common
#     test subset
#   - a QQ check that per-category
#     marginal draw distributions match
#--------------------------------------

suppressMessages({
  library(drf)
  library(tidyverse)
  library(data.table)
})

set.seed(76)

cat('Loading cached wealth_drf (~940 MB)...\n')
t0 = Sys.time()
m = readRDS('resources/cache/qrf/wealth_drf.rds')
cat(sprintf('  loaded in %.1f s\n', as.numeric(Sys.time() - t0, units = 'secs')))

cat('Loading Y_mat + X_mat from wealth_diagnostics.rds...\n')
t0 = Sys.time()
diag = readRDS('resources/cache/wealth_diagnostics.rds')
Y_mat = diag$Y_mat
X_mat = diag$X_mat
features = diag$features
y_vars = diag$y_vars
cat(sprintf('  loaded in %.1f s\n', as.numeric(Sys.time() - t0, units = 'secs')))
cat(sprintf('  Y_mat: %d x %d  |  X_mat: %d x %d\n',
            nrow(Y_mat), ncol(Y_mat), nrow(X_mat), ncol(X_mat)))

# Pull forest fields once (avoid repeated `m[['_xxx']]` list lookups inside loops)
n_trees = m[['_num_trees']]
root    = sapply(m[['_root_nodes']], identity)              # int vector, len=T
child_L = m[['_child_nodes']]                               # list: [[t]][[1]] = left children
child_R = child_L
for (t in seq_len(n_trees)) child_R[[t]] = m[['_child_nodes']][[t]][[2]]
for (t in seq_len(n_trees)) child_L[[t]] = m[['_child_nodes']][[t]][[1]]
split_v = m[['_split_vars']]                                # list: [[t]] = vec<int>
split_s = m[['_split_values']]                              # list: [[t]] = vec<double>
leaves  = m[['_leaf_samples']]                              # list: [[t]][[node]] = int vec

cat(sprintf('Forest summary: %d trees loaded\n', n_trees))

#---------------------------------------------------------------------------
# Build a test subset of PUF-like rows. Use X_mat (the training features
# themselves) so both methods see valid X. Take a sample of 1000 rows.
#---------------------------------------------------------------------------

n_test = 200L
# Drop any rows with NA in X (X_mat inherits NAs from upstream, e.g. male1
# NA on NPEU tax-unit rows). drf's C++ predictor handles NAs internally
# but our pure-R walker doesn't mimic that, so use an NA-free sample for
# the benchmark. The core speedup argument doesn't depend on how NAs are
# handled.
X_clean_idx = which(rowSums(is.na(X_mat)) == 0)
cat(sprintf('X_mat: %d rows total, %d NA-free\n',
            nrow(X_mat), length(X_clean_idx)))
test_idx = sample(X_clean_idx, n_test)
X_test   = X_mat[test_idx, , drop = FALSE]
cat(sprintf('Test subset: %d rows (NA-free)\n', n_test))

#---------------------------------------------------------------------------
# Method A (current): get_sample_weights -> apply(W, 1, sample.int)
#---------------------------------------------------------------------------

cat('\n=== Method A: current (dense W + apply+sample.int) ===\n')
t0 = Sys.time()
W_A = drf::get_sample_weights(m, newdata = X_test)
t_wA = as.numeric(Sys.time() - t0, units = 'secs')
cat(sprintf('  get_sample_weights: %.2f s (W is %d x %d)\n', t_wA, nrow(W_A), ncol(W_A)))

t0 = Sys.time()
donors_A = apply(W_A, 1, function(p) sample.int(nrow(Y_mat), size = 1, prob = p))
t_sA = as.numeric(Sys.time() - t0, units = 'secs')
cat(sprintf('  apply+sample.int:    %.2f s\n', t_sA))
cat(sprintf('  A total:             %.2f s\n', t_wA + t_sA))

#---------------------------------------------------------------------------
# Method B (proposed): pick one tree per row, walk, sample from leaf
#---------------------------------------------------------------------------

walk_to_leaf = function(t, x_row) {
  L = child_L[[t]]; R = child_R[[t]]
  v = split_v[[t]]; s = split_s[[t]]
  leaf_list = leaves[[t]]
  node = root[t] + 1L
  repeat {
    # Ground-truth leaf check: leaf_samples has a non-empty vector at leaves,
    # empty at internal nodes. Avoids NA traps in s[node] / L,R boundaries.
    if (length(leaf_list[[node]]) > 0L) return(node)
    sv = s[node]
    xv = x_row[v[node] + 1L]
    if (is.na(xv) || is.na(sv)) {
      # Fallback: treat as leaf if anything is NA (shouldn't reach here).
      return(node)
    }
    node = if (xv <= sv) L[node] + 1L else R[node] + 1L
  }
}

draw_one = function(i) {
  t = sample.int(n_trees, 1L)
  nd = walk_to_leaf(t, X_test[i, ])
  leaf = leaves[[t]][[nd]] + 1L
  as.integer(leaf[sample.int(length(leaf), 1L)])
}

cat('\n=== Method B: proposed (one tree per row, leaf sample) ===\n')
t0 = Sys.time()
donors_B = vapply(seq_len(n_test), draw_one, integer(1L))
t_B = as.numeric(Sys.time() - t0, units = 'secs')
cat(sprintf('  total:               %.2f s\n', t_B))

cat(sprintf('\nSpeedup (A_total / B_total): %.1fx   (single-row, n_test=%d)\n',
            (t_wA + t_sA) / t_B, n_test))

#---------------------------------------------------------------------------
# Distributional equivalence check
#
# Both methods sample donors ~ Categorical(W[i, :]) per prediction row in
# expectation. Over many replicates of many rows, the marginal distribution
# of Y[donor, k] for each category k should match between methods.
#
# We repeat each method R times on the same test subset and compare
# per-category weighted quantile profiles.
#---------------------------------------------------------------------------

R = 10L
cat(sprintf('\nEquivalence test: running each method %d times on the same %d rows\n',
            R, n_test))

collect = function(method) {
  out = matrix(0, nrow = n_test * R, ncol = length(y_vars),
               dimnames = list(NULL, y_vars))
  row_cursor = 1L
  for (rep in seq_len(R)) {
    if (method == 'A') {
      donors = apply(W_A, 1, function(p) sample.int(nrow(Y_mat), size = 1, prob = p))
    } else {
      donors = vapply(seq_len(n_test), draw_one, integer(1L))
    }
    out[row_cursor:(row_cursor + n_test - 1L), ] = Y_mat[donors, , drop = FALSE]
    row_cursor = row_cursor + n_test
  }
  out
}

cat('  Method A replicates...\n')
YA = collect('A')
cat('  Method B replicates...\n')
YB = collect('B')

# Per-category P50/P90/P99 — should line up
probs = c(0.5, 0.9, 0.99)
qA = apply(YA, 2, quantile, probs = probs, na.rm = TRUE)
qB = apply(YB, 2, quantile, probs = probs, na.rm = TRUE)

cat('\n  Per-category quantiles (A vs B):\n')
cat(sprintf('  %-20s | %12s %12s %12s | %12s %12s %12s\n',
            'variable', 'A:P50', 'A:P90', 'A:P99', 'B:P50', 'B:P90', 'B:P99'))
cat(paste0(rep('-', 110), collapse = ''), '\n')
for (k in seq_along(y_vars)) {
  cat(sprintf('  %-20s | %12.0f %12.0f %12.0f | %12.0f %12.0f %12.0f\n',
              y_vars[k],
              qA[1, k], qA[2, k], qA[3, k],
              qB[1, k], qB[2, k], qB[3, k]))
}

#---------------------------------------------------------------------------
# KS test per category for a tighter equivalence check
#---------------------------------------------------------------------------

cat('\n  KS test per category (D statistic, should be small if equivalent):\n')
for (k in seq_along(y_vars)) {
  ks = suppressWarnings(ks.test(YA[, k], YB[, k]))
  flag = if (ks$p.value < 0.01) ' <-- DIVERGES' else ''
  cat(sprintf('  %-20s  D = %.4f   p = %.3f%s\n',
              y_vars[k], as.numeric(ks$statistic), ks$p.value, flag))
}

cat('\nDone.\n')
