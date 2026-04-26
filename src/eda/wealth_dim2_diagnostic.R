#---------------------------------------------
# wealth_dim2_diagnostic.R
#
# Disentangles the two sources of PUF-vs-SCF
# wealth gap by holding PUF out of the loop:
#
#   Dim 1 (X-shift) — feed SCF X (not PUF X)
#        into a trained DRF and see if SCF-
#        weighted aggregates match SCF truth.
#        If they do, the DRF is internally
#        self-consistent; any PUF gap is X-
#        shift-attributable.
#
#   Dim 2 (weights) — compare two weight-
#        handling schemes, each run with the
#        same SCF X inputs:
#
#     (a) population-scale bootstrap (n=250k,
#         prob ∝ w_i) + unweighted DRF +
#         uniform leaf draw at inference.
#     (b) unweighted DRF on raw SCF (n=29k) +
#         weight-proportional leaf draw at
#         inference. (CURRENT PRODUCTION.)
#
# Output: per-scheme aggregates (total NW,
# per-quintile NW, per-category totals, top
# 1% share) vs SCF truth, side-by-side.
#
# Usage:
#   Rscript src/eda/wealth_dim2_diagnostic.R
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(drf);   library(Hmisc)
})

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/wealth.R')   # defines scf_to_y (short-circuits if already collapsed)


#--- Load + prepare SCF ----------------------------------------------------

cat('Loading SCF tax units...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'age_younger', 'n_dep_hh',
             'pctile_wages', 'has_wages',
             'pctile_business', 'has_business_act', 'has_business_pos',
             'pctile_int_div', 'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act', 'has_capital_gains_pos',
             'pctile_ss_pens', 'has_ss_pens')

make_has = function(x) case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)

# Feature engineering — ported verbatim from wealth.R.
scf_tax_units = scf_tax_units %>%
  mutate(
    n_dep_hh    = n_dep,
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                          NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    has_income_act        = as.integer(income != 0),
    has_income_pos        = as.integer(income >  0),
    has_negative_income   = as.integer(income <  0),
    pctile_income         = compute_percentile(income, weight, FINE_PCTILE_PROBS),
    has_wages             = make_has(wages_scf),
    pctile_wages          = compute_percentile(wages_scf, weight, FINE_PCTILE_PROBS),
    has_business_act      = as.integer(business_scf != 0),
    has_business_pos      = as.integer(business_scf >  0),
    pctile_business       = compute_percentile(business_scf, weight, FINE_PCTILE_PROBS),
    has_int_div           = make_has(int_div_scf),
    pctile_int_div        = compute_percentile(int_div_scf, weight, FINE_PCTILE_PROBS),
    has_capital_gains_act = as.integer(capital_gains_scf != 0),
    has_capital_gains_pos = as.integer(capital_gains_scf >  0),
    pctile_capital_gains  = compute_percentile(capital_gains_scf, weight, FINE_PCTILE_PROBS),
    has_ss_pens           = make_has(ss_pens_scf),
    pctile_ss_pens        = compute_percentile(ss_pens_scf, weight, FINE_PCTILE_PROBS)
  )

# scf_to_y short-circuits when the 23 collapsed Y-vars are already present
# (which they are — stage1_scf_tax_units.R does the collapse upstream).
scf_y_df = scf_to_y(scf_tax_units)

X_scf = as.matrix(scf_tax_units[features])
Y_scf = as.matrix(scf_y_df[wealth_y_vars])
W_scf = scf_tax_units$weight
N_scf = nrow(X_scf)

# Income-rank bins for per-quintile reporting.
to_bin = function(pct) {
  cut(pct, breaks = c(-1, 19, 39, 59, 79, 89, 98, 100),
      labels = c('p0-20','p20-40','p40-60','p60-80',
                 'p80-90','p90-99','top1'),
      include.lowest = TRUE, right = TRUE)
}
scf_pct = compute_percentile(scf_tax_units$income, scf_tax_units$weight,
                              seq(0.01, 0.99, 0.01))
scf_bin = to_bin(scf_pct)

cat(sprintf('  %d SCF rows, total pop weight %.1fM, total NW truth $%.2fT\n',
            N_scf, sum(W_scf)/1e6,
            sum(W_scf * (rowSums(Y_scf[, wealth_asset_vars]) -
                         rowSums(Y_scf[, wealth_debt_vars]))) / 1e12))


#--- Forest-walking utilities ----------------------------------------------

extract_forest = function(drf_model) {
  list(
    n_trees = drf_model[['_num_trees']],
    root    = sapply(drf_model[['_root_nodes']], identity),
    child_L = lapply(drf_model[['_child_nodes']], `[[`, 1L),
    child_R = lapply(drf_model[['_child_nodes']], `[[`, 2L),
    split_v = drf_model[['_split_vars']],
    split_s = drf_model[['_split_values']],
    leaves  = drf_model[['_leaf_samples']]
  )
}

walk_to_leaf_one = function(f, t, x_row) {
  L = f$child_L[[t]]; R = f$child_R[[t]]
  v = f$split_v[[t]]; s = f$split_s[[t]]
  ll = f$leaves[[t]]
  node = f$root[t] + 1L
  repeat {
    if (length(ll[[node]]) > 0L) return(node)
    xv = x_row[v[node] + 1L]; sv = s[node]
    node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L else R[node] + 1L
  }
}

# For every row of X, walk a randomly-picked tree to a leaf, then draw one
# donor from that leaf. `donor_weights`, if non-NULL, makes the leaf draw
# proportional to those weights; NULL means uniform. Returns a vector of
# indices into the forest's training set (i.e. into _leaf_samples).
walk_and_sample = function(f, X, donor_weights = NULL, seed = 101) {
  set.seed(seed)
  n = nrow(X)
  tree_pick = sample.int(f$n_trees, size = n, replace = TRUE)
  picks = integer(n)
  for (i in seq_len(n)) {
    t  = tree_pick[i]
    nd = walk_to_leaf_one(f, t, X[i, ])
    lr = f$leaves[[t]][[nd]] + 1L
    p  = if (is.null(donor_weights)) NULL else donor_weights[lr]
    picks[i] = lr[sample.int(length(lr), 1L, prob = p)]
  }
  picks
}


#--- Aggregation report ----------------------------------------------------

aggregate_report = function(y_pred_mat, w, bin_vec, label) {
  nw = rowSums(y_pred_mat[, wealth_asset_vars]) -
       rowSums(y_pred_mat[, wealth_debt_vars])

  total = sum(w * nw) / 1e12

  cat_totals = sapply(wealth_y_vars, function(v) sum(w * y_pred_mat[, v]) / 1e12)

  q_nw_t    = tapply(w * nw, bin_vec, sum) / 1e12
  q_nw_mean = tapply(w * nw, bin_vec, sum) / tapply(w, bin_vec, sum)

  ord = order(nw, decreasing = TRUE)
  cw  = cumsum(w[ord])
  top1_mask = cw <= 0.01 * sum(w)
  top1_share = sum(w[ord][top1_mask] * nw[ord][top1_mask]) / sum(w * nw)
  top10_mask = cw <= 0.10 * sum(w)
  top10_share = sum(w[ord][top10_mask] * nw[ord][top10_mask]) / sum(w * nw)

  list(
    label       = label,
    total_nw    = total,
    cat_totals  = cat_totals,
    q_nw_t      = q_nw_t,
    q_nw_mean   = q_nw_mean,
    top1_share  = top1_share,
    top10_share = top10_share
  )
}

truth = aggregate_report(Y_scf, W_scf, scf_bin, 'SCF truth')


#--- Diagnostic A — option (b): cached forest + weight-prop leaf draw ------

cat('\n\n==============================================================\n')
cat('Diagnostic A: option (b) — unweighted DRF (raw SCF), weight-prop leaf draw\n')
cat('==============================================================\n\n')

cat('Loading cached forest resources/cache/qrf/wealth_drf.rds ...\n')
drf_b = read_rds('resources/cache/qrf/wealth_drf.rds')
f_b   = extract_forest(drf_b)
cat(sprintf('  %d trees; median leaf size = %d\n',
            f_b$n_trees,
            median(unlist(lapply(f_b$leaves, function(ll)
              lengths(ll)[lengths(ll) > 0L])))))

# For option (b), the forest was trained directly on scf_tax_units, so
# _leaf_samples are indices into scf_tax_units. donor_weights = W_scf.
cat(sprintf('Walking %d SCF rows with weight-prop sampling ...\n', N_scf))
t0 = Sys.time()
picks_b = walk_and_sample(f_b, X_scf, donor_weights = W_scf, seed = 101)
cat(sprintf('  %.1fs\n', as.numeric(Sys.time() - t0, units = 'secs')))
Y_hat_b  = Y_scf[picks_b, , drop = FALSE]
report_b = aggregate_report(Y_hat_b, W_scf, scf_bin,
                             'option (b) on SCF X, weight-prop leaf')


#--- Diagnostic B — option (a): weighted bootstrap + uniform leaf draw -----

cat('\n\n==============================================================\n')
cat('Diagnostic B: option (a) — weighted bootstrap + unweighted DRF + uniform leaf\n')
cat('==============================================================\n\n')

# Bootstrap N=250k rows from SCF prop to w_i. Matches the scheme wealth.R
# used before the uniform-training switch.
n_boot = 250000L
set.seed(200)
boot_idx = sample.int(N_scf, size = n_boot, replace = TRUE, prob = W_scf)

X_boot = X_scf[boot_idx, , drop = FALSE]
Y_boot = Y_scf[boot_idx, , drop = FALSE]

cat(sprintf('Bootstrap: %d rows (unique SCF rows sampled: %d of %d)\n',
            n_boot, length(unique(boot_idx)), N_scf))

# Quick diagnostic on missing donors — how many of the top-NW SCF rows
# never appear in the bootstrap?
scf_nw_truth = rowSums(Y_scf[, wealth_asset_vars]) -
               rowSums(Y_scf[, wealth_debt_vars])
top50 = order(scf_nw_truth, decreasing = TRUE)[1:50]
missing_top50 = sum(!(top50 %in% boot_idx))
cat(sprintf('  top-50 highest-NW SCF rows missing from bootstrap: %d\n',
            missing_top50))

cat(sprintf('Training new DRF on %d bootstrap rows (this is the slow step)...\n',
            n_boot))
estimate_models = 1L
t0 = Sys.time()
drf_a = train_or_load_drf(
  name          = 'wealth_drf_dim2_optionA',
  X             = X_boot,
  Y             = Y_boot,
  num.features  = 50,
  min.node.size = 5
)
cat(sprintf('  train+cache %.1fs\n', as.numeric(Sys.time() - t0, units = 'secs')))
f_a = extract_forest(drf_a)
cat(sprintf('  %d trees; median leaf size = %d\n',
            f_a$n_trees,
            median(unlist(lapply(f_a$leaves, function(ll)
              lengths(ll)[lengths(ll) > 0L])))))

cat(sprintf('Walking %d SCF rows with uniform sampling ...\n', N_scf))
t0 = Sys.time()
picks_a_boot = walk_and_sample(f_a, X_scf, donor_weights = NULL, seed = 201)
cat(sprintf('  %.1fs\n', as.numeric(Sys.time() - t0, units = 'secs')))

# picks_a_boot indexes into BOOT rows. Map back to original SCF rows.
picks_a_scf = boot_idx[picks_a_boot]
Y_hat_a     = Y_scf[picks_a_scf, , drop = FALSE]
report_a    = aggregate_report(Y_hat_a, W_scf, scf_bin,
                                'option (a) on SCF X, uniform leaf')


#--- Side-by-side ----------------------------------------------------------

cat('\n\n==============================================================\n')
cat('SIDE-BY-SIDE COMPARISON\n')
cat('==============================================================\n\n')

pctd = function(num, ref) if (ref == 0) NA_real_ else 100 * (num - ref) / ref

cat('Total NW ($T):\n')
cat(sprintf('  SCF truth:    %7.2f\n', truth$total_nw))
cat(sprintf('  option (a):   %7.2f  (%+.1f%%)\n',
            report_a$total_nw, pctd(report_a$total_nw, truth$total_nw)))
cat(sprintf('  option (b):   %7.2f  (%+.1f%%)\n',
            report_b$total_nw, pctd(report_b$total_nw, truth$total_nw)))

cat('\nNW ($T) by SCF income rank bin:\n')
q_tbl = tibble(
  bin  = names(truth$q_nw_t),
  scf  = as.numeric(truth$q_nw_t),
  opta = as.numeric(report_a$q_nw_t),
  optb = as.numeric(report_b$q_nw_t)
) %>%
  mutate(
    opta_pct = round(100 * (opta - scf) / pmax(abs(scf), 1e-6), 1),
    optb_pct = round(100 * (optb - scf) / pmax(abs(scf), 1e-6), 1),
    scf      = round(scf, 2),
    opta     = round(opta, 2),
    optb     = round(optb, 2)
  )
print(q_tbl, n = Inf)

cat('\nPer-category totals ($T) [first 13 = assets, next 6 = debts, last 4 = kg]:\n')
cat_tbl = tibble(
  category = wealth_y_vars,
  scf      = as.numeric(truth$cat_totals),
  opta     = as.numeric(report_a$cat_totals),
  optb     = as.numeric(report_b$cat_totals)
) %>%
  mutate(
    opta_pct = round(100 * (opta - scf) / pmax(abs(scf), 1e-6), 1),
    optb_pct = round(100 * (optb - scf) / pmax(abs(scf), 1e-6), 1),
    scf      = round(scf, 3),
    opta     = round(opta, 3),
    optb     = round(optb, 3)
  )
print(cat_tbl, n = Inf)

cat('\nConcentration:\n')
cat(sprintf('  SCF truth:    top1 %5.3f   top10 %5.3f\n',
            truth$top1_share, truth$top10_share))
cat(sprintf('  option (a):   top1 %5.3f   top10 %5.3f\n',
            report_a$top1_share, report_a$top10_share))
cat(sprintf('  option (b):   top1 %5.3f   top10 %5.3f\n',
            report_b$top1_share, report_b$top10_share))


#--- Donor-reachability sanity check ---------------------------------------

cat('\n\n==============================================================\n')
cat('Donor reachability: ultra-wealthy SCF rows under each scheme\n')
cat('==============================================================\n\n')

top15 = order(scf_nw_truth, decreasing = TRUE)[1:15]
a_hits = sapply(top15, function(r) sum(picks_a_scf == r))
b_hits = sapply(top15, function(r) sum(picks_b == r))
reach = tibble(
  scf_row        = top15,
  scf_nw_B       = round(scf_nw_truth[top15] / 1e9, 2),
  scf_weight     = round(W_scf[top15]),
  in_boot        = as.integer(top15 %in% boot_idx),
  picks_option_a = a_hits,
  picks_option_b = b_hits
)
print(reach, n = Inf)

cat('\nDone.\n')
