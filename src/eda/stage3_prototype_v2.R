#--------------------------------------
# stage3_prototype_v2.R
#
# v2 changes vs v1:
# - Feature rescaling: normalize each f_k by sd(f_k over donors) so Newton
#   sees comparable magnitudes per target dim. Probabilities are invariant
#   under per-col rescale (exp(lam^T f) = exp((lam*s)^T (f/s))), only lambda
#   units change.
# - Max iters 40 -> 80.
# - More aggressive Hessian regularization (1e-6 relative).
# - Per-iter log line (first 5 iters) for diagnostic visibility.
# - Report scaled lambda AND unscaled lambda for interpretability.
#--------------------------------------

cat('\n====== stage3_prototype_v2 ======\n\n')

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')
estimate_models = 1
do_lp = 0

# Seeds. Match test_wealth_X_ablation.R so the DRF cache hit is deterministic.
TRAIN_SEED      = 1337L
DONOR_SEED      = 1337L
TILT_DONOR_SEED = 42L
set.seed(TRAIN_SEED)

#---------------------------------------------------------------------------
# Pipeline up to demographics + ages (mirrors test_wealth_X_ablation.R).
#---------------------------------------------------------------------------

source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
source('./src/impute_nonfilers.R')

source('./src/imputations/helpers.R')
source('./src/imputations/demographics.R')
source('./src/imputations/ages.R')

source('./src/imputations/stage1_scf_tax_units.R')

stopifnot(exists('scf_tax_units'))
stopifnot(all(c('wages_scf', 'business_scf', 'int_div_scf',
                'capital_gains_scf', 'rent_scf',
                'ss_pens_scf', 'ui_other_scf') %in% names(scf_tax_units)))

#---------------------------------------------------------------------------
# Y schema (copy of wealth.R).
#---------------------------------------------------------------------------

wealth_asset_vars = c(
  'cash', 'equities', 'bonds', 'retirement', 'life_ins', 'annuities',
  'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
  're_fund', 'other_nonfin'
)
wealth_debt_vars = c(
  'primary_mortgage', 'other_mortgage', 'credit_lines',
  'credit_cards', 'installment_debt', 'other_debt'
)
wealth_kg_vars = c(
  'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other'
)
wealth_y_vars = c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)

scf_to_y = function(df) {
  if (all(wealth_y_vars %in% names(df))) return(df)
  df %>% mutate(
    cash             = LIQ + CDS,
    equities         = STOCKS + STMUTF + COMUTF,
    bonds            = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    retirement       = IRAKH + THRIFT + FUTPEN + CURRPEN,
    life_ins         = CASHLI,
    annuities        = ANNUIT,
    trusts           = TRUSTS,
    other_fin        = OTHFIN + OMUTF,
    pass_throughs    = BUS,
    primary_home     = HOUSES,
    other_home       = ORESRE,
    re_fund          = NNRESRE,
    other_nonfin     = VEHIC + OTHNFIN,
    primary_mortgage = MRTHEL,
    other_mortgage   = RESDBT,
    credit_lines     = OTHLOC,
    credit_cards     = CCBAL,
    installment_debt = INSTALL,
    other_debt       = ODEBT,
    kg_primary_home  = KGHOUSE,
    kg_other_re      = KGORE,
    kg_pass_throughs = KGBUS,
    kg_other         = KGSTMF
  )
}

make_has = function(x) {
  case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)
}

#---------------------------------------------------------------------------
# SCF-side feature engineering (large spec, copy of wealth.R).
#---------------------------------------------------------------------------

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
    pctile_income         = compute_percentile(income, weight),
    has_wages             = make_has(wages_scf),
    pctile_wages          = compute_percentile(wages_scf, weight),
    has_business_act      = as.integer(business_scf != 0),
    has_business_pos      = as.integer(business_scf >  0),
    pctile_business       = compute_percentile(business_scf, weight),
    has_int_div           = make_has(int_div_scf),
    pctile_int_div        = compute_percentile(int_div_scf, weight),
    has_capital_gains_act = as.integer(capital_gains_scf != 0),
    has_capital_gains_pos = as.integer(capital_gains_scf >  0),
    pctile_capital_gains  = compute_percentile(capital_gains_scf, weight),
    has_ss_pens           = make_has(ss_pens_scf),
    pctile_ss_pens        = compute_percentile(ss_pens_scf, weight)
  )

feats = c('has_income_act', 'has_income_pos', 'pctile_income',
          'married', 'age_older', 'age_younger', 'n_dep_hh',
          'pctile_wages',         'has_wages',
          'pctile_business',      'has_business_act',
                                  'has_business_pos',
          'pctile_int_div',       'has_int_div',
          'pctile_capital_gains', 'has_capital_gains_act',
                                  'has_capital_gains_pos',
          'pctile_ss_pens',       'has_ss_pens')

#---------------------------------------------------------------------------
# PUF-side feature engineering (large spec, copy of wealth.R).
#---------------------------------------------------------------------------

puf_raw_inputs = c('wages', 'sole_prop', 'farm',
                   'scorp_active', 'scorp_active_loss', 'scorp_179',
                   'scorp_passive', 'scorp_passive_loss',
                   'part_active', 'part_active_loss', 'part_179',
                   'part_passive', 'part_passive_loss',
                   'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
                   'kg_lt', 'kg_st',
                   'rent', 'rent_loss', 'estate', 'estate_loss',
                   'gross_ss', 'gross_pens_dist', 'ui')
missing_cols = setdiff(puf_raw_inputs, names(tax_units))
if (length(missing_cols) > 0)
  stop('stage3_prototype: missing PUF composition inputs: ',
       paste(missing_cols, collapse = ', '))

tax_units = tax_units %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                          NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    married = as.integer(!is.na(male2)),
    n_dep_hh = (!is.na(dep_age1) & dep_age1 < 18) +
               (!is.na(dep_age2) & dep_age2 < 18) +
               (!is.na(dep_age3) & dep_age3 < 18),
    income  = wages +
              sole_prop + farm +
              scorp_active  - scorp_active_loss  - scorp_179 +
              scorp_passive - scorp_passive_loss +
              part_active   - part_active_loss   - part_179 +
              part_passive  - part_passive_loss +
              txbl_int + exempt_int + div_ord + div_pref +
              kg_lt + kg_st +
              gross_ss + gross_pens_dist +
              ui +
              rent - rent_loss + estate - estate_loss,
    wages_puf         = wages,
    business_puf      = sole_prop + farm +
                        scorp_active  - scorp_active_loss  - scorp_179 +
                        scorp_passive - scorp_passive_loss +
                        part_active   - part_active_loss   - part_179 +
                        part_passive  - part_passive_loss,
    int_div_puf       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains_puf = kg_lt + kg_st,
    ss_pens_puf       = gross_ss + gross_pens_dist,
    has_income_act = as.integer(income != 0),
    has_income_pos = as.integer(income >  0),
    pctile_income  = compute_percentile(income, weight),
    has_wages             = make_has(wages_puf),
    pctile_wages          = compute_percentile(wages_puf, weight),
    has_business_act      = as.integer(business_puf != 0),
    has_business_pos      = as.integer(business_puf >  0),
    pctile_business       = compute_percentile(business_puf, weight),
    has_int_div           = make_has(int_div_puf),
    pctile_int_div        = compute_percentile(int_div_puf, weight),
    has_capital_gains_act = as.integer(capital_gains_puf != 0),
    has_capital_gains_pos = as.integer(capital_gains_puf >  0),
    pctile_capital_gains  = compute_percentile(capital_gains_puf, weight),
    has_ss_pens           = make_has(ss_pens_puf),
    pctile_ss_pens        = compute_percentile(ss_pens_puf, weight)
  )

stopifnot(all(feats %in% names(scf_tax_units)))
stopifnot(all(feats %in% names(tax_units)))

#---------------------------------------------------------------------------
# Bootstrap expansion of SCF (matches test_wealth_X_ablation.R seeding so the
# DRF cache hit is valid and donor indices (1..250000) map into the same
# Y_mat the forest was trained on).
#---------------------------------------------------------------------------

scf_mapped = scf_to_y(scf_tax_units)

n_boot     = 250000L
boot_probs = scf_mapped$weight / sum(scf_mapped$weight)
boot_idx   = sample.int(nrow(scf_mapped), size = n_boot,
                        replace = TRUE, prob = boot_probs)
scf_boot   = scf_mapped[boot_idx, ]
Y_mat      = as.matrix(scf_boot[wealth_y_vars])
X_mat      = as.matrix(scf_boot[feats])

cat(sprintf('bootstrap: %d rows from %d SCF tax units\n',
            nrow(scf_boot), nrow(scf_mapped)))

#---------------------------------------------------------------------------
# Load cached DRF (large spec, f=50, t=1337). Should hit cache immediately.
#---------------------------------------------------------------------------

DRF_NUM_FEATURES = 50L
DRF_NUM_TREES    = 500L
DRF_MIN_NODE_SIZE = 20L

t0 = Sys.time()
fit = train_or_load_drf(
  name          = sprintf('wealth_drf_ablation_large_t%d_f%d',
                          TRAIN_SEED, DRF_NUM_FEATURES),
  X             = X_mat,
  Y             = Y_mat,
  num.trees     = DRF_NUM_TREES,
  min.node.size = DRF_MIN_NODE_SIZE,
  honesty       = TRUE,
  num.features  = DRF_NUM_FEATURES
)
cat(sprintf('DRF load: %.1f s\n',
            as.numeric(Sys.time() - t0, units = 'secs')))

#---------------------------------------------------------------------------
# walk_to_leaf (copied from wealth.R).
#---------------------------------------------------------------------------

walk_to_leaf_factory = function(fit) {
  n_trees = fit[['_num_trees']]
  root    = sapply(fit[['_root_nodes']], identity)
  child_L = lapply(fit[['_child_nodes']], `[[`, 1L)
  child_R = lapply(fit[['_child_nodes']], `[[`, 2L)
  split_v = fit[['_split_vars']]
  split_s = fit[['_split_values']]
  leaves  = fit[['_leaf_samples']]
  walk = function(t, x_row) {
    L  = child_L[[t]]; R  = child_R[[t]]
    v  = split_v[[t]]; s  = split_s[[t]]
    ll = leaves [[t]]
    node = root[t] + 1L
    repeat {
      if (length(ll[[node]]) > 0L) return(node)
      xv = x_row[v[node] + 1L]; sv = s[node]
      node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L
             else R[node] + 1L
    }
  }
  list(walk = walk, leaves = leaves, n_trees = n_trees)
}

wtl = walk_to_leaf_factory(fit)

#---------------------------------------------------------------------------
# Walk every PUF record, capture per-record leaf donor list, draw baseline
# uniform donor. Seed matches test_wealth_X_ablation.R convention.
#---------------------------------------------------------------------------

set.seed(DONOR_SEED)
X_puf     = as.matrix(tax_units[, feats])
n_pred    = nrow(X_puf)
tree_pick = sample.int(wtl$n_trees, size = n_pred, replace = TRUE)

leaf_donors_list = vector('list', n_pred)
baseline_donors  = integer(n_pred)
leaf_sizes       = integer(n_pred)

t0 = Sys.time()
for (i in seq_len(n_pred)) {
  tr  = tree_pick[i]
  nd  = wtl$walk(tr, X_puf[i, ])
  lr  = wtl$leaves[[tr]][[nd]] + 1L
  leaf_donors_list[[i]] = lr
  leaf_sizes[i]         = length(lr)
  baseline_donors[i]    = lr[sample.int(length(lr), 1L)]
}
cat(sprintf('leaf walk + baseline draw: %.1f s (leaf_size p10=%d p50=%d p90=%d)\n',
            as.numeric(Sys.time() - t0, units = 'secs'),
            as.integer(quantile(leaf_sizes, 0.10)),
            median(leaf_sizes),
            as.integer(quantile(leaf_sizes, 0.90))))

#---------------------------------------------------------------------------
# Donor feature matrix F_donor: n_boot × 6 target statistics.
# Columns = {nw_total, nw_pos_count, cash_total, cash_pos_count,
#            eq_total, eq_pos_count} as f(Y) per donor.
#---------------------------------------------------------------------------

asset_mat = Y_mat[, wealth_asset_vars, drop = FALSE]
debt_mat  = Y_mat[, wealth_debt_vars,  drop = FALSE]
donor_nw   = rowSums(asset_mat) - rowSums(debt_mat)
donor_cash = Y_mat[, 'cash']
donor_eq   = Y_mat[, 'equities']

F_donor = cbind(
  nw_total       = donor_nw,
  nw_pos_count   = as.numeric(donor_nw   > 0),
  cash_total     = donor_cash,
  cash_pos_count = as.numeric(donor_cash > 0),
  eq_total       = donor_eq,
  eq_pos_count   = as.numeric(donor_eq   > 0)
)
k_target = ncol(F_donor)
target_names = colnames(F_donor)
cat(sprintf('F_donor: %d donors × %d targets\n', nrow(F_donor), k_target))

#---------------------------------------------------------------------------
# SCF ground-truth targets per income decile.
# Decile = ceiling(pctile_income / 10), floored at 1 (pctile=0 from
# non-positive income → decile 1).
#---------------------------------------------------------------------------

# Recompute pctile_income via compute_percentile already above for SCF. For
# target computation we want raw dollar values on scf_tax_units.
scf_y   = scf_to_y(scf_tax_units)
scf_nw  = rowSums(scf_y[, wealth_asset_vars]) - rowSums(scf_y[, wealth_debt_vars])
scf_cash = scf_y$cash
scf_eq   = scf_y$equities
scf_w    = scf_y$weight
scf_pct  = scf_y$pctile_income
scf_dec  = pmin(pmax(ceiling(scf_pct / 10), 1L), 10L)

compute_cell_metrics = function(nw, cash, eq, w, decile, dec_val) {
  sel = decile == dec_val
  ww  = w[sel]
  c(nw_total       = sum(ww * nw[sel]),
    nw_pos_count   = sum(ww * (nw[sel]   > 0)),
    cash_total     = sum(ww * cash[sel]),
    cash_pos_count = sum(ww * (cash[sel] > 0)),
    eq_total       = sum(ww * eq[sel]),
    eq_pos_count   = sum(ww * (eq[sel]   > 0)))
}

scf_targets = lapply(1:10, function(d)
  compute_cell_metrics(scf_nw, scf_cash, scf_eq, scf_w, scf_dec, d))

puf_pct = tax_units$pctile_income
puf_dec = pmin(pmax(ceiling(puf_pct / 10), 1L), 10L)
puf_w   = tax_units$weight

# Diagnostic: weight totals (should be on the same order of magnitude).
cat(sprintf('weight totals: SCF=%.2e, PUF=%.2e\n',
            sum(scf_w), sum(puf_w)))
cat(sprintf('decile counts:\n'))
for (d in 1:10) {
  cat(sprintf('  d%2d: SCF n=%5d w_sum=%.2e | PUF n=%6d w_sum=%.2e\n',
              d, sum(scf_dec == d), sum(scf_w[scf_dec == d]),
              sum(puf_dec == d), sum(puf_w[puf_dec == d])))
}

#---------------------------------------------------------------------------
# Baseline (Stage 2 uniform draw) metrics.
#---------------------------------------------------------------------------

baseline_Y  = Y_mat[baseline_donors, , drop = FALSE]
baseline_nw = rowSums(baseline_Y[, wealth_asset_vars]) -
              rowSums(baseline_Y[, wealth_debt_vars])
baseline_cash = baseline_Y[, 'cash']
baseline_eq   = baseline_Y[, 'equities']

baseline_metrics = lapply(1:10, function(d)
  compute_cell_metrics(baseline_nw, baseline_cash, baseline_eq,
                       puf_w, puf_dec, d))

#---------------------------------------------------------------------------
# Tilt solver: convex Newton on the max-entropy dual.
#   l(lambda) = sum_i w_i log Z_i(lambda) - lambda^T T
#   Z_i = sum_{j in L_i} exp(lambda^T f(Y_j))
# Gradient = achieved - T; Hessian = sum_i w_i Cov_{p_i}[f].
# Backtracking on relative gradient norm.
#---------------------------------------------------------------------------

solve_tilt = function(records, weights, leaf_donors, F_mat, T_target,
                      max_iter = 80, tol_rel = 1e-5,
                      max_bt = 30, verbose = FALSE, log_iters = 5) {
  n = length(records)
  k = ncol(F_mat)
  stopifnot(length(T_target) == k)

  ls       = lengths(leaf_donors)
  all_d    = unlist(leaf_donors, use.names = FALSE)
  rec_idx  = rep(seq_len(n), times = ls)
  F_all_raw = F_mat[all_d, , drop = FALSE]
  w_expand = rep(weights, times = ls)
  M        = length(rec_idx)

  # Per-column rescale: divide each f_k by its RMS (over donor-row pool in
  # THIS cell). Probabilities under exp(lam^T f) are invariant under this
  # rescale (lam absorbs 1/s); the Newton solve becomes better conditioned.
  # Use max(..., 1e-12) to avoid zero-division if a column is identically 0.
  col_scale = pmax(sqrt(colMeans(F_all_raw^2)), 1e-12)
  F_all  = sweep(F_all_raw, 2, col_scale, `/`)
  T_eff  = T_target / col_scale
  t_scale_eff = pmax(abs(T_eff), 1)

  eval_fit = function(lam) {
    u     = as.vector(F_all %*% lam)
    u_max = ave(u, rec_idx, FUN = max)
    eu    = exp(u - u_max)
    Z     = ave(eu, rec_idx, FUN = sum)
    p_all = eu / Z
    ach   = as.vector(crossprod(F_all, w_expand * p_all))
    list(p_all = p_all, achieved = ach,
         grad = ach - T_eff,
         rel = max(abs(ach - T_eff) / t_scale_eff))
  }

  lambda = rep(0, k)
  fit0 = eval_fit(lambda)
  hist = list(list(iter = 0, rel = fit0$rel, lambda = lambda))

  cur = fit0
  n_bt_fail = 0
  for (iter in seq_len(max_iter)) {
    if (cur$rel < tol_rel) break
    wp = w_expand * cur$p_all

    F_wp = F_all * sqrt(wp)
    H1 = crossprod(F_wp)

    E_per = matrix(0, nrow = n, ncol = k)
    for (kk in seq_len(k)) {
      E_per[, kk] = rowsum(cur$p_all * F_all[, kk], rec_idx)[, 1]
    }
    E_w = E_per * sqrt(weights)
    H2 = crossprod(E_w)

    H = H1 - H2
    # More aggressive regularization (1e-6 relative to H's diagonal) helps
    # when within-cell variance on a binary target has a few near-zero rows.
    reg = 1e-6 * max(1, max(abs(diag(H))))
    H_reg = H + diag(reg, k)

    step = tryCatch(
      solve(H_reg, cur$grad),
      error = function(e) {
        if (verbose)
          cat(sprintf('  iter %d: H solve failed (%s), using grad step\n',
                      iter, conditionMessage(e)))
        cur$grad / max(diag(H_reg))
      }
    )

    alpha = 1
    improved = FALSE
    for (bt in seq_len(max_bt)) {
      lam_try = lambda - alpha * step
      fit_try = eval_fit(lam_try)
      if (fit_try$rel < cur$rel) {
        improved = TRUE
        break
      }
      alpha = alpha / 2
    }
    if (!improved) {
      # One retry: restart with gradient-descent direction (steepest on
      # scaled coords), which is orthogonal to Newton when H is badly
      # conditioned. If that also fails, stop.
      alpha_gd = 1 / (max(diag(H_reg)) + reg)
      lam_try = lambda - alpha_gd * cur$grad
      fit_try = eval_fit(lam_try)
      if (fit_try$rel < cur$rel) {
        improved = TRUE
      } else {
        n_bt_fail = n_bt_fail + 1
        if (verbose && n_bt_fail <= 2)
          cat(sprintf('  iter %d: backtrack + GD retry both failed, stopping\n',
                      iter))
        break
      }
    }
    lambda = lam_try
    cur = fit_try
    hist[[length(hist) + 1]] = list(iter = iter, rel = cur$rel, lambda = lambda)

    if (verbose && iter <= log_iters)
      cat(sprintf('  iter %2d: rel=%.2e, ||lam||=%.3g\n',
                  iter, cur$rel, sqrt(sum(lambda^2))))
  }

  ess = as.vector(tapply(cur$p_all, rec_idx,
                         function(p) 1 / sum(p^2)))

  # Build seg_offsets so caller can redraw without rebuilding.
  seg_end   = cumsum(ls)
  seg_start = c(0L, head(seg_end, -1L))

  list(lambda       = lambda,
       lambda_unsc  = lambda / col_scale,   # unscaled units: lam' f_raw = lam f_scaled
       col_scale    = col_scale,
       achieved_raw = cur$achieved * col_scale,   # raw aggregate (T_target units)
       iter      = length(hist) - 1L,
       rel       = cur$rel,
       grad      = cur$grad,
       achieved  = cur$achieved,
       p_all     = cur$p_all,
       rec_idx   = rec_idx,
       all_donors = all_d,
       seg_start = seg_start,
       seg_end   = seg_end,
       ls        = ls,
       ess       = ess,
       history   = hist)
}

#---------------------------------------------------------------------------
# Run per-decile solvers.
#---------------------------------------------------------------------------

cat('\n--- Solving per-decile tilts ---\n')
tilt_results = vector('list', 10)
for (d in 1:10) {
  rec_d = which(puf_dec == d)
  w_d   = puf_w[rec_d]
  ld_d  = leaf_donors_list[rec_d]
  T_d   = scf_targets[[d]]

  t0 = Sys.time()
  res = solve_tilt(rec_d, w_d, ld_d, F_donor, T_d, verbose = TRUE)
  dt  = as.numeric(Sys.time() - t0, units = 'secs')

  cat(sprintf('  d%2d: n=%6d, iter=%2d, rel=%.2e, ||lam||=%.3g, ESS p10=%.1f p50=%.1f, %.1fs\n',
              d, length(rec_d), res$iter, res$rel,
              sqrt(sum(res$lambda^2)),
              as.numeric(quantile(res$ess, 0.10)),
              median(res$ess), dt))
  tilt_results[[d]] = res
}

#---------------------------------------------------------------------------
# Redraw donors under tilted probabilities.
#---------------------------------------------------------------------------

cat('\n--- Redrawing tilted donors ---\n')
set.seed(TILT_DONOR_SEED)
tilted_donors = integer(n_pred)

t0 = Sys.time()
for (d in 1:10) {
  rec_d = which(puf_dec == d)
  res   = tilt_results[[d]]
  for (local_i in seq_along(rec_d)) {
    sl         = (res$seg_start[local_i] + 1L):res$seg_end[local_i]
    donor_pool = res$all_donors[sl]
    prob_pool  = res$p_all[sl]
    tilted_donors[rec_d[local_i]] =
      donor_pool[sample.int(length(donor_pool), 1L, prob = prob_pool)]
  }
}
cat(sprintf('redraw: %.1f s\n', as.numeric(Sys.time() - t0, units = 'secs')))

#---------------------------------------------------------------------------
# Post-tilt metrics.
#---------------------------------------------------------------------------

tilted_Y   = Y_mat[tilted_donors, , drop = FALSE]
tilted_nw  = rowSums(tilted_Y[, wealth_asset_vars]) -
             rowSums(tilted_Y[, wealth_debt_vars])
tilted_cash = tilted_Y[, 'cash']
tilted_eq   = tilted_Y[, 'equities']

tilted_metrics = lapply(1:10, function(d)
  compute_cell_metrics(tilted_nw, tilted_cash, tilted_eq,
                       puf_w, puf_dec, d))

#---------------------------------------------------------------------------
# Report: per-decile + overall, pre-tilt vs tilted vs SCF target.
#---------------------------------------------------------------------------

fmt_metric = function(x) {
  if (is.na(x)) return('    NA')
  if (abs(x) >= 1e12) sprintf('%8.2fT', x / 1e12) else
  if (abs(x) >= 1e9)  sprintf('%8.2fB', x / 1e9)  else
  if (abs(x) >= 1e6)  sprintf('%8.2fM', x / 1e6)  else
                       sprintf('%8.0f ', x)
}

pct = function(num, den) {
  if (!is.finite(den) || abs(den) < 1e-12) return(NA_real_)
  (num - den) / abs(den) * 100
}

cat('\n\n==========================================================\n')
cat('    SCF TARGET vs STAGE 2 (PRE) vs STAGE 3 (POST)\n')
cat('    (%-dev from SCF target in parens)\n')
cat('==========================================================\n\n')

for (d in 1:10) {
  cat(sprintf('--- Decile %d ---\n', d))
  for (m in target_names) {
    scf_v = scf_targets[[d]][[m]]
    pre_v = baseline_metrics[[d]][[m]]
    post_v = tilted_metrics[[d]][[m]]
    cat(sprintf('  %-18s  SCF=%s   pre=%s (%+7.2f%%)   post=%s (%+7.2f%%)\n',
                m, fmt_metric(scf_v),
                fmt_metric(pre_v), pct(pre_v, scf_v),
                fmt_metric(post_v), pct(post_v, scf_v)))
  }
  cat('\n')
}

overall_scf  = Reduce(`+`, scf_targets)
overall_pre  = Reduce(`+`, baseline_metrics)
overall_post = Reduce(`+`, tilted_metrics)
cat('--- Overall (sum over deciles) ---\n')
for (m in target_names) {
  cat(sprintf('  %-18s  SCF=%s   pre=%s (%+7.2f%%)   post=%s (%+7.2f%%)\n',
              m, fmt_metric(overall_scf[[m]]),
              fmt_metric(overall_pre[[m]]),  pct(overall_pre[[m]], overall_scf[[m]]),
              fmt_metric(overall_post[[m]]), pct(overall_post[[m]], overall_scf[[m]])))
}

#---------------------------------------------------------------------------
# Tolerance check (3%).
#---------------------------------------------------------------------------

cat('\n\n==========================================================\n')
cat('    TOLERANCE CHECK (3% relative)\n')
cat('==========================================================\n\n')

n_pass = 0; n_fail = 0; fail_rows = character()
for (d in 1:10) {
  for (m in target_names) {
    scf_v = scf_targets[[d]][[m]]
    post_v = tilted_metrics[[d]][[m]]
    rel = pct(post_v, scf_v) / 100
    flag = if (is.na(rel)) 'SKIP' else
           if (abs(rel) < 0.03) 'PASS' else 'FAIL'
    if (flag == 'PASS') n_pass = n_pass + 1 else
      if (flag == 'FAIL') {
        n_fail = n_fail + 1
        fail_rows = c(fail_rows,
          sprintf('  d%d %-18s rel=%+7.2f%%', d, m, rel * 100))
      }
  }
}
cat(sprintf('Passing cells (3%% tol): %d / %d\n',
            n_pass, n_pass + n_fail))
if (n_fail > 0) {
  cat('Failures:\n')
  cat(paste(fail_rows, collapse = '\n'), '\n')
}

#---------------------------------------------------------------------------
# Save artifact.
#---------------------------------------------------------------------------

out = list(
  config = list(
    TRAIN_SEED = TRAIN_SEED, DONOR_SEED = DONOR_SEED,
    TILT_DONOR_SEED = TILT_DONOR_SEED,
    spec = 'large', num_features = DRF_NUM_FEATURES,
    num_trees = DRF_NUM_TREES, min_node_size = DRF_MIN_NODE_SIZE,
    n_boot = n_boot
  ),
  target_names    = target_names,
  scf_targets     = scf_targets,
  baseline_metrics = baseline_metrics,
  tilted_metrics  = tilted_metrics,
  overall         = list(scf = overall_scf,
                         pre = overall_pre,
                         post = overall_post),
  tilt_lambdas    = lapply(tilt_results, `[[`, 'lambda'),
  tilt_rel        = sapply(tilt_results, `[[`, 'rel'),
  tilt_iter       = sapply(tilt_results, `[[`, 'iter'),
  ess_summary     = lapply(tilt_results, function(r) {
    list(min = min(r$ess), p10 = as.numeric(quantile(r$ess, 0.10)),
         p50 = median(r$ess), p90 = as.numeric(quantile(r$ess, 0.90)),
         mean = mean(r$ess))
  }),
  leaf_size_summary = list(
    min = min(leaf_sizes),
    p10 = as.numeric(quantile(leaf_sizes, 0.10)),
    p50 = median(leaf_sizes),
    p90 = as.numeric(quantile(leaf_sizes, 0.90)),
    max = max(leaf_sizes)
  ),
  decile_counts   = list(
    puf = tapply(puf_w, puf_dec, sum),
    scf = tapply(scf_w, scf_dec, sum)),
  tolerance = 0.03,
  n_pass = n_pass, n_fail = n_fail
)
write_rds(out, 'resources/cache/stage3_prototype_v2.rds')
cat('\nwrote resources/cache/stage3_prototype_v2.rds\n')
cat('Done.\n')
