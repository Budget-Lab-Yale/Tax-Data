#--------------------------------------
# wealth.R
#
# SCF → PUF wealth imputation, runs at
# the SCF donor year (2022) as part of
# Phase 3 (donor-year imputations).
#
# Exports `run_wealth_imputation()`, a
# pure function that takes a materialized
# 2022 PUF tibble + the cached
# `scf_tax_units` and returns a tibble
# (id, 23 wealth cols) at 2022 values.
#
# Stage architecture (see design memo
# `stage3_calibration_design.md`):
#   Stage 1 (upstream) — SCF PEU →
#             tax-unit split, produces
#             `scf_tax_units`.
#   Stage 2 (this file, current) —
#             joint DRF over the 23-dim
#             Y vector; uniform donor
#             draw from DRF leaf.
#   Stage 3 (this file, follow-up PR) —
#             exponential-tilt donor
#             selection against SCF
#             extensive + DFA intensive
#             margin targets. Not yet
#             implemented here; the v2
#             prototype lives in
#             `src/eda/stage3_prototype_v2.R`.
#
# Feature spec: "large" (19 features)
# with dual-binary encoding for
# sign-bearing variables — matches
# the X-ablation (2026-04-22) and the
# cached DRF
# `wealth_drf_ablation_large_t1337_f50`.
#--------------------------------------


source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')


# Module-scope so eda harnesses can reuse. Collapses SCFP raw fields to
# the 23 Y-var schema; no-op if already in that form.
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


#--------------------------------------
# run_wealth_imputation
#
# Args:
#   puf_tax_units : tibble, the PUF at 2022 donor-year state. Must carry
#                   `id`, `weight`, `filing_status`, `male1`/`male2`,
#                   `age1`/`age2`, `dep_age1/2/3`, and the raw income
#                   composition columns used by the large-spec feature
#                   engineering (wages, sole_prop, farm, scorp_*, part_*,
#                   txbl_int, exempt_int, div_ord, div_pref, kg_lt, kg_st,
#                   rent, rent_loss, estate, estate_loss, gross_ss,
#                   gross_pens_dist, ui).
#   scf_tax_units : tibble from stage1_scf_tax_units.R; 29,290 rows with
#                   SCFP asset/debt/kg fields + 7 income-composition
#                   fields (wages_scf, business_scf, int_div_scf,
#                   capital_gains_scf, rent_scf, ss_pens_scf,
#                   ui_other_scf).
#
# Returns:
#   list(y, y_pre_tilt, qc_report, rescale_factors) — y keyed by
#   puf_tax_units$id with 23 wealth columns populated.
#--------------------------------------
run_wealth_imputation = function(puf_tax_units, scf_tax_units) {

  features = c('has_income_act', 'has_income_pos', 'has_negative_income',
               'pctile_income',
               'married', 'age_older', 'age_younger', 'n_dep_hh',
               'pctile_wages',         'has_wages',
               'pctile_business',      'has_business_act',
                                       'has_business_pos',
               'pctile_int_div',       'has_int_div',
               'pctile_capital_gains', 'has_capital_gains_act',
                                       'has_capital_gains_pos',
               'pctile_ss_pens',       'has_ss_pens')

  make_has = function(x) {
    case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)
  }

  #---------------------------------------------------------------------------
  # SCF-side feature engineering (unchanged from pre-refactor)
  #---------------------------------------------------------------------------

  stopifnot(all(c('wages_scf', 'business_scf', 'int_div_scf',
                  'capital_gains_scf', 'rent_scf',
                  'ss_pens_scf', 'ui_other_scf') %in% names(scf_tax_units)))

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

  scf_training = scf_tax_units %>%
    scf_to_y() %>%
    select(weight, all_of(features), all_of(wealth_y_vars))

  n_boot = 250000
  boot_probs = scf_training$weight / sum(scf_training$weight)
  boot_idx   = sample.int(nrow(scf_training), size = n_boot,
                          replace = TRUE, prob = boot_probs)
  scf_boot   = scf_training[boot_idx, ]

  Y_mat = as.matrix(scf_boot[wealth_y_vars])
  X_mat = as.matrix(scf_boot[features])

  #---------------------------------------------------------------------------
  # Fit/load DRF (hits cache on repeat runs)
  #---------------------------------------------------------------------------

  # min.node.size = 5 (down from default 20): smaller leaves → more
  # homogeneous donor pools → less heavy-tail leaf contamination (a few
  # ultra-wealth donors sharing a leaf with bottom-quintile records is
  # what was driving the 10× per-cell seed variance).
  wealth_drf = train_or_load_drf(
    name          = 'wealth_drf',
    X             = X_mat,
    Y             = Y_mat,
    num.features  = 50,
    min.node.size = 5
  )

  #---------------------------------------------------------------------------
  # PUF-side feature engineering. `puf_tax_units` comes in at 2022 values,
  # so derived rank-based features reflect the 2022 state of the income
  # distribution.
  #---------------------------------------------------------------------------

  puf_raw_inputs = c('wages', 'sole_prop', 'farm',
                     'scorp_active', 'scorp_active_loss', 'scorp_179',
                     'scorp_passive', 'scorp_passive_loss',
                     'part_active', 'part_active_loss', 'part_179',
                     'part_passive', 'part_passive_loss',
                     'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
                     'kg_lt', 'kg_st',
                     'rent', 'rent_loss', 'estate', 'estate_loss',
                     'gross_ss', 'gross_pens_dist', 'ui',
                     'dep_age1', 'dep_age2', 'dep_age3')
  missing_cols = setdiff(puf_raw_inputs, names(puf_tax_units))
  if (length(missing_cols) > 0)
    stop('wealth.R: missing PUF composition inputs: ',
         paste(missing_cols, collapse = ', '))

  puf = puf_tax_units %>%
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

      has_income_act      = as.integer(income != 0),
      has_income_pos      = as.integer(income >  0),
      has_negative_income = as.integer(income <  0),
      pctile_income       = compute_percentile(income, weight, FINE_PCTILE_PROBS),

      has_wages             = make_has(wages_puf),
      pctile_wages          = compute_percentile(wages_puf, weight, FINE_PCTILE_PROBS),
      has_business_act      = as.integer(business_puf != 0),
      has_business_pos      = as.integer(business_puf >  0),
      pctile_business       = compute_percentile(business_puf, weight, FINE_PCTILE_PROBS),
      has_int_div           = make_has(int_div_puf),
      pctile_int_div        = compute_percentile(int_div_puf, weight, FINE_PCTILE_PROBS),
      has_capital_gains_act = as.integer(capital_gains_puf != 0),
      has_capital_gains_pos = as.integer(capital_gains_puf >  0),
      pctile_capital_gains  = compute_percentile(capital_gains_puf, weight, FINE_PCTILE_PROBS),
      has_ss_pens           = make_has(ss_pens_puf),
      pctile_ss_pens        = compute_percentile(ss_pens_puf, weight, FINE_PCTILE_PROBS)
    ) %>%
    select(id, weight, income, age_older, all_of(features))

  #---------------------------------------------------------------------------
  # Stage 2: walk forest + collect per-record leaf donor lists.
  # (Donor selection itself happens under tilted probabilities in Stage 3;
  # Stage 2 here just identifies each record's leaf.)
  #---------------------------------------------------------------------------

  n_trees = wealth_drf[['_num_trees']]
  root    = sapply(wealth_drf[['_root_nodes']], identity)
  child_L = lapply(wealth_drf[['_child_nodes']], `[[`, 1L)
  child_R = lapply(wealth_drf[['_child_nodes']], `[[`, 2L)
  split_v = wealth_drf[['_split_vars']]
  split_s = wealth_drf[['_split_values']]
  leaves  = wealth_drf[['_leaf_samples']]

  walk_to_leaf = function(t, x_row) {
    L  = child_L[[t]]; R  = child_R[[t]]
    v  = split_v[[t]]; s  = split_s[[t]]
    ll = leaves[[t]]
    node = root[t] + 1L
    repeat {
      if (length(ll[[node]]) > 0L) return(node)
      xv = x_row[v[node] + 1L]; sv = s[node]
      node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L else R[node] + 1L
    }
  }

  X_puf     = as.matrix(puf[, features])
  n_pred    = nrow(X_puf)
  tree_pick = sample.int(n_trees, size = n_pred, replace = TRUE)
  leaf_donors_list = vector('list', n_pred)

  t0 = Sys.time()
  for (i in seq_len(n_pred)) {
    t  = tree_pick[i]
    nd = walk_to_leaf(t, X_puf[i, ])
    leaf_donors_list[[i]] = leaves[[t]][[nd]] + 1L
  }
  cat(sprintf('wealth.R: leaf walk over %d rows: %.1fs (median leaf size = %d)\n',
              n_pred, as.numeric(Sys.time() - t0, units = 'secs'),
              median(lengths(leaf_donors_list))))

  #---------------------------------------------------------------------------
  # Stage 3: TWO-STEP calibration per cell (income × age).
  #
  #   Step A — extensive-only exponential tilt.
  #     Solve the convex dual with ONLY "count > 0" targets. 6 categories
  #     {nw, equities, bonds, homes, other, debt} × 1 margin × up to 14
  #     cells = ≤ 84 targets total, pruned to ~68 after QC.
  #   Step B — per-(cell × category) intensive rescale.
  #     For each cell and each category C ∈ {equities, bonds, homes,
  #     other, debt}, compute
  #        s = SCF_total(cell, C) / PUF_post_tilt_total(cell, C)
  #     and multiply every underlying Y-var of C on every record in the
  #     cell by s. This hits SCF intensive aggregates EXACTLY per cell.
  #     NW isn't rescaled directly (it's assets−debts and falls out).
  #
  # Design rationale: the two margins are orthogonal. Extensive is a
  # discrete donor-support problem; intensive is a continuous levels
  # problem. Combining them in one lambda caused the 12-target solver
  # to collapse onto a few donors (ESS→1) and inflate top shares.
  # Decoupling restores clean convergence on A and hits B exactly by
  # construction.
  #
  # Rescaling synthesizes values no SCF donor had — accepted tradeoff.
  # Within-cell distribution shape (and thus top-share within cell)
  # is preserved under rescaling.
  #
  # Categories-to-Y-vars map for rescaling:
  CAT_MEMBERS = list(
    equities = 'equities',
    bonds    = 'bonds',
    homes    = 'primary_home',
    other    = setdiff(wealth_asset_vars, c('equities', 'bonds',
                                             'primary_home')),
    debt     = wealth_debt_vars
  )
  # kg_* rescale follows the inheritance rule (same as in dfa_factors):
  KG_PARENT = list(
    kg_primary_home  = 'homes',
    kg_other_re      = 'other',
    kg_pass_throughs = 'other',
    kg_other         = 'equities'
  )
  #---------------------------------------------------------------------------

  # ---------- Donor-side category values + 6-column feature matrix -----------
  # Extensive-only: 6 binary columns, one per category.
  donor_y_tbl = as.data.frame(Y_mat)
  names(donor_y_tbl) = wealth_y_vars
  donor_cats = compute_category_values(donor_y_tbl)

  F_donor_parts = lapply(CALIB_CATEGORIES, function(cc) {
    v = donor_cats[[paste0('cat_', cc)]]
    m = matrix(as.numeric(v > 0), ncol = 1)
    colnames(m) = paste0(cc, '.extensive')
    m
  })
  F_donor = do.call(cbind, F_donor_parts)

  # ---------- Cell assignment on SCF side ----------
  # Age: pmin(80) to match the PUF topcode; 0 for the synthetic 2nd
  # filer on singles. Income: unfloored sum of the 7 SCF composition
  # cols (same formula as the forest feature, matching build_record_bucket's
  # PUF-side formula).
  scf_y = scf_to_y(scf_tax_units)
  scf_age2    = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
  scf_age_older = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
  scf_income  = with(scf_tax_units,
    wages_scf + business_scf + int_div_scf + capital_gains_scf +
    rent_scf + ss_pens_scf + ui_other_scf
  )
  scf_cells_df = data.frame(
    weight    = scf_y$weight,
    age_older = scf_age_older,
    income    = scf_income
  )
  scf_cells_df = cbind(scf_cells_df, compute_category_values(scf_y))
  scf_cells_df = assign_calibration_cells(scf_cells_df,
                                          scf_cells_df$income,
                                          scf_cells_df$age_older,
                                          scf_cells_df$weight)

  # ---------- Cell assignment on PUF side ----------
  puf_w = puf_tax_units$weight
  puf_age2 = ifelse(is.na(puf_tax_units$age2), 0L, puf_tax_units$age2)
  puf_age_older = pmax(pmin(80L, puf_tax_units$age1), pmin(80L, puf_age2))
  puf_income_vec = with(puf_tax_units,
    wages + sole_prop + farm +
    scorp_active  - scorp_active_loss  - scorp_179 +
    scorp_passive - scorp_passive_loss +
    part_active   - part_active_loss   - part_179 +
    part_passive  - part_passive_loss +
    txbl_int + exempt_int + div_ord + div_pref +
    kg_lt + kg_st +
    gross_ss + gross_pens_dist + ui +
    rent - rent_loss + estate - estate_loss
  )
  puf_cells = data.frame(
    id        = puf_tax_units$id,
    weight    = puf_w,
    age_older = puf_age_older,
    income    = puf_income_vec
  )
  puf_cells = assign_calibration_cells(puf_cells, puf_cells$income,
                                        puf_cells$age_older, puf_cells$weight)

  # ---------- Target viability QC (EXTENSIVE ONLY for the tilt) -------------
  # Intensive is handled by rescaling below; never enters the solver.
  requested_spec = default_wealth_target_spec() %>%
    filter(margin == 'extensive')
  qc_report = assess_target_viability(requested_spec, scf_cells_df)
  summarize_qc(qc_report)
  kept = qc_report %>% filter(status == 'keep') %>%
    mutate(feature_col = paste(category, margin, sep = '.'))

  # ---------- Pre-tilt donors (uniform leaf draw) ----------
  pre_tilt_donors = integer(n_pred)
  for (i in seq_len(n_pred)) {
    lr = leaf_donors_list[[i]]
    pre_tilt_donors[i] = lr[sample.int(length(lr), 1L)]
  }

  # Convex-Newton dual solver (RMS feature rescaling + Levenberg-Marquardt
  # style damping + gradient-descent fallback). Port of the v2 prototype
  # at src/eda/stage3_prototype_v2.R. k-general.
  solve_tilt = function(records, weights, leaf_donors, F_mat, T_target,
                        max_iter = 80, tol_rel = 1e-5,
                        max_bt = 30) {
    n = length(records); k = ncol(F_mat)
    ls       = lengths(leaf_donors)
    all_d    = unlist(leaf_donors, use.names = FALSE)
    rec_idx  = rep(seq_len(n), times = ls)
    F_raw    = F_mat[all_d, , drop = FALSE]
    w_expand = rep(weights, times = ls)

    # Per-column RMS rescale — invariant change of coordinates on λ that
    # makes the Newton system better-conditioned under mixed dollar/count
    # targets.
    col_scale  = pmax(sqrt(colMeans(F_raw^2)), 1e-12)
    F_all      = sweep(F_raw, 2, col_scale, `/`)
    T_eff      = T_target / col_scale
    t_scale    = pmax(abs(T_eff), 1)

    eval_fit = function(lam) {
      u     = as.vector(F_all %*% lam)
      u_max = ave(u, rec_idx, FUN = max)
      eu    = exp(u - u_max)
      Z     = ave(eu, rec_idx, FUN = sum)
      p_all = eu / Z
      ach   = as.vector(crossprod(F_all, w_expand * p_all))
      list(p_all = p_all, grad = ach - T_eff,
           rel = max(abs(ach - T_eff) / t_scale))
    }

    lambda = rep(0, k)
    cur    = eval_fit(lambda)
    for (iter in seq_len(max_iter)) {
      if (cur$rel < tol_rel) break
      wp = w_expand * cur$p_all
      H1 = crossprod(F_all * sqrt(wp))

      E_per = matrix(0, nrow = n, ncol = k)
      for (kk in seq_len(k))
        E_per[, kk] = rowsum(cur$p_all * F_all[, kk], rec_idx)[, 1]
      H2 = crossprod(E_per * sqrt(weights))

      H     = H1 - H2
      reg   = 1e-6 * max(1, max(abs(diag(H))))
      step  = tryCatch(solve(H + diag(reg, k), cur$grad),
                       error = function(e) cur$grad / max(diag(H) + reg))

      alpha    = 1
      improved = FALSE
      for (bt in seq_len(max_bt)) {
        lam_try = lambda - alpha * step
        fit_try = eval_fit(lam_try)
        if (fit_try$rel < cur$rel) { improved = TRUE; break }
        alpha = alpha / 2
      }
      if (!improved) {
        # Gradient-descent retry on scaled coordinates.
        alpha_gd = 1 / (max(diag(H)) + reg)
        lam_try  = lambda - alpha_gd * cur$grad
        fit_try  = eval_fit(lam_try)
        if (fit_try$rel < cur$rel) improved = TRUE
        else break
      }
      lambda = lam_try; cur = fit_try
    }

    seg_end   = cumsum(ls)
    seg_start = c(0L, head(seg_end, -1L))
    list(iter = iter, rel = cur$rel, lambda = lambda,
         p_all = cur$p_all, all_donors = all_d,
         seg_start = seg_start, seg_end = seg_end,
         ess = as.vector(tapply(cur$p_all, rec_idx, function(p) 1 / sum(p^2))))
  }

  # Per-cell solve over income × age. Cells with zero viable targets
  # (should be rare / zero in practice) fall through to the pre-tilt
  # uniform draw for those records.
  cat('wealth.R: solving per-cell tilts (income × age)\n')
  tilt_results = list()
  post_tilt_donors = integer(n_pred)
  t0 = Sys.time()

  for (ci in CALIB_INCOME_BUCKETS) {
    for (ca in CALIB_AGE_BUCKETS) {
      rec_cell = which(puf_cells$cell_income == ci & puf_cells$cell_age == ca)
      if (length(rec_cell) == 0L) next

      kept_cell = kept %>%
        filter(cell_income == ci, cell_age == ca)

      if (nrow(kept_cell) == 0L) {
        post_tilt_donors[rec_cell] = pre_tilt_donors[rec_cell]
        cat(sprintf('  %-10s × %-9s: n=%6d  [no viable targets → pre-tilt]\n',
                    ci, ca, length(rec_cell)))
        next
      }

      F_cell = F_donor[, kept_cell$feature_col, drop = FALSE]
      T_cell = setNames(kept_cell$target_value, kept_cell$feature_col)

      res = solve_tilt(rec_cell, puf_w[rec_cell],
                       leaf_donors_list[rec_cell],
                       F_cell, T_cell)

      cat(sprintf(
        '  %-10s × %-9s: n=%6d k=%2d iter=%2d rel=%.2e ESS p10=%.1f\n',
        ci, ca, length(rec_cell), ncol(F_cell),
        res$iter, res$rel,
        as.numeric(quantile(res$ess, 0.10))))
      tilt_results[[paste(ci, ca, sep = ':')]] = res

      # Sample donors for this cell under tilted probabilities.
      for (local_i in seq_along(rec_cell)) {
        sl = (res$seg_start[local_i] + 1L):res$seg_end[local_i]
        donor_pool = res$all_donors[sl]
        prob_pool  = res$p_all[sl]
        post_tilt_donors[rec_cell[local_i]] =
          donor_pool[sample.int(length(donor_pool), 1L, prob = prob_pool)]
      }
    }
  }
  cat(sprintf('wealth.R: tilt solver %.1fs\n',
              as.numeric(Sys.time() - t0, units = 'secs')))

  post_y = Y_mat[post_tilt_donors, , drop = FALSE]
  colnames(post_y) = wealth_y_vars
  pre_y  = Y_mat[pre_tilt_donors,  , drop = FALSE]
  colnames(pre_y)  = wealth_y_vars

  #---------------------------------------------------------------------------
  # Step B: per-(cell × category) intensive rescale.
  # For each cell c and category C, compute s = SCF_total / PUF_total and
  # multiply every Y-var of C on records in c by s. Also applies to kg_*
  # via KG_PARENT inheritance. Safe fallback: skip when either total
  # rounds to 0.
  #---------------------------------------------------------------------------

  cat('wealth.R: applying per-(cell × category) intensive rescale\n')
  rescale_rows = list()
  for (ci in CALIB_INCOME_BUCKETS) {
    for (ca in CALIB_AGE_BUCKETS) {
      rec_cell = which(puf_cells$cell_income == ci & puf_cells$cell_age == ca)
      if (length(rec_cell) == 0L) next
      rec_w    = puf_w[rec_cell]

      scf_mask = scf_cells_df$cell_income == ci & scf_cells_df$cell_age == ca
      scf_w    = scf_cells_df$weight[scf_mask]

      for (cat_name in names(CAT_MEMBERS)) {
        members = CAT_MEMBERS[[cat_name]]
        # SCF target (intensive total)
        scf_vals = if (length(members) == 1L) scf_y[scf_mask, members]
                   else rowSums(scf_y[scf_mask, members, drop = FALSE])
        scf_total = sum(scf_w * scf_vals)

        # PUF post-tilt aggregate
        puf_vals = if (length(members) == 1L) post_y[rec_cell, members]
                   else rowSums(post_y[rec_cell, members, drop = FALSE])
        puf_total = sum(rec_w * puf_vals)

        # Safe fallback on near-zero denominator; also if target is zero
        # while PUF has anything, skip (would zero out the category).
        skip = abs(puf_total) < max(1e3, 1e-6 * max(abs(scf_total), 1)) ||
               abs(scf_total) < max(1e3, 1e-6 * max(abs(puf_total), 1))
        factor = if (skip) 1 else scf_total / puf_total

        if (!skip) {
          for (m in members) post_y[rec_cell, m] = post_y[rec_cell, m] * factor
          # Apply same factor to kg_* with this parent category.
          for (kv in names(KG_PARENT)) {
            if (KG_PARENT[[kv]] == cat_name)
              post_y[rec_cell, kv] = post_y[rec_cell, kv] * factor
          }
        }

        rescale_rows[[length(rescale_rows) + 1L]] = tibble(
          cell_income          = ci,
          cell_age             = ca,
          category             = cat_name,
          scf_total            = scf_total,
          puf_pre_rescale_total = puf_total,
          factor               = factor,
          applied              = !skip
        )
      }
    }
  }
  rescale_factors = bind_rows(rescale_rows)

  # Summary print: distribution of factors.
  f_applied = rescale_factors %>% filter(applied)
  cat(sprintf(
    'wealth.R: rescale factors applied=%d skipped=%d  min=%.3f p10=%.3f median=%.3f p90=%.3f max=%.3f\n',
    sum(rescale_factors$applied), sum(!rescale_factors$applied),
    min(f_applied$factor), quantile(f_applied$factor, 0.10),
    median(f_applied$factor), quantile(f_applied$factor, 0.90),
    max(f_applied$factor)))
  extreme = f_applied %>% filter(factor < 0.5 | factor > 2.0)
  if (nrow(extreme) > 0) {
    cat(sprintf('  %d factors outside [0.5, 2.0]:\n', nrow(extreme)))
    print(extreme %>% select(cell_income, cell_age, category, factor),
          n = Inf)
  }

  # Return a list so the caller can route post-tilt into module_deltas and
  # save pre-tilt + qc_report + rescale_factors as diagnostic artifacts.
  list(
    y               = bind_cols(tibble(id = puf$id), as_tibble(post_y)),
    y_pre_tilt      = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
    qc_report       = qc_report,
    rescale_factors = rescale_factors
  )
}
