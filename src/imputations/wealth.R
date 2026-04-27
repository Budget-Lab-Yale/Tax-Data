#--------------------------------------
# wealth.R
#
# SCF → PUF wealth imputation, runs at
# the SCF donor year (2022) as part of
# Phase 3 (donor-year imputations).
#
# Exports run_wealth_imputation(), a pure
# function that takes a materialized 2022
# PUF tibble + the cached scf_tax_units
# and returns a tibble (id, 23 wealth
# cols) at 2022 values.
#
# Stage architecture:
#   Stage 1 (upstream) — SCF PEU →
#             tax-unit split, produces
#             scf_tax_units.
#   Stage 2 (this file) —
#             per-cell DRFs over the
#             23-dim Y vector. drf gives
#             forest-averaged donor probs
#             p_ij = get_sample_weights().
#   Stage 3 (this file) —
#             Step A: per-bucket
#             calibrated donor-tilt
#             (see src/imputations/
#             tilt_solver.R). For each
#             (income × age) bucket: tilt
#             p_ij by exp(Z_j' lambda),
#             solve lambda for SCF cell
#             targets via L-BFGS-B, sample
#             one donor per record under
#             tilted q with top-K
#             sparsification.
#             Step B: per-(cell × category)
#             intensive rescale closes any
#             residual SCF aggregate gap.
#             Expected near-1.0 factors.
#
# Feature spec: "large" (19 features) with
# dual-binary encoding for sign-bearing
# variables.
#--------------------------------------


source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
source('src/imputations/tilt_solver.R')


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
#   list(y, y_pre_tilt, y_post_tilt_pre_rescale, qc_report,
#        rescale_factors, tilt_diagnostics, post_tilt_donors,
#        pre_tilt_donors). y is a tibble keyed by puf_tax_units$id with
#   the 23 wealth columns populated.
#--------------------------------------
run_wealth_imputation = function(puf_tax_units, scf_tax_units,
                                  debug_output  = FALSE,
                                  min_node_size = NULL,
                                  tilt_options  = list(),
                                  skip_tilt     = FALSE) {

  # skip_tilt = TRUE: bail out after Stage 2's uniform-leaf-draw
  # (pre_tilt_donors). No tilt, no Step B rescale. Returns the same shape
  # of result list with `y` = `y_pre_tilt`, empty diagnostics. Used by
  # eda/diagnose_age_income.R to inspect raw-DRF behavior on new data
  # before deciding on tilt design.

  # tilt_options: list of arguments forwarded to solve_tilt_block /
  # sample_tilt_donors. Recognized fields:
  #   ridge_eta    (default 1e-4)  -- L2 penalty on lambda
  #   max_iters    (default 200)   -- L-BFGS-B max iterations per bucket
  #   tol_rel      (default 0.01)  -- per-target relative-miss tolerance
  #                                   for 'converged' status
  #   amount_denom_floor (default 1e9) -- min |target| in the scaled
  #                                       residual denominator (amount)
  #   q_threshold  (default 1e-6)  -- drop donors with q below this at
  #                                   sampling time
  #   top_k        (default 300)   -- cap to this many donors per record
  #                                   at sampling time
  #   chunk_size   (default 2000)  -- PUF rows per drf::get_sample_weights
  #                                   call
  stopifnot(is.list(tilt_options))
  ridge_eta            = tilt_options$ridge_eta            %||% 1e-4
  tilt_max_iters       = tilt_options$max_iters            %||% 30L
  tilt_tol_rel         = tilt_options$tol_rel              %||% 0.01
  amount_denom_floor   = tilt_options$amount_denom_floor   %||% 1e9
  q_threshold          = tilt_options$q_threshold          %||% 1e-6
  top_k                = tilt_options$top_k                %||% 300L
  chunk_size           = tilt_options$chunk_size           %||% 2000L
  lambda_max           = tilt_options$lambda_max           %||% 7
  use_fallback_uniform = tilt_options$use_fallback_uniform %||% FALSE
  # Wealth-percentile-share targets: at each SCF percentile p in this
  # vector, add a per-bucket target of "dollar mass in cat_nw above the
  # SCF p-th percentile threshold". Default: top 1% and top 0.1%.
  # Set to NULL or empty vector to disable.
  wealth_share_pcts    = tilt_options$wealth_share_pcts    %||% c(0.99, 0.999)

  # Per-cell DRF leaf-size knob. Bigger leaves = larger donor pool per leaf
  # (helps reach feasibility on tight buckets) at cost of marginal Y|X
  # precision. Production default is 50 — see swap_results.html for the
  # forest-size sweep that picked this.
  if (is.null(min_node_size)) {
    e = Sys.getenv('WEALTH_MIN_NODE_SIZE')
    min_node_size = if (nzchar(e)) as.integer(e) else 50L
  }
  stopifnot(min_node_size >= 1L)

  cat(sprintf('wealth.R: Stage 3 tilt solver, min.node.size=%d\n',
              min_node_size))

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
    mutate(income = wages_scf + business_scf + int_div_scf +
                    capital_gains_scf + rent_scf + ss_pens_scf +
                    ui_other_scf) %>%
    select(weight, income, all_of(features), all_of(wealth_y_vars))

  # Per-cell weighted-bootstrap architecture (2026-04-24).
  #
  # Single global forest + weight-prop leaf draw (the prior scheme) produced
  # +22.8% SCF self-consistency error and was structurally leaking heavy-
  # tail donors across middle-income leaves. Per-cell stratification limits
  # that leakage to the cell where top-NW donors actually live, and lets
  # each cell's weighted bootstrap reach low-weight donors because within-
  # cell weight dispersion collapses from ~600× (global) to ~10-20× (most
  # cells) and ~200× (top 0.1%). See src/eda/wealth_cell_sizes.R and the
  # SCF-self test in src/eda/wealth_percell_diagnostic.R (+0.4%).
  #
  # Architecture:
  #   - Assign each SCF row to one of 8 income cells (CALIB_INCOME_BUCKETS).
  #   - Per cell: weighted bootstrap to n_boot_cell rows, train unweighted
  #     DRF with min.node.size=20, mtry=10, num.features=50.
  #   - Per cell: uniform leaf draw at inference (no per-row weights at
  #     sampling — bootstrap already encodes them).
  #   - Combined scf_boot = stacked cell bootstraps; leaf_donors_list
  #     indexes into this combined scf_boot so Stage 3 code runs unchanged.

  scf_cells_training = assign_calibration_cells(
    data.frame(weight = scf_training$weight),
    scf_training$income, scf_training$age_older, scf_training$weight
  )$cell_income

  n_boot_cell = c(
    'pct00to20'    = 100000L, 'pct20to40'    = 100000L,
    'pct40to60'    = 100000L, 'pct60to80'    = 100000L,
    'pct80to90'    = 100000L, 'pct90to99'    = 100000L,
    'pct99to99.9'  = 100000L, 'pct99.9to100' = 150000L
  )

  extract_forest = function(m) list(
    n_trees = m[['_num_trees']],
    root    = sapply(m[['_root_nodes']], identity),
    child_L = lapply(m[['_child_nodes']], `[[`, 1L),
    child_R = lapply(m[['_child_nodes']], `[[`, 2L),
    split_v = m[['_split_vars']],
    split_s = m[['_split_values']],
    leaves  = m[['_leaf_samples']]
  )

  scf_boot_list = vector('list', length(CALIB_INCOME_BUCKETS))
  f_cell_list   = setNames(vector('list', length(CALIB_INCOME_BUCKETS)),
                           CALIB_INCOME_BUCKETS)
  drf_obj_list  = setNames(vector('list', length(CALIB_INCOME_BUCKETS)),
                           CALIB_INCOME_BUCKETS)  # full drf objects, used
                                                    # by Stage 3 tilt for
                                                    # drf::get_sample_weights
  boot_idx_list = vector('list', length(CALIB_INCOME_BUCKETS))

  for (ci_idx in seq_along(CALIB_INCOME_BUCKETS)) {
    ci = CALIB_INCOME_BUCKETS[ci_idx]
    scf_rows_in_cell = which(scf_cells_training == ci)
    cell_w = scf_training$weight[scf_rows_in_cell]

    set.seed(100 + ci_idx)
    boot_in_cell = sample.int(length(scf_rows_in_cell),
                               size = n_boot_cell[[ci]],
                               replace = TRUE, prob = cell_w)
    boot_scf_row = scf_rows_in_cell[boot_in_cell]

    scf_boot_list[[ci_idx]] = scf_training[boot_scf_row, , drop = FALSE]
    boot_idx_list[[ci_idx]] = boot_scf_row

    cat(sprintf('wealth.R: cell %s — SCF rows %d, bootstrap %d\n',
                ci, length(scf_rows_in_cell), n_boot_cell[[ci]]))

    # Cache name encodes min.node.size in the suffix so different leaf
    # sizes (production = 50) don't collide on the same cache file.
    drf_cell = train_or_load_drf(
      name          = paste0('wealth_percell_', ci,
                              '_mns', min_node_size),
      X             = as.matrix(scf_boot_list[[ci_idx]][features]),
      Y             = as.matrix(scf_boot_list[[ci_idx]][wealth_y_vars]),
      num.features  = 50,
      min.node.size = as.integer(min_node_size),
      mtry          = 10L
    )
    f_cell_list[[ci]]   = extract_forest(drf_cell)
    drf_obj_list[[ci]]  = drf_cell
  }

  # Combined scf_boot (all cell bootstraps stacked). Stage 3 code downstream
  # uses this as a flat table with leaf_donors_list indexing into it.
  scf_boot = bind_rows(scf_boot_list)
  Y_mat    = as.matrix(scf_boot[wealth_y_vars])
  boot_idx = unlist(boot_idx_list)  # combined scf_boot row -> SCF row

  # Per-cell offsets into the combined scf_boot (0-indexed for addition).
  cell_offsets = setNames(
    cumsum(c(0L, sapply(scf_boot_list, nrow)))[seq_along(CALIB_INCOME_BUCKETS)],
    CALIB_INCOME_BUCKETS
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
      int_div_puf_raw   = txbl_int + exempt_int + div_ord + div_pref,
      # $100 threshold on PUF int_div only (SCF is unchanged). PUF captures
      # every reportable 1099-INT/DIV dollar, SCF's survey only captures
      # "substantial" holdings. Thresholding the low end harmonizes the
      # has_int_div concept between datasets: PUF share-positive drops
      # from 29.8% → 20.4%, matching SCF's 19.4% to within 1pp, with
      # 0.1% loss of int_div aggregate. The threshold reweight analysis
      # (src/eda/intdiv_threshold_reweight.R) shows this closes ~$16T of
      # the $27T (age × int_div) X-shift.
      int_div_puf       = if_else(int_div_puf_raw > 0 & int_div_puf_raw <= 100,
                                  0, int_div_puf_raw),
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

  # PUF cell assignment (used both here for walk routing and below for
  # Stage 3 cell-keyed operations).
  puf_cells_raw = assign_calibration_cells(
    data.frame(weight = puf$weight),
    puf$income, puf$age_older, puf$weight
  )

  #---------------------------------------------------------------------------
  # Stage 2: walk per-cell forests for PUF. Each PUF record routes by its
  # own income-cell; within the cell, the cell's forest produces the leaf
  # donor pool. Leaf donor indices are shifted by cell_offsets so they
  # index into the combined scf_boot.
  #---------------------------------------------------------------------------

  walk_to_leaf = function(f, t, x_row) {
    L  = f$child_L[[t]]; R  = f$child_R[[t]]
    v  = f$split_v[[t]]; s  = f$split_s[[t]]
    ll = f$leaves[[t]]
    node = f$root[t] + 1L
    repeat {
      if (length(ll[[node]]) > 0L) return(node)
      xv = x_row[v[node] + 1L]; sv = s[node]
      node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L else R[node] + 1L
    }
  }

  X_puf  = as.matrix(puf[, features])
  n_pred = nrow(X_puf)
  leaf_donors_list = vector('list', n_pred)
  tree_pick = integer(n_pred)

  t0 = Sys.time()
  for (ci_idx in seq_along(CALIB_INCOME_BUCKETS)) {
    ci     = CALIB_INCOME_BUCKETS[ci_idx]
    f_cell = f_cell_list[[ci]]
    offset = cell_offsets[[ci]]

    puf_rows_in_cell = which(puf_cells_raw$cell_income == ci)
    if (length(puf_rows_in_cell) == 0L) next

    set.seed(300 + ci_idx)
    local_tree_pick = sample.int(f_cell$n_trees,
                                  size = length(puf_rows_in_cell),
                                  replace = TRUE)

    for (j in seq_along(puf_rows_in_cell)) {
      i  = puf_rows_in_cell[j]
      t  = local_tree_pick[j]
      nd = walk_to_leaf(f_cell, t, X_puf[i, ])
      lr_local = f_cell$leaves[[t]][[nd]] + 1L
      leaf_donors_list[[i]] = offset + lr_local
      tree_pick[i] = t
    }
  }
  cat(sprintf('wealth.R: per-cell leaf walk over %d rows: %.1fs (median leaf size = %d)\n',
              n_pred, as.numeric(Sys.time() - t0, units = 'secs'),
              median(lengths(leaf_donors_list))))

  #---------------------------------------------------------------------------
  # Stage 3: TWO-STEP calibration per cell (income × age).
  #
  #   Step A — calibrated DRF donor-tilt (replaces the prior swap solver).
  #     For each (cell_income × cell_age) bucket, we ask the per-income-cell
  #     DRF for forest-averaged donor probabilities W = drf::get_sample_weights
  #     over its bootstrap, then exponentially tilt these probabilities by a
  #     small set of donor target features Z so that the implied bucket
  #     aggregate hits the SCF cell target. The bucket's tilted q_ij is
  #     row-normalized; one donor per record is sampled from q with top-K +
  #     threshold sparsification (proposal §10).
  #
  #     Donors in income cell ci are shared across (ci, 'nonsenior') and
  #     (ci, 'senior'); the bucket-level mask zeroes W columns of the
  #     opposite-age donors and renormalizes, decoupling the two age
  #     buckets at the cost of a renormalization. Each bucket's λ is then
  #     solved independently by L-BFGS-B (block-diagonal gradient).
  #
  #   Step B — per-(cell × category) intensive rescale (residual cleanup).
  #     For each cell and category C, compute
  #        s = SCF_total(cell, C) / PUF_post_tilt_total(cell, C)
  #     and multiply every Y-var of C on every record in the cell by s.
  #     With the tilt absorbing most of the dollar calibration, factors are
  #     expected to be near 1.0 (vs the prior swap pipeline's [0.10, 1.20]).
  #     NW isn't rescaled directly (assets − debts; falls out).
  #
  # Categories: retirement and business (pass_throughs) are first-class
  # targets, NOT bundled inside "other". The monolithic "other" used to
  # cover 10 y-vars and gave the solver one knob for an aggregate of 10
  # heterogeneous components, leaving an irreducible +30% amount gap.
  # After the split "other" is the residual (8 vars, more homogeneous).
  CAT_MEMBERS = list(
    equities   = 'equities',
    bonds      = 'bonds',
    homes      = 'primary_home',
    retirement = 'retirement',
    business   = 'pass_throughs',
    other      = setdiff(wealth_asset_vars,
                          c('equities', 'bonds', 'primary_home',
                            'retirement', 'pass_throughs')),
    debt       = wealth_debt_vars
  )
  # kg_* rescale follows the inheritance rule (same as in dfa_factors).
  # kg_pass_throughs now follows 'business' (was 'other').
  KG_PARENT = list(
    kg_primary_home  = 'homes',
    kg_other_re      = 'other',
    kg_pass_throughs = 'business',
    kg_other         = 'equities'
  )
  #---------------------------------------------------------------------------

  # ---------- Donor-side category values + cat-level matrices -----------
  donor_y_tbl = as.data.frame(Y_mat)
  names(donor_y_tbl) = wealth_y_vars
  donor_cats = compute_category_values(donor_y_tbl)

  # nw is excluded from amount targets — it's a linear combination of the
  # other 7 cats (assets − debts), so including it in amount targets would
  # double-weight the asset/debt balance. Counts: nw is included (a
  # household has positive nw or not; not directly recoverable from
  # per-category counts).
  AMOUNT_CATS = setdiff(CALIB_CATEGORIES, 'nw')
  COUNT_CATS  = CALIB_CATEGORIES   # all 8 cats can carry an extensive target

  cat_pos_matrix = vapply(COUNT_CATS,
    function(cc) as.integer(donor_cats[[paste0('cat_', cc)]] > 0L),
    integer(nrow(donor_cats)))
  colnames(cat_pos_matrix) = COUNT_CATS

  cat_amount_matrix = vapply(AMOUNT_CATS, function(cc) {
    members = CAT_MEMBERS[[cc]]
    if (length(members) == 1L) donor_y_tbl[[members]]
    else                       rowSums(donor_y_tbl[, members, drop = FALSE])
  }, numeric(nrow(donor_y_tbl)))
  colnames(cat_amount_matrix) = AMOUNT_CATS

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

  # ---------- Donor-side age bucket (for cross-age masking) ---------------
  # scf_boot inherits age_older from scf_training (carried via features).
  # Map the bootstrap-row age to a senior/nonsenior flag, matching the PUF
  # senior cutoff in stage3_target_qc.R.
  scf_boot_age_older = scf_boot$age_older
  scf_boot_is_senior = scf_boot_age_older >= SENIOR_AGE

  # ---------- Target viability QC (extensive AND intensive) ---------------
  # Both margins go through assess_target_viability. Extensive: drops
  # near-zero / near-one fractions (no donor signal). Intensive: drops
  # cells with frac_pos < degen_lo (no positive support to push toward
  # the target). See stage3_target_qc.R for thresholds.
  requested_spec = default_wealth_target_spec()
  qc_report = assess_target_viability(requested_spec, scf_cells_df)
  summarize_qc(qc_report)
  kept = qc_report %>% filter(status == 'keep') %>%
    mutate(feature_col = paste(category, margin, sep = '.'))

  # ---------- Wealth-percentile-share thresholds (SCF-derived) -----------
  # For each requested percentile p, compute the SCF cat_nw threshold —
  # the cutoff above which p×100% of SCF tax units sit in NW. These
  # thresholds are FIXED inputs to the tilt: each donor j contributes
  # cat_nw_j * 1{cat_nw_j > threshold} to the tilt's Z, and the
  # per-bucket target equals the SCF cell's actual dollar mass above
  # that threshold. Sum across buckets gives global SCF top-N share.
  scf_nw_thresholds = numeric(0)
  if (length(wealth_share_pcts) > 0L) {
    scf_nw_thresholds = wtd.quantile(scf_cells_df$cat_nw,
                                     scf_cells_df$weight,
                                     probs = wealth_share_pcts)
    names(scf_nw_thresholds) = paste0('top_', sprintf('%g',
                                       100 * (1 - wealth_share_pcts)))
    cat(sprintf('wealth.R: wealth-share thresholds (SCF):\n'))
    for (k in seq_along(scf_nw_thresholds)) {
      cat(sprintf('  %-12s NW > $%.2fM\n',
                  names(scf_nw_thresholds)[k],
                  scf_nw_thresholds[k] / 1e6))
    }
  }

  # ---------- Pre-tilt diagnostic baseline (uniform leaf draw) ------------
  # Kept ONLY as a diagnostic so harnesses can compare raw-DRF leaf draw
  # vs tilted output. Not fed into the solver.
  pre_tilt_donors = integer(n_pred)
  for (i in seq_len(n_pred)) {
    lr = leaf_donors_list[[i]]
    if (length(lr) > 0L) {
      pre_tilt_donors[i] = lr[sample.int(length(lr), 1L)]
    }
  }

  # ---------- Early exit: skip_tilt mode -----------------------------------
  if (skip_tilt) {
    cat('wealth.R: skip_tilt=TRUE, returning Stage-2 output (uniform leaf draw)\n')
    pre_y = Y_mat[pre_tilt_donors, , drop = FALSE]
    colnames(pre_y) = wealth_y_vars
    dep_rows = which(puf_tax_units$dep_status == 1L)
    if (length(dep_rows) > 0L) {
      cat(sprintf('  zeroing wealth for %d dep_status==1 rows\n', length(dep_rows)))
      pre_y[dep_rows, ] = 0
    }
    return(list(
      y                       = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
      y_pre_tilt              = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
      y_post_tilt_pre_rescale = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
      qc_report               = qc_report,
      rescale_factors         = tibble(),
      tilt_diagnostics        = list(),
      pre_tilt_donors         = pre_tilt_donors,
      post_tilt_donors        = pre_tilt_donors,
      # Back-compat aliases
      y_pre_swap              = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
      y_post_step_a_pre_rescale = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
      step_a_diagnostics      = list(),
      pre_swap_donors         = pre_tilt_donors,
      post_swap_donors        = pre_tilt_donors,
      min_node_size           = min_node_size
    ))
  }

  # ---------- Per-bucket tilt + sample loop -------------------------------
  post_tilt_donors  = integer(n_pred)
  tilt_diagnostics  = list()
  t0 = Sys.time()

  cat(sprintf(
    'wealth.R: solving per-bucket tilt  [ridge_eta=%.0e tol_rel=%.2f max_iters=%d top_k=%d chunk_size=%d]\n',
    ridge_eta, tilt_tol_rel, tilt_max_iters, top_k, chunk_size))

  bucket_idx = 0L
  for (ci in CALIB_INCOME_BUCKETS) {
    drf_obj = drf_obj_list[[ci]]
    offset  = cell_offsets[[ci]]
    n_donor_in_cell = nrow(scf_boot_list[[match(ci, CALIB_INCOME_BUCKETS)]])
    # Donor age flag for THIS cell's bootstrap (column-aligned to W).
    donor_age_global = scf_boot_is_senior[(offset + 1L):(offset + n_donor_in_cell)]

    for (ca in CALIB_AGE_BUCKETS) {
      bucket_idx = bucket_idx + 1L
      rec_cell = which(puf_cells$cell_income == ci & puf_cells$cell_age == ca)
      n_b = length(rec_cell)
      if (n_b == 0L) next

      # Mask donor columns by matching age. Renormalize each row of W to
      # sum to 1 over kept donors. If a record has zero matching-age
      # support (rare), fall back to uniform leaf draw.
      donor_keep_mask = if (ca == 'senior') donor_age_global else !donor_age_global
      donor_keep_idx_local = which(donor_keep_mask)
      if (length(donor_keep_idx_local) == 0L) {
        post_tilt_donors[rec_cell] = pre_tilt_donors[rec_cell]
        cat(sprintf('  %-12s × %-9s: n=%6d  [no matching-age donors → uniform leaf]\n',
                    ci, ca, n_b))
        next
      }

      kept_b = kept %>% filter(cell_income == ci, cell_age == ca)
      if (nrow(kept_b) == 0L) {
        post_tilt_donors[rec_cell] = pre_tilt_donors[rec_cell]
        cat(sprintf('  %-12s × %-9s: n=%6d  [no viable targets → uniform leaf]\n',
                    ci, ca, n_b))
        next
      }

      # Build donor_Z for this bucket: one column per (cat, margin) target
      # in kept_b. Order is preserved so target/scale align with Z's columns.
      n_kept = nrow(kept_b)
      donor_Z = matrix(0, nrow = length(donor_keep_idx_local), ncol = n_kept)
      target_b = numeric(n_kept)
      scale_b  = numeric(n_kept)
      col_names = character(n_kept)
      for (mm in seq_len(n_kept)) {
        cc      = kept_b$category[mm]
        mg      = kept_b$margin[mm]
        col_names[mm] = sprintf('%s.%s', cc, mg)
        if (mg == 'extensive') {
          stopifnot(cc %in% colnames(cat_pos_matrix))
          col_full = cat_pos_matrix[, cc]
        } else {
          if (cc == 'nw') {
            # nw amount column; built fresh from cat_amount_matrix sums
            # (assets − debts). Mostly skipped — nw extensive can't
            # appear in COUNT_CATS as a kept_b row's margin can be
            # 'intensive' for nw too. Build it on the fly to be safe.
            col_full = donor_cats$cat_nw
          } else {
            stopifnot(cc %in% colnames(cat_amount_matrix))
            col_full = cat_amount_matrix[, cc]
          }
        }
        donor_Z[, mm] = col_full[offset + donor_keep_idx_local]
        target_b[mm] = kept_b$target_value[mm]
        scale_b[mm]  = if (mg == 'extensive') {
          max(target_b[mm], 1)
        } else {
          max(abs(target_b[mm]), amount_denom_floor)
        }
      }
      colnames(donor_Z) = col_names
      names(target_b)   = col_names
      names(scale_b)    = col_names

      # ---- Append wealth-percentile-share targets ----
      # For each SCF threshold p, donor j's contribution is
      #   cat_nw_j * 1{cat_nw_j > scf_threshold_p}
      # Per-bucket target = SCF cell × age dollar mass above threshold.
      # Sum of per-bucket targets across all buckets = SCF total
      # dollar mass above threshold = SCF top-N share × SCF total NW.
      if (length(scf_nw_thresholds) > 0L) {
        scf_mask_bucket = scf_cells_df$cell_income == ci &
                          scf_cells_df$cell_age == ca
        scf_w_b   = scf_cells_df$weight[scf_mask_bucket]
        scf_nw_b  = scf_cells_df$cat_nw[scf_mask_bucket]
        donor_nw  = donor_cats$cat_nw[offset + donor_keep_idx_local]

        extra_Z   = matrix(0, nrow = nrow(donor_Z),
                            ncol = length(scf_nw_thresholds))
        extra_tgt = numeric(length(scf_nw_thresholds))
        extra_scl = numeric(length(scf_nw_thresholds))
        extra_nm  = character(length(scf_nw_thresholds))

        for (k in seq_along(scf_nw_thresholds)) {
          thr = scf_nw_thresholds[k]
          extra_Z[, k]    = donor_nw * (donor_nw > thr)
          extra_tgt[k]    = sum(scf_w_b * scf_nw_b * (scf_nw_b > thr))
          extra_scl[k]    = max(abs(extra_tgt[k]), amount_denom_floor)
          extra_nm[k]     = paste0('share.', names(scf_nw_thresholds)[k])
        }
        colnames(extra_Z) = extra_nm
        names(extra_tgt)  = extra_nm
        names(extra_scl)  = extra_nm

        donor_Z = cbind(donor_Z, extra_Z)
        target_b = c(target_b, extra_tgt)
        scale_b  = c(scale_b,  extra_scl)
        col_names = c(col_names, extra_nm)
      }

      # Defensive: any NA in donor_Z would propagate through exp(Z lambda)
      # and break L-BFGS-B. SCFP source fields are usually clean but a
      # stray NA on a debt or kg field can sneak through. Zero them out.
      n_na = sum(!is.finite(donor_Z))
      if (n_na > 0L) {
        cat(sprintf('  [NOTE] %s × %s: %d non-finite donor_Z entries → 0\n',
                    ci, ca, n_na))
        donor_Z[!is.finite(donor_Z)] = 0
      }

      # Predict W for this bucket's PUF records by chunked
      # drf::get_sample_weights, then column-mask to matching-age donors
      # and renormalize. We materialize a [n_b × n_donor_kept] matrix --
      # large but still much smaller than the un-masked [n_b × 100k]
      # version. For n_b > 5k this can exceed several GB; chunk_size
      # controls how much we ask drf for at a time.
      X_b = X_puf[rec_cell, , drop = FALSE]
      chunks = split(seq_len(n_b),
                     ceiling(seq_len(n_b) / chunk_size))
      W_chunks = vector('list', length(chunks))
      t_chunk = Sys.time()
      for (k in seq_along(chunks)) {
        W_raw = drf::get_sample_weights(drf_obj,
                                         newdata = X_b[chunks[[k]], , drop = FALSE])
        # Column-subset BEFORE dense conversion. The dense conversion of
        # a sparse [chunk x 100k] matrix is the dominant per-chunk cost;
        # subsetting first halves the dense allocation and the as.matrix
        # work because we only keep matching-age donor columns (~50% of
        # the bootstrap typically).
        W_raw  = W_raw[, donor_keep_idx_local, drop = FALSE]
        W_full = as.matrix(W_raw)
        rm(W_raw)
        rs = rowSums(W_full)
        # Records with zero matching-age leaf overlap: renormalize falls
        # back to uniform across the matching-age donor pool.
        zero_rows = which(rs <= 0)
        if (length(zero_rows) > 0L) {
          W_full[zero_rows, ] = 1 / ncol(W_full)
          rs[zero_rows] = 1
        }
        W_full = W_full / rs
        W_chunks[[k]] = W_full
        rm(W_full)
      }
      W_b = do.call(rbind, W_chunks)
      rm(W_chunks); gc(verbose = FALSE)
      cat(sprintf('  %-12s × %-9s: predict W [%dx%d] in %.1fs\n',
                  ci, ca, nrow(W_b), ncol(W_b),
                  as.numeric(Sys.time() - t_chunk, units = 'secs')))
      flush.console()

      t_solve = Sys.time()
      tilt_res = solve_tilt_block(
        W           = W_b,
        Z           = donor_Z,
        puf_w       = puf_w[rec_cell],
        target      = target_b,
        scale       = scale_b,
        ridge_eta   = ridge_eta,
        max_iters   = tilt_max_iters,
        tol_rel     = tilt_tol_rel,
        init_lambda = NULL,
        lambda_max  = lambda_max
      )
      cat(sprintf('  %-12s × %-9s: solve %.1fs (iters=%s status=%s)\n',
                  ci, ca,
                  as.numeric(Sys.time() - t_solve, units = 'secs'),
                  as.character(tilt_res$n_iters), tilt_res$status))
      flush.console()

      # Sample one donor per record from tilted q. Top-K + threshold
      # sparsification per record. (Q_at_optimum has the converged q for
      # all bucket records; reuse rather than re-predict.)
      Q_b = tilt_res$Q_at_optimum
      bucket_seed = 1000L + bucket_idx
      set.seed(bucket_seed)
      eff_donors_sampled = numeric(n_b)
      k_used_vec         = integer(n_b)
      for (j in seq_len(n_b)) {
        qrow = Q_b[j, ]
        keep = which(qrow >= q_threshold)
        if (top_k > 0L && length(keep) > top_k) {
          ord  = order(qrow[keep], decreasing = TRUE)
          keep = keep[ord[seq_len(top_k)]]
        }
        if (length(keep) == 0L) keep = which.max(qrow)
        qi = qrow[keep] / sum(qrow[keep])
        local_donor = keep[sample.int(length(keep), 1L, prob = qi)]
        # local_donor is an index into the matching-age donor pool. Map
        # it back to the global scf_boot row index.
        global_donor = offset + donor_keep_idx_local[local_donor]
        post_tilt_donors[rec_cell[j]] = global_donor
        eff_donors_sampled[j] = 1 / sum(qi^2)
        k_used_vec[j]         = length(keep)
      }
      rm(Q_b, W_b); gc(verbose = FALSE)

      attain = attainable_bounds(donor_Z, puf_w[rec_cell])
      out_of_range = (target_b < attain['min', ]) | (target_b > attain['max', ])

      cat(sprintf(
        '  %-12s × %-9s: n=%6d M=%2d max_rel=%.2e ||lambda||=%.2f ESS=%.1f k_used=%.0f %s%s\n',
        ci, ca, n_b, n_kept, tilt_res$max_rel, tilt_res$lambda_norm,
        tilt_res$effective_donors_mean, mean(k_used_vec),
        tilt_res$status,
        if (any(out_of_range)) sprintf(' [%d targets infeasible]',
                                        sum(out_of_range)) else ''))

      tilt_diagnostics[[paste(ci, ca, sep = ':')]] = list(
        n               = n_b,
        n_kept_targets  = n_kept,
        col_names       = col_names,
        target          = target_b,
        scale           = scale_b,
        T_hat           = tilt_res$T_hat,
        residuals       = tilt_res$residuals,
        rel_residuals   = tilt_res$rel_residuals,
        max_rel         = tilt_res$max_rel,
        lambda          = tilt_res$lambda,
        lambda_norm     = tilt_res$lambda_norm,
        status          = tilt_res$status,
        n_iters         = tilt_res$n_iters,
        eff_donors_solver_mean = tilt_res$effective_donors_mean,
        eff_donors_solver_p10  = tilt_res$effective_donors_p10,
        kl_to_uniform_mean     = tilt_res$kl_to_uniform_mean,
        eff_donors_sampled_mean = mean(eff_donors_sampled),
        k_used_mean     = mean(k_used_vec),
        attainable_min  = attain['min', ],
        attainable_max  = attain['max', ],
        out_of_range    = out_of_range
      )
    }
  }
  cat(sprintf('wealth.R: tilt solver %.1fs\n',
              as.numeric(Sys.time() - t0, units = 'secs')))

  post_y = Y_mat[post_tilt_donors, , drop = FALSE]
  colnames(post_y) = wealth_y_vars
  pre_y  = Y_mat[pre_tilt_donors, , drop = FALSE]
  colnames(pre_y)  = wealth_y_vars

  # Dependent returns (dep_status==1) are filed by people claimed as
  # dependents on someone else's return. Their household wealth lives on
  # the parents' return, not theirs — imputing wealth here double-counts.
  # Zero before Step B so the rescale calibrates the survivors to hit
  # the SCF cell aggregate. ~10M tax units (~5% of PUF), mostly 18-29.
  dep_rows = which(puf_tax_units$dep_status == 1L)
  if (length(dep_rows) > 0L) {
    cat(sprintf('wealth.R: zeroing wealth for %d dep_status==1 rows (%.2fM tax units)\n',
                length(dep_rows), sum(puf_w[dep_rows]) / 1e6))
    pre_y[dep_rows,  ] = 0
    post_y[dep_rows, ] = 0
  }

  # Snapshot of Step A (tilt+sample) output BEFORE Step B's intensive
  # rescale. Useful for diagnostics that need to see what the tilt
  # achieved on amounts on its own, separate from the deterministic
  # rescale that always forces aggregates to SCF totals.
  post_y_pre_rescale = post_y

  #---------------------------------------------------------------------------
  # Step B: per-(cell × category) intensive rescale.
  # For each cell c and category C, compute s = SCF_total / PUF_total and
  # multiply every Y-var of C on records in c by s. Also applies to kg_*
  # via KG_PARENT inheritance. Three cases:
  #   (a) puf_total > 0 and scf_total > 0  → multiplicative rescale
  #   (b) puf_total ~ 0 and scf_total > 0  → uniform additive fallback
  #       (otherwise the cell stays at zero forever; this case is the
  #        proposal §16 "tilt too strong" failure mode where tilt
  #        collapsed onto donors with 0 in the category).
  #   (c) scf_total ~ 0  → skip (don't fabricate amounts SCF says aren't
  #       there).
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

        # PUF post-Step-A aggregate
        puf_vals = if (length(members) == 1L) post_y[rec_cell, members]
                   else rowSums(post_y[rec_cell, members, drop = FALSE])
        puf_total = sum(rec_w * puf_vals)

        # Decide which case (a) / (b) / (c) applies.
        scf_alive = abs(scf_total) >= max(1e3, 1e-6 * max(abs(puf_total), 1))
        puf_alive = abs(puf_total) >= max(1e3, 1e-6 * max(abs(scf_total), 1))
        # `fallback_uniform` is gated by use_fallback_uniform (default
        # FALSE in v4 — when tilt is well-behaved, the fallback is
        # unnecessary and inflates counts). When disabled, treat the
        # zero-PUF case as `skip`.
        skip = !scf_alive || (!puf_alive && !use_fallback_uniform)
        fallback_uniform = use_fallback_uniform && scf_alive && !puf_alive
        factor = if (skip || fallback_uniform) 1 else scf_total / puf_total

        if (fallback_uniform) {
          # Distribute scf_total uniformly across rec_cell records, split
          # equally across category members. Each record gets
          #   scf_total / (sum(rec_w) * length(members))
          # ASSIGNED (not multiplied — post_y was zero in this case). The
          # weighted PUF total then comes back to scf_total exactly.
          per_record_per_member = scf_total /
                                  (max(sum(rec_w), 1) * length(members))
          for (m in members) {
            post_y[rec_cell, m] = post_y[rec_cell, m] + per_record_per_member
          }
          for (kv in names(KG_PARENT)) {
            if (KG_PARENT[[kv]] == cat_name) {
              # kg_* fallback: keep at zero (no SCF target for kg_* by cat,
              # and synthesizing kg gains in a frame-mismatch cell would be
              # spurious).
            }
          }
        } else if (!skip) {
          for (m in members) post_y[rec_cell, m] = post_y[rec_cell, m] * factor
          # Apply same factor to kg_* with this parent category.
          for (kv in names(KG_PARENT)) {
            if (KG_PARENT[[kv]] == cat_name)
              post_y[rec_cell, kv] = post_y[rec_cell, kv] * factor
          }
        }

        rescale_rows[[length(rescale_rows) + 1L]] = tibble(
          cell_income           = ci,
          cell_age              = ca,
          category              = cat_name,
          scf_total             = scf_total,
          puf_pre_rescale_total = puf_total,
          factor                = factor,
          applied               = !skip,
          mode                  = if (skip) 'skip'
                                  else if (fallback_uniform) 'fallback_uniform'
                                  else 'multiplicative'
        )
      }
    }
  }
  rescale_factors = bind_rows(rescale_rows)

  # Summary print: distribution of factors. With the tilt absorbing the
  # cell × cat dollar work, factors are expected to be tight around 1.0.
  # Routine deviations outside [0.95, 1.05] are flagged as a tilt-
  # feasibility signal — large factors mean the tilt couldn't reach the
  # target on its own and Step B is doing real work.
  f_applied = rescale_factors %>% filter(applied)
  cat(sprintf(
    'wealth.R: rescale factors applied=%d skipped=%d  min=%.3f p10=%.3f median=%.3f p90=%.3f max=%.3f\n',
    sum(rescale_factors$applied), sum(!rescale_factors$applied),
    min(f_applied$factor), quantile(f_applied$factor, 0.10),
    median(f_applied$factor), quantile(f_applied$factor, 0.90),
    max(f_applied$factor)))
  outside_tight = f_applied %>% filter(factor < 0.95 | factor > 1.05)
  cat(sprintf('  %d / %d factors outside [0.95, 1.05] (tilt-feasibility flag)\n',
              nrow(outside_tight), nrow(f_applied)))
  extreme = f_applied %>% filter(factor < 0.5 | factor > 2.0)
  if (nrow(extreme) > 0) {
    cat(sprintf('  %d factors outside [0.5, 2.0] (very large rescale):\n', nrow(extreme)))
    print(extreme %>% select(cell_income, cell_age, category, factor),
          n = Inf)
  }

  # Return a list so the caller can route post-Step-A into module_deltas
  # and save pre-Step-A + qc_report + rescale_factors as diagnostic artifacts.
  result = list(
    y                         = bind_cols(tibble(id = puf$id), as_tibble(post_y)),
    y_pre_tilt                = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
    y_post_tilt_pre_rescale   = bind_cols(tibble(id = puf$id),
                                          as_tibble(post_y_pre_rescale)),
    qc_report         = qc_report,
    rescale_factors   = rescale_factors,
    min_node_size     = min_node_size,
    tilt_diagnostics  = tilt_diagnostics,
    post_tilt_donors  = post_tilt_donors,
    pre_tilt_donors   = pre_tilt_donors,
    # Back-compat aliases so older harnesses still work (deprecate later).
    y_pre_swap        = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
    y_post_step_a_pre_rescale = bind_cols(tibble(id = puf$id),
                                          as_tibble(post_y_pre_rescale)),
    step_a_diagnostics = tilt_diagnostics,
    post_swap_donors   = post_tilt_donors,
    pre_swap_donors    = pre_tilt_donors
  )
  if (debug_output) {
    # Let diagnostic harnesses trace each PUF record back to the SCF
    # donor it picked. pre_tilt_donors / post_tilt_donors hold indices
    # into scf_boot (~850k rows); boot_idx maps scf_boot back to the
    # original SCF tax-unit table.
    result$pre_tilt_donor_boot_idx = pre_tilt_donors
    result$boot_idx                = boot_idx
    result$scf_boot_income         = scf_boot$income
    result$puf_cells               = puf_cells
    result$scf_cells_df            = scf_cells_df
    # Expose also the forest leaf each PUF record landed in, for the
    # heavy-tail leaf census.
    result$tree_pick               = tree_pick
    result$leaf_donors_list        = leaf_donors_list
  }
  result
}
