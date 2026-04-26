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
source('src/imputations/swap_solver.R')


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
run_wealth_imputation = function(puf_tax_units, scf_tax_units,
                                  debug_output         = FALSE,
                                  stage3_method        = NULL,
                                  min_node_size        = NULL,
                                  swap_options         = list(),
                                  n_restarts           = 1L,
                                  init_donors_override = NULL) {

  # swap_options: list of args forwarded to solve_swap_bucket (objective,
  # anneal_T0, anneal_iters, proposal_strategy, max_iters, n_trials_stuck,
  # tol_rel). Has no effect when stage3_method == 'tilt'.
  # n_restarts: > 1 → use solve_swap_bucket_multistart (best of N seeds).
  # init_donors_override: if non-NULL, an integer vector of donor indices
  #   (length n_puf) used to initialize the swap solver. Lets the harness
  #   warm-start swap from tilt's output. Indices are scf_boot rows.
  stopifnot(is.list(swap_options), n_restarts >= 1L)

  # Stage 3 Step A method dispatch.
  #   'tilt' — exponential-tilt donor selection (legacy). Hits soft count
  #            targets via convex Newton on λ; ESS collapses when λ goes
  #            extreme, leaving ~15-30% of (cell × age × cat) buckets
  #            outside ±5% count match.
  #   'swap' — randomized swap-based reassignment (new). Hits per-bucket
  #            counts EXACTLY when the leaf donor pool supports it;
  #            decomposes by (cell × age) for parallelism. See
  #            src/imputations/swap_solver.R.
  # Default: env var WEALTH_STAGE3_METHOD if set, else 'tilt' (legacy
  # production behavior). Switch to 'swap' as the default after the
  # comparison harness validates swap on count match + top shares + joint
  # correlation — flipping the default silently changes results for every
  # caller (main.R, wealth_harness.R, etc.) so don't preempt that decision.
  if (is.null(stage3_method)) {
    e = Sys.getenv('WEALTH_STAGE3_METHOD')
    stage3_method = if (nzchar(e)) e else 'tilt'
  }
  stopifnot(stage3_method %in% c('tilt', 'swap'))

  # Per-cell DRF leaf-size knob. Bigger leaves = larger donor pool per leaf
  # (helps swap reach feasibility on tight buckets) at cost of marginal
  # Y|X precision. Default 20 matches the production cache.
  if (is.null(min_node_size)) {
    e = Sys.getenv('WEALTH_MIN_NODE_SIZE')
    min_node_size = if (nzchar(e)) as.integer(e) else 20L
  }
  stopifnot(min_node_size >= 1L)

  cat(sprintf('wealth.R: Stage 3 method=%s  min.node.size=%d\n',
              stage3_method, min_node_size))

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

    # Cache name suffix encodes min.node.size when non-default so different
    # leaf sizes don't collide on the same cache file.
    cache_suffix = if (min_node_size != 20L)
                     paste0('_mns', min_node_size) else ''
    drf_cell = train_or_load_drf(
      name          = paste0('wealth_percell_', ci, cache_suffix),
      X             = as.matrix(scf_boot_list[[ci_idx]][features]),
      Y             = as.matrix(scf_boot_list[[ci_idx]][wealth_y_vars]),
      num.features  = 50,
      min.node.size = as.integer(min_node_size),
      mtry          = 10L
    )
    f_cell_list[[ci]] = extract_forest(drf_cell)
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

  # ---------- Pre-tilt donors (UNIFORM leaf draw) ----------
  # Uniform within-leaf sampling: the per-cell weighted bootstrap already
  # encodes population weights structurally (low-weight donors appear
  # proportionally in the bootstrap), so each leaf's empirical composition
  # is already population-weighted. Uniform draw at inference correctly
  # samples F_pop(Y | X ∈ leaf). This matches src/eda/wealth_percell_*
  # which validated +0.4% aggregate on SCF self-test.
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
                        max_bt = 30,
                        base_weights = NULL) {
    n = length(records); k = ncol(F_mat)
    ls       = lengths(leaf_donors)
    all_d    = unlist(leaf_donors, use.names = FALSE)
    rec_idx  = rep(seq_len(n), times = ls)
    F_raw    = F_mat[all_d, , drop = FALSE]
    w_expand = rep(weights, times = ls)

    # Base distribution within each leaf (prior on donor selection before
    # tilting). If base_weights is given, p_all ∝ base × exp(λ·f) — this
    # is the correct extension of the exp-tilt to a non-uniform base, so
    # that under λ=0 the distribution reduces to weighted leaf sampling
    # rather than uniform. Pass as log so we don't accumulate rounding
    # in the exp.
    if (is.null(base_weights)) {
      log_base = rep(0, length(all_d))
    } else {
      bw = base_weights[all_d]
      stopifnot(all(bw > 0))
      log_base = log(bw)
    }

    # Per-column RMS rescale — invariant change of coordinates on λ that
    # makes the Newton system better-conditioned under mixed dollar/count
    # targets.
    col_scale  = pmax(sqrt(colMeans(F_raw^2)), 1e-12)
    F_all      = sweep(F_raw, 2, col_scale, `/`)
    T_eff      = T_target / col_scale
    t_scale    = pmax(abs(T_eff), 1)

    eval_fit = function(lam) {
      u     = as.vector(F_all %*% lam) + log_base
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

  # Per-cell donor-side category positivity matrix (donors × 6). Used by the
  # swap solver to score Δglobal_err in O(K) per proposal. Cheap precompute,
  # ~5MB for a 850k-donor combined scf_boot.
  cat_pos_matrix = vapply(CALIB_CATEGORIES,
    function(cc) as.integer(donor_cats[[paste0('cat_', cc)]] > 0L),
    integer(nrow(donor_cats)))
  colnames(cat_pos_matrix) = CALIB_CATEGORIES

  # Per-cell Step A: choose post-Step-A donor for each PUF record. Two
  # implementations dispatch on stage3_method; both produce the same shape
  # of output (post_tilt_donors integer vector + a per-bucket diagnostics
  # list). Cells with zero viable targets fall through to the pre-tilt
  # uniform draw.
  post_tilt_donors      = integer(n_pred)
  step_a_diagnostics    = list()
  t0 = Sys.time()

  if (stage3_method == 'tilt') {
    cat('wealth.R: solving per-cell tilts (income × age)\n')

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
                         F_cell, T_cell,
                         base_weights = NULL)

        cat(sprintf(
          '  %-10s × %-9s: n=%6d k=%2d iter=%2d rel=%.2e ESS p10=%.1f\n',
          ci, ca, length(rec_cell), ncol(F_cell),
          res$iter, res$rel,
          as.numeric(quantile(res$ess, 0.10))))
        step_a_diagnostics[[paste(ci, ca, sep = ':')]] = list(
          method = 'tilt', n = length(rec_cell), k = ncol(F_cell),
          iter = res$iter, rel = res$rel,
          ess_p10 = as.numeric(quantile(res$ess, 0.10))
        )

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
  } else {
    # stage3_method == 'swap'
    cat(sprintf('wealth.R: solving per-cell swaps (income × age)  [n_restarts=%d, init=%s, options=%s]\n',
                n_restarts,
                if (is.null(init_donors_override)) 'uniform' else 'override',
                if (length(swap_options) == 0L) '<defaults>'
                  else paste(names(swap_options), unlist(swap_options),
                             sep = '=', collapse = ' ')))

    init_swap = if (is.null(init_donors_override)) pre_tilt_donors
                else as.integer(init_donors_override)
    stopifnot(length(init_swap) == n_pred)

    bucket_idx = 0L
    for (ci in CALIB_INCOME_BUCKETS) {
      for (ca in CALIB_AGE_BUCKETS) {
        bucket_idx = bucket_idx + 1L
        rec_cell = which(puf_cells$cell_income == ci & puf_cells$cell_age == ca)
        if (length(rec_cell) == 0L) next

        kept_cell = kept %>%
          filter(cell_income == ci, cell_age == ca)

        if (nrow(kept_cell) == 0L) {
          post_tilt_donors[rec_cell] = init_swap[rec_cell]
          cat(sprintf('  %-10s × %-9s: n=%6d  [no viable targets → pre-tilt]\n',
                      ci, ca, length(rec_cell)))
          next
        }

        # Subset cat_pos_matrix to the categories with viable targets in
        # this bucket (may be a strict subset of the 6).
        cats_here = kept_cell$category
        cat_pos_bucket = cat_pos_matrix[, cats_here, drop = FALSE]
        target_bucket  = setNames(kept_cell$target_value, cats_here)

        solver_args = c(
          list(
            puf_weights            = puf_w[rec_cell],
            leaf_donors_per_record = leaf_donors_list[rec_cell],
            cat_pos_matrix         = cat_pos_bucket,
            target_counts          = target_bucket,
            init_donors            = init_swap[rec_cell],
            init_seed              = 1000L + bucket_idx
          ),
          swap_options
        )
        res = if (n_restarts > 1L) {
          do.call(solve_swap_bucket_multistart,
                  c(list(n_restarts = n_restarts), solver_args))
        } else {
          do.call(solve_swap_bucket, solver_args)
        }

        cat(sprintf(
          '  %-10s × %-9s: n=%6d k=%2d swaps=%6d props=%6d max_rel=%.2e %s\n',
          ci, ca, length(rec_cell), ncol(cat_pos_bucket),
          res$swaps_accepted, res$total_proposals,
          res$final_max_rel, res$status))

        step_a_diagnostics[[paste(ci, ca, sep = ':')]] = list(
          method = 'swap', n = length(rec_cell), k = ncol(cat_pos_bucket),
          status = res$status,
          swaps_accepted = res$swaps_accepted,
          total_proposals = res$total_proposals,
          final_max_rel = res$final_max_rel,
          final_global_err = res$final_global_err,
          final_residuals = res$final_residuals
        )

        post_tilt_donors[rec_cell] = res$donor_assignment
      }
    }
  }
  cat(sprintf('wealth.R: %s solver %.1fs\n',
              stage3_method, as.numeric(Sys.time() - t0, units = 'secs')))

  post_y = Y_mat[post_tilt_donors, , drop = FALSE]
  colnames(post_y) = wealth_y_vars
  pre_y  = Y_mat[pre_tilt_donors,  , drop = FALSE]
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

  # Snapshot of Step A's output BEFORE Step B's intensive rescale.
  # Useful for diagnostics that need to see what swap (or tilt) achieved on
  # amounts on its own, separate from the deterministic rescale that always
  # forces aggregates to SCF totals.
  post_y_pre_rescale = post_y

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

  # Return a list so the caller can route post-Step-A into module_deltas
  # and save pre-Step-A + qc_report + rescale_factors as diagnostic artifacts.
  # `y_pre_tilt` is named for backward compatibility but holds the pre-Step-A
  # uniform draw regardless of whether stage3_method is 'tilt' or 'swap'.
  result = list(
    y                  = bind_cols(tibble(id = puf$id), as_tibble(post_y)),
    y_pre_tilt         = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
    y_post_step_a_pre_rescale = bind_cols(tibble(id = puf$id),
                                          as_tibble(post_y_pre_rescale)),
    qc_report          = qc_report,
    rescale_factors    = rescale_factors,
    stage3_method      = stage3_method,
    min_node_size      = min_node_size,
    step_a_diagnostics = step_a_diagnostics,
    step_a_donors      = post_tilt_donors,
    pre_step_a_donors  = pre_tilt_donors
  )
  if (debug_output) {
    # Let src/eda/wealth_diagnosis.R trace each PUF record back to the
    # SCF donor it picked. pre_tilt_donors holds indices into scf_boot
    # (250k rows); boot_idx maps scf_boot back to the original SCF
    # tax-unit table.
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
