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
#             23-dim Y vector; uniform
#             donor draw from each leaf
#             gives a starting assignment
#             for Stage 3.
#   Stage 3 (this file) —
#             Step A: joint count + amount
#             swap solver (see
#             src/imputations/swap_solver.R)
#             over 8 wealth categories ×
#             16 (income × age) cells.
#             Step B: per-(cell × category)
#             intensive rescale fixes any
#             residual SCF aggregate gap.
#
# Feature spec: "large" (19 features) with
# dual-binary encoding for sign-bearing
# variables.
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
#   list(y, y_pre_swap, qc_report, rescale_factors) — y keyed by
#   puf_tax_units$id with 23 wealth columns populated.
#--------------------------------------
run_wealth_imputation = function(puf_tax_units, scf_tax_units,
                                  debug_output         = FALSE,
                                  min_node_size        = NULL,
                                  swap_options         = list(),
                                  n_restarts           = 1L,
                                  init_donors_override = NULL) {

  # swap_options: list of args forwarded to solve_swap_bucket (objective,
  # anneal_T0, anneal_iters, proposal_strategy, max_iters, n_trials_stuck,
  # tol_rel, amount_weight, amount_denom_floor). Defaults give joint
  # count + amount mode at amount_weight = 0.5.
  # n_restarts: > 1 → use solve_swap_bucket_multistart (best of N seeds).
  # init_donors_override: if non-NULL, an integer vector of donor indices
  #   (length n_puf) used to initialize the swap solver instead of the
  #   uniform leaf draw. Lets a harness warm-start from a non-uniform
  #   starting assignment.
  stopifnot(is.list(swap_options), n_restarts >= 1L)

  # Per-cell DRF leaf-size knob. Bigger leaves = larger donor pool per leaf
  # (helps reach feasibility on tight buckets) at cost of marginal Y|X
  # precision. Production default is 50 — see swap_results.html for the
  # forest-size sweep that picked this.
  if (is.null(min_node_size)) {
    e = Sys.getenv('WEALTH_MIN_NODE_SIZE')
    min_node_size = if (nzchar(e)) as.integer(e) else 50L
  }
  stopifnot(min_node_size >= 1L)

  cat(sprintf('wealth.R: Stage 3 swap solver, min.node.size=%d\n',
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
  #   Step A — joint count + amount swap solver.
  #     Each PUF record starts with a uniform random pick from its forest
  #     leaf. The swap solver then proposes (record, new_leaf_donor) swaps
  #     and accepts iff the bucket-global count + amount error decreases.
  #     8 categories {nw, equities, bonds, homes, retirement, business,
  #     other, debt} × 16 (cell × age) buckets = 128 buckets, ~105 kept
  #     after QC. See src/imputations/swap_solver.R.
  #   Step B — per-(cell × category) intensive rescale.
  #     For each cell and each category C ∈ {equities, bonds, homes,
  #     retirement, business, other, debt}, compute
  #        s = SCF_total(cell, C) / PUF_post_step_a_total(cell, C)
  #     and multiply every underlying Y-var of C on every record in the
  #     cell by s. With the joint Step A above, the median rescale factor
  #     is ~1.0 — Step B is a residual cleanup, not a load-bearing
  #     calibrator. NW isn't rescaled directly (assets − debts; falls out).
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

  # ---------- Donor-side category values + 6-column feature matrix -----------
  # Extensive-only: 6 binary columns, one per category.
  donor_y_tbl = as.data.frame(Y_mat)
  names(donor_y_tbl) = wealth_y_vars
  donor_cats = compute_category_values(donor_y_tbl)

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

  # ---------- Target viability QC (extensive margin, count targets) --------
  # Amount targets are constructed below from SCF aggregates and don't
  # need a separate viability pass — they're computed only for cats that
  # passed the count QC, and only when amount_weight > 0.
  requested_spec = default_wealth_target_spec() %>%
    filter(margin == 'extensive')
  qc_report = assess_target_viability(requested_spec, scf_cells_df)
  summarize_qc(qc_report)
  kept = qc_report %>% filter(status == 'keep') %>%
    mutate(feature_col = paste(category, margin, sep = '.'))

  # ---------- Initial donor assignment (uniform leaf draw) -----------------
  # The per-cell weighted bootstrap already encodes SCF population weights
  # structurally (low-weight donors appear proportionally in the
  # bootstrap), so each leaf's empirical composition is already
  # population-weighted. A uniform within-leaf draw at inference correctly
  # samples F_pop(Y | X ∈ leaf). The swap solver below then refines this
  # initial assignment toward count + amount targets.
  pre_swap_donors = integer(n_pred)
  for (i in seq_len(n_pred)) {
    lr = leaf_donors_list[[i]]
    pre_swap_donors[i] = lr[sample.int(length(lr), 1L)]
  }

  # Per-cell donor-side category positivity matrix (donors × 8). Used by
  # the swap solver to score Δglobal_err in O(K) per proposal. Cheap
  # precompute, ~5MB for a 850k-donor combined scf_boot.
  cat_pos_matrix = vapply(CALIB_CATEGORIES,
    function(cc) as.integer(donor_cats[[paste0('cat_', cc)]] > 0L),
    integer(nrow(donor_cats)))
  colnames(cat_pos_matrix) = CALIB_CATEGORIES

  # Donor-side category AMOUNT matrix (donors × 7). Indexed by the same 7
  # categories that Step B rescales (equities, bonds, homes, retirement,
  # business, other, debt). nw is intentionally excluded — it's a linear
  # combination of the other 7 (assets − debts), so including it in the
  # joint amount objective would double-weight the asset/debt balance.
  AMOUNT_CATS = setdiff(CALIB_CATEGORIES, 'nw')
  cat_amount_matrix = vapply(AMOUNT_CATS, function(cc) {
    members = CAT_MEMBERS[[cc]]
    if (length(members) == 1L) donor_y_tbl[[members]]
    else                       rowSums(donor_y_tbl[, members, drop = FALSE])
  }, numeric(nrow(donor_y_tbl)))
  colnames(cat_amount_matrix) = AMOUNT_CATS

  # Default amount_weight = 0.5 enables joint count + amount mode (the
  # production setting). Override via swap_options$amount_weight = 0 to get
  # count-only behavior for diagnostics / comparisons.
  amount_weight       = swap_options$amount_weight       %||% 0.5
  amount_denom_floor  = swap_options$amount_denom_floor  %||% 1e6

  # Per-cell Step A. Cells with zero viable count targets after QC fall
  # through to the uniform initial assignment for those records.
  post_swap_donors   = integer(n_pred)
  step_a_diagnostics = list()
  t0 = Sys.time()

  cat(sprintf(
    'wealth.R: solving per-cell swaps  [n_restarts=%d, init=%s, options=%s]\n',
    n_restarts,
    if (is.null(init_donors_override)) 'uniform' else 'override',
    if (length(swap_options) == 0L) '<defaults>'
      else paste(names(swap_options), unlist(swap_options),
                 sep = '=', collapse = ' ')))

  init_swap = if (is.null(init_donors_override)) pre_swap_donors
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
        post_swap_donors[rec_cell] = init_swap[rec_cell]
        cat(sprintf('  %-10s × %-9s: n=%6d  [no viable targets → uniform init]\n',
                    ci, ca, length(rec_cell)))
        next
      }

      # Subset cat_pos_matrix to the categories with viable count targets
      # in this bucket (may be a strict subset of the 8).
      cats_here      = kept_cell$category
      cat_pos_bucket = cat_pos_matrix[, cats_here, drop = FALSE]
      target_bucket  = setNames(kept_cell$target_value, cats_here)

      # Amount targets from SCF aggregates for the non-nw cats present
      # in this bucket. Per-bucket amount_denom_floor avoids the
      # solver burning iterations on small-target cells: floor =
      # max($1B, 0.1% × sum of bucket amount targets), so cells whose
      # SCF amount target is sub-floor get capped at the floor instead
      # of contributing massive relative-error spikes.
      amount_args = list()
      if (amount_weight > 0) {
        amt_cats = intersect(cats_here, AMOUNT_CATS)
        if (length(amt_cats) > 0L) {
          scf_mask = scf_cells_df$cell_income == ci &
                     scf_cells_df$cell_age == ca
          scf_w_b  = scf_cells_df$weight[scf_mask]
          tgt_amt  = vapply(amt_cats, function(cc) {
            members = CAT_MEMBERS[[cc]]
            vals = if (length(members) == 1L) scf_y[scf_mask, members]
                   else rowSums(scf_y[scf_mask, members, drop = FALSE])
            sum(scf_w_b * vals)
          }, numeric(1))
          names(tgt_amt) = amt_cats
          bucket_floor = max(1e9, 0.001 * sum(abs(tgt_amt)))
          amount_args = list(
            cat_amount_matrix  = cat_amount_matrix[, amt_cats, drop = FALSE],
            target_amounts     = tgt_amt,
            amount_denom_floor = bucket_floor
          )
        }
      }

      # Strip amount_denom_floor from swap_options if the user passed it
      # globally — the bucket-adaptive value in amount_args takes
      # precedence and a duplicate named arg would error in do.call.
      swap_opts_eff = swap_options
      swap_opts_eff$amount_denom_floor = NULL

      solver_args = c(
        list(
          puf_weights            = puf_w[rec_cell],
          leaf_donors_per_record = leaf_donors_list[rec_cell],
          cat_pos_matrix         = cat_pos_bucket,
          target_counts          = target_bucket,
          init_donors            = init_swap[rec_cell],
          init_seed              = 1000L + bucket_idx
        ),
        amount_args,
        swap_opts_eff
      )
      res = if (n_restarts > 1L) {
        do.call(solve_swap_bucket_multistart,
                c(list(n_restarts = n_restarts), solver_args))
      } else {
        do.call(solve_swap_bucket, solver_args)
      }

      amt_str = if (!is.null(res$final_amount_max_rel) &&
                     !is.na(res$final_amount_max_rel))
                  sprintf(' amt_max_rel=%.2e', res$final_amount_max_rel)
                else ''
      cat(sprintf(
        '  %-10s × %-9s: n=%6d k=%2d swaps=%6d props=%6d max_rel=%.2e%s %s\n',
        ci, ca, length(rec_cell), ncol(cat_pos_bucket),
        res$swaps_accepted, res$total_proposals,
        res$final_max_rel, amt_str, res$status))

      step_a_diagnostics[[paste(ci, ca, sep = ':')]] = list(
        n = length(rec_cell), k = ncol(cat_pos_bucket),
        status = res$status,
        swaps_accepted = res$swaps_accepted,
        total_proposals = res$total_proposals,
        final_amount_max_rel = res$final_amount_max_rel,
        final_amounts = res$final_amounts,
        final_target_amounts = res$final_target_amounts,
        final_max_rel = res$final_max_rel,
        final_global_err = res$final_global_err,
        final_residuals = res$final_residuals
      )

      post_swap_donors[rec_cell] = res$donor_assignment
    }
  }
  cat(sprintf('wealth.R: swap solver %.1fs\n',
              as.numeric(Sys.time() - t0, units = 'secs')))

  post_y = Y_mat[post_swap_donors, , drop = FALSE]
  colnames(post_y) = wealth_y_vars
  pre_y  = Y_mat[pre_swap_donors,  , drop = FALSE]
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
  # Useful for diagnostics that need to see what the swap solver achieved
  # on amounts on its own, separate from the deterministic rescale that
  # always forces aggregates to SCF totals.
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

        # PUF post-Step-A aggregate
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
  result = list(
    y                         = bind_cols(tibble(id = puf$id), as_tibble(post_y)),
    y_pre_swap                = bind_cols(tibble(id = puf$id), as_tibble(pre_y)),
    y_post_step_a_pre_rescale = bind_cols(tibble(id = puf$id),
                                          as_tibble(post_y_pre_rescale)),
    qc_report          = qc_report,
    rescale_factors    = rescale_factors,
    min_node_size      = min_node_size,
    step_a_diagnostics = step_a_diagnostics,
    post_swap_donors   = post_swap_donors,
    pre_swap_donors    = pre_swap_donors
  )
  if (debug_output) {
    # Let diagnostic harnesses trace each PUF record back to the SCF
    # donor it picked. pre_swap_donors holds indices into scf_boot
    # (~850k rows); boot_idx maps scf_boot back to the original SCF
    # tax-unit table.
    result$pre_swap_donor_boot_idx = pre_swap_donors
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
