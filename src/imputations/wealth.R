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
#   tibble keyed by puf_tax_units$id with 23 wealth columns populated.
#--------------------------------------
run_wealth_imputation = function(puf_tax_units, scf_tax_units) {

  #---------------------------------------------------------------------------
  # Schema + helpers
  #---------------------------------------------------------------------------

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

  features = c('has_income_act', 'has_income_pos', 'pctile_income',
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

  wealth_drf = train_or_load_drf(
    name         = 'wealth_drf',
    X            = X_mat,
    Y            = Y_mat,
    num.features = 50
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
    ) %>%
    select(id, all_of(features))

  #---------------------------------------------------------------------------
  # Stage 2: sparse donor sampling (pick tree → walk leaf → uniform draw).
  # See pre-refactor wealth.R for the equivalence proof vs DRF's dense
  # sample-weights path.
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
  donors    = integer(n_pred)

  t0 = Sys.time()
  for (i in seq_len(n_pred)) {
    t  = tree_pick[i]
    nd = walk_to_leaf(t, X_puf[i, ])
    lr = leaves[[t]][[nd]] + 1L
    donors[i] = as.integer(lr[sample.int(length(lr), 1L)])
  }
  cat(sprintf('wealth.R: sparse donor sampling over %d rows: %.1f s\n',
              n_pred, as.numeric(Sys.time() - t0, units = 'secs')))

  donor_y = Y_mat[donors, , drop = FALSE]
  colnames(donor_y) = wealth_y_vars

  bind_cols(tibble(id = puf$id), as_tibble(donor_y))
}
