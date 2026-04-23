#--------------------------------------
# wealth.R
#
# Imputes tax-unit wealth from SCF 2022
# via a joint distributional random
# forest over the 23-dim vector:
#   13 assets + 6 debts + 4 unrealized
#   capital-gains fields.
#
# The Y schema mirrors
# Wealth-Tax-Simulator/src/data.R:28-84
# so downstream consumers can wire in
# mechanically.
#
# Stage architecture (see CLAUDE.md
# "Wealth imputation"):
#   Stage 1 — SCF PEU → tax-unit split
#             (upstream; produces
#              `scf_tax_units`)
#   Stage 2 — this file: joint DRF
#             draws Y from SCF
#   Stage 3 — DFA / top-tail / aging
#             benchmarking (deferred)
#
# Feature spec: "large" from the X-ablation
# (src/eda/test_wealth_X_ablation*, 2026-04-22).
# 19 features with dual-binary encoding for
# variables that can go negative (income,
# business, capital_gains).
#
# FourierMMD num.features = 50 (up from
# drf default 10) — reduces training-seed
# variance ~1.5x for our heavy-tailed Y.
#--------------------------------------

stage1_cache = 'resources/cache/scf_tax_units.rds'
if (!exists('scf_tax_units', inherits = TRUE) && file.exists(stage1_cache)) {
  message('wealth.R: loading scf_tax_units from Stage 1 cache at ', stage1_cache)
  scf_tax_units = read_rds(stage1_cache)
}

if (!exists('scf_tax_units', inherits = TRUE)) {

  message('wealth.R: `scf_tax_units` not available (Stage 1 has not been run). ',
          'Run src/imputations/stage1_scf_tax_units.R first. ',
          'Skipping wealth imputation; pipeline continues without wealth vars.')

} else {

  #---------------------------------------------------------------------------
  # Y schema (23-dim). Must match Wealth-Tax-Simulator/src/data.R:28-84.
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

  # Raw-SCFP → canonical Y mapping. If Stage 1 already renamed the raw SCF
  # fields into the canonical Y names, skip the transform.
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

  #---------------------------------------------------------------------------
  # Feature schema — "large" spec validated in the X-ablation
  # (src/eda/test_wealth_X_ablation*, 2026-04-22). 19 features:
  #
  #   - dual-binary income encoding (has_income_act + has_income_pos)
  #   - age_older / age_younger (labeling-invariant, SCF capped at 80 to
  #     match PUF topcode)
  #   - n_dep_hh (household kids only — PUF-side reconstructed from
  #     dep_age1/2/3 < 18, SCF-side aliased to KIDS_u)
  #   - pctile + has_* for each income component SCF stage1 carries
  #     through: wages, business (dual-binary), int_div, capital_gains
  #     (dual-binary), ss_pens
  #
  # rent and ui_other intentionally dropped on conceptual-gap grounds
  # (SCFP cash-basis rent vs PUF tax-basis; SCFP TRANSFOTHINC bundles
  # more than PUF `ui`).
  #---------------------------------------------------------------------------

  features = c('has_income_act', 'has_income_pos', 'pctile_income',
               'married', 'age_older', 'age_younger', 'n_dep_hh',
               'pctile_wages',         'has_wages',
               'pctile_business',      'has_business_act',
                                       'has_business_pos',
               'pctile_int_div',       'has_int_div',
               'pctile_capital_gains', 'has_capital_gains_act',
                                       'has_capital_gains_pos',
               'pctile_ss_pens',       'has_ss_pens')

  # Ordinal has_* for components that can't go negative (wages, int_div,
  # ss_pens). Components that can go negative (income, business,
  # capital_gains) use the dual-binary encoding: has_*_act distinguishes
  # zero from nonzero, has_*_pos distinguishes positive from negative
  # within nonzero. Ordinal -1/0/1 was tried first but forced the DRF to
  # lump {neg, zero} or {zero, pos} and missed the wealth signal in
  # loss-holders (see ablation memo).
  make_has = function(x) {
    case_when(x >  0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)
  }

  #---------------------------------------------------------------------------
  # Training frame from SCF tax units: feature engineering on the SCF side
  #---------------------------------------------------------------------------

  # Safety: require the income-composition columns stage1 carries through.
  stopifnot(all(c('wages_scf', 'business_scf', 'int_div_scf',
                  'capital_gains_scf', 'rent_scf',
                  'ss_pens_scf', 'ui_other_scf') %in% names(scf_tax_units)))

  scf_tax_units = scf_tax_units %>%
    mutate(
      # SCF n_dep is already household-kids-only (KIDS_u from roster; see
      # stage1_scf_tax_units.R:496). Alias as n_dep_hh so the DRF feature
      # name matches the PUF-side kids-only reconstruction below.
      n_dep_hh = n_dep,

      # Age harmonization: cap at 80 (PUF IRS topcode), then recode as
      # age_older / age_younger (max / min of primary and spouse ages,
      # singles get age_younger = 0) to remove the primary-filer vs
      # respondent labeling asymmetry across datasets.
      age1_capped = pmin(as.integer(age1), 80L),
      age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                            NA_integer_),
      age_older   = if_else(!is.na(age2_capped),
                            pmax(age1_capped, age2_capped), age1_capped),
      age_younger = if_else(!is.na(age2_capped),
                            pmin(age1_capped, age2_capped), 0L),

      # Unfloor stage1's INCOME (which is SCFP's published INCOME with a
      # floor at 0) by reconstructing from the 7 component columns. Lets
      # ~0.2% of SCF tax units with net losses land in the negative
      # bucket, matching PUF's ~1% after the income-expression fix.
      income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
               rent_scf + ss_pens_scf + ui_other_scf,
      has_income_act = as.integer(income != 0),
      has_income_pos = as.integer(income >  0),
      pctile_income  = compute_percentile(income, weight),

      has_wages             = make_has(wages_scf),
      pctile_wages          = compute_percentile(wages_scf, weight),

      # Dual-binary for business and capital_gains (variables that can
      # go negative); see ablation memo for rationale.
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

  # Bootstrap-expand to n_boot rows sampled with probability proportional
  # to SCF weight. Under unweighted MMD this gives each tree a roughly
  # uniform-weight training pool where heavy rows appear with their
  # population-relative frequency. Keeps tree structure fine-grained
  # (unlike weighted MMD, which carves tight leaves around heavy rows)
  # while still producing correct donor-draw probabilities in expectation.
  # n_boot = 250k is large enough that each heavy row's per-tree copy count
  # (~3-4) stays well below min.node.size (default 20), so leaf pooling is
  # automatic — no need to bump min.node.size.
  n_boot = 250000
  boot_probs = scf_training$weight / sum(scf_training$weight)
  boot_idx   = sample.int(nrow(scf_training), size = n_boot,
                          replace = TRUE, prob = boot_probs)
  scf_boot   = scf_training[boot_idx, ]

  Y_mat = as.matrix(scf_boot[wealth_y_vars])
  X_mat = as.matrix(scf_boot[features])

  #---------------------------------------------------------------------------
  # Joint DRF over the full 23-dim Y
  #
  # Single model, single draw per tax unit. Donor sampling preserves the full
  # SCF joint distribution of (assets, debts, kg) by assigning one real
  # SCF household's vector per PUF unit. No share-normalization step: levels
  # are drawn directly, which is necessary because net worth can be negative
  # (debts exceed assets) and share-decomposition would break.
  #---------------------------------------------------------------------------

  # num.features = 50 (up from drf default of 10) for the FourierMMD random
  # Fourier basis. Reduces training-seed variance by ~1.5x (empirically
  # tested on large spec, 5 seeds: SD went from $7.7T to $5.1T). Key
  # variance lever given our heavy-tailed 23-dim Y.
  wealth_drf = train_or_load_drf(
    name         = 'wealth_drf',
    X            = X_mat,
    Y            = Y_mat,
    num.features = 50
  )

  #---------------------------------------------------------------------------
  # PUF-side feature construction (large spec)
  #
  # Income expression mirrors SCFP INCOME (= WAGEINC + BUSSEFARMINC +
  # INTDIVINC + KGINC + SSRETINC + TRANSFOTHINC + RENTINC) so PUF's
  # pctile_income is computed on the same concept stage1 uses on SCF.
  # Adds farm, kg_lt + kg_st, estate - estate_loss, exempt_int, and ui
  # relative to the prior (consumption.R-compatible) income expression —
  # the fixes that let PUF income legitimately go negative for filers
  # with realized losses.
  #
  # Business concept mirrors SCFP BUSSEFARMINC:
  #   sole_prop + farm + S-corp (net active+passive, net of loss/179)
  #   + partnership (net active+passive, net of loss/179)
  #---------------------------------------------------------------------------

  # Safety: require all raw composition inputs. Filers get these from
  # process_puf; nonfilers get 0-fill from impute_nonfilers.R.
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
  missing_cols = setdiff(puf_raw_inputs, names(tax_units))
  if (length(missing_cols) > 0)
    stop('wealth.R: missing PUF composition inputs: ',
         paste(missing_cols, collapse = ', '))

  puf = tax_units %>%
    mutate(
      # Age harmonization + labeling-invariant recoding (see SCF side).
      age1_capped = pmin(as.integer(age1), 80L),
      age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L),
                            NA_integer_),
      age_older   = if_else(!is.na(age2_capped),
                            pmax(age1_capped, age2_capped), age1_capped),
      age_younger = if_else(!is.na(age2_capped),
                            pmin(age1_capped, age2_capped), 0L),

      married = as.integer(!is.na(male2)),

      # Household-kids-only dep count (see SCF side alias). Pattern matches
      # src/imputations/mortgage.R:78-80 and childcare.R.
      n_dep_hh = (!is.na(dep_age1) & dep_age1 < 18) +
                 (!is.na(dep_age2) & dep_age2 < 18) +
                 (!is.na(dep_age3) & dep_age3 < 18),

      # SCFP-matching income expression.
      income  = wages +
                # BUSSEFARMINC
                sole_prop + farm +
                scorp_active  - scorp_active_loss  - scorp_179 +
                scorp_passive - scorp_passive_loss +
                part_active   - part_active_loss   - part_179 +
                part_passive  - part_passive_loss +
                # INTDIVINC
                txbl_int + exempt_int + div_ord + div_pref +
                # KGINC
                kg_lt + kg_st +
                # SSRETINC
                gross_ss + gross_pens_dist +
                # TRANSFOTHINC (PUF only captures UI on this bundle)
                ui +
                # RENTINC
                rent - rent_loss + estate - estate_loss,

      # Component-specific reconstructions.
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
  # Donor sampling via sparse leaf traversal.
  #
  # Standard DRF predict (drf::get_sample_weights) returns a dense
  # n_pred × n_boot weight matrix W with
  #     W[i, j] = (1/T) Σ_t  1[j ∈ leaf(t, i)] / |leaf(t, i)|,
  # and donor_i ~ Categorical(W[i, :]) is then drawn in R with
  # apply(W, 1, sample.int(prob = …)). That's O(n_boot) work per PUF row
  # plus an 8 GB per-batch dense matrix.
  #
  # Equivalent, but cheaper: marginalizing the tree index recovers W, so
  #   (1) pick tree t ∼ Uniform(1..T),
  #   (2) walk row i down tree t to its leaf,
  #   (3) draw one training row uniformly from that leaf
  # samples the same distribution. Per-row work drops from O(n_boot) to
  # O(tree_depth + leaf_size).
  #
  # Validated against the dense method in src/eda/bench_sparse_sample.R
  # (per-category KS D < 0.025, p > 0.5 for all 23 Y vars).
  #---------------------------------------------------------------------------

  # Extract forest internals once; inner-loop list lookups would dominate.
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
      # Ground-truth leaf check: leaf_samples is non-empty at leaves, empty
      # at internal nodes.
      if (length(ll[[node]]) > 0L) return(node)
      xv = x_row[v[node] + 1L]; sv = s[node]
      # NA safety: PUF features should be fully populated post
      # demographics.R, but default NA splits to the left child defensively
      # rather than erroring.
      node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L else R[node] + 1L
    }
  }

  X_puf     = as.matrix(puf[, features])
  n_pred    = nrow(X_puf)
  tree_pick = sample.int(n_trees, size = n_pred, replace = TRUE)
  donors    = integer(n_pred)

  # Under bootstrap expansion each training row has (implicit) weight 1,
  # so uniform leaf sampling already produces weight-proportional donor
  # draws in expectation. The heavy rows' effective probability = their
  # bootstrap-copy count in the leaf, which tracks their population share.
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

  puf_wealth = bind_cols(tibble(id = puf$id), as_tibble(donor_y))

  tax_units %<>% left_join(puf_wealth, by = 'id')

  #---------------------------------------------------------------------------
  # Diagnostics cache (written only when the caller opts in, matching the
  # consumption.R convention).
  #---------------------------------------------------------------------------

  if (exists('save_wealth_diagnostics') && save_wealth_diagnostics) {
    write_rds(list(puf_wealth = puf_wealth,
                   Y_mat      = Y_mat,
                   X_mat      = X_mat,
                   features   = features,
                   y_vars     = wealth_y_vars),
              'resources/cache/wealth_diagnostics.rds')
  }

  rm(puf, puf_wealth, scf_training, scf_boot, Y_mat, X_mat, donors,
     donor_y, boot_idx, boot_probs, n_boot, wealth_drf, scf_to_y,
     wealth_asset_vars, wealth_debt_vars, wealth_kg_vars, wealth_y_vars,
     features, X_puf, tree_pick, n_pred, n_trees, root,
     child_L, child_R, split_v, split_s, leaves, walk_to_leaf)

}
