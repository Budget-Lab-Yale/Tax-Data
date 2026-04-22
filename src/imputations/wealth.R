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
# Phase 2a (current): CEX-minimal X.
# Phase 2b adds capital-income
# conditioners, gated on the coverage
# analysis in src/eda/scf_puf_coverage.R.
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
  # Feature schema (Phase 2a: CEX-minimal).
  #
  # Phase 2b expansion waits on src/eda/scf_puf_coverage.R verdicts before
  # adding capital-income conditioners (txbl_int, div_pref, sole_prop, ...).
  # See config/variable_guide/baseline.csv for the full PUF-side menu.
  #---------------------------------------------------------------------------

  features = c('has_income', 'pctile_income', 'married', 'age1', 'n_dep')

  #---------------------------------------------------------------------------
  # Training frame from SCF tax units
  #---------------------------------------------------------------------------

  # Fill in features if Stage 1 didn't already compute them.
  if (!'has_income' %in% names(scf_tax_units)) {
    scf_tax_units$has_income = case_when(
      scf_tax_units$income >  0 ~ 1,
      scf_tax_units$income == 0 ~ 0,
      TRUE                      ~ -1
    )
  }
  if (!'pctile_income' %in% names(scf_tax_units)) {
    scf_tax_units$pctile_income = compute_percentile(scf_tax_units$income,
                                                     scf_tax_units$weight)
  }

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

  wealth_drf = train_or_load_drf(
    name = 'wealth_drf',
    X    = X_mat,
    Y    = Y_mat
  )

  #---------------------------------------------------------------------------
  # PUF-side feature construction
  #
  # Income definition mirrors src/imputations/consumption.R so the same
  # pctile_income grid anchors both imputations. Keep in sync if
  # consumption.R changes.
  #---------------------------------------------------------------------------

  puf = tax_units %>%
    mutate(
      married = as.integer(!is.na(male2)),
      income  = wages + sole_prop + part_active + part_passive -
                part_active_loss - part_passive_loss - part_179 +
                scorp_active + scorp_passive -
                scorp_active_loss - scorp_passive_loss - scorp_179 +
                gross_ss + txbl_int + div_ord + div_pref +
                gross_pens_dist + rent - rent_loss,
      has_income = case_when(
        income >  0 ~ 1,
        income == 0 ~ 0,
        TRUE        ~ -1
      ),
      pctile_income = compute_percentile(income, weight)
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
