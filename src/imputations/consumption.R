#--------------------------------------
# consumption.R
#
# Imputes consumer expenditure using
# CEX data, ranger quantile forests
# for total consumption levels, and
# DRF for expenditure composition.
#--------------------------------------

# The models here impute annual consumption (CEX training data is annualized in cex.R)
# Stage A: Ranger QRF predicts total consumption C, with power-law top-tail adjustment
# Stage B: DRF on household-level share vectors gives composition

source('src/cex.R')
cex_training    = build_cex_training()
elasticity_beta = attr(cex_training, 'elasticity_beta')
y_ref           = attr(cex_training, 'y_ref')

features = c('has_income', 'pctile_income', 'married', 'age1', 'n_dep', 'male1')

# 8 consumption categories (collapsed from 20 BEA PCE for tariff analysis)
pce_cats = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
             'c_food_off_premises', 'c_gasoline', 'c_housing_utilities', 'c_other_services_health')

#---------------------------------------------------------------------------
# Stage A: Train ranger models for total consumption level
#---------------------------------------------------------------------------

consumption_rf = train_or_load_ranger(
  name          = 'consumption_rf',
  formula       = total_consumption ~ .,
  data          = cex_training[c(features, 'total_consumption')],
  case_weights  = cex_training$WT_ANNUAL,
  mtry          = length(features),
  min_node_size = 10
)

#---------------------------------------------------------------------------
# Stage B: DRF composition model on bootstrap-expanded CEX sample
#---------------------------------------------------------------------------

# Bootstrap-expand to n_boot rows sampled with probability proportional to
# CEX weight. Under unweighted MMD this gives each tree a uniform-weight
# training pool while preserving weighted representation in expectation.
# See src/imputations/wealth.R for the rationale (heavy-tail handling).
n_boot = 200000
boot_probs = cex_training$WT_ANNUAL / sum(cex_training$WT_ANNUAL)
boot_idx   = sample.int(nrow(cex_training), size = n_boot,
                        replace = TRUE, prob = boot_probs)
cex_boot   = cex_training[boot_idx, ]

# Compute household-level expenditure shares (normalized over sourced categories)
sourced_total = rowSums(cex_boot[pce_cats])
share_matrix = as.matrix(cex_boot[pce_cats] / sourced_total)
share_matrix[!is.finite(share_matrix)] = 0

# DRF features include total_consumption (the level of C) — composition depends
# on spending level
drf_features = c(features, 'total_consumption')

share_drf = train_or_load_drf(
  name = 'share_drf',
  X    = as.matrix(cex_boot[drf_features]),
  Y    = share_matrix
)

#---------------------------------------------------------------------------
# Predict consumption on PUF tax units
#---------------------------------------------------------------------------

puf = tax_units %>%
  mutate(
    married = as.numeric(!is.na(male2)),
    size = 1 + married + n_dep,
    # Income definition to match CEX training: member-level (wages, self-emp, SS,
    # pensions) + CU-level capital income (interest+dividends, net rent)
    income = wages + sole_prop + part_active + part_passive - part_active_loss -
      part_passive_loss - part_179 + scorp_active + scorp_passive -
      scorp_active_loss - scorp_passive_loss - scorp_179 + gross_ss +
      txbl_int + div_ord + div_pref + gross_pens_dist +
      rent - rent_loss,
    has_income = case_when(
      income >  0 ~ 1,
      income == 0 ~ 0,
      T           ~ -1
    )
  ) %>%
  mutate(
    pctile_income = compute_percentile(income, weight)
  ) %>%
  select(id, weight, male1, age1, married, pctile_income, n_dep, n_dep_ctc,
         income, size, has_income)

# Stochastic quantile predictions from ranger
pred_direct = predict_ranger_draw(consumption_rf, puf[features])

puf = puf %>%
  mutate(
    C = pred_direct,
    # Top-tail adjustment: for PUF units above CEX coverage (Y_ref = CEX P98
    # median income), scale QRF draw by (Y/Y_ref)^beta to extrapolate the
    # consumption-income gradient into the far right tail
    C = ifelse(income > y_ref,
               C * (income / y_ref) ^ elasticity_beta,
               C),
    C = pmax(C, 0)
  )

#---------------------------------------------------------------------------
# Distribute total consumption across 8 PCE categories via DRF donor sampling.
#
# Sparse leaf-traversal sampling: instead of materializing the dense
# n_pred × n_boot weight matrix and running apply+sample.int, pick one
# random tree per row, walk to its leaf, uniformly draw one training row
# from the leaf. Distributionally equivalent to the dense apply+sample.int
# it replaces (marginalizing over tree choice recovers drf's weights).
# Matches the fix applied to src/imputations/wealth.R. See
# src/eda/bench_sparse_sample.R for the equivalence benchmark.
#---------------------------------------------------------------------------

# Use predicted C as total_consumption for DRF feature matching
puf$total_consumption = puf$C

n_trees = share_drf[['_num_trees']]
root    = sapply(share_drf[['_root_nodes']], identity)
child_L = lapply(share_drf[['_child_nodes']], `[[`, 1L)
child_R = lapply(share_drf[['_child_nodes']], `[[`, 2L)
split_v = share_drf[['_split_vars']]
split_s = share_drf[['_split_values']]
leaves  = share_drf[['_leaf_samples']]

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

X_puf     = as.matrix(puf[, drf_features])
n_pred    = nrow(X_puf)
tree_pick = sample.int(n_trees, size = n_pred, replace = TRUE)
donors    = integer(n_pred)

# Uniform leaf sampling is correct under bootstrap expansion: all training
# rows have implicit weight 1, and heavy-weight CEX rows get their
# population share via bootstrap-copy count in the leaf.
t0 = Sys.time()
for (i in seq_len(n_pred)) {
  t  = tree_pick[i]
  nd = walk_to_leaf(t, X_puf[i, ])
  lr = leaves[[t]][[nd]] + 1L
  donors[i] = as.integer(lr[sample.int(length(lr), 1L)])
}
cat(sprintf('consumption.R: sparse donor sampling over %d rows: %.1f s\n',
            n_pred, as.numeric(Sys.time() - t0, units = 'secs')))

donor_shares = share_matrix[donors, , drop = FALSE]
colnames(donor_shares) = pce_cats

# Category amounts = C * donor share
for (cat in pce_cats) {
  puf[[cat]] = puf$C * donor_shares[, cat]
}

puf = puf %>% select(id, all_of(pce_cats))

tax_units %<>% left_join(puf, by = 'id')

# Save diagnostics for plotting (only during test runs)
if (exists('save_consumption_diagnostics') && save_consumption_diagnostics) {
  write_rds(list(puf = puf, elasticity_beta = elasticity_beta, y_ref = y_ref),
            'resources/cache/consumption_diagnostics.rds')
}

rm(puf, cex_training, cex_boot, share_matrix, donor_shares,
   boot_probs, boot_idx, sourced_total, donors,
   consumption_rf, share_drf, pred_direct,
   elasticity_beta, y_ref,
   X_puf, tree_pick, n_pred, n_trees, root,
   child_L, child_R, split_v, split_s, leaves, walk_to_leaf)
