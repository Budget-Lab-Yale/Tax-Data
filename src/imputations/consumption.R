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
# Stage B: DRF composition model on weight-unpacked CEX sample
#---------------------------------------------------------------------------

# Create unweighted representative sample from CEX via bootstrap resampling
n_boot = 200000
boot_probs = cex_training$WT_ANNUAL / sum(cex_training$WT_ANNUAL)
boot_idx = sample.int(nrow(cex_training), size = n_boot, replace = TRUE, prob = boot_probs)
cex_boot = cex_training[boot_idx, ]

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
# Distribute total consumption across 20 PCE categories via DRF donor sampling
#---------------------------------------------------------------------------

# Use predicted C as total_consumption for DRF feature matching
puf$total_consumption = puf$C

batch_size = 5000
donor_shares = matrix(0, nrow = nrow(puf), ncol = length(pce_cats),
                      dimnames = list(NULL, pce_cats))

for (start in seq(1, nrow(puf), by = batch_size)) {
  end = min(start + batch_size - 1, nrow(puf))
  idx = start:end

  W = drf::get_sample_weights(share_drf,
        newdata = as.matrix(puf[idx, drf_features]))

  # Sample one donor per PUF unit (vectorized within batch)
  donors = apply(W, 1, function(p) sample.int(n_boot, size = 1, prob = p))
  donor_shares[idx, ] = share_matrix[donors, , drop = FALSE]
}

# Category amounts = C * donor share
for (cat in pce_cats) {
  puf[[cat]] = puf$C * donor_shares[, cat]
}

puf = puf %>% select(id, C, all_of(pce_cats))

tax_units %<>% left_join(puf, by = 'id')

# Save diagnostics for plotting (only during test runs)
if (exists('save_consumption_diagnostics') && save_consumption_diagnostics) {
  write_rds(list(puf = puf, elasticity_beta = elasticity_beta, y_ref = y_ref),
            'resources/cache/consumption_diagnostics.rds')
}

rm(puf, cex_training, cex_boot, share_matrix, donor_shares,
   boot_probs, boot_idx, sourced_total, W, donors,
   consumption_rf, share_drf, pred_direct,
   elasticity_beta, y_ref)
