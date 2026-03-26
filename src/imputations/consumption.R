#--------------------------------------
# consumption.R
#
# Imputes consumer expenditure using
# CEX data and ranger quantile forests.
#--------------------------------------

# The models here impute quarterly consumption (FMLI CQ = 3-month expenditures)
# We annualize by multiplying by 4

source('src/cex.R')
cex_training = build_cex_training()

features = c('has_income', 'pctile_income', 'married', 'age1', 'n_dep', 'n_dep_ctc', 'male1')

# All 20 BEA PCE major categories
pce_cats = c('clothing', 'motor_vehicles', 'other_durables', 'furnishings',
             'rec_goods', 'other_nondurables', 'food_off_premises', 'communication',
             'npish', 'other_services', 'transport_services', 'rec_services',
             'net_foreign_travel', 'food_accommodations', 'health_care', 'utilities',
             'gasoline', 'education', 'financial_insurance', 'housing')

#---------------------------------------------------------------------------
# Train ranger models for total consumption
#---------------------------------------------------------------------------

consumption_rf = train_or_load_ranger(
  name         = 'consumption_rf',
  formula      = total_consumption ~ .,
  data         = cex_training[c(features, 'total_consumption')],
  case_weights = cex_training$WT_ANNUAL,
  mtry         = 5,
  min_node_size = 10
)

pct_train = cex_training %>% filter(has_income == 1, total_consumption_per < 4)

consumption_per_rf = train_or_load_ranger(
  name         = 'consumption_per_rf',
  formula      = total_consumption_per ~ .,
  data         = pct_train[c(features, 'total_consumption_per')],
  case_weights = pct_train$WT_ANNUAL,
  mtry         = 5,
  min_node_size = 10
)

#---------------------------------------------------------------------------
# Compute PCE expenditure shares by income percentile from CEX training data
#---------------------------------------------------------------------------

pce_sourced = setdiff(pce_cats, c('npish', 'net_foreign_travel'))

pce_shares = cex_training %>%
  filter(total_consumption > 0) %>%
  mutate(pctile_bin = pmax(pctile_income, 0)) %>%
  group_by(pctile_bin) %>%
  summarise(
    across(all_of(pce_sourced), ~ weighted.mean(.x / total_consumption, WT_ANNUAL, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  # Normalize so shares sum to exactly 1 within each bin
  mutate(
    share_total = rowSums(across(all_of(pce_sourced))),
    across(all_of(pce_sourced), ~ .x / share_total)
  ) %>%
  select(-share_total)

#---------------------------------------------------------------------------
# Predict consumption on PUF tax units
#---------------------------------------------------------------------------

cex = tax_units %>%
  mutate(
    married = as.numeric(!is.na(male2)),
    size = 1 + married + n_dep,
    # Income definition expanded to match CEX MEMI XM variables
    income = wages + sole_prop + part_active + part_passive - part_active_loss -
      part_passive_loss - part_179 + scorp_active + scorp_passive -
      scorp_active_loss - scorp_passive_loss - scorp_179 + gross_ss +
      txbl_int + div_ord + div_pref + gross_pens_dist,
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
pred_direct = predict_ranger_draw(consumption_rf, cex[features])
pred_ratio  = predict_ranger_draw(consumption_per_rf, cex[features])

cex = cex %>%
  mutate(
    # 50% direct + 50% ratio*income for positive income; 100% direct otherwise
    # Multiply by 4 to annualize (FMLI CQ training data is quarterly)
    C = ifelse(has_income == 1,
               0.5 * pred_direct + 0.5 * pred_ratio * income,
               pred_direct) * 4,
    C = pmax(C, 0)
  )

#---------------------------------------------------------------------------
# Distribute total consumption across 20 PCE categories using shares
#---------------------------------------------------------------------------

cex = cex %>%
  mutate(pctile_bin = pmax(pctile_income, 0)) %>%
  left_join(pce_shares, by = 'pctile_bin')

# Multiply shares by total consumption to get category amounts
for (cat in pce_sourced) {
  cex[[cat]] = cex[[cat]] * cex$C
}
cex$npish = 0
cex$net_foreign_travel = 0

cex = cex %>% select(id, C, all_of(pce_cats))

tax_units %<>% left_join(cex, by = 'id')

rm(cex, cex_training, pct_train, pce_shares, consumption_rf, consumption_per_rf,
   pred_direct, pred_ratio)
