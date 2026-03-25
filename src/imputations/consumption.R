#--------------------------------------
# consumption.R
#
# Imputes consumer expenditure (goods
# and services) using CEX data and QRF.
# Currently disabled.
#--------------------------------------


# The models here impute 3 months of consumption, not a full year
# We correct at the end by multiplying the result by 4
# The training data includes every month of the year so seasonality is avoided (I think)

# if(estimate_models) {
#   source("src/cex.R")
#
#   # QRF Training data
#   cex_training = build_cex_training()
#
#   # Training data for consumption as a share of income
#   # Restricted to consumption being less than 400% of income and to tax units
#   # with positive income
#   pct_train = cex_training %>% filter(expenses_per < 4) %>% filter(has_income == 1)
#
#   # We impute consumption directly and consumption as a percent of income to
#   # account for CEX's weaker measurement of income. The resulting imputations
#   # are then summed with a .5 weight on each (see further below)
#
#   # This method requires significantly more compute than using ranger or (improper)
#   # qrf modelling.
#   # TODO Let external users opt out of this if they don't have tons of spare compute
#   goods_qrf = quantregForest(
#     x = cex_training[c("has_income", 'pctile_income', 'married', 'age1', 'n_dep_ctc', 'male1')],
#     y = cex_training$goods,
#     nthreads = 8,
#     mtry = 5,
#     nodesize = 10
#   )
#   write_rds(goods_qrf, "resources/cache/qrf/goods_qrf.rds")
#
#   goods_per_qrf = quantregForest(
#     x = pct_train[c("has_income", 'pctile_income', 'married', 'age1', 'n_dep_ctc', 'male1')],
#     y = pct_train$goods_per,
#     nthreads = 8,
#     mtry = 5,
#     nodesize = 10
#   )
#   write_rds(goods_per_qrf, "resources/cache/qrf/goods_per_qrf.rds")
#
#   services_qrf = quantregForest(
#     x = cex_training[c("has_income", 'pctile_income', 'married', 'age1', 'n_dep_ctc', 'male1', 'goods')],
#     y = cex_training$services,
#     nthreads = 8,
#     mtry = 5,
#     nodesize = 10
#   )
#   write_rds(services_qrf, "resources/cache/qrf/services_qrf.rds")
#
#   services_per_qrf = quantregForest(
#     x = pct_train[c("has_income", 'pctile_income', 'married', 'age1', 'n_dep_ctc', 'male1', 'goods')],
#     y = pct_train$services_per,
#     nthreads = 8,
#     mtry = 5,
#     nodesize = 10
#   )
#   write_rds(services_per_qrf, "resources/cache/qrf/services_per_qrf.rds")
#
# } else {
#   goods_qrf        = read_rds("resources/cache/qrf/goods_qrf.rds")
#   goods_per_qrf    = read_rds("resources/cache/qrf/goods_per_qrf.rds")
#   services_qrf     = read_rds("resources/cache/qrf/services_qrf.rds")
#   services_per_qrf = read_rds("resources/cache/qrf/services_per_qrf.rds")
# }
#
# cex = tax_units %>%
#   mutate(
#     married = as.numeric(!is.na(male2)),
#     size = 1 + married + n_dep,
#     # Income definition expanded to match CEX MEMI XM variables
#     income = wages + sole_prop + part_active + part_passive - part_active_loss -
#       part_passive_loss - part_179 + scorp_active + scorp_passive -
#       scorp_active_loss - scorp_passive_loss - scorp_179 + gross_ss +
#       txbl_int + div_ord + div_pref + gross_pens_dist + rent - rent_loss,
#
#     has_income = case_when(
#       income >  0 ~ 1,
#       income == 0 ~ 0,
#       T           ~ -1
#     )
#   ) %>%
#   mutate(
#     across(
#       .cols = c(income),
#       .fns  = ~ cut(
#         x      = . ,
#         breaks = wtd.quantile(.[. > 0], weight[. > 0], 0:100/100),
#         labels = 1:100
#       ) %>% as.character() %>% as.integer() %>% replace_na(0),
#       .names = 'pctile_income'
#     )
#   ) %>%
#   select(id, weight, male1, age1, married, pctile_income, n_dep_ctc, income, size, has_income) %>%
#   mutate(
#     goods = predict(
#       object  = goods_qrf,
#       newdata = (.),
#       what    = function(x) sample(x, 1)
#     ),
#     goods_per = predict(
#       object  = goods_per_qrf,
#       newdata = (.),
#       what    = function(x) sample(x, 1)
#     )) %>%
#   mutate(
#     services = predict(
#       object  = services_qrf,
#       newdata = (.),
#       what    = function(x) sample(x, 1)
#     ),
#     services_per = predict(
#       object  = services_per_qrf,
#       newdata = (.),
#       what    = function(x) sample(x, 1)
#     ))  %>%
#   mutate(
#     # If a tax unit has irregular income, consumption is just the direct imputation
#     # and consumption as a percent of income is weighted 0
#     w_direct = if_else(has_income == 1, 0.5, 1.0),
#     w_ratio  = if_else(has_income == 1, 0.5, 0.0),
#     goods.c    = (w_direct * goods    + w_ratio * goods_per    * income) * 4,
#     services.c = (w_direct * services + w_ratio * services_per * income) * 4,
#     C          = goods.c + services.c
#   ) %>%
#   select(id, goods.c, services.c, C)
#
# tax_units %<>% left_join(cex)
#
# rm(cex, goods_qrf, goods_per_qrf, services_qrf, services_per_qrf)
