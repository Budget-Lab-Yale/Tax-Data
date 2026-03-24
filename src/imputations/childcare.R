#--------------------------------------
# childcare.R
#
# Imputes childcare expenses using
# CPS-ASEC 2017 and QRF model
#--------------------------------------


childcare_qrf = train_or_load_qrf(
  name     = 'childcare_qrf',
  x        = if (estimate_models) {
    childcare_train = cps %>%
      filter(YEAR == 2017, RELATE %in% c(101, 201)) %>%
      mutate(
        married   = as.integer(MARST %in% 1:2),
        n_dep_old = NCHILD - NCHLT5,
        care_exp  = if_else(married == 1, SPMCHXPNS / 2, SPMCHXPNS)
      ) %>%
      select(SERIAL, FAMUNIT, weight = ASECWT, age = AGE, married, n_dep_young = NCHLT5, n_dep_old, wages = INCWAGE, care_exp)
    childcare_train[c('wages', 'married', 'n_dep_young', 'n_dep_old', 'age')]
  } else NULL,
  y        = if (estimate_models) childcare_train$care_exp else NULL,
  weights  = if (estimate_models) childcare_train$weight else NULL,
  mtry     = 4,
  nodesize = 5
)

if (estimate_models) rm(childcare_train)

# Fit values on tax data
childcare = tax_units %>%
  mutate(
    n_dep_young = (
      (!is.na(dep_age1) & dep_age1 <= 4) +
      (!is.na(dep_age2) & dep_age2 <= 4) +
      (!is.na(dep_age3) & dep_age3 <= 4)
    ),
    n_dep_old = (
      (!is.na(dep_age1) & dep_age1 > 4 & dep_age1 <= 24) +
      (!is.na(dep_age2) & dep_age2 > 4 & dep_age2 <= 24) +
      (!is.na(dep_age3) & dep_age3 > 4 & dep_age3 <= 24)
    ),
    married = as.integer(filing_status == 2)
  ) %>%
  select(id, weight, married, n_dep_young, n_dep_old, wages1, wages2, age1, age2) %>%
  pivot_to_spouses() %>%
  mutate(
    care_exp = predict(
      object  = childcare_qrf,
      newdata = (.),
      what    = function(x) sample(x, 1)
    )
  )

# Add care expenses to tax unit data
tax_units %<>%
  select(-care_exp) %>%
  left_join(
    childcare %>%
      select(id, index, care_exp) %>%
      pivot_wider(
        names_from   = index,
        names_prefix = 'care_exp',
        values_from  = care_exp,
      ),
    by = 'id'
  ) %>%
  mutate(care_exp = replace_na(care_exp1, 0) + replace_na(care_exp2, 0)) %>%
  select(-care_exp1, -care_exp2)
