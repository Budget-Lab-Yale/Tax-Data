#--------------------------------------
# overtime.R
#
# Imputes FLSA overtime earnings
# using QRF on CPS/SIPP wage data
#--------------------------------------


# Read and clean data
ot_microdata = haven::read_dta('./resources/otdata2023.dta') %>%
  mutate(
    wages    = total_weekly_earnings * wkswork1,
    ot       = ot_weekly_earnings * wkswork1 * (flsa_eligible > 0),
    has_ot   = as.integer(ot > 0),
    ot_share = ot / wages,
    married  = married == 1,
    parent   = numkids > 0,
    age      = as.numeric(age)
  ) %>%
  filter(wages > 0, !is.na(ot), age >= 18) %>%
  mutate(
    wage_pctile = cut(
      x      = wages,
      breaks = wtd.quantile(wages + runif(nrow(.)), weight, 0:100 / 100),
      labels = 1:100
    ) %>% as.character() %>% as.numeric()
  ) %>%
  filter(!is.na(wage_pctile))

has_ot_qrf = train_or_load_qrf(
  name     = 'has_ot_qrf',
  x        = ot_microdata[c('wage_pctile', 'parent', 'married', 'age')],
  y        = as.factor(ot_microdata$has_ot),
  weights  = ot_microdata$annualweight,
  mtry     = 3,
  nodesize = 1
)

if (estimate_models) {
  has_ot_microdata = ot_microdata %>%
    filter(ot > 0)
}

ot_share_qrf = train_or_load_qrf(
  name     = 'ot_share_qrf',
  x        = if (estimate_models) has_ot_microdata[c('wage_pctile', 'parent', 'married', 'age')] else NULL,
  y        = if (estimate_models) has_ot_microdata$ot_share else NULL,
  weights  = if (estimate_models) has_ot_microdata$annualweight else NULL,
  mtry     = 3,
  nodesize = 5
)

if (estimate_models) rm(has_ot_microdata)


# Fit values on tax data
max_ot = max(ot_microdata$ot)
actual_p = ot_microdata %>%
  summarise(weighted.mean(has_ot, annualweight)) %>%
  deframe()
ot = tax_units %>%
  filter(wages1 > 0 | wages2 > 0) %>%
  mutate(
    parent = (
      (!is.na(dep_age1) & dep_age1 <= 24) |
      (!is.na(dep_age2) & dep_age2 <= 24) |
      (!is.na(dep_age3) & dep_age3 <= 24)
    ),
    married = married == 1
  ) %>%
  select(id, weight, parent, married, wages1, wages2, age1, age2) %>%
  pivot_to_spouses() %>%
  mutate(
    wage_pctile = compute_percentile(wages + runif(nrow(.)), weight)
  ) %>%
  mutate(
    p = predict(
      object  = has_ot_qrf,
      newdata = (.),
      what    = function(x) mean(x - 1)
    ),
    p  = p * actual_p / weighted.mean(p, weight),
    ot = pmin(max_ot, predict(
      object  = ot_share_qrf,
      newdata = (.),
      what    = function(x) sample(x, 1)
    ) * wages * (runif(nrow(.)) < p))
  )


# Add OT-related info to tax unit data
tax_units %<>%
  left_join(
    ot %>%
      select(id, index, ot) %>%
      pivot_wider(
        names_from   = index,
        names_prefix = 'ot',
        values_from  = ot,
      ),
    by = 'id'
  ) %>%
  mutate(
    ot1    = replace_na(ot1, 0),
    ot2    = replace_na(ot2, 0),
    ot     = ot1 + ot2
  )
