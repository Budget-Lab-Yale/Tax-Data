#--------------------------------------
# auto_loan.R
#
# Imputes auto loan interest using
# SCF 2022 data and QRF models
#--------------------------------------


if (estimate_models) {

  # Read and clean data
  scf = interface_paths$SCF %>%
    file.path('SCFP2022.csv') %>%
    fread() %>%
    tibble() %>%

    # Select required variables
    select(
      yy1 = YY1, y1 = Y1, weight = WGT,
      age1 = AGE, married = MARRIED, n_kids = KIDS,
      wages = WAGEINC, income = INCOME, balance = VEH_INST
    ) %>%

    # Bring in Ernie's imputed auto loan interest
    left_join(
      read_dta('./resources/scf2022_carinterest.dta'), by = c('yy1', 'y1')
    ) %>%
    rename(auto_int_exp = carinterest) %>%
    mutate(has_auto_int_exp = as.integer(auto_int_exp > 0)) %>%

    # Create percentile variables
    mutate(
      wages  = if_else(wages > 0,  wages  + runif(nrow(.)), 0),
      income = if_else(income > 0, income + runif(nrow(.)), 0),
      across(
        .cols = c(wages, income),
        .fns  = ~ cut(
          x      = . ,
          breaks = wtd.quantile(.[. > 0], weight[. > 0], 0:100/100),
          labels = 1:100
        ) %>% as.character() %>% as.integer() %>% replace_na(0),
        .names = 'pctile_{col}'
      )
    ) %>%

    # Recode kids and marital status variables to match PUF
    mutate(
      n_kids  = pmin(n_kids, 3),
      married = as.integer(married == 1)
    )
}

has_auto_qrf = train_or_load_qrf(
  name     = 'has_auto_qrf',
  x        = if (estimate_models) scf[c('pctile_wages', 'pctile_income', 'n_kids', 'married', 'age1')] else NULL,
  y        = if (estimate_models) as.factor(scf$has_auto_int_exp) else NULL,
  weights  = if (estimate_models) scf$weight else NULL,
  mtry     = 4,
  nodesize = 1
)

if (estimate_models) {
  scf_int = scf %>%
    filter(auto_int_exp > 0)
}

auto_qrf = train_or_load_qrf(
  name     = 'auto_qrf',
  x        = if (estimate_models) scf_int[c('pctile_wages', 'pctile_income', 'n_kids', 'married', 'age1')] else NULL,
  y        = if (estimate_models) scf_int$auto_int_exp else NULL,
  weights  = if (estimate_models) scf_int$weight else NULL,
  mtry     = 4,
  nodesize = 5
)

if (estimate_models) rm(scf_int)


# Impute on tax unit data observations
auto_int_exp = tax_units %>%
  mutate(
    n_kids = (
      (!is.na(dep_age1) & dep_age1 <= 24) +
      (!is.na(dep_age2) & dep_age2 <= 24) +
      (!is.na(dep_age3) & dep_age3 <= 24)
    ),
    across(
      .cols = c(wages, income),
      .fns  = ~ cut(
        x      = . ,
        breaks = wtd.quantile(.[. > 0], weight[. > 0], 0:100/100),
        labels = 1:100
      ) %>% as.character() %>% as.integer() %>% replace_na(0),
      .names = 'pctile_{col}'
    )
  ) %>%
  select(id, weight, age1, n_kids, married, pctile_income, pctile_wages) %>%
  mutate(
    p = predict(
      object  = has_auto_qrf,
      newdata = (.),
      what    = function(x) mean(x - 1)
    ),
    auto_int_exp = pmin(max_ot, predict(
      object  = auto_qrf,
      newdata = (.),
      what    = function(x) sample(x, 1)
    ) * (runif(nrow(.)) < p))
  )


# Add to tax unit data, expressing in 2017 model-year dollars since this is
# estimated on 2022 SCF data (i.e. deflating by amount of per-tax unit interest
# income growth over the 2017-2022 period
tax_units$auto_int_exp = auto_int_exp$auto_int_exp / 1.3886
