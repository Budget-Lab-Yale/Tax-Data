#--------------------------------------
# mortgage.R
#
# Imputes primary residence mortgage
# share using SCF 2022 data and QRF
#--------------------------------------


# NOTE 2026-09-13: an `estimate_models = 1` sat here, so this module retrained
# its forest on every run whatever the operator asked for, and left the flag
# at 1 for consumption.R below it. Because a forest fit draws from the global
# stream, that made the FIT depend on the record count: prim_mort_share moved
# for ~28,000 filers between two vintages with record-identical model inputs.
# It was the last column failing the S23 invariance test. See rng.R.
if (estimate_models) {
  # Read and clean SCF data for mortgage analysis
  scf_mortgage = interface_paths$SCF %>%
    file.path('SCFP2022.csv') %>%
    fread() %>%
    tibble() %>%

    # Select required variables
    select(
      yy1 = YY1, y1 = Y1, weight = WGT,
      age1 = AGE, married = MARRIED, n_kids = KIDS,
      wages = WAGEINC, income = INCOME,
      prim_mort_bal = MRTHEL,      # Primary residence mortgage balance
      sec_mort_bal = RESDBT,      # Secondary/other residential real estate debt
    ) %>%

    # Calculate total mortgage balance and primary residence share
    mutate(
      total_mort_bal  = prim_mort_bal + sec_mort_bal,
      prim_mort_share = if_else(total_mort_bal > 0, prim_mort_bal / total_mort_bal, 1),  # Default to 100% primary if no mortgage
    ) %>%

    # Create percentile variables for income stratification. Not id-keyed
    # (S23): the SCF extract is fixed in size, independent of `tax_units`.
    mutate(
      income = if_else(income > 0, income + runif(nrow(.)), 0),
      across(
        .cols = income,
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
    ) %>%

    # Filter to households with mortgages for the share model
    filter(total_mort_bal > 0)


  # Estimate model of primary residence mortgage share among those with mortgages
  prim_mort_share_qrf = with_model_seed('prim_mort_share_qrf', quantregForest(
    x        = scf_mortgage[c('pctile_income', 'n_kids', 'married', 'age1')],
    y        = scf_mortgage$prim_mort_share,
    nthreads = n_threads(),
    weights  = scf_mortgage$weight,
    mtry     = 4,
    nodesize = 5
  ))

  write_rds(prim_mort_share_qrf, 'resources/cache/qrf/prim_mort_share_qrf.rds')
  rm(scf_mortgage)
} else {
  prim_mort_share_qrf = read_rds('resources/cache/qrf/prim_mort_share_qrf.rds')
}


# Impute primary residence mortgage share on tax unit data
prim_mort_share_imputed = tax_units %>%
  filter(int_exp > 0) %>%  # Only for those with mortgage interest
  mutate(
    n_kids = (
      (!is.na(dep_age1) & dep_age1 <= 24) +
      (!is.na(dep_age2) & dep_age2 <= 24) +
      (!is.na(dep_age3) & dep_age3 <= 24)
    ),
    across(
      .cols = income,
      .fns  = ~ cut(
        x      = . ,
        breaks = wtd.quantile(.[. > 0], weight[. > 0], 0:100/100),
        labels = 1:100
      ) %>% as.character() %>% as.integer() %>% replace_na(0),
      .names = 'pctile_{col}'
    )
  ) %>%
  select(id, weight, age1, n_kids, married, pctile_income) %>%
  mutate(
    prim_mort_share = predict_qrf_draw_by_id(prim_mort_share_qrf, (.), id,
                                             'mortgage_quantile')
  ) %>%
  select(id, prim_mort_share)


# Add primary residence mortgage share to tax unit data
tax_units %<>%
  left_join(prim_mort_share_imputed, by = 'id') %>%
  mutate(

    # Default to 100% primary residence for those without mortgage interest
    prim_mort_share = if_else(is.na(prim_mort_share) | int_exp == 0, 1, prim_mort_share),
  )
