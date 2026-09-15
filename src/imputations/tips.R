#--------------------------------------
# tips.R
#
# Imputes tipped income using SIPP
# microdata and QRF models, benchmarked
# to IRS W-2 aggregates
#--------------------------------------


# Set benchmarks from 2017 IRS W2 data
n_tipped_married   = 1164973
n_tipped_unmarried = 4800747
tip_avg_married    = 7072
tip_avg_unmarried  = 5686

# Read and process SIPP data
sipp = file.path(interface_paths$SIPP, 'tip_ind_occ_full_split.csv') %>%
  fread() %>%
  tibble() %>%

  # Filter to nondependent wage workers in the weighted universe. SIPP
  # carries a zero person weight for respondents outside the survey universe
  # in the reference period (215 of 114,272 wage-worker rows in vintage
  # 2024090311). randomForest >= 4.7-1.2, the engine under quantregForest,
  # refuses any non-positive training weight; under 4.7-1.1 the weights
  # argument did not exist and was silently ignored, which is how the July
  # 2026 cached fits trained. A zero-weight row contributes nothing to a
  # weighted fit, so this restricts the universe rather than dropping data.
  filter(!is_dep, inc_wages > 0, weight > 0) %>%
  mutate(
    year      = year - 1,
    tipped    = as.integer(inc_wages_tips > 0),
    tip_share = inc_wages_tips / inc_wages,
    parent    = as.integer(n_dep > 0),
    married   = as.integer(!is.na(marriage) & marriage < 3),
    wages     = case_when(
      year == 2017 ~ inc_wages / 1,
      year == 2018 ~ inc_wages / 1.024,
      year == 2019 ~ inc_wages / 1.043,
      year == 2020 ~ inc_wages / 1.056,
      year == 2021 ~ inc_wages / 1.105,
      year == 2022 ~ inc_wages / 1.194
    )
  ) %>%

  # Assign new industry categories
  mutate(
    tips_lh = replace_na(earn_tip_wages_i_1 * (ind_1 >= 8561 & ind_1 <= 8690), 0) +
              replace_na(earn_tip_wages_i_2 * (ind_2 >= 8561 & ind_2 <= 8690), 0) +
              replace_na(earn_tip_wages_i_3 * (ind_3 >= 8561 & ind_3 <= 8690), 0) +
              replace_na(earn_tip_wages_i_4 * (ind_4 >= 8561 & ind_4 <= 8690), 0),
    tips_lh = as.integer(tips_lh / inc_wages_tips > 0.5)
  ) %>%
  select(
    u_ID, year, weight, age, n_dep, married, parent, wages,
    tipped, tips = inc_wages_tips, tip_share, tips_lh
  )

# Create subsets for training data
sipp_tipped = sipp %>%
  filter(tipped == 1)

sipp_tipped_21 = sipp %>%
  filter(tipped == 1, year >= 2021)

# Run or read in Quantile Regression Forest objects
tip_qrf = train_or_load_qrf(
  name     = 'tip_qrf',
  x        = sipp[c('wages', 'parent', 'married', 'age')],
  y        = as.factor(sipp$tipped),
  weights  = sipp$weight,
  mtry     = 3,
  nodesize = 5
)

tip_share_qrf = train_or_load_qrf(
  name     = 'tip_share_qrf',
  x        = sipp_tipped[c('wages')],
  y        = sipp_tipped$tip_share,
  weights  = sipp_tipped$weight,
  mtry     = 1,
  nodesize = 3
)

tip_lh_qrf = train_or_load_qrf(
  name     = 'tip_lh_qrf',
  x        = sipp_tipped_21[c('tip_share', 'wages', 'parent', 'married', 'age')],
  y        = as.factor(sipp_tipped_21$tips_lh),
  weights  = sipp_tipped_21$weight,
  mtry     = 4,
  nodesize = 1
)

# Fit values on tax data
tips = tax_units %>%
  filter(wages1 > 0 | wages2 > 0) %>%
  mutate(
    parent = (
        (!is.na(dep_age1) & dep_age1 <= 17) |
        (!is.na(dep_age2) & dep_age2 <= 17) |
        (!is.na(dep_age3) & dep_age3 <= 17)
      ) %>%
      as.integer() %>%
      as.character()
  ) %>%
  select(id, weight, parent, married, wages1, wages2, age1, age2) %>%
  pivot_to_spouses() %>%
  mutate(
    p = predict(
      object  = tip_qrf,
      newdata = (.),
      what    = function(x) mean(x - 1)
    ),
    # S23: the same ensemble member is chosen, by the record's own draw
    # rather than by position. `index` (1 = primary, 2 = spouse) sub-keys it,
    # so the two earners in a joint unit draw independently, as they did
    # when each was a separate row under the positional sample().
    tip_share = predict_qrf_draw_by_id(tip_share_qrf, (.), id,
                                       'tips_quantile', sub = index)
  ) %>%
  mutate(
    tips_lh = predict_qrf_draw_by_id(tip_lh_qrf, (.), id, 'tips_year',
                                     offset = -1, sub = index)
  )


# Calculate scaling factor to benchmark to IRS aggregates
scaling_factors = tips %>%
  group_by(married) %>%
  summarise(
    total     = sum(wages * tip_share * p * weight),
    n         = sum(p * (wages > 0) * weight),
    n_workers = sum((wages > 0) * weight),
    avg       = total / n,
  ) %>%
  mutate(
    factor_p   = if_else(married == 1, n_tipped_married, n_tipped_unmarried) / n,
    factor_avg = if_else(married == 1, tip_avg_married,  tip_avg_unmarried)  / avg
  )

# ... and adjust pre-pandemic IRS benchmarking factors to account for extensive
# margin growth in tipping since the pandemic
covid_factor = sipp %>%
  summarise(
    pre_pandemic  = weighted.mean(tipped, weight * (year %in% 2017:2019)),
    post_pandemic = weighted.mean(tipped, weight * (year %in% 2021:2022))
  ) %>%
  mutate(covid_factor = post_pandemic / pre_pandemic) %>%
  select(covid_factor) %>%
  deframe()

# Simulate tips accounting for scaling factors
tips %<>%
  left_join(scaling_factors, by = 'married') %>%
  mutate(
    tips = wages * tip_share *
           (draw_by_id(id, 'tips_receipt', sub = index) <
              (p * factor_p * covid_factor)) * factor_avg,
    tips_lh = na_if(tips_lh, tips == 0)
  )


# Add tips-related info to tax unit data
tax_units %<>%
  left_join(
    tips %>%
      select(id, index, tips, tips_lh) %>%
      pivot_wider(
        names_from  = index,
        names_sep   = '',
        values_from = c(tips, tips_lh),
      ),
    by = 'id'
  ) %>%
  mutate(
    tips1    = replace_na(tips1, 0),
    tips2    = replace_na(tips2, 0),
    tips     = tips1 + tips2,
    tips_lh1 = replace_na(tips_lh1, 0),
    tips_lh2 = replace_na(tips_lh2, 0)
  )
