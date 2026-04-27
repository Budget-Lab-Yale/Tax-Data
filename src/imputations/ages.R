#--------------------------------------
# ages.R
#
# Imputes primary/secondary ages,
# dependent ages, and CTC eligibility
#--------------------------------------


#----------------------------
# Primary and secondary ages
#----------------------------

# Read the CPS
cps = interface_paths$`CPS-ASEC` %>%
  file.path('cps_00027.csv.gz') %>%
  read_csv()

# Estimate age gap distribution
age_gap_dist = cps %>%
  filter(RELATE %in% c(0101, 0201), CPSID != 0, AGE >= 18) %>%
  mutate(role = if_else(RELATE == 0101, 'primary', 'secondary')) %>%
  select(YEAR, CPSID, role, age = AGE, weight = ASECWTH) %>%
  mutate(age = if_else(age >= 80, 80, age)) %>%
  pivot_wider(names_from  = role,
              values_from = age) %>%
  group_by(age1 = primary, age2 = secondary) %>%
  filter(!is.na(age2)) %>%
  summarise(n = sum(weight),
            .groups = 'drop') %>%
  arrange(age1, age2) %>%
  group_by(age1) %>%
  mutate(p = n / sum(n)) %>%
  ungroup() %>%
  select(-n)

# Estimate distribution of ages within PUF age categories
cps_ages = cps %>%
  mutate(age = if_else(AGE >= 80, 80, AGE)) %>%
  group_by(age) %>%
  summarise(n = sum(ASECWT))

age_dist = cps_ages %>%
  mutate(age = if_else(age >= 80, 80, age)) %>%
  filter(age >= 18) %>%
  # Cuts use `<` to match IRS AGERANGE: band 6 is "65 and over"
  # (see Internal Revenue Bulletin/SOI Tab 1.6 footnote). Must agree
  # with project_puf.R:325-330 — otherwise CPS draws of e.g. age 65
  # land in band 5 here but are re-binned to band 6 downstream,
  # leaking ~10% of each band's top-year mass into the next band up.
  group_by(age_group = case_when(
    age < 26 ~ 1,
    age < 35 ~ 2,
    age < 45 ~ 3,
    age < 55 ~ 4,
    age < 65 ~ 5,
    T        ~ 6
  )) %>%
  mutate(p = n / sum(n)) %>%
  select(-n)


# Impute ages
imputed_ages = tax_units %>%
  select(id, age_group) %>%
  left_join(age_dist,
            by = 'age_group',
            relationship = 'many-to-many') %>%
  group_by(id) %>%
  sample_n(size = 1, weight = p) %>%
  ungroup() %>%
  select(id, age1 = age)

# Impute secondary ages
imputed_secondary_ages = tax_units %>%
  left_join(imputed_ages, by = 'id') %>%
  filter(filing_status == 2) %>%
  select(id, age1) %>%
  left_join(age_gap_dist,
            by = 'age1',
            relationship = 'many-to-many') %>%
  group_by(id) %>%
  sample_n(size = 1, weight = p) %>%
  ungroup() %>%
  select(id, age2)

tax_units %<>%
  left_join(imputed_ages, by = 'id') %>%
  left_join(imputed_secondary_ages, by = 'id') %>%
  select(-age_group)


#------------------------------------
# Dependent ages and CTC eligibility
#------------------------------------

dep_ages = tax_units %>%
  mutate(

    # Impute dependent ages uniformly within bands
    dep_age1 = case_when(
      dep_age_group1 == 1 ~ floor(runif(nrow(.), 0, 5)),
      dep_age_group1 == 2 ~ floor(runif(nrow(.), 5, 13)),
      dep_age_group1 == 3 ~ floor(runif(nrow(.), 13, 17)),
      dep_age_group1 == 4 ~ floor(runif(nrow(.), 17, 19)),
      dep_age_group1 == 5 ~ floor(runif(nrow(.), 19, 24)),
      dep_age_group1 == 6 ~ pmin(80, floor(runif(nrow(.), 24, 95))),
      T                   ~ NA
    ),
    dep_age2 = case_when(
      dep_age_group2 == 1 ~ floor(runif(nrow(.), 0, 5)),
      dep_age_group2 == 2 ~ floor(runif(nrow(.), 5, 13)),
      dep_age_group2 == 3 ~ floor(runif(nrow(.), 13, 17)),
      dep_age_group2 == 4 ~ floor(runif(nrow(.), 17, 19)),
      dep_age_group2 == 5 ~ floor(runif(nrow(.), 19, 24)),
      dep_age_group2 == 6 ~ pmin(80, floor(runif(nrow(.), 24, 95))),
      T                   ~ NA
    ),
    dep_age3 = case_when(
      dep_age_group3 == 1 ~ floor(runif(nrow(.), 0, 5)),
      dep_age_group3 == 2 ~ floor(runif(nrow(.), 5, 13)),
      dep_age_group3 == 3 ~ floor(runif(nrow(.), 13, 17)),
      dep_age_group3 == 4 ~ floor(runif(nrow(.), 17, 19)),
      dep_age_group3 == 5 ~ floor(runif(nrow(.), 19, 24)),
      dep_age_group3 == 6 ~ pmin(80, floor(runif(nrow(.), 24, 95))),
      T                   ~ NA
    )

  ) %>%
  select(id, dep_age1, dep_age2, dep_age3) %>%

  # Re-sort ages
  pivot_longer(cols      = -id,
               names_to  = 'series',
               values_to = 'age') %>%
  mutate(series = str_sub(series, end = -2)) %>%
  arrange(id, series, age) %>%
  group_by(id, series) %>%
  mutate(dep_number = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from  = c(series, dep_number),
              names_sep   = '',
              values_from = age)


# Add to dataframe and assign CTC status for each dependent
tax_units %<>%
  select(-starts_with('dep_age_group')) %>%
  left_join(dep_ages, by = 'id') %>%
  mutate(dep_ctc1 = if_else(!is.na(dep_age1), dep_age1 < 17 & n_dep_ctc > 0, NA),
         dep_ctc2 = if_else(!is.na(dep_age2), dep_age2 < 17 & n_dep_ctc > 1, NA),
         dep_ctc3 = if_else(!is.na(dep_age3), dep_age3 < 17 & n_dep_ctc > 2, NA))

# Simulate CTC ineligibility for 17 year old dependents -- some small share of
# this group will still not qualify for CTC under counterfactuals in which 17 year olds
# are CTC-qualifying children, per other rules. Estimate this parameter based on
# the gap between under-16 dependents and CTC-qualifying children
share_17yo_ineligible = dep_ages %>%
  left_join(tax_units %>%
              select(id, weight, n_dep_ctc),
            by = 'id') %>%
  mutate(n_16u = (!is.na(dep_age1) & dep_age1 < 17) +
                 (!is.na(dep_age2) & dep_age2 < 17) +
                 (!is.na(dep_age3) & dep_age3 < 17)) %>%
  filter(n_16u > 0) %>%
  group_by(ineligible = n_16u > n_dep_ctc) %>%
  summarise(n = sum(weight)) %>%
  mutate(share = n / sum(n)) %>%
  filter(ineligible) %>%
  select(share) %>%
  deframe()

tax_units %<>%
  mutate(dep_ctc1 = if_else(!is.na(dep_age1) & dep_age1 == 17,
                            runif(nrow(.)) > share_17yo_ineligible,
                            dep_ctc1),
         dep_ctc2 = if_else(!is.na(dep_age2) & dep_age2 == 17,
                            runif(nrow(.)) > share_17yo_ineligible,
                            dep_ctc2),
         dep_ctc3 = if_else(!is.na(dep_age3) & dep_age3 == 17,
                            runif(nrow(.)) > share_17yo_ineligible,
                            dep_ctc3))
