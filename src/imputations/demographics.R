#--------------------------------------
# demographics.R
#
# Imputes blindness, gender, and
# SSN status for children
#--------------------------------------


#-----------
# Blindness
#-----------

# Set parameter based on 1040 line item estimates
share_blind = (260535 + 83983) / 104013115  # number of blind standard deductions taken over number of nonitemizers

# Impute
tax_units %<>%
  mutate(blind1 = runif(nrow(.)) < share_blind,
         blind2 = if_else(filing_status == 2,
                          runif(nrow(.)) < share_blind,
                          NA))


#-------------------
# Gender for adults
#-------------------

# Estimate target male distribution parameters for unmarried tax units on DINA data
male_dist = dina_2017 %>%
  filter(married == 0) %>%
  group_by(filer, has_kids = xkidspop > 0, employed = fiwag > 0, female) %>%
  summarise(n = sum(dweght),
            .groups = 'drop') %>%
  group_by(filer, has_kids, employed) %>%
  mutate(p_male = 1 - n / sum(n)) %>%
  filter(female == 1) %>%
  select(-n, -female) %>%
  ungroup()

# Back out imputation probabilities by targeting DINA male shares
male_dist_impute = tax_units %>%
  mutate(sex = as.integer(if_else(filer == 0, NA, GENDER == 1))) %>%
  filter(filing_status != 2) %>%
  group_by(filer, has_kids = n_dep_ctc > 0, employed = wages > 0) %>%
  summarise(male     = sum((!is.na(sex) & sex == 1) * weight),
            missing  = sum(is.na(sex)),
            n_weight = sum(weight),
            n        = n(),
            .groups = 'drop') %>%
  mutate(p_male_reported = male / n_weight,
         p_reported      = 1 - missing / n) %>%
  select(-male, -missing, -n) %>%

  left_join(male_dist, by = c('filer', 'has_kids', 'employed')) %>%
  mutate(p_male_impute = pmin(1, pmax(0, (p_male - p_male_reported * p_reported) / (1 - p_reported))))


# Impute in data
tax_units %<>%

  # Join unmarried imputation probabilities
  mutate(has_kids = n_dep_ctc > 0, employed = wages > 0) %>%
  left_join(male_dist_impute %>%
              select(filer, has_kids, employed, p_male_impute),
            by = c('filer', 'has_kids', 'employed')) %>%
  mutate(

    # Choose randomly for all nonfilers
    GENDER = if_else(filer == 0, NA, GENDER),

    # Primary earner
    male1 = case_when(

      # Married and missing: choose randomly
      filing_status == 2 & is.na(GENDER) ~ as.integer(runif(nrow(.)) < 0.5),

      # Married and nonmissing: take from PUF
      filing_status == 2 & GENDER == 1 ~ 1,
      filing_status == 2 & GENDER == 2 ~ 0,

      # Unmarried and missing: simulate
      filing_status != 2 & is.na(GENDER) ~ runif(nrow(.)) < p_male_impute,

      # Unmarried and nonmissing: take from PUF
      filing_status != 2 & GENDER == 1 ~ 1,
      filing_status != 2 & GENDER == 2 ~ 0
    ),

    # Secondary earner...assume 1% of marrages are same-sex
    male2 = case_when(
      filing_status == 2 & male1 == 1 ~ if_else(runif(nrow(.)) < 0.01, 1, 0),
      filing_status == 2 & male1 == 0 ~ if_else(runif(nrow(.)) < 0.01, 0, 1),
      TRUE                            ~ NA
    )
  ) %>%

  select(-p_male_impute, -has_kids, -employed)



#--------------------------------
# Impute SSN status for children
#--------------------------------

# Source: ITEP (https://itep.org/inclusive-child-tax-credit-reform-would-restore-benefit-to-1-million-young-dreamers/)
share_without_ssn = 1098100 / 72e6

# Assume perfect correlation of SSN status within tax units
tax_units %<>%
  mutate(ssn          = runif(nrow(.)) > share_without_ssn,
         dep_ssn1     = if_else(!is.na(dep_age1), ssn, NA),
         dep_ssn2     = if_else(!is.na(dep_age2), ssn, NA),
         dep_ssn3     = if_else(!is.na(dep_age3), ssn, NA)) %>%
  select(-ssn)
