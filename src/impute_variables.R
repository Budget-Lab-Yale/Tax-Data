#--------------------------------------
# impute_variables.R
# 
# Models variables not included in PUF
#--------------------------------------

# TODO list for the future
# - model 65+ 
# - get actual distributions of ages within buckets
# - improve secondary age imputation
# - improve earnings split imputation
# - vary SSN status by income
# - actually impute pretax contributions
# - higher fidelty wage earnings split for EARNINGS var, including consistent notion of gender/primary status interaction  

tax_units = puf_2017




#----------------------------
# Primary and secondary ages
#----------------------------

tax_units %<>% 
  mutate(
    
    # Distribute ages uniformly within bands; top-code ages at 65
    age1 = case_when(
      age_group == 1 ~ floor(runif(nrow(.), 18, 26)), 
      age_group == 2 ~ floor(runif(nrow(.), 26, 35)),
      age_group == 3 ~ floor(runif(nrow(.), 35, 45)), 
      age_group == 4 ~ floor(runif(nrow(.), 45, 55)),
      age_group == 5 ~ floor(runif(nrow(.), 55, 65)),
      T              ~ 65
    ),
    
    # Set secondary age to primary age for joint returns
    age2 = if_else(filing_status == 2, age1, NA)
  ) %>% 
  
  # Clean up 
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
      dep_age_group1 == 6 ~ floor(runif(nrow(.), 24, 95)),
      T                   ~ NA
    ),
    dep_age2 = case_when(
      dep_age_group2 == 1 ~ floor(runif(nrow(.), 0, 5)), 
      dep_age_group2 == 2 ~ floor(runif(nrow(.), 5, 13)), 
      dep_age_group2 == 3 ~ floor(runif(nrow(.), 13, 17)), 
      dep_age_group2 == 4 ~ floor(runif(nrow(.), 17, 19)),
      dep_age_group2 == 5 ~ floor(runif(nrow(.), 19, 24)),
      dep_age_group2 == 6 ~ floor(runif(nrow(.), 24, 95)),
      T                   ~ NA
    ),
    dep_age3 = case_when(
      dep_age_group3 == 1 ~ floor(runif(nrow(.), 0, 5)), 
      dep_age_group3 == 2 ~ floor(runif(nrow(.), 5, 13)), 
      dep_age_group3 == 3 ~ floor(runif(nrow(.), 13, 17)), 
      dep_age_group3 == 4 ~ floor(runif(nrow(.), 17, 19)),
      dep_age_group3 == 5 ~ floor(runif(nrow(.), 19, 24)),
      dep_age_group3 == 6 ~ floor(runif(nrow(.), 24, 95)),
      T                   ~ NA
    ),
    
    # Assign age variables for CTC
    dep_ctc_age1 = if_else(!is.na(dep_age1) & dep_age1 < 17 & n_dep_ctc > 0, 
                           dep_age1,
                           NA),
    dep_ctc_age2 = if_else(!is.na(dep_age2) & dep_age2 < 17 & n_dep_ctc > 1, 
                           dep_age2,
                           NA),
    dep_ctc_age3 = if_else(!is.na(dep_age3) & dep_age3 < 17 & n_dep_ctc > 2, 
                           dep_age3,
                           NA)
  ) %>%   
  select(id, dep_age1, dep_age2, dep_age3, dep_ctc_age1, dep_ctc_age2, dep_ctc_age3) %>% 
  
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

# Add to dataframe
tax_units %<>% 
  select(-starts_with('dep_age_group')) %>% 
  left_join(dep_ages, by = 'id')
  
  
#--------------------------------
# Impute SSN status for children
#--------------------------------

# Source: ITEP (https://itep.org/inclusive-child-tax-credit-reform-would-restore-benefit-to-1-million-young-dreamers/)
share_without_ssn = 1098100 / 75e6

# Assume perfect correlation of SSN status within tax units
tax_units %<>% 
  mutate(ssn          = runif(nrow(.)) > share_without_ssn,
         dep_ssn1     = if_else(!is.na(dep_age1), ssn, NA),
         dep_ssn2     = if_else(!is.na(dep_age2), ssn, NA),
         dep_ssn3     = if_else(!is.na(dep_age3), ssn, NA),
         dep_ctc_ssn1 = if_else(!is.na(dep_ctc_age1), ssn, NA),
         dep_ctc_ssn2 = if_else(!is.na(dep_ctc_age2), ssn, NA),
         dep_ctc_ssn3 = if_else(!is.na(dep_ctc_age3), ssn, NA)) %>% 
  select(-ssn)


#-----------------------
# Impute earnings split
#-----------------------


# Get and impute detail for Saez's tabulations of wage split tabulations 
wage_split_cdf = read_csv('./resources/wagesplit.csv') %>% 
  filter(year == 2014) %>% 
  
  # Assign midpoint as earnings share 
  select(wage_pctile  = wagepercentilelower, 
         `0`          = share_female_wages_0th_instant, 
         `0.025`      = share_female_wages_0th_5th, 
         `0.15`       = share_female_wages_5th_25th,
         `0.375`      = share_female_wages_25th_50th, 
         `0.675`      = share_female_wages_50th_75th, 
         `0.875`      = share_female_wages_75th_99_99th, 
         `1`          = share_female_wages_99_99th_100th) %>% 
  pivot_longer(cols      = -wage_pctile, 
               names_to  = 'secondary_share', 
               values_to = 'p') %>%
  group_by(wage_pctile) %>% 
  mutate(secondary_share = as.numeric(secondary_share), 
         p               = cumsum(p))


# Calculate wage earnings percentile on PUF
pctiles = unique(wage_split_cdf$wage_pctile)
wage_thresholds = c(wtd.quantile(x       = tax_units$wages[tax_units$wages > 0], 
                                 weights = tax_units$weight[tax_units$wages > 0], 
                                 probs   = pctiles),
                    1e99)

wage_primary_share = tax_units %>% 
  
  # Assign wage percentile
  mutate(wage_pctile = cut(x              = wages, 
                           breaks         = wage_thresholds,
                           include.lowest = T, 
                           right          = F,
                           labels         = pctiles) %>% 
           as.character() %>% 
           as.numeric() %>% 
           replace_na(0)) %>% 
  
  mutate(draw = runif(nrow(.))) %>% 
  left_join(wage_split_cdf, 
            by           = 'wage_pctile', 
            relationship = 'many-to-many') %>% 
  filter(draw <= p) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(wage_primary_share = 1 - secondary_share) %>%
  select(id, wage_primary_share)


# Add imputations to dataframe
tax_units %<>% 
  left_join(wage_primary_share, by = 'id') %>% 
  mutate(
    
    # Override imputation for records with supplemental demographic info available
    wage_primary_share = case_when(EARNSPLIT == 1 ~ 1,
                                   EARNSPLIT == 2 ~ 0.5, 
                                   EARNSPLIT == 3 ~ 0, 
                                   T              ~ wage_primary_share),
    
    # Set to 100% for all non-joint returns
    wage_primary_share = if_else(filing_status != 2, 1, wage_primary_share),
    
    # Set wages and placeholder pretax contribution variables
    wages1 = wages * wage_primary_share, 
    wages2 = wages * (1 - wage_primary_share),
    
    trad_contr_er1 = 0, 
    trad_contr_er2 = 0
  )


# Impute self-employment component split in proportion to total split
tax_units %<>%
  mutate(
    se_primary_share = E30400 / (E30400 + E30500),
    
    sole_prop1 = sole_prop * se_primary_share,
    sole_prop2 = sole_prop * (1 - se_primary_share),
    
    farm1 = farm * se_primary_share,
    farm2 = farm * (1 - se_primary_share),
    
    part_se1 = part_se * se_primary_share,
    part_se2 = part_se * (1 - se_primary_share)
  )


#------------------------------------------
# W2 wages paid in pass through businesses
#------------------------------------------

# TODO load data


# TODO impute presence of wages 

# TODO impute value | precence



#-----------------------------------------
# SSTB status for pass through businesses
#-----------------------------------------

# TODO



