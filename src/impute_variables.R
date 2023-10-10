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

tax_units %<>% 
  select(-starts_with('dep_age_group')) %>% 
  left_join(dep_ages, by = 'id')
  
  

#--------------------------------
# Impute SSN status for children
#--------------------------------

# TODO


#-----------------------
# Impute earnings split
#-----------------------

# TODO

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




