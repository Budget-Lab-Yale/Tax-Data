#--------------------------------------
# impute_variables.R
# 
# Models variables not included in PUF
#--------------------------------------

# TODO list for the future
# - improve dependent age imputation
# - vary SSN status by income
# - actually impute pretax contributions
# - higher fidelty wage earnings split for EARNINGS var, including consistent notion of gender/primary status interaction  
# - gender based on Saez-Zucman 2016 tabulations



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
  group_by(age_group = case_when(
    age <= 26 ~ 1, 
    age <= 35 ~ 2, 
    age <= 45 ~ 3,
    age <= 55 ~ 4, 
    age <= 65 ~ 5,
    T         ~ 6
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
  
  mutate(draw = runif(nrow(.), max = 0.999)) %>%  # due to rounding, some of the PDFs do not sum to exactly 1  
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
    
    # Set wages
    wages1 = wages * wage_primary_share, 
    wages2 = wages * (1 - wage_primary_share)
  )


# Impute self-employment component split in proportion to total split
tax_units %<>%
  mutate(
    se_primary_share = if_else(E30400 + E30500 == 0,
                               1,
                               E30400 / (E30400 + E30500)),
    
    sole_prop1 = sole_prop * se_primary_share,
    sole_prop2 = sole_prop * (1 - se_primary_share),
    
    farm1 = farm * se_primary_share,
    farm2 = farm * (1 - se_primary_share),
    
    part_se1 = part_se * se_primary_share,
    part_se2 = part_se * (1 - se_primary_share)
  )



#-----------------------
# QBI-related variables
#-----------------------

# Read QBI variable imputation parameters and consolidate by entity form and SSTB status
qbi_params = read_csv('./resources/qbi.csv') %>% 
  group_by(form, sstb) %>% 
  summarise(share_employer = mean(share_employer),
            n              = sum(n), 
            wages          = sum(wages) * 1000, 
            net_income     = sum(net_income) * 1000, 
            .groups = 'drop')

# Calculate probability of SSTB status
sstb_params = qbi_params %>% 
  select(form, sstb, n) %>% 
  pivot_wider(names_from   = sstb, 
              names_prefix = 'sstb', 
              values_from  = n) %>% 
  mutate(p_sstb = (sstb1 / (sstb0 + sstb1)) / 5) %>%  # Scaling factor calibrated to match QBI totals  
  select(form, p_sstb)

# Assumption: wages paid are shared out in proportion to:
# (1) equally
# (2) share of positive net income
# ...plus a Gaussian noise term. 
# The idea is that wages are a linear function of (non-loss) profits, with a 
# nonzero intercept to account for "fixed costs". The weights are arbitrary, 
# roughly calibrated to match aggregate QBI statistics. 
pass_thru_micro = puf %>% 
  mutate(part  = part_active - part_active_loss + part_passive - part_passive_loss - part_179, 
         scorp = scorp_active - scorp_active_loss + scorp_passive - scorp_passive_loss - scorp_179) %>% 
  select(id, weight, sole_prop, part, scorp) %>%
  pivot_longer(cols      = -c(id, weight), 
               names_to  = 'form', 
               values_to = 'net_income') %>% 
  
  # Impute SSTB status
  left_join(sstb_params, by = 'form') %>% 
  mutate(sstb = runif(nrow(.)) < p_sstb) %>%
  
  # Impute employer status (i.e. nonzero wages)
  left_join(qbi_params %>% 
              select(form, sstb, share_employer, total_wages = wages), 
            by = c('form', 'sstb')) %>% 
  mutate(employer = runif(nrow(.)) < (share_employer + 0.2))  # scale up factor to account for the fact that employer share is defined w/r/t/ $10K, not $0K, wage definition
  
  
# Impute wages paid
qbi_variables = pass_thru_micro %>% 
  left_join(pass_thru_micro %>% 
              group_by(form, sstb) %>% 
              summarise(total_count  = sum(employer * (net_income != 0) * weight), 
                        total_profit = sum(employer * net_income * (net_income > 0) * weight), 
                        .groups = 'drop'), 
            by = c('form', 'sstb')) %>%
  mutate(share_count  = as.integer(employer * (net_income != 0))   / total_count, 
         share_profit = (employer * (net_income > 0) * net_income) / total_profit,
         random_term  = rnorm(nrow(.), mean = 1, sd = 0.15),
         wagebill     = (share_count * total_wages * 0.5 + 
                         share_profit * total_wages * 0.5) * random_term) %>% 
  
  # Convert SSTB to boolean and set values to NA for returns with no net income
  mutate(sstb = (sstb == 1),
         across(.cols = c(sstb, wagebill), 
                .fns  = ~ if_else(net_income == 0, NA, .))) %>% 
  select(id, form, sstb, wagebill) %>% 
  pivot_wider(names_from = form, 
              values_from = c(sstb, wagebill))
  

# Add to main dataframe
tax_units %<>% 
  left_join(qbi_variables, by = 'id') %>% 
  
  # Add placeholder farm QBI imputations: all farm income is eligible for QBI
  mutate(sstb_farm     = if_else(farm == 0, NA, F),
         wagebill_farm = if_else(farm > 0, farm, if_else(farm == 0, NA, 0)))
  

#---------------------------------------------
# Parent's income percentile during childhood
#---------------------------------------------

# Read intergenerational mobility parameters
mobility_matrix = 'resources/mobility_matrix.csv' %>%
  read_csv(show_col_types = F) %>% 
  pivot_longer(cols            = -child_rank, 
               names_to        = 'parent_rank', 
               names_transform = as.integer,
               values_to       = 'pdf') %>%
  relocate(parent_rank, .before = everything()) %>% 
  arrange(parent_rank, child_rank) %>%
  
  # Aggregate bottom 10 percent of children to deal with gaps around 0 income
  mutate(child_rank = pmax(10, child_rank)) %>% 
  group_by(parent_rank, child_rank) %>% 
  summarise(pdf = sum(pdf), 
            .groups = 'drop')


tax_units %<>%
  
  # Derive income measure
  mutate(
    income = wages + txbl_int + div_ord + div_pref + state_ref + 
             txbl_ira_dist + txbl_pens_dist + kg_lt + kg_st + other_gains + 
             sole_prop + part_active + part_passive - part_active_loss - 
             part_passive_loss - part_179 + scorp_active + scorp_passive -
             scorp_active_loss - scorp_passive_loss - scorp_179 + rent - 
             rent_loss + estate - estate_loss + farm + ui + gross_ss + other_inc
  ) %>% 
  
  # Add income rank
  mutate(
    child_rank = cut(
      x = income, 
      breaks = wtd.quantile(
        x       = .$income, 
        weights = .$weight, 
        probs = 9:100/100
      ), 
      labels = 10:100
    ) %>% 
      as.character() %>% 
      as.integer() %>% 
      replace_na(100), 
   group = ceiling(row_number() / 2000)
  )

  
# Expand to parent rank and sample, in chunks for memory reasons
parent_ranks = list()
for (g in unique(tax_units$group)) {
  parent_ranks[[g]] = tax_units %>% 
    filter(group == g) %>% 
    left_join(mobility_matrix, by = 'child_rank', relationship = 'many-to-many') %>% 
    group_by(id) %>%
    sample_n(1, weight = pdf) %>% 
    select(id, parent_rank)
} 

tax_units %<>% 
  left_join(parent_ranks %>% 
              bind_rows(), 
            by = 'id')


#---------------
# Tipped income 
#---------------

# Set benchmarks from 2017 IRS W2 data
n_tipped_married   = 1164973
n_tipped_unmarried = 4800747
tip_avg_married    = 7072
tip_avg_unmarried  = 5686

# Read and process SIPP data
sipp = file.path(interface_paths$SIPP, 'tip_ind_occ_full_split.csv') %>%
  fread() %>%
  tibble() %>% 
  
  # Filter to nondependent wage workers 
  filter(!is_dep, inc_wages > 0) %>% 
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


# Estimate model of being a tipped worker
tip_qrf = quantregForest(
  x        = sipp[c('wages', 'parent', 'married', 'age')],
  y        = as.factor(sipp$tipped), 
  nthreads = parallel::detectCores(),
  weights  = sipp$weight,
  mtry     = 3,
  nodesize = 5
)

# Estimate distribution parameters of tip-share-of-wages among tipped workers
tip_share_qrf = quantregForest(
  x        = sipp_tipped[c('wages')],
  y        = sipp_tipped$tip_share, 
  nthreads = parallel::detectCores(),
  weights  = sipp_tipped$weight,
  mtry     = 1,
  nodesize = 3
)

# Estimate model of the whether tips are attributable to leisure and hospitality
tip_lh_qrf = quantregForest(
  x        = sipp_tipped_21[c('tip_share', 'wages', 'parent', 'married', 'age')],
  y        = as.factor(sipp_tipped_21$tips_lh), 
  nthreads = parallel::detectCores(),
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
  pivot_longer(
    cols            = c(wages1, wages2), 
    names_prefix    = 'wages', 
    names_transform = as.integer, 
    names_to        = 'index', 
    values_to       = 'wages'
  ) %>% 
  mutate(age = if_else(index == 1, age1, age2)) %>% 
  select(-age1, -age2) %>% 
  filter(wages > 0) %>% 
  mutate(
    p = predict(
      object  = tip_qrf, 
      newdata = (.),
      what    = function(x) mean(x - 1)
    ), 
    tip_share = predict(
      object  = tip_share_qrf, 
      newdata = (.),
      what    = function(x) sample(x, 1)
    )
  ) %>% 
  mutate(
    tips_lh = predict(
      object  = tip_lh_qrf, 
      newdata = (.),
      what    = function(x) sample(x - 1, 1)
    )
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
    tips = wages * tip_share * (runif(nrow(.)) < (p * factor_p * covid_factor)) * factor_avg, 
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


#---------
# FLSA OT
#---------

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


# Estimate model of presence of FLSA-eligible OT  
has_ot_qrf = quantregForest(
  x        = ot_microdata[c('wage_pctile', 'parent', 'married', 'age')],
  y        = as.factor(ot_microdata$has_ot), 
  nthreads = parallel::detectCores(),
  weights  = ot_microdata$annualweight,
  mtry     = 3,
  nodesize = 1
)

# Estimate model of FLSA-eligible OT  
has_ot_microdata = ot_microdata %>% 
  filter(ot > 0)
ot_share_qrf = quantregForest(
  x        = has_ot_microdata[c('wage_pctile', 'parent', 'married', 'age')],
  y        = has_ot_microdata$ot_share, 
  nthreads = parallel::detectCores(),
  weights  = has_ot_microdata$annualweight,
  mtry     = 3,
  nodesize = 5
)


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
  pivot_longer(
    cols            = c(wages1, wages2), 
    names_prefix    = 'wages', 
    names_transform = as.integer, 
    names_to        = 'index', 
    values_to       = 'wages'
  ) %>% 
  mutate(age = if_else(index == 1, age1, age2)) %>% 
  select(-age1, -age2) %>% 
  filter(wages > 0) %>% 
  mutate(
    wage_pctile = cut(
      x      = wages, 
      breaks = wtd.quantile(wages + runif(nrow(.)), weight, 0:100 / 100), 
      labels = 1:100
    ) %>% as.character() %>% as.numeric() %>% replace_na(1)
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


#--------------------
# Auto loan interest
#--------------------

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


# Estimate model of presence auto loan interest  
has_auto_qrf = quantregForest(
  x        = scf[c('pctile_wages', 'pctile_income', 'n_kids', 'married', 'age1')],
  y        = as.factor(scf$has_auto_int_exp), 
  nthreads = parallel::detectCores(),
  weights  = scf$weight,
  mtry     = 4,
  nodesize = 1
)

# Estimate model of auto loan interest among those with loans
scf_int = scf %>% 
  filter(auto_int_exp > 0)
auto_qrf = quantregForest(
  x        = scf_int[c('pctile_wages', 'pctile_income', 'n_kids', 'married', 'age1')],
  y        = scf_int$auto_int_exp, 
  nthreads = parallel::detectCores(),
  weights  = scf_int$weight,
  mtry     = 4,
  nodesize = 5
)


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



#--------------------
# Childcare expenses
#--------------------

childcare_train = cps %>%
  filter(YEAR == 2017, RELATE %in% c(101, 201)) %>% 
  mutate(
    married   = as.integer(MARST %in% 1:2),
    n_dep_old = NCHILD - NCHLT5,
    care_exp  = if_else(married == 1, SPMCHXPNS / 2, SPMCHXPNS)
  ) %>% 
  select(SERIAL, FAMUNIT, weight = ASECWT, age = AGE, married, n_dep_young = NCHLT5, n_dep_old, wages = INCWAGE, care_exp)

childcare_qrf = quantregForest(
  x        = childcare_train[c('wages', 'married', 'n_dep_young', 'n_dep_old', 'age')],
  y        = childcare_train$care_exp, 
  nthreads = parallel::detectCores(),
  weights  = childcare_train$weight,
  mtry     = 4,
  nodesize = 5
)

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
  pivot_longer(
    cols            = c(wages1, wages2), 
    names_prefix    = 'wages', 
    names_transform = as.integer, 
    names_to        = 'index', 
    values_to       = 'wages'
  ) %>% 
  mutate(age = if_else(index == 1, age1, age2)) %>% 
  select(-age1, -age2) %>% 
  filter(wages > 0) %>% 
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


#----------------------------------
# Primary residence mortgage share
#----------------------------------

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
  
  # Create percentile variables for income stratification
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
prim_mort_share_qrf = quantregForest(
  x        = scf_mortgage[c('pctile_income', 'n_kids', 'married', 'age1')],
  y        = scf_mortgage$prim_mort_share, 
  nthreads = parallel::detectCores(),
  weights  = scf_mortgage$weight,
  mtry     = 4,
  nodesize = 5
)


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
    prim_mort_share = predict(
      object  = prim_mort_share_qrf, 
      newdata = (.),
      what    = function(x) sample(x, 1)
    )
  ) %>% 
  select(id, prim_mort_share)


# Add primary residence mortgage share to tax unit data
tax_units %<>% 
  left_join(prim_mort_share_imputed, by = 'id') %>% 
  mutate(
    
    # Default to 100% primary residence for those without mortgage interest
    prim_mort_share = if_else(is.na(prim_mort_share) | int_exp == 0, 1, prim_mort_share),
  )


#-----------
# TODO LIST
#-----------

# This section includes placeholder imputations that will be revisited 
# when time allows
tax_units %<>% 
  mutate(
    
    # Prior-year earnings
    ei_prior_yr = 0,
    
    # Pretax contributions via employer
    trad_contr_er1 = 0,
    trad_contr_er2 = 0,
    
    # Divorce year 
    divorce_year = if_else(alimony > 0 | alimony_exp > 0, 0, NA),
    
    # Net operating losses (currently captured through other income residual)
    nols = 0,
    
    # Other above-the-line deductions (currently captured through other income residual)
    other_above_ded = 0,
    
    # Mortgage and investment interest deduction info
    first_mort_int   = int_exp,
    second_mort_int  = 0,
    first_mort_bal   = 0, 
    second_mort_bal  = 0, 
    first_mort_year  = 0, 
    second_mort_year = 0, 
    inv_int_exp      = 0,
    
    # Personal property taxes
    salt_pers = 0
  )
