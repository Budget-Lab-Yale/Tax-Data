#------------------------------------
# process_puf.R
# 
# TODO
#------------------------------------



# Read 2015 PUF
raw_puf = interface_paths$`IRS-PUF` %>% 
  file.path('puf_2015.csv') %>%
  read_csv() %>% 
  left_join(interface_paths$`IRS-PUF` %>% 
              file.path('demographics_2015.csv') %>%
              read_csv(), 
            by = 'RECID')

puf = raw_puf


#-------------------
# Impute age groups
#-------------------

# Set agi bins for age imputation modeling 
age_dist_agi_groups = c(-1e99, 75000, 1e99)

# Get target distribution of ages from SOI table 1.6
age_dist = tables$table_1_6 %>% 
  filter(year == 2015) %>% 
  mutate(agi_group = cut(x              = agi, 
                         breaks         = age_dist_agi_groups, 
                         include.lowest = T, 
                         right          = F, 
                         labels         = head(age_dist_agi_groups, -1))) %>% 
  group_by(filing_status, agi_group, age_group) %>% 
  summarise(actual = sum(count), 
            .groups = 'drop')


# Impute topcoded age range for dependent returns
puf %<>% 
  mutate(
    
    # Add AGI group
    agi_group = cut(x              = E00100, 
                    breaks         = age_dist_agi_groups, 
                    include.lowest = T, 
                    right          = F, 
                    labels         = head(age_dist_agi_groups, -1)),
    
    # ASSUMPTION: dependent ages are uniformly distributed above age 26
    p = runif(nrow(.)),
    old_dep_group = case_when(
      p < 1/5 ~ 2, 
      p < 2/5 ~ 3,
      p < 3/5 ~ 4,
      p < 4/5 ~ 5,
      T       ~ 6
    ),
    
    # Recode age group variabe
    age_group = if_else(DSI == 1,
                        case_when(AGERANGE == 0     ~ 0,
                                  AGERANGE %in% 1:2 ~ 1, 
                                  T                 ~ old_dep_group), 
                        AGERANGE)
  )

age_dist_cdf = age_dist %>% 
  
  # Calculate difference between PUF  and actuals
  left_join(
    puf %>% 
      group_by(filing_status = MARS, 
               agi_group, 
               age_group) %>% 
      summarise(puf = sum(S006 / 100), 
                .groups = 'drop'),
    by = c('filing_status', 'agi_group', 'age_group')
  ) %>% 
  mutate(diff = puf - actual) %>% 
  
  # Calculate CDF for ages by filing status / AGI group
  filter(diff < 0) %>%
  group_by(filing_status, agi_group) %>% 
  mutate(p = cumsum(diff / sum(diff))) %>% 
  select(filing_status, agi_group, p, age_group)


# Assign age groups to records with missing age info 
imputed_age_groups = puf %>% 
  filter(is.na(age_group) | age_group == 0) %>%
  select(RECID, filing_status = MARS, agi_group) %>%
  mutate(draw = runif(nrow(.))) %>% 
  left_join(age_dist_cdf, 
            by           = c('filing_status', 'agi_group'), 
            relationship = 'many-to-many') %>% 
  filter(draw <= p) %>% 
  group_by(RECID) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(RECID, imputed_age_group = age_group)

puf %<>%
  left_join(imputed_age_groups, by = 'RECID') %>% 
  mutate(age_group = if_else(is.na(age_group) | age_group == 0, 
                             imputed_age_group,
                             age_group)) %>% 
  select(-agi_group, -imputed_age_group)
  

#-----------------------------
# Impute dependent age groups
#-----------------------------

# Recode dependent info
puf %<>%
  mutate(n_dep_kids_home = XOCAH,
         n_dep_kids_away = XOCAWH,
         n_dep_parents   = XOPAR,
         n_dep_other     = XOODEP,
         n_dep           = XOCAH + XOCAWH + XOPAR + XOODEP, 
         across(.cols = contains('AGEDP'), 
                .fns  = ~ if_else(. == 0, NA, .))) %>% 
  rename(dep_age_group1 = AGEDP1, 
         dep_age_group2 = AGEDP2, 
         dep_age_group3 = AGEDP3)

# Get age range CDFs by dependent types 
# ASSUMPTION: estimated on 1-dependent filers only 
dep_age_cdf = puf %>% 
  filter(!is.na(dep_age_group1), n_dep == 1) %>% 
  mutate(dep_type = case_when(
    N24             == 1 ~ 'kids_ctc',
    n_dep_kids_home == 1 ~ 'kids_home', 
    n_dep_kids_away == 1 ~ 'kids_away', 
    n_dep_parents   == 1 ~ 'parents',
    T                    ~ 'other'
  )) %>% 
  group_by(dep_type, dep_age_group = dep_age_group1) %>% 
  summarise(n = sum(S006) / 100, 
            .groups = 'drop') %>%
  group_by(dep_type) %>% 
  mutate(p = cumsum(n) / sum(n)) %>% 
  ungroup() %>% 
  select(-n)
  

imputed_dep_age_groups = puf %>% 
  
  # Determine number of dependent age ranges to impute by record
  mutate(n_missing   = is.na(dep_age_group1) + 
                       is.na(dep_age_group2) + 
                       is.na(dep_age_group3), 
         n_to_impute = n_missing + n_dep - 3) %>% 
  filter(n_to_impute > 0) %>% 
  
  # Determine number of dependents to impute by type
  mutate(n_to_impute_kids_ctc  = pmin(N24, n_to_impute), 
         n_to_impute_kids_home = pmin(n_dep_kids_home,
                                      n_to_impute - 
                                      n_to_impute_kids_ctc),
         n_to_impute_kids_away = pmin(n_dep_kids_away,
                                      n_to_impute - 
                                      n_to_impute_kids_ctc - 
                                      n_to_impute_kids_home),
         n_to_impute_parents   = pmin(n_dep_parents,
                                      n_to_impute - 
                                      n_to_impute_kids_ctc - 
                                      n_to_impute_kids_home - 
                                      n_to_impute_kids_away),
         n_to_impute_other     = pmin(n_dep_other,
                                      n_to_impute - 
                                      n_to_impute_kids_ctc - 
                                      n_to_impute_kids_home - 
                                      n_to_impute_kids_away - 
                                      n_to_impute_parents)) %>% 
  
  # Reshape long in dependent type
  select(RECID, starts_with('n_to_impute_')) %>% 
  pivot_longer(cols         = -RECID, 
               names_to     = 'dep_type', 
               names_prefix = 'n_to_impute_', 
               values_to    = 'n') %>% 
  filter(n > 0) %>% 
  
  # Impute ages for each dependent
  mutate(draw = runif(nrow(.))) %>% 
  left_join(dep_age_cdf, 
            by           = 'dep_type', 
            relationship = 'many-to-many') %>% 
  filter(draw <= p) %>% 
  group_by(RECID, dep_type) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(RECID, dep_age_group)


# Re-sort dependent age groups and merge into PUF
puf %<>% 
  left_join(
    puf %>% 
      select(RECID, starts_with('dep_age_')) %>% 
      pivot_longer(cols         = -RECID, 
                   names_to     = 'dep', 
                   names_prefix = 'dep_age_group', 
                   values_to    = 'dep_age_group') %>% 
      select(-dep) %>% 
      filter(!is.na(dep_age_group)) %>% 
      bind_rows(imputed_dep_age_groups) %>% 
      group_by(RECID) %>% 
      arrange(dep_age_group) %>% 
      mutate(dep = row_number()) %>% 
      ungroup() %>% 
      pivot_wider(names_from   = dep, 
                  names_prefix = 'imputed_dep_age_group',
                  values_from  = dep_age_group), 
    by = 'RECID') %>% 
  mutate(dep_age_group1 = if_else(n_dep > 0, imputed_dep_age_group1, dep_age_group1), 
         dep_age_group2 = if_else(n_dep > 0, imputed_dep_age_group2, dep_age_group2), 
         dep_age_group3 = if_else(n_dep > 0, imputed_dep_age_group3, dep_age_group3))
    
    







