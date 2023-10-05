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
            by = c('filing_status', 'agi_group'), 
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
  


