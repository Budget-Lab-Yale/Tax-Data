#---------------------------------------------------
# process_puf.R
# 
# Reads, cleans, and imputes variables required for 
# reweighting process
#---------------------------------------------------



# Read 2015 PUF
raw_puf = interface_paths$`IRS-PUF` %>% 
  file.path('puf_2015.csv') %>%
  read_csv() %>% 
  left_join(interface_paths$`IRS-PUF` %>% 
              file.path('demographics_2015.csv') %>%
              read_csv(), 
            by = 'RECID')

# Remove aggregate returns (for now)
puf = raw_puf %>% 
  filter(RECID < 999996) 


#-----------------------------------------------
# Impute age groups (necessary for reweighting)
#-----------------------------------------------

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
    
    # Recode age group variable
    age_group = if_else(DSI == 1,
                        case_when(AGERANGE == 0     ~ 0,
                                  AGERANGE %in% 1:2 ~ 1, 
                                  T                 ~ old_dep_group), 
                        AGERANGE)
  )

# Calculate PDF for age ranges by filing status / AGI group
age_dist_pdf = age_dist %>% 
  
  # Calculate difference between PUF and actuals
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
  
  # Calculate CDF
  filter(diff < 0) %>%
  group_by(filing_status, agi_group) %>% 
  mutate(p = diff / sum(diff)) %>% 
  select(filing_status, agi_group, p, age_group)


# Assign age groups to records with missing age info 
imputed_age_groups = puf %>% 
  filter(is.na(age_group) | age_group == 0) %>%
  select(RECID, filing_status = MARS, agi_group) %>%
  mutate(draw = runif(nrow(.))) %>% 
  left_join(age_dist_pdf, 
            by           = c('filing_status', 'agi_group'), 
            relationship = 'many-to-many') %>% 
  group_by(RECID) %>% 
  sample_n(size = 1, weight = p) %>% 
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
                .fns  = ~ na_if(., 0))) %>% 
  rename(dep_age_group1 = AGEDP1, 
         dep_age_group2 = AGEDP2, 
         dep_age_group3 = AGEDP3)

# Get age range PDFs by dependent types 
# ASSUMPTION: estimated on 1-dependent filers only 
dep_age_pdf = puf %>% 
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
  mutate(p = n / sum(n)) %>% 
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
  expand_grid(dep_id = 1:3) %>% 
  filter(dep_id <= n) %>% 
  
  # Impute ages for each dependent
  left_join(dep_age_pdf, 
            by           = 'dep_type', 
            relationship = 'many-to-many') %>% 
  group_by(RECID, dep_type, dep_id) %>% 
  sample_n(size = 1, weight = p) %>% 
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
    

#-------------------------------------------
# Rename, process, and subset PUF variables
#-------------------------------------------


# Create named list for to crosswalk between PUF and tax simulator names
variable_name_crosswalk = variable_guide %>% 
  filter(!is.na(name_puf)) 
variable_name_crosswalk = variable_name_crosswalk$variable %>% 
  set_names(variable_name_crosswalk$name_puf)
  
  
# Define AGI groups
agi_groups_2015 = tables$table_1_6 %>% 
  filter(year == 2015) %>% 
  distinct(agi) %>%
  deframe() %>% 
  c(1e99)


puf %<>%
  
  mutate(
    
    # Create correct-unit weight variable
    weight = S006 / 100,
    
    # Add filer variable
    filer = 1,
    
    # Add target variable dummies
    returns = 1, 
    has_dep = as.integer(n_dep > 0),
    
    # Add AGI group variable per table 1.6 
    agi_group = cut(x              = E00100, 
                    breaks         = agi_groups_2015, 
                    include.lowest = T, 
                    right          = F, 
                    labels         = head(agi_groups_2015, -1)) %>% 
      as.character() %>% 
      as.numeric(),
    
    # Add partnership-scorp var for targeting
    part_scorp      = if_else(E26270 > 0, E26270, 0),
    part_scorp_loss = if_else(E26270 < 0, E26270, 0),
    
    # Derived variable: non-preferred dividend income
    div_ord = E00600 - E00650,
    
    # Derived variable: partnership income subject to SECA
    part_se = if_else(E25940 + E25980 - E25920 - E25960 != 0,  
                      (E30400 + E30500) / 0.9235 - E00900 - E02100,
                      0),
    
    # Other income residual in AGI
    other_inc = E00100 - E00200 - E00300 - E00600 - E00700 - E00800 - E00900 - 
                E01000 - E01100 - E01200 - E01400 - E01700 - E02000 - E02100 - 
                E02300 - E02500,
    
    #  itemized deductions
    sch_a_4 = pmax(0, E17500 - pmax(0, E00100 * if_else(age_group == 6, 0.075, 0.1))), 
    sch_a_9 = E18400 + E18500,
    other_item_exp = if_else(FDED == 1, 
                             P04470 + E21040 - sch_a_4 - sch_a_9 - E19200 - E19700 - E20500 - E20800, 
                             0), 
    
    # Recode dependent status
    DSI = DSI == 1
  ) %>% 
  
  # Rename and subset variables
  rename_with(.fn = ~ if_else(. %in% names(variable_name_crosswalk), 
                              variable_name_crosswalk[.], 
                              .)) %>% 
  select(any_of(variable_guide$variable), 
         GENDER,
         EARNSPLIT,
         contains('age_group'), 
         returns, 
         has_dep, 
         part_scorp, 
         part_scorp_loss,
         E00100,
         E30400,
         E30500,
         agi_group) %>%
  
  # Split income-loss variables 
  mutate(across(.cols  = all_of(inc_loss_vars),
                .fns   = list(income = ~ if_else(. > 0, ., 0), 
                              loss   = ~ if_else(. < 0, - ., 0)), 
                .names = '{col}.{fn}'))

