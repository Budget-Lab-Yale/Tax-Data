#---------------------------
# project_puf.R 
# 
# TODO
#---------------------------



#------------------------------
# Clean and save base year PUF
#------------------------------

# Write output
tax_units %<>% 
  select(all_of(variable_guide$variable)) %>%
  write_csv(file.path(output_path, 'tax_units_2017.csv'))

# clean up enviroment 
rm(raw_puf, puf, puf_2017, qbi_variables, wage_primary_share)

#--------------------------
# Project PUF through 2019
#--------------------------

# Note: these projections sections are temporary -- will be replaced by an LP targeting approach.

# Read macro projections
macro_projections = bind_rows(
  read_csv(file.path(interface_paths$`Macro-Projections`, 'historical.csv')), 
  read_csv(file.path(interface_paths$`Macro-Projections`, 'projections.csv'))
)

# Read intermediate demographic growth targets through 2019
growth_factors = read_csv('./resources/return_counts_2019.csv') %>%
  mutate(across(.cols = -c(filing_status, age_group), 
                .fns  = ~ . / `2017`)) %>% 
  pivot_longer(cols      = -c(filing_status, age_group), 
               names_to  = 'year', 
               values_to = 'population_factor') %>% 
  mutate(year = as.integer(year)) %>% 
  
  # Add per-capita income growth components
  left_join(macro_projections %>% 
              mutate(income_factor = (gdp / pop) / (gdp[year == 2017] / pop[year == 2017])) %>%
              select(year, income_factor), 
            by = 'year')
  
  
# Project tax unit data through 2019 and write output
tax_units_2019 = 2018:2019 %>% 
  map(.f = function(y) {
    tax_units %>% 
      mutate(year      = y, 
             age_group = case_when(
               age1 < 26 ~ 1, 
               age1 < 35 ~ 2, 
               age1 < 45 ~ 3, 
               age1 < 55 ~ 4, 
               age1 < 65 ~ 5, 
               T         ~ 6)) %>% 
      left_join(growth_factors, by = c('year', 'filing_status', 'age_group')) %>% 
      mutate(weight = weight * population_factor, 
             across(.cols = all_of(variable_guide %>% 
                                     filter(income_var == 1) %>% 
                                     select(variable) %>% 
                                     deframe()), 
                    .fns  = ~ . * income_factor)) %>% 
      select(-year, -age_group, -ends_with('_factor')) %>% 
      write_csv(file.path(output_path, paste0('tax_units_', y, '.csv')))
  }) %>% 
  `[[`(2)


#--------------------------
# Project PUF through 2022
#--------------------------

tax_units_2023 = 2020:2023 %>% 
  map(.f = function(y) {
    tax_units_2019 %>% 
      mutate(year      = y, 
             age_group = case_when(
               age1 < 26 ~ 1, 
               age1 < 35 ~ 2, 
               age1 < 45 ~ 3, 
               age1 < 55 ~ 4, 
               age1 < 65 ~ 5, 
               T         ~ 6)) %>% 
      left_join(macro_projections %>% 
                  filter(year %in% 2019:2023) %>%
                  mutate(population_factor = pop / pop[year == 2019],
                         income_factor     = (gdp / pop) / (gdp[year == 2019] / pop[year == 2019])) %>%
                  select(year, population_factor, income_factor), 
                by = 'year') %>% 
      mutate(weight = weight * population_factor, 
             across(.cols = all_of(variable_guide %>% 
                                     filter(income_var == 1) %>% 
                                     select(variable) %>% 
                                     unlist() %>% 
                                     set_names(NULL)), 
                    .fns  = ~ . * income_factor)) %>% 
      select(-year, -age_group, -ends_with('_factor')) %>% 
      write_csv(file.path(output_path, paste0('tax_units_', y, '.csv')))
  }) %>% 
  `[[`(4)

rm(tax_units_2019)


#-------------------------------------
# Project PUF beyond historical years
#-------------------------------------

# Get age-specified demographic growth factors
population_factors = macro_projections %>% 
  filter(year >= 2023) %>% 
  mutate(age_group1 = pop_18_24, 
         age_group2 = pop_25_34, 
         age_group3 = pop_35_44, 
         age_group4 = pop_45_54, 
         age_group5 = pop_55_64, 
         age_group6 = pop_65_74 + pop_75_84 + pop_85_plus) %>%
  select(year, starts_with('age_group')) %>% 
  pivot_longer(cols         = -year, 
               names_prefix = 'age_group',
               names_to     = 'age_group', 
               values_to    = 'population_factor') %>% 
  group_by(age_group = as.integer(age_group)) %>%
  mutate(population_factor = population_factor / population_factor[year == 2023])
  
# Get income growth factors
income_factors = macro_projections %>% 
  filter(year >= 2023) %>% 
  select(year, pop, all_of(variable_guide$grow_with[!is.na(variable_guide$grow_with)])) %>% 
  mutate(across(.cols = -year, 
                .fns  = ~ . / .[year == 2023]), 
         across(.cols = -year, 
                .fns  = ~ . / pop)) %>% 
  select(-pop)


for (y in 2024:2053) {
  
    # Update weights 
    output = tax_units_2023 %>% 
      mutate(year      = y,
             age_group = case_when(age1 < 26 ~ 1, 
                                   age1 < 35 ~ 2, 
                                   age1 < 45 ~ 3, 
                                   age1 < 55 ~ 4, 
                                   age1 < 65 ~ 5, 
                                   T         ~ 6)) %>% 
      left_join(population_factors, by = c('year', 'age_group')) %>% 
      left_join(income_factors, by = 'year') %>% 
      mutate(weight = weight * population_factor)
    
    # Update values
    for (var in variable_guide$variable) {
      grow_with = variable_guide %>% 
        filter(variable == var) %>% 
        select(grow_with) %>% 
        deframe()
      if (!is.na(grow_with)) {
        output[[var]] = output[[var]] * output[[grow_with]]
      }
    }
    
    # Clean up and write
    output %>% 
      select(-age_group, -population_factor, -all_of(colnames(income_factors))) %>% 
      write_csv(file.path(output_path, paste0('tax_units_', y, '.csv')))
    
}




  
  
  
  
  

