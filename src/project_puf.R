#---------------------------------------------------
# project_puf.R 
# 
# Projects PUF past historical years based on CBO's 
# demographic and macroeconomic projections
#---------------------------------------------------


#------------------------------
# Clean and save base year PUF
#------------------------------

# Write output
tax_units %<>% 
  select(all_of(variable_guide$variable)) %>%
  write_csv(file.path(output_path, 'tax_units_2017.csv'))

# clean up enviroment 
rm(raw_puf, puf, puf_2017, qbi_variables, wage_primary_share)



#-------------------------------------
# Build demographic projection inputs
#-------------------------------------

# Read macro projections
macro_projections = bind_rows(
  read_csv(file.path(interface_paths$`Macro-Projections`, 'historical.csv')), 
  read_csv(file.path(interface_paths$`Macro-Projections`, 'projections.csv'))
)


# Read and process CBO's demographic projections for 2023 and on  
# TODO put in Macro-Projections
cbo_demog = read_csv('./resources/cbo_demographic_projections.csv') %>% 
  pivot_longer(cols      = -year, 
               names_sep = '_',
               names_to  = c('married', 'age'), 
               values_to = 'n') %>% 
  mutate(married = as.integer(married == 'married'), 
         age     = pmin(80, as.integer(age))) %>% 
  group_by(year, married, age) %>% 
  summarise(n = sum(n),
            .groups = 'drop')
  

# Read and process ACS data for 2019-2023 demographics
# Read ACS and immediately do summary stats for memory reasons
acs_demog = read_csv(file       = file.path(interface_paths$ACS, 'usa_00008.csv.gz'), 
                     col_select = c(YEAR, PERNUM, PERWT, AGE, MARST)) %>% 
  group_by(year = YEAR, age = pmin(80, AGE), married = as.integer(!(MARST %in% 3:6))) %>%
  summarise(n = sum(PERWT),
            .groups = 'drop')

demog = bind_rows(acs_demog, cbo_demog)



#--------------------------------
# Build income projection inputs
#--------------------------------

# Read CBO's 1040 line item projections
income_near = read_csv('resources/cbo_1040.csv') %>% 
  
  # Convert to growth rates
  mutate(across(.cols = -year, 
                .fns  = ~ . / lag(.) - 1)) %>% 
  
  # Back out interest income growth rate -- based on interest's share of 
  # taxable interest + nonqualified dividends as of 2019 
  mutate(int = 1 + ((txbl_int_div_ord - 1) - (div_pref - 1) / 3) / (2 / 3)) %>% 
  
  # Map to larger categories and reshape long in variable
  select(year, income, wages, int, div = div_pref, kg = txbl_kg, pt, 
         pensions = txbl_pensions, ss = txbl_ss, ui) %>% 
  pivot_longer(cols      = -year, 
               names_to  = 'variable', 
               values_to = 'near')

# Get longer-term income factors
income_far = macro_projections %>% 
  mutate(income   = gdp, 
         wages    = gdp_wages, 
         int      = gdp_interest, 
         div      = gdp_corp, 
         kg       = gdp_corp, 
         pt       = gdp_proprietors + gdp_rent + gdp_corp, 
         rent     = gdp_rent,
         pensions = outlays_mand_oasi,
         ss       = outlays_mand_oasi,
         ui       = u3) %>% 
  select(year, all_of(variable_guide %>% 
                        filter(!is.na(grow_with)) %>% 
                        select(grow_with) %>% 
                        deframe())) %>% 
  pivot_longer(cols      = -year, 
               names_to  = 'variable', 
               values_to = 'far') %>% 
  group_by(variable) %>% 
  mutate(far = far / lag(far) - 1) %>% 
  ungroup()

# Combine, using near-term where possible
income_factors = income_far %>% 
  filter(year > 2019) %>%
  left_join(income_near, by = c('year', 'variable')) %>% 
  mutate(income_factor = if_else(is.na(near), far, near)) %>% 
  
  # Use income for rent which is missing for a few years
  left_join((.) %>% 
              filter(variable == 'income') %>% 
              select(year, rent = income_factor), 
            by = c('year')) %>% 
  mutate(income_factor = if_else(is.na(income_factor), rent, income_factor)) %>% 
  select(year, variable, income_factor) %>% 
  
  # Convert to index
  group_by(variable) %>% 
  mutate(income_factor = cumprod(1 + income_factor)) %>% 
  ungroup()



#---------------------------
# Project data through 2019
#---------------------------

# Read and process intermediate IRS-based demographic growth targets through 2019
irs_growth_factors = read_csv('./resources/return_counts_2019.csv') %>%
  mutate(across(.cols = -c(filing_status, age_group), 
                .fns  = ~ . / `2017`)) %>% 
  pivot_longer(cols      = -c(filing_status, age_group), 
               names_to  = 'year', 
               values_to = 'population_factor') %>% 
  mutate(year = as.integer(year)) 


tables$table_1_4 %>% 
  filter(variable %in% c('income', 'wages', 'txbl_int', 'div', 'part_scorp', 
                         'txbl_kg.income', 'gross_pens_dist', 'rent', 'ui', 'gross_ss')) %>% 
  mutate(average = amount / count) %>% 
  pivot_longer(cols      = c(count, amount, average), 
               names_to  = 'metric') %>% 
  pivot_wider(names_from = variable) %>% 
  rename(int = txbl_int, pt = part_scorp, kg = txbl_kg.income, 
         pensions = gross_pens_dist, ss = gross_ss) %>% 
  filter(metric == 'average') %>%
  select(-metric) %>% 
  pivot_longer(cols      = -c(year, agi), 
               names_to  = 'variable', 
               values_to = 'average') %>% 
  group_by(agi) %>% 
  mutate(factor = average / average[year == 2017], 
         factor = if_else(is.nan(factor), 1, factor))


# WHY NOT WORKING
  
  
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
      left_join(irs_growth_factors, by = c('year', 'filing_status', 'age_group')) %>% 
      mutate(weight = weight * population_factor, 
             across(.cols = all_of(variable_guide %>% 
                                     filter(!is.na(grow_with)) %>% 
                                     select(variable) %>% 
                                     deframe()), 
                    .fns  = ~ . * income_factor)) %>% 
      select(-year, -age_group, -ends_with('_factor')) %>% 
      write_csv(file.path(output_path, paste0('tax_units_', y, '.csv')))
  }) %>% 
  `[[`(2)

#--------------------------------------------------------------
# Project PUF beyond years with (noncovid) historical tax data
#--------------------------------------------------------------


# Get age-marital status demographic growth factors
population_factors = demog %>% 
  filter(year >= 2019) %>% 
  group_by(married, age) %>% 
  mutate(population_factor = ifelse(n > 0, 
                                    n / n[year == 2019], 
                                    1)) %>% 
  ungroup() %>% 
  select(-n)

for (y in 2020:2053) {
  
    # Calculate new weights based on tax unit age composition
    new_weights = tax_units_2019 %>%
      mutate(married = as.integer(filing_status == 2)) %>% 
      select(id, weight, married, age1, age2, starts_with('dep_age')) %>% 
      pivot_longer(cols         = -c(id, weight, married), 
                   names_prefix = 'age', 
                   names_to     = 'person', 
                   values_to    = 'age') %>% 
      filter(!is.na(age)) %>% 
      mutate(married = if_else(person == '1' | person == '2', married, 0)) %>% 
      left_join(population_factors %>% 
                  filter(year == y), 
                by = c('married', 'age')) %>% 
      mutate(weight = weight * population_factor) %>% 
      group_by(id) %>% 
      summarise(new_weight = mean(weight), 
                .groups = 'drop')
  
    # Update weights 
    output = tax_units_2019 %>% 
      left_join(new_weights, by = 'id')
    
    # Calculate intensive margin growth factors
    intensive_factors = output %>% 
      
      # First calculate extensive margin growth in each variable
      summarise(across(.cols = variable_guide %>% 
                         filter(!is.na(grow_with)) %>% 
                         select(variable) %>% 
                         deframe() %>% 
                         all_of(), 
                       .fns  = ~ sum((. != 0) * new_weight) / sum((. != 0) * weight))) %>% 
      mutate(across(.cols = everything(), 
                    .fns  = ~ if_else(is.nan(.), 1, .))) %>% 
      pivot_longer(cols      = everything(), 
                   names_to  = 'variable', 
                   values_to = 'extensive_factor') %>% 
      
      # Add totals and derive intensive margin growth as a residual
      left_join(variable_guide %>% 
                  select(variable, grow_with), 
                by = 'variable') %>% 
      left_join(income_factors %>% 
                  filter(year == y) %>% 
                  select(-year), 
                by = c('grow_with' = 'variable')) %>% 
      mutate(intensive_factor = income_factor / extensive_factor) %>% 
      select(variable, intensive_factor) 
      
    # Apply intensive margin rescaling factors
    for (var in intensive_factors$variable) {
      this_factor = intensive_factors %>% 
        filter(variable == var) %>% 
        select(intensive_factor) %>% 
        deframe()
      output[[var]] = output[[var]] * this_factor
    }
      
    
    # Clean up and write
    output %>% 
      select(all_of(variable_guide$variable)) %>% 
      write_csv(file.path(output_path, paste0('tax_units_', y, '.csv')))
    
}




  
  
  




test = 2019:2053 %>% 
  map(~ output_path %>% 
        file.path(paste0('tax_units_', .x, '.csv')) %>% 
        fread() %>% 
        tibble() %>% 
        summarise(year = .x,
                  wages = sum(weight * wages) / 1e9, 
                  kg = sum(pmax(-3000, kg_lt + kg_st) * weight) / 1e9)) %>% 
  bind_rows()
