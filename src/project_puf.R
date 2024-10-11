#---------------------------------------------------
# project_puf.R 
# 
# Projects PUF past historical years based on CBO's 
# demographic and macroeconomic projections
#---------------------------------------------------


# Remove extraneous variables that were used for targeting only
vars_to_ignore = c('int_exp')

#------------------------------
# Clean and save base year PUF
#------------------------------

# Write output
tax_units %<>% 
  
  # Limit to subset of required variables, as defined by those listed in the variable guide
  select(all_of(variable_guide$variable)) %>%
  
  # Remove extraneous variables that were used for targeting only
  select(-all_of(vars_to_ignore)) %>% 
  write_csv(file.path(output_path, 'tax_units_2017.csv'))


# Clean up enviroment 
rm(raw_puf, puf, puf_2017, scf, sipp, pctile_outcomes, person_level_data, ot, ot_microdata)



#-------------------------------------
# Build demographic projection inputs
#-------------------------------------

# Read macro projections
macro_projections = bind_rows(
  read_csv(file.path(interface_paths$`Macro-Projections`, 'historical.csv')), 
  read_csv(file.path(interface_paths$`Macro-Projections`, 'projections.csv'))
)

# Extract and process demographic projections   
demog = macro_projections %>% 
  select(year, contains('married_')) %>% 
  pivot_longer(cols      = -year, 
               names_sep = '_',
               names_to  = c('married', 'age'), 
               values_to = 'n') %>% 
  mutate(married = as.integer(married == 'married'), 
         age     = pmin(80, as.integer(age))) %>% 
  group_by(year, married, age) %>% 
  summarise(n = sum(n),
            .groups = 'drop')


#--------------------------------
# Build income projection inputs
#--------------------------------

# Read supplemental projections for itemized deductions
itemized_deduction_projections = read_csv('resources/itemized_deduction_projections.csv') %>% 
  mutate(across(.cols = -year, .fns  = ~ . / lag(.) - 1))

# Read CBO's 1040 line item projections
income_near = read_csv('resources/cbo_1040.csv') %>% 
  
  # Convert to growth rates
  mutate(across(.cols = -year, 
                .fns  = ~ . / lag(.) - 1)) %>% 
  
  # Join itemized deduction projections and use where available
  left_join(itemized_deduction_projections, by = 'year') %>% 
  mutate(mortgage = if_else(is.na(mortgage), txbl_int_div_ord, mortgage), 
         charity  = if_else(is.na(charity), income, charity)) %>% 
  
  # Map to larger categories and reshape long in variable
  select(year, income, wages, int = txbl_int_div_ord, div = div_pref, kg = txbl_kg, pt, 
         pensions = txbl_pensions, ss = txbl_ss, ui, mortgage, charity) %>% 
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
         pensions = outlays_mand_oasdi,
         ss       = outlays_mand_oasdi,
         ui       = gdp, 
         mortgage = gdp_interest, 
         charity  = gdp) %>% 
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
            by = 'year') %>% 
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
irs_growth_factors_demog = read_csv('./resources/return_counts_2019.csv') %>%
  mutate(across(.cols = -c(filing_status, age_group), 
                .fns  = ~ . / `2017`)) %>% 
  pivot_longer(cols      = -c(filing_status, age_group), 
               names_to  = 'year', 
               values_to = 'population_factor') %>% 
  mutate(year = as.integer(year)) 

irs_growth_factors_income = tables$table_1_4 %>% 
  filter(variable %in% c('total_inc', 'wages', 'txbl_int', 'div', 'part_scorp', 
                         'txbl_kg.income', 'gross_pens_dist', 'rent', 'ui', 'gross_ss')) %>% 
  group_by(year, variable) %>%
  summarise(across(.cols = c(count, amount), 
                   .fns  = sum), 
            .groups = 'drop') %>% 
  mutate(average = amount / count) %>% 
  pivot_longer(cols      = c(count, amount, average), 
               names_to  = 'metric') %>% 
  pivot_wider(names_from = variable) %>% 
  rename(income = total_inc, int = txbl_int, pt = part_scorp, kg = txbl_kg.income, 
         pensions = gross_pens_dist, ss = gross_ss) %>% 
  filter(metric == 'average') %>%
  select(-metric) %>% 
  pivot_longer(cols      = -year, 
               names_to  = 'variable', 
               values_to = 'average') %>% 
  mutate(income_factor = average / average[year == 2017]) %>% 
  group_by(year) %>% 
  mutate(income_factor = ifelse(is.na(income_factor), 
                                income_factor[variable == 'income'], 
                                income_factor)) %>% 
  ungroup() %>% 
  select(-average) %>% 
  
  # Add itemized deductions, adjusting factors to be per-capita
  bind_rows(
    itemized_deduction_projections %>% 
      filter(year <= 2019) %>% 
      pivot_longer(cols      = -year, 
                   names_to  = 'variable', 
                   values_to = 'growth') %>% 
      group_by(variable) %>% 
      mutate(income_factor = cumprod(1 + replace_na(growth, 0))) %>%
      ungroup() %>% 
      left_join(
        read_csv('./resources/return_counts_2019.csv') %>% 
          pivot_longer(cols = -c(filing_status, age_group), 
                       names_to = 'year', 
                       names_transform = as.integer, 
                       values_to = 'n') %>% 
          group_by(year) %>% 
          summarise(n = sum(n), 
                    .groups = 'drop') %>%
          mutate(n = n / n[year == 2017]), 
        by = 'year') %>% 
      mutate(income_factor = income_factor / n) %>% 
      select(-growth, -n)
  )
  

# Get overall population growth factors for nonfilers
population_factors = demog %>% 
  filter(year %in% 2017:2019) %>% 
  group_by(year, married) %>% 
  summarise(n = sum(n), 
            .groups = 'drop') %>% 
  group_by(married) %>% 
  mutate(population_factor = ifelse(n > 0, 
                                    n / n[year == 2017], 
                                    1)) %>% 
  ungroup() %>% 
  select(-n)


  
# Project tax unit data through 2019 and write output
for (y in 2018:2019) {
  
  # Join and apply demographic growth factors
  output = tax_units %>% 
    
    # Create requisite groups on which to join
    mutate(year      = y, 
           age_group = case_when(
             age1 < 26 ~ 1, 
             age1 < 35 ~ 2, 
             age1 < 45 ~ 3, 
             age1 < 55 ~ 4, 
             age1 < 65 ~ 5, 
             T         ~ 6)) %>% 
    
    # Rescale weights for filers
    left_join(irs_growth_factors_demog, by = c('year', 'filing_status', 'age_group')) %>%
    mutate(weight = weight * if_else(filer == 1, population_factor, 1)) %>% 
    select(-population_factor) %>%
    
    # Rescale weights for nonfilers
    mutate(married = as.integer(filing_status == 2)) %>% 
    left_join(population_factors, by = c('year', 'married')) %>%
    mutate(weight = weight * if_else(filer == 0, population_factor, 1)) %>% 
    select(-population_factor) 
  
  # Apply intensive margin growth factors
  vars_to_grow = variable_guide %>% 
    filter(!is.na(grow_with)) %>% 
    select(variable) %>% 
    deframe() 
  for (var in vars_to_grow) {
    if (var %in% c(vars_to_ignore)) {
      next
    }
    grow_with = variable_guide %>% 
      filter(variable == var) %>% 
      select(grow_with) %>% 
      deframe()
    this_factor = irs_growth_factors_income %>% 
      filter(year == y, variable == grow_with) %>%
      select(income_factor) %>% 
      deframe() 
    output[[var]] = output[[var]] * this_factor
  }
  
  # Write output
  write_csv(output, file.path(output_path, paste0('tax_units_', y, '.csv')))
}
    
tax_units_2019 = output
    
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

for (y in 2020:2097) {
  
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
      summarise(across(.cols = all_of(variable_guide %>% 
                                        filter(!is.na(grow_with), !(variable %in% vars_to_ignore)) %>% 
                                        select(variable) %>% 
                                        deframe()), 
                       .fns  = ~ sum((. != 0) * new_weight, na.rm = T) / sum((. != 0) * weight, na.rm = T))) %>% 
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
      
      # For split variables, use overall average
      grow_with = var
      if (str_sub(var, end = -2) %in% c('wages', 'sole_prop', 'farm', 'part_se')) {
        grow_with = str_sub(var, end = -2)
      }
      
      # Get factor and apply
      this_factor = intensive_factors %>% 
        filter(variable == grow_with) %>% 
        select(intensive_factor) %>% 
        deframe()
      output[[var]] = output[[var]] * this_factor
    }
      
    # Clean up and write
    output %>% 
      mutate(weight = new_weight) %>%  
      select(variable_guide$variable[!(variable_guide$variable %in% vars_to_ignore)]) %>% 
      write_csv(file.path(output_path, paste0('tax_units_', y, '.csv')))
    
}




