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

  # Convert to index
  filter(year >= 2018) %>% 
  group_by(variable) %>% 
  mutate(income_factor = cumprod(1 + far)) %>% 
  ungroup()


#--------------------------------------------------------------
# Project PUF beyond years with (noncovid) historical tax data
#--------------------------------------------------------------


# Get age-marital status demographic growth factors
population_factors = demog %>% 
  filter(year >= 2017) %>% 
  group_by(married, age) %>% 
  mutate(population_factor = ifelse(n > 0, 
                                    n / n[year == 2017], 
                                    1)) %>% 
  ungroup() %>% 
  select(-n)

for (y in 2018:2091) {
  
    # Calculate new weights based on tax unit age composition
    new_weights = tax_units %>%
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
    output = tax_units %>% 
      left_join(new_weights, by = 'id')
    
    # Calculate intensive margin growth factors
    intensive_factors = output %>% 
      
      # First calculate extensive margin growth in each variable
      summarise(across(.cols = all_of(variable_guide %>% 
                                        filter(!is.na(grow_with)) %>% 
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
      select(all_of(variable_guide$variable)) %>% 
      write_csv(file.path(output_path, paste0('tax_units_', y, '.csv')))
    
}




  
  
  

