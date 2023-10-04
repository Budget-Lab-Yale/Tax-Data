#------------------------------------
# process_targets.R
# 
# TODO
#------------------------------------


# Read tables 
table_names = c('table_1_4', 
                'table_1_4_kg', 
                'table_1_6', 
                'table_2_1', 
                'table_2_3', 
                'table_3_3',
                'line_item_estimates')

tables = table_names %>% 
  map(.f = ~ file.path(interface_paths$`Compiled-SOI-Tables`, paste0(.x, '.csv')) %>% 
               read_csv()) %>% 
  set_names(table_names)


#----------------
# Process tables
#----------------


# Table 1.4: reshape long in variable, wide in series type
tables$table_1_4 %<>% 
  pivot_longer(cols      = contains('__'), 
               names_to  = c('series', 'variable'), 
               names_sep = '__', 
               values_to = 'value') %>% 
  pivot_wider(names_from  = series, 
              values_from = value)

# Table 1.4 - capital gains: reshape long in variable, wide in series type
tables$table_1_4_kg %<>% 
  pivot_longer(cols      = contains('__'), 
               names_to  = c('series', 'variable'), 
               names_sep = '__', 
               values_to = 'value') %>% 
  pivot_wider(names_from  = series, 
              values_from = value)

# Table 1.6: reshape long in agi group
tables$table_1_6 %<>% 
  pivot_longer(cols      = -c(year, filing_status, age_group), 
               names_to  = 'agi', 
               values_to = 'count') %>% 
  mutate(variable = 'returns', 
         agi      = as.numeric(agi)) %>% 
  relocate(year, variable)

# Table 2.1: reshape long in variable, wide in series type
tables$table_2_1 %<>% 
  pivot_longer(cols      = contains('__'), 
               names_to  = c('series', 'variable'), 
               names_sep = '__', 
               values_to = 'value') %>% 
  pivot_wider(names_from  = series, 
              values_from = value)

# Table 2.3: reshape long in variable 
tables$table_2_3 %<>%
  pivot_longer(cols      = contains('dep'), 
               names_to  = 'variable', 
               values_to = 'count') %>% 
  relocate(year, variable) %>%
  arrange(year, variable)

# Table 3.3: reshape long in variable, wide in series type
tables$table_3_3 %<>% 
  pivot_longer(cols      = contains('__'), 
               names_to  = c('series', 'variable'), 
               names_sep = '__', 
               values_to = 'value') %>% 
  pivot_wider(names_from  = series, 
              values_from = value)



#-------------
# Get targets
#-------------

# Define function to get targets for a given row in the constraints info
get_target_value = function(constraint) {
  
  # Convert from row of tibble to list
  constraint %<>%
    as.list()
  
  # Parse categoricals
  constraint$filing_status %<>% 
    str_split_1(' ') %>% 
    as.integer()
  constraint$age_group %<>% 
    str_split_1(' ') %>% 
    as.integer()
  
  # Get copy of table
  this_table = tables[[paste0('table_', constraint$target_source)]]
  
  # Add fake grouping variables if not present in data 
  if (!('filing_status' %in% colnames(this_table))) {
    this_table$filing_status = constraint$filing_status[1]
  }
  if (!('age_group' %in% colnames(this_table))) {
    this_table$age_group = constraint$age_group[1]
  }
  
  # Get target value and return
  this_table %>% 
    filter(year == constraint$year,
           variable == constraint$variable, 
           filing_status %in% constraint$filing_status, 
           age_group %in% constraint$age_group, 
           agi >= constraint$agi_min, 
           agi < constraint$agi_max) %>% 
    summarise(target_value = sum(count)) %>% 
    return()
}



# Get targets for all specified constraints
target_info %<>% 
  bind_cols(
     map_df(.x = 1:nrow(target_info), 
            .f = ~ get_target_value(target_info[.x, ]))
  )


