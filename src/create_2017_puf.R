#------------------------------------------------------------------------------
# create_2017_puf.R 
#
# Reweights data to match target-year count totals PUF, then re-scales 
# financial values so as to match reporting amount totals 
#------------------------------------------------------------------------------


#--------------------------------------------------------------
# Calculate and apply group-specific scaling factor to weights 
#--------------------------------------------------------------

puf %<>%
  mutate(married = as.integer(filing_status == 2), 
         senior  = as.integer(age_group == 6)) 

# Calculate weight scaling factors by semi-aggregated filing status X age X AGI groups
weight_scaling_factors = tables$table_1_6 %>% 
  filter(year == 2017) %>% 
  group_by(married   = as.integer(filing_status == 2), 
           senior    = as.integer(age_group == 6), 
           agi_group = agi) %>% 
  summarise(count2017 = sum(count), 
            .groups = 'drop') %>% 
  left_join(puf %>% 
              group_by(married, senior, agi_group) %>% 
              summarise(puf = sum(weight), 
                        .groups = 'drop'), 
            by = c('married', 'senior', 'agi_group')) %>%
  mutate(factor = count2017 / puf) %>% 
  select(-count2017, -puf)

  
# Apply adjustment factors to PUF, creating 2017 tibble
puf_2017 = puf %>% 
  left_join(weight_scaling_factors, by = c('married', 'senior', 'agi_group')) %>% 
  mutate(weight = weight * factor) %>% 
  select(-factor)


#------------------------------------------------
# Target specified count variables via LP method
#------------------------------------------------

# Get target values for all specified constraints
targets = target_info %>% 
  bind_cols(
    map(.x = 1:nrow(target_info), 
        .f = ~ get_target_value(target_info[.x, ])) %>% 
      bind_rows()
  )

# Solve reweighting problem and reweight data
weight_deltas = reweight_lp(puf_2017, targets, e = 0.66)

puf_2017 %<>% 
  mutate(weight = weight * weight_deltas)


#------------------------------------------------
# Re-scale values to match dollar amount targets
#------------------------------------------------


# Get PUF-actual comparison on post-reweighting data
summary_stats = compare_puf_actual(puf_2017, 2017)

# Set AGI bins by which to rescale values
rescaling_agi_groups = c(-1e99, 1, 50000, 1e5, 2e5, 5e5, 1e6,  1e7, 1e99)

# Get vector of variables to rescale 
to_rescale = variable_guide %>% 
  filter(!is.na(grow_with), 
         !(variable %in% inc_loss_vars), 
         variable %in% summary_stats$totals$variable) %>% 
  select(variable) %>% 
  deframe() %>% 
  c(paste0(inc_loss_vars, '.income'), 
    paste0(inc_loss_vars, '.loss'))

for (var in to_rescale) {
  
  # Calculate value adjustment factors by agi group
  adj_factors = summary_stats$by_agi %>% 
    filter(variable == var, 
           series   == 'amount') %>% 
    mutate(agi = cut(x              = as.numeric(agi), 
                     breaks         = rescaling_agi_groups,
                     include.lowest = T, 
                     right          = F, 
                     labels         = head(rescaling_agi_groups, -1)) %>% 
             as.character()) %>% 
    group_by(agi) %>% 
    summarise(factor = sum(actual) / sum(puf)) 
  
  
  # Assign AGI-specific rescaling factors on PUF  
  if (nrow(adj_factors) == 1) {
    puf_2017 %<>% 
      mutate(agi    = '-1e99', 
             factor = adj_factors$factor)  
  } else {
    puf_2017 %<>%  
      mutate(agi = cut(x              = E00100, 
                       breaks         = rescaling_agi_groups, 
                       include.lowest = T, 
                       right          = F, 
                       labels         = head(rescaling_agi_groups, -1)) %>% 
               as.character()) %>% 
      left_join(adj_factors, by = 'agi') 
  }
  
  # Apply factors
  puf_2017 %<>%
    mutate(across(.cols = all_of(var), 
                  .fns  = ~ . * if_else(is.nan(factor) | is.infinite(factor), 
                                        1, 
                                        factor))) %>%
    select(-agi, -factor)
}

# Manually sale up other variables
# (1) Self employment component of partnership income
puf_2017 %<>% 
  mutate(
    part_se = part_se * (summary_stats$by_agi %>% 
                           filter(variable == 'part_active', 
                                  series   == 'amount') %>% 
                           summarise(factor = sum(actual) / sum(puf)) %>% 
                           select(factor) %>% 
                           deframe()) 
  )

# Re-join income-loss variables
for (var in inc_loss_vars) {
  puf_2017[var] = puf_2017[[paste0(var, '.income')]] - puf_2017[[paste0(var, '.loss')]]
}

