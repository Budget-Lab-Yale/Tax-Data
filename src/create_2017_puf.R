#------------------------------------------------------------------------------
# create_2017_puf.R 
#
# TODO
#------------------------------------------------------------------------------


#--------------------------------------------------------------
# Calculate and apply group-specific scaling factor to weights 
#--------------------------------------------------------------

puf %<>%
  mutate(married = as.integer(MARS == 2), 
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
              summarise(puf = sum(S006 / 100), 
                        .groups = 'drop'), 
            by = c('married', 'senior', 'agi_group')) %>%
  mutate(factor = count2017 / puf)

  
# Apply adjustment factors to PUF, creating 2017 tibble
puf_2017 = puf %>% 
  left_join(weight_scaling_factors, by = c('married', 'senior', 'agi_group')) %>% 
  mutate(S006 = S006 * factor) %>% 
  select(-factor)


#------------------------------------------------
# Target specified count variables via LP method
#------------------------------------------------

# Get targets for all specified constraints
targets = target_info %>% 
  bind_cols(
    map_df(.x = 1:nrow(target_info), 
           .f = ~ get_target_value(target_info[.x, ]))
  )


weight_deltas = reweight_lp(puf_2017, targets, e = 0.5)

puf_2017 %<>% 
  mutate(S006 = S006 * weight_deltas)

