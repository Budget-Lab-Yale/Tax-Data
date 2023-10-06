#------------------------------------------------------------------------------
# create_2017_puf.R 
#
# TODO
#------------------------------------------------------------------------------


#------------------------------------------------------------------
# Calculate and apply AGI-group-specific scaling factor to weights 
#------------------------------------------------------------------

# Calculate weight scaling factors
weight_scaling_factors = tables$table_1_6 %>% 
  filter(year == 2017) %>% 
  group_by(agi_group = agi) %>% 
  summarise(count2017 = sum(count)) %>% 
  left_join(puf %>% 
              group_by(agi_group) %>% 
              summarise(puf = sum(S006 / 100)), 
            by = 'agi_group') %>%
  mutate(factor = count2017 / puf)

  
# Apply adjustment factors to PUF, creating 2017 tibble
puf_2017 = puf %>% 
  left_join(weight_scaling_factors, by = 'agi_group') %>% 
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


test = reweight_lp(puf_2017, targets, e = 0.5)



