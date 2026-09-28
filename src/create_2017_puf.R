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
# Target return counts: S40 calibration (stage 1)
#------------------------------------------------

# The filers are calibrated to Pub 1304 counts (src/refit_filers.R), from the
# weights above, with records classed by E00100 -- the AGI basis the value
# rescaling below uses -- so the amounts are rescaled on calibrated weights.
# Ages enter as the age-group band (1 = under 26, ..., 6 = 65+). Stage 2
# (src/refit_filers_2017.R) refits by the AGI proxy after the imputations.
# This replaces the LP (reweight_lp(), src/reweight.R), whose objective -- the
# sum of the multipliers -- left every record at a bound
# (research/state_weights/notes/filer_refit_design.md).
source('./src/refit_filers.R')
AGE_GROUP_AGE = c(20, 30, 40, 50, 60, 70)   # an age inside each age_group band
stopifnot(all(puf_2017$age_group %in% 1:6), all(puf_2017$filing_status %in% 1:4))
stage1 = refit_classed(
  puf_2017 %>% transmute(id, weight, agi_proxy = E00100, filing_status,
                         age1 = AGE_GROUP_AGE[age_group],
                         dep_status = as.integer(dep_status), wages),
  2017L, label = 'refit_filers stage 1 (E00100)')
stopifnot(identical(stage1$id, puf_2017$id))
puf_2017$weight = stage1$weight
rm(stage1)


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

