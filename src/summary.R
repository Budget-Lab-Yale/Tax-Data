#------------------------------------------------------------------
# summary.R
#
# Defines functions for getting summary stats from actuals vs PUF
#-----------------------------------------------------------------


compare_puf_actual = function(puf, yr) {
  
  # Get actuals in long format
  totals_actual = tables %>% 
    bind_rows() %>% 
    filter(year == yr,
           variable %in% colnames(puf),
           (variable %in% variable_guide$variable | 
            variable %in% paste0(inc_loss_vars, '.income') | 
            variable %in% paste0(inc_loss_vars, '.loss'))) %>% 
    select(variable, agi, count, amount) %>% 
    mutate(amount = amount / 1e6,
           count  = count / 1e6,
           agi    = as.character(agi)) %>% 
    arrange(variable, agi) %>% 
    pivot_longer(cols      = c(count, amount), 
                 names_to  = 'series', 
                 values_to = 'actual') 
  
  # Get PUF totals
  totals_puf = totals_actual %>% 
    distinct(variable) %>% 
    deframe() %>% 
    map(.f = ~ get_puf_totals(puf, totals_actual, .x)) %>%
    bind_rows() %>% 
    mutate(agi = as.character(agi)) %>% 
    pivot_longer(cols      = c(count, amount), 
                 names_to  = 'series', 
                 values_to = 'puf') 
  
  # Join and compare 
  by_agi = totals_actual %>% 
    left_join(totals_puf, by = c('variable', 'agi', 'series')) %>% 
    mutate(diff     = puf - actual,
           pct_diff = puf / actual - 1)
  
  # Look at comparison without AGI grouping
  totals = by_agi %>% 
    group_by(variable, series) %>% 
    summarise(across(.cols = c(actual, puf), 
                     .fns  = ~ sum(., na.rm = T)), 
              .groups = 'drop') %>%
    mutate(diff     = puf - actual,
           pct_diff = puf / actual - 1)
  
  return(list(by_agi = by_agi, 
              totals = totals))
}




get_puf_totals = function(puf, totals_actual, var) {
  
  # Get AGI breaks for this variable
  agi_groups = totals_actual %>% 
    filter(variable == var) %>%
    mutate(agi = as.numeric(agi)) %>% 
    distinct(agi) %>% 
    arrange(agi) %>% 
    deframe() %>% 
    c(1e99)
  
  puf %>%  
    
    # Assign AGI groups
    mutate(agi = cut(x              = E00100, 
                     breaks         = agi_groups, 
                     include.lowest = T, 
                     right          = F, 
                     labels         = head(agi_groups, -1))) %>% 
    
    # Get counts and amounts
    group_by(agi) %>% 
    summarise(across(.cols  = all_of(var), 
                     .fns   = list(count  = ~ sum((. != 0) * weight) / 1e6, 
                                   amount = ~ sum(. * weight) / 1e9), 
                     .names = '{fn}')) %>%
    
    # Pivot long in variable type 
    mutate(variable = var) %>% 
    return()
}
