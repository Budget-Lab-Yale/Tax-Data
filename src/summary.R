

reweight_summary = function(puf, rw, vars) {
  
  
  target_summary = get_target_summary(vars)
  
  counts = puf %>%
    mutate(across(weight, ~ .x * rw)) %>%
    group_by(agi_group) %>%
    summarise(across(all_of(vars), ~ sum((.x > 0) * weight))) %>%
    pivot_longer(!agi_group,
                 names_to= "Variable",
                 values_to = "PUF_Count")
    
  puf %>%
    mutate(across(weight, ~ .x * rw)) %>%
    group_by(agi_group) %>%
    summarise(across(all_of(vars), ~ sum(.x * weight))) %>%
    pivot_longer(!agi_group,
                 names_to= "Variable",
                 values_to = "PUF_Amount") %>%
    left_join(counts, by = c('agi_group', 'Variable')) %>%
    left_join(target_summary, by = c('agi_group', "Variable")) %>%
    mutate(Amount_Gap = SOI_Amount - PUF_Amount,
           Amount_pct_off = Amount_Gap * 100 / SOI_Amount,
           Count_Gap = SOI_Count - PUF_Count,
           Count_pct_off = Count_Gap * 100 / SOI_Count) %>%
    select(Variable, agi_group, 
           SOI_Amount, PUF_Amount, SOI_Count, PUF_Count, 
           Amount_Gap, Amount_pct_off, Count_Gap, Count_pct_off)  %>%
    return()
}


get_target_summary = function(vars) {
  
  
  guide = read_csv("resources/variable_guide.csv")
  
  target_summary = data.frame(matrix(ncol = 4, nrow = 1))
  colnames(target_summary) = c("agi", "variable", "count", "amount")
  
  for(i in vars) {
    this_var = filter(guide, variable==i)
    
    if(!is.na(this_var$table_source)) {
      this_table = tables[[paste0('table_', this_var$table_source)]]
      
      target_summary %<>%
        bind_rows(select(filter(this_table, variable==this_var$variable),
                         agi, variable, count, amount))
    }
  }
  
  target_summary = target_summary[-1,]
  
  target_summary %>%
    rename(Variable = variable,
           SOI_Amount = amount,
           SOI_Count = count,
           agi_group = agi) %>%
    return()
}


