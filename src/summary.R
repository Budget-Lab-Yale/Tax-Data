

reweight_summary = function(puf, vars) {
  
  
  target_summary = get_target_summary(vars)
  
  puf %>%
    group_by(agi_group) %>%
    summarise(across(all_of(vars), ~ sum((.x > 0) * weight))) %>%
    pivot_longer(!agi_group,
                 names_to= "Variable",
                 values_to = "PUF_Count") %>%
    left_join(target_summary, by = c('agi_group', "Variable")) %>%
    mutate(           Count_Gap = PUF_Count - SOI_Count,
           Count_pct_off = Count_Gap * 100 / SOI_Count) %>%
    select(Variable, agi_group, 
           SOI_Count, PUF_Count, 
           Count_Gap, Count_pct_off)  %>%
    return()
}


get_target_summary = function(vars) {
  
  
  guide = read_csv("resources/variable_guide_t.csv")
  
  target_summary = data.frame(matrix(ncol = 4, nrow = 1))
  colnames(target_summary) = c("agi", "variable", "count")
  
  for(i in vars) {
    
    this_var = filter(guide, variable==i)
    
    if(!is.na(this_var$table_source)) {
      this_table = tables[[paste0('table_', this_var$table_source)]]
      
      target_summary %<>%
        bind_rows(select(filter(this_table, variable==this_var$variable),
                           agi, variable, count))
    }
  }
  
  target_summary = target_summary[-1,]
  
  target_summary %>%
    rename(Variable = variable,
           SOI_Count = count,
           agi_group = agi) %>%
    return()
}



sum1 = reweight_summary(puf_2017, c("wages"))

viewtiful = puf_2017 %>%
  group_by(agi_group) %>%
  summarise(tot = sum((wages != 0) * weight)) %>%
  mutate(pct = tot * 100 / sum(tot)) %>%
  left_join(sum1, ., by="agi_group")


