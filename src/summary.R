

reweight_summary = function(puf, rw, vars) {
  
  target_summary = get_target_summary(vars)
  
  puf %>%
    mutate(across(weight, ~ .x * rw)) %>%
    group_by(agi_group) %>%
    summarise(across(vars, ~ sum(.x * weight))) %>%
    pivot_longer(!agi_group,
                 names_to= "Variable",
                 values_to = "PUF_Amount") %>%
    left_join(target_summary, by = c('agi_group', "Variable")) %>%
    filter(!is.na(SOI_Amount)) %>%
    mutate(gap = SOI_Amount - PUF_Amount) %>%
    select(Variable, agi_group, SOI_Amount, PUF_Amount, gap)  %>%
    return()
}


get_target_summary = function(vars) {
  
  guide = read_csv("resources/variable_guide.csv")
  
  target_summary = data.frame(matrix(ncol = 3, nrow = 1))
  colnames(target_summary) = c("agi", "variable", "amount")
  
  for(i in vars) {
    this_var = filter(guide, variable==i)
    
    if(!is.na(this_var$table_source)) {
      this_table = tables[[paste0('table_', this_var$table_source)]]
      
      target_summary %<>%
        bind_rows(select(filter(this_table, variable==this_var$variable),
                         agi, variable, amount))
    }
  }
  
  target_summary = target_summary[-1,]
  
  target_summary %>%
    rename(Variable = variable,
           SOI_Amount = amount,
           agi_group = agi) %>%
    return()
}


