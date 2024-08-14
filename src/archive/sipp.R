
# Define function to assign ranks for a given variable 
assign_ranks = function(values, weights, quantiles, pos_only = T) {
  
  # Add jitter to ensure unique breaks
  values = values + ((values != 0) * runif(n = length(values), min = -1, max = 1))
  
  # Subset data 
  values_subset  = values
  weights_subset = weights
  if (pos_only) {
    values_subset  = values[values > 5]  # 5 dollar assumed de-minimus reporting standard
    weights_subset = weights[values > 5]
  }
  
  # Calculate cutoffs
  cutoffs = c(wtd.quantile(values_subset, weights_subset, quantiles), Inf) 
  
  # Apply 
  cut(x = values, breaks = cutoffs, include.lowest = T, labels = quantiles) %>% 
    as.character() %>% 
    as.numeric() %>% 
    return()
}


build_tip_panel = function(year, write = F) {
  
  an_data = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/SIPP', year, 'annual_data.csv') %>%
    fread() %>%
    tibble()
  
  employed = an_data %>% filter(inc_earn > 0 & !is.na(inc_earn)) %>%
    mutate(
      parent = as.integer(n_dep > 0),
      earn_dec = cut(
        inc_earn,
        breaks = wtd.quantile(
          inc_earn,
          weights = weight,
          probs = seq(0,1,.1)
        ),
        labels = 1:10,
        include.lowest = T
      )
    )
  
  base_panel = employed %>%
    group_by(earn_dec, parent) %>%
    reframe(
      count = sum(weight),
      n_rec = sum(1 * tipped),
      t_count = sum(weight * tipped),
      
      p = (t_count / count),
      mean = wtd.mean(tips_pct, (weight * tipped))
    ) %>%
    select(earn_dec, parent, t_count, n_rec, p, mean) %>%
    left_join(
      employed %>%
        filter(tipped) %>%
        group_by(earn_dec, parent) %>%
        reframe(
          p = paste0('p',seq(0, 100, 10)),
          val = wtd.quantile(tips_pct, weight, seq(0, 1, .1)),
        ) %>%
        pivot_wider(
          names_from = p, values_from = val
        )
    ) %>%
    mutate(earn_dec = factor(earn_dec), parent = factor(parent)) %>%
    rbind(.,
          employed %>%
            filter(tipped) %>%
            reframe(
              earn_dec = factor('total'),
              parent   = factor('total'),
              t_count = sum(weight),
              n_rec = n(),
              p = t_count / sum(employed$weight),
              mean = wtd.mean(tips_pct, weight),
              val = wtd.quantile(tips_pct, weight, seq(0,1,.1)),
              p_ = paste0('p',seq(0, 100, 10))
            ) %>%
            pivot_wider(
              names_from = p_, values_from = val
            ) 
    ) %>%
    rename(
      weighted_n = t_count
    ) 
  
  if(write){
     write_csv(base_panel, 
               file.path('/gpfs/gibbs/project/sarin/shared/model_data/SIPP', year, 'tip_panel.csv')
               )
  }
  
  return(base_panel)
}


apply_tip_probs = function(tax_units, panel, filter = NA, prob = "p") {
  persons = tax_units %>%
    mutate(
      married.1      = as.integer(filing_status == 2), 
      n_kids.1       = as.integer(!is.na(dep_age1) & dep_age1 <= 24) + 
        as.integer(!is.na(dep_age2) & dep_age2 <= 24) + 
        as.integer(!is.na(dep_age3) & dep_age3 <= 24), 
      n_kids_u5.1    = as.integer(!is.na(dep_age1) & dep_age1 < 5) + 
        as.integer(!is.na(dep_age2) & dep_age2 < 5) + 
        as.integer(!is.na(dep_age3) & dep_age3 < 5),
      n_kids_0.1     = as.integer(!is.na(dep_age1) & dep_age1 == 0) + 
        as.integer(!is.na(dep_age2) & dep_age2 == 0) + 
        as.integer(!is.na(dep_age3) & dep_age3 == 0), 
      n_workers.1    = as.integer(wages1 > 0) + as.integer(wages2 > 0),
      tu_other_inc.1 = txbl_int + exempt_int + div_ord + div_pref + sole_prop + 
        txbl_ira_dist + gross_pens_dist + part_active - part_active_loss + 
        part_passive - part_passive_loss + scorp_active - scorp_active_loss +
        scorp_passive - scorp_passive_loss + rent - rent_loss +
        estate - estate_loss + farm + ui,
    ) %>%
    select(
      year, tax_unit_id = id, weight.1 = weight, married.1, age1, age2, 
      starts_with('n_kids'), starts_with('dep_age'), n_workers.1, filing_status,
      wages1, wages2, tu_wages.1 = wages, tu_ss.1 = gross_ss, tu_other_inc.1
    ) %>%
    
    # Pivot long in person
    pivot_longer(
      cols            = c(age1, age2), 
      names_prefix    = 'age', 
      names_transform = as.integer, 
      names_to        = 'person_id', 
      values_to       = 'age.1'
    ) %>% 
    filter(!is.na(age.1)) %>% 
    
    # Assign ranks for income variables
    mutate(
      wages.1 = if_else(person_id == 1, wages1, wages2), 
      across(
        .cols = c(wages.1, tu_wages.1, tu_ss.1, tu_other_inc.1), 
        .fns  = ~ assign_ranks(
          values    = ., 
          weights   = weight.1, 
          quantiles = seq(0,.9,.1) #c(seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001))
        ) %>% replace_na(-1), 
        .names = 'q.{col}'
      ),
      earn_dec = if_else(q.wages.1 == -1, -1, (q.wages.1 * 10)+1)
    ) %>% 
    select(-wages1, -wages2)
  
  if(!is.na(filter)) {
    
    if(filter==2) {
      persons %<>%
        filter(filing_status == filter)  
    } else {
      persons %<>%
        filter(filing_status != 2)
    }
  }
  
  1:10 %>%
    map(.f = ~ persons %>%
          filter(earn_dec == .x) %>%
          sample_frac(.,
                      size = unlist(panel[.x, prob])
          ) %>%
          mutate(
            tips_pct = sample(
              c(unname(unlist(panel[.x, c(paste0('p',seq(0,100,10)))]))),
              n(), 
              replace = T
            ),
            wages_tip = wages.1 * tips_pct
          ) %>%
          select(tax_unit_id, person_id, wages_tip)
    ) %>%
    bind_rows() %>%
    group_by(tax_unit_id) %>%
    pivot_wider(
      names_from = person_id,
      names_prefix = 'tips.',
      values_from = wages_tip
    )  %>%
    return()

}

assign_tips = function(tax_units, panel) {
  
  # ALTERNATIVELY, READ IN PANEL HERE SINCE IT DOESN'T CHANGE IN THIS FUNCTION
    
  dist_1 = tax_units %>%
    left_join(., apply_tip_probs(tax_units, panel), by = join_by(id == tax_unit_id)) %>%
    mutate(
      tips.1 = if_else(is.na(tips.1), 0, tips.1),
      tips.2 = if_else(is.na(tips.2), 0, tips.2),
      tips = tips.1 + tips.2
    ) %>%
    filter(wages > 0) %>%
    mutate(married = filing_status == 2,
           tipped = tips > 0) %>%
    group_by(married, tipped) %>%
    reframe(
      count = sum(weight)
    ) %>%
    group_by(married) %>%
    mutate(pct = count / sum(count)) %>%
    mutate(
      scalar = (
        (((married == 1) * .0172 ) + ((married == 0) * .0641))  / pct 
      )
    ) %>% ungroup() %>%
    filter(tipped) %>%
    print()
  
  
  panel2 = panel %>%
    mutate(
      p_m = p * unname(unlist(dist_1 %>% filter(married) %>% select(scalar))),
      p_s = p * unname(unlist(dist_1 %>% filter(!married) %>% select(scalar))),
    )
  
  print(panel2[,c("p","p_m","p_s")])
  
  tipped = apply_tip_probs(tax_units, panel2, filter = 2, prob = "p_m") %>%
    bind_rows(
      apply_tip_probs(tax_units, panel2, filter = 1, prob = "p_s")
    )
  
  tax_units %>%
    left_join(., tipped, by = join_by(id == tax_unit_id)) %>%
    mutate(
      tips.1 = if_else(is.na(tips.1), 0, tips.1),
      tips.2 = if_else(is.na(tips.2), 0, tips.2),
      tips = tips.1 + tips.2
    ) %>%
    return()
  
}

tax_units = read_csv('/gpfs/gibbs/project/sarin/shared/model_data/Tax-Data/v1/2024070817/baseline/tax_units_2018.csv') 

check = assign_tips(tax_units, base_panel)

check %>%
  filter(wages > 0) %>%
  mutate(
    tipped = tips > 0,
    married = filing_status == 2
  ) %>%
  group_by(married, tipped) %>%
  reframe(
    count = sum(weight)
  ) %>%
  group_by(married) %>%
  mutate(pct = count / sum(count))


  