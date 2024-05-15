#------------------------------------
# impute_nonfilers.R
# 
# Appends DINA nonfiling population  
# to the 2017 PUF 
#------------------------------------

# Read DINA mircodata 
dina_2017 = interface_paths$DINA %>% 
  file.path('usdina2017.dta') %>% 
  read_dta()

# Filter to nonfilers and select variables for which we have useful info
nonfilers_2017 = dina_2017 %>% 
  filter(filer == 0) %>% 
  
  # Aggregate by tax unit
  group_by(id) %>% 
  summarise(
    
    # Sample weight
    weight = mean(dweghttaxu) / 1e5,
    
    # Filing status and dependent status
    filing_status = if_else(mean(married) == 1, 2, if_else(xkidspop > 0, 4, 1)),
    dep_status    = 0,   
    
    # Other missing demographics
    EARNSPLIT = NA, 
    GENDER    = NA,
    
    # Adult ages group 
    age_group = case_when(
      max(ageprim) == 20 ~ floor(runif(nrow(.), 1, 4)),
      max(ageprim) == 45 ~ floor(runif(nrow(.), 4, 6)),
      T             ~ 6
    ),
    
    # Dependent variables (assume all dependents are kids at home)
    n_dep = pmin(3, mean(xkidspop)),
    
    # Dependent age groups
    dep_age_group1 = if_else(mean(xkidspop) == 0, NA,
                             sample(x       = c(1, 2, 3, 4), 
                                    size    = nrow(.), 
                                    replace = T, 
                                    prob    = c((5 - 0)   / 19, 
                                                (13 - 5)  / 19, 
                                                (17 - 13) / 19, 
                                                (19 - 17) / 19))),
    dep_age_group2 = if_else(mean(xkidspop) <= 1, NA,
                             sample(x       = c(1, 2, 3, 4), 
                                    size    = nrow(.), 
                                    replace = T, 
                                    prob    = c((5 - 0)   / 19, 
                                                (13 - 5)  / 19, 
                                                (17 - 13) / 19, 
                                                (19 - 17) / 19))),
    dep_age_group3 = if_else(mean(xkidspop) <= 2, NA,
                             sample(x       = c(1, 2, 3, 4), 
                                    size    = nrow(.), 
                                    replace = T, 
                                    prob    = c((5 - 0)   / 19, 
                                                (13 - 5)  / 19, 
                                                (17 - 13) / 19, 
                                                (19 - 17) / 19))),
    
    # Wage earnings
    wages  = sum(wagind), 
    wages1 = wageind[1],
   
    # Interest income (assume all taxable)
    txbl_int = sum(fiint),
    
    # Dividends (assume all qualified)
    qual_div = sum(fidiv),
    
    # Sole prop net income
    sole_prop  = sum(schcinc),
    sole_prop1 = schcinc[1],
    
    # Capital gains (assume all are long-term)
    kg_lt = sum(fikgi),
    
    # Pension income (includes IRA distributions here)
    gross_pens_dist = sum(peninc),
    txbl_pens_dist  = sum(peninc),
    
    # S corp income/loss (assume all active)
    scorp_active      = if_else(mean(scorinc) > 0, mean(scorinc), 0),
    scorp_active_loss = if_else(mean(scorinc) < 0, mean(scorinc), 0),
    
    # Partnership income/loss (assume all active)
    part_active      = if_else(mean(partinc) > 0, mean(partinc), 0),
    part_active_loss = if_else(mean(partinc) < 0, mean(partinc), 0),
    part_se          = mean(partinc),
    part_se1         = partinc[1],

    # Estate/trust income and loss
    estate      = if_else(mean(estinc) > 0, mean(estinc), 0),
    estate_loss = if_else(mean(estinc) < 0, mean(estinc), 0),
   
    # Rent/royalty income and loss
    rent      = if_else(mean(rentinc + rylinc) > 0, mean(rentinc + rylinc), 0),
    rent_loss = if_else(mean(rentinc + rylinc) < 0, mean(rentinc + rylinc), 0),
    
    # Unemployment benefits
    ui = mean(uiinc),
    
    # Social Security benefits
    gross_ss = mean(ssinc_oa + ssinc_di)
    
  ) %>% 
  
  mutate(
    
    # Record ID
    id = 1e6 + row_number(),
    
    # Credit-qualifying children: assume all age-qualifying dependents qualify
    n_dep_ctc  = (!is.na(dep_age_group1) & dep_age_group1 < 4) + 
      (!is.na(dep_age_group2) & dep_age_group2 < 4) + 
      (!is.na(dep_age_group3) & dep_age_group3 < 4),
    n_dep_eitc = n_dep_ctc
    
  ) %>% 
  
  # Select variables defined above
  select(
    id,
    filer,
    weight,
    filing_status,
    dep_status,
    age_group,
    n_dep,
    dep_age_group1,
    dep_age_group2,
    dep_age_group3,
    n_dep_ctc,
    n_dep_eitc,
    wages,
    wages1,
    txbl_int,
    qual_div,
    sole_prop,
    sole_prop1,
    kg_lt,
    gross_pens_dist,
    txbl_pens_dist,
    scorp_active,
    scorp_active_loss,
    part_active,
    part_active_loss,
    part_se,
    part_se1,
    estate,
    estate_loss,
    rent,
    rent_loss,
    ui,
    gross_ss
  ) 

# Add remaining PUF variables, set to 0 
remaining_vars = colnames(puf_2017)[!(colnames(puf_2017) %in% colnames(nonfilers_2017))]
nonfilers_2017 %<>% 
  bind_cols(
    rep(0, length(remaining_vars)) %>% 
      set_names(remaining_vars) %>% 
      map_df(.f = ~ 0)     
  )

# Add to PUF
tax_units = puf_2017 %>% 
  bind_rows(nonfilers_2017) 
  

# # Adjust nonfiler weights to match 2017 population 
# pop_actual     = 325e6
# scaling_factor = tax_units %>% 
#   group_by(filer) %>% 
#   summarise(n = sum(weight * (1 + (filing_status == 2) + n_dep))) %>% 
#   mutate(excess = sum(n) - pop_actual) %>% 
#   filter(filer == 0) %>% 
#   mutate(scaling_factor = (n - excess) / n) %>% 
#   select(scaling_factor) %>% 
#   deframe()
#   
# tax_units %<>% 
#   mutate(weight = if_else(filer == 1, 
#                           weight, 
#                           weight * scaling_factor))


