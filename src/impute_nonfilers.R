#------------------------------------
# impute_nonfilers.R
# 
# TODO
#------------------------------------

# Read DINA mircodata 
dina_2017 = interface_paths$DINA %>% 
  file.path('usdina2017.dta') %>% 
  read_dta()

# Filter to nonfilers and select variables for which we have useful info
nonfilers_2017 = dina_2017 %>% 
  filter(filer == 0) %>% 
  mutate(
    
    # Record ID
    id = 1e6 + row_number(),
      
    # Sample weight
    weight = dweghttaxu / 1e5,
    
    # Filing status and dependent status
    filing_status = if_else(married == 1, 2, if_else(xkidspop > 0, 4, 1)),
    dep_status    = 0,   
    
    # Other missing demographics
    EARNSPLIT = NA, 
    GENDER    = NA,
    
    # Adult ages group 
    age_group = case_when(
      ageprim == 20 ~ floor(runif(nrow(.), 1, 4)),
      ageprim == 45 ~ floor(runif(nrow(.), 4, 6)),
      T               ~ 6
    ),
    
    # Dependent variables (assume all dependents are kids at home)
    n_dep = pmin(3, xkidspop),
    
    # Dependent age groups
    dep_age_group1 = if_else(xkidspop == 0, NA,
                             sample(x       = c(1, 2, 3, 4), 
                                    size    = nrow(.), 
                                    replace = T, 
                                    prob    = c((5 - 0)   / 19, 
                                                (13 - 5)  / 19, 
                                                (17 - 13) / 19, 
                                                (19 - 17) / 19))),
    dep_age_group2 = if_else(xkidspop <= 1, NA,
                             sample(x       = c(1, 2, 3, 4), 
                                    size    = nrow(.), 
                                    replace = T, 
                                    prob    = c((5 - 0)   / 19, 
                                                (13 - 5)  / 19, 
                                                (17 - 13) / 19, 
                                                (19 - 17) / 19))),
    dep_age_group3 = if_else(xkidspop <= 2, NA,
                             sample(x       = c(1, 2, 3, 4), 
                                    size    = nrow(.), 
                                    replace = T, 
                                    prob    = c((5 - 0)   / 19, 
                                                (13 - 5)  / 19, 
                                                (17 - 13) / 19, 
                                                (19 - 17) / 19))),
    
    # credit-qualifying children: assume all age-qualifying dependents qualify
    dep_ctc_age_group1 = if_else(!is.na(dep_age_group1) & dep_age_group1 < 4, dep_age_group1, NA),
    dep_ctc_age_group2 = if_else(!is.na(dep_age_group2) & dep_age_group2 < 4, dep_age_group2, NA),
    dep_ctc_age_group3 = if_else(!is.na(dep_age_group3) & dep_age_group3 < 4, dep_age_group3, NA),
    n_dep_ctc          = !is.na(dep_ctc_age_group1) + 
                         !is.na(dep_ctc_age_group2) + 
                         !is.na(dep_ctc_age_group3),
    n_dep_eitc = n_dep_ctc,
    
    # Wage earnings (assume 100% is earned by primary)
    wages  = wagind, 
    wages1 = wagind,
   
    # Interest income (assume all taxable)
    txbl_int = fiint,
    
    # Dividends (assume all qualified)
    qual_div = fidiv,
    
    # Sole prop net income
    sole_prop  = schcinc,
    sole_prop1 = schcinc,
    
    # Capital gains (assume all are long-term)
    kg_lt = fikgi,
    
    # Pension income (includes IRA distributions here)
    gross_pens_dist = peninc,
    txbl_pens_dist  = peninc,
    
    # S corp income/loss (assume all active)
    scorp_active      = if_else(scorinc > 0, scorinc, 0),
    scorp_active_loss = if_else(scorinc < 0, scorinc, 0),
    
    # Partnership income/loss (assume all active)
    part_active      = if_else(partinc > 0, partinc, 0),
    part_active_loss = if_else(partinc < 0, partinc, 0),
    part_se          = partinc,
    part_se1         = partinc,

    # Estate/trust income and loss
    estate      = if_else(estinc > 0, estinc, 0),
    estate_loss = if_else(estinc < 0, estinc, 0),
   
    # Rent/royalty income and loss
    rent      = if_else(rentinc + rylinc > 0, rentinc + rylinc, 0),
    rent_loss = if_else(rentinc + rylinc < 0, rentinc + rylinc, 0),
    
    # Unemployment benefits
    ui = uiinc,
    
    # Social Security benefits
    gross_ss = ssinc_oa + ssinc_di
    
    # Imputed variables: 
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
  
  