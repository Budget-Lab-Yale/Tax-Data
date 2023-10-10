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
    
    # Filing status
    filing_status = if_else(married == 1, 2, if_else(xkidspop > 0, 4, 1)),
       
    # Adult ages
    age1 = ageprim, 
    age2 = if_else(married == 1, agesec, NA),
    
    # Dependent variables (assume all dependents are kids at home)
    n_dep           = pmin(3, xkidspop),
    n_dep_kids_home = n_dep,
    
    # Dependent ages (assume uniform distribution below 18)
    dep_age1 = if_else(xkidspop > 0, floor(runif(nrow(.), 0, 18)), NA),
    dep_age2 = if_else(xkidspop > 1, floor(runif(nrow(.), 0, 18)), NA),
    dep_age3 = if_else(xkidspop > 2, floor(runif(nrow(.), 0, 18)), NA),
    
    # CTC-qualifying children
    dep_ctc_age1 = if_else(!is.na(dep_age1) & dep_age1 < 17, dep_age1, NA),
    dep_ctc_age2 = if_else(!is.na(dep_age2) & dep_age2 < 17, dep_age2, NA),
    dep_ctc_age3 = if_else(!is.na(dep_age3) & dep_age3 < 17, dep_age3, NA),
    n_dep_ctc    = !is.na(dep_ctc_age1) + 
                   !is.na(dep_ctc_age2) + 
                   !is.na(dep_ctc_age3),
    
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
    lt_kg = fikgi,
    
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
  ) %>% 
  
  # Select variables defined above
  select(
    id,
    weight,
    filing_status,
    age1,
    age2,
    n_dep,
    n_dep_kids_home,
    dep_age1,
    dep_age2,
    dep_age3,
    dep_ctc_age1,
    dep_ctc_age2,
    dep_ctc_age3,
    n_dep_ctc,
    wages,
    wages1,
    txbl_int,
    qual_div,
    sole_prop,
    sole_prop1,
    lt_kg,
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

# Add to puf
test = puf_2017 %>% 
  bind_rows(nonfilers_2017)
  
  
  