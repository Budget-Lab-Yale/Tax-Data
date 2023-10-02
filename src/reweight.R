
############### COMPLETED #####################

build_rhs = function(path_soi, path_targ_info) {
  
  parse_soi(path_soi) %>%
    calibrate_bounds(., parse_targ(path_targ_info)) %>%
    rename(AGI = AGI_Group2) %>%
    select(Year, AGI, Variable, targ, upper, lower) %>%
    return()
  
}

parse_soi = function(path) {
  
  read_csv(path) %>%
    filter(!grepl('taxable return', Variable)) %>%
    mutate(targ_raw = as.numeric(gsub(",","", `#_of_returns`)),
           AGI_Group2 = case_when(
             AGI_Group == "No adjusted gross income"                            ~ "-Inf",
             AGI_Group %in% c("$1 under $5,000", "$5,000 under $10,000")        ~ "0",
             AGI_Group %in% c("$10,000 under $15,000", "$15,000 under $20,000") ~ "10000",
             AGI_Group %in% c("$20,000 under $25,000", "$25,000 under $30,000") ~ "20000",
             AGI_Group == "$30,000 under $40,000"                               ~ "30000",
             AGI_Group == "$40,000 under $50,000"                               ~ "40000",
             AGI_Group == "$50,000 under $75,000"                               ~ "50000", 
             AGI_Group == "$75,000 under $100,000"                              ~ "75000", 
             AGI_Group == "$100,000 under $200,000"                             ~ "100000",
             AGI_Group == "$200,000 under $500,000"                             ~ "200000",
             AGI_Group == "$500,000 under $1,000,000"                           ~ "500000", 
             AGI_Group == "$1,000,000 under $1,500,000"                         ~ "1000000",
             AGI_Group == "$1,500,000 under $2,000,000"                         ~ "1500000", 
             AGI_Group == "$2,000,000 under $5,000,000"                         ~ "2000000",
             AGI_Group == "$5,000,000 under $10,000,000"                        ~ "5000000", 
             AGI_Group == "$10,000,000 or more"                                 ~ "10000000",
             TRUE                                                               ~ NA
           )) %>%
    group_by(Year, Variable, AGI_Group2) %>%
    summarise(targ = sum(targ_raw)) %>%
    select(Year, AGI_Group2, Variable, targ) %>%
    return()
}

parse_targ = function(path) {
  
  raw_targ = read_yaml(path)
  
  alphas = data.frame()
  
  for (iter in names(raw_targ)) {
    alphas = bind_rows(alphas,
                       data.frame(unlist(raw_targ[[iter]])) %>%
                         mutate(AGI_Group2 = rownames(.),
                                Variable = iter) %>%
                         rename(alpha = colnames(.)[1])
    )
    
  }
  
  rownames(alphas) = 1:nrow(alphas)
  
  return(select(alphas, Variable, AGI_Group2, alpha))
}

calibrate_bounds = function(targs, alphas) {
  
  targs %>%
    left_join(alphas) %>%
    mutate(upper = targ * (1 + alpha),
           lower = targ * (1 - alpha)) %>%
    return()
  
}



############### IN PROGRESS #####################



reweight_lp = function(tax_units, alpha, epsilon) {
  soi_14 = read_csv(PATH)
  soi_21 = read_csv(PATH)
  soi_23 = read_csv(PATH)
  soi_33 = read_csv(PATH)
  
  years = select(soi_14, Year) %>% unique()
  
  soi_14 %<>% pivot_wider(names_from = Variable, values_from = )

  tax_units %<>%
    rename(wt = S006) %>%
    mutate(AGI_Group = case_when(
      # Group AGI to match SOI categories
      AGIR1 == 0          ~ "No adjusted gross income",
      AGIR1 %in% c(1:5)   ~ "$1 under $5,000",
      AGIR1 %in% c(6:10)  ~ "$5,000 under $10,000",
      AGIR1 %in% c(11:15) ~ "$10,000 under $15,000",
      AGIR1 %in% c(16:20) ~ "$15,000 under $20,000",
      AGIR1 == 21         ~ "$20,000 under $25,000",
      AGIR1 == 22         ~ "$25,000 under $30,000",
      AGIR1 == 23         ~ "$30,000 under $40,000", 
      AGIR1 == 24         ~ "$40,000 under $50,000",
      AGIR1 == 25         ~ "$50,000 under $75,000", 
      AGIR1 == 26         ~ "$75,000 under $100,000", 
      AGIR1 == 27         ~ "$100,000 under $200,000",
      AGIR1 == 28         ~ "$200,000 under $500,000",
      AGIR1 == 29         ~ "$500,000 under $1,000,000", 
      AGIR1 == 30         ~ "$1,000,000 under $1,500,000",
      AGIR1 == 31         ~ "$1,500,000 under $2,000,000", 
      AGIR1 == 32         ~ "$2,000,000 under $5,000,000",
      AGIR1 == 33         ~ "$5,000,000 under $10,000,000", 
      AGIR1 == 34         ~ "$10,000,000 or more",
      TRUE                ~ NA
      ), 
      # Make binary dummies for variables we want to target
      wage_bin = E00200>0,
      
      ) %>%
    filter(!is.na(AGI_Group)) %>%
    mutate(val=1) %>%
    spread(AGI_Group, val, 0)
  
  tax_units_grouped = tax_units  %>%
    select(
      "No adjusted gross income",
      "$1 under $5,000",
      "$5,000 under $10,000",
      "$10,000 under $15,000",
      "$15,000 under $20,000",
      "$20,000 under $25,000",
      "$25,000 under $30,000",
      "$30,000 under $40,000",
      "$40,000 under $50,000",
      "$50,000 under $75,000",
      "$75,000 under $100,000",
      "$100,000 under $200,000",
      "$200,000 under $500,000",
      "$500,000 under $1,000,000",
      "$1,000,000 under $1,500,000",
      "$1,500,000 under $2,000,000",
      "$2,000,000 under $5,000,000",
      "$5,000,000 under $10,000,000",
      "$10,000,000 or more"
    )
  
  
}

build_lhs = function(tax_units) {
  tax_units %<>%
    rename(wt = S006) %>%
    mutate(AGI_Group = case_when(
      # Group AGI to match SOI categories
      AGIR1 == 0          ~ "No adjusted gross income",
      AGIR1 %in% c(1:5)   ~ "$1 under $5,000",
      AGIR1 %in% c(6:10)  ~ "$5,000 under $10,000",
      AGIR1 %in% c(11:15) ~ "$10,000 under $15,000",
      AGIR1 %in% c(16:20) ~ "$15,000 under $20,000",
      AGIR1 == 21         ~ "$20,000 under $25,000",
      AGIR1 == 22         ~ "$25,000 under $30,000",
      AGIR1 == 23         ~ "$30,000 under $40,000", 
      AGIR1 == 24         ~ "$40,000 under $50,000",
      AGIR1 == 25         ~ "$50,000 under $75,000", 
      AGIR1 == 26         ~ "$75,000 under $100,000", 
      AGIR1 == 27         ~ "$100,000 under $200,000",
      AGIR1 == 28         ~ "$200,000 under $500,000",
      AGIR1 == 29         ~ "$500,000 under $1,000,000", 
      AGIR1 == 30         ~ "$1,000,000 under $1,500,000",
      AGIR1 == 31         ~ "$1,500,000 under $2,000,000", 
      AGIR1 == 32         ~ "$2,000,000 under $5,000,000",
      AGIR1 == 33         ~ "$5,000,000 under $10,000,000", 
      AGIR1 == 34         ~ "$10,000,000 or more",
      TRUE                ~ NA
    ), 
    # Make binary dummies for variables we want to target
    wage_bin = E00200>0,
    
    ) %>%
    filter(!is.na(AGI_Group)) %>%
    mutate(val=1) %>%
    spread(AGI_Group, val, 0) %>%
    
}





run_lp = function(puf_sel, targs, year, epsilon) {
  
  
  lprw = make.lp(0, nrow(puf_sel))
  
  # Objective Function
  set.objfn(lprw, rep(1, nrow(puf_sel)))
  
  # Set up lhs
  
  # Set up directions
  
  # Set up targets
  rhs = targs %>%
    filter(Year = year) %>%
    select(-Year)
  
  for(i in vars) {
    add.constraint(lprw, 
                   select(lhs, one_of(paste0(i, "_flag"))), 
                   "<=", 
                   select(filter(rhs, ), upper)
                   )
  }
  
  set.bounds(lprw, lower = rep(1-epsilon, nrow(FLAG)), upper = rep(1 + epsilon, nrow(FLAG)))
  
  solve(lprw)
  
  return(get.variables(lprw))
}




