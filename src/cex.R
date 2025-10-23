
build_cex_training = function() {
  set.seed(54321)
  
  cex_path = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/CEX/2023')
  years = c(2023)
  
  # Read in individual members of consumption units to create their constituents tax units
  memi = years %>%
    map(.f = ~ lapply(dir(path       = cex_path,
                          pattern    = paste0('^memi((', .x%%100, ')[1-4]|(', .x%%100+1, '1))[.]csv$'),
                          full.names = TRUE),
                      fread) %>%
          bind_rows()
    ) %>%
    bind_rows()
  
  
  cex_tus = memi %>%
    select(NEWID, AGE, CU_CODE, SEX, IN_COLL, IN_COLL_, MARITAL, MEMBNO, TAX_UNIT, TU_CODE, TU_CODE_, TU_DPNDT, INCNONWK,
           EARNER, SALARYX, SEMPFRMX, SOCRRX,
           SALARYXM, SEMPFRMM, SOCRRXM
    ) %>%
    group_by(NEWID) %>%
    mutate(
      SEX = SEX %% 2,
      # Set NAs for income to 0 for future merging
      across(all_of(c("SALARYX", "SEMPFRMX", "SOCRRX")), ~ if_else(is.na(.x), 0, .x)),
      across(all_of(c("SALARYXM", "SEMPFRMM", "SOCRRXM")), ~ if_else(is.na(.x), 0, .x))
    ) %>%
    ungroup() %>%
    group_by(NEWID, TAX_UNIT) %>%
    mutate(
      # Flag if tax unit doesn't have a payer or a spouse
      no_payer = !any(TU_CODE == 1),
      no_spouse = !any(TU_CODE == 2) & any(CU_CODE == 2),
    ) %>%
    ungroup() %>%
    mutate(
      # Self reported yearly income
      inc = SALARYX + SEMPFRMX + SOCRRX,
      # Mean of 5 income imputations
      incm = SALARYXM + SEMPFRMM + SOCRRXM,
      
      # If there is no taxpayer, whomever is listed as the first member of the
      # consumption unit is the "payer"
      TU_CODE = if_else(
        no_payer,
        if_else(
          CU_CODE == 1,
          1,
          TU_CODE
        ),
        TU_CODE
      ),
      # If there is no spouse, whomever is listed as the spouse of the payer in the
      # consumption unit is the spouse
      TU_CODE = if_else(
        no_spouse,
        if_else(
          CU_CODE == 2,
          2,
          TU_CODE
        ),
        TU_CODE
      ),
      
      # Unique tax unit id codes
      TU_ID = as.numeric(paste0(NEWID, "00", TAX_UNIT)),
      # The tax unit of which you are a dependent
      TU_DPNDT = case_when(
        is.na(TU_DPNDT) ~ as.numeric(paste0(NEWID, "00", TU_CODE)),
        TU_DPNDT == ""  ~ as.numeric(paste0(NEWID, "00", TU_CODE)),
        TU_DPNDT == "B" ~ as.numeric(paste0(NEWID, "00", TU_CODE)),
        T               ~ as.numeric(paste0(NEWID, "00", TU_DPNDT))
      ),
      
      # Are a dependent if you aren't the taxpayer or their spouse
      is_dep = as.numeric(TU_CODE == 3),
      # Are CTC aged if under 17
      is_ctc = as.numeric(AGE < 17),
      
      # Count dependents who need to file taxes as part of their caretaker unit
      TU_ID = if_else(is_dep == 1 & as.character(TU_ID) != TU_DPNDT, TU_DPNDT, TU_ID)
      
    ) %>%
    group_by(NEWID, is_dep) %>%
    arrange(MEMBNO, .by_group = T) %>%
    mutate(
      # Count the number of dependents and give them numbers
      dep_no = if_else(is_dep==1, row_number(), NA_integer_)
    ) %>%
    ungroup() %>%
    mutate(
      # Tax unit members
      # 1 is payer
      # 2 is spouse
      # dep.[x] is dependent number
      TU_NO = if_else(is_dep == 1, paste0("dep", dep_no), as.character(TU_CODE))
    ) %>%
    ungroup() %>%
    select(NEWID, TU_ID, is_dep, TU_CODE, MEMBNO, AGE, SEX, MARITAL, is_dep, is_ctc, TU_NO, TU_DPNDT, inc, incm) %>%
    pivot_longer(!c(NEWID, TU_ID, TU_NO), names_to = 'vars', values_to = 'values')  %>%
    pivot_wider(
      names_from = c(vars, TU_NO),
      names_sep = ".",
      values_from = values
    ) %>%
    relocate(ends_with(".2"), .after = TU_ID) %>%
    relocate(ends_with(".1"), .after = TU_ID) %>%
    rename(
      # Rebuild to PUF format
      # .1 is payer
      # .2 is spouse
      age1 = AGE.1,
      male1 = SEX.1,
      age2 = AGE.2,
      male2 = SEX.2,
      
      dep_age1 = AGE.dep1,
      dep_age2 = AGE.dep2,
      dep_age3 = AGE.dep3,
      
    )%>%
    group_by(TU_ID) %>%
    mutate(
      married = !is.na(age2),
      n_dep = sum(c_across(all_of(contains("is_dep"))), na.rm = T),
      n_dep_ctc = sum(c_across(all_of(contains("is_ctc"))), na.rm = T),
    ) %>%
    mutate(
      across(all_of(contains("TU_DPNDT")), ~ .x == TU_ID),
      dep_ctc1 = if_else(!is.na(dep_age1), dep_age1 < 17, NA),
      dep_ctc2 = if_else(!is.na(dep_age2), dep_age2 < 17, NA),
      dep_ctc3 = if_else(!is.na(dep_age3), dep_age3 < 17, NA),
      filer = !is.na(age1)
    ) %>%
    mutate(
      filing_status = case_when(
        !is.na(age2) ~ 2,
        !is.na(age1) & is.na(age2) & !is.na(n_dep) & n_dep > 0 ~ 4,
        !is.na(age1) & is.na(age2) & !is.na(n_dep) & n_dep == 0 ~ 1,
        T ~ -1
      ),
      tu_size = as.numeric(!is.na(age1)) + as.numeric(!is.na(age2)) + n_dep
    ) %>%
    group_by(TU_ID) %>%
    mutate(income = sum(c_across(all_of(contains('inc.'))), na.rm = T),
           income2 = sum(c_across(all_of(contains('incm.'))), na.rm = T)) %>%
    ungroup() %>%
    # Restrict to the variables we end up using in QRF and vars needed for
    # merging
    select(NEWID, TU_ID, tu_size, filer, filing_status, married, age1, male1,
           income, income2, n_dep_ctc)
  
  # current quarter expenditures
  expcq = c('ETOTALC', 'TOTEXPCQ', 'FOODCQ', 'ALCBEVCQ', 'HOUSCQ', 'APPARCQ',
            'TRANSCQ', 'HEALTHCQ', 'ENTERTCQ', 'PERSCACQ', 'READCQ', 'EDUCACQ',
            'TOBACCCQ', 'EMISCELC', 'CASHCOCQ', 'LIFINSCQ', 'MISCCQ', 'RETPENCQ')
  # prior quarter expenditures
  exppq = c('ETOTALP', 'TOTEXPPQ', 'FOODPQ', 'ALCBEVPQ', 'HOUSPQ', 'APPARPQ',
            'TRANSPQ', 'HEALTHPQ', 'ENTERTPQ', 'PERSCAPQ', 'READPQ', 'EDUCAPQ',
            'TOBACCPQ', 'EMISCELP', 'CASHCOPQ', 'LIFINSPQ', 'MISCPQ', 'RETPENPQ')
  # Names for merged expenditure
  exp   = c('FOOD_exp', 'ALCBEV_exp', 'HOUS_exp', 'APPAR_exp', 'TRANS_exp',
            'HEALTH_exp', 'ENTERT_exp', 'PERSCA_exp', 'READ_exp', 'EDUCA_exp',
            'TOBACC_exp', 'EMISCEL_exp', 'CASHCO_exp', 'LIFINS_exp', "MISC_exp", "RETPEN_exp")
  
  # technical variables
  tech = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', "FAM_TYPE")
  # demographic variables
  demo = c('FAM_SIZE')
  
  fmli = years %>%
    map(.f = ~ lapply(dir(path       = cex_path,
                          pattern    = paste0('^fmli((', .x%%100, ')[1-4]|(', .x%%100+1, '1))[.]csv$'),
                          full.names = TRUE),
                      fread,
                      select = c(tech, demo, 
                                 expcq, exppq)) %>%
          bind_rows() %>%
          
          rename_with(.fn   = ~gsub('(C|CQ)$', '_CQ', .x),
                      .cols = all_of(expcq)) %>%
          rename_with(.fn   = ~gsub('(P|PQ)$', '_PQ', .x),
                      .cols = all_of(exppq)) 
    ) %>% bind_rows()
  
  
  joined = cex_tus %>% left_join(fmli, by="NEWID") %>%
    group_by(NEWID) %>%
    mutate(
      # Calculate what percent of the members of the Consumption Unit are in 
      # this tax unit
      CU_pct = tu_size / FAM_SIZE,
      # Flags for irregular income
      has_income = case_when(
        income2  > 0 ~ 1,
        income2 == 0 ~ 0,
        T            ~ -1
      )
    )  %>%
    # Want valid income
    filter(!is.na(income2)) %>%
    # Adults
    filter(!is.na(age1) & age1 > 17) 
  
  irregular_income = joined %>% filter(has_income < 1)  %>%
    mutate(
      # Flag for irregular income
      pctile_income = -1,
      across(.cols  = all_of(gsub('(C|CQ)$', '_CQ', expcq)),
             .fns   = ~ (.x + get(gsub('CQ$', 'PQ', cur_column()))),
             .names = '{gsub("CQ$", "exp", .col)}'),
      
      # Scale consumption by share of consumption unit in this tax unit
      goods    = (FOOD_exp + ALCBEV_exp + APPAR_exp + TOBACC_exp) * CU_pct,
      services = (HOUS_exp + HEALTH_exp + TRANS_exp + ENTERT_exp + PERSCA_exp + READ_exp + EDUCA_exp + LIFINS_exp) * CU_pct,  
      expenses = goods + services,
      
      # Can't do percentage of zero or negative income (irregular income is not
      # used to impute consumption as a percent of income)
      goods_per    = 0,
      services_per = 0,
      expenses_per = 0,
      
      # Cap age at PUF max
      age1      = pmin(age1, 80),
      # Cap at number of ctc elligible children
      n_dep_ctc = pmin(n_dep_ctc, 3) 
    ) %>%
    as_tibble() %>% select(
      NEWID, QINTRVYR, FINLWT21, pctile_income, married, age1, n_dep_ctc, male1, income2, has_income,
      goods, services, expenses,
      goods_per, services_per, expenses_per
    )
  
  joined %<>% filter(has_income == 1)
  
  pct_breaks = c(-Inf,
                 wtd.quantile(x       = joined$income2,
                              probs   = seq(0.01, 0.99, 0.01),
                              weights = joined$FINLWT21),
                 Inf)
  
  joined = joined %>%
    mutate(
      pctile_income = cut(x              = income2,
                          breaks         = pct_breaks,
                          labels         = seq(1, 100, 1),
                          include.lowest = TRUE),
      pctile_income = as.numeric(levels(pctile_income))[pctile_income],
      
      across(.cols  = all_of(gsub('(C|CQ)$', '_CQ', expcq)),
             .fns   = ~ (.x + get(gsub('CQ$', 'PQ', cur_column()))),
             .names = '{gsub("CQ$", "exp", .col)}'),
      
      # Scale consumption by share of consumption unit in this tax unit
      goods    = (FOOD_exp + ALCBEV_exp + APPAR_exp + TOBACC_exp) * CU_pct,
      services = (HOUS_exp + HEALTH_exp + TRANS_exp + ENTERT_exp + PERSCA_exp + READ_exp + EDUCA_exp + LIFINS_exp) * CU_pct,
      expenses = goods + services,
      
      # Forms of consumption as a percent of income
      goods_per    = goods / income2,
      services_per = services / income2,
      expenses_per = expenses / income2,
      
      # Cap age at PUF max
      age1      = pmin(age1, 80),
      # Cap at number of ctc elligible children
      n_dep_ctc = pmin(n_dep_ctc, 3) 
    ) %>%
    as_tibble() %>% select(
      NEWID, QINTRVYR, FINLWT21, pctile_income, married, age1, n_dep_ctc, male1, income2, has_income,
      goods, services, expenses,
      goods_per, services_per, expenses_per
    ) %>%
    bind_rows(irregular_income)
  
  # Select a random sample using weights to sample
  joined %>%
    slice_sample(n = 100000, replace = T, weight_by = FINLWT21) %>%
    return()
}
