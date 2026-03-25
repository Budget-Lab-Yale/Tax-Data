
build_cex_training = function() {
  set.seed(54321)

  cex_path = file.path('/gpfs/gibbs/project/sarin/shared/raw_data/CEX/2023')
  years = c(2023)

  # Read in individual members of consumption units to create their constituent tax units
  memi = years %>%
    map(.f = ~ lapply(dir(path       = cex_path,
                          pattern    = paste0('^memi((', .x%%100, ')[1-4]|(', .x%%100+1, '1))[.]csv$'),
                          full.names = TRUE),
                      fread) %>%
          bind_rows()
    ) %>%
    bind_rows()


  cex_tus = memi %>%
    select(NEWID, AGE, CU_CODE, SEX, MARITAL, MEMBNO, TAX_UNIT, TU_CODE, TU_CODE_, TU_DPNDT,
           SALARYXM, SEMPFRMM, SOCRRXM, INTEARNM, PENSIONM, DIVIDM
    ) %>%
    group_by(NEWID) %>%
    mutate(
      SEX = SEX %% 2,
      across(all_of(c("SALARYXM", "SEMPFRMM", "SOCRRXM", "INTEARNM", "PENSIONM", "DIVIDM")),
             ~ if_else(is.na(.x), 0, .x))
    ) %>%
    ungroup() %>%
    # Flag and fix missing payer/spouse within each tax unit
    group_by(NEWID, TAX_UNIT) %>%
    mutate(
      no_payer  = !any(TU_CODE == 1),
      no_spouse = !any(TU_CODE == 2) & any(CU_CODE == 2),
      TU_CODE = if_else(no_payer  & CU_CODE == 1, 1, TU_CODE),
      TU_CODE = if_else(no_spouse & CU_CODE == 2, 2, TU_CODE),
    ) %>%
    ungroup() %>%
    mutate(
      # XM-based income (BLS multiply-imputed; assumed actual if reported, imputed if missing — verify against BLS CEX documentation)
      inc = SALARYXM + SEMPFRMM + SOCRRXM + INTEARNM + PENSIONM + DIVIDM,

      # Tax unit ID from NEWID + TAX_UNIT (not TU_CODE)
      TU_ID = as.numeric(paste0(NEWID, "00", TAX_UNIT)),

      # Valid TU_DPNDT means this person is claimed on another TU's return
      has_valid_dpndt = !is.na(TU_DPNDT) & TU_DPNDT != "" & TU_DPNDT != "B",

      # Claiming TU's ID (only for people with valid TU_DPNDT)
      claiming_TU_ID = if_else(
        has_valid_dpndt,
        as.numeric(paste0(NEWID, "00", TU_DPNDT)),
        NA_real_
      ),

      # A person is a dependent if TU_CODE == 3 OR they have a valid TU_DPNDT
      # (catches dependent filers: TU_CODE == 1 on own return but claimed elsewhere)
      is_dep = as.numeric(TU_CODE == 3 | has_valid_dpndt),
      is_ctc = as.numeric(AGE < 17),

      # Reassign dependents to claiming TU when TU_DPNDT points elsewhere
      TU_ID = if_else(
        !is.na(claiming_TU_ID) & claiming_TU_ID != TU_ID,
        claiming_TU_ID,
        TU_ID
      )
    ) %>%
    # Number dependents within each tax unit (not CU-wide)
    group_by(NEWID, TU_ID, is_dep) %>%
    arrange(MEMBNO, .by_group = T) %>%
    mutate(
      dep_no = if_else(is_dep == 1, row_number(), NA_integer_)
    ) %>%
    ungroup() %>%
    mutate(
      # Tax unit member role labels
      # 1 = payer, 2 = spouse, dep.[x] = dependent number
      TU_NO = if_else(is_dep == 1, paste0("dep", dep_no), as.character(TU_CODE))
    ) %>%
    select(NEWID, TU_ID, AGE, SEX, is_dep, is_ctc, TU_NO, inc) %>%
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

    ) %>%
    group_by(TU_ID) %>%
    mutate(
      married   = !is.na(age2),
      n_dep     = sum(c_across(all_of(contains("is_dep"))), na.rm = T),
      n_dep_ctc = sum(c_across(all_of(contains("is_ctc"))), na.rm = T),
      filer     = !is.na(age1),
      tu_size   = as.numeric(!is.na(age1)) + as.numeric(!is.na(age2)) + n_dep,
      income    = sum(c_across(all_of(contains('inc.'))), na.rm = T)
    ) %>%
    ungroup() %>%
    select(NEWID, TU_ID, tu_size, filer, married, age1, male1, income, n_dep, n_dep_ctc)

  # Current quarter expenditures only (PQ dropped to avoid double-counting)
  expcq = c('ETOTALC', 'TOTEXPCQ', 'FOODCQ', 'ALCBEVCQ', 'HOUSCQ', 'APPARCQ',
            'TRANSCQ', 'HEALTHCQ', 'ENTERTCQ', 'PERSCACQ', 'READCQ', 'EDUCACQ',
            'TOBACCCQ', 'EMISCELC', 'MISCCQ')
  # Names for expenditure variables
  exp   = c('FOOD_exp', 'ALCBEV_exp', 'HOUS_exp', 'APPAR_exp', 'TRANS_exp',
            'HEALTH_exp', 'ENTERT_exp', 'PERSCA_exp', 'READ_exp', 'EDUCA_exp',
            'TOBACC_exp', 'EMISCEL_exp', 'MISC_exp')

  # Technical variables
  tech = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_TYPE', 'COMP_INC')
  # Demographic variables
  demo = c('FAM_SIZE')

  fmli = years %>%
    map(.f = ~ lapply(dir(path       = cex_path,
                          pattern    = paste0('^fmli((', .x%%100, ')[1-4]|(', .x%%100+1, '1))[.]csv$'),
                          full.names = TRUE),
                      fread,
                      select = c(tech, demo, expcq)) %>%
          bind_rows() %>%
          rename_with(.fn   = ~gsub('(C|CQ)$', '_CQ', .x),
                      .cols = all_of(expcq))
    ) %>% bind_rows()


  joined = cex_tus %>% left_join(fmli, by = "NEWID") %>%
    # Filter to complete income reporters
    filter(COMP_INC == 1) %>%
    group_by(NEWID) %>%
    mutate(
      # Calculate what percent of the CU members are in this tax unit
      CU_pct = tu_size / FAM_SIZE,
      # Flags for irregular income
      has_income = case_when(
        income  > 0 ~ 1,
        income == 0 ~ 0,
        T           ~ -1
      )
    )  %>%
    # Want valid income
    filter(!is.na(income)) %>%
    # Adults
    filter(!is.na(age1) & age1 > 17)

  irregular_income = joined %>% filter(has_income < 1)  %>%
    mutate(
      pctile_income = -1,
      # CQ only — rename to _exp
      across(.cols  = all_of(gsub('(C|CQ)$', '_CQ', expcq)),
             .fns   = ~ .x,
             .names = '{gsub("CQ$", "exp", .col)}'),

      # Scale consumption by share of consumption unit in this tax unit
      goods    = (FOOD_exp + ALCBEV_exp + APPAR_exp + TOBACC_exp) * CU_pct,
      services = (HOUS_exp + HEALTH_exp + TRANS_exp + ENTERT_exp + PERSCA_exp + READ_exp + EDUCA_exp) * CU_pct,
      expenses = goods + services,

      # Can't do percentage of zero or negative income
      goods_per    = 0,
      services_per = 0,
      expenses_per = 0,

      # Cap age at PUF max
      age1      = pmin(age1, 80),
      n_dep_ctc = pmin(n_dep_ctc, 3)
    ) %>%
    as_tibble() %>% select(
      NEWID, QINTRVYR, FINLWT21, pctile_income, married, age1, n_dep, n_dep_ctc, male1, income, has_income,
      goods, services, expenses,
      goods_per, services_per, expenses_per
    )

  joined %<>% filter(has_income == 1)

  pct_breaks = c(-Inf,
                 wtd.quantile(x       = joined$income,
                              probs   = seq(0.01, 0.99, 0.01),
                              weights = joined$FINLWT21),
                 Inf)

  joined = joined %>%
    mutate(
      pctile_income = cut(x              = income,
                          breaks         = pct_breaks,
                          labels         = seq(1, 100, 1),
                          include.lowest = TRUE),
      pctile_income = as.numeric(levels(pctile_income))[pctile_income],

      # CQ only — rename to _exp
      across(.cols  = all_of(gsub('(C|CQ)$', '_CQ', expcq)),
             .fns   = ~ .x,
             .names = '{gsub("CQ$", "exp", .col)}'),

      # Scale consumption by share of consumption unit in this tax unit
      goods    = (FOOD_exp + ALCBEV_exp + APPAR_exp + TOBACC_exp) * CU_pct,
      services = (HOUS_exp + HEALTH_exp + TRANS_exp + ENTERT_exp + PERSCA_exp + READ_exp + EDUCA_exp) * CU_pct,
      expenses = goods + services,

      # Forms of consumption as a percent of income
      goods_per    = goods / income,
      services_per = services / income,
      expenses_per = expenses / income,

      # Cap age at PUF max
      age1      = pmin(age1, 80),
      n_dep_ctc = pmin(n_dep_ctc, 3)
    ) %>%
    as_tibble() %>% select(
      NEWID, QINTRVYR, FINLWT21, pctile_income, married, age1, n_dep, n_dep_ctc, male1, income, has_income,
      goods, services, expenses,
      goods_per, services_per, expenses_per
    ) %>%
    bind_rows(irregular_income)

  # Select a random sample using weights to sample
  joined %>%
    slice_sample(n = 100000, replace = T, weight_by = FINLWT21) %>%
    return()
}
