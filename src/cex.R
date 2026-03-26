
# Read CEX files for a calendar year from BLS survey-year directories.
# Each BLS annual release contains Q2-Q4 of year N plus Q1 of year N+1:
#   CEX/2022/: *222, *223, *224, *231
#   CEX/2023/: *232, *233, *234, *241
# A calendar-year read searches two directories:
#   CEX/{year-1}/ for Q1 (^{prefix}{yy}1\.csv$)
#   CEX/{year}/   for Q2-Q4 + boundary Q1 of next year
read_cex = function(base_path, prefix, year, ...) {
  yy = year %% 100
  # Q1 of target year lives in the prior survey-year release
  files_q1 = dir(path       = file.path(base_path, year - 1),
                 pattern    = paste0('^', prefix, yy, '1\\.csv$'),
                 full.names = TRUE)
  # Q2-Q4 of target year
  files_q234 = dir(path       = file.path(base_path, year),
                   pattern    = paste0('^', prefix, yy, '[2-4]\\.csv$'),
                   full.names = TRUE)
  # Boundary Q1 of next year (in same release as target year Q2-Q4)
  files_boundary = dir(path       = file.path(base_path, year),
                       pattern    = paste0('^', prefix, yy + 1, '1\\.csv$'),
                       full.names = TRUE)
  all_files = c(files_q1, files_q234, files_boundary)
  if (length(all_files) == 0) return(data.table::data.table())
  lapply(all_files, fread, ...) %>% bind_rows()
}


build_cex_training = function() {
  set.seed(54321)

  cex_base = '/gpfs/gibbs/project/sarin/shared/raw_data/CEX'
  years = c(2022, 2023, 2024)

  # CPI-U (IRS year avg) deflators to 2017 dollars: cpiu_irs[year] / cpiu_irs[2017]
  cpi_deflator = c('2022' = 1.17443, '2023' = 1.23825, '2024' = 1.27760)

  income_vars = c("SALARYXM", "SEMPFRMM", "SOCRRXM", "SSIXM", "ANGOVRTM", "ANPRVPNM")

  # Read in individual members of consumption units to create their constituent tax units
  memi = years %>%
    map(.f = ~ read_cex(cex_base, 'memi', .x) %>% mutate(SURVEY_YEAR = .x)) %>%
    bind_rows() %>%
    distinct(NEWID, MEMBNO, .keep_all = TRUE) %>%
    # CPI-deflate income variables to 2017 dollars
    mutate(across(all_of(income_vars), ~ .x / cpi_deflator[as.character(SURVEY_YEAR)]))


  cex_tus = memi %>%
    select(NEWID, SURVEY_YEAR, AGE, CU_CODE, SEX, MARITAL, MEMBNO, TAX_UNIT, TU_CODE, TU_CODE_, TU_DPNDT,
           SALARYXM, SEMPFRMM, SOCRRXM, SSIXM, ANGOVRTM, ANPRVPNM
    ) %>%
    group_by(NEWID) %>%
    mutate(
      SEX = SEX %% 2,
      across(all_of(c("SALARYXM", "SEMPFRMM", "SOCRRXM", "SSIXM", "ANGOVRTM", "ANPRVPNM")),
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
      # Member-level income (BLS multiply-imputed): wages, self-emp, SS, SSI, pensions
      # Capital income (interest+dividends, rent) is CU-level and allocated after FMLI join
      inc = SALARYXM + SEMPFRMM + SOCRRXM + SSIXM + ANGOVRTM + ANPRVPNM,

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
    select(NEWID, SURVEY_YEAR, TU_ID, AGE, SEX, is_dep, is_ctc, TU_NO, inc) %>%
    pivot_longer(!c(NEWID, SURVEY_YEAR, TU_ID, TU_NO), names_to = 'vars', values_to = 'values')  %>%
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
    select(NEWID, SURVEY_YEAR, TU_ID, tu_size, filer, married, age1, male1, income, n_dep, n_dep_ctc)

  #---------------------------------------------------------------------------
  # FMLI: weights, demographics, tech variables, and CQ expenditure sub-variables
  #---------------------------------------------------------------------------

  tech    = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_TYPE')
  demo    = c('FAM_SIZE')
  cap_inc = c('INTRDVXM', 'NETRENTM')  # CU-level capital income (allocated to TUs below)

  # FMLI CQ expenditure sub-variables (integrate both Interview + Diary data)
  expcq = c('FDHOMECQ', 'FDAWAYCQ', 'ALCBEVCQ',
            'OWNDWECQ', 'RENDWECQ', 'OTHLODCQ',
            'NTLGASCQ', 'ELCTRCCQ', 'ALLFULCQ', 'TELEPHCQ', 'WATRPSCQ',
            'HOUSOPCQ', 'HOUSEQCQ', 'APPARCQ',
            'CARTKNCQ', 'CARTKUCQ', 'OTHVEHCQ', 'GASMOCQ',
            'VEHFINCQ', 'MAINRPCQ', 'VEHINSCQ', 'VRNTLOCQ', 'PUBTRACQ',
            'HEALTHCQ',
            'FEEADMCQ', 'TVRDIOCQ', 'OTHEQPCQ', 'PETTOYCQ', 'OTHENTCQ',
            'PERSCACQ', 'READCQ', 'EDUCACQ', 'TOBACCCQ', 'MISCCQ',
            'LIFINSCQ')

  fmli = years %>%
    map(.f = ~ read_cex(cex_base, 'fmli', .x, select = c(tech, demo, cap_inc, expcq)) %>% mutate(SURVEY_YEAR = .x)) %>%
    bind_rows() %>%
    distinct(NEWID, .keep_all = TRUE) %>%
    # CPI-deflate monetary variables to 2017 dollars
    mutate(
      across(all_of(c(cap_inc, expcq)), ~ replace_na(.x, 0)),
      across(all_of(c(cap_inc, expcq)), ~ .x / cpi_deflator[as.character(SURVEY_YEAR)])
    ) %>%
    # MO_SCOPE: how many of this interview's 3 CQ reference months fall in the
    # target calendar year. Uses SURVEY_YEAR (not a fixed year) for multi-year pooling.
    #   WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3) / length(years)
    # Division by length(years) prevents triple-counting the population.
    mutate(
      MO_SCOPE = case_when(
        QINTRVYR == SURVEY_YEAR     & QINTRVMO <= 1  ~ 0L,
        QINTRVYR == SURVEY_YEAR     & QINTRVMO == 2  ~ 1L,
        QINTRVYR == SURVEY_YEAR     & QINTRVMO == 3  ~ 2L,
        QINTRVYR == SURVEY_YEAR     & QINTRVMO >= 4  ~ 3L,
        QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 1  ~ 3L,
        QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 2  ~ 2L,
        QINTRVYR == SURVEY_YEAR + 1 & QINTRVMO == 3  ~ 1L,
        TRUE                                           ~ 0L
      ),
      WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3) / length(years)
    )

  #---------------------------------------------------------------------------
  # PCE category definitions
  #---------------------------------------------------------------------------

  # All 20 BEA PCE major categories
  # NPISH and net_foreign_travel have no CEX source — set to 0 here,
  # allocated proportionally to total consumption in pce_benchmark.R
  pce_cats     = c('clothing', 'motor_vehicles', 'other_durables', 'furnishings',
                   'rec_goods', 'other_nondurables', 'food_off_premises', 'communication',
                   'npish', 'other_services', 'transport_services', 'rec_services',
                   'net_foreign_travel', 'food_accommodations', 'health_care', 'utilities',
                   'gasoline', 'education', 'financial_insurance', 'housing')
  pce_cats_per = paste0(pce_cats, '_per')

  #---------------------------------------------------------------------------
  # Join tax units to FMLI (weights, demo, expenditures)
  #---------------------------------------------------------------------------

  joined = cex_tus %>%
    left_join(fmli, by = c("NEWID", "SURVEY_YEAR")) %>%
    group_by(NEWID) %>%
    mutate(
      # Allocate CU-level capital income (interest+dividends, rent) to TUs.
      # Pro-rata by member-level income; equal split when all TUs have zero member income.
      cu_member_inc = sum(pmax(income, 0)),
      n_tu = n(),
      cap_share = if_else(cu_member_inc > 0, pmax(income, 0) / cu_member_inc, 1 / n_tu),
      capital_income = (INTRDVXM + NETRENTM) * cap_share,
      income = income + capital_income,

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
    filter(!is.na(age1) & age1 > 17) %>%
    # Map FMLI CQ sub-variables to 20 PCE categories, scaled by CU_pct
    mutate(
      clothing            = APPARCQ * CU_pct,
      motor_vehicles      = (CARTKNCQ + CARTKUCQ + OTHVEHCQ) * CU_pct,
      other_durables      = PETTOYCQ * CU_pct,
      furnishings         = HOUSEQCQ * CU_pct,
      rec_goods           = (TVRDIOCQ + OTHEQPCQ) * CU_pct,
      other_nondurables   = (TOBACCCQ + PERSCACQ + READCQ) * CU_pct,
      food_off_premises   = (FDHOMECQ + ALCBEVCQ) * CU_pct,
      communication       = TELEPHCQ * CU_pct,
      other_services      = (HOUSOPCQ + MISCCQ + OTHENTCQ) * CU_pct,
      transport_services  = (MAINRPCQ + VRNTLOCQ + PUBTRACQ) * CU_pct,
      rec_services        = FEEADMCQ * CU_pct,
      food_accommodations = (FDAWAYCQ + OTHLODCQ) * CU_pct,
      health_care         = HEALTHCQ * CU_pct,
      utilities           = (NTLGASCQ + ELCTRCCQ + ALLFULCQ + WATRPSCQ) * CU_pct,
      gasoline            = GASMOCQ * CU_pct,
      education           = EDUCACQ * CU_pct,
      financial_insurance = (LIFINSCQ + VEHINSCQ + VEHFINCQ) * CU_pct,
      housing             = (OWNDWECQ + RENDWECQ) * CU_pct,
      npish               = 0,
      net_foreign_travel  = 0,
      cex_consumption     = rowSums(across(all_of(setdiff(pce_cats, c('npish', 'net_foreign_travel'))))),
      total_consumption   = cex_consumption,
      # Cap age at PUF max
      age1      = pmin(age1, 80),
      n_dep_ctc = pmin(n_dep_ctc, 3)
    )

  #---------------------------------------------------------------------------
  # Irregular income (zero or negative): direct consumption only
  #---------------------------------------------------------------------------

  irregular_income = joined %>% filter(has_income < 1) %>%
    mutate(
      pctile_income = -1,
      across(all_of(pce_cats), ~ 0, .names = '{.col}_per'),
      total_consumption_per = 0
    ) %>%
    as_tibble() %>% select(
      NEWID, SURVEY_YEAR, QINTRVYR, FINLWT21, WT_ANNUAL, pctile_income, married, age1, n_dep, n_dep_ctc,
      male1, income, has_income,
      all_of(pce_cats), total_consumption,
      all_of(pce_cats_per), total_consumption_per
    )

  #---------------------------------------------------------------------------
  # Regular income (positive): consumption levels and ratios
  #---------------------------------------------------------------------------

  joined %<>% filter(has_income == 1)

  pct_quantiles = wtd.quantile(x       = joined$income,
                               probs   = seq(0.01, 0.99, 0.01),
                               weights = joined$WT_ANNUAL)

  joined = joined %>%
    mutate(
      # findInterval handles tied quantile breaks (common with concentrated incomes)
      pctile_income = findInterval(income, pct_quantiles) + 1L,
      pctile_income = pmin(pctile_income, 100L)
    ) %>%
    # Consumption-to-income ratios (used for dual prediction approach)
    mutate(
      across(all_of(pce_cats), ~ .x / income, .names = '{.col}_per'),
      total_consumption_per = total_consumption / income
    ) %>%
    as_tibble() %>% select(
      NEWID, SURVEY_YEAR, QINTRVYR, FINLWT21, WT_ANNUAL, pctile_income, married, age1, n_dep, n_dep_ctc,
      male1, income, has_income,
      all_of(pce_cats), total_consumption,
      all_of(pce_cats_per), total_consumption_per
    ) %>%
    bind_rows(irregular_income)

  # NOTE: PCE benchmarking (scaling category totals to match BEA aggregates)
  # is deferred to project_puf.R, where it can be applied in projection-year dollars
  joined
}


#' Validate tax unit construction against NTAXI
#'
#' Optional diagnostic function — not called during normal pipeline execution.
#' Reads NTAXI (BLS tax-unit-level) files and compares dependent counts, filing
#' status, and TU counts per CU against our MEMI-based construction.
#'
#' @param cex_path Path to CEX data directory
#' @param years Target year(s)
#' @return List with match rates and crosstabs of disagreements
validate_tax_units_against_ntaxi = function(
    cex_base = '/gpfs/gibbs/project/sarin/shared/raw_data/CEX',
    years    = c(2023)) {

  # -------------------------------------------------------------------------
  # 1. Read NTAXI: one record per tax unit per interview quarter
  # -------------------------------------------------------------------------
  ntaxi = years %>%
    map(.f = ~ read_cex(cex_base, 'ntaxi', .x,
                        select = c('NEWID', 'TAX_UNIT', 'DEPCNT', 'FILESTAT'))) %>%
    bind_rows() %>%
    mutate(
      TU_ID_NTAXI = as.numeric(paste0(NEWID, "00", TAX_UNIT)),
      # BLS FILESTAT: 1=single, 2=MFJ, 3=MFS, 5=HoH, 6=QW
      ntaxi_married = as.numeric(FILESTAT == 2)
    )

  # -------------------------------------------------------------------------
  # 2. Read MEMI and build tax units (same logic as build_cex_training)
  # -------------------------------------------------------------------------
  memi = years %>%
    map(.f = ~ read_cex(cex_base, 'memi', .x)) %>%
    bind_rows()

  cex_tus = memi %>%
    select(NEWID, AGE, CU_CODE, SEX, MARITAL, MEMBNO, TAX_UNIT, TU_CODE, TU_CODE_, TU_DPNDT,
           SALARYXM, SEMPFRMM, SOCRRXM, INTEARNM, PENSIONM, DIVIDM
    ) %>%
    group_by(NEWID) %>%
    mutate(
      across(all_of(c("SALARYXM", "SEMPFRMM", "SOCRRXM", "INTEARNM", "PENSIONM", "DIVIDM")),
             ~ if_else(is.na(.x), 0, .x))
    ) %>%
    ungroup() %>%
    group_by(NEWID, TAX_UNIT) %>%
    mutate(
      no_payer  = !any(TU_CODE == 1),
      no_spouse = !any(TU_CODE == 2) & any(CU_CODE == 2),
      TU_CODE = if_else(no_payer  & CU_CODE == 1, 1, TU_CODE),
      TU_CODE = if_else(no_spouse & CU_CODE == 2, 2, TU_CODE),
    ) %>%
    ungroup() %>%
    mutate(
      TU_ID = as.numeric(paste0(NEWID, "00", TAX_UNIT)),
      has_valid_dpndt = !is.na(TU_DPNDT) & TU_DPNDT != "" & TU_DPNDT != "B",
      claiming_TU_ID = if_else(
        has_valid_dpndt,
        as.numeric(paste0(NEWID, "00", TU_DPNDT)),
        NA_real_
      ),
      is_dep = as.numeric(TU_CODE == 3 | has_valid_dpndt),
      TU_ID = if_else(
        !is.na(claiming_TU_ID) & claiming_TU_ID != TU_ID,
        claiming_TU_ID,
        TU_ID
      )
    ) %>%
    group_by(NEWID, TU_ID) %>%
    summarise(
      our_n_dep  = sum(is_dep),
      our_married = as.numeric(any(TU_CODE == 2)),
      our_size   = n(),
      .groups = 'drop'
    )

  # -------------------------------------------------------------------------
  # 3. Compare: join on NEWID + TU_ID
  # -------------------------------------------------------------------------
  comparison = cex_tus %>%
    inner_join(ntaxi, by = c('NEWID', 'TU_ID' = 'TU_ID_NTAXI'))

  n_matched = nrow(comparison)

  dep_match_rate     = mean(comparison$our_n_dep == comparison$DEPCNT, na.rm = TRUE)
  married_match_rate = mean(comparison$our_married == comparison$ntaxi_married, na.rm = TRUE)

  # TU count per CU
  our_tu_counts   = cex_tus %>% count(NEWID, name = 'our_n_tu')
  ntaxi_tu_counts = ntaxi %>% count(NEWID, name = 'ntaxi_n_tu')
  tu_count_compare = our_tu_counts %>%
    inner_join(ntaxi_tu_counts, by = 'NEWID')
  tu_count_match_rate = mean(tu_count_compare$our_n_tu == tu_count_compare$ntaxi_n_tu, na.rm = TRUE)

  # Crosstabs of disagreements
  dep_disagree = comparison %>%
    filter(our_n_dep != DEPCNT) %>%
    count(our_n_dep, DEPCNT, name = 'n_disagreements') %>%
    arrange(desc(n_disagreements))

  married_disagree = comparison %>%
    filter(our_married != ntaxi_married) %>%
    count(our_married, FILESTAT, name = 'n_disagreements') %>%
    arrange(desc(n_disagreements))

  # -------------------------------------------------------------------------
  # 4. Report
  # -------------------------------------------------------------------------
  cat("NTAXI Validation Report\n")
  cat("=======================\n")
  cat(sprintf("Tax units matched:          %d\n", n_matched))
  cat(sprintf("Dependent count agreement:  %.1f%%\n", dep_match_rate * 100))
  cat(sprintf("Married flag agreement:     %.1f%%\n", married_match_rate * 100))
  cat(sprintf("TU count per CU agreement:  %.1f%%\n", tu_count_match_rate * 100))

  if (nrow(dep_disagree) > 0) {
    cat("\nDependent count disagreements (top 10):\n")
    print(head(dep_disagree, 10))
  }

  if (nrow(married_disagree) > 0) {
    cat("\nMarried/filing status disagreements (top 10):\n")
    print(head(married_disagree, 10))
  }

  invisible(list(
    n_matched            = n_matched,
    dep_match_rate       = dep_match_rate,
    married_match_rate   = married_match_rate,
    tu_count_match_rate  = tu_count_match_rate,
    dep_disagree         = dep_disagree,
    married_disagree     = married_disagree,
    comparison           = comparison
  ))
}
