
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

  #---------------------------------------------------------------------------
  # FMLI: weights, demographics, and tech variables (no expenditure data)
  #---------------------------------------------------------------------------

  tech = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_TYPE', 'COMP_INC')
  demo = c('FAM_SIZE')

  fmli = years %>%
    map(.f = ~ lapply(dir(path       = cex_path,
                          pattern    = paste0('^fmli((', .x%%100, ')[1-4]|(', .x%%100+1, '1))[.]csv$'),
                          full.names = TRUE),
                      fread,
                      select = c(tech, demo)) %>%
          bind_rows()
    ) %>% bind_rows() %>%
    # MO_SCOPE: how many of this interview's 3 CQ reference months fall in the
    # target calendar year. BLS prescribed annual weighting formula:
    #   WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3)
    # Interviews with MO_SCOPE = 0 (e.g., Jan interview referencing only prior
    # year) get zero weight and are effectively excluded from annual estimates.
    # See https://www.bls.gov/cex/pumd-getting-started-guide.htm
    mutate(
      MO_SCOPE = case_when(
        QINTRVYR == years[1]     & QINTRVMO <= 1  ~ 0L,
        QINTRVYR == years[1]     & QINTRVMO == 2  ~ 1L,
        QINTRVYR == years[1]     & QINTRVMO == 3  ~ 2L,
        QINTRVYR == years[1]     & QINTRVMO >= 4  ~ 3L,
        QINTRVYR == years[1] + 1 & QINTRVMO == 1  ~ 3L,
        QINTRVYR == years[1] + 1 & QINTRVMO == 2  ~ 2L,
        QINTRVYR == years[1] + 1 & QINTRVMO == 3  ~ 1L,
        TRUE                                       ~ 0L
      ),
      WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3)
    )

  #---------------------------------------------------------------------------
  # MTBI: UCC-level expenditure detail
  #---------------------------------------------------------------------------
  #
  # Replaces FMLI sub-variables with UCC-level data from MTBI, enabling
  # correct PCE classification (e.g., health insurance → financial_insurance,
  # prescription drugs → other_nondurables, medical equipment → other_durables).
  # See resources/ucc_pce_bridge.csv for the UCC→PCE mapping.

  mtbi = years %>%
    map(.f = ~ lapply(dir(path       = cex_path,
                          pattern    = paste0('^mtbi((', .x%%100, ')[1-4]|(', .x%%100+1, '1))[.]csv$'),
                          full.names = TRUE),
                      fread,
                      select = c('NEWID', 'UCC', 'COST')) %>%
          bind_rows()
    ) %>%
    bind_rows()

  # Read bridge and validate completeness — every UCC in the data must have
  # a row in the bridge CSV (even non-consumption UCCs, which are marked 'exclude')
  bridge = fread('resources/ucc_pce_bridge.csv')
  unmapped = setdiff(unique(mtbi$UCC), bridge$ucc)
  if (length(unmapped) > 0) {
    stop(paste0("Unmapped UCCs in MTBI data: ", paste(unmapped, collapse = ", "),
                ". Add these to resources/ucc_pce_bridge.csv"))
  }

  # All 20 BEA PCE major categories
  # NPISH and net_foreign_travel have no CEX source — set to 0 here,
  # allocated proportionally to total consumption in pce_benchmark.R
  pce_cats     = c('clothing', 'motor_vehicles', 'other_durables', 'furnishings',
                   'rec_goods', 'other_nondurables', 'food_off_premises', 'communication',
                   'npish', 'other_services', 'transport_services', 'rec_services',
                   'net_foreign_travel', 'food_accommodations', 'health_care', 'utilities',
                   'gasoline', 'education', 'financial_insurance', 'housing')
  pce_cats_per = paste0(pce_cats, '_per')

  # Join to bridge, exclude non-consumption UCCs, aggregate by NEWID + PCE category
  mtbi_agg = mtbi %>%
    inner_join(bridge %>% select(ucc, pce_major), by = c('UCC' = 'ucc')) %>%
    filter(pce_major != 'exclude') %>%
    group_by(NEWID, pce_major) %>%
    summarise(cost = sum(COST, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = pce_major, values_from = cost, values_fill = 0)

  # Ensure all 20 PCE columns exist (npish, net_foreign_travel will be missing)
  for (cat in pce_cats) {
    if (!(cat %in% names(mtbi_agg))) mtbi_agg[[cat]] = 0
  }

  #---------------------------------------------------------------------------
  # Join tax units to FMLI (weights/demo) and MTBI (consumption)
  #---------------------------------------------------------------------------

  joined = cex_tus %>%
    left_join(fmli, by = "NEWID") %>%
    left_join(mtbi_agg, by = "NEWID") %>%
    # Filter to complete income reporters
    filter(COMP_INC == 1) %>%
    # Fill NAs in PCE columns (CUs with no MTBI records)
    mutate(across(all_of(pce_cats), ~ replace_na(.x, 0))) %>%
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
    filter(!is.na(age1) & age1 > 17) %>%
    # Apply CU_pct allocation and compute consumption totals
    mutate(
      across(all_of(pce_cats), ~ .x * CU_pct),
      cex_consumption   = rowSums(across(all_of(setdiff(pce_cats, c('npish', 'net_foreign_travel'))))),
      total_consumption = cex_consumption,
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
      NEWID, QINTRVYR, FINLWT21, WT_ANNUAL, pctile_income, married, age1, n_dep, n_dep_ctc,
      male1, income, has_income,
      all_of(pce_cats), total_consumption,
      all_of(pce_cats_per), total_consumption_per
    )

  #---------------------------------------------------------------------------
  # Regular income (positive): consumption levels and ratios
  #---------------------------------------------------------------------------

  joined %<>% filter(has_income == 1)

  pct_breaks = c(-Inf,
                 wtd.quantile(x       = joined$income,
                              probs   = seq(0.01, 0.99, 0.01),
                              weights = joined$WT_ANNUAL),
                 Inf)

  joined = joined %>%
    mutate(
      pctile_income = cut(x              = income,
                          breaks         = pct_breaks,
                          labels         = seq(1, 100, 1),
                          include.lowest = TRUE),
      pctile_income = as.numeric(levels(pctile_income))[pctile_income]
    ) %>%
    # Consumption-to-income ratios (used for dual QRF prediction approach)
    mutate(
      across(all_of(pce_cats), ~ .x / income, .names = '{.col}_per'),
      total_consumption_per = total_consumption / income
    ) %>%
    as_tibble() %>% select(
      NEWID, QINTRVYR, FINLWT21, WT_ANNUAL, pctile_income, married, age1, n_dep, n_dep_ctc,
      male1, income, has_income,
      all_of(pce_cats), total_consumption,
      all_of(pce_cats_per), total_consumption_per
    ) %>%
    bind_rows(irregular_income)

  # Select a random sample using weights to sample
  joined %>%
    slice_sample(n = 100000, replace = T, weight_by = WT_ANNUAL) %>%
    return()
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
    cex_path = '/gpfs/gibbs/project/sarin/shared/raw_data/CEX/2023',
    years    = c(2023)) {

  # -------------------------------------------------------------------------
  # 1. Read NTAXI: one record per tax unit per interview quarter
  # -------------------------------------------------------------------------
  ntaxi = years %>%
    map(.f = ~ lapply(dir(path       = cex_path,
                          pattern    = paste0('^ntaxi((', .x%%100, ')[1-4]|(', .x%%100+1, '1))[.]csv$'),
                          full.names = TRUE),
                      fread,
                      select = c('NEWID', 'TAX_UNIT', 'DEPCNT', 'FILESTAT')) %>%
          bind_rows()
    ) %>%
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
