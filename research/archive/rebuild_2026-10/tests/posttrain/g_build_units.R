#--------------------------------------
# g_build_units.R  (test 3, piece G): copy of test 2's a_build_units.R that
# runs g_scf_tax_units.R (extra columns, G-CHANGE) and writes to
# $P/g/scf_units/. Filing rule and checks unchanged; extra G checks added.
# Original header (test 2, piece A):
#
# Builds SCF tax units for the waves given on the command line (default
# 2016 2013 2019) with the copied builder a_scf_tax_units.R, applies the
# filing rule, runs the wave checks, and writes per wave:
#   $S/data/scf_units/scf_units_<wave>.csv.gz   one row per tax unit x implicate
#   $S/data/scf_units/checks_<wave>.csv         check tables (long format)
# Run from the repo root through Slurm (a_build_units.sbatch).
#--------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(data.table); library(haven)
})

S   = '/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint'
OUT = '/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/g/scf_units'
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
RAWDIR = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1'

source('src/imputations/wealth_schema.R')
source('research/rebuild/tests/posttrain/g_scf_tax_units.R')

args  = commandArgs(trailingOnly = TRUE)
waves = if (length(args)) as.integer(args) else c(2013L, 2016L)

# Filing threshold for a single filer (gross income), by tax year = wave - 1.
# IRS Publication 501 for each year: under 65 / 65 and older.
thresholds = list(`2012` = c(9750, 11200), `2015` = c(10300, 11850),
                  `2018` = c(12000, 13600), `2021` = c(12550, 14250))

wealth_nonkg = c(wealth_asset_vars, wealth_debt_vars)
raw_income_items = c('X5702', 'X5704', 'X5706', 'X5708', 'X5710', 'X5712', 'X5714', 'X5716', 'X5718', 'X5720', 'X5722', 'X5724', 'X5729')
inc7 = c('wages', 'business', 'int_div', 'capital_gains', 'rent', 'ss_pens', 'ui_other')

for (wave in waves) {
  t0 = Sys.time()
  yy = substr(as.character(wave), 3, 4)
  raw_path  = file.path(RAWDIR, wave, 'historical', sprintf('p%si6.dta', yy))
  scfp_path = file.path(RAWDIR, wave, 'historical', sprintf('SCFP%d.csv', wave))
  cat(sprintf('\n===== wave %d =====\n', wave))

  tu = build_scf_tax_units(default_stage1_config(), raw_path, scfp_path, syear = wave)

  # --- the seven categories, named for the test ---
  for (v in inc7) tu[[v]] = tu[[paste0(v, '_raw')]]

  # --- NA audit on every column used downstream ---
  used = c('taxunitid', 'scf_hh_id', 'implicate', 'weight', 'age1', 'n_dep',
           'married', 'taxunit_code', 'x5744', 'x5746', inc7, wealth_y_vars)
  na_ct = sapply(used, function(v) sum(is.na(tu[[v]])))
  if (any(na_ct > 0)) {
    print(na_ct[na_ct > 0])
    stop('NA values in used columns (see counts above)')
  }

  # --- filing rule ---
  thr = thresholds[[as.character(wave - 1)]]
  filed_hh = tu$x5744 %in% c(1, 6)
  is_npeu  = tu$taxunit_code >= 3
  # Gross income for the filing requirement of a split-off (NPEU) unit:
  # everything except Social Security and welfare. NPEU rows carry their
  # Section-Y pieces only (shares are 0), so the categories are theirs.
  # ss_pens for NPEU mixes Social Security (GSSINC) and pensions (PENINC);
  # it is left out here, which understates gross income for pensioners.
  gross_npeu = tu$wages + tu$business + tu$int_div + tu$capital_gains +
               tu$rent + tu$ui_other
  required = (gross_npeu >= ifelse(tu$age1 >= 65, thr[2], thr[1])) |
             (tu$business >= 400)
  tu$npeu_required_to_file = as.integer(is_npeu & required)
  tu$npeu_has_wages        = as.integer(is_npeu & tu$wages > 0)
  tu$filer = case_when(
    tu$taxunit_code == 0 ~ filed_hh,
    tu$taxunit_code == 1 ~ filed_hh & tu$x5746 != 4,   # 4 = only spouse/partner filed
    tu$taxunit_code == 2 ~ filed_hh & tu$x5746 != 3,   # 3 = only respondent filed
    is_npeu              ~ required | tu$wages > 0
  )
  tu$filer_rule = case_when(
    tu$taxunit_code == 0 ~ 'household answer X5744',
    tu$taxunit_code %in% c(1, 2) ~ 'household answers X5744 and X5746',
    TRUE ~ 'split-off member: gross income at or above the filing threshold, or self-employment income of $400 or more, or any wages'
  )
  tu$wave = wave

  # --- checks ---
  w = tu$weight
  chk = list()
  add = function(table, key, value) chk[[length(chk) + 1]] <<- tibble(wave = wave, table = table, key = key, value = value)

  add('counts', 'households', n_distinct(tu$scf_hh_id))
  add('counts', 'unit_rows_all_implicates', nrow(tu))
  add('counts', 'unit_rows_per_household_implicate', nrow(tu) / (5 * n_distinct(tu$scf_hh_id)))
  hi = tu %>% group_by(taxunitid %/% 100) %>% summarise(n = n(), split = any(taxunit_code %in% c(1, 2)), npeu = any(taxunit_code >= 3), w = first(weight), .groups = 'drop')
  add('counts', 'share_household_implicates_more_than_one_unit_unweighted', mean(hi$n > 1))
  add('counts', 'share_household_implicates_more_than_one_unit_weighted', sum(hi$w * (hi$n > 1)) / sum(hi$w))
  add('counts', 'share_household_implicates_spouse_partner_split_weighted', sum(hi$w * hi$split) / sum(hi$w))
  add('counts', 'share_household_implicates_with_npeu_units_weighted', sum(hi$w * hi$npeu) / sum(hi$w))
  add('counts', 'weighted_households_millions', sum(hi$w) / 1e6)
  add('counts', 'weighted_units_millions', sum(w) / 1e6)
  for (k in c(0, 1, 2)) add('units_by_code_weighted_millions', paste0('code_', k), sum(w[tu$taxunit_code == k]) / 1e6)
  add('units_by_code_weighted_millions', 'npeu', sum(w[is_npeu]) / 1e6)
  # filing
  add('filing', 'weighted_filers_millions', sum(w[tu$filer]) / 1e6)
  add('filing', 'weighted_nonfilers_millions', sum(w[!tu$filer]) / 1e6)
  for (k in c(0, 1, 2)) add('filing', paste0('filer_share_code_', k), sum(w[tu$filer & tu$taxunit_code == k]) / sum(w[tu$taxunit_code == k]))
  add('filing', 'npeu_weighted_millions', sum(w[is_npeu]) / 1e6)
  add('filing', 'npeu_required_to_file_weighted_millions', sum(w[is_npeu & required]) / 1e6)
  add('filing', 'npeu_wages_but_not_required_weighted_millions', sum(w[is_npeu & !required & tu$wages > 0]) / 1e6)
  add('filing', 'npeu_filers_rule_weighted_millions', sum(w[is_npeu & tu$filer]) / 1e6)
  add('filing', 'npeu_unit_rows', sum(is_npeu))
  add('filing', 'npeu_filer_unit_rows', sum(is_npeu & tu$filer))
  add('filing', 'filer_unit_rows', sum(tu$filer))
  for (code in sort(unique(tu$x5746))) add('filing_x5746_split_units_weighted_millions', paste0('x5746_', code, '_filedhh_', 'yes'), sum(w[tu$taxunit_code %in% c(1, 2) & filed_hh & tu$x5746 == code]) / 1e6)
  add('filing', 'split_units_household_did_not_file_weighted_millions', sum(w[tu$taxunit_code %in% c(1, 2) & !filed_hh]) / 1e6)

  # income: units sum vs household raw items, vs SCFP fields
  scfp = fread(scfp_path)
  setnames(scfp, toupper(names(scfp)))
  hh_raw = tu %>% filter(taxunit_code %in% c(0, 1)) %>% distinct(Y1 = taxunitid %/% 100, .keep_all = TRUE)
  hh_raw = hh_raw %>% left_join(as_tibble(scfp) %>% select(Y1, WGT, WAGEINC, BUSSEFARMINC, INTDIVINC, KGINC, SSRETINC, TRANSFOTHINC, INCOME, NETWORTH), by = 'Y1')
  if (anyNA(hh_raw$WGT)) stop('SCFP join failed for some households')
  add('scfp_join', 'max_abs_weight_diff_raw_vs_scfp_over_5', max(abs(hh_raw$weight - hh_raw$WGT / 5)))
  # raw household totals (sum over implicate rows, weighted)
  rawtab = tu %>% filter(taxunit_code %in% c(0, 1)) %>% distinct(Y1 = taxunitid %/% 100, .keep_all = TRUE)
  # Ratio SCFP / raw per category: SCFP converts to survey-year dollars.
  rt = function(a, b) sum(hh_raw$weight * a) / sum(hh_raw$weight * b)
  hx = function(v) { x = hh_raw[[paste0('hh_', v)]]; ifelse(x == -1, 0, x) }
  add('scfp_vs_raw_ratio', 'WAGEINC/X5702', rt(hh_raw$WAGEINC, hx('X5702')))
  add('scfp_vs_raw_ratio', 'BUSSEFARMINC/(X5704+X5714)', rt(hh_raw$BUSSEFARMINC, hx('X5704') + hx('X5714')))
  add('scfp_vs_raw_ratio', 'BUSSEFARMINC/X5704', rt(hh_raw$BUSSEFARMINC, hx('X5704')))
  add('scfp_vs_raw_ratio', 'INTDIVINC/(X5706+X5708+X5710)', rt(hh_raw$INTDIVINC, hx('X5706') + hx('X5708') + hx('X5710')))
  add('scfp_vs_raw_ratio', 'KGINC/X5712', rt(hh_raw$KGINC, hx('X5712')))
  add('scfp_vs_raw_ratio', 'SSRETINC/X5722', rt(hh_raw$SSRETINC, hx('X5722')))
  add('scfp_vs_raw_ratio', 'TRANSFOTHINC/(X5716+X5718+X5720+X5724)', rt(hh_raw$TRANSFOTHINC, hx('X5716') + hx('X5718') + hx('X5720') + hx('X5724')))
  add('scfp_vs_raw_ratio', 'INCOME/X5729', rt(hh_raw$INCOME, hx('X5729')))
  # record-level exactness of the ratio for wages
  r_w = hh_raw$WAGEINC[hh_raw$hh_X5702 > 0] / hh_raw$hh_X5702[hh_raw$hh_X5702 > 0]
  add('scfp_vs_raw_ratio', 'WAGEINC/X5702_min_record', min(r_w))
  add('scfp_vs_raw_ratio', 'WAGEINC/X5702_max_record', max(r_w))
  # units add up to the household (PEU part) plus NPEU amounts
  for (v in inc7) {
    add('weighted_totals_billions_all_units', v, sum(w * tu[[v]]) / 1e9)
    add('weighted_totals_billions_filers', v, sum(w[tu$filer] * tu[[v]][tu$filer]) / 1e9)
    add('weighted_totals_billions_npeu_units', v, sum(w[is_npeu] * tu[[v]][is_npeu]) / 1e9)
    add('share_nonzero_filers', v, sum(w[tu$filer] * (tu[[v]][tu$filer] != 0)) / sum(w[tu$filer]))
    add('share_negative_filers', v, sum(w[tu$filer] * (tu[[v]][tu$filer] < 0)) / sum(w[tu$filer]))
  }
  peu_items = c(wages = 'X5702', business = 'X5704', capital_gains = 'X5712', rent = 'X5714', ss_pens = 'X5722')
  for (v in names(peu_items)) {
    a = sum(w[!is_npeu] * tu[[v]][!is_npeu]); b = sum(hh_raw$weight * hx(peu_items[[v]]))
    add('peu_units_sum_over_household_raw', v, a / b)
  }
  add('masked_negative', 'business_X5704_eq_-9_household_implicates', sum(hh_raw$hh_X5704 == -9))
  add('masked_negative', 'rent_X5714_eq_-9_household_implicates', sum(hh_raw$hh_X5714 == -9))
  # net worth: units vs SCFP
  tu$networth = rowSums(as.matrix(tu[, wealth_asset_vars])) - rowSums(as.matrix(tu[, wealth_debt_vars]))
  add('networth', 'units_weighted_total_trillions', sum(w * tu$networth) / 1e12)
  add('networth', 'scfp_weighted_total_trillions', sum(hh_raw$weight * hh_raw$NETWORTH) / 1e12)
  add('networth', 'filer_units_weighted_total_trillions', sum(w[tu$filer] * tu$networth[tu$filer]) / 1e12)
  nw_hh = tu %>% group_by(Y1 = taxunitid %/% 100) %>% summarise(nw = sum(networth), .groups = 'drop') %>% left_join(hh_raw %>% select(Y1, NETWORTH), by = 'Y1')
  add('networth', 'max_abs_household_diff_units_vs_scfp_dollars', max(abs(nw_hh$nw - nw_hh$NETWORTH)))
  add('networth', 'share_filers_weighted_of_networth', sum(w[tu$filer] * tu$networth[tu$filer]) / sum(w * tu$networth))
  for (v in wealth_y_vars) {
    add('wealth_weighted_totals_billions_all_units', v, sum(w * tu[[v]]) / 1e9)
    add('wealth_share_nonzero_filers', v, sum(w[tu$filer] * (tu[[v]][tu$filer] != 0)) / sum(w[tu$filer]))
    add('wealth_share_negative_filers', v, sum(w[tu$filer] * (tu[[v]][tu$filer] < 0)) / sum(w[tu$filer]))
  }
  # demographics of filers
  for (m in c(0, 1)) add('filers_married_share', paste0('married_', m), sum(w[tu$filer & tu$married == m & tu$filestat == ifelse(m == 1, 2, tu$filestat)]) / sum(w[tu$filer]))
  add('filers_mfj_share', 'filestat_2', sum(w[tu$filer & tu$filestat == 2]) / sum(w[tu$filer]))
  for (k in 0:3) add('filers_n_dep_share', paste0('n_dep_', k, ifelse(k == 3, '_plus', '')), sum(w[tu$filer & pmin(tu$n_dep, 3) == k]) / sum(w[tu$filer]))
  add('filers_age', 'share_age_lt_26', sum(w[tu$filer & tu$age1 < 26]) / sum(w[tu$filer]))
  add('filers_age', 'share_age_ge_65', sum(w[tu$filer & tu$age1 >= 65]) / sum(w[tu$filer]))
  add('filers_age', 'min_age1', min(tu$age1[tu$filer]))

  # --- G-CHANGE: Social Security and pensions, schedule flags, sex ---
  ss_hh = tu$g_ss_r + tu$g_ss_sp
  tu$ss_gross = case_when(tu$taxunit_code == 0 ~ ss_hh,
                          tu$taxunit_code == 1 ~ tu$g_ss_r,
                          tu$taxunit_code == 2 ~ tu$g_ss_sp,
                          TRUE ~ tu$npeu_gssinc)
  x5722 = ifelse(tu$hh_X5722 == -1, 0, tu$hh_X5722)
  tu$pens_total = ifelse(is_npeu, tu$npeu_peninc, pmax(x5722 - ss_hh, 0) * tu$share_earn)
  hh1 = !is_npeu & tu$taxunit_code %in% c(0, 1)
  add('g_ss', 'weighted_hh_X5722_billions', sum(w[hh1] * x5722[hh1]) / 1e9)
  add('g_ss', 'weighted_hh_ss_current_annualized_billions', sum(w[hh1] * ss_hh[hh1]) / 1e9)
  add('g_ss', 'share_hh_with_ss_current', sum(w[hh1] * (ss_hh[hh1] > 0)) / sum(w[hh1]))
  add('g_ss', 'share_hh_with_X5722', sum(w[hh1] * (x5722[hh1] > 0)) / sum(w[hh1]))
  add('g_ss', 'share_hh_ss_current_gt_X5722_among_ss', sum(w[hh1] * (ss_hh[hh1] > x5722[hh1] & ss_hh[hh1] > 0)) / sum(w[hh1] * (ss_hh[hh1] > 0)))
  add('g_ss', 'share_hh_X5722_pos_but_no_ss_no_pension_items', sum(w[hh1] * (x5722[hh1] > 0 & ss_hh[hh1] == 0)) / sum(w[hh1]))
  for (v in c('g_schc', 'g_sche', 'g_schf')) {
    tb = table(tu[[v]][hh1], useNA = 'ifany')
    for (i in seq_along(tb)) add(paste0('g_codes_', v), ifelse(is.na(names(tb)[i]), 'NA', names(tb)[i]), as.numeric(tb[i]))
  }
  tb = table(tu$male1[tu$filer], useNA = 'ifany')
  for (i in seq_along(tb)) add('g_codes_male1_filers', ifelse(is.na(names(tb)[i]), 'NA', names(tb)[i]), as.numeric(tb[i]))
  tb = table(pmin(tu$n_u17[tu$filer], 4), useNA = 'ifany')
  for (i in seq_along(tb)) add('g_codes_n_u17_filers', ifelse(is.na(names(tb)[i]), 'NA', names(tb)[i]), as.numeric(tb[i]))
  add('g_npeu', 'n_dep_minus_n_u17_negative_rows', sum(tu$n_u17 > tu$n_dep))

  chk = bind_rows(chk)
  write_csv(chk, file.path(OUT, sprintf('checks_%d.csv', wave)))
  print(as.data.frame(chk), digits = 6)

  keep = c('wave', 'taxunitid', 'scf_hh_id', 'implicate', 'taxunit_code', 'weight',
           'age1', 'age2', 'married', 'filestat', 'n_dep', 'x5744', 'x5746',
           'filer', 'npeu_required_to_file', 'npeu_has_wages', 'masked_neg_business', 'masked_neg_rent',
           inc7, paste0(inc7, '_scf'), 'income', wealth_y_vars, 'networth',
           'ui_other_broad_raw', 'share_earn', 'share_labor', 'share_business', 'share_housing',
           paste0('hh_', raw_income_items),
           # G-CHANGE
           'male1', 'n_u17', 'g_ss_r', 'g_ss_sp', 'g_schc', 'g_sche', 'g_schf',
           'npeu_intinc', 'npeu_divinc', 'npeu_unempinc', 'npeu_gssinc', 'npeu_peninc', 'npeu_tbusinc',
           'ss_gross', 'pens_total')
  fwrite(tu[, keep], file.path(OUT, sprintf('scf_units_%d.csv.gz', wave)))
  cat(sprintf('wave %d written: %d rows, %.1f min\n', wave, nrow(tu),
              as.numeric(difftime(Sys.time(), t0, units = 'mins'))))
}
