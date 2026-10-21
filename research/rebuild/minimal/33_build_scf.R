#--------------------------------------
# 33_build_scf.R  (minimal example, step 33: more variables; 33_STATUS.md)
#
# What it does: for one SCF wave, splits four more household income items
# into tax units with the production builder (build_scf_tax_units in
# src/imputations/stage1_scf_tax_units.R), exactly as 01_build_table.R's wave
# mode does for wages, interest, dividends and capital gains. The builder
# splits every income column of the SCFP it is handed, so the raw interview
# items are put into its income columns ("carriers"):
#   WAGEINC      = X5702 wages                       (split by share_labor)
#   INTDIVINC    = X5708 taxable interest            (share_earn)
#   RENT         = X5710 dividends                   (share_earn)
#   KGINC        = X5712 capital gains, -1 -> 0      (share_earn)
#   BUSSEFARMINC = X5704 business, professional practice, limited
#                  partnership or farm income        (share_business)
#   TRANSFOTHINC = X5714 net rent, trusts, royalties (share_earn)
#   SSRETINC     = X5722 Social Security or other pensions, annuities,
#                  disability or retirement programs (share_earn)
#   INCOME       = X5706 tax-exempt interest         (share_earn)
# The first four carriers are as in 01_build_table.R, so wages, interest,
# dividends and capital gains come out identical to the earlier waves
# (checked in 33_tables.py). The SCFP's income columns are used by the
# builder only in this final split (stage1_scf_tax_units.R lines 643-665).
# Codes -1 ("nothing" where the codebook lists it) are set to 0 for every
# item and counted. Amounts are the tax year's dollars (raw items).
# Split-off units (taxunit_code >= 3) are dropped, as in 01_build_table.R.
#
# Writes (scratch data/): scf<wave>_units33.csv.gz, checks_scf<wave>_33.csv
# Run: sbatch research/rebuild/minimal/33_scf.sbatch <wave>
#--------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(data.table); library(haven); library(yaml)
})

OUT  = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
args = commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1)
WAVE = as.integer(args[1])
YEAR = WAVE - 1L

chk = list()
add = function(group, key, value) {
  chk[[length(chk) + 1]] <<- tibble(group = group, key = key, value = as.numeric(value))
}

output_roots = read_yaml('config/interfaces/output_roots.yaml')
iv = read_yaml('config/interfaces/interface_versions.yaml')
scf_dir = file.path(output_roots$production, iv$SCF$type, 'SCF',
                    paste0('v', iv$SCF$version), WAVE, 'historical')

source('src/imputations/wealth_schema.R')
ex = as.list(parse('src/imputations/stage1_scf_tax_units.R', keep.source = FALSE))
is_def = function(e) is.call(e) && as.character(e[[1]]) %in% c('=', '<-') &&
  is.name(e[[2]]) && as.character(e[[2]]) %in% c('default_stage1_config', 'build_scf_tax_units')
defs = Filter(is_def, ex)
stopifnot(length(defs) == 2)
for (e in defs) eval(e, envir = globalenv())

raw_path  = file.path(scf_dir, sprintf('p%02di6.dta', WAVE %% 100))
scfp_path = file.path(scf_dir, sprintf('SCFP%d.csv', WAVE))
stopifnot(file.exists(raw_path), file.exists(scfp_path))

new_items = c('x5704', 'x5706', 'x5714', 'x5716', 'x5718', 'x5720', 'x5722', 'x5724')
items = unique(c('y1', 'yy1', 'x42001', 'x5702', 'x5708', 'x5710', 'x5712',
                 'x5701', 'x5703', 'x5705', 'x5713', 'x5721', new_items))
hh = read_dta(raw_path, col_select = all_of(items))
for (v in items) cat(sprintf('label %d %s: %s\n', WAVE, v, paste(attr(hh[[v]], 'label'), collapse = ' ')))
for (v in c('x5704', 'x5714', 'x5722', 'x5706')) {
  lb = attr(hh[[v]], 'labels')
  if (!is.null(lb)) cat(sprintf('value labels %d %s: %s\n', WAVE, v,
                                paste(names(lb), lb, sep = '=', collapse = '; ')))
}
hh = hh %>% mutate(across(everything(), as.numeric))
names(hh) = toupper(names(hh))
stopifnot(!anyDuplicated(hh$Y1), !anyNA(hh))
w5 = hh$X42001 / 5
for (v in toupper(c('x5702', new_items))) {
  add('raw_eq_-1_rows', v, sum(hh[[v]] == -1))
  add('raw_below_-1_rows', v, sum(hh[[v]] < -1))
  add('raw_weighted_total_billions_after_-1_to_0', v, sum(w5 * pmax(hh[[v]], 0) * (hh[[v]] != -1) +
                                                             w5 * hh[[v]] * (hh[[v]] < -1)) / 1e9 / 5)
  add('raw_weighted_households_nonzero_millions', v, sum(w5 * (hh[[v]] != 0 & hh[[v]] != -1)) / 1e6 / 5)
}
fix = function(x) if_else(x == -1, 0, x)
hh = hh %>% mutate(X5702 = fix(X5702), kg_raw = fix(X5712),
                   biz = fix(X5704), rent = fix(X5714), ssret = fix(X5722), texi = fix(X5706))
stopifnot(all(hh$X5702 >= 0), all(hh$X5708 >= 0), all(hh$X5710 >= 0),
          all(hh$ssret >= 0), all(hh$texi >= 0))

scfp = fread(scfp_path)
stopifnot(nrow(scfp) == nrow(hh), all(scfp$Y1 %in% hh$Y1))
scfp_mod = as_tibble(scfp) %>%
  select(-WAGEINC, -INTDIVINC, -RENT, -KGINC, -BUSSEFARMINC, -TRANSFOTHINC, -SSRETINC, -INCOME) %>%
  left_join(hh %>% transmute(Y1, WAGEINC = X5702, INTDIVINC = X5708, RENT = X5710, KGINC = kg_raw,
                             BUSSEFARMINC = biz, TRANSFOTHINC = rent, SSRETINC = ssret, INCOME = texi),
            by = 'Y1')
mod_path = file.path(OUT, sprintf('wave%d_scfp_rawincome33.csv', WAVE))
fwrite(scfp_mod, mod_path)

tu = build_scf_tax_units(default_stage1_config(), raw_path, mod_path, syear = WAVE)
tu = tu %>% mutate(Y1 = taxunitid %/% 100)
is_npeu = tu$taxunit_code >= 3

scf_tab = tu %>%
  transmute(year = YEAR, record_id = as.numeric(taxunitid), household_id = as.numeric(scf_hh_id),
            implicate = as.integer(implicate), weight = weight,
            wages = wages_scf, capital_gains = capital_gains_scf,
            business_income = business_scf, rent_royalty_estate = ui_other_scf,
            retirement_gross = ss_pens_scf, tax_exempt_interest = income)
scf_tab = scf_tab[!is_npeu, ]

# Couple and single units add up to the household's raw items.
peu = scf_tab %>% mutate(Y1 = tu$Y1[!is_npeu]) %>% group_by(Y1) %>%
  summarise(across(c(wages, capital_gains, business_income, rent_royalty_estate,
                     retirement_gross, tax_exempt_interest), sum), .groups = 'drop') %>%
  inner_join(hh %>% select(Y1, X5702, kg_raw, biz, rent, ssret, texi), by = 'Y1')
stopifnot(nrow(peu) == nrow(hh))
add('identity_max_abs_dollars', 'wages_vs_X5702', max(abs(peu$wages - peu$X5702)))
add('identity_max_abs_dollars', 'capital_gains_vs_X5712', max(abs(peu$capital_gains - peu$kg_raw)))
add('identity_max_abs_dollars', 'business_vs_X5704', max(abs(peu$business_income - peu$biz)))
add('identity_max_abs_dollars', 'rent_vs_X5714', max(abs(peu$rent_royalty_estate - peu$rent)))
add('identity_max_abs_dollars', 'retirement_vs_X5722', max(abs(peu$retirement_gross - peu$ssret)))
add('identity_max_abs_dollars', 'tax_exempt_vs_X5706', max(abs(peu$tax_exempt_interest - peu$texi)))
for (v in names(scf_tab)) add('na_count', v, sum(is.na(scf_tab[[v]])))
if (any(is.na(scf_tab))) stop('NA in the step-33 SCF items; see checks')
stopifnot(max(abs(peu$business_income - peu$biz)) < 0.01, max(abs(peu$rent_royalty_estate - peu$rent)) < 0.01,
          max(abs(peu$retirement_gross - peu$ssret)) < 0.01, max(abs(peu$tax_exempt_interest - peu$texi)) < 0.01)

fwrite(scf_tab, file.path(OUT, sprintf('scf%d_units33.csv.gz', WAVE)))
chk = bind_rows(chk)
write_csv(chk, file.path(OUT, sprintf('checks_scf%d_33.csv', WAVE)))
print(as.data.frame(chk), digits = 10)
cat(sprintf('33_build_scf: wave %d, wrote %d unit rows\n', WAVE, nrow(scf_tab)))
