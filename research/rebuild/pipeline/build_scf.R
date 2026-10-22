#-------------------------------------------------------------------------------
# build_scf.R: the SCF waves 2013, 2016, 2019 and 2022 split into tax units.
#
# Each wave's raw interview items replace the income columns of a copy of the
# SCF summary file, and the production tax-unit builder
# (src/imputations/stage1_scf_tax_units.R) splits each household into couple
# and single units. Raw items are used, not the summary file's incomes,
# because those are inflated to 2022 dollars:
#   wages X5702, taxable interest X5708, dividends X5710, capital gains X5712,
#   business income X5704, net rent/trusts/royalties X5714, Social Security
#   and pensions X5722, tax-exempt interest X5706. A value of -1 means
#   "nothing" and is set to 0.
# The builder runs twice per wave, as the tables of record were built:
#   run 1 swaps in wages, interest, dividends and capital gains only;
#   run 2 swaps in all eight items, and supplies the last four.
# (The builder's sharing rules read the other income columns, so one run with
# all eight could split couples' interest slightly differently.)
# Split-off units (taxunit_code >= 3; their wealth is not asked) are dropped.
# Net worth: the builder's wealth (2022 dollars) over the wave's asset price
# factor (SAVBND / X3902), which puts it in the interview year's dollars. The
# same for the builder's 20 balance-sheet components and 4 unrealized gains
# (src/imputations/wealth_schema.R), carried as w_<name>.
# Business ownership from the raw business section (BUSINESS_ITEMS below):
# owns_business (owns any private business) and k1_owner (a partnership,
# S corporation or LLC interest).
# Ages and sexes of the unit's adults (age1, male1; on couple units age2,
# male2) and the builder's unit code, for the primary filer's age range
# (ages.py).
#
# Writes {out}/data/scf_units_{wave}.csv.gz, one row per tax unit and
# implicate. Run from the repository root through Slurm (run.sbatch, stage scf).
#-------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(data.table); library(haven)
  library(jsonlite)
})
settings = fromJSON('research/rebuild/pipeline/settings.json')
OUT = file.path(Sys.getenv('REBUILD_OUT', settings$paths$out), 'data')
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# The production builder: only its two function definitions are taken, so its
# file-level code does not run.
source('src/imputations/wealth_schema.R')
defs = Filter(function(e) is.call(e) && as.character(e[[1]]) %in% c('=', '<-') && is.name(e[[2]]) &&
                as.character(e[[2]]) %in% c('default_stage1_config', 'build_scf_tax_units'),
              as.list(parse('src/imputations/stage1_scf_tax_units.R', keep.source = FALSE)))
stopifnot(length(defs) == 2)
for (e in defs) eval(e, envir = globalenv())

minus_one_to_zero = function(x) if_else(x == -1, 0, x)
# Business section: owns a private business (X3103), owns a business not actively managed (X3401); legal
# form of actively managed businesses 1 and 2 (X3119, X3219: 1 partnership, 3 S corporation, 11 and 12
# limited partnership, LLP or LLC); not actively managed limited partnerships (X3407), other partnerships
# (X3411), LLCs (X3451) and S corporations (X3415).
BUSINESS_ITEMS = c('x3103', 'x3401', 'x3119', 'x3219', 'x3407', 'x3411', 'x3451', 'x3415')
K1_FORMS = c(1, 3, 11, 12)

run_builder = function(scfp, carriers, raw_path, wave, tag) {
  mod = as_tibble(scfp) %>% select(-all_of(setdiff(names(carriers), 'Y1'))) %>%
    left_join(carriers, by = 'Y1')
  path = file.path(OUT, sprintf('scfp%d_%s.csv', wave, tag))
  fwrite(mod, path)
  tu = build_scf_tax_units(default_stage1_config(), raw_path, path, syear = wave)
  file.remove(path)
  tu
}

for (wave in settings$paths$raw$scf_waves) {
  dir = sub('{wave}', wave, settings$paths$raw$scf, fixed = TRUE)
  raw_path = file.path(dir, sprintf('p%02di6.dta', wave %% 100))
  scfp = fread(file.path(dir, sprintf('SCFP%d.csv', wave)))
  hh = read_dta(raw_path, col_select = all_of(c('y1', 'yy1', 'x42001', 'x3902', 'x5702', 'x5704', 'x5706',
                                                 'x5708', 'x5710', 'x5712', 'x5714', 'x5722', BUSINESS_ITEMS))) %>%
    mutate(across(everything(), as.numeric))
  names(hh) = toupper(names(hh))
  stopifnot(!anyDuplicated(hh$Y1), !anyNA(hh), nrow(hh) == nrow(scfp))
  hh = hh %>% mutate(across(c(X5702, X5704, X5706, X5712, X5714, X5722), minus_one_to_zero))
  stopifnot(all(hh$X5702 >= 0), all(hh$X5706 >= 0), all(hh$X5708 >= 0), all(hh$X5710 >= 0), all(hh$X5722 >= 0))

  # the asset price factor: SAVBND / X3902, the same on every household
  fx = as_tibble(scfp) %>% select(Y1, SAVBND) %>% inner_join(hh, by = 'Y1') %>% filter(X3902 > 0)
  asset_factor = median(fx$SAVBND / fx$X3902)
  stopifnot(max(abs(fx$SAVBND / fx$X3902 / asset_factor - 1)) < 1e-3)

  basic = hh %>% transmute(Y1, WAGEINC = X5702, INTDIVINC = X5708, RENT = X5710, KGINC = X5712)
  tu = run_builder(scfp, basic, raw_path, wave, 'run1')
  keep = tu$taxunit_code < 3
  nw_2022 = rowSums(as.matrix(tu[, wealth_asset_vars])) - rowSums(as.matrix(tu[, wealth_debt_vars]))
  units = tu %>% mutate(net_worth = nw_2022 / asset_factor) %>% filter(keep) %>%
    transmute(record_id = as.numeric(taxunitid), household_id = as.numeric(scf_hh_id),
              implicate = as.integer(implicate), year = wave - 1L, weight = weight,
              married = as.integer(married == 1 & filestat == 2), n_dep = as.integer(pmin(n_dep, 3)),
              age1, age2, male1, male2, taxunit_code,
              wages = wages_scf, interest = int_div_scf, dividends = rent_scf,
              capital_gains = capital_gains_scf, net_worth, asset_factor = asset_factor)
  # the 20 balance-sheet components and the 4 unrealized gains, in the interview year's dollars
  comps = as_tibble(as.matrix(tu[keep, c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)]) / asset_factor)
  names(comps) = paste0('w_', names(comps))
  stopifnot(nrow(comps) == nrow(units), !anyNA(comps))
  units = bind_cols(units, comps)
  stopifnot(max(abs(rowSums(as.matrix(units[, paste0('w_', wealth_asset_vars)])) -
                    rowSums(as.matrix(units[, paste0('w_', wealth_debt_vars)])) - units$net_worth)) < 1e-3)

  # Business ownership, asked of the household (per implicate): owns any private business, and holds a
  # partnership, S corporation or LLC interest. Each goes to the units holding the household's business
  # equity, or to the respondent's unit (code 1) when that equity is zero, or to the household's first
  # unit when no unit has code 1.
  biz = hh %>% transmute(Y1, owns = X3103 == 1 | X3401 == 1,
                         k1 = X3119 %in% K1_FORMS | X3219 %in% K1_FORMS | X3407 == 1 | X3411 == 1 |
                           X3451 == 1 | X3415 == 1)
  stopifnot(all(biz$owns | !biz$k1))
  ub = tibble(Y1 = tu$scf_hh_id[keep] * 10 + tu$implicate[keep], code = tu$taxunit_code[keep],
              bus = tu$pass_throughs[keep]) %>%
    left_join(biz, by = 'Y1') %>% group_by(Y1) %>%
    mutate(rule = if (any(bus > 0)) 'equity' else if (any(code == 1)) 'respondent' else 'first unit',
           holder = if (any(bus > 0)) bus > 0 else if (any(code == 1)) code == 1 else row_number() == 1) %>%
    ungroup()
  stopifnot(!anyNA(ub$owns), nrow(ub) == nrow(units))
  print(ub %>% filter(owns) %>% distinct(Y1, rule) %>% count(rule, name = 'household-implicates owning a business'))
  stopifnot(nrow(biz %>% filter(owns) %>% anti_join(ub %>% filter(holder), by = 'Y1')) == 0)
  units = units %>% mutate(owns_business = as.integer(ub$owns & ub$holder),
                           k1_owner = as.integer(ub$k1 & ub$holder))

  all8 = basic %>% left_join(hh %>% transmute(Y1, BUSSEFARMINC = X5704, TRANSFOTHINC = X5714,
                                             SSRETINC = X5722, INCOME = X5706), by = 'Y1')
  tu2 = run_builder(scfp, all8, raw_path, wave, 'run2')
  more = tu2 %>% filter(taxunit_code < 3) %>%
    transmute(record_id = as.numeric(taxunitid), business_income = business_scf,
              rent_royalty_estate = ui_other_scf, retirement_gross = ss_pens_scf, tax_exempt_interest = income)
  units = units %>% inner_join(more, by = 'record_id')
  # The spouse's age and sex exist only on joint units; elsewhere -1 (not in universe).
  stopifnot(!anyNA(units$age1), !anyNA(units$male1),
            all(!is.na(units$age2[units$married == 1])), all(!is.na(units$male2[units$married == 1])))
  cat(sprintf('wave %d: %d units with a spouse age but not filing jointly (set to -1)\n', wave,
              sum(!is.na(units$age2) & units$married == 0)))
  units = units %>% mutate(age2 = if_else(married == 1, age2, -1L), male2 = if_else(married == 1, male2, -1L))
  stopifnot(nrow(units) == sum(keep), !anyDuplicated(units$record_id), !anyNA(units))
  fwrite(units, file.path(OUT, sprintf('scf_units_%d.csv.gz', wave)))
  cat(sprintf('wave %d: %d unit rows, asset factor %.6f\n', wave, nrow(units), asset_factor))
}
