#-------------------------------------------------------------------------------
# The CEX interview survey in tax units: one row per tax unit and interview, with
# the unit's income and its share of the consumer unit's spending in the 8 PCE
# categories, in nominal dollars of the survey year.
#
# roster_cex.py runs first. It builds the tax units and writes, for each person,
# the return they are on. This script reads the CEX member and family files and
# that person file. Each unit takes its members' own income items (wages,
# self-employment income, Social Security and SSI). The consumer unit's other
# income is divided among its tax units in proportion to their members' own
# income. Its spending is divided in proportion to the number of people. Writes
# data/cex_units_<year>.csv.gz and summary checks. Income and spending follow
# the production reader (src/cex.R). FEATURES.md, "The CEX", has the method.
# Run from the repository root.
#-------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(data.table); library(jsonlite)
})
options(width = 200, dplyr.summarise.inform = FALSE)

settings = fromJSON(Sys.getenv('REBUILD_SETTINGS', 'research/rebuild/pipeline/settings.json'))
CEX_RAW = settings$paths$raw$cex
OUT     = file.path(Sys.getenv('REBUILD_OUT', settings$paths$out), 'data')
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
YEARS   = settings$cex$years

#-------------------------------------------------------------------------------
# Reading the CEX files
#-------------------------------------------------------------------------------

# The five quarterly files of survey year N: the first quarter of N ({yy}1x in
# release N up to 2019, {yy}1 in release N-1 from 2020), the second to fourth
# quarters of N, and the first quarter of N+1 in release N.
year_files = function(prefix, year) {
  yy = year %% 100
  q1 = if (year <= 2019) file.path(year, sprintf('%s%02d1x.csv', prefix, yy))
       else file.path(year - 1, sprintf('%s%02d1.csv', prefix, yy))
  c(q1, file.path(year, sprintf('%s%02d%d.csv', prefix, yy, 2:4)),
    file.path(year, sprintf('%s%02d1.csv', prefix, (yy + 1) %% 100)))
}

# Reads and stacks a year's five files of one kind. The `need` columns must be in
# every file; the `optional` ones are read where present. A missing file stops
# the run.
read_year = function(prefix, year, need, optional = character(0), ...) {
  paths = file.path(CEX_RAW, year_files(prefix, year))
  have = file.exists(paths)
  if (!all(have))
    stop(sprintf('%s %d: missing file(s) %s', prefix, year, paste(paths[!have], collapse = ', ')))
  map_dfr(paths[have], function(p) {
    h = toupper(names(fread(p, nrows = 0)))
    miss = setdiff(need, h)
    if (length(miss) > 0) stop(sprintf('%s lacks %s', p, paste(miss, collapse = ', ')))
    # '.' is BLS's mark for a missing value in some files (AGE in 2015's third and fourth quarters). It is read as
    # NA and reported.
    d = fread(p, select = c(need, intersect(optional, h)), na.strings = c('', 'NA', '.'), ...) %>% as_tibble()
    d$src = basename(p)
    d
  })
}

MEMBER_INC = c(wages = 'SALARYXM', se_income = 'SEMPFRMM', social_security = 'SOCRRXM', ssi = 'SSIXM')
CU_INC     = c(int_div = 'INTRDVXM', net_rent = 'NETRENTM', pensions = 'RETSURVM',
               other_regular = 'OTHREGXM', roy_est = 'ROYESTXM')

# The family file's summary spending items in each PCE category (production's
# map). HEALTHCQ is taken as its four parts, so that drugs and medical supplies
# count as goods.
CATS = list(
  c_clothing              = c('APPARCQ'),
  c_motor_vehicles        = c('CARTKNCQ', 'CARTKUCQ', 'OTHVEHCQ'),
  c_durables              = c('HOUSEQCQ', 'PETTOYCQ', 'TVRDIOCQ', 'MEDSUPCQ'),
  c_other_nondurables     = c('TOBACCCQ', 'PERSCACQ', 'READCQ', 'PREDRGCQ'),
  c_food_off_premises     = c('FDHOMECQ', 'ALCBEVCQ'),
  c_gasoline              = c('GASMOCQ'),
  c_housing_utilities     = c('OWNDWECQ', 'RENDWECQ', 'NTLGASCQ', 'ELCTRCCQ', 'ALLFULCQ', 'WATRPSCQ'),
  c_other_services_health = c('TELEPHCQ', 'HOUSOPCQ', 'MISCCQ', 'OTHENTCQ', 'MAINRPCQ', 'VRNTLOCQ',
                              'PUBTRACQ', 'FEEADMCQ', 'FDAWAYCQ', 'OTHLODCQ', 'HLTHINCQ', 'MEDSRVCQ',
                              'EDUCACQ', 'LIFINSCQ', 'VEHINSCQ', 'VEHFINCQ'))
PCE_CATS = names(CATS)
EXP_CQ   = unlist(CATS, use.names = FALSE)
stopifnot(!anyDuplicated(EXP_CQ))

# CU_CODE: the person's relationship to the consumer unit's reference person.
REL = c('0' = 'roommate or other non-relative', '1' = 'reference person', '2' = 'spouse',
        '3' = 'child', '4' = 'grandchild', '5' = 'in-law', '6' = 'brother or sister',
        '7' = 'parent', '8' = 'other relative', '9' = 'unmarried partner')

# Prints the number of missing values in each column that has any.
report_na = function(df, label) {
  na = colSums(is.na(df))
  na = na[na > 0]
  if (length(na) == 0) cat(sprintf('  %s: no NA in any column\n', label))
  else { cat(sprintf('  %s: NA counts\n', label)); print(na) }
}

#-------------------------------------------------------------------------------
# Tax units from the shared rules' person file (roster_cex.py): one row per
# return, with its primary filer (role 1), spouse (role 2) and dependents
# (role 3), all of whom are on it. Each unit's member income is the sum over the
# people on it.
#-------------------------------------------------------------------------------
build_tax_units = function(year) {
  memi = read_year('memi', year, c('NEWID', 'MEMBNO', 'AGE', 'SEX', 'CU_CODE', unname(MEMBER_INC))) %>% select(-src) %>%
    mutate(across(all_of(unname(MEMBER_INC)), ~ replace_na(.x, 0)))     # an item not asked means no such income
  mod = fread(file.path(OUT, sprintf('cex_persons_module_%d.csv.gz', year))) %>% as_tibble()
  # The person file holds only the interviews with a positive weight. The others are dropped below in any case.
  m = memi %>% inner_join(mod, by = c('NEWID', 'MEMBNO'))
  cat(sprintf('\n=== %d: %d member rows, %d in the shared rules\' person file, %d matched\n', year, nrow(memi),
              nrow(mod), nrow(m)))
  # AGE may be missing (BLS's '.'; seven children in 2015). Those people are dependents, and only primary filers'
  # and spouses' ages are used.
  stopifnot(nrow(m) == nrow(mod), !anyNA(m %>% select(-AGE)), !anyNA(m$AGE[m$role != 3]))
  heads = m %>% group_by(NEWID, ret_tu) %>% summarise(n_head = sum(role == 1), n_spouse = sum(role == 2), .groups = 'drop')
  stopifnot(all(heads$n_head == 1), all(heads$n_spouse <= 1))
  head_tbl = m %>% filter(role == 1) %>%
    transmute(NEWID, home_tu = ret_tu, age1 = AGE, male1 = as.integer(SEX == 1), cu_code1 = CU_CODE,
              married_mod = married, mfs, any_link_assumed)
  spouse_tbl = m %>% filter(role == 2) %>% transmute(NEWID, home_tu = ret_tu, age2 = AGE, male2 = as.integer(SEX == 1))
  deps = m %>% filter(role == 3) %>% count(NEWID, home_tu = ret_tu, name = 'n_dep')
  inc = m %>% group_by(NEWID, home_tu = ret_tu) %>%
    summarise(tu_size = n(), across(all_of(unname(MEMBER_INC)), sum), .groups = 'drop')
  names(inc)[match(MEMBER_INC, names(inc))] = names(MEMBER_INC)
  tu = head_tbl %>%
    left_join(spouse_tbl, by = c('NEWID', 'home_tu')) %>%
    left_join(deps, by = c('NEWID', 'home_tu')) %>%
    left_join(inc, by = c('NEWID', 'home_tu')) %>%
    mutate(n_dep = replace_na(n_dep, 0L),          # a unit that claims no one has 0 dependents
           married = as.integer(!is.na(age2))) %>%
    group_by(NEWID) %>% mutate(cu_members = sum(tu_size), n_tu_cu = n()) %>% ungroup()
  stopifnot(all(tu$married == tu$married_mod), !anyNA(tu$tu_size))
  cat(sprintf('  units %d (separate returns %d)\n', nrow(tu), sum(tu$mfs)))
  list(tu = tu %>% select(-married_mod), members = m)
}

#-------------------------------------------------------------------------------
# The family file, one row per interview: the weight, the consumer unit's income
# and its spending. Spending items are quarterly: the current quarter (CQ) plus
# the previous quarter (PQ), times 4, is a yearly amount. The weight is
# FINLWT21 / 4 x MO_SCOPE / 3, where MO_SCOPE is the number of the interview's
# three reference months that fall in year N.
#-------------------------------------------------------------------------------
read_fmli = function(year) {
  cols = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_SIZE', 'FINCBTXI', unname(CU_INC),
           EXP_CQ, sub('CQ$', 'PQ', EXP_CQ))
  f = read_year('fmli', year, cols, optional = c('GROCERCQ', 'GROCERPQ', 'FOODCQ', 'FOODPQ', 'TOTEXPCQ', 'TOTEXPPQ'))
  cat(sprintf('  FMLI: %d interviews\n', nrow(f)))
  cat('  population (sum of FINLWT21, millions) by quarterly file:\n')
  print(f %>% group_by(src) %>% summarise(interviews = n(), pop_m = sum(FINLWT21) / 1e6))
  report_na(f %>% select(-any_of(c('GROCERCQ', 'GROCERPQ', 'FOODCQ', 'FOODPQ'))), 'raw FMLI items')
  # Food at home is GROCER in the files that have it (from the 2024 release on). Those files keep FDHOME, empty.
  if ('GROCERCQ' %in% names(f)) {
    g = !is.na(f$GROCERCQ) | !is.na(f$GROCERPQ)
    cat(sprintf('  GROCER present on %d interviews (files %s); FDHOME nonzero on %d of them\n', sum(g),
                paste(unique(f$src[g]), collapse = ' '), sum(g & (f$FDHOMECQ != 0 | f$FDHOMEPQ != 0), na.rm = TRUE)))
    stopifnot(all(f$FDHOMECQ[g] == 0, na.rm = TRUE), all(f$FDHOMEPQ[g] == 0, na.rm = TRUE))
    f = f %>% mutate(FDHOMECQ = if_else(g, GROCERCQ, FDHOMECQ), FDHOMEPQ = if_else(g, GROCERPQ, FDHOMEPQ))
  }
  # Total food and total spending, for the summary only. They are NA where BLS dropped FOOD.
  for (v in c('FOODCQ', 'FOODPQ', 'TOTEXPCQ', 'TOTEXPPQ')) if (!(v %in% names(f))) f[[v]] = NA_real_
  f = f %>% mutate(FOODCQ = (FOODCQ + FOODPQ) * 4, TOTEXPCQ = (TOTEXPCQ + TOTEXPPQ) * 4) %>%
    select(-any_of(c('GROCERCQ', 'GROCERPQ', 'FOODPQ', 'TOTEXPPQ')))
  stopifnot(!anyNA(f[, c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_SIZE', 'FINCBTXI')]),
            !anyDuplicated(f$NEWID))
  f %>%
    # A missing spending or income item means no such spending or income (production's rule).
    mutate(across(all_of(c(unname(CU_INC), EXP_CQ, sub('CQ$', 'PQ', EXP_CQ))), ~ replace_na(.x, 0))) %>%
    mutate(across(all_of(EXP_CQ), ~ (.x + get(sub('CQ$', 'PQ', cur_column()))) * 4)) %>%
    mutate(MO_SCOPE = case_when(
             QINTRVYR == year     & QINTRVMO <= 1 ~ 0L,
             QINTRVYR == year     & QINTRVMO == 2 ~ 1L,
             QINTRVYR == year     & QINTRVMO == 3 ~ 2L,
             QINTRVYR == year     & QINTRVMO >= 4 ~ 3L,
             QINTRVYR == year + 1 & QINTRVMO == 1 ~ 3L,
             QINTRVYR == year + 1 & QINTRVMO == 2 ~ 2L,
             QINTRVYR == year + 1 & QINTRVMO == 3 ~ 1L,
             TRUE                                 ~ 0L),
           weight = FINLWT21 / 4 * MO_SCOPE / 3)
}

#-------------------------------------------------------------------------------
# Each year
#-------------------------------------------------------------------------------
pce = read_csv(settings$paths$raw$pce, show_col_types = FALSE)
summary_rows = list()
item_rows = list()

for (year in YEARS) {
  # 1. Read the units and the family file, and report the interviews found in only one of them.
  b = build_tax_units(year)
  tu = b$tu
  fmli = read_fmli(year)
  only_memi = setdiff(unique(tu$NEWID), fmli$NEWID); only_fmli = setdiff(fmli$NEWID, unique(b$members$NEWID))
  cat(sprintf('  interviews in member file only: %d; in FMLI only: %d\n', length(only_memi), length(only_fmli)))

  # 2. Drop the interviews with weight 0, and keep the weighted total of every family-file item used ($ billions a
  #    year), for comparison across years.
  zero_w = fmli %>% filter(weight == 0)
  cat(sprintf('  interviews with weight 0 dropped: %d of %d (MO_SCOPE 0: %d; FINLWT21 0: %d)\n',
              nrow(zero_w), nrow(fmli), sum(zero_w$MO_SCOPE == 0), sum(zero_w$FINLWT21 == 0)))
  fmli = fmli %>% filter(weight > 0)
  item_rows[[as.character(year)]] = tibble(year = year, item = c(EXP_CQ, 'FOODCQ', 'TOTEXPCQ'),
    billions = map_dbl(c(EXP_CQ, 'FOODCQ', 'TOTEXPCQ'), ~ sum(fmli$weight * fmli[[.x]]) / 1e9))

  # 3. The consumer unit's spending in each category, held at zero from below, and its income items.
  cu_cats = map_dfc(CATS, ~ pmax(0, rowSums(as.matrix(fmli[, .x]))))
  names(cu_cats) = paste0(PCE_CATS, '_cu')
  cu = bind_cols(fmli %>% select(NEWID, weight, FAM_SIZE, FINCBTXI, all_of(unname(CU_INC))), cu_cats)
  names(cu)[match(CU_INC, names(cu))] = paste0(names(CU_INC), '_cu')
  raw_neg = map_int(CATS, ~ sum(rowSums(as.matrix(fmli[, .x])) < 0))
  cat('  interviews with a negative category before the floor:\n'); print(raw_neg[raw_neg > 0])

  # 4. Join the units to their consumer units, and report interviews where the people on the units differ from
  #    FAM_SIZE.
  units = tu %>% inner_join(cu, by = 'NEWID')
  cat(sprintf('  units: %d built, %d in weighted interviews\n', nrow(tu), nrow(units)))
  size_gap = units %>% distinct(NEWID, cu_members, FAM_SIZE) %>% filter(cu_members != FAM_SIZE)
  cat(sprintf('  interviews where the people attached to units differ from FAM_SIZE: %d\n', nrow(size_gap)))
  if (nrow(size_gap) > 0) print(count(size_gap, gap = FAM_SIZE - cu_members))

  # 5. Divide the consumer unit's income among its tax units in proportion to their positive member income
  #    (equally if there is none), and its spending in proportion to the number of people.
  units = units %>%
    group_by(NEWID) %>%
    mutate(
           cu_member_inc = sum(pmax(wages + se_income + social_security + ssi, 0)),
           inc_share = if_else(cu_member_inc > 0, pmax(wages + se_income + social_security + ssi, 0) / cu_member_inc,
                               1 / n())) %>%
    ungroup() %>%
    mutate(tu_share = tu_size / FAM_SIZE,
           dep_filer = 0L,             # dependents who file stay people of the unit that claims them
           one_tu = as.integer(n_tu_cu == 1),
           complete_income = as.integer(FINCBTXI == 100),
           year = year,
           newid = NEWID,
           household_id = NEWID %/% 10,
           record_id = year * 1e10 + NEWID * 100 + home_tu)
  for (v in names(CU_INC)) units[[v]] = units[[paste0(v, '_cu')]] * units$inc_share
  for (v in PCE_CATS) units[[v]] = units[[paste0(v, '_cu')]] * units$tu_share
  units = units %>%
    mutate(total_consumption = rowSums(across(all_of(PCE_CATS))),
           total_consumption_cu = rowSums(across(all_of(paste0(PCE_CATS, '_cu')))))
  stopifnot(all(units$home_tu < 100), max(units$NEWID) * 100 + 99 < 1e10, !anyDuplicated(units$record_id),
            max(units$record_id) < 2^53)

  # 6. The output columns.
  out = units %>%
    transmute(year, record_id, household_id, newid, weight,
              married, mfs, n_dep, age1, male1, age2, male2, dep_filer, any_link_assumed,
              n_tu_cu, one_tu, tu_share, complete_income,
              wages, se_income, social_security, ssi,
              int_div, net_rent, pensions, other_regular, roy_est,
              across(all_of(paste0(names(CU_INC), '_cu'))),
              across(all_of(PCE_CATS)), total_consumption,
              across(all_of(paste0(PCE_CATS, '_cu'))), total_consumption_cu)

  # 7. Checks. Only the spouse's age and sex may be missing, and only on units without a spouse. In every
  #    interview the income shares and the spending shares sum to 1, and the units' spending adds up to the
  #    consumer unit's.
  cat('  output NA check:\n')
  na = colSums(is.na(out)); print(na[na > 0])
  stopifnot(all(is.na(out$age2) == (out$married == 0)), all(is.na(out$male2) == (out$married == 0)),
            !anyNA(out %>% select(-age2, -male2)))
  chk = units %>% group_by(NEWID) %>% summarise(s = sum(tu_share), i = sum(inc_share))
  stopifnot(all(abs(chk$i - 1) < 1e-9))
  cat(sprintf('  interviews whose spending shares do not sum to 1: %d\n', sum(abs(chk$s - 1) > 1e-9)))
  stopifnot(all(abs(chk$s - 1) < 1e-9))
  sp = units %>% group_by(NEWID) %>% summarise(across(all_of(PCE_CATS), sum), across(all_of(paste0(PCE_CATS, '_cu')), first))
  stopifnot(all(abs(as.matrix(sp[, PCE_CATS]) - as.matrix(sp[, paste0(PCE_CATS, '_cu')])) < 1e-6))
  cat('  check passed: in every interview the units\' spending shares sum to 1 and their spending adds to the CU totals\n')

  # 8. Write the units, and in the target year a table of spending by the primary filer's age.
  fwrite(out, file.path(OUT, sprintf('cex_units_%d.csv.gz', year)))
  if (year == settings$years$target) {
    age = out %>% group_by(age1) %>%
      summarise(rows = n(), weighted_units = sum(weight),
                across(all_of(c(PCE_CATS, 'total_consumption')), ~ sum(weight * .x))) %>% arrange(age1)
    fwrite(age, file.path(OUT, sprintf('cex_age_check_%d.csv', year)))
    cat(sprintf('  wrote age check: %d ages, %.2f m units\n', nrow(age), sum(age$weighted_units) / 1e6))
  }

  # 9. Print the year's summary: counts of units, who heads the units other than the reference person's, and
  #    income and spending totals against PCE.
  w = out$weight
  cuw = cu$weight
  cat(sprintf('\n  SUMMARY %d: interviews %d, consumer units (CUID) %d, unit rows %d\n', year,
              n_distinct(out$newid), n_distinct(out$household_id), nrow(out)))
  cat(sprintf('  weighted consumer units %.2f m; weighted tax units %.2f m; separate returns %.2f m; married %.2f m\n',
              sum(cuw) / 1e6, sum(w) / 1e6, sum(w * out$mfs) / 1e6, sum(w * out$married) / 1e6))
  one = out %>% distinct(newid, weight, one_tu)
  cat(sprintf('  share of CU weight in one-unit CUs %.3f; share of unit rows in one-unit CUs %.3f; of weighted units %.3f\n',
              sum(one$weight * one$one_tu) / sum(one$weight), mean(out$one_tu), sum(w * out$one_tu) / sum(w)))
  cat(sprintf('  units with primary filer aged 17 or under: %d rows, %.2f m weighted\n',
              sum(out$age1 <= 17), sum(w[out$age1 <= 17]) / 1e6))
  cat(sprintf('  complete income reporters: %.3f of weighted units\n', sum(w * out$complete_income) / sum(w)))
  rel = units %>% filter(n_tu_cu > 1, cu_code1 != 1) %>%
    mutate(rel = REL[as.character(cu_code1)]) %>%
    group_by(rel) %>% summarise(rows = n(), weighted_m = sum(weight) / 1e6) %>%
    mutate(share = weighted_m / sum(weighted_m)) %>% arrange(desc(weighted_m))
  cat('  heads of units other than the reference person\'s unit, by relationship to the reference person (millions):\n')
  print(rel)
  cat('  number of units per CU (weighted CUs, millions):\n')
  print(units %>% distinct(NEWID, weight, n_tu_cu) %>% group_by(n_tu_cu) %>% summarise(cu_m = sum(weight) / 1e6))

  tot = function(x) sum(w * x) / 1e9
  p = pce %>% filter(year == !!year)
  row = tibble(year = year,
               item = c('wages', 'se_income', 'int_div', 'social_security', 'pensions', 'ss_plus_pensions',
                        PCE_CATS, 'total_consumption'),
               cex_billions = c(tot(out$wages), tot(out$se_income), tot(out$int_div), tot(out$social_security),
                                tot(out$pensions), tot(out$social_security + out$pensions),
                                map_dbl(PCE_CATS, ~ tot(out[[.x]])), tot(out$total_consumption)),
               cu_level_billions = c(NA, NA, sum(cuw * cu$int_div_cu) / 1e9, NA, sum(cuw * cu$pensions_cu) / 1e9, NA,
                                     map_dbl(PCE_CATS, ~ sum(cuw * cu[[paste0(.x, '_cu')]]) / 1e9), NA),
               pce_billions = c(rep(NA, 6),
                                if (nrow(p) == 1) unlist(p[1, PCE_CATS]) else rep(NA, length(PCE_CATS)),
                                if (nrow(p) == 1) sum(unlist(p[1, PCE_CATS])) else NA)) %>%
    mutate(cex_over_pce = cex_billions / pce_billions)
  print(row, n = 50)
  summary_rows[[as.character(year)]] = row
}

# The family-file items by year, and the summary of every year
items = bind_rows(item_rows)
fwrite(items, file.path(OUT, 'cex_items_by_year.csv'))
cat('\nweighted totals of FMLI items by year ($ billions):\n')
print(items %>% pivot_wider(names_from = year, values_from = billions) %>% mutate(across(-item, ~ round(.x, 1))), n = 60)
summary = bind_rows(summary_rows)
fwrite(summary, file.path(OUT, 'cex_summary.csv'))
cat('\nwrote', file.path(OUT, 'cex_summary.csv'), '\n')
