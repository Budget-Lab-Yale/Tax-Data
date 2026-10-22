#-------------------------------------------------------------------------------
# build_cex.R: the CEX interview survey, survey years 2015, 2017, 2019, 2021,
# 2023 and 2024, split into tax units, with each unit's income and its share of
# the consumer unit's spending in the 8 PCE categories.
#
# Follows production's reader (src/cex.R, build_cex_training) with four
# deliberate differences:
#   - nominal dollars of the survey year (production deflates to 2017);
#   - each year's weights sum to that year's population (production divides
#     by the number of years pooled);
#   - every consumer unit is kept, with complete_income = 1 where BLS imputed
#     none of the unit's income (FINCBTXI == 100); production keeps only those;
#   - a dependent who has a tax unit of their own (a dependent filer) is
#     counted in the claiming unit's n_dep, and their head count is added to
#     the claiming unit's spending share; their own row is then dropped, and
#     their own income goes nowhere (it belongs on their own return, which is
#     not in this file). Production folds the person, income included, into
#     the claiming unit. The consumer unit's interest, rent, pension and other
#     unit-level income is split among the remaining units only.
#
# Sources. A survey year N uses five quarterly files of interviews: Q1, Q2,
# Q3 and Q4 of N, and Q1 of N+1 (whose interviews still cover the last months
# of N). Releases up to 2019 hold all five, the first as {yy}1x. From 2020 a
# release holds Q2 of N to Q1 of N+1, so Q1 of N is read from the previous
# release's folder (2021's 211 files from 2020/, and so on). Production's
# read_cex reads only four files and so misses Q1 of N.
# The member files (MEMI) give each person's tax unit (TAX_UNIT), role in it
# (TU_CODE: 1 primary filer, 2 spouse, 3 dependent) and, for dependents, the
# unit that claims them (TU_DPNDT; in some years it also names the person's
# own unit for non-dependents, so a pointer counts as a claim only when it
# names a different unit). Two repairs from production: a unit with no
# primary filer gets the reference person as primary filer, and a unit holding
# the reference person and his or her spouse but no TU_CODE 2 gets the spouse
# as spouse. (Production applies the second repair even when the spouse sits
# in a different unit; here it is applied only when the spouse shares the
# reference person's unit.) BLS codes many reference persons and spouses,
# mostly elderly couples, as dependents of their own unit; these repairs undo
# that. One more repair, not in production: a unit still without a primary
# filer gets its oldest member as primary filer. Mostly these are dependents
# with a unit number of their own who are claimed by another unit (NTAXI gives
# them FILESTAT 8 and computes their own tax): dependent filers, dep_filer = 1.
# In 2024 they are mostly roommates and partners coded as dependents of their
# own unit.
#
# Weights. FINLWT21 / 4 x MO_SCOPE / 3. MO_SCOPE is the number of the
# interview's three reference months that fall in year N, so the five files
# together represent the year's population once. Interviews with weight 0
# (January interviews of N, which cover only months of N-1) are dropped.
#
# Ids. record_id = year x 1e10 + NEWID x 100 + tax unit number (one 2017 interview
# has a unit 10), unique across all
# files (the interviews of Q1 of N+1 sit in both year N's and year N+1's file).
# household_id = NEWID %/% 10, the consumer unit across its interviews and years.
#
# Income, nominal annual dollars as reported (last 12 months).
#   Member items, summed over the people attached to the unit (filer, spouse,
#   dependents living in it): wages SALARYXM, se_income SEMPFRMM,
#   social_security SOCRRXM, ssi SSIXM. A missing member item means BLS did
#   not ask it (no such income) and is 0, as in production.
#   Consumer-unit items, split across the CU's tax units in proportion to each
#   unit's positive member income, or equally when no unit has any (production's
#   rule): int_div INTRDVXM, net_rent NETRENTM, pensions RETSURVM,
#   other_regular OTHREGXM, roy_est ROYESTXM. The CU totals carry suffix _cu.
#
# Consumption, nominal annual dollars. Each FMLI summary item is CQ + PQ (the
# two parts of the three-month reference period) times 4, and the items map
# to the 8 categories as in production. Each category is floored at 0 for the
# consumer unit (refunds can make an item negative) and then split across tax
# units by tu_share = tu_size / FAM_SIZE, where tu_size counts the people
# attached to the unit. From the 2024 release (files 242 on) BLS renamed food
# at home FDHOMECQ/PQ to GROCERCQ/PQ; GROCER is used in any file that has it.
#
# Paths and years from settings.json ("paths": raw cex; "cex": years); the settings file is
# REBUILD_SETTINGS if set (run.sbatch sets it), else the pipeline's own.
# Writes {out}/data/cex_units_{year}.csv.gz, one row per tax unit per
# interview; cex_summary.csv (weighted totals beside NIPA PCE);
# cex_items_by_year.csv (weighted total of every FMLI item used, by year);
# cex_age_check_<target year>.csv (weighted units and spending by the primary
# filer's age). Prints diagnostics: repairs, NA counts, the NTAXI comparison.
# Run from the repository root through Slurm.
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

# The five quarterly files of survey year N: Q1 of N ({yy}1x in release N up to 2019, {yy}1 in
# release N-1 from 2020), Q2-Q4 of N and Q1 of N+1 in release N.
year_files = function(prefix, year) {
  yy = year %% 100
  q1 = if (year <= 2019) file.path(year, sprintf('%s%02d1x.csv', prefix, yy))
       else file.path(year - 1, sprintf('%s%02d1.csv', prefix, yy))
  c(q1, file.path(year, sprintf('%s%02d%d.csv', prefix, yy, 2:4)),
    file.path(year, sprintf('%s%02d1.csv', prefix, (yy + 1) %% 100)))
}

# Reads a year's files. `need` columns must be in every file; `optional` ones are read where
# present. A missing file stops the run, unless optional_files (NTAXI), where it is reported.
read_year = function(prefix, year, need, optional = character(0), optional_files = FALSE, ...) {
  paths = file.path(CEX_RAW, year_files(prefix, year))
  have = file.exists(paths)
  if (!all(have)) {
    msg = sprintf('%s %d: missing file(s) %s', prefix, year, paste(paths[!have], collapse = ', '))
    if (!optional_files) stop(msg)
    cat('  ', msg, '\n')
  }
  map_dfr(paths[have], function(p) {
    h = toupper(names(fread(p, nrows = 0)))
    miss = setdiff(need, h)
    if (length(miss) > 0) stop(sprintf('%s lacks %s', p, paste(miss, collapse = ', ')))
    # '.' is BLS's missing-value mark in some files (AGE in 2015 Q3-Q4); read it as NA and report it
    d = fread(p, select = c(need, intersect(optional, h)), na.strings = c('', 'NA', '.'), ...) %>% as_tibble()
    d$src = basename(p)
    d
  })
}

MEMBER_INC = c(wages = 'SALARYXM', se_income = 'SEMPFRMM', social_security = 'SOCRRXM', ssi = 'SSIXM')
CU_INC     = c(int_div = 'INTRDVXM', net_rent = 'NETRENTM', pensions = 'RETSURVM',
               other_regular = 'OTHREGXM', roy_est = 'ROYESTXM')

# FMLI summary items in each category (production's map; HEALTHCQ is taken as
# its four parts so drugs and medical supplies go to goods).
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

# CU_CODE: the person's relationship to the CU's reference person.
REL = c('0' = 'roommate or other non-relative', '1' = 'reference person', '2' = 'spouse',
        '3' = 'child', '4' = 'grandchild', '5' = 'in-law', '6' = 'brother or sister',
        '7' = 'parent', '8' = 'other relative', '9' = 'unmarried partner')

report_na = function(df, label) {
  na = colSums(is.na(df))
  na = na[na > 0]
  if (length(na) == 0) cat(sprintf('  %s: no NA in any column\n', label))
  else { cat(sprintf('  %s: NA counts\n', label)); print(na) }
}

#-------------------------------------------------------------------------------
# Tax units from the member file
#-------------------------------------------------------------------------------
build_tax_units = function(year) {
  memi = read_year('memi', year, c('NEWID', 'MEMBNO', 'AGE', 'SEX', 'CU_CODE', 'TAX_UNIT', 'TU_CODE', 'TU_DPNDT',
                                    unname(MEMBER_INC)),
                   colClasses = list(character = 'TU_DPNDT')) %>% select(-src)
  cat(sprintf('\n=== %d: %d member rows, %d interviews\n', year, nrow(memi), n_distinct(memi$NEWID)))
  report_na(memi, 'raw member file')
  cat('  SEX, TU_CODE and CU_CODE values:\n')
  print(table(memi$SEX, useNA = 'ifany')); print(table(memi$TU_CODE, useNA = 'ifany')); print(table(memi$CU_CODE, useNA = 'ifany'))
  cat(sprintf('  oldest age %d\n', max(memi$AGE, na.rm = TRUE)))   # NA ages listed next
  if (anyNA(memi$AGE)) { cat('  people with age missing:\n'); print(memi %>% filter(is.na(AGE))) }
  stopifnot(!anyNA(memi[, c('NEWID', 'MEMBNO', 'SEX', 'CU_CODE', 'TAX_UNIT', 'TU_CODE')]),
            all(memi$SEX %in% 1:2), all(memi$TU_CODE %in% 1:3), !anyDuplicated(memi[, c('NEWID', 'MEMBNO')]))
  cat('  TU_DPNDT values:\n'); print(table(memi$TU_DPNDT, useNA = 'ifany'))

  m = memi %>%
    mutate(across(all_of(unname(MEMBER_INC)), ~ replace_na(.x, 0))) %>%
    group_by(NEWID, TAX_UNIT) %>%
    mutate(no_payer = !any(TU_CODE == 1), has_ref = any(CU_CODE == 1),
           no_spouse = !any(TU_CODE == 2) & any(CU_CODE == 2)) %>%
    ungroup() %>%
    mutate(fix_payer  = no_payer & CU_CODE == 1,
           fix_spouse = no_spouse & CU_CODE == 2 & has_ref & TU_CODE != 2,
           prod_spouse_only = no_spouse & CU_CODE == 2 & !has_ref & TU_CODE != 2,
           TU_CODE = case_when(fix_payer ~ 1L, fix_spouse ~ 2L, TRUE ~ as.integer(TU_CODE)))
  cat(sprintf('  repairs: reference person made primary filer %d; spouse made spouse %d; production would also recode %d spouses in a unit without the reference person (not done here)\n',
              sum(m$fix_payer), sum(m$fix_spouse), sum(m$prod_spouse_only)))

  # A unit still without a primary filer (no reference person in it) gets its oldest member as
  # primary filer. Two kinds: (a) a dependent listed in a unit of their own and claimed by another
  # unit -- a dependent filer (NTAXI gives these units FILESTAT 8 and computes their own tax);
  # (b) in 2024, a roommate or partner coded as a dependent of their own unit.
  m = m %>%
    mutate(claim = suppressWarnings(as.integer(TU_DPNDT)),
           claimed_elsewhere = !is.na(claim) & claim != TAX_UNIT) %>%
    group_by(NEWID, TAX_UNIT) %>%
    mutate(fix_promote = !any(TU_CODE == 1) &
             row_number() == order(-AGE, MEMBNO)[1]) %>%
    ungroup() %>%
    mutate(TU_CODE = if_else(fix_promote, 1L, TU_CODE))
  cat('  oldest member made primary filer of a unit with none, by relationship (CU_CODE) and claimed by another unit:\n')
  print(m %>% filter(fix_promote) %>% count(CU_CODE, claimed_elsewhere))

  m = m %>%
    mutate(# the unit the person is attached to: a dependent claimed by another unit moves to it
           home_tu = if_else(TU_CODE == 3 & claimed_elsewhere, claim, as.integer(TAX_UNIT)),
           # the unit that counts the person as a dependent (dependent filers included)
           claimer_tu = case_when(TU_CODE == 3 ~ home_tu,
                                  claimed_elsewhere ~ claim,
                                  TRUE ~ NA_integer_))
  repaired = m %>% filter(fix_payer | fix_spouse | fix_promote) %>% distinct(NEWID, home_tu) %>% mutate(repaired = TRUE)
  stopifnot(all(is.na(m$TU_DPNDT) | m$TU_DPNDT %in% c('', 'B') | !is.na(m$claim)))
  cat('  claimed by another unit, by role (1 filer, 2 spouse, 3 dependent):\n')
  print(table(role = m$TU_CODE, claimed_elsewhere = m$claimed_elsewhere))

  # Units: one per (interview, home unit). Each needs one primary filer.
  heads = m %>% group_by(NEWID, home_tu) %>%
    summarise(n_head = sum(TU_CODE == 1), n_spouse = sum(TU_CODE == 2), n = n(), .groups = 'drop')
  cat('  units by number of primary filers and spouses:\n'); print(count(heads, n_head, n_spouse))
  headless = heads %>% filter(n_head == 0)
  if (nrow(headless) > 0) {
    cat(sprintf('  %d units have no primary filer (%d people); their people:\n', nrow(headless), sum(headless$n)))
    print(m %>% semi_join(headless, by = c('NEWID', 'home_tu')) %>%
            count(CU_CODE, TU_CODE, TU_DPNDT, TAX_UNIT, home_tu))
  }
  # After the repairs every unit has exactly one primary filer, and every claim names a unit that exists.
  units_ok = heads %>% filter(n_head == 1)
  dangling = m %>% filter(!is.na(claimer_tu)) %>% anti_join(units_ok, by = c('NEWID', 'claimer_tu' = 'home_tu'))
  cat(sprintf('  dependents whose claiming unit has no primary filer or does not exist: %d\n', nrow(dangling)))
  stopifnot(all(heads$n_head == 1), all(heads$n_spouse <= 1), nrow(dangling) == 0)

  head_tbl = m %>% filter(TU_CODE == 1) %>%
    transmute(NEWID, home_tu, age1 = AGE, male1 = as.integer(SEX == 1), cu_code1 = CU_CODE,
              dep_filer = as.integer(claimed_elsewhere), claim_tu = if_else(claimed_elsewhere, claim, NA_integer_))
  spouse_tbl = m %>% filter(TU_CODE == 2) %>%
    transmute(NEWID, home_tu, age2 = AGE, male2 = as.integer(SEX == 1), spouse_claimed = claimed_elsewhere)
  cat(sprintf('  spouses claimed as dependents by another unit: %d\n', sum(spouse_tbl$spouse_claimed)))
  deps = m %>% filter(!is.na(claimer_tu)) %>% count(NEWID, claimer_tu, name = 'n_dep')
  inc = m %>% group_by(NEWID, home_tu) %>%
    summarise(tu_size = n(), across(all_of(unname(MEMBER_INC)), sum), .groups = 'drop')
  names(inc)[match(MEMBER_INC, names(inc))] = names(MEMBER_INC)

  tu = head_tbl %>%
    left_join(spouse_tbl %>% select(-spouse_claimed), by = c('NEWID', 'home_tu')) %>%
    left_join(deps, by = c('NEWID', 'home_tu' = 'claimer_tu')) %>%
    left_join(inc, by = c('NEWID', 'home_tu')) %>%
    mutate(n_dep = replace_na(n_dep, 0L),          # no one claimed: 0 dependents
           married = as.integer(!is.na(age2))) %>%
    left_join(repaired, by = c('NEWID', 'home_tu')) %>%
    mutate(repaired = replace_na(repaired, FALSE)) %>%   # no member recoded
    group_by(NEWID) %>% mutate(cu_members = sum(tu_size), n_tu_cu = n(),
                               has_ref_unit = any(cu_code1 == 1)) %>% ungroup()
  stopifnot(nrow(tu) == nrow(units_ok), !anyNA(tu$tu_size))
  cat(sprintf('  interviews whose reference person does not head a unit: %d\n',
              n_distinct(tu$NEWID[!tu$has_ref_unit])))
  list(tu = tu, members = m)
}

#-------------------------------------------------------------------------------
# Interview-level file: weights, CU income, spending
#-------------------------------------------------------------------------------
read_fmli = function(year) {
  cols = c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_SIZE', 'FINCBTXI', unname(CU_INC),
           EXP_CQ, sub('CQ$', 'PQ', EXP_CQ))
  f = read_year('fmli', year, cols, optional = c('GROCERCQ', 'GROCERPQ', 'FOODCQ', 'FOODPQ', 'TOTEXPCQ', 'TOTEXPPQ'))
  cat(sprintf('  FMLI: %d interviews\n', nrow(f)))
  cat('  population (sum of FINLWT21, millions) by quarterly file:\n')
  print(f %>% group_by(src) %>% summarise(interviews = n(), pop_m = sum(FINLWT21) / 1e6))
  report_na(f %>% select(-any_of(c('GROCERCQ', 'GROCERPQ', 'FOODCQ', 'FOODPQ'))), 'raw FMLI items')
  # Food at home: GROCER in files that have it (2024 release on), where FDHOME is kept but empty.
  if ('GROCERCQ' %in% names(f)) {
    g = !is.na(f$GROCERCQ) | !is.na(f$GROCERPQ)
    cat(sprintf('  GROCER present on %d interviews (files %s); FDHOME nonzero on %d of them\n', sum(g),
                paste(unique(f$src[g]), collapse = ' '), sum(g & (f$FDHOMECQ != 0 | f$FDHOMEPQ != 0), na.rm = TRUE)))
    stopifnot(all(f$FDHOMECQ[g] == 0, na.rm = TRUE), all(f$FDHOMEPQ[g] == 0, na.rm = TRUE))
    f = f %>% mutate(FDHOMECQ = if_else(g, GROCERCQ, FDHOMECQ), FDHOMEPQ = if_else(g, GROCERPQ, FDHOMEPQ))
  }
  for (v in c('FOODCQ', 'FOODPQ', 'TOTEXPCQ', 'TOTEXPPQ')) if (!(v %in% names(f))) f[[v]] = NA_real_
  f = f %>% mutate(FOODCQ = (FOODCQ + FOODPQ) * 4, TOTEXPCQ = (TOTEXPCQ + TOTEXPPQ) * 4) %>%   # NA where BLS dropped FOOD
    select(-any_of(c('GROCERCQ', 'GROCERPQ', 'FOODPQ', 'TOTEXPPQ')))
  stopifnot(!anyNA(f[, c('NEWID', 'FINLWT21', 'QINTRVMO', 'QINTRVYR', 'FAM_SIZE', 'FINCBTXI')]),
            !anyDuplicated(f$NEWID))
  f %>%
    # A missing spending or CU income item is no such spending or income (production's rule).
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
# Build each year
#-------------------------------------------------------------------------------
pce = read_csv(settings$paths$raw$pce, show_col_types = FALSE)
summary_rows = list()
item_rows = list()

for (year in YEARS) {
  b = build_tax_units(year)
  tu = b$tu
  fmli = read_fmli(year)

  # every member-file interview has an FMLI record, and the reverse
  only_memi = setdiff(unique(tu$NEWID), fmli$NEWID); only_fmli = setdiff(fmli$NEWID, unique(b$members$NEWID))
  cat(sprintf('  interviews in member file only: %d; in FMLI only: %d\n', length(only_memi), length(only_fmli)))

  zero_w = fmli %>% filter(weight == 0)
  cat(sprintf('  interviews with weight 0 dropped: %d of %d (MO_SCOPE 0: %d; FINLWT21 0: %d)\n',
              nrow(zero_w), nrow(fmli), sum(zero_w$MO_SCOPE == 0), sum(zero_w$FINLWT21 == 0)))
  fmli = fmli %>% filter(weight > 0)
  # weighted total of every FMLI item used (annual, $ billions), for year-to-year comparison
  item_rows[[as.character(year)]] = tibble(year = year, item = c(EXP_CQ, 'FOODCQ', 'TOTEXPCQ'),
    billions = map_dbl(c(EXP_CQ, 'FOODCQ', 'TOTEXPCQ'), ~ sum(fmli$weight * fmli[[.x]]) / 1e9))

  cu_cats = map_dfc(CATS, ~ pmax(0, rowSums(as.matrix(fmli[, .x]))))
  names(cu_cats) = paste0(PCE_CATS, '_cu')
  cu = bind_cols(fmli %>% select(NEWID, weight, FAM_SIZE, FINCBTXI, all_of(unname(CU_INC))), cu_cats)
  names(cu)[match(CU_INC, names(cu))] = paste0(names(CU_INC), '_cu')

  # this must be computed before the floors would matter: check the floor's bite at CU level
  raw_neg = map_int(CATS, ~ sum(rowSums(as.matrix(fmli[, .x])) < 0))
  cat('  interviews with a negative category before the floor:\n'); print(raw_neg[raw_neg > 0])

  units = tu %>% inner_join(cu, by = 'NEWID')
  cat(sprintf('  units: %d built, %d in weighted interviews\n', nrow(tu), nrow(units)))
  size_gap = units %>% distinct(NEWID, cu_members, FAM_SIZE) %>% filter(cu_members != FAM_SIZE)
  cat(sprintf('  interviews where the people attached to units differ from FAM_SIZE: %d\n', nrow(size_gap)))
  if (nrow(size_gap) > 0) print(count(size_gap, gap = FAM_SIZE - cu_members))

  # Dependent filers: their head count joins the claiming unit's spending share and their row is
  # dropped. Their own income is not moved.
  dfil = units %>% filter(dep_filer == 1)
  add = dfil %>% group_by(NEWID, home_tu = claim_tu) %>% summarise(df_size = sum(tu_size), n_df = n(), .groups = 'drop')
  stopifnot(!anyNA(dfil$claim_tu),
            nrow(semi_join(add, dfil, by = c('NEWID', 'home_tu'))) == 0,          # no claimant is itself a dependent filer
            nrow(anti_join(add, units, by = c('NEWID', 'home_tu'))) == 0)         # every claimant exists
  cat(sprintf('  dependent filers folded into their claiming unit: %d rows, %.2f m weighted; their own wages dropped %.1f bn\n',
              nrow(dfil), sum(dfil$weight) / 1e6, sum(dfil$weight * dfil$wages) / 1e9))
  units = units %>% filter(dep_filer == 0) %>%
    left_join(add, by = c('NEWID', 'home_tu')) %>%
    mutate(df_size = replace_na(df_size, 0L), n_df = replace_na(n_df, 0L)) %>%   # 0: claims no dependent filer
    group_by(NEWID) %>% mutate(n_tu_cu = n()) %>% ungroup()
  stopifnot(all(units$n_dep >= units$n_df), sum(units$n_df) == nrow(dfil))

  units = units %>%
    group_by(NEWID) %>%
    mutate(
           cu_member_inc = sum(pmax(wages + se_income + social_security + ssi, 0)),
           inc_share = if_else(cu_member_inc > 0, pmax(wages + se_income + social_security + ssi, 0) / cu_member_inc,
                               1 / n())) %>%
    ungroup() %>%
    mutate(tu_share = (tu_size + df_size) / FAM_SIZE,
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

  out = units %>%
    transmute(year, record_id, household_id, newid, weight,
              married, n_dep, age1, male1, age2, male2, dep_filer,
              n_tu_cu, one_tu, tu_share, complete_income,
              wages, se_income, social_security, ssi,
              int_div, net_rent, pensions, other_regular, roy_est,
              across(all_of(paste0(names(CU_INC), '_cu'))),
              across(all_of(PCE_CATS)), total_consumption,
              across(all_of(paste0(PCE_CATS, '_cu'))), total_consumption_cu)

  # NA check: only the spouse's age and sex may be missing, and only on single units
  cat('  output NA check:\n')
  na = colSums(is.na(out)); print(na[na > 0])
  stopifnot(all(is.na(out$age2) == (out$married == 0)), all(is.na(out$male2) == (out$married == 0)),
            !anyNA(out %>% select(-age2, -male2)))
  # shares across a CU's units: spending shares sum to 1 where people match FAM_SIZE; income shares always
  chk = units %>% group_by(NEWID) %>% summarise(s = sum(tu_share), i = sum(inc_share))
  stopifnot(all(abs(chk$i - 1) < 1e-9))
  cat(sprintf('  interviews whose spending shares do not sum to 1: %d\n', sum(abs(chk$s - 1) > 1e-9)))
  stopifnot(all(abs(chk$s - 1) < 1e-9))
  sp = units %>% group_by(NEWID) %>% summarise(across(all_of(PCE_CATS), sum), across(all_of(paste0(PCE_CATS, '_cu')), first))
  stopifnot(all(abs(as.matrix(sp[, PCE_CATS]) - as.matrix(sp[, paste0(PCE_CATS, '_cu')])) < 1e-6))
  cat('  check passed: in every interview the units\' spending shares sum to 1 and their spending adds to the CU totals\n')

  fwrite(out, file.path(OUT, sprintf('cex_units_%d.csv.gz', year)))
  if (year == settings$years$target) {
    age = out %>% group_by(age1) %>%
      summarise(rows = n(), weighted_units = sum(weight),
                across(all_of(c(PCE_CATS, 'total_consumption')), ~ sum(weight * .x))) %>% arrange(age1)
    fwrite(age, file.path(OUT, sprintf('cex_age_check_%d.csv', year)))
    cat(sprintf('  wrote age check: %d ages, %.2f m units\n', nrow(age), sum(age$weighted_units) / 1e6))
  }

  #-----------------------------------------------------------------------------
  # Diagnostics for the report
  #-----------------------------------------------------------------------------
  w = out$weight
  cuw = cu$weight
  cat(sprintf('\n  SUMMARY %d: interviews %d, consumer units (CUID) %d, unit rows %d\n', year,
              n_distinct(out$newid), n_distinct(out$household_id), nrow(out)))
  cat(sprintf('  weighted consumer units %.2f m; weighted tax units %.2f m; dependent filers %.2f m; married %.2f m\n',
              sum(cuw) / 1e6, sum(w) / 1e6, sum(w * out$dep_filer) / 1e6, sum(w * out$married) / 1e6))
  one = out %>% distinct(newid, weight, one_tu)
  cat(sprintf('  share of CU weight in one-unit CUs %.3f; share of unit rows in one-unit CUs %.3f; of weighted units %.3f\n',
              sum(one$weight * one$one_tu) / sum(one$weight), mean(out$one_tu), sum(w * out$one_tu) / sum(w)))
  cat(sprintf('  units with primary filer aged 17 or under: %d rows, %.2f m weighted\n',
              sum(out$age1 <= 17), sum(w[out$age1 <= 17]) / 1e6))
  cat(sprintf('  complete income reporters: %.3f of weighted units\n', sum(w * out$complete_income) / sum(w)))
  # who heads the second and later units in multi-unit CUs
  rel = units %>% filter(n_tu_cu > 1, cu_code1 != 1) %>%
    mutate(rel = REL[as.character(cu_code1)]) %>%
    group_by(rel) %>% summarise(rows = n(), weighted_m = sum(weight) / 1e6, dep_filer = sum(weight * dep_filer) / 1e6) %>%
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

  #-----------------------------------------------------------------------------
  # NTAXI comparison (2023 only: the only release with the tax-unit file)
  #-----------------------------------------------------------------------------
  cat('\n  NTAXI comparison\n')
  ntaxi = read_year('ntaxi', year, c('NEWID', 'TAX_UNIT', 'DEPCNT', 'FILESTAT'), optional = c('AGE_TP', 'AGE_SP'),
                    optional_files = TRUE)
  if (nrow(ntaxi) > 0) {
    for (v in c('AGE_TP', 'AGE_SP')) if (!(v %in% names(ntaxi))) ntaxi[[v]] = NA_integer_   # not in the 2015-2017 files
    stopifnot(!anyDuplicated(ntaxi[, c('NEWID', 'TAX_UNIT')]))
    # compare only interviews that have NTAXI records (whole quarterly files can be missing)
    ours = tu %>% filter(NEWID %in% ntaxi$NEWID) %>%
      transmute(NEWID, TAX_UNIT = home_tu, n_dep, married, age1, age2, dep_filer, repaired)
    cat(sprintf('  NTAXI %d: files %s; %d units in NTAXI, %d built here for the same interviews; FILESTAT:\n', year,
                paste(unique(ntaxi$src), collapse = ' '), nrow(ntaxi), nrow(ours)))
    print(table(ntaxi$FILESTAT, useNA = 'ifany'))
    both = inner_join(ours, ntaxi, by = c('NEWID', 'TAX_UNIT'))
    cat(sprintf('  matched on interview and unit number: %d; ours only %d; NTAXI only %d\n', nrow(both),
                nrow(anti_join(ours, ntaxi, by = c('NEWID', 'TAX_UNIT'))),
                nrow(anti_join(ntaxi, ours, by = c('NEWID', 'TAX_UNIT')))))
    cat(sprintf('  agreement: dependents %.3f; married (FILESTAT 2) %.3f; primary age %.3f; spouse age (joint) %.3f\n',
                mean(both$n_dep == both$DEPCNT), mean(both$married == (both$FILESTAT == 2)),
                mean(both$age1 == both$AGE_TP),
                mean((both$age2 == both$AGE_SP)[both$married == 1 & both$FILESTAT == 2])))   # NaN/NA where NTAXI has no ages
    nr = both %>% filter(!repaired)
    cat(sprintf('  units with no recoded member: %d; dependents agree on %.3f of them; married %.3f\n', nrow(nr),
                mean(nr$n_dep == nr$DEPCNT), mean(nr$married == (nr$FILESTAT == 2))))
    cat('  dependent count agreement by whether a member was recoded:\n')
    print(both %>% group_by(repaired) %>% summarise(units = n(), agree = mean(n_dep == DEPCNT)))
    cat('  dependent filers (ours) by NTAXI FILESTAT:\n'); print(count(both, dep_filer, FILESTAT))
    cnt = full_join(count(ours, NEWID, name = 'ours'), count(ntaxi, NEWID, name = 'ntaxi'), by = 'NEWID')
    cat(sprintf('  units per interview agree: %.3f of %d interviews\n',
                mean(cnt$ours == cnt$ntaxi, na.rm = FALSE), nrow(cnt)))
    print(count(cnt, ours, ntaxi) %>% filter(is.na(ours) | is.na(ntaxi) | ours != ntaxi))
    cat('  dependent count disagreements (ours, NTAXI):\n')
    print(both %>% filter(n_dep != DEPCNT) %>% count(n_dep, DEPCNT, sort = TRUE) %>% head(10))
    cat('  married disagreements (ours, FILESTAT):\n')
    print(both %>% filter(married != (FILESTAT == 2)) %>% count(married, FILESTAT, sort = TRUE))
  }
}

items = bind_rows(item_rows)
fwrite(items, file.path(OUT, 'cex_items_by_year.csv'))
cat('\nweighted totals of FMLI items by year ($ billions):\n')
print(items %>% pivot_wider(names_from = year, values_from = billions) %>% mutate(across(-item, ~ round(.x, 1))), n = 60)
summary = bind_rows(summary_rows)
fwrite(summary, file.path(OUT, 'cex_summary.csv'))
cat('\nwrote', file.path(OUT, 'cex_summary.csv'), '\n')
