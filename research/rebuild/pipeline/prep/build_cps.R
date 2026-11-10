#-------------------------------------------------------------------------------
# The CPS ASEC in tax units: one row per unit, for each tax year in settings
# cps.years. The ASEC of March of year Y+1 asks about tax year Y.
#
# The filing stage (filing.py and filing_requirement.R) runs first. It gives the
# units, the couples split into separate returns, each return's filing status,
# its filing requirement and its chance of filing. This script adds each unit's
# income items, each adult's work, overtime pay and Social Security number
# status, the dependents' ages, child care spending and the state. A
# dependent's own return is dropped: the dependent counts in the claiming
# unit's n_dep, and their income goes nowhere. In a couple of a man and a woman
# person 1 is the man; in a same-sex couple person 1 is the older partner.
# Writes data/cps_units_<year>.csv.gz and check files by person. Needs ipumsr
# (REBUILD_R_LIBS). FEATURES.md, "The CPS", has the method. Run from the
# repository root.
#-------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(ipumsr); library(stringr); library(dplyr); library(yaml); library(jsonlite)
})
options(width = 200)

settings = fromJSON(Sys.getenv('REBUILD_SETTINGS', 'research/rebuild/pipeline/settings.json'))
OUT = file.path(Sys.getenv('REBUILD_OUT', settings$paths$out), 'data')
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
args = commandArgs(trailingOnly = TRUE)
YEARS = if (length(args)) as.integer(args) else as.integer(settings$cps$years)
stopifnot(all(YEARS %in% settings$cps$years))

source('src/nonfilers/state_weights.R')     # raw_data_root(), age_band()
source('src/nonfilers/asec_tax_units.R')    # read_asec(), the income cleaning, build_asec_unit_table()
source('research/rebuild/pipeline/prep/tax_units_cbo.R')
source('research/rebuild/pipeline/prep/overtime_cps.R')  # cps_overtime(): each person's overtime pay, by Tedeschi's rules
# The filing stage's folder inside the run's data folder (settings cps.step1_dir), and the folder of its returns
# inside that (cps.step1_out)
if (is.null(settings$cps$step1_dir) || is.null(settings$cps$step1_out))
  stop('settings cps.step1_dir and cps.step1_out are required')
STEP1 = file.path(OUT, settings$cps$step1_dir)
STEP1_OUT = settings$cps$step1_out
cat(sprintf('tax units: the filing stage\'s CBO-rule units and calibrated chances (%s, %s)\n', STEP1, STEP1_OUT))
step1_units_path = function(yr) file.path(STEP1, STEP1_OUT, sprintf('units_%d.csv.gz', yr))

#-------------------------------------------------------------------------------
# The filing stage's returns
#-------------------------------------------------------------------------------

# The filing stage's returns for one year, matched to the units that
# cbo_tax_units() built. Each unit takes its return's filing status, filing
# chance and filing requirement. A couple split into separate returns becomes
# two units, and person 1's keeps the dependents. Every household unit must
# match one return and every return one unit, or the build stops.
step1_returns = function(built, yr) {
  su = fread(step1_units_path(yr))
  need = c('hh', 'head_pn', 'couple_uid', 'person', 'filing_status', 'p_file_cal', 'must_file', 'n24', 'n_dep_eitc',
           'eitc_eligible', 'year')
  stopifnot(all(need %in% names(su)), all(su$year == yr), !anyNA(su[, ..need]),
            all(su$p_file_cal >= 0 & su$p_file_cal <= 1), all(su$filing_status %in% c('single', 'joint', 'separate', 'hoh')))
  su[, unit_id := as.numeric(hh) * 100 + head_pn]
  stopifnot(!anyDuplicated(su$unit_id))
  p = copy(built$persons)
  sep = su[couple_uid != -1]
  stopifnot(all(sep$filing_status == 'separate'), all(su[couple_uid == -1, filing_status] != 'separate'))
  if (nrow(sep)) {
    stopifnot(all(sep$couple_uid %in% p$unit_id), all(sep[, .N, by = couple_uid]$N == 2))
    h = dcast(sep, couple_uid ~ person, value.var = 'head_pn')
    setnames(h, c('1', '2'), c('h1', 'h2'))
    p[h, on = c(unit_id = 'couple_uid'), `:=`(h1 = i.h1, h2 = i.h2)]
    sp = !is.na(p$h1)
    stopifnot(all(p[sp & role %in% c('primary', 'spouse'), PERNUM == h1 | PERNUM == h2]))
    p[sp, `:=`(unit_id = as.numeric(SERIAL) * 100 + fifelse(PERNUM == h2, h2, h1),
               role = fifelse(role == 'dependent', 'dependent', 'primary'))]
    p[, c('h1', 'h2') := NULL]
    cat(sprintf('  the filing stage\'s separate returns: %d couples split, %.2f m weighted couples\n',
                uniqueN(sep$couple_uid), sep[person == 1, sum(weight)] / 1e6))
  }
  chk = p[role == 'primary', .N, by = unit_id]
  stopifnot(all(chk$N == 1), uniqueN(p$unit_id) == nrow(chk))
  units = build_asec_unit_table(p, yr)
  hh_ids = units[unit_type == 'nondependent', unit_id]
  if (!setequal(hh_ids, su$unit_id))
    stop(sprintf('the filing stage\'s returns and the units here differ: %d units with no return, %d returns with no unit',
                 sum(!hh_ids %in% su$unit_id), sum(!su$unit_id %in% hh_ids)))
  units[su, on = 'unit_id', `:=`(filing_status = i.filing_status, p_file_cal = i.p_file_cal,
                                 must_file = as.logical(i.must_file), n24_step1 = i.n24, n_eitc_step1 = i.n_dep_eitc,
                                 eitc_eligible = as.logical(i.eitc_eligible))]
  units[unit_type == 'dependent', `:=`(p_file_cal = NA_real_, must_file = NA)]   # dependents' rows, dropped below
  cat(sprintf('  the filing stage\'s calibrated chances: filers %.2f m of %.2f m household units\n',
              units[unit_type == 'nondependent', sum(weight * p_file_cal)] / 1e6,
              units[unit_type == 'nondependent', sum(weight)] / 1e6))
  list(persons = p[], units = units)
}
POOL_FILE = settings$paths$raw$nonfiler_pool   # the old non-filer pool, for one comparison line in the summary

# Prints the number of missing values in each column that has any.
report_na = function(df, label) {
  na = colSums(is.na(df))
  na = na[na > 0]
  if (length(na) == 0) cat(sprintf('  %s: no NA in any column\n', label))
  else { cat(sprintf('  %s: NA counts\n', label)); print(na) }
}
bn = function(w, x) sum(w * x) / 1e9    # a weighted total in billions

#-------------------------------------------------------------------------------
# Survivor and disability benefits: pensions on the return, and the rest
#-------------------------------------------------------------------------------

# Survivor and disability benefits by source (the ASEC's codes 1-10). A source in
# class "pension" is a pension or annuity reported on the return's pension lines
# (Form 1099-R). A source in class "other" is anything else. "MIXED" marks a
# source whose tax treatment is split; it is put in the closer class. The
# reasons cite IRS Publications 525, 575, 915, 721, 559 and 907.
SURVDIS_SOURCES = rbind(
  data.table(benefit = 'survivor', code = 1:10, label = c(
    'Company or union survivor pension', 'Federal government pension', 'US military retirement survivor pension',
    'State or local govt survivor pension', 'US railroad retirement survivor pension', 'Workers compensation pension',
    'Black lung survivor pension', 'Regular payments from estates or trusts',
    'Regular payments from annuities or paid-up life insurance', 'Other or do not know'),
    class = c('pension', 'pension', 'pension', 'pension', 'pension', 'other', 'other', 'other', 'pension', 'other'),
    reason = c(
      'survivor annuity from an employer plan, taxable on Form 1099-R (Pub 575, survivors of employees and retirees)',
      'civil service survivor annuity, taxable on Form 1099-R (Pub 721; Pub 575)',
      'Survivor Benefit Plan annuity, taxable on Form 1099-R (Pub 525, military retirement pay; Pub 575)',
      'survivor annuity from a government plan, taxable on Form 1099-R (Pub 575)',
      'MIXED: tier 2 and the rest on Form RRB-1099-R are pensions (Pub 575); tier 1 is taxed as Social Security (Form RRB-1099, Pub 915)',
      'workers\' compensation is exempt, and the exemption applies to survivors (Pub 525, workers\' compensation)',
      'black lung benefits are like workers\' compensation and not taxable in most cases (Pub 525, other sickness and injury benefits)',
      'a beneficiary reports trust or estate income from Schedule K-1 (Form 1041) on Schedule E and by character, not as a pension (Pub 559)',
      'MIXED: annuity payments are pensions and annuities on Form 1099-R, taxable part above the cost (Pub 575); life insurance paid in installments is excluded up to the proceeds, the rest interest (Pub 525, life insurance proceeds)',
      'source unknown; not shown to be a Form 1099-R pension')),
  data.table(benefit = 'disability', code = 1:10, label = c(
    'Workers compensation', 'Company or union disability', 'Federal govt disability',
    'US military retirement disability', 'State or local govt employee disability', 'US railroad retirement disability',
    'Accident or disability insurance', 'Black lung miners disability', 'State temporary sickness',
    'Other or don\'t know'),
    class = c('other', 'pension', 'pension', 'pension', 'pension', 'pension', 'other', 'other', 'other', 'other'),
    reason = c(
      'workers\' compensation for occupational sickness or injury is exempt (Pub 525, workers\' compensation; Pub 907)',
      'MIXED: a disability pension from an employer-paid plan is taxable on Form 1099-R, but before minimum retirement age it is reported on line 1h with wages, after it as a pension (Pub 525, disability pensions; Pub 575)',
      'MIXED: civil service disability annuity on Form 1099-R; before minimum retirement age reported on line 1h with wages, after it as a pension (Pub 721; Pub 525, disability pensions)',
      'MIXED: military disability retirement pay is on Form 1099-R and taxable as a pension, but excluded for combat-related injury, entitlement before 25 Sep 1975, or up to the VA compensation the person could get (Pub 525, military and government disability pensions)',
      'MIXED: as company disability pensions (Pub 525, disability pensions); a plan in the nature of workers\' compensation (e.g. line-of-duty disability) is exempt',
      'MIXED: tier 2 and the rest on Form RRB-1099-R are pensions, before minimum retirement age reported with wages (Pub 575; Pub 525); tier 1 is taxed as Social Security (Pub 915)',
      'MIXED: taxable only if the employer paid the premiums, then as sick pay reported with wages (Form W-2), not a pension; not taxable if the person paid them (Pub 525, sick pay; Pub 907)',
      'black lung benefits are like workers\' compensation and not taxable in most cases (Pub 525, other sickness and injury benefits)',
      'MIXED: state sickness or disability fund payments are sick pay, taxable to the extent of employer contributions and reported with wages, not a pension; employee-funded plans (e.g. California SDI) are not taxable (Pub 525, sick pay)',
      'source unknown; not shown to be a Form 1099-R pension')))
stopifnot(all(SURVDIS_SOURCES$class %in% c('pension', 'other')))
SURVDIS_ITEMS = c('SRCSURV1', 'SRCSURV2', 'INCSURV1', 'INCSURV2', 'SRCDISA1', 'SRCDISA2', 'INCDISA1', 'INCDISA2')

# Whether each source code of benefit b ('survivor' or 'disability') is in the
# pension class of SURVDIS_SOURCES.
is_pension_source = function(b, code) {
  s = SURVDIS_SOURCES[benefit == b]
  k = s$class[match(code, s$code)]
  !is.na(k) & k == 'pension'   # code 0 (not in universe; no amount, checked above) is not a pension
}

# Splits each person's survivor and disability benefits by the class of their
# source into survdis_pension and survdis_other, which sum to INCSURV + INCDISAB.
# Returns a table by source of people and dollars. The dollars of people under
# 62 are included as a rough guide to disability pensions.
split_survivor_disability = function(x, ddi, yr) {
  # 1. Clean the amounts, and check that the DDI's labels for the source codes are the ones classified above.
  stopifnot(!anyNA(x[, ..SURVDIS_ITEMS]))
  for (v in c('INCSURV1', 'INCSURV2', 'INCDISA1', 'INCDISA2')) set(x, j = v, value = clean_asec_income(x, ddi, v))
  for (b in c('survivor', 'disability')) {
    for (v in if (b == 'survivor') c('SRCSURV1', 'SRCSURV2') else c('SRCDISA1', 'SRCDISA2')) {
      vl = ipumsr::ipums_val_labels(ddi, tidyselect::all_of(v))
      want = SURVDIS_SOURCES[benefit == b]
      got = vl[match(want$code, vl$val), ]
      if (anyNA(got$val) || !identical(got$lbl, want$label) || !setequal(vl$val, 0:10))
        stop(sprintf('%d: the DDI\'s labels for %s differ from the classified ones', yr, v))
      stopifnot(all(x[[v]] %in% 0:10))
    }
  }
  # 2. The two sources' amounts add to the total for all but at most one person a year. The likely cause is that
  #    disclosure protection perturbs the total and the sources separately. There the total is kept and divided
  #    between the sources in proportion to their amounts. More than 5 such people in a year, or a total with no
  #    source amount, stops the run.
  for (b in c('survivor', 'disability')) {
    tot = if (b == 'survivor') 'INCSURV' else 'INCDISAB'
    a1 = if (b == 'survivor') 'INCSURV1' else 'INCDISA1'
    a2 = if (b == 'survivor') 'INCSURV2' else 'INCDISA2'
    bad = which(x[[a1]] + x[[a2]] != x[[tot]])
    if (length(bad) > 5 || any(x[[a1]][bad] + x[[a2]][bad] <= 0))
      stop(sprintf('%d: first plus second source of %s benefits differs from the total for %d people (%d with no source amount)',
                   yr, b, length(bad), sum(x[[a1]][bad] + x[[a2]][bad] <= 0)))
    for (i in bad) {
      cat(sprintf('  %s benefits: total $%.0f but the sources (codes %d, %d) add to $%.0f + $%.0f: the total shared between them in proportion\n',
                  b, x[[tot]][i], x[[sub('INC', 'SRC', a1)]][i], x[[sub('INC', 'SRC', a2)]][i],
                  x[[a1]][i], x[[a2]][i]))
      s1 = round(x[[tot]][i] * x[[a1]][i] / (x[[a1]][i] + x[[a2]][i]))   # whole dollars, so the sum stays exact
      set(x, i, a1, s1)
      set(x, i, a2, x[[tot]][i] - s1)
    }
    stopifnot(all(x[[a1]] + x[[a2]] == x[[tot]]))
  }
  no_src = x[(INCSURV1 != 0 & SRCSURV1 == 0) | (INCSURV2 != 0 & SRCSURV2 == 0) |
             (INCDISA1 != 0 & SRCDISA1 == 0) | (INCDISA2 != 0 & SRCDISA2 == 0), .N]
  if (no_src > 0) stop(sprintf('%d: %d people have a survivor or disability amount with no source', yr, no_src))
  # 3. Split each person's benefits into the pension class and the rest.
  x[, survdis_pension := INCSURV1 * is_pension_source('survivor', SRCSURV1) +
                         INCSURV2 * is_pension_source('survivor', SRCSURV2) +
                         INCDISA1 * is_pension_source('disability', SRCDISA1) +
                         INCDISA2 * is_pension_source('disability', SRCDISA2)]
  x[, survdis_other := (INCSURV + INCDISAB) - survdis_pension]
  stopifnot(!anyNA(x$survdis_pension), all(x$survdis_pension + x$survdis_other == x$INCSURV + x$INCDISAB))
  # 4. The table: one row per person and source with an amount. A person who names one source twice counts once.
  x[, pid_ := .I]
  long = rbind(
    x[INCSURV1 != 0, .(pid_, w = ASECWT, age = AGE, benefit = 'survivor', code = SRCSURV1, amt = INCSURV1)],
    x[INCSURV2 != 0, .(pid_, w = ASECWT, age = AGE, benefit = 'survivor', code = SRCSURV2, amt = INCSURV2)],
    x[INCDISA1 != 0, .(pid_, w = ASECWT, age = AGE, benefit = 'disability', code = SRCDISA1, amt = INCDISA1)],
    x[INCDISA2 != 0, .(pid_, w = ASECWT, age = AGE, benefit = 'disability', code = SRCDISA2, amt = INCDISA2)])
  x[, pid_ := NULL]
  long = long[, .(w = w[1], age = age[1], amt = sum(amt)), by = .(pid_, benefit, code)]
  tab = long[, .(records = .N, people_m = sum(w) / 1e6, billions = sum(w * amt) / 1e9,
                 billions_age_under_62 = sum((w * amt)[age < 62]) / 1e9), by = .(benefit, code)]
  tab = merge(SURVDIS_SOURCES, tab, by = c('benefit', 'code'), all.x = TRUE)
  # A source nobody reports this year has zero people and dollars (the universe is everyone in the frame).
  for (v in c('records', 'people_m', 'billions', 'billions_age_under_62')) set(tab, which(is.na(tab[[v]])), v, 0)
  tab[, benefit := factor(benefit, c('survivor', 'disability'))]
  setorder(tab, benefit, code)
  tab[, benefit := as.character(benefit)]
  cbind(year = yr, tab[, .(benefit, code, label, class, records, people_m, billions, billions_age_under_62, reason)])
}
sources_rows = list()

# The raw ASEC columns used, checked for missing values
RAW_USED = c('SERIAL', 'PERNUM', 'AGE', 'SEX', 'RELATE', 'MARST', 'SPLOC', 'MOMLOC', 'POPLOC', 'FTYPE', 'FAMID',
             'FAMREL', 'SCHLCOLL', 'ASECWT', 'GQ', 'EDUC', 'RACE', 'HISPAN', 'HIMCAIDLY', 'FOODSTAMP', 'INCWELFR',
             'INCTOT', 'INCSS', ASEC_GROSS_INCOME_ITEMS, 'WKSWORK1', 'UHRSWORKLY', 'SPMFAMUNIT', 'SPMCHXPNS',
             'SPMWT', 'FILESTAT', 'CITIZEN', 'INCSSI', 'HIMCARELY', 'HICHAMP', 'VETSTAT', 'CLASSWKR', 'PUBHOUS', 'STATEFIP')

#-------------------------------------------------------------------------------
# Social Security number status
#-------------------------------------------------------------------------------

# Each person's Social Security number status: 1 a citizen (CITIZEN 1-4); 2 a
# noncitizen with a sign of lawful status; 3 a noncitizen without one, who is
# taken to have no number. An adult's signs are Social Security or SSI,
# Medicare, military health cover, veteran status, a government job,
# unemployment insurance, public housing, Medicaid, or marriage to a citizen
# (Borjas's edit). A dependent's signs are their own Social Security, SSI,
# Medicare or military cover, or a parent in the unit who is a citizen or has a
# sign. Children's Medicaid is not a sign: several states give it to children
# without lawful status.
SSN_STATUS_ITEMS = c('CITIZEN', 'INCSS', 'INCSSI', 'INCUNEMP', 'HIMCARELY', 'HICHAMP', 'HIMCAIDLY', 'VETSTAT',
                     'CLASSWKR', 'PUBHOUS')
ssn_status = function(p) {
  stopifnot(all(SSN_STATUS_ITEMS %in% names(p)), !anyNA(p[, ..SSN_STATUS_ITEMS]),
            all(p$CITIZEN %in% 1:5), all(p$HIMCARELY %in% 0:2), all(p$HICHAMP %in% c(1:2, 9)),
            all(p$HIMCAIDLY %in% c(1:2, 9)), all(p$VETSTAT %in% c(0:2, 9)), all(p$PUBHOUS %in% 0:2),
            all(p$role %in% c('primary', 'spouse', 'dependent')))
  # The codes as the DDIs label them (ASECs 2016-2024): 2 yes, 1 no, and "not in universe" 0 (HIMCARELY, PUBHOUS,
  # VETSTAT) or 9 (HICHAMP, HIMCAIDLY; VETSTAT 9 means unknown). Only "yes" counts as a sign.
  cat(sprintf('  status items not in universe or unknown (people): HICHAMP %d, HIMCAIDLY %d, HIMCARELY %d, VETSTAT 9 %d, PUBHOUS %d\n',
              sum(p$HICHAMP == 9), sum(p$HIMCAIDLY == 9), sum(p$HIMCARELY == 0), sum(p$VETSTAT == 9), sum(p$PUBHOUS == 0)))
  s = p[, .(SERIAL, PERNUM, unit_id, role, AGE, ASECWT, CITIZEN, INCSS, INCSSI, INCUNEMP, HIMCARELY, HICHAMP,
            HIMCAIDLY, VETSTAT, CLASSWKR, PUBHOUS,
            citizen = CITIZEN %in% 1:4, noncit = CITIZEN == 5L, medicaid = HIMCAIDLY == 2L,
            own_cover = INCSS > 0 | INCSSI > 0 | HIMCARELY == 2L | HICHAMP == 2L,
            adult_sign = INCSS > 0 | INCSSI > 0 | HIMCARELY == 2L | HICHAMP == 2L | VETSTAT == 2L |
              CLASSWKR %in% 24:28 | INCUNEMP > 0 | PUBHOUS == 2L)]
  s[, adult := role != 'dependent']   # the unit's primary filer and spouse
  # An adult has lawful status as a citizen, as a noncitizen with a sign of their own or Medicaid, or as a
  # noncitizen married to a citizen.
  s[, n_cit_adults := sum(citizen & adult), by = unit_id]
  s[, other_adult_citizen := adult & (n_cit_adults - (citizen & adult)) > 0]
  s[, lawful_adult := adult & (citizen | (noncit & (adult_sign | medicaid)) | (noncit & other_adult_citizen))]
  s[, `:=`(parent_citizen = any(citizen & adult), parent_lawful = any(lawful_adult)), by = unit_id]
  s[, ssn_status := fifelse(citizen, 1L, 3L)]
  s[adult & noncit & lawful_adult, ssn_status := 2L]
  s[!adult & noncit & (own_cover | parent_citizen | parent_lawful), ssn_status := 2L]
  stopifnot(!anyNA(s$ssn_status), all(s[noncit == TRUE, ssn_status] %in% 2:3), all(s[citizen == TRUE, ssn_status] == 1L))
  s[, c('n_cit_adults') := NULL]
  s[]
}
summary_rows = list()

#-------------------------------------------------------------------------------
# Each year
#-------------------------------------------------------------------------------
for (yr in YEARS) {
  # 1. Read the ASEC and check the raw columns used.
  cat(sprintf('\n=== tax year %d (ASEC %d)\n', yr, yr + 1))
  if (!file.exists(step1_units_path(yr)))
    stop(sprintf('the filing stage has no returns with calibrated filing chances for %d (%s)', yr, step1_units_path(yr)))
  x = read_asec(yr)
  ddi = attr(x, 'ddi')
  ret_cols = if (yr <= 2017) 'INCRETIR' else ASEC_RETIREMENT_COMPONENTS
  miss = setdiff(SURVDIS_ITEMS, names(x))
  if (length(miss))
    stop(sprintf('tax year %d (ASEC %d) lacks the survivor and disability source items %s: cannot split survivor_disability into pensions and the rest',
                 yr, yr + 1, paste(miss, collapse = ', ')))
  report_na(x[, c(RAW_USED, ret_cols, SURVDIS_ITEMS), with = FALSE], 'raw ASEC columns used')
  cat(sprintf('  %d person records, %d households\n', nrow(x), uniqueN(x$SERIAL)))
  # 2. Each person's overtime pay (overtime_cps.R), from the extract with the earnings questions. It is NA where
  #    not asked. Write the person file of overtime.
  otp = cps_overtime(yr, x[, .(SERIAL, PERNUM)])
  ot_out = otp[, .(SERIAL, PERNUM, MISH, EARNWT, asked, basic_pay, pay_missing, ELIGORG, EMPSTAT, CLASSWKR, OCC2010, IND,
                  hourly, weekpay, wage, usual, lastwk, finalhours, inelig, exempt, eap_code, eap_prob, passes_duties,
                  ot_cat, ot_week, WKSWORK1, INCWAGE = wages_, ot_year_raw, ot)]
  fwrite(ot_out, file.path(OUT, sprintf('cps_overtime_persons_%d.csv.gz', yr)))
  cat(sprintf('  overtime: %d people asked (EARNWT %.1f m); among them %d with pay missing; %.2f m people (EARNWT) with overtime, $%.1f bn (EARNWT); the cap at own wages takes off $%.2f bn\n',
              sum(otp$asked), otp[asked == TRUE, sum(EARNWT)] / 1e6, sum(otp$pay_missing), otp[ot > 0, sum(EARNWT)] / 1e6,
              otp[asked == TRUE & !pay_missing, sum(EARNWT * ot)] / 1e9,
              otp[asked == TRUE & !pay_missing, sum(EARNWT * (ot_year_raw - ot))] / 1e9))
  cat('  named industry codes, people with the code (a zero would mean the code is not in this year\'s scheme):\n')
  print(otp[IND %in% OT_IND_NAMED, .N, by = IND][order(IND)][, paste(IND, N, sep = ':')], quote = FALSE)

  # 3. The income items, cleaned by production's ASEC functions (src/nonfilers/asec_tax_units.R). Capital gains are
  #    asked from tax year 2018 only (the redesigned survey); in earlier years they are missing.
  x[, INCSS := clean_asec_income(x, ddi, 'INCSS')]
  x[, INCSSI := clean_asec_income(x, ddi, 'INCSSI')]   # used only for the Social Security number status
  x = add_asec_income_concepts(x, ddi)
  if (yr >= 2018) {
    stopifnot('INCCAPG' %in% names(x))
    x[, capital_gains := clean_asec_income(x, ddi, 'INCCAPG')]
  } else {
    if ('INCCAPG' %in% names(x)) cat('  INCCAPG is in this year\'s extract but the survey did not ask it: not used\n')
    x[, capital_gains := NA_real_]
  }
  # 4. Work: weeks worked, 0 to 52 (0 means the person did not work), and usual weekly hours. Hours of 999 mean not
  #    in universe (did not work) and are read as 0.
  stopifnot(all(x$WKSWORK1 >= 0 & x$WKSWORK1 <= 52), all(x$UHRSWORKLY %in% c(0:99, 999)))
  cat(sprintf('  usual hours not in universe (999) but weeks worked > 0: %d people; weeks 0 but hours given: %d\n',
              sum(x$UHRSWORKLY == 999 & x$WKSWORK1 > 0), sum(x$UHRSWORKLY != 999 & x$WKSWORK1 == 0)))
  x[, usual_hours := fifelse(UHRSWORKLY == 999, 0, as.numeric(UHRSWORKLY))]

  # 5. Leave out people in group quarters.
  gq_w = x[GQ == 2, sum(ASECWT)]
  x = x[GQ != 2]
  cat(sprintf('  frame: %.2f m people in households; %.2f m in group quarters left out (as in the pool)\n',
              x[, sum(ASECWT)] / 1e6, gq_w / 1e6))

  # 6. Split survivor and disability benefits by source into pensions on the return and the rest (SURVDIS_SOURCES).
  sv = split_survivor_disability(x, ddi, yr)
  cat('  survivor and disability benefits by source, people in households (a person counts once per source; dollars as reported):\n')
  print(sv[, .(benefit, code, label = substr(label, 1, 45), class, records, people_m = round(people_m, 3),
               billions = round(billions, 2), billions_age_under_62 = round(billions_age_under_62, 2))])
  cat(sprintf('  survivor and disability, people in households: pensions $%.2f bn, the rest $%.2f bn, together $%.2f bn (INCSURV + INCDISAB $%.2f bn)\n',
              bn(x$ASECWT, x$survdis_pension), bn(x$ASECWT, x$survdis_other),
              bn(x$ASECWT, x$survdis_pension + x$survdis_other), bn(x$ASECWT, x$INCSURV + x$INCDISAB)))
  sources_rows[[as.character(yr)]] = sv

  # 7. The units, their filing status and their filing chances, from the filing stage. Each person's Social
  #    Security number status is set before the filing stage's separate returns split the couples. Write the
  #    person file of the status, its items, and the filing chance of the person's return.
  built = cbo_tax_units(x, yr, file.path(STEP1, 'units',
                                         sprintf('roster_%d_cbo_nogq.csv.gz', yr)))
  ssn = ssn_status(built$persons)
  built = step1_returns(built, yr)
  p = built$persons
  u = built$units
  p[ssn, on = c('SERIAL', 'PERNUM'), ssn_status := i.ssn_status]
  stopifnot(!anyNA(p$ssn_status), nrow(ssn) == nrow(p))
  ret = p[, .(SERIAL, PERNUM, return_id = unit_id, return_role = role)]
  ssn_out = merge(ssn, ret, by = c('SERIAL', 'PERNUM'), all.x = TRUE)
  ssn_out[u[unit_type == 'nondependent'], on = c(return_id = 'unit_id'), p_file := i.p_file_cal]
  stopifnot(!anyNA(ssn_out))
  fwrite(ssn_out, file.path(OUT, sprintf('cps_ssn_persons_%d.csv.gz', yr)))
  sw = function(m) sum(ssn_out$ASECWT[m]) / 1e6
  cat(sprintf('  Social Security number status (millions, all units): primary filers and spouses %.2f citizens, %.2f noncitizens with a sign, %.2f without; dependents under 17 %.2f, %.2f, %.2f\n',
              sw(ssn_out$role != 'dependent' & ssn_out$ssn_status == 1), sw(ssn_out$role != 'dependent' & ssn_out$ssn_status == 2),
              sw(ssn_out$role != 'dependent' & ssn_out$ssn_status == 3),
              sw(ssn_out$role == 'dependent' & ssn_out$AGE < 17 & ssn_out$ssn_status == 1),
              sw(ssn_out$role == 'dependent' & ssn_out$AGE < 17 & ssn_out$ssn_status == 2),
              sw(ssn_out$role == 'dependent' & ssn_out$AGE < 17 & ssn_out$ssn_status == 3)))

  # 8. Persons 1 and 2 of each unit. Dependents' own returns are dropped. In a couple of a man and a woman person 1
  #    is the man; in a same-sex couple, the older partner; on a tie, the builder's head. A unit's overtime is
  #    missing when either adult's is (not asked, or pay missing).
  units = u[unit_type == 'nondependent']
  dep_units = u[unit_type == 'dependent']
  cat(sprintf('  units: %d (%.2f m weighted); dependent scoring units dropped: %d (%.2f m weighted, own wages $%.1f bn)\n',
              nrow(units), sum(units$weight) / 1e6, nrow(dep_units), sum(dep_units$weight) / 1e6,
              bn(dep_units$weight, dep_units$INCWAGE)))
  p[otp, on = c('SERIAL', 'PERNUM'), `:=`(ot = i.ot, ot_asked = i.asked)]
  stopifnot(!anyNA(p$ot_asked))
  hs = p[role %in% c('primary', 'spouse'),
         .(unit_id, PERNUM, role, AGE, SEX, INCWAGE, se = INCBUS + INCFARM, weeks = as.numeric(WKSWORK1),
           hours = usual_hours, ot, ot_asked, ssn_status)]
  stopifnot(all(hs$SEX %in% 1:2))
  hs[, n_adult := .N, by = unit_id]
  stopifnot(all(hs$n_adult %in% 1:2))
  hs[, mixed := n_adult == 2 & uniqueN(SEX) == 2, by = unit_id]
  hs[, `:=`(k1 = fifelse(mixed, as.numeric(SEX), 0), k2 = fifelse(mixed, 0, -as.numeric(AGE)),
            k3 = as.integer(role != 'primary'))]
  setorder(hs, unit_id, k1, k2, k3)
  hs[, pos := seq_len(.N), by = unit_id]
  cat(sprintf('  couples: %d mixed-sex, %d same-sex; builder\'s head is not person 1 in %d couples\n',
              hs[pos == 1 & n_adult == 2 & mixed, .N], hs[pos == 1 & n_adult == 2 & !mixed, .N],
              hs[pos == 1 & n_adult == 2 & role != 'primary', .N]))
  hs[, ot_unit_na := anyNA(ot), by = unit_id]
  ot_asked_units = as.integer(unique(hs[ot_asked == TRUE, unit_id]))
  p1 = hs[pos == 1, .(unit_id, age1 = AGE, sex1 = SEX, wages1 = INCWAGE, se1 = se, weeks_worked1 = weeks,
                      usual_hours1 = hours, ot1 = fifelse(ot_unit_na, NA_real_, ot), ot_unit_na,
                      ssn_status1 = ssn_status)]
  p2 = hs[pos == 2, .(unit_id, age2 = AGE, sex2 = SEX, wages2 = INCWAGE, se2 = se, weeks_worked2 = weeks,
                      usual_hours2 = hours, ot2 = fifelse(ot_unit_na, NA_real_, ot), ssn_status2 = ssn_status)]

  # 9. The dependents: their number, the numbers under 13 and under 6, child credit children (qualifying children
  #    under 17), EITC children (qualifying children, tax_units.py's rule), and the ages and Social Security number
  #    status of the three youngest.
  deps = p[role == 'dependent'][order(unit_id, AGE, PERNUM)]
  stopifnot('is_qc' %in% names(deps), !anyNA(deps$is_qc))
  dtab = deps[, .(n_dep_raw = .N, n13 = sum(AGE < 13), n6 = sum(AGE < 6),
                  n_ctc_raw = sum(is_qc & AGE < 17), n_eitc_raw = sum(is_qc),
                  dep_age1 = as.numeric(AGE[1]), dep_age2 = if (.N >= 2) as.numeric(AGE[2]) else -1,
                  dep_age3 = if (.N >= 3) as.numeric(AGE[3]) else -1,
                  dep_ssn_status1 = ssn_status[1], dep_ssn_status2 = if (.N >= 2) ssn_status[2] else 0L,
                  dep_ssn_status3 = if (.N >= 3) ssn_status[3] else 0L), by = unit_id]
  cat(sprintf('  dependents: %.2f m weighted (%.2f m aged 18+); units with more than 3: %d\n',
              sum(deps$ASECWT) / 1e6, sum(deps$ASECWT[deps$AGE >= 18]) / 1e6, sum(dtab$n_dep_raw > 3)))

  # 10. Child care. The SPM unit's child care spending goes to the units that claim its children under 18, in
  #     proportion to the number of children each claims. An SPM unit with spending but no such children gives it
  #     to its reference person's unit.
  spm = p[, .(chx = SPMCHXPNS[1], n_val = uniqueN(SPMCHXPNS), spmwt = SPMWT[1],
              ref_pernum = if (any(RELATE == RELATE_HEAD)) PERNUM[RELATE == RELATE_HEAD][1] else min(PERNUM),
              SERIAL = SERIAL[1], n_serial = uniqueN(SERIAL)), by = SPMFAMUNIT]
  stopifnot(all(spm$n_val == 1), all(spm$n_serial == 1), all(spm$chx >= 0))
  ref_unit = p[, .(SERIAL, ref_pernum = PERNUM, ref_unit = unit_id, ref_w = ASECWT)]
  spm = merge(spm, ref_unit, by = c('SERIAL', 'ref_pernum'), all.x = TRUE)
  stopifnot(!anyNA(spm$ref_unit))
  kids = p[role == 'dependent' & AGE < 18, .(n_kids = .N), by = .(SPMFAMUNIT, unit_id)]
  kids[, share := n_kids / sum(n_kids), by = SPMFAMUNIT]
  paid = spm[chx > 0]
  alloc = rbind(merge(paid[, .(SPMFAMUNIT, chx)], kids[, .(SPMFAMUNIT, unit_id, share)], by = 'SPMFAMUNIT'),
                paid[!SPMFAMUNIT %in% kids$SPMFAMUNIT, .(SPMFAMUNIT, chx, unit_id = ref_unit, share = 1)])
  alloc[, care_exp := chx * share]
  cat(sprintf('  SPM units paying for child care: %d; to units claiming their children %d, to the reference person\'s unit %d\n',
              nrow(paid), nrow(paid[SPMFAMUNIT %in% kids$SPMFAMUNIT]), nrow(paid[!SPMFAMUNIT %in% kids$SPMFAMUNIT])))
  chk_spm = alloc[, .(got = sum(care_exp), chx = chx[1]), by = SPMFAMUNIT]
  stopifnot(nrow(chk_spm) == nrow(paid), max(abs(chk_spm$got - chk_spm$chx)) < 1e-6)
  cc = alloc[, .(care_exp = sum(care_exp)), by = unit_id]
  stopifnot(all(cc$unit_id %in% units$unit_id))

  # 11. Assemble one row per unit: the unit's items from the filing stage and the unit table (EITC eligibility under
  #     the tax year's law is filing.py's eitc_eligible), the head's and spouse's other items, persons 1 and 2, the
  #     dependents and child care.
  stopifnot(!anyNA(units$eitc_eligible))
  units[, eitc_ok := as.integer(eitc_eligible)]
  out = units[, .(unit_id, SERIAL, weight, filing_status, must_file, p_file = p_file_cal, eitc_ok,
                  wages = INCWAGE, sch_cf_income = se_income, txbl_int = INCINT, dividends = INCDIVID,
                  gross_ss = INCSS, ui = INCUNEMP, retirement_income, rent_royalty_estate = INCRENT)]
  extra = p[role %in% c('primary', 'spouse'),
            .(survivor_disability = sum(INCSURV + INCDISAB), survdis_pension = sum(survdis_pension),
              survdis_other = sum(survdis_other), other_income = sum(INCOTHER),
              capital_gains = sum(capital_gains)), by = unit_id]
  out = merge(out, extra, by = 'unit_id', all.x = TRUE)
  out = merge(out, p1, by = 'unit_id', all.x = TRUE)
  out = merge(out, p2, by = 'unit_id', all.x = TRUE)
  out = merge(out, dtab, by = 'unit_id', all.x = TRUE)
  out = merge(out, cc, by = 'unit_id', all.x = TRUE)
  stopifnot(nrow(out) == nrow(units), !anyNA(out$age1), !anyNA(out$survivor_disability),
            !anyNA(out$survdis_pension), !anyNA(out$survdis_other),
            all(out$survivor_disability == out$survdis_pension + out$survdis_other))
  cat('  NA counts after the joins, before filling (expected: person 2 on units with no spouse, dependents on units with none, child care on units given none):\n')
  na = colSums(is.na(out)); print(na[na > 0])
  married = out$filing_status == 'joint'
  stopifnot(all(is.na(out$age2) == !married), all(is.na(out$n_dep_raw) == (out$unit_id %in% dtab$unit_id == FALSE)))
  out[, married := as.integer(filing_status == 'joint')]
  # Where an item does not apply: a unit with no spouse has person 2's amounts 0, age -1 and sex 0; a unit with no
  # dependents has counts 0 and ages -1; a unit given no child care has 0.
  out[married == 0, `:=`(age2 = -1, sex2 = 0, wages2 = 0, se2 = 0, weeks_worked2 = 0, usual_hours2 = 0,
                         ot2 = fifelse(ot_unit_na, NA_real_, 0), ssn_status2 = 0L)]
  out[is.na(n_dep_raw), `:=`(n_dep_raw = 0L, n13 = 0L, n6 = 0L, n_ctc_raw = 0L, n_eitc_raw = 0L,
                             dep_age1 = -1, dep_age2 = -1, dep_age3 = -1,
                             dep_ssn_status1 = 0L, dep_ssn_status2 = 0L, dep_ssn_status3 = 0L)]
  stopifnot(all(out$filing_status %in% c('single', 'joint', 'separate', 'hoh')))
  out[is.na(care_exp), care_exp := 0]

  # The split sums must be exact: each pair of person amounts adds to the unit's amount.
  stopifnot(identical(out$wages1 + out$wages2, out$wages), identical(out$se1 + out$se2, out$sch_cf_income),
            identical(out$survdis_pension + out$survdis_other, out$survivor_disability))

  # The unit's state is its household's (STATEFIP). A household has one state.
  hh_state = p[, .(statefip = STATEFIP[1], n_states = uniqueN(STATEFIP)), by = SERIAL]
  stopifnot(all(hh_state$n_states == 1), !anyNA(hh_state$statefip))
  out[hh_state, on = 'SERIAL', statefip := i.statefip]
  stopifnot(!anyNA(out$statefip))

  # 12. The output columns. Each unit's filer flag is a seeded draw against its filing chance. Counts are capped
  #     at 3, as on the PUF.
  setorder(out, unit_id)
  stopifnot(!anyDuplicated(out$unit_id), all(out$unit_id == round(out$unit_id)), max(out$unit_id) < 2^31)
  set.seed(as.integer(settings$cps$filer_seed) + yr)
  draw = runif(nrow(out))
  out = out[, .(year = yr, record_id = as.integer(unit_id), household_id = as.integer(SERIAL), weight,
                statefip = as.integer(statefip),
                married, n_dep = pmin(n_dep_raw, 3L), dep_filer = 0L,
                head_of_household = as.integer(filing_status == 'hoh'),
                filing_status = fcase(filing_status == 'single', 1L, filing_status == 'joint', 2L,
                             filing_status == 'separate', 3L, filing_status == 'hoh', 4L),
                # EITC children as the PUF's EIC counts them: only on units eligible for the credit
                n_dep_ctc = pmin(n_ctc_raw, 3L), n_dep_eitc = pmin(n_eitc_raw, 3L) * eitc_ok,
                eitc_eligible = eitc_ok,
                filer = as.integer(draw < p_file), p_file, must_file = as.integer(must_file),
                age1 = as.integer(age1), age2 = as.integer(age2), sex1 = as.integer(sex1), sex2 = as.integer(sex2),
                male1 = as.integer(sex1 == 1), male2 = fifelse(married == 1, as.integer(sex2 == 1), -1L),
                dep_age1 = as.integer(dep_age1), dep_age2 = as.integer(dep_age2), dep_age3 = as.integer(dep_age3),
                n_dep_under13 = pmin(n13, 3L), n_dep_under6 = pmin(n6, 3L),
                wages, sch_cf_income, txbl_int, dividends, gross_ss, ui, retirement_income,
                rent_royalty_estate, survivor_disability, survdis_pension, survdis_other, other_income, capital_gains,
                wages1, wages2, se1, se2, weeks_worked1, weeks_worked2, usual_hours1, usual_hours2, care_exp,
                ot1, ot2, ssn_status1 = as.integer(ssn_status1), ssn_status2 = as.integer(ssn_status2),
                dep_ssn_status1 = as.integer(dep_ssn_status1), dep_ssn_status2 = as.integer(dep_ssn_status2),
                dep_ssn_status3 = as.integer(dep_ssn_status3))]
  # 13. Checks, then write the units. Overtime is missing on both adults together and is never above the person's
  #     wages. Capital gains are missing exactly in the years they were not asked (before tax year 2018).
  stopifnot(identical(is.na(out$ot1), is.na(out$ot2)),
            all(out[!is.na(ot1), ot1 <= wages1 + 1e-6 & ot1 >= 0]),   # the units asked (the rest are NA by design)
            all(out[!is.na(ot2), ot2 <= wages2 + 1e-6 & ot2 >= 0]),
            all(out[married == 0 & !is.na(ot2), ot2] == 0))
  cat(sprintf('  overtime on units: asked %d of %d units (%.1f%% of the weight); missing on %d units (%d of them asked but with a worker\'s pay missing)\n',
              sum(!is.na(out$ot1)), nrow(out), 100 * sum(out$weight[!is.na(out$ot1)]) / sum(out$weight),
              sum(is.na(out$ot1)), sum(is.na(out$ot1) & out$record_id %in% ot_asked_units)))
  stopifnot(if (yr >= 2018) !anyNA(out$capital_gains) else all(is.na(out$capital_gains)))
  stopifnot(!anyNA(out[, !c('ot1', 'ot2', 'capital_gains')]), all(out$weight > 0), all(out$n_dep_under13 <= out$n_dep),
            all(out$n_dep_eitc <= out$n_dep), all((out$filing_status == 2) == (out$married == 1)),
            all((out$dep_age1 >= 0) == (out$n_dep > 0)),
            all(out$ssn_status1 %in% 1:3), all((out$ssn_status2 > 0) == (out$married == 1)),
            all(out$ssn_status2 %in% 0:3), all((out$dep_ssn_status1 > 0) == (out$dep_age1 >= 0)),
            all((out$dep_ssn_status2 > 0) == (out$dep_age2 >= 0)), all((out$dep_ssn_status3 > 0) == (out$dep_age3 >= 0)))
  fwrite(out, file.path(OUT, sprintf('cps_units_%d.csv.gz', yr)))
  cat(sprintf('  wrote %s\n', file.path(OUT, sprintf('cps_units_%d.csv.gz', yr))))

  # 14. Print the year's summary and keep its rows: filers and non-filers, EITC returns, the income totals, and
  #     child care against the SPM units' total.
  w = out$weight
  fs_returns = p[FILESTAT %in% 1:3, sum(ASECWT)] / 2 + p[FILESTAT %in% 4:5, sum(ASECWT)]
  pool_hh = NA_real_
  pool_path = sub('{year}', yr, POOL_FILE, fixed = TRUE)
  if (file.exists(pool_path)) {
    pool = fread(pool_path)
    pool_hh = pool[source == 'asec_household', sum(weight)]
  }
  cat(sprintf('\n  SUMMARY %d: rows %d; weighted units %.2f m; married %.3f; head of household %.3f; filer %.3f (expected %.3f); statutory requirement %.3f\n',
              yr, nrow(out), sum(w) / 1e6, sum(w * out$married) / sum(w), sum(w * out$head_of_household) / sum(w),
              sum(w * out$filer) / sum(w), sum(w * out$p_file) / sum(w), sum(w * out$must_file) / sum(w)))
  cat(sprintf('  filers drawn %.2f m (expected %.2f m); non-filers %.2f m (expected %.2f m; the pool\'s household non-filer units %s m)\n',
              sum(w * out$filer) / 1e6, sum(w * out$p_file) / 1e6, sum(w * (1 - out$filer)) / 1e6,
              sum(w * (1 - out$p_file)) / 1e6, if (is.na(pool_hh)) 'n/a' else sprintf('%.2f', pool_hh / 1e6)))
  cat(sprintf('  Census tax model (FILESTAT) simulated returns %.2f m, for comparison only\n', fs_returns / 1e6))
  ef = out[, .(eitc_returns_expected_filers_m = sum(weight * p_file * eitc_eligible) / 1e6,
               eitc_children_expected_filers_m = sum(weight * p_file * n_dep_eitc) / 1e6), by = .(children = n_dep_eitc)]
  setorder(ef, children)
  cat(sprintf('  EITC (the filing stage\'s eligibility): EITC children on expected filers %.2f m (capped at 3 per unit); EITC returns, expected filers, by children:\n',
              sum(out$weight * out$p_file * out$n_dep_eitc) / 1e6))
  print(ef[, .(children, eitc_returns_m = round(eitc_returns_expected_filers_m, 2))])
  fwrite(cbind(year = yr, ef), file.path(OUT, sprintf('cps_eitc_%d.csv', yr)))
  cat(sprintf('  units with person 1 under 18: %d rows, %.2f m weighted; dependents claimed, counted by the claiming unit\'s weight, capped at 3: %.2f m\n',
              sum(out$age1 < 18), sum(w[out$age1 < 18]) / 1e6, sum(w * out$n_dep) / 1e6))
  spm_total_spmwt = sum(spm$chx * spm$spmwt) / 1e9
  spm_total_refw = sum(spm$chx * spm$ref_w) / 1e9
  amounts = c('wages', 'sch_cf_income', 'txbl_int', 'dividends', 'gross_ss', 'ui', 'retirement_income',
              'rent_royalty_estate', 'survivor_disability', 'survdis_pension', 'survdis_other', 'other_income',
              'capital_gains', 'care_exp')
  tot = data.table(year = yr, item = amounts, billions = sapply(amounts, function(v) bn(w, out[[v]])),
                   recipients_m = sapply(amounts, function(v) sum(w * (out[[v]] != 0)) / 1e6))
  print(tot[, .(item, billions = round(billions, 1), recipients_m = round(recipients_m, 2))])
  cat(sprintf('  child care: tax units $%.2f bn; SPM units $%.2f bn weighted by SPMWT, $%.2f bn by the reference person\'s ASECWT; unweighted sums agree exactly\n',
              bn(w, out$care_exp), spm_total_spmwt, spm_total_refw))
  summary_rows[[as.character(yr)]] = rbind(
    tot,
    data.table(year = yr, item = c('weighted_units_m', 'married_share', 'filer_share', 'expected_filer_share',
                                   'must_file_share', 'filestat_returns_m', 'pool_household_nonfilers_m',
                                   'calibrated_filing_model'),
               billions = c(sum(w) / 1e6, sum(w * out$married) / sum(w), sum(w * out$filer) / sum(w),
                            sum(w * out$p_file) / sum(w), sum(w * out$must_file) / sum(w), fs_returns / 1e6,
                            pool_hh / 1e6, 1),
               recipients_m = NA_real_))
}

# The summary file and the table of survivor and disability sources each hold every year built so far.
s = rbindlist(summary_rows)
sp = file.path(OUT, 'cps_summary.csv')
if (file.exists(sp)) s = rbind(fread(sp)[!year %in% YEARS], s)
fwrite(s, sp)
cat('\nwrote', sp, '\n')
sv = rbindlist(sources_rows)
svp = file.path(OUT, 'cps_survivor_disability_sources.csv')
if (file.exists(svp)) sv = rbind(fread(svp)[!year %in% YEARS], sv)
setorder(sv, year)
fwrite(sv, svp)
cat('wrote', svp, '\n')
