#-------------------------------------------------------------------------------
# build_cps.R: the CPS ASEC split into tax units, tax years 2015, 2017, 2019,
# 2021 and 2023 (settings "cps"). The ASEC asking about tax year Y is the
# survey of March Y+1 (read_asec(Y) maps this).
#
# Tax units. CBO's rules (tax_units.py, the shared tax-unit module, read
# through tax_units_cbo.R), as the filing stage (filing.py, run before this
# script) built and calibrated them, inside the run's data folder: the roster
# from <step1_dir>/units/roster_<year>_<cps.cbo_variant>_nogq.csv.gz and the
# returns from <step1_dir>/<step1_out>/units_<year>.csv.gz (step1_returns
# below; settings cps.step1_dir and cps.step1_out, both required; "step 1" is
# PLAN_v2.md's name for the filing stage). The returns give the separate
# returns split from joint couples and each return's filing status, statutory
# filing requirement and calibrated filing chance. A year with no such file
# stops. The frame is the household population: people
# in group quarters (GQ 2) are left out, and their weighted count is printed.
#
# Dependent filers. The unit table (production's build_asec_unit_table) also
# makes a scoring row for every dependent aged 15 and over. Those rows are
# dropped here: the dependent is counted once, in the n_dep of the unit that
# claims them, and their own income goes nowhere (it belongs on their own
# return). A unit's income is the primary filer's plus the spouse's.
#
# Filing. p_file is step 1's calibrated probability (the training table's
# filer_p, which training draws the filing flag from in every pass); filer = 1
# when a uniform draw is below it (kept for the table's checks and reports,
# which need a 0/1 value on every row) (one draw per unit, seeded by settings
# cps.filer_seed + the year, in record_id order). must_file is step 1's
# statutory filing requirement.
#
# Primary filer and spouse. Person 1 is the person the rebuild's rule makes the
# primary filer before its one random step: in a mixed-sex couple the man, in a
# same-sex couple the older partner (the builder's head on a tie), otherwise
# the unit's only adult. The rule's random step (the woman is the primary with
# a chance by the man's age range, set from the PUF in build_table.py, ages.py)
# happens in the table stage, keyed by household_id as for the CEX: where it
# makes the woman the primary, the table stage swaps every pair of person
# columns (age, sex, wages, se, weeks, hours). The age range does not depend on
# the order in this file.
#
# Ids. record_id = SERIAL x 100 + the PERNUM of the builder's unit head
# (the builder's unit_id), unique within the year. household_id = SERIAL.
#
# Columns, one row per tax unit, no NA except overtime (below). Dollars are nominal dollars of the tax
# year as reported (the ASEC asks about the previous calendar year).
#   year, record_id, household_id, weight (the head's ASECWT)
#   married (1 joint unit), n_dep (dependents claimed, capped at 3 as the SCF
#     builder does), dep_filer (0 on every row: dependent filers are folded),
#     head_of_household (1 unmarried with a qualifying-child dependent)
#   filer, p_file, must_file (above)
#   age1, age2, sex1, sex2, male1, male2: person 1 and person 2 (the spouse).
#     Ages as the ASEC reports them (top-coded: 80 is 80-84, 85 is 85 and over
#     in these years). No spouse: age2 = -1, sex2 = 0, male2 = -1. sex 1 male,
#     2 female; male 1 or 0.
#   dep_age1..dep_age3: ages of up to three dependents, youngest first; -1
#     where there is none. n_dep_under13, n_dep_under6: dependents under 13 and
#     under 6, capped at 3 like n_dep.
#   Unit amounts (person 1 plus person 2), ASEC items with their not-in-universe
#   code set to 0 (clean_asec_income):
#     wages               INCWAGE
#     sch_cf_income       INCBUS + INCFARM (non-farm business and farm
#                         self-employment income, can be negative)
#     interest            INCINT
#     dividends           INCDIVID
#     ss_gross            INCSS (gross Social Security)
#     unemployment        INCUNEMP
#     retirement_income   the builder's harmonised retirement series (S16):
#                         INCRETIR through tax year 2017; from 2018 (the
#                         redesigned survey) INCRET1 + INCRET2 + INCPEN1 +
#                         INCPEN2 + INCRANN. INCRETIR alone is not comparable
#                         across the redesign.
#     rent_royalty_estate INCRENT (net rent, roomers and boarders, estates,
#                         trusts and royalties; can be negative)
#     survivor_disability INCSURV + INCDISAB (survivor and disability
#                         pensions other than Social Security), the sum of
#                         the next two, exactly
#     survdis_pension     the part of it from sources that are pensions or
#                         annuities on a return (Form 1099-R): survivor and
#                         disability amounts from sources of class "pension"
#                         in SURVDIS_SOURCES below
#     survdis_other       the rest: workers' compensation, black lung, estates
#                         and trusts, accident or disability insurance, state
#                         temporary sickness, other or don't know (class
#                         "other" in SURVDIS_SOURCES)
#                         The split reads the ASEC's source items: the first
#                         and second source of survivor benefits (SRCSURV1,
#                         SRCSURV2, amounts INCSURV1, INCSURV2) and of
#                         disability benefits (SRCDISA1, SRCDISA2, INCDISA1,
#                         INCDISA2). The two amounts must add to INCSURV and
#                         INCDISAB after cleaning, and a year without the items
#                         stops. Per year and source, the people, the dollars
#                         and the class are written to
#                         cps_survivor_disability_sources.csv.
#     other_income        INCOTHER (includes alimony)
#   overtime1, overtime2 (since 28 October): the primary filer's and the
#     spouse's overtime pay in the tax year, by Tedeschi's rules
#     (overtime_cps.R), at most the person's wages (INCWAGE); missing (NA),
#     not zero, on every unit whose adults were not asked the earnings
#     questions (about three quarters of units: only months 4 and 8 in sample
#     are asked), and on the few asked workers with no pay. overtime2 is 0 on
#     an asked unit with no spouse. The only columns that can be NA.
#   CPS-only, per person: wages1, wages2 (INCWAGE); se1, se2 (INCBUS +
#     INCFARM); weeks_worked1/2 (WKSWORK1, weeks worked last year);
#     usual_hours1/2 (UHRSWORKLY, usual weekly hours last year, its
#     not-in-universe code 999 set to 0). Person 2 is 0 where there is none.
#     wages1 + wages2 = wages and se1 + se2 = sch_cf_income exactly.
#   childcare: the SPM unit's child care expenses (SPMCHXPNS), given to the tax
#     units that claim the SPM unit's children (dependents under 18 who belong
#     to that SPM unit), in proportion to the number each claims; where no unit
#     claims one of its children, to the unit of the SPM unit's reference
#     person (the householder when in the SPM unit, else its lowest PERNUM).
#     Each SPM unit's expenses are kept in full.
#     capital_gains       INCCAPG, from tax year 2018 (the redesigned survey); 0 in earlier years, which do not
#                         ask it (the translation table's row for it says the same, settings "translation")
#   mars: filing status as step 1's returns carry it, 1 single, 2 joint, 3 separate, 4 head of household
#   n_ctc, n_eitc: child credit and EITC children, capped at 3: dependents who are qualifying children (role 3
#     in tax_units.py), under 17 for the child credit; the approximation tax_units.py makes. n_eitc counts them
#     only when the unit is eligible for the EITC under the year's law (step 1's eitc_eligible), as the PUF's EIC
#     counts children only on returns that get the credit
#   eitc_eligible: 1 or 0, step 1's (filing.py)
#
# Paths and years from settings.json ("cps"); the settings file is
# REBUILD_SETTINGS if set, else the pipeline's own; output to
# REBUILD_OUT/data. Optional arguments name tax years to build (default: all
# of settings cps.years). Writes {out}/data/cps_units_{year}.csv.gz,
# cps_summary.csv (the printed totals per year) and
# cps_survivor_disability_sources.csv (the sources' table, every year built), and per year
# cps_overtime_persons_{year}.csv.gz (every person's overtime build, for the checks). Run from the repository root
# through Slurm. The R modules lack ipumsr; it is installed (ipumsr 0.10.0,
# hipread 0.2.6) in John's library for R/4.4.1-foss-2022b,
# R_LIBS=$HOME/R/4.4.1-foss-2022b (~jar335); run.sbatch, stage cps.
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
source('research/rebuild/pipeline/tax_units_cbo.R')
source('research/rebuild/pipeline/overtime_cps.R')  # cps_overtime(): each person's overtime pay, Tedeschi's rules
# the filing stage's folder inside the run's data folder (cps.step1_dir) and the folder of its returns inside it
# (cps.step1_out)
if (is.null(settings$cps$step1_dir) || is.null(settings$cps$step1_out))
  stop('settings cps.step1_dir and cps.step1_out are required')
STEP1 = file.path(OUT, settings$cps$step1_dir)
STEP1_OUT = settings$cps$step1_out
cat(sprintf('tax units: step 1\'s CBO-rule units and calibrated chances (%s, %s)\n', STEP1, STEP1_OUT))
step1_units_path = function(yr) file.path(STEP1, STEP1_OUT, sprintf('units_%d.csv.gz', yr))

# Step 1's returns for one year, from the CBO-rule units cbo_tax_units() made from the roster. Step 1's units file
# (filing.py: one row per return, separate returns included) gives each return's head (hh, head_pn), filing status,
# calibrated chance (p_file_cal) and statutory requirement (must_file). A joint couple step 1 split into two separate
# returns (couple_uid, person 1 and 2) becomes two units here: the head of return 2 leaves with nobody, return 1 keeps
# the dependents (step 1's rule, dep_rule 'primary'). Unit ids stay SERIAL x 100 + the head's PERNUM. Every household
# unit must match one return of step 1's file and the reverse, or the build stops.
step1_returns = function(built, yr) {
  su = fread(step1_units_path(yr))
  need = c('hh', 'head_pn', 'couple_uid', 'person', 'filing_status', 'p_file_cal', 'must_file', 'n24', 'n_eitc',
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
    cat(sprintf('  step 1\'s separate returns: %d couples split, %.2f m weighted couples\n',
                uniqueN(sep$couple_uid), sep[person == 1, sum(weight)] / 1e6))
  }
  chk = p[role == 'primary', .N, by = unit_id]
  stopifnot(all(chk$N == 1), uniqueN(p$unit_id) == nrow(chk))
  units = build_asec_unit_table(p, yr)
  hh_ids = units[unit_type == 'nondependent', unit_id]
  if (!setequal(hh_ids, su$unit_id))
    stop(sprintf('step 1\'s returns and the units here differ: %d units with no return, %d returns with no unit',
                 sum(!hh_ids %in% su$unit_id), sum(!su$unit_id %in% hh_ids)))
  units[su, on = 'unit_id', `:=`(filing_status = i.filing_status, p_file_cal = i.p_file_cal,
                                 must_file = as.logical(i.must_file), n24_step1 = i.n24, n_eitc_step1 = i.n_eitc,
                                 eitc_eligible = as.logical(i.eitc_eligible))]
  units[unit_type == 'dependent', `:=`(p_file_cal = NA_real_, must_file = NA)]   # scoring rows, dropped below
  cat(sprintf('  step 1\'s calibrated chances: filers %.2f m of %.2f m household units\n',
              units[unit_type == 'nondependent', sum(weight * p_file_cal)] / 1e6,
              units[unit_type == 'nondependent', sum(weight)] / 1e6))
  list(persons = p[], units = units)
}
POOL_FILE = settings$paths$raw$nonfiler_pool   # the old non-filer pool, for one comparison line in the summary

report_na = function(df, label) {
  na = colSums(is.na(df))
  na = na[na > 0]
  if (length(na) == 0) cat(sprintf('  %s: no NA in any column\n', label))
  else { cat(sprintf('  %s: NA counts\n', label)); print(na) }
}
bn = function(w, x) sum(w * x) / 1e9

# Survivor and disability benefits by source (the ASEC's codes 1-10; 0 is not in universe). Class "pension": a
# pension or annuity on the return's pension lines (Form 1099-R). Class "other": not a pension on the return. The
# label must match the DDI's, or the build stops. "MIXED" marks a source whose tax treatment is split; it sits in the
# class judged closer. Publications: IRS Pub 525 (Taxable and Nontaxable Income), 575 (Pension and Annuity Income),
# 915 (Social Security and Equivalent Railroad Retirement Benefits), 721 (Civil Service Retirement Benefits), 559
# (Survivors, Executors and Administrators), 907 (Persons with Disabilities).
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

# Splits each person's survivor and disability benefits by the class of their source. Adds survdis_pension and
# survdis_other to x (per person; their sum is INCSURV + INCDISAB exactly) and returns the table by source of the
# people in x (people with an amount from that source, weighted by ASECWT; their dollars; and the dollars of people
# aged under 62, the earliest age most pension plans pay without a disability, as a rough guide to the disability
# pensions reported with wages).
split_survivor_disability = function(x, ddi, yr) {
  miss = setdiff(SURVDIS_ITEMS, names(x))
  if (length(miss))
    stop(sprintf('tax year %d (ASEC %d) lacks the survivor and disability source items %s: cannot split survivor_disability into pensions and the rest',
                 yr, yr + 1, paste(miss, collapse = ', ')))
  stopifnot(!anyNA(x[, ..SURVDIS_ITEMS]))
  for (v in c('INCSURV1', 'INCSURV2', 'INCDISA1', 'INCDISA2')) set(x, j = v, value = clean_asec_income(x, ddi, v))
  # the DDI's labels must be the ones classified above
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
  # The two sources add to the totals. In the 2016-2024 ASECs they do for every person but one in some years (one
  # survivor in tax years 2015, 2021 and 2023, checked 27 October 2026: totals of $47,892, $86,838 and $30,410
  # against sources adding to $25,092, $50,400 and $30,309), presumably the Census Bureau's disclosure protection
  # applied to the total and the sources separately. There the total is kept (survivor_disability does not change)
  # and shared between the two sources in proportion to their amounts. More than 5 such people in a year, or a
  # total with no source amount, stops.
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
  is_p = function(b, code) {
    s = SURVDIS_SOURCES[benefit == b]
    k = s$class[match(code, s$code)]
    !is.na(k) & k == 'pension'   # code 0 (not in universe; no amount, checked above) is not a pension
  }
  x[, survdis_pension := INCSURV1 * is_p('survivor', SRCSURV1) + INCSURV2 * is_p('survivor', SRCSURV2) +
                         INCDISA1 * is_p('disability', SRCDISA1) + INCDISA2 * is_p('disability', SRCDISA2)]
  x[, survdis_other := (INCSURV + INCDISAB) - survdis_pension]
  stopifnot(!anyNA(x$survdis_pension), all(x$survdis_pension + x$survdis_other == x$INCSURV + x$INCDISAB))
  # the table: one row per person and source with an amount (a person naming one source twice counts once in it)
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
  # a source nobody reports this year: zero people and dollars (the universe is everyone in the frame)
  for (v in c('records', 'people_m', 'billions', 'billions_age_under_62')) set(tab, which(is.na(tab[[v]])), v, 0)
  tab[, benefit := factor(benefit, c('survivor', 'disability'))]
  setorder(tab, benefit, code)
  tab[, benefit := as.character(benefit)]
  cbind(year = yr, tab[, .(benefit, code, label, class, records, people_m, billions, billions_age_under_62, reason)])
}
sources_rows = list()

RAW_USED = c('SERIAL', 'PERNUM', 'AGE', 'SEX', 'RELATE', 'MARST', 'SPLOC', 'MOMLOC', 'POPLOC', 'FTYPE', 'FAMID',
             'FAMREL', 'SCHLCOLL', 'ASECWT', 'GQ', 'EDUC', 'RACE', 'HISPAN', 'HIMCAIDLY', 'FOODSTAMP', 'INCWELFR',
             'INCTOT', 'INCSS', ASEC_GROSS_INCOME_ITEMS, 'WKSWORK1', 'UHRSWORKLY', 'SPMFAMUNIT', 'SPMCHXPNS',
             'SPMWT', 'FILESTAT')
summary_rows = list()

for (yr in YEARS) {
  cat(sprintf('\n=== tax year %d (ASEC %d)\n', yr, yr + 1))
  if (!file.exists(step1_units_path(yr)))
    stop(sprintf('step 1 has no returns with calibrated filing chances for %d (%s)', yr, step1_units_path(yr)))
  x = read_asec(yr)
  ddi = attr(x, 'ddi')
  ret_cols = if (yr <= 2017) 'INCRETIR' else ASEC_RETIREMENT_COMPONENTS
  miss = setdiff(SURVDIS_ITEMS, names(x))
  if (length(miss))
    stop(sprintf('tax year %d (ASEC %d) lacks the survivor and disability source items %s: cannot split survivor_disability into pensions and the rest',
                 yr, yr + 1, paste(miss, collapse = ', ')))
  report_na(x[, c(RAW_USED, ret_cols, SURVDIS_ITEMS), with = FALSE], 'raw ASEC columns used')
  cat(sprintf('  %d person records, %d households\n', nrow(x), uniqueN(x$SERIAL)))
  # Overtime pay per person (overtime_cps.R), from the extract with the earnings questions; NA where not asked
  ot = cps_overtime(yr, x[, .(SERIAL, PERNUM)])
  ot_out = ot[, .(SERIAL, PERNUM, MISH, EARNWT, asked, basic_pay, pay_missing, ELIGORG, EMPSTAT, CLASSWKR, OCC2010, IND,
                  hourly, weekpay, wage, usual, lastwk, finalhours, inelig, exempt, eap_code, eap_prob, passes_duties,
                  ot_cat, ot_week, WKSWORK1, INCWAGE = wages_, ot_year_raw, overtime)]
  fwrite(ot_out, file.path(OUT, sprintf('cps_overtime_persons_%d.csv.gz', yr)))
  cat(sprintf('  overtime: %d people asked (EARNWT %.1f m); among them %d with pay missing; %.2f m people (EARNWT) with overtime, $%.1f bn (EARNWT); the cap at own wages takes off $%.2f bn\n',
              sum(ot$asked), ot[asked == TRUE, sum(EARNWT)] / 1e6, sum(ot$pay_missing), ot[overtime > 0, sum(EARNWT)] / 1e6,
              ot[asked == TRUE & !pay_missing, sum(EARNWT * overtime)] / 1e9,
              ot[asked == TRUE & !pay_missing, sum(EARNWT * (ot_year_raw - overtime))] / 1e9))
  cat('  named industry codes, people with the code (a zero would mean the code is not in this year\'s scheme):\n')
  print(ot[IND %in% OT_IND_NAMED, .N, by = IND][order(IND)][, paste(IND, N, sep = ':')], quote = FALSE)

  # Income concepts and the filing model's inputs, cleaned as the pool's driver (01_build_units.R) does
  x[, INCSS := clean_asec_income(x, ddi, 'INCSS')]
  x = add_asec_income_concepts(x, ddi)
  # capital gains: asked from tax year 2018 only (the redesigned survey); earlier years do not have the item
  if (yr >= 2018) {
    stopifnot('INCCAPG' %in% names(x))
    x[, capital_gains := clean_asec_income(x, ddi, 'INCCAPG')]
  } else {
    if ('INCCAPG' %in% names(x)) cat('  INCCAPG is in this year\'s extract but the survey did not ask it: not used\n')
    x[, capital_gains := 0]   # not asked before tax year 2018: zero, as the translation table's row says
  }
  x[, welfare_income   := clean_asec_income(x, ddi, 'INCWELFR')]
  x[, total_income     := clean_asec_income(x, ddi, 'INCTOT')]
  x[, foodstamp_amount := as.numeric(FOODSTAMP)]
  x[, on_medicaid      := HIMCAIDLY == 2L]
  # Work: weeks 0-52 (0 did not work); usual hours, 999 not in universe (did not work) -> 0
  stopifnot(all(x$WKSWORK1 >= 0 & x$WKSWORK1 <= 52), all(x$UHRSWORKLY %in% c(0:99, 999)))
  cat(sprintf('  usual hours not in universe (999) but weeks worked > 0: %d people; weeks 0 but hours given: %d\n',
              sum(x$UHRSWORKLY == 999 & x$WKSWORK1 > 0), sum(x$UHRSWORKLY != 999 & x$WKSWORK1 == 0)))
  x[, usual_hours := fifelse(UHRSWORKLY == 999, 0, as.numeric(UHRSWORKLY))]

  gq_w = x[GQ == 2, sum(ASECWT)]
  x = x[GQ != 2]
  cat(sprintf('  frame: %.2f m people in households; %.2f m in group quarters left out (as in the pool)\n',
              x[, sum(ASECWT)] / 1e6, gq_w / 1e6))

  # Survivor and disability benefits split by source into pensions on the return and the rest (SURVDIS_SOURCES)
  sv = split_survivor_disability(x, ddi, yr)
  cat('  survivor and disability benefits by source, people in households (a person counts once per source; dollars as reported):\n')
  print(sv[, .(benefit, code, label = substr(label, 1, 45), class, records, people_m = round(people_m, 3),
               billions = round(billions, 2), billions_age_under_62 = round(billions_age_under_62, 2))])
  cat(sprintf('  survivor and disability, people in households: pensions $%.2f bn, the rest $%.2f bn, together $%.2f bn (INCSURV + INCDISAB $%.2f bn)\n',
              bn(x$ASECWT, x$survdis_pension), bn(x$ASECWT, x$survdis_other),
              bn(x$ASECWT, x$survdis_pension + x$survdis_other), bn(x$ASECWT, x$INCSURV + x$INCDISAB)))
  sources_rows[[as.character(yr)]] = sv

  #-----------------------------------------------------------------------------
  # Units, filing status and filing chances: step 1's
  #-----------------------------------------------------------------------------
  built = cbo_tax_units(x, yr, file.path(STEP1, 'units',
                                         sprintf('roster_%d_%s_nogq.csv.gz', yr, settings$cps$cbo_variant)))
  built = step1_returns(built, yr)
  p = built$persons
  u = built$units
  calibrated = TRUE   # step 1's chances are calibrated (the summary's calibrated_filing_model)

  #-----------------------------------------------------------------------------
  # Persons 1 and 2 of each unit
  #-----------------------------------------------------------------------------
  units = u[unit_type == 'nondependent']
  dep_units = u[unit_type == 'dependent']
  cat(sprintf('  units: %d (%.2f m weighted); dependent scoring units dropped: %d (%.2f m weighted, own wages $%.1f bn)\n',
              nrow(units), sum(units$weight) / 1e6, nrow(dep_units), sum(dep_units$weight) / 1e6,
              bn(dep_units$weight, dep_units$INCWAGE)))

  p[ot, on = c('SERIAL', 'PERNUM'), `:=`(overtime = i.overtime, ot_asked = i.asked)]
  stopifnot(!anyNA(p$ot_asked))
  hs = p[role %in% c('primary', 'spouse'),
         .(unit_id, PERNUM, role, AGE, SEX, INCWAGE, se = INCBUS + INCFARM, weeks = as.numeric(WKSWORK1),
           hours = usual_hours, overtime, ot_asked)]
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
  # a unit's overtime is missing when either adult's is (not asked, or pay missing)
  hs[, ot_unit_na := anyNA(overtime), by = unit_id]
  ot_asked_units = as.integer(unique(hs[ot_asked == TRUE, unit_id]))
  p1 = hs[pos == 1, .(unit_id, age1 = AGE, sex1 = SEX, wages1 = INCWAGE, se1 = se, weeks_worked1 = weeks,
                      usual_hours1 = hours, overtime1 = fifelse(ot_unit_na, NA_real_, overtime), ot_unit_na)]
  p2 = hs[pos == 2, .(unit_id, age2 = AGE, sex2 = SEX, wages2 = INCWAGE, se2 = se, weeks_worked2 = weeks,
                      usual_hours2 = hours, overtime2 = fifelse(ot_unit_na, NA_real_, overtime))]

  #-----------------------------------------------------------------------------
  # Dependents
  #-----------------------------------------------------------------------------
  deps = p[role == 'dependent'][order(unit_id, AGE, PERNUM)]
  stopifnot('is_qc' %in% names(deps), !anyNA(deps$is_qc))
  # child credit children: qualifying children under 17; EITC children: qualifying children (tax_units.py's rule)
  dtab = deps[, .(n_dep_raw = .N, n13 = sum(AGE < 13), n6 = sum(AGE < 6),
                  n_ctc_raw = sum(is_qc & AGE < 17), n_eitc_raw = sum(is_qc),
                  dep_age1 = as.numeric(AGE[1]), dep_age2 = if (.N >= 2) as.numeric(AGE[2]) else -1,
                  dep_age3 = if (.N >= 3) as.numeric(AGE[3]) else -1), by = unit_id]
  cat(sprintf('  dependents: %.2f m weighted (%.2f m aged 18+); units with more than 3: %d\n',
              sum(deps$ASECWT) / 1e6, sum(deps$ASECWT[deps$AGE >= 18]) / 1e6, sum(dtab$n_dep_raw > 3)))

  #-----------------------------------------------------------------------------
  # Child care: the SPM unit's expenses to the units that claim its children
  #-----------------------------------------------------------------------------
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
  alloc[, childcare := chx * share]
  cat(sprintf('  SPM units paying for child care: %d; to units claiming their children %d, to the reference person\'s unit %d\n',
              nrow(paid), nrow(paid[SPMFAMUNIT %in% kids$SPMFAMUNIT]), nrow(paid[!SPMFAMUNIT %in% kids$SPMFAMUNIT])))
  chk_spm = alloc[, .(got = sum(childcare), chx = chx[1]), by = SPMFAMUNIT]
  stopifnot(nrow(chk_spm) == nrow(paid), max(abs(chk_spm$got - chk_spm$chx)) < 1e-6)
  cc = alloc[, .(childcare = sum(childcare)), by = unit_id]
  stopifnot(all(cc$unit_id %in% units$unit_id))

  #-----------------------------------------------------------------------------
  # Assemble
  #-----------------------------------------------------------------------------
  # EITC eligibility under the tax year's law: step 1's (filing.py, eitc_eligible: earned income and AGI below
  # the phase-out end for the number of children and filing status, investment income under the limit, the age
  # range for no children, none on separate returns)
  stopifnot(!anyNA(units$eitc_eligible))
  units[, eitc_ok := as.integer(eitc_eligible)]
  out = units[, .(unit_id, SERIAL, weight, filing_status, must_file, p_file = p_file_cal, eitc_ok,
                  wages = INCWAGE, sch_cf_income = se_income, interest = INCINT, dividends = INCDIVID,
                  ss_gross = INCSS, unemployment = INCUNEMP, retirement_income, rent_royalty_estate = INCRENT)]
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
  # not in universe: no spouse (person 2 zero, age -1, sex 0); no dependents (0, ages -1); no child care (0)
  out[married == 0, `:=`(age2 = -1, sex2 = 0, wages2 = 0, se2 = 0, weeks_worked2 = 0, usual_hours2 = 0,
                         overtime2 = fifelse(ot_unit_na, NA_real_, 0))]
  out[is.na(n_dep_raw), `:=`(n_dep_raw = 0L, n13 = 0L, n6 = 0L, n_ctc_raw = 0L, n_eitc_raw = 0L,
                             dep_age1 = -1, dep_age2 = -1, dep_age3 = -1)]
  stopifnot(all(out$filing_status %in% c('single', 'joint', 'separate', 'hoh')))
  out[is.na(childcare), childcare := 0]

  # split sums: exact
  stopifnot(identical(out$wages1 + out$wages2, out$wages), identical(out$se1 + out$se2, out$sch_cf_income),
            identical(out$survdis_pension + out$survdis_other, out$survivor_disability))

  setorder(out, unit_id)
  stopifnot(!anyDuplicated(out$unit_id), all(out$unit_id == round(out$unit_id)), max(out$unit_id) < 2^31)
  set.seed(as.integer(settings$cps$filer_seed) + yr)
  draw = runif(nrow(out))
  out = out[, .(year = yr, record_id = as.integer(unit_id), household_id = as.integer(SERIAL), weight,
                married, n_dep = pmin(n_dep_raw, 3L), dep_filer = 0L,
                head_of_household = as.integer(filing_status == 'hoh'),
                mars = fcase(filing_status == 'single', 1L, filing_status == 'joint', 2L,
                             filing_status == 'separate', 3L, filing_status == 'hoh', 4L),
                # EITC children as the PUF's EIC means them: the qualifying children of a unit eligible for the
                # credit (step 1's eligibility)
                n_ctc = pmin(n_ctc_raw, 3L), n_eitc = pmin(n_eitc_raw, 3L) * eitc_ok,
                eitc_eligible = eitc_ok,
                filer = as.integer(draw < p_file), p_file, must_file = as.integer(must_file),
                age1 = as.integer(age1), age2 = as.integer(age2), sex1 = as.integer(sex1), sex2 = as.integer(sex2),
                male1 = as.integer(sex1 == 1), male2 = fifelse(married == 1, as.integer(sex2 == 1), -1L),
                dep_age1 = as.integer(dep_age1), dep_age2 = as.integer(dep_age2), dep_age3 = as.integer(dep_age3),
                n_dep_under13 = pmin(n13, 3L), n_dep_under6 = pmin(n6, 3L),
                wages, sch_cf_income, interest, dividends, ss_gross, unemployment, retirement_income,
                rent_royalty_estate, survivor_disability, survdis_pension, survdis_other, other_income, capital_gains,
                wages1, wages2, se1, se2, weeks_worked1, weeks_worked2, usual_hours1, usual_hours2, childcare,
                overtime1, overtime2)]
  # overtime: missing on both adults together, never above the person's wages
  stopifnot(identical(is.na(out$overtime1), is.na(out$overtime2)),
            all(out$overtime1 <= out$wages1 + 1e-6 & out$overtime1 >= 0, na.rm = TRUE),
            all(out$overtime2 <= out$wages2 + 1e-6 & out$overtime2 >= 0, na.rm = TRUE),
            all(out[married == 0 & !is.na(overtime2), overtime2] == 0))
  cat(sprintf('  overtime on units: asked %d of %d units (%.1f%% of the weight); missing on %d units (%d of them asked but with a worker\'s pay missing)\n',
              sum(!is.na(out$overtime1)), nrow(out), 100 * sum(out$weight[!is.na(out$overtime1)]) / sum(out$weight),
              sum(is.na(out$overtime1)), sum(is.na(out$overtime1) & out$record_id %in% ot_asked_units)))
  stopifnot(!anyNA(out[, !c('overtime1', 'overtime2')]), all(out$weight > 0), all(out$n_dep_under13 <= out$n_dep),
            all(out$n_eitc <= out$n_dep), all((out$mars == 2) == (out$married == 1)),
            all((out$dep_age1 >= 0) == (out$n_dep > 0)))
  fwrite(out, file.path(OUT, sprintf('cps_units_%d.csv.gz', yr)))
  cat(sprintf('  wrote %s\n', file.path(OUT, sprintf('cps_units_%d.csv.gz', yr))))

  #-----------------------------------------------------------------------------
  # Diagnostics
  #-----------------------------------------------------------------------------
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
  if (all(out$eitc_eligible >= 0)) {
    ef = out[, .(eitc_returns_expected_filers_m = sum(weight * p_file * eitc_eligible) / 1e6,
                 eitc_children_expected_filers_m = sum(weight * p_file * n_eitc) / 1e6), by = .(children = n_eitc)]
    setorder(ef, children)
    cat(sprintf('  EITC (step 1\'s eligibility): EITC children on expected filers %.2f m (capped at 3 per unit); EITC returns, expected filers, by children:\n',
                sum(out$weight * out$p_file * out$n_eitc) / 1e6))
    print(ef[, .(children, eitc_returns_m = round(eitc_returns_expected_filers_m, 2))])
    fwrite(cbind(year = yr, ef), file.path(OUT, sprintf('cps_eitc_%d.csv', yr)))
  }
  cat(sprintf('  units with person 1 under 18: %d rows, %.2f m weighted; dependents claimed, counted by the claiming unit\'s weight, capped at 3: %.2f m\n',
              sum(out$age1 < 18), sum(w[out$age1 < 18]) / 1e6, sum(w * out$n_dep) / 1e6))
  spm_total_spmwt = sum(spm$chx * spm$spmwt) / 1e9
  spm_total_refw = sum(spm$chx * spm$ref_w) / 1e9
  amounts = c('wages', 'sch_cf_income', 'interest', 'dividends', 'ss_gross', 'unemployment', 'retirement_income',
              'rent_royalty_estate', 'survivor_disability', 'survdis_pension', 'survdis_other', 'other_income',
              'capital_gains', 'childcare')
  tot = data.table(year = yr, item = amounts, billions = sapply(amounts, function(v) bn(w, out[[v]])),
                   recipients_m = sapply(amounts, function(v) sum(w * (out[[v]] != 0)) / 1e6))
  print(tot[, .(item, billions = round(billions, 1), recipients_m = round(recipients_m, 2))])
  cat(sprintf('  child care: tax units $%.2f bn; SPM units $%.2f bn weighted by SPMWT, $%.2f bn by the reference person\'s ASECWT; unweighted sums agree exactly\n',
              bn(w, out$childcare), spm_total_spmwt, spm_total_refw))
  summary_rows[[as.character(yr)]] = rbind(
    tot,
    data.table(year = yr, item = c('weighted_units_m', 'married_share', 'filer_share', 'expected_filer_share',
                                   'must_file_share', 'filestat_returns_m', 'pool_household_nonfilers_m',
                                   'calibrated_filing_model'),
               billions = c(sum(w) / 1e6, sum(w * out$married) / sum(w), sum(w * out$filer) / sum(w),
                            sum(w * out$p_file) / sum(w), sum(w * out$must_file) / sum(w), fs_returns / 1e6,
                            pool_hh / 1e6, as.numeric(calibrated)),
               recipients_m = NA_real_))
}

s = rbindlist(summary_rows)
# the summary file holds every year built so far
sp = file.path(OUT, 'cps_summary.csv')
if (file.exists(sp)) s = rbind(fread(sp)[!year %in% YEARS], s)
fwrite(s, sp)
cat('\nwrote', sp, '\n')
# the survivor and disability sources' table holds every year built so far
sv = rbindlist(sources_rows)
svp = file.path(OUT, 'cps_survivor_disability_sources.csv')
if (file.exists(svp)) sv = rbind(fread(svp)[!year %in% YEARS], sv)
setorder(sv, year)
fwrite(sv, svp)
cat('wrote', svp, '\n')
