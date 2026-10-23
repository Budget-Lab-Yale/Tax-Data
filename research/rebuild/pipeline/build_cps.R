#-------------------------------------------------------------------------------
# build_cps.R: the CPS ASEC split into tax units, tax years 2015, 2017, 2019,
# 2021 and 2023 (settings "cps"). The ASEC asking about tax year Y is the
# survey of March Y+1 (read_asec(Y) maps this).
#
# Tax units. Production's builder, unchanged (src/nonfilers/asec_tax_units.R,
# build_asec_tax_units, the rules of the non-filer pool): spouses are one unit;
# children attach to a parent under the qualifying-child tests; a related
# subfamily whose head has no earnings, and low-income relatives who are not
# self-supporting, become dependents of the householder. The frame is the
# household population, as in the pool: people in group quarters (GQ 2) are
# left out, and their weighted count is printed. With settings cps.tax_units = "cbo" the units are instead CBO's
# rules (tax_units.py, read through tax_units_cbo.R) and the calibration is the one redone on those units
# (cps.cbo_pool_dir); the default, "production", is the builder above.
#
# Dependent filers. The builder also makes a scoring unit for every dependent
# aged 15 and over (to score their own filing). Those rows are dropped here:
# the dependent is counted once, in the n_dep of the unit that claims them, and
# their own income goes nowhere (it belongs on their own return). A unit's
# income is the primary filer's plus the spouse's, as in the builder.
#
# Filing. filer is drawn from the production filing model as the non-filer
# pool applies it: Mok's probits below the filing threshold and the Pub 5785
# hazard above it (score_filing_model), then the pool's calibration (script
# 04: one intercept shift per age band of the primary filer and the wage
# coefficient adjustment, read from nonfiler_pool/results/calibration_<year>.csv)
# for units below the threshold.
#
# The calibration is a standard step here, not a file someone must have made
# first. When a year has no calibration_<year>.csv, this script runs the pool's
# own steps for that year, unchanged (nonfiler_pool/01_build_units.R,
# 02_filing_model.R, 03_gq_backfill.R, 04_calibrate.R, each with the year as
# its argument), which write their results where the pool keeps them, and then
# reads the file as for any other year. There is one implementation of the
# calibration: the pool's. A year whose inputs are missing stops with the list
# of what is missing; nothing is built from the uncalibrated model. What a
# year needs, beyond its ASEC and the thresholds in filing_requirement_params():
#   - the national residual by age band: Census population adults by band less
#     SOI filing adults by band (Pub 1304 Table 1.6), as
#     nonfiler_residual/resources/nonfiler_age_shape_<year>.csv and
#     nonfiler_residual/results/national_anchor_<year>.csv
#     (nonfiler_residual/02_build_residual_anchors.R);
#   - the Pub 1304 Table 1.6 workbook itself (returns_marital_age_<year>.xls,
#     step 01's check of filing status counts);
#   - the ASEC income totals of nonfiler_residual/09 (asec_A4_income_<year>.csv,
#     step 01's check that wages are read the same way);
#   - Pub 5785's count of non-filers above the filing threshold: published for
#     2014-2016, otherwise a row of nonfiler_pool/results/
#     pub5785_projected_targets.csv (nonfiler_pool/10_project_targets.R);
#   - the ACS 1-year extract for that year (group quarters, step 03).
# The ACS read in step 03 needs about 48 GB of memory. p_file is the
# probability; filer = 1 when a uniform draw is below it (one draw per unit,
# seeded by settings cps.filer_seed + the year, in record_id order). must_file
# is the statutory filing requirement (add_filing_requirement).
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
# Columns, one row per tax unit, no NA. Dollars are nominal dollars of the tax
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
#                         pensions other than Social Security)
#     other_income        INCOTHER (includes alimony)
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
# Not included: capital gains (INCCAPG exists only from tax year 2018).
#
# Paths and years from settings.json ("cps"); the settings file is
# REBUILD_SETTINGS if set, else the pipeline's own; output to
# REBUILD_OUT/data. Optional arguments name tax years to build (default: all
# of settings cps.years). Writes {out}/data/cps_units_{year}.csv.gz and
# cps_summary.csv (the printed totals per year). Run from the repository root
# through Slurm. The R modules lack ipumsr; it is installed (ipumsr 0.10.0,
# hipread 0.2.6) in John's library for R/4.4.1-foss-2022b,
# R_LIBS=$HOME/R/4.4.1-foss-2022b (~jar335). The pool's steps, when this script
# runs them, inherit the same R and library:
#   sbatch build_cps.sbatch [years]   (in /nfs/roberts/scratch/pi_nrs36/jar335/rebuild_cps/logs/)
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
source('src/nonfilers/asec_tax_units.R')
source('src/nonfilers/filing_model.R')
POOL_RES = 'research/state_weights/nonfiler_pool/results'
# Which tax units (settings cps.tax_units): 'production' (the default; production's builder and the pool's own
# calibration) or 'cbo' (CBO's rules from tax_units.py, read from the roster files in cps.cbo_units_dir, with the
# calibration redone on those units in cps.cbo_pool_dir; see tax_units_cbo.R)
TAX_UNITS = if (is.null(settings$cps$tax_units)) 'production' else settings$cps$tax_units
stopifnot(TAX_UNITS %in% c('production', 'cbo'))
if (TAX_UNITS == 'cbo') {
  source('research/rebuild/pipeline/tax_units_cbo.R')
  POOL_RES = settings$cps$cbo_pool_dir
  cat(sprintf('tax units: CBO rules (variant %s), calibration from %s\n', settings$cps$cbo_variant, POOL_RES))
}
POOL_FILE = settings$paths$raw$nonfiler_pool
mok = read_mok_coefs()
POOL_DIR = 'research/state_weights/nonfiler_pool'
RESID_DIR = 'research/state_weights/nonfiler_residual'

# The inputs the pool's steps 01-04 read for one tax year, beyond the ASEC.
# Returns a description of each one that is missing (none: the year can be
# calibrated).
pool_inputs_missing = function(yr) {
  files = c(
    'non-filer age shape (population adults less SOI filing adults, by age band)' =
      file.path(RESID_DIR, 'resources', sprintf('nonfiler_age_shape_%d.csv', yr)),
    'national residual anchor' = file.path(RESID_DIR, 'results', sprintf('national_anchor_%d.csv', yr)),
    'SOI Pub 1304 Table 1.6 workbook' =
      file.path(raw_data_root(), 'IRS-Ind/national/by_size', sprintf('returns_marital_age_%d.xls', yr)),
    'ASEC income totals of nonfiler_residual/09' =
      file.path(RESID_DIR, 'results', sprintf('asec_A4_income_%d.csv', yr)),
    'ACS 1-year extract' = file.path(acs_extract_dir(yr), sprintf('usa_%da.dat.gz', yr)))
  miss = sprintf('%s (%s)', names(files), files)[!file.exists(files)]
  projected = if (file.exists(PUB5785_PROJECTED_PATH)) fread(PUB5785_PROJECTED_PATH)$tax_year else integer(0)
  if (!(yr %in% PUB5785_YEARS) && !(yr %in% projected))
    miss = c(miss, sprintf('Pub 5785 target: not a published year (%s) and no row in %s',
                           paste(range(PUB5785_YEARS), collapse = '-'), PUB5785_PROJECTED_PATH))
  if (is.null(tryCatch(filing_requirement_params(yr), error = function(e) NULL)))
    miss = c(miss, 'filing thresholds: no entry in filing_requirement_params()')
  miss
}

# Run the pool's own steps 01-04 for one year (they write to POOL_DIR/results)
run_pool_calibration = function(yr) {
  miss = pool_inputs_missing(yr)
  if (length(miss))
    stop(sprintf('tax year %d has no pool calibration and cannot be calibrated; missing inputs:\n  %s',
                 yr, paste(miss, collapse = '\n  ')))
  for (s in c('01_build_units', '02_filing_model', '03_gq_backfill', '04_calibrate')) {
    cat(sprintf('  running the pool\'s %s.R for %d\n', s, yr))
    status = system2('Rscript', c(file.path(POOL_DIR, paste0(s, '.R')), yr))
    if (status != 0) stop(sprintf('the pool\'s %s.R failed for %d (exit status %d)', s, yr, status))
  }
}

report_na = function(df, label) {
  na = colSums(is.na(df))
  na = na[na > 0]
  if (length(na) == 0) cat(sprintf('  %s: no NA in any column\n', label))
  else { cat(sprintf('  %s: NA counts\n', label)); print(na) }
}
bn = function(w, x) sum(w * x) / 1e9

RAW_USED = c('SERIAL', 'PERNUM', 'AGE', 'SEX', 'RELATE', 'MARST', 'SPLOC', 'MOMLOC', 'POPLOC', 'FTYPE', 'FAMID',
             'FAMREL', 'SCHLCOLL', 'ASECWT', 'GQ', 'EDUC', 'RACE', 'HISPAN', 'HIMCAIDLY', 'FOODSTAMP', 'INCWELFR',
             'INCTOT', 'INCSS', ASEC_GROSS_INCOME_ITEMS, 'WKSWORK1', 'UHRSWORKLY', 'SPMFAMUNIT', 'SPMCHXPNS',
             'SPMWT', 'FILESTAT')
summary_rows = list()

for (yr in YEARS) {
  cat(sprintf('\n=== tax year %d (ASEC %d)\n', yr, yr + 1))
  # The pool's calibration for this year; when there is none, the pool's own steps make it now
  cal_path = file.path(POOL_RES, sprintf('calibration_%d.csv', yr))
  if (!file.exists(cal_path) && TAX_UNITS == 'cbo')
    stop(sprintf('no calibration of the CBO-rule units for %d (%s): run calibrate_cbo.R first', yr, cal_path))
  if (!file.exists(cal_path)) {
    cat(sprintf('  no pool calibration file for %d: running the pool\'s steps 01-04\n', yr))
    run_pool_calibration(yr)
  }
  x = read_asec(yr)
  ddi = attr(x, 'ddi')
  ret_cols = if (yr <= 2017) 'INCRETIR' else ASEC_RETIREMENT_COMPONENTS
  report_na(x[, c(RAW_USED, ret_cols), with = FALSE], 'raw ASEC columns used')
  cat(sprintf('  %d person records, %d households\n', nrow(x), uniqueN(x$SERIAL)))

  # Income concepts and the filing model's inputs, cleaned as the pool's driver (01_build_units.R) does
  x[, INCSS := clean_asec_income(x, ddi, 'INCSS')]
  x = add_asec_income_concepts(x, ddi)
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

  #-----------------------------------------------------------------------------
  # Units and the filing model (production code)
  #-----------------------------------------------------------------------------
  built = if (TAX_UNITS == 'cbo') {
    cbo_tax_units(x, yr, file.path(settings$cps$cbo_units_dir,
                                   sprintf('roster_%d_%s_nogq.csv.gz', yr, settings$cps$cbo_variant)))
  } else build_asec_tax_units(x, yr)
  p = built$persons
  u = add_filing_requirement(built$units, yr)
  u = add_mok_covariates(u, p)
  tg = pub5785_targets_for_year(yr)
  adj = pandemic_filing_adjustment(yr)
  if (!is.null(adj)) cat('  pandemic hazard deflation applied (S20)\n')
  u = score_filing_model(u, mok$coefs, tg, band_deflators = adj)
  cat(sprintf('  Pub 5785 basis: %s\n', tg$basis))

  # Reproduction check against the pool's committed scoring gates (before calibration)
  gates_path = file.path(POOL_RES, sprintf('filing_model_gates_%d.csv', yr))
  if (file.exists(gates_path)) {
    g = fread(gates_path)
    ours = u[, .(units_M = sum(weight) / 1e6, p_file = sum(weight * p_file) / sum(weight)), by = mok_group]
    cmp = merge(g[, .(mok_group, pool_units_M = units_M, pool_p_file = p_file)], ours, by = 'mok_group')
    cat('  filing model before calibration, by Mok group, against the pool\'s gates file:\n')
    print(cmp[, .(mok_group, pool_units_M = round(pool_units_M, 3), units_M = round(units_M, 3),
                  pool_p_file = round(pool_p_file, 4), p_file = round(p_file, 4))])
    cat(sprintf('  largest gap: units %.2e m, p_file %.2e\n', max(abs(cmp$units_M - cmp$pool_units_M)),
                max(abs(cmp$p_file - cmp$pool_p_file))))
  }

  # The pool's calibration (04_calibrate.R, central scenario) for units below the threshold. The file
  # exists for every year by now: made at the top of this loop when it was missing.
  stopifnot(file.exists(cal_path))
  u[, band := as.character(age_band(age_head))]
  u[, p_file_cal := p_file]
  cal = fread(cal_path)[scenario == 'central']
  stopifnot(nrow(cal) == 7, uniqueN(cal$gamma) == 1, !anyNA(cal$delta))
  band_map = setNames(cal$delta, cal$band)
  gam = cal$gamma[1]
  u[unit_type == 'nondependent' & age_head >= 18 & must_file == FALSE,
    p_file_cal := pnorm(qnorm(p_file_mok) + gam * src_wages + band_map[band])]
  stopifnot(!anyNA(u$p_file_cal))
  chk = u[unit_type == 'nondependent' & age_head >= 18,
          .(pool = sum(weight * fifelse(filing_status == 'joint', 2, 1) * (1 - p_file_cal))), by = band]
  chk = merge(chk, cal[, .(band, achieved, gq_nonfiling)], by = 'band')
  cat(sprintf('  calibration applied (gamma %.3f); non-filing adults in household units by band against the pool\'s calibration file:\n', gam))
  print(chk[, .(band, ours_M = round(pool / 1e6, 4), pool_M = round((achieved - gq_nonfiling) / 1e6, 4))])
  # the units here are the pool's units, so the calibration must land on the pool's numbers (within one person)
  stopifnot(nrow(chk) == 7, max(abs(chk$pool - (chk$achieved - chk$gq_nonfiling))) < 1)
  calibrated = TRUE

  #-----------------------------------------------------------------------------
  # Persons 1 and 2 of each unit
  #-----------------------------------------------------------------------------
  units = u[unit_type == 'nondependent']
  dep_units = u[unit_type == 'dependent']
  cat(sprintf('  units: %d (%.2f m weighted); dependent scoring units dropped: %d (%.2f m weighted, own wages $%.1f bn)\n',
              nrow(units), sum(units$weight) / 1e6, nrow(dep_units), sum(dep_units$weight) / 1e6,
              bn(dep_units$weight, dep_units$INCWAGE)))

  hs = p[role %in% c('primary', 'spouse'),
         .(unit_id, PERNUM, role, AGE, SEX, INCWAGE, se = INCBUS + INCFARM, weeks = as.numeric(WKSWORK1),
           hours = usual_hours)]
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
  p1 = hs[pos == 1, .(unit_id, age1 = AGE, sex1 = SEX, wages1 = INCWAGE, se1 = se, weeks_worked1 = weeks,
                      usual_hours1 = hours)]
  p2 = hs[pos == 2, .(unit_id, age2 = AGE, sex2 = SEX, wages2 = INCWAGE, se2 = se, weeks_worked2 = weeks,
                      usual_hours2 = hours)]

  #-----------------------------------------------------------------------------
  # Dependents
  #-----------------------------------------------------------------------------
  deps = p[role == 'dependent'][order(unit_id, AGE, PERNUM)]
  dtab = deps[, .(n_dep_raw = .N, n13 = sum(AGE < 13), n6 = sum(AGE < 6),
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
  out = units[, .(unit_id, SERIAL, weight, filing_status, must_file, p_file = p_file_cal,
                  wages = INCWAGE, sch_cf_income = se_income, interest = INCINT, dividends = INCDIVID,
                  ss_gross = INCSS, unemployment = INCUNEMP, retirement_income, rent_royalty_estate = INCRENT)]
  extra = p[role %in% c('primary', 'spouse'),
            .(survivor_disability = sum(INCSURV + INCDISAB), other_income = sum(INCOTHER)), by = unit_id]
  out = merge(out, extra, by = 'unit_id', all.x = TRUE)
  out = merge(out, p1, by = 'unit_id', all.x = TRUE)
  out = merge(out, p2, by = 'unit_id', all.x = TRUE)
  out = merge(out, dtab, by = 'unit_id', all.x = TRUE)
  out = merge(out, cc, by = 'unit_id', all.x = TRUE)
  stopifnot(nrow(out) == nrow(units), !anyNA(out$age1), !anyNA(out$survivor_disability))
  cat('  NA counts after the joins, before filling (expected: person 2 on units with no spouse, dependents on units with none, child care on units given none):\n')
  na = colSums(is.na(out)); print(na[na > 0])
  married = out$filing_status == 'joint'
  stopifnot(all(is.na(out$age2) == !married), all(is.na(out$n_dep_raw) == (out$unit_id %in% dtab$unit_id == FALSE)))
  out[, married := as.integer(filing_status == 'joint')]
  # not in universe: no spouse (person 2 zero, age -1, sex 0); no dependents (0, ages -1); no child care (0)
  out[married == 0, `:=`(age2 = -1, sex2 = 0, wages2 = 0, se2 = 0, weeks_worked2 = 0, usual_hours2 = 0)]
  out[is.na(n_dep_raw), `:=`(n_dep_raw = 0L, n13 = 0L, n6 = 0L, dep_age1 = -1, dep_age2 = -1, dep_age3 = -1)]
  out[is.na(childcare), childcare := 0]

  # split sums: exact
  stopifnot(identical(out$wages1 + out$wages2, out$wages), identical(out$se1 + out$se2, out$sch_cf_income))

  setorder(out, unit_id)
  stopifnot(!anyDuplicated(out$unit_id), all(out$unit_id == round(out$unit_id)), max(out$unit_id) < 2^31)
  set.seed(as.integer(settings$cps$filer_seed) + yr)
  draw = runif(nrow(out))
  out = out[, .(year = yr, record_id = as.integer(unit_id), household_id = as.integer(SERIAL), weight,
                married, n_dep = pmin(n_dep_raw, 3L), dep_filer = 0L,
                head_of_household = as.integer(filing_status == 'hoh'),
                filer = as.integer(draw < p_file), p_file, must_file = as.integer(must_file),
                age1 = as.integer(age1), age2 = as.integer(age2), sex1 = as.integer(sex1), sex2 = as.integer(sex2),
                male1 = as.integer(sex1 == 1), male2 = fifelse(married == 1, as.integer(sex2 == 1), -1L),
                dep_age1 = as.integer(dep_age1), dep_age2 = as.integer(dep_age2), dep_age3 = as.integer(dep_age3),
                n_dep_under13 = pmin(n13, 3L), n_dep_under6 = pmin(n6, 3L),
                wages, sch_cf_income, interest, dividends, ss_gross, unemployment, retirement_income,
                rent_royalty_estate, survivor_disability, other_income,
                wages1, wages2, se1, se2, weeks_worked1, weeks_worked2, usual_hours1, usual_hours2, childcare)]
  stopifnot(!anyNA(out), all(out$weight > 0), all(out$n_dep_under13 <= out$n_dep),
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
  cat(sprintf('  units with person 1 under 18: %d rows, %.2f m weighted; dependents claimed, counted by the claiming unit\'s weight, capped at 3: %.2f m\n',
              sum(out$age1 < 18), sum(w[out$age1 < 18]) / 1e6, sum(w * out$n_dep) / 1e6))
  spm_total_spmwt = sum(spm$chx * spm$spmwt) / 1e9
  spm_total_refw = sum(spm$chx * spm$ref_w) / 1e9
  amounts = c('wages', 'sch_cf_income', 'interest', 'dividends', 'ss_gross', 'unemployment', 'retirement_income',
              'rent_royalty_estate', 'survivor_disability', 'other_income', 'childcare')
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
