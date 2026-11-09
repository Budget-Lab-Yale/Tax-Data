#-------------------------------------------------------------------------------
# filing_requirement.R [year ...]: the statutory filing requirement of every CPS
# tax unit, the second part of the filing stage (filing.py runs the first and the
# third; run.py, stage filing).
#
# The ASEC is cleaned as build_cps.R cleans it, people in group quarters (GQ 2)
# are left out, and the units are the roster filing.py wrote from the shared
# tax-unit rules (tax_units.py), turned into production's unit table by
# cbo_tax_units() (tax_units_cbo.R): head and spouse income summed, and a row for
# every dependent aged 15 and over, for that dependent's own return. The filing
# requirement is production's add_filing_requirement()
# (src/nonfilers/asec_tax_units.R): gross income at or above the threshold for
# the unit's filing status and age (Form 1040, Chart A), the dependent tests
# (Chart B) on the dependents' rows, and net self-employment income of $400 or
# more (Chart C). Nothing else is scored: no outside filing model.
#
# Writes, under <run's data folder>/<cps.step1_dir>/requirement/:
#   units_<year>.csv.gz    one row per unit (household units, unit_type
#                          'nondependent', and dependents' rows, 'dependent'):
#                          unit_id, unit_type, SERIAL, head_pernum (the head's
#                          PERNUM; on a dependent's row, the dependent's),
#                          weight, age_head, must_file, threshold (the unit's
#                          Chart A gross income threshold), INCWAGE
#   persons_<year>.csv.gz  one row per person: SERIAL, PERNUM, unit_id, role,
#                          AGE, ASECWT, INCWAGE (filing.py checks that these are
#                          the same people in the same units as its own)
# Optional arguments name tax years (default settings cps.years). Run from the
# repository root through Slurm; ipumsr is in John's library
# (R_LIBS=$HOME/R/4.4.1-foss-2022b), as for build_cps.R.
#-------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(ipumsr); library(stringr); library(dplyr); library(yaml); library(jsonlite)
})
settings = fromJSON(Sys.getenv('REBUILD_SETTINGS', 'research/rebuild/pipeline/settings.json'))
DIR = file.path(Sys.getenv('REBUILD_OUT', settings$paths$out), 'data', settings$cps$step1_dir)
args = commandArgs(trailingOnly = TRUE)
YEARS = if (length(args)) as.integer(args) else as.integer(settings$cps$years)
stopifnot(all(YEARS %in% settings$cps$years))
source('src/nonfilers/state_weights.R')     # raw_data_root(), age_band()
source('src/nonfilers/asec_tax_units.R')    # read_asec(), the income cleaning, build_asec_unit_table(), add_filing_requirement()
source('research/rebuild/pipeline/prep/tax_units_cbo.R')
dir.create(file.path(DIR, 'requirement'), recursive = TRUE, showWarnings = FALSE)

for (yr in YEARS) {
  cat(sprintf('\n=== tax year %d (ASEC %d)\n', yr, yr + 1))
  x = read_asec(yr)
  ddi = attr(x, 'ddi')
  x[, INCSS := clean_asec_income(x, ddi, 'INCSS')]
  x = add_asec_income_concepts(x, ddi)
  x = x[GQ != 2]
  built = cbo_tax_units(x, yr, file.path(DIR, 'units', sprintf('roster_%d_cbo_nogq.csv.gz', yr)))
  u = add_filing_requirement(built$units, yr)
  keep = c('unit_id', 'unit_type', 'SERIAL', 'head_pernum', 'weight', 'age_head', 'must_file', 'threshold', 'INCWAGE')
  out = u[, ..keep]
  stopifnot(!anyNA(out))
  cat(sprintf('TY%d: %.2f m household units, %.2f m of them required to file; %.2f m dependents\' rows (aged 15+)\n',
              yr, out[unit_type == 'nondependent', sum(weight)] / 1e6,
              out[unit_type == 'nondependent' & must_file == TRUE, sum(weight)] / 1e6,
              out[unit_type == 'dependent', sum(weight)] / 1e6))
  fwrite(out, file.path(DIR, 'requirement', sprintf('units_%d.csv.gz', yr)))
  pp = built$persons[, .(SERIAL, PERNUM, unit_id, role, AGE, ASECWT, INCWAGE)]
  stopifnot(!anyNA(pp))
  fwrite(pp, file.path(DIR, 'requirement', sprintf('persons_%d.csv.gz', yr)))
}
cat('done\n')
