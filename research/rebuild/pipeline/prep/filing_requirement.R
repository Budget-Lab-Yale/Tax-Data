#-------------------------------------------------------------------------------
# The statutory filing requirement of every CPS tax unit, the middle step of the
# filing stage. Reads the cleaned ASEC and the tax-unit roster filing.py wrote;
# applies production's add_filing_requirement() (Form 1040, Charts A to C,
# including a row for each dependent aged 15 and over); writes units_<year> and
# persons_<year> under data/filing/requirement/. Run from the repository root.
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
