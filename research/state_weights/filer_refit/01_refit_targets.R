#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 01_refit_targets.R  (Phase 3b step 2, S40)
#
# Builds the targets for the per-year national filer refit, TY2017-2023, from
# the IRS-Ind aligned panels of Pub 1304 (read through src/nonfilers/sw_soi.R)
# into a committed file, resources/filer_refit_targets.csv, so the pipeline
# never reads the unversioned IRS-Ind store directly.
#
# Why: 2018-2023 filer weights are the 2017 weights scaled per filing status
# x age group (compute_weights_for_year(), src/project_puf.R), which freezes
# each cell's 2017 income mix; TY2023 wage filers at AGI $1-10k come out ~30%
# above the IRS, ~2.9M of the ~3.0M excess dependent filers
# (research/state_weights/notes/low_income_filer_aging.md).
#
# Output: one row per member cell of a target, long form, so a target spanning
# several cells (a Table 1.6 block of combined cells) is one target_id:
#   year, target_id, source, filing_status, age_lo, age_hi, dependent,
#   wages, agi_lo, agi_hi, target_value
# with NA in a dimension meaning "all" and
#   source  t16     returns by filing status x age x AGI class (Table 1.6
#                   status blocks; blocks of combined cells as one target)
#           t16_u18 the all-returns panel's under-18 returns by AGI class, the
#                   one age split the status blocks lack (a block holding an
#                   under-18 cell comes whole, with its 18-25 / 26-34 cells)
#           t14_wages   returns with wages by AGI class (Table 1.4)
#           t17_all     dependent returns by AGI class (Table 1.7)
#           t17_wages   dependent returns with wages by AGI class (Table 1.7)
#   filing_status  Tax-Data codes: 1 single, 2 joint incl. surviving spouses,
#                  3 married filing separately, 4 head of household
#   age_lo/age_hi  primary taxpayer's age, [lo, hi) (-Inf for 'under')
#   dependent      1 = return of a person claimed as a dependent
#   wages          1 = return with wages
#   agi_lo/agi_hi  dollars, [lo, hi); 'No adjusted gross income' is (-Inf, 1)
#
# Gates (the run stops on failure; the table is written to
# results/refit_target_checks.csv):
#   * each t16 status block's targets sum to resources/return_counts_2023.csv's
#     status total (the file that ages filer weights today, built from the raw
#     sheets by a different reader)
#   * each (status, age group) no block crosses sums to return_counts_2023.csv
#   * t14_wages and t17 classes sum to their published totals
#
# Login-node safe, seconds.
#   Rscript research/state_weights/filer_refit/01_refit_targets.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(yaml) })
source('src/nonfilers/state_weights.R')

YEARS    <- 2017:2023
OUT      <- 'resources/filer_refit_targets.csv'
RES      <- 'research/state_weights/filer_refit/results'
COUNTS   <- 'resources/return_counts_2023.csv'
# Published counts are whole returns; a total can miss the sum of n rounded
# cells by up to about n/2.
tolerance <- function(n_cells) n_cells

AGE_BOUNDS <- list('under 18' = c(-Inf, 18), '18 under 26' = c(18, 26),
                   'under 26' = c(-Inf, 26), '26 under 35' = c(26, 35),
                   '35 under 45' = c(35, 45), '45 under 55' = c(45, 55),
                   '55 under 65' = c(55, 65), '65 and over' = c(65, Inf))

#-----------------------
# Table 1.6
#-----------------------

t16 <- read_t16_targets(YEARS)
t16[, `:=`(age_lo = vapply(age, function(a) AGE_BOUNDS[[a]][1], numeric(1)),
           age_hi = vapply(age, function(a) AGE_BOUNDS[[a]][2], numeric(1)))]
# a target is a block, or a cell on its own
t16[, unit := fifelse(is.na(block_id),
                      sprintf('%d|%s|%s|%s', tax_year, panel, age, agi_lo), block_id)]
t16[, target_value := fifelse(is.na(block_id), as.numeric(value), as.numeric(block_value))]
stopifnot(!anyNA(t16$target_value))

status_cells <- t16[!is.na(filing_status)]
u18_units    <- t16[is.na(filing_status) & age == 'under 18', unique(unit)]
u18_cells    <- t16[is.na(filing_status) & unit %in% u18_units]

as_targets <- function(d, source_name) {
  d[, .(year = tax_year, unit, source = source_name,
        filing_status = if (source_name == 't16') filing_status else NA_integer_,
        age_lo, age_hi, dependent = NA_integer_, wages = NA_integer_,
        agi_lo, agi_hi, target_value)]
}
targets_t16 <- rbind(as_targets(status_cells, 't16'), as_targets(u18_cells, 't16_u18'))

#-----------------------
# Tables 1.4 and 1.7
#-----------------------

class_targets <- function(d, source_name, dependent, wages) {
  cls <- d[!is.na(agi_lo)]
  stopifnot(!anyNA(cls$value))   # no class of these series is combined or suppressed
  cls[, .(year = tax_year, unit = sprintf('%d|%s|%s', tax_year, source_name, agi_lo),
          source = source_name, filing_status = NA_integer_,
          age_lo = NA_real_, age_hi = NA_real_, dependent = dependent, wages = wages,
          agi_lo, agi_hi, target_value = as.numeric(value))]
}
t14_wages <- read_bysize_series('income_sources', 'wages', 'returns', years = YEARS)
t17_all   <- read_bysize_series('dependent_returns', 'all', 'returns', years = YEARS)
t17_wages <- read_bysize_series('dependent_returns', 'salaries and wages', 'returns', years = YEARS)
targets_cls <- rbind(class_targets(t14_wages, 't14_wages', NA_integer_, 1L),
                     class_targets(t17_all,   't17_all',   1L, NA_integer_),
                     class_targets(t17_wages, 't17_wages', 1L, 1L))

targets <- rbind(targets_t16, targets_cls)
targets[, target_id := match(unit, unique(unit))]

#-----------------------
# Gates
#-----------------------

one_per_unit <- function(d) unique(d[, .(year, source, unit, target_value)])
checks <- list()
gate <- function(check, year, key, got, want, n) {
  checks[[length(checks) + 1]] <<- data.table(check, year, key, got, want,
                                              diff = got - want, n_cells = n,
                                              pass = abs(got - want) <= tolerance(n))
}

counts <- melt(fread(COUNTS), id.vars = c('filing_status', 'age_group'),
               variable.name = 'year', value.name = 'n', variable.factor = FALSE)
counts[, year := as.integer(year)]

for (y in YEARS) {
  # status totals against return_counts_2023.csv, which 13_filer_return_counts.R
  # built from the raw Table 1.6 sheets through a different reader
  for (fs in 1:4) {
    got  <- one_per_unit(targets[year == y & source == 't16' & filing_status == fs])[, sum(target_value)]
    want <- counts[year == y & filing_status == fs, sum(n)]
    gate('t16 status total = return_counts', y, fs, got, want,
         targets[year == y & source == 't16' & filing_status == fs, .N])
  }
  # (status, age group) cells no block crosses
  s <- status_cells[tax_year == y]
  s[, age_group_bl := uniqueN(age_group), by = unit]
  clean <- s[, .(crossed = any(age_group_bl > 1)), by = .(filing_status, age_group)][crossed == FALSE]
  for (i in seq_len(nrow(clean))) {
    fs <- clean$filing_status[i]; ag <- clean$age_group[i]
    cell_units <- s[filing_status == fs & age_group == ag]
    got <- unique(cell_units[, .(unit, target_value)])[, sum(target_value)]
    gate('t16 status x age = return_counts', y, sprintf('%d x %d', fs, ag), got,
         counts[year == y & filing_status == fs & age_group == ag, n], nrow(cell_units))
  }
  for (src in c('t14_wages', 't17_all', 't17_wages')) {
    series <- switch(src, t14_wages = t14_wages, t17_all = t17_all, t17_wages = t17_wages)
    got <- targets[year == y & source == src, sum(target_value)]
    gate(sprintf('%s classes = published total', src), y, src, got,
         series[tax_year == y & is.na(agi_lo), value], targets[year == y & source == src, .N])
  }
}
checks <- rbindlist(checks)
dir.create(RES, showWarnings = FALSE, recursive = TRUE)
fwrite(checks, file.path(RES, 'refit_target_checks.csv'))

cat(sprintf('%d checks, %d fail\n', nrow(checks), sum(!checks$pass)))
print(checks[, .(n = .N, pass = sum(pass)), by = check])
if (any(!checks$pass)) {
  print(checks[pass == FALSE])
  stop('refit target gates failed')
}

#-----------------------
# Write
#-----------------------

setcolorder(targets, c('year', 'target_id', 'source', 'filing_status', 'age_lo', 'age_hi',
                       'dependent', 'wages', 'agi_lo', 'agi_hi', 'target_value'))
targets[, unit := NULL]
setorder(targets, year, target_id)
fwrite(targets, OUT, na = '')
summary_units <- unique(targets[, .(year, source, target_id)])[, .N, by = .(year, source)]
print(dcast(summary_units, source ~ year, value.var = 'N'))
cat('Wrote', OUT, '\n')
