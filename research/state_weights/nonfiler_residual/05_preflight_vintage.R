#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# 05_preflight_vintage.R  (group E, the federal validation battery's layer 1)
#
# The cheap gate that runs BEFORE any Tax-Simulator job: compare two Tax-Data
# vintages on the properties the downstream A/B depends on, so a failure costs
# minutes on a login node rather than hours of cluster time and a confusing
# diff at the end.
#
# It exists because of two verified hazards (nonfiler_federal_validation.md
# section 4.0):
#
#   (i)  Tax-Simulator binds precomputed random numbers to records by ROW
#        POSITION -- `filter(id %in% sample_ids)` then `bind_cols()`
#        (run.R:349-357) -- so a record set whose order moves between years
#        silently re-randomises take-up and behaviour.
#   (ii) `sample_ids` is taken from the 2017 file of ONE vintage
#        (config_parser.R:219-225) and applied to every year and scenario.
#
# The checks, and what each guards:
#
#   1  WITHIN each vintage: the id vector after Tax-Simulator's own filter is
#      identical, in order, to the 2017 id vector, in every year. This is the
#      contract hazard (i) and (ii) need. Rows that 2017 does not contain --
#      the Forbes splice appends synthetic billionaires from 2022 -- are
#      dropped by that filter and are reported, not failed.
#   2  BETWEEN the vintages: the FILER slice is identical on every column,
#      including weight, in every year checked. This is what proves a change
#      is "non-filer only" BEFORE any tax is calculated, and it is what makes
#      section 4a an exact-equality test rather than a tolerance argument.
#   3  Schema: same column set, no unexpected new names, `qual_div` absent
#      (the producer-side rename that broke this interface once).
#   4  Domain: filer and dep_status in {0,1} with no NA, and
#      filer == 0 implies dep_status == 0.
#   5  Non-filer mass, reported per year: units, adults, wages. Not a gate --
#      this is the quantity the rework is meant to move, printed so the
#      reviewer sees its size before reading any downstream diff.
#
# Record counts need NOT match between vintages. Under Block E (design A, the
# union base) the new vintage carries every pool year's records, so it has
# about 3x the rows; check 2 compares the filer slice, which is the same 207k
# records in both, and check 1 is what guarantees the extra rows are harmless.
#
# Login-node safe, and deliberately so: it reads at most BATCH columns from
# each vintage at a time rather than two whole files. Two 930MB files held
# open together is about 12GB in memory, over the ~5GB interactive cgroup cap,
# and the kill reads as a mysterious exit rather than an OOM -- which is what
# happened on the first run of this script against a Block E vintage.
#
#   Rscript research/state_weights/nonfiler_residual/05_preflight_vintage.R \
#     --old=2026083115 --new=2026091119 [--years=2017:2035] [--full=2017,2020]
#     [--out=results/preflight_{old}_{new}.csv]
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table); library(yaml)
})
source('src/nonfilers/state_weights.R')

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  hit <- grep(sprintf('^--%s=', name), args, value = TRUE)
  if (length(hit)) sub(sprintf('^--%s=', name), '', hit[1]) else default
}
OLD <- arg('old'); NEW <- arg('new')
if (is.null(OLD) || is.null(NEW)) {
  stop('usage: --old=<vintage> --new=<vintage> [--years=2017:2035] [--full=2017,2020]',
       call. = FALSE)
}
YEARS <- eval(parse(text = arg('years', '2017:2035')))
FULL  <- as.integer(strsplit(arg('full', '2017,2020'), ',')[[1]])
SCEN  <- arg('scenario', 'baseline')
OUT   <- arg('out', file.path('research/state_weights/nonfiler_residual/results',
                              sprintf('preflight_%s_%s.csv', OLD, NEW)))
# Columns read from each vintage at once. 24 columns of 1.4M rows is ~270MB a
# side, so the peak stays well inside an interactive session.
BATCH <- as.integer(arg('batch', '24'))

vintage_dir <- function(v) {
  d <- file.path(model_data_root(), 'Tax-Data/v1', v, SCEN)
  if (!dir.exists(d)) stop('vintage not found: ', d, call. = FALSE)
  d
}
DIRS <- c(old = vintage_dir(OLD), new = vintage_dir(NEW))
year_file <- function(v, y) file.path(DIRS[[v]], sprintf('tax_units_%d.csv', y))

# Years both vintages actually wrote.
YEARS <- YEARS[vapply(YEARS, function(y)
  file.exists(year_file('old', y)) && file.exists(year_file('new', y)), logical(1))]
stopifnot(length(YEARS) > 0, min(YEARS) == 2017L)

findings <- list()
note <- function(check, year, status, detail) {
  findings[[length(findings) + 1L]] <<-
    data.table(check = check, year = year, status = status, detail = detail)
  message(sprintf('  %-5s %-34s %s%s', status, check,
                  if (is.na(year)) '' else paste0('TY', year, '  '), detail))
}

#-------------------------------------------------------------------------------
# 1. Within-vintage id order stability, after Tax-Simulator's own filter
#-------------------------------------------------------------------------------
message('=== 1. within-vintage id order (the positional random-number contract)')
sample_ids <- list()
for (v in c('old', 'new')) {
  sample_ids[[v]] <- fread(year_file(v, 2017L), select = 'id')$id
  if (anyDuplicated(sample_ids[[v]])) {
    note('id uniqueness at 2017', 2017L, 'FAIL',
         sprintf('%s: %d duplicate ids', v, sum(duplicated(sample_ids[[v]]))))
  } else {
    note('id uniqueness at 2017', 2017L, 'ok',
         sprintf('%s: %s ids, all unique', v, format(length(sample_ids[[v]]), big.mark = ',')))
  }
}
for (v in c('old', 'new')) {
  for (y in YEARS) {
    iy   <- fread(year_file(v, y), select = 'id')$id
    keep <- iy[iy %in% sample_ids[[v]]]
    dropped <- length(iy) - length(keep)
    if (!identical(keep, sample_ids[[v]])) {
      note(sprintf('%s: id order vs 2017', v), y, 'FAIL',
           sprintf('%d rows after filter vs %d at 2017',
                   length(keep), length(sample_ids[[v]])))
    } else if (dropped > 0L && y == max(YEARS)) {
      note(sprintf('%s: id order vs 2017', v), y, 'ok',
           sprintf('identical; %d rows absent from 2017 are dropped by the filter',
                   dropped))
    } else if (y == max(YEARS)) {
      note(sprintf('%s: id order vs 2017', v), y, 'ok', 'identical in every year')
    }
  }
}

#-------------------------------------------------------------------------------
# 2. Filer-slice equality between vintages -- the "non-filer only" proof
#-------------------------------------------------------------------------------
message('=== 2. filer slice, old vs new (every column, including weight)')
KEY_COLS <- c('weight', 'wages', 'txbl_int', 'div_pref', 'kg_lt', 'gross_ss',
              'sole_prop', 'filing_status', 'age1', 'male1', 'n_dep')

for (y in YEARS) {
  full <- y %in% FULL
  cols_o <- names(fread(year_file('old', y), nrows = 0L))
  cols_n <- names(fread(year_file('new', y), nrows = 0L))
  if (!identical(sort(cols_o), sort(cols_n))) {
    note('column set', y, 'FAIL',
         sprintf('old-only: %s | new-only: %s',
                 paste(setdiff(cols_o, cols_n), collapse = ','),
                 paste(setdiff(cols_n, cols_o), collapse = ',')))
  }
  if ('qual_div' %in% c(cols_o, cols_n)) {
    note('qual_div absent', y, 'FAIL', 'the producer-side rename is back')
  }
  common <- intersect(cols_o, cols_n)

  # The filer key, read once: id and filer from each side. Everything below
  # compares the filer rows in id order without ever holding a whole file.
  ko <- fread(year_file('old', y), select = c('id', 'filer', 'dep_status', 'weight'))
  kn <- fread(year_file('new', y), select = c('id', 'filer', 'dep_status', 'weight'))
  for (v in c('old', 'new')) {
    d <- if (v == 'old') ko else kn
    bad <- d[!(filer %in% c(0, 1)) | !(dep_status %in% c(0, 1)) |
             is.na(filer) | is.na(dep_status) | (filer == 0 & dep_status != 0), .N]
    if (bad > 0L) note(sprintf('%s: filer/dep_status domain', v), y, 'FAIL',
                       sprintf('%d offending rows', bad))
  }
  oo <- which(ko$filer == 1); oo <- oo[order(ko$id[oo])]
  nn <- which(kn$filer == 1); nn <- nn[order(kn$id[nn])]
  if (length(oo) != length(nn)) {
    note('filer slice', y, 'FAIL',
         sprintf('%d filer rows old vs %d new', length(oo), length(nn)))
  } else if (!identical(ko$id[oo], kn$id[nn])) {
    note('filer slice', y, 'FAIL', 'filer id sets differ')
  } else {
    cmp_cols <- setdiff(if (full) common else intersect(common, c('id', KEY_COLS)), 'id')
    moved <- character(0); worst <- numeric(0)
    for (grp in split(cmp_cols, ceiling(seq_along(cmp_cols) / BATCH))) {
      bo <- fread(year_file('old', y), select = grp)
      bn <- fread(year_file('new', y), select = grp)
      for (cc in grp) {
        a <- bo[[cc]][oo]; b <- bn[[cc]][nn]
        if (isTRUE(all.equal(a, b, tolerance = 0))) next
        moved <- c(moved, cc)
        an <- suppressWarnings(as.numeric(a)); bn2 <- suppressWarnings(as.numeric(b))
        worst <- c(worst, if (all(is.na(an))) NA_real_ else
          max(abs(bn2 - an) / pmax(abs(an), 1e-9), na.rm = TRUE))
      }
      rm(bo, bn); invisible(gc())
    }
    label <- if (full) 'filer slice, ALL columns' else 'filer slice, key columns'
    if (length(moved)) {
      note(label, y, 'MOVED',
           sprintf('%d of %d columns: %s', length(moved), length(cmp_cols),
                   paste(sprintf('%s(%.1e)', moved, worst), collapse = ' ')))
    } else {
      note(label, y, 'ok', sprintf('identical across %d columns', length(cmp_cols)))
    }
  }

  # Non-filer mass, reported. Needs three columns, read once per side.
  for (v in c('old', 'new')) {
    k <- if (v == 'old') ko else kn
    fs <- fread(year_file(v, y), select = c('filing_status', 'wages'))
    sel <- k$filer == 0 & k$weight > 0
    note(sprintf('%s: non-filer mass', v), y, 'info',
         sprintf('%.2fM units | %.2fM adults | $%.1fB wages',
                 sum(k$weight[sel]) / 1e6,
                 sum(k$weight[sel] * (1 + (fs$filing_status[sel] == 2))) / 1e6,
                 sum(k$weight[sel] * fs$wages[sel]) / 1e9))
    rm(fs)
  }
  rm(ko, kn, oo, nn); invisible(gc())
}

#-------------------------------------------------------------------------------
# Report
#-------------------------------------------------------------------------------
out <- rbindlist(findings)
dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
fwrite(out, OUT)
fails <- out[status == 'FAIL']
message(sprintf('\n%s -> %s: %d checks, %d FAIL, %d MOVED',
                OLD, NEW, out[status != 'info', .N], nrow(fails),
                out[status == 'MOVED', .N]))
message('wrote ', OUT)
if (nrow(fails)) {
  message('PREFLIGHT FAILED -- do not spend cluster time until these are explained')
  quit(status = 1L)
}
message('PREFLIGHT PASS')
