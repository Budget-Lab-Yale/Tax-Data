#!/usr/bin/env Rscript
#------------------------------------------------------------------------------
# research/state_weights/scripts/phase3_compare_fits.R <dir_a> <dir_b>
#
# Are two directories of fit_{year}.rds the same fits? identical() on P,
# theta, ids and jurisdictions per year, with the largest absolute difference
# printed when they are not. Written for Phase 4 P3, to certify that the
# chains moved into src/nonfilers/sw_fit.R reproduce the S39 fits from the
# same hardened inputs; reusable for any two fit directories. Exits non-zero
# on any difference.
#------------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2, dir.exists(args[1]), dir.exists(args[2]))
files <- sort(list.files(args[1], pattern = '^fit_[0-9]{4}\\.rds$'))
stopifnot(length(files) > 0, setequal(files, list.files(args[2], pattern = '^fit_[0-9]{4}\\.rds$')))

all_ok <- TRUE
for (f in files) {
  a <- readRDS(file.path(args[1], f)); b <- readRDS(file.path(args[2], f))
  same <- vapply(c('P', 'theta', 'ids', 'jurisdictions'), function(k) identical(a[[k]], b[[k]]), logical(1))
  dP <- if (same[['P']]) 0 else max(abs(a$P - b$P))
  cat(sprintf('%s  P %s  theta %s  ids %s  jurisdictions %s%s\n', f,
              same[['P']], same[['theta']], same[['ids']], same[['jurisdictions']],
              if (same[['P']]) '' else sprintf('  (max |dP| = %.3e)', dP)))
  all_ok <- all_ok && all(same)
}
cat(if (all_ok) 'PHASE3_FITS_IDENTICAL\n' else 'PHASE3_FITS_DIFFER\n')
quit(status = if (all_ok) 0 else 1)
