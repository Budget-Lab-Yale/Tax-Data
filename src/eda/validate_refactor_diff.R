#--------------------------------------
# validate_refactor_diff.R
#
# Compares per-year tax_units_{year}.csv
# between a pre-refactor (baseline) run
# and the post-refactor run. Exits 0
# when all non-wealth variables match
# within tolerance at every year;
# otherwise reports discrepancies.
#
# Usage:
#   OLD_DIR=<baseline_output_path> \
#   NEW_DIR=<refactor_output_path> \
#   TOL=1e-8 \
#   Rscript src/eda/validate_refactor_diff.R
#
# Default TOL=1e-8; columns where
# max|diff|/max(|old|) < TOL are
# considered matching. Matches on the
# intersection of (id × column) present
# in both files.
#--------------------------------------

suppressMessages({
  library(readr)
  library(dplyr)
})

OLD_DIR = Sys.getenv('OLD_DIR', unset = '')
NEW_DIR = Sys.getenv('NEW_DIR', unset = '')
TOL     = as.numeric(Sys.getenv('TOL', unset = '1e-8'))

if (OLD_DIR == '' || NEW_DIR == '')
  stop('Set OLD_DIR and NEW_DIR env vars.')
stopifnot(dir.exists(OLD_DIR), dir.exists(NEW_DIR))

YEARS = 2017L:2097L

cat(sprintf('OLD_DIR: %s\n', OLD_DIR))
cat(sprintf('NEW_DIR: %s\n', NEW_DIR))
cat(sprintf('TOL:     %g (relative)\n\n', TOL))

# Accumulators
all_issues = list()
per_year_summary = list()

for (y in YEARS) {
  old_path = file.path(OLD_DIR, sprintf('tax_units_%d.csv', y))
  new_path = file.path(NEW_DIR, sprintf('tax_units_%d.csv', y))
  if (!file.exists(old_path) || !file.exists(new_path)) {
    cat(sprintf('[%d] MISSING: old=%s new=%s\n',
                y, file.exists(old_path), file.exists(new_path)))
    next
  }

  old = read_csv(old_path, show_col_types = FALSE, progress = FALSE)
  new = read_csv(new_path, show_col_types = FALSE, progress = FALSE)

  # Common column names
  common = intersect(names(old), names(new))
  only_old = setdiff(names(old), names(new))
  only_new = setdiff(names(new), names(old))

  # Align by id
  stopifnot('id' %in% common)
  old = old[match(sort(old$id), old$id), ]
  new = new[match(sort(new$id), new$id), ]
  stopifnot(identical(old$id, new$id))

  # Compare each common column
  per_col = lapply(common, function(cn) {
    o = old[[cn]]
    n = new[[cn]]
    if (!is.numeric(o) || !is.numeric(n)) {
      # non-numeric: require identical
      if (identical(o, n)) {
        return(list(col = cn, status = 'IDENT', max_abs = 0, max_rel = 0,
                    worst_rows = NULL))
      } else {
        diffs = which(!mapply(identical, o, n))
        return(list(col = cn, status = 'DIFF_NONNUMERIC',
                    max_abs = NA_real_, max_rel = NA_real_,
                    worst_rows = head(diffs, 5)))
      }
    }
    na_both = is.na(o) & is.na(n)
    na_diff = xor(is.na(o), is.na(n))
    d = abs(o - n)
    d[is.na(d)] = 0
    denom = max(abs(o), na.rm = TRUE)
    if (!is.finite(denom) || denom < 1e-12) denom = 1
    max_abs = max(d, na.rm = TRUE)
    max_rel = max_abs / denom
    na_mismatch_count = sum(na_diff, na.rm = TRUE)
    status = if (max_rel < TOL && na_mismatch_count == 0) 'IDENT' else 'DIFF'
    worst = if (status == 'DIFF') order(d, decreasing = TRUE)[1:min(5, length(d))]
            else NULL
    list(col = cn, status = status, max_abs = max_abs, max_rel = max_rel,
         na_mismatch = na_mismatch_count, worst_rows = worst)
  })
  names(per_col) = common

  diff_cols = Filter(function(x) x$status != 'IDENT', per_col)

  per_year_summary[[as.character(y)]] = list(
    year = y,
    n_rows_old = nrow(old), n_rows_new = nrow(new),
    n_common = length(common),
    n_diff = length(diff_cols),
    only_old = only_old, only_new = only_new
  )

  if (length(diff_cols) > 0) {
    all_issues[[as.character(y)]] = diff_cols
    cat(sprintf('[%d] %d/%d cols differ — worst offenders:\n',
                y, length(diff_cols), length(common)))
    ordered = diff_cols[order(sapply(diff_cols, function(x)
      if (is.numeric(x$max_rel)) -x$max_rel else -Inf))]
    for (dc in head(ordered, 5)) {
      cat(sprintf('   %-25s  max|d|=%.3g  max|d|/max|old|=%.3g  na_mismatch=%s\n',
                  dc$col,
                  if (is.numeric(dc$max_abs)) dc$max_abs else NA,
                  if (is.numeric(dc$max_rel)) dc$max_rel else NA,
                  if (!is.null(dc$na_mismatch)) dc$na_mismatch else 0))
    }
    if (length(only_old) > 0)
      cat(sprintf('   only-in-old: %s\n', paste(only_old, collapse = ', ')))
    if (length(only_new) > 0)
      cat(sprintf('   only-in-new: %s\n', paste(only_new, collapse = ', ')))
  } else {
    cat(sprintf('[%d] OK (%d cols match; old-only=%d new-only=%d)\n',
                y, length(common), length(only_old), length(only_new)))
  }
}

# ---------------------------------------------------------------------------
# Final verdict
# ---------------------------------------------------------------------------

n_years_ok = sum(sapply(per_year_summary, function(s) s$n_diff == 0))
n_years_total = length(per_year_summary)

cat(sprintf('\n========== SUMMARY ==========\n'))
cat(sprintf('Years with zero diff: %d / %d\n', n_years_ok, n_years_total))
cat(sprintf('Tolerance: relative %g\n', TOL))

if (length(all_issues) > 0) {
  cat(sprintf('\n========== ISSUES (per year) ==========\n'))
  for (y in names(all_issues)) {
    issues = all_issues[[y]]
    cat(sprintf('Year %s: %d column(s) differ\n', y, length(issues)))
    for (iss in issues) {
      cat(sprintf('  %-25s  max|d|=%.3g  max|d|/max|old|=%.3g\n',
                  iss$col,
                  if (is.numeric(iss$max_abs)) iss$max_abs else NA,
                  if (is.numeric(iss$max_rel)) iss$max_rel else NA))
    }
  }
  cat('\nVERDICT: DIFFS DETECTED (see above)\n')
  quit(status = 1)
} else {
  cat('\nVERDICT: ALL CLEAN — refactor is numerically equivalent to baseline.\n')
}
