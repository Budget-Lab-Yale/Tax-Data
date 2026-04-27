#---------------------------------------------
# parse_tilt_log.R
#
# Pulls per-bucket tilt diagnostics directly out
# of slurm_tilt_harness.out so we can build an
# AM-readable summary without depending on the
# saved RDS (which only future harness runs
# write -- the in-flight 9580593 run won't save
# it because wealth_harness.R was edited after
# the job started).
#
# Usage:
#   Rscript src/eda/parse_tilt_log.R [path/to/slurm_tilt_harness.out]
# Default path: ./slurm_tilt_harness.out
#---------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(tibble) })

args = commandArgs(trailingOnly = TRUE)
log_path = if (length(args) >= 1L) args[1] else 'slurm_tilt_harness.out'
stopifnot(file.exists(log_path))

lines = readLines(log_path)

# Per-bucket "predict W" lines:
#   "  pct00to20    × nonsenior: predict W [26538x78216] in 144.4s"
predict_re = '^\\s+(\\S+)\\s+×\\s+(\\S+)\\s*:\\s+predict W \\[(\\d+)x(\\d+)\\] in (\\d+\\.\\d+)s'
predict_rows = list()
for (ln in lines) {
  m = regmatches(ln, regexec(predict_re, ln))[[1]]
  if (length(m) > 1L) {
    predict_rows[[length(predict_rows) + 1L]] = tibble(
      cell_income = m[2], cell_age = m[3],
      n_puf = as.integer(m[4]), n_donor_kept = as.integer(m[5]),
      predict_sec = as.numeric(m[6])
    )
  }
}

# Per-bucket "solve" lines:
#   "  pct00to20    × nonsenior: solve 144.8s (iters=4 status=cap)"
solve_re = '^\\s+(\\S+)\\s+×\\s+(\\S+)\\s*:\\s+solve\\s+(\\d+\\.\\d+)s\\s+\\(iters=(\\S+)\\s+status=(\\S+)\\)'
solve_rows = list()
for (ln in lines) {
  m = regmatches(ln, regexec(solve_re, ln))[[1]]
  if (length(m) > 1L) {
    solve_rows[[length(solve_rows) + 1L]] = tibble(
      cell_income = m[2], cell_age = m[3],
      solve_sec = as.numeric(m[4]),
      iters = m[5],
      solve_status = m[6]
    )
  }
}

# Per-bucket summary lines:
#   "  pct00to20    × nonsenior: n= 26538 M=14 max_rel=1.00e+00 ||lambda||=141.66 ESS=862.4 k_used=290 cap"
summary_re = paste0(
  '^\\s+(\\S+)\\s+×\\s+(\\S+)\\s*:\\s+n=\\s*(\\d+)\\s+M=\\s*(\\d+)\\s+',
  'max_rel=([0-9.eE+-]+)\\s+\\|\\|lambda\\|\\|=([0-9.eE+-]+)\\s+',
  'ESS=([0-9.eE+-]+)\\s+k_used=(\\d+)'
)
summary_rows = list()
for (ln in lines) {
  m = regmatches(ln, regexec(summary_re, ln))[[1]]
  if (length(m) > 1L) {
    summary_rows[[length(summary_rows) + 1L]] = tibble(
      cell_income = m[2], cell_age = m[3],
      n_puf = as.integer(m[4]), M = as.integer(m[5]),
      max_rel = as.numeric(m[6]), lambda_norm = as.numeric(m[7]),
      ESS = as.numeric(m[8]), k_used = as.integer(m[9])
    )
  }
}

cat(sprintf('Parsed %d predict rows, %d solve rows, %d summary rows from %s\n',
            length(predict_rows), length(solve_rows), length(summary_rows),
            log_path))

if (length(summary_rows) > 0L) {
  predict_tbl = bind_rows(predict_rows) %>%
    select(cell_income, cell_age, n_donor_kept, predict_sec)
  solve_tbl = bind_rows(solve_rows) %>%
    select(cell_income, cell_age, solve_sec, iters, solve_status)
  summary_tbl = bind_rows(summary_rows)

  joined = summary_tbl %>%
    left_join(predict_tbl, by = c('cell_income', 'cell_age')) %>%
    left_join(solve_tbl,   by = c('cell_income', 'cell_age')) %>%
    select(cell_income, cell_age, n_puf, n_donor_kept, M,
           predict_sec, solve_sec, iters, solve_status,
           max_rel, lambda_norm, ESS, k_used)

  cat('\n=== Per-bucket tilt diagnostics ===\n')
  print(joined, n = Inf)

  cat('\n=== Convergence summary ===\n')
  conv_tbl = joined %>%
    mutate(converged_strict = max_rel < 0.01,
           converged_loose  = max_rel < 0.05,
           feasible_close   = max_rel < 0.10) %>%
    summarise(
      n_buckets        = n(),
      n_strict         = sum(converged_strict),
      n_loose          = sum(converged_loose),
      n_feasible_close = sum(feasible_close),
      max_residual     = max(max_rel, na.rm = TRUE),
      median_lambda    = median(lambda_norm, na.rm = TRUE),
      max_lambda       = max(lambda_norm, na.rm = TRUE),
      median_ESS       = median(ESS, na.rm = TRUE),
      total_predict_s  = sum(predict_sec, na.rm = TRUE),
      total_solve_s    = sum(solve_sec, na.rm = TRUE)
    )
  print(conv_tbl)

  cat('\n=== Worst residual cells ===\n')
  print(joined %>% arrange(desc(max_rel)) %>% head(5) %>%
          select(cell_income, cell_age, max_rel, lambda_norm, ESS, M),
        n = 5)
}

# Step B factor distribution if present
rescale_lines = grep('rescale factors applied', lines, value = TRUE)
if (length(rescale_lines) > 0L) {
  cat('\n=== Step B residual rescale ===\n')
  cat(rescale_lines, sep = '\n')
}

# Aggregate totals table if present (wealth_harness.R prints it)
agg_idx = grep('Aggregate totals per category', lines)
if (length(agg_idx) > 0L) {
  start = agg_idx[1] + 1L
  end = min(start + 12L, length(lines))
  cat('\n=== Aggregate totals ===\n')
  cat(lines[start:end], sep = '\n')
}

# Top NW shares
shr_idx = grep('Top NW shares', lines)
if (length(shr_idx) > 0L) {
  start = shr_idx[1]
  end = min(start + 6L, length(lines))
  cat('\n')
  cat(lines[start:end], sep = '\n')
}
