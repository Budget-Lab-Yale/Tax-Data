#--------------------------------------
# verify_pce_bench.R
#
# Standalone sanity check of src/pce_benchmark.R::benchmark_to_pce()
# against real imputed data, without running the full pipeline.
#
# Loads the cached pre-benchmark PUF from a prior test_consumption.R
# run, and calls benchmark_to_pce() once with as-is 2023-dollar
# targets and once with targets deflated to 2017$ (the PUF cache is
# in 2017$ — production still passes 2023$ targets to 2017$ data;
# see docs/cex_pce_mapping.md "target-year mismatch").
#
# Pass criteria:
#   - post_benchmark == nipa_target for all 8 categories (exact)
#   - capped categories show deficit_allocated > 0
#   - applied scale factors all <= sf_cap (default 2)
#
# Inputs: resources/cache/consumption_analysis.rds
# Output: console report; temp file resources/cache/pce_targets_2017_tmp.csv
#--------------------------------------

lapply(c('tidyverse', 'magrittr', 'data.table', 'Hmisc'),
       library, character.only = TRUE)

source('src/pce_benchmark.R')

puf = read_rds('resources/cache/consumption_analysis.rds')
cat(sprintf('PUF cache: %d rows, %d cols\n', nrow(puf), ncol(puf)))

old_names = c('clothing', 'motor_vehicles', 'durables', 'other_nondurables',
              'food_off_premises', 'gasoline', 'housing_utilities',
              'other_services_health')
new_names = paste0('c_', old_names)

if (all(old_names %in% names(puf)) && !any(new_names %in% names(puf))) {
  puf = puf %>% rename_with(~ paste0('c_', .x), all_of(old_names))
  cat(sprintf('Renamed %d category columns to c_*\n', length(new_names)))
} else {
  cat('Cache already uses c_* names — no rename needed.\n')
}
stopifnot(all(new_names %in% names(puf)))

# Build a 2017-deflated targets file (CPI-U 2023/2017 = 1.23825)
cpi_2023_to_2017 = 1.23825
targets_2023     = fread('resources/pce_targets_2023.csv')
targets_2017     = copy(targets_2023)
targets_2017[, pce_billions := pce_billions / cpi_2023_to_2017]
targets_2017_path = 'resources/cache/pce_targets_2017_tmp.csv'
fwrite(targets_2017, targets_2017_path)

cat('\n========== Run 1: 2023-dollar targets (production behavior) ==========\n')
cat('(Note: 2023$ targets vs 2017$ PUF — scale factors ~1.24x too high)\n\n')
b23 = benchmark_to_pce(puf, weight_col = 'weight',
                       target_file = 'resources/pce_targets_2023.csv',
                       annualize = 1)
print(b23$diagnostics, row.names = FALSE)

cat('\n========== Run 2: 2017-dollar targets (apples-to-apples) ==========\n\n')
b17 = benchmark_to_pce(puf, weight_col = 'weight',
                       target_file = targets_2017_path,
                       annualize = 1)
print(b17$diagnostics, row.names = FALSE)

# ---------- Pass-fail: post_benchmark == nipa_target (exact) ----------
cat('\n========== Pass check: post == target (2017$ run) ==========\n')
d = b17$diagnostics
d$abs_err     = round(abs(d$post_benchmark - d$nipa_target), 3)
d$rel_err_pct = round(abs(d$post_benchmark - d$nipa_target) /
                      d$nipa_target * 100, 4)
print(d[, c('category', 'nipa_target', 'post_benchmark', 'abs_err', 'rel_err_pct')],
      row.names = FALSE)
cat(sprintf('\nMax |post - target| / target: %.4f%%\n', max(d$rel_err_pct)))
cat(sprintf('Applied scale factor range (2017$): [%.2f, %.2f]\n',
            min(d$scale_factor_applied), max(d$scale_factor_applied)))
cat(sprintf('Total deficit allocated (2017$): $%.0f B\n',
            sum(d$deficit_allocated)))

cat('\n========== Done ==========\n')
