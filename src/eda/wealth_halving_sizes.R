#---------------------------------------------
# wealth_halving_sizes.R
#
# Per-cell halving: split every row with weight
# > D × cell_min_weight until no row exceeds it,
# so the final max/min ratio inside each cell is
# ≤ D. Report resulting cell sizes across a grid
# of dispersion targets D.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(Hmisc); library(tibble)
})

source('src/imputations/helpers.R')

scf = read_rds('resources/cache/scf_tax_units.rds')
scf$income = with(scf, wages_scf + business_scf + int_div_scf + capital_gains_scf +
                       rent_scf + ss_pens_scf + ui_other_scf)

ord = order(scf$income)
cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank = numeric(nrow(scf))
rank[ord] = 100 * cum_w
scf$rank_0_100 = rank

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')
idx = findInterval(scf$rank_0_100, CALIB_INCOME_EDGES,
                   rightmost.closed = TRUE, all.inside = TRUE)
scf$cell = CALIB_INCOME_BUCKETS[idx]

# Number of copies under halving with absolute threshold T.
n_copies = function(w, T) {
  2 ^ pmax(0, ceiling(log2(pmax(w / T, 1))))
}

cat('Per-cell min weight (the halving floor):\n')
cell_mins = tapply(scf$weight, scf$cell, min)[CALIB_INCOME_BUCKETS]
print(as.data.frame(tibble(cell = CALIB_INCOME_BUCKETS,
                           min_weight = as.integer(cell_mins))),
      row.names = FALSE)

cat('\n\nPer-cell TOTAL rows after halving to target max/min = D within each cell.\n')
cat('(Each cell gets its own T = D × cell_min_weight.)\n\n')

D_grid = c(2, 3, 5, 8, 13, 20, 50, 100)

rows_tbl = purrr::map_dfr(D_grid, function(D) {
  # Compute per-row copies using each cell's own T_max.
  T_per_row = cell_mins[scf$cell] * D
  n_per_row = n_copies(scf$weight, T_per_row)
  per_cell = tapply(n_per_row, scf$cell, sum)
  per_cell = per_cell[CALIB_INCOME_BUCKETS]
  tibble(D = D,
         !!!setNames(as.list(as.integer(per_cell)), CALIB_INCOME_BUCKETS),
         total = as.integer(sum(per_cell)))
})
print(as.data.frame(rows_tbl), row.names = FALSE)

cat('\n\nFor reference — original (unhalved) per-cell row count:\n')
orig_counts = tapply(rep(1, nrow(scf)), scf$cell, sum)[CALIB_INCOME_BUCKETS]
print(as.data.frame(tibble(cell = CALIB_INCOME_BUCKETS,
                           n_unwt = as.integer(orig_counts))),
      row.names = FALSE)

# Also report final weight distribution under D=2 for one cell as a sanity check
cat('\n\nSanity check: pct99.9to100 weight distribution after halving at D=2:\n')
T_top = cell_mins['pct99.9to100'] * 2
top = scf %>% filter(cell == 'pct99.9to100') %>%
  mutate(n_cp = n_copies(weight, T_top),
         final_wt = weight / n_cp)
cat(sprintf('  T = %d × %d = %d; resulting max/min = %.2f\n',
            2L, as.integer(cell_mins['pct99.9to100']), as.integer(T_top),
            max(top$final_wt) / min(top$final_wt)))
cat(sprintf('  %d unhalved rows → %d halved rows\n', nrow(top), sum(top$n_cp)))
cat(sprintf('  Final weights: min=%.1f p10=%.1f p50=%.1f p90=%.1f max=%.1f\n',
            min(top$final_wt),
            as.numeric(quantile(top$final_wt, 0.10)),
            median(top$final_wt),
            as.numeric(quantile(top$final_wt, 0.90)),
            max(top$final_wt)))

cat('\nDone.\n')
