#---------------------------------------------
# wealth_cell_sizes.R
#
# Report, per 7-income-bucket Stage 3 cell:
#   - unweighted row count
#   - total population weight (M)
#   - weight distribution (min, p10, p25, mean,
#     median, p75, p90, p99, max)
#   - dispersion ratios (p99/p10, max/min)
#
# Also report cell membership of the 15 highest-
# NW SCF rows — these are the top-tail donors
# the imputation needs to keep reachable.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(Hmisc); library(tibble)
})

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')

scf = read_rds('resources/cache/scf_tax_units.rds')

# Unfloored income (same construction as wealth.R and stage3).
scf$income = with(scf, wages_scf + business_scf + int_div_scf + capital_gains_scf +
                       rent_scf + ss_pens_scf + ui_other_scf)
scf$pctile = compute_percentile(scf$income, scf$weight, seq(0.01, 0.99, 0.01))

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')

# Use a FINE percentile grid so we can split past 99.
scf$pctile_fine = compute_percentile(scf$income, scf$weight, FINE_PCTILE_PROBS)
# Map back to 0..100 scale: FINE_PCTILE_PROBS has 99 + 9 + 1 = 109 breakpoints
# so pctile_fine in 0..110; we want a continuous 0..100 rank.
# Simpler: recompute a 0..100 rank directly from weighted income.
ord = order(scf$income)
cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank = numeric(nrow(scf))
rank[ord] = 100 * cum_w
scf$rank_0_100 = rank
idx = findInterval(scf$rank_0_100, CALIB_INCOME_EDGES,
                   rightmost.closed = TRUE, all.inside = TRUE)
scf$cell = factor(CALIB_INCOME_BUCKETS[idx], levels = CALIB_INCOME_BUCKETS)

# NW for top-row membership diagnostic
scf$nw = rowSums(scf[, wealth_asset_vars]) - rowSums(scf[, wealth_debt_vars])

cat('SCF tax units: ', nrow(scf), ' rows, total pop weight ', round(sum(scf$weight)/1e6, 1),
    'M\n\n', sep = '')

cat('Per-cell sample size and weight dispersion (weight in population units):\n')
out = scf %>%
  group_by(cell) %>%
  summarise(
    n_unwt     = dplyr::n(),
    pop_M      = round(sum(weight) / 1e6, 1),
    wt_min     = round(min(weight)),
    wt_p10     = round(as.numeric(quantile(weight, 0.10))),
    wt_p25     = round(as.numeric(quantile(weight, 0.25))),
    wt_mean    = round(mean(weight)),
    wt_p50     = round(median(weight)),
    wt_p75     = round(as.numeric(quantile(weight, 0.75))),
    wt_p90     = round(as.numeric(quantile(weight, 0.90))),
    wt_p99     = round(as.numeric(quantile(weight, 0.99))),
    wt_max     = round(max(weight)),
    p99_over_p10 = round(as.numeric(quantile(weight, 0.99)) /
                         pmax(as.numeric(quantile(weight, 0.10)), 1), 1),
    max_over_min = round(max(weight) / pmax(min(weight), 1), 1),
    .groups    = 'drop'
  )
print(as.data.frame(out), row.names = FALSE)

cat('\nTop 15 highest-NW SCF rows (cell, weight, NW $B, income $K):\n')
top15 = scf %>%
  arrange(desc(nw)) %>%
  head(15) %>%
  transmute(
    rank   = seq_len(15),
    cell   = as.character(cell),
    weight = round(weight),
    nw_B   = round(nw / 1e9, 2),
    inc_K  = round(income / 1e3)
  )
print(as.data.frame(top15), row.names = FALSE)

cat('\nDone.\n')
