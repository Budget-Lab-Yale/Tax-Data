#---------------------------------------------
# age_wealth_gradient.R
#
# Within each of the 8 income cells, compute the
# weighted mean NW by age bracket on the SCF
# side only. Tests whether the age-NW gradient
# within cells is steep enough that a few-year
# PUF-SCF age shift could plausibly drive the
# observed per-cell PUF over-imputation.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(Hmisc)
})

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')

scf_raw = read_rds('resources/cache/scf_tax_units.rds')
scf = scf_raw %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf
  )
# NW from collapsed 23 Y-vars (already present in cached tibble).
scf$nw = rowSums(scf[, wealth_asset_vars]) - rowSums(scf[, wealth_debt_vars])

ord = order(scf$income); cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank_scf = numeric(nrow(scf)); rank_scf[ord] = 100 * cum_w
scf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

age_bins = c(-1, 34, 44, 54, 64, 74, 80)
age_labels = c('<=34','35-44','45-54','55-64','65-74','75-80')
scf$age_bin = cut(scf$age_older, breaks = age_bins, labels = age_labels)

cat('Weighted mean NW ($K) by (cell, age bracket of age_older)\n')
cat('Cells in rows, age brackets in columns.\n\n')

tbl = scf %>%
  group_by(cell, age_bin) %>%
  summarise(
    n       = dplyr::n(),
    pop_M   = sum(weight) / 1e6,
    mean_nw = sum(weight * nw) / sum(weight),
    .groups = 'drop'
  ) %>%
  select(cell, age_bin, mean_nw) %>%
  mutate(mean_nw_K = round(mean_nw / 1e3, 0)) %>%
  select(-mean_nw) %>%
  pivot_wider(names_from = age_bin, values_from = mean_nw_K, values_fill = 0) %>%
  mutate(cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell)
print(as.data.frame(tbl), row.names = FALSE)

# Same table but weighted count (population share by age within cell)
cat('\n\nPopulation share by age bracket within cell (row sums to 1):\n')
share_tbl = scf %>%
  group_by(cell) %>%
  mutate(cell_pop = sum(weight)) %>%
  ungroup() %>%
  group_by(cell, age_bin) %>%
  summarise(share = sum(weight) / first(cell_pop), .groups = 'drop') %>%
  mutate(share = round(share, 3)) %>%
  pivot_wider(names_from = age_bin, values_from = share, values_fill = 0) %>%
  mutate(cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell)
print(as.data.frame(share_tbl), row.names = FALSE)

# Slope: (mean_nw at top age - mean_nw at bottom age) / (age diff in years)
cat('\n\nWithin-cell age-NW slope: diff in $K between oldest and youngest age bin\n')
slope_tbl = scf %>%
  filter(!is.na(age_bin)) %>%
  group_by(cell, age_bin) %>%
  summarise(mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop') %>%
  pivot_wider(names_from = age_bin, values_from = mean_nw, values_fill = 0) %>%
  mutate(
    slope_K_per_yr = round(((`65-74` + `75-80`) / 2 - (`<=34` + `35-44`) / 2) / 35 / 1e3, 1),
    oldest_over_youngest = round(((`65-74` + `75-80`) / 2) / pmax(((`<=34` + `35-44`) / 2), 1), 2),
    cell = factor(cell, levels = CALIB_INCOME_BUCKETS)
  ) %>%
  arrange(cell) %>%
  select(cell, slope_K_per_yr, oldest_over_youngest)
print(as.data.frame(slope_tbl), row.names = FALSE)

cat('\nDone.\n')
