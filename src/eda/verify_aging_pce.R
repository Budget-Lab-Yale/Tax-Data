#---------------------------------------------------
# verify_aging_pce.R
#
# Quick sanity check on Step 4 aging-layer rework:
# compare weighted per-category consumption aggregates
# from tax_units_YYYY.csv to NIPA PCE targets for 2017,
# 2019, and 2024.
#---------------------------------------------------

library(tidyverse)
library(data.table)

OUT = '/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026041712/baseline'

c_cols = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
           'c_food_off_premises', 'c_gasoline', 'c_housing_utilities', 'c_other_services_health')

pce_hist = read_csv('resources/pce_historical.csv', show_col_types = F)

agg_year = function(y) {
  f = file.path(OUT, paste0('tax_units_', y, '.csv'))
  d = fread(f, select = c('weight', c_cols))
  totals = sapply(setNames(c_cols, c_cols),
                  function(col) sum(d[[col]] * d$weight) / 1e9)
  tibble(year = y, variable = names(totals), puf_billions = as.numeric(totals))
}

puf_agg = map_dfr(c(2017L, 2019L, 2024L), agg_year)

pce_long = pce_hist %>%
  pivot_longer(cols = -year, names_to = 'variable', values_to = 'nipa_billions')

cmp = puf_agg %>%
  left_join(pce_long, by = c('year', 'variable')) %>%
  mutate(
    diff_pct = (puf_billions - nipa_billions) / nipa_billions * 100,
    puf_billions  = round(puf_billions, 1),
    nipa_billions = round(nipa_billions, 1),
    diff_pct      = round(diff_pct, 2)
  ) %>%
  arrange(year, variable)

print(cmp, n = Inf)

# 2017 sum-of-8 aggregate for the top-level check
c_sum_2017 = puf_agg %>%
  filter(year == 2017) %>%
  summarise(s = sum(puf_billions)) %>%
  pull(s)
cat('\n2017: sum of 8 c_* =', round(c_sum_2017, 1), 'B\n')
