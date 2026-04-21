#---------------------------------------------------
# sanity_check_aging.R
#
# Plausibility pass on tax_units_YYYY.csv across all
# years in shared/v1/2026041714/baseline. Computes:
#
#   - Weighted aggregate per c_* and total each year
#   - Year-over-year growth rates per c_* (look for jumps)
#   - C/GDP ratio (consumption as share of nominal GDP)
#   - C per tax unit (per-weight-sum)
#   - Anomaly check: negatives, NAs, Infs in c_*
#
# Compared against NIPA actuals (resources/pce_historical.csv)
# for 2017-2025 and against Macro-Projections gdp + gdp_c
# for 2017-2098.
#---------------------------------------------------

library(tidyverse)
library(data.table)

OUT = '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026041714/baseline'
MP  = '/nfs/roberts/project/pi_nrs36/shared/model_data/Macro-Projections/v3/2026022522/baseline'

c_cols = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
           'c_food_off_premises', 'c_gasoline', 'c_housing_utilities', 'c_other_services_health')


#---------------
# Aggregate PUF
#---------------

agg_year = function(y) {
  f = file.path(OUT, paste0('tax_units_', y, '.csv'))
  if (!file.exists(f)) return(NULL)
  d = fread(f, select = c('weight', c_cols))
  totals = sapply(c_cols, function(col) sum(d[[col]] * d$weight) / 1e9)
  anomalies = sapply(c_cols, function(col) {
    v = d[[col]]
    list(n_neg = sum(v < 0, na.rm = TRUE),
         n_na  = sum(is.na(v)),
         n_inf = sum(is.infinite(v)),
         min   = min(v, na.rm = TRUE))
  })
  data.frame(year = y,
             total_weight = sum(d$weight),
             total_c      = sum(totals),
             t(totals),
             n_neg_total = sum(unlist(anomalies['n_neg', ])),
             n_na_total  = sum(unlist(anomalies['n_na',  ])),
             n_inf_total = sum(unlist(anomalies['n_inf', ])))
}

cat('reading 82 files...\n')
puf_yearly = map_dfr(2017:2098, agg_year)


#---------------
# NIPA + macro
#---------------

pce_hist = read_csv('resources/pce_historical.csv', show_col_types = F)

mp = bind_rows(
  fread(file.path(MP, 'historical.csv'),  select = c('year', 'gdp')),
  fread(file.path(MP, 'projections.csv'), select = c('year', 'gdp', 'gdp_c'))
)


#---------------
# Per-year report
#---------------

cmp = puf_yearly %>%
  as_tibble() %>%
  left_join(pce_hist %>% select(year, nipa_total = gdp_c), by = 'year') %>%
  left_join(mp %>% as_tibble(), by = 'year') %>%
  mutate(
    diff_pct = (total_c - nipa_total) / nipa_total * 100,
    c_per_tu = total_c * 1e9 / total_weight,
    c_to_gdp = total_c / gdp
  )


cat('\n=== Aggregates by year (B$, weight in M tax units) ===\n')
cmp %>%
  transmute(year,
            tax_units_M = round(total_weight / 1e6, 1),
            total_c_B   = round(total_c, 0),
            nipa_B      = round(nipa_total, 0),
            diff_pct    = round(diff_pct, 2),
            gdp_B       = round(gdp, 0),
            c_per_tu    = round(c_per_tu, 0),
            c_to_gdp    = round(c_to_gdp, 3)) %>%
  print(n = Inf)


cat('\n=== Year-over-year growth, per c_* (%) ===\n')
puf_yearly %>%
  as_tibble() %>%
  select(year, all_of(c_cols)) %>%
  arrange(year) %>%
  mutate(across(all_of(c_cols), ~ round((. / lag(.) - 1) * 100, 2))) %>%
  print(n = Inf)


cat('\n=== Anomaly counts (negatives/NAs/Infs across all c_* and years) ===\n')
puf_yearly %>%
  as_tibble() %>%
  summarise(across(c(n_neg_total, n_na_total, n_inf_total), sum)) %>%
  print()


cat('\n=== Per-category trajectory check: largest YoY jump per category ===\n')
puf_yearly %>%
  as_tibble() %>%
  select(year, all_of(c_cols)) %>%
  arrange(year) %>%
  mutate(across(all_of(c_cols), ~ . / lag(.) - 1)) %>%
  pivot_longer(-year, names_to = 'cat', values_to = 'gr') %>%
  filter(!is.na(gr)) %>%
  group_by(cat) %>%
  filter(abs(gr) == max(abs(gr))) %>%
  ungroup() %>%
  transmute(cat, year, growth_pct = round(gr * 100, 2)) %>%
  arrange(desc(abs(growth_pct))) %>%
  print()


cat('\n=== C/GDP trajectory at decade endpoints ===\n')
cmp %>%
  filter(year %in% c(2017, 2019, 2025, 2026, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2098)) %>%
  transmute(year, total_c_B = round(total_c, 0), gdp_B = round(gdp, 0),
            c_to_gdp = round(c_to_gdp, 3)) %>%
  print()
