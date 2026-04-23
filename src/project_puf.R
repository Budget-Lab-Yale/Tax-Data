#---------------------------------------------------
# project_puf.R  (Phase 2: factor-ledger producer)
#
# Produces factor_ledger and weight_ledger. Does NOT
# mutate tax_units, does NOT write per-year CSVs.
# Those happen in Phase 4 (src/write_outputs.R) via
# materialize().
#
# factor_ledger: (year, variable, factor, source)
#   factor is CUMULATIVE from each variable's base
#   year (2017 for base-PUF variables) to `year`.
# weight_ledger: (year, id, weight)
#   per-(year, record) weight; 2017..2097.
#---------------------------------------------------


# Variables used for targeting only, dropped from per-year output.
vars_to_ignore = c('int_exp')

#------------------------------
# PCE benchmark consumption
#------------------------------

# Scale imputed consumption categories to NIPA PCE control totals. This
# modifies tax_units in place (Phase 1's final touch on the 2017 base).
source('src/pce_benchmark.R')
if ('c_clothing' %in% names(tax_units)) {
  bench = benchmark_to_pce(tax_units, weight_col = 'weight', annualize = 1)
  tax_units = bench$data
}

# Clean up Phase 1 intermediates.
rm(raw_puf, puf, puf_2017, sipp, ot, ot_microdata)

#-------------------------------------
# Factor-source inputs (unchanged)
#-------------------------------------

macro_projections = bind_rows(
  read_csv(file.path(interface_paths$`Macro-Projections`, 'historical.csv')),
  read_csv(file.path(interface_paths$`Macro-Projections`, 'projections.csv'))
)

pce_historical = read_csv('resources/pce_historical.csv', show_col_types = F) %>%
  pivot_longer(cols      = -year,
               names_to  = 'variable',
               values_to = 'value')

demog = macro_projections %>%
  select(year, contains('married_')) %>%
  pivot_longer(cols      = -year,
               names_sep = '_',
               names_to  = c('married', 'age'),
               values_to = 'n') %>%
  mutate(married = as.integer(married == 'married'),
         age     = pmin(80, as.integer(age))) %>%
  group_by(year, married, age) %>%
  summarise(n = sum(n),
            .groups = 'drop')

itemized_deduction_projections = read_csv('resources/itemized_deduction_projections.csv') %>%
  mutate(across(.cols = -year, .fns  = ~ . / lag(.) - 1))

income_near = read_csv('resources/cbo_1040.csv') %>%
  mutate(across(.cols = -year, .fns  = ~ . / lag(.) - 1)) %>%
  left_join(itemized_deduction_projections, by = 'year') %>%
  mutate(
    salt_inc_sales = if_else(is.na(salt_inc_sales), income,           salt_inc_sales),
    salt_prop      = if_else(is.na(salt_prop),      income,           salt_prop),
    mortgage       = if_else(is.na(mortgage),       txbl_int_div_ord, mortgage),
    charity        = if_else(is.na(charity),        income,           charity)
  ) %>%
  select(year, income, wages, int = txbl_int_div_ord, div = div_pref, kg = txbl_kg, pt,
         pensions = txbl_pensions, ui, mortgage, charity, salt_inc_sales, salt_prop) %>%
  pivot_longer(cols      = -year,
               names_to  = 'variable',
               values_to = 'near') %>%
  bind_rows(
    pce_historical %>%
      arrange(variable, year) %>%
      group_by(variable) %>%
      mutate(near = value / lag(value) - 1) %>%
      ungroup() %>%
      filter(!is.na(near)) %>%
      select(year, variable, near)
  )

income_far = macro_projections %>%
  mutate(income         = gdp,
         wages          = gdp_wages,
         int            = gdp_interest,
         div            = gdp_corp,
         kg             = gdp_corp,
         pt             = gdp_proprietors + gdp_rent + gdp_corp,
         rent           = gdp_rent,
         pensions       = outlays_mand_oasdi,
         ss             = outlays_mand_oasdi,
         ui             = gdp,
         mortgage       = gdp_interest,
         charity        = gdp,
         salt_inc_sales = gdp,
         salt_prop      = gdp,
         c_clothing              = gdp_c,
         c_motor_vehicles        = gdp_c,
         c_durables              = gdp_c,
         c_other_nondurables     = gdp_c,
         c_food_off_premises     = gdp_c,
         c_gasoline              = gdp_c,
         c_housing_utilities     = gdp_c,
         c_other_services_health = gdp_c) %>%
  select(year, all_of(variable_guide %>%
                        filter(!is.na(grow_with)) %>%
                        select(grow_with) %>%
                        deframe())) %>%
  pivot_longer(cols      = -year,
               names_to  = 'variable',
               values_to = 'far') %>%
  group_by(variable) %>%
  mutate(far = far / lag(far) - 1) %>%
  ungroup()

# income_factors: cumulative-from-2019 growth index per (year >= 2020, variable)
income_factors = income_far %>%
  filter(year > 2019) %>%
  left_join(income_near, by = c('year', 'variable')) %>%
  mutate(income_factor = if_else(is.na(near), far, near)) %>%
  left_join((.) %>%
              filter(variable == 'income') %>%
              select(year, rent = income_factor),
            by = 'year') %>%
  mutate(income_factor = if_else(is.na(income_factor), rent, income_factor)) %>%
  select(year, variable, income_factor) %>%
  group_by(variable) %>%
  mutate(income_factor = cumprod(1 + income_factor)) %>%
  ungroup()

#----------------------------------------------
# Basis adjustment factors (cycle-aware model)
#----------------------------------------------

cbo_kg_levels = read_csv('resources/cbo_1040.csv') %>% select(year, txbl_kg)

kg_growth = cbo_kg_levels %>%
  filter(year >= 2025) %>%
  mutate(kg_growth = txbl_kg / lag(txbl_kg) - 1) %>%
  filter(!is.na(kg_growth))

gdp_corp_growth = macro_projections %>%
  select(year, gdp_corp) %>%
  filter(year >= max(kg_growth$year)) %>%
  mutate(gdp_corp_growth = gdp_corp / lag(gdp_corp) - 1) %>%
  filter(!is.na(gdp_corp_growth))

sp500_extended = sp500_index %>% select(year, index)
for (y in 2026:2097) {
  prev_idx = sp500_extended$index[sp500_extended$year == y - 1]
  if (y <= max(kg_growth$year)) {
    g = kg_growth$kg_growth[kg_growth$year == y]
  } else {
    g = gdp_corp_growth$gdp_corp_growth[gdp_corp_growth$year == y]
  }
  sp500_extended = bind_rows(sp500_extended, tibble(year = y, index = prev_idx * (1 + g)))
}
sp500_interp_ext = approxfun(sp500_extended$year, sp500_extended$index, rule = 2)

pi_g = read_csv('resources/soca_hp_ingredients.csv')$pi_g

wb_shape = 0.7711
wb_scale = 9.1458
dw_at_boundary   = dweibull(19, shape = wb_shape, scale = wb_scale)
wb_mass_bucket_8 = pweibull(19, wb_shape, wb_scale) - pweibull(14, wb_shape, wb_scale)
exp_lambda       = dw_at_boundary / wb_mass_bucket_8 * pi_g[8] / pi_g[9]
h_top = 20 + 1 / exp_lambda
rm(dw_at_boundary, wb_mass_bucket_8, exp_lambda)

bucket_h   = c(1.25, 1.75, 2.50, 3.50, 4.50, 7.50, 12.50, 17.50, h_top)
bucket_names = c('Under 18 months', '18 months under 2 years', '2 years under 3 years',
                 '3 years under 4 years', '4 years under 5 years', '5 years under 10 years',
                 '10 years under 15 years', '15 years under 20 years', '20 years or more')

predict_weighted_ratio = function(y, model, sp500_fn, h_vals, buckets, weights) {
  newdata = tibble(
    bucket       = buckets,
    h            = h_vals,
    h_log_return = h_vals * log(1 + (sp500_fn(y) / sp500_fn(y - h_vals))^(1 / h_vals) - 1)
  )
  predicted = exp(predict(model, newdata = newdata))
  sum(weights * predicted)
}

r_bar_2017 = predict_weighted_ratio(2017, basis_model, sp500_interp_ext, bucket_h, bucket_names, pi_g)

basis_adjustment_factors = list()
for (y in 2018:2097) {
  r_bar_y = predict_weighted_ratio(y, basis_model, sp500_interp_ext, bucket_h, bucket_names, pi_g)
  basis_adjustment_factors[[as.character(y)]] = r_bar_y / r_bar_2017
}

#-------------------------------------
# Weight-source inputs
#-------------------------------------

irs_growth_factors_demog = read_csv('./resources/return_counts_2019.csv') %>%
  mutate(across(.cols = -c(filing_status, age_group),
                .fns  = ~ . / `2017`)) %>%
  pivot_longer(cols      = -c(filing_status, age_group),
               names_to  = 'year',
               values_to = 'population_factor') %>%
  mutate(year = as.integer(year))

irs_growth_factors_income = tables$table_1_4 %>%
  filter(variable %in% c('total_inc', 'wages', 'txbl_int', 'div', 'part_scorp',
                         'txbl_kg.income', 'gross_pens_dist', 'rent', 'ui', 'gross_ss')) %>%
  group_by(year, variable) %>%
  summarise(across(.cols = c(count, amount),
                   .fns  = sum),
            .groups = 'drop') %>%
  mutate(average = amount / count) %>%
  pivot_longer(cols      = c(count, amount, average),
               names_to  = 'metric') %>%
  pivot_wider(names_from = variable) %>%
  rename(income = total_inc, int = txbl_int, pt = part_scorp, kg = txbl_kg.income,
         pensions = gross_pens_dist, ss = gross_ss) %>%
  filter(metric == 'average') %>%
  select(-metric) %>%
  pivot_longer(cols      = -year,
               names_to  = 'variable',
               values_to = 'average') %>%
  mutate(income_factor = average / average[year == 2017]) %>%
  group_by(year) %>%
  mutate(income_factor = ifelse(is.na(income_factor),
                                income_factor[variable == 'income'],
                                income_factor)) %>%
  ungroup() %>%
  select(-average) %>%
  bind_rows(
    itemized_deduction_projections %>%
      filter(year <= 2019) %>%
      pivot_longer(cols      = -year,
                   names_to  = 'variable',
                   values_to = 'growth') %>%
      group_by(variable) %>%
      mutate(income_factor = cumprod(1 + replace_na(growth, 0))) %>%
      ungroup() %>%
      left_join(
        read_csv('./resources/return_counts_2019.csv') %>%
          pivot_longer(cols = -c(filing_status, age_group),
                       names_to = 'year',
                       names_transform = as.integer,
                       values_to = 'n') %>%
          group_by(year) %>%
          summarise(n = sum(n),
                    .groups = 'drop') %>%
          mutate(n = n / n[year == 2017]),
        by = 'year') %>%
      mutate(income_factor = income_factor / n) %>%
      select(-growth, -n)
  ) %>%
  bind_rows(
    pce_historical %>%
      group_by(variable) %>%
      mutate(income_factor = value / value[year == 2017]) %>%
      ungroup() %>%
      filter(year %in% 2018:2019) %>%
      left_join(
        read_csv('./resources/return_counts_2019.csv') %>%
          pivot_longer(cols            = -c(filing_status, age_group),
                       names_to        = 'year',
                       names_transform = as.integer,
                       values_to       = 'n') %>%
          group_by(year) %>%
          summarise(n = sum(n),
                    .groups = 'drop') %>%
          mutate(n = n / n[year == 2017]),
        by = 'year') %>%
      mutate(income_factor = income_factor / n) %>%
      select(year, variable, income_factor)
  )

# Two population_factors variants: 2018-19 uses a married-only table,
# 2020+ uses a married × age table. Rename to avoid the clobber that
# the original in-place mutation relied on.
population_factors_2018_19 = demog %>%
  filter(year %in% 2017:2019) %>%
  group_by(year, married) %>%
  summarise(n = sum(n),
            .groups = 'drop') %>%
  group_by(married) %>%
  mutate(population_factor = ifelse(n > 0,
                                    n / n[year == 2017],
                                    1)) %>%
  ungroup() %>%
  select(-n)

population_factors_2020plus = demog %>%
  filter(year >= 2019) %>%
  group_by(married, age) %>%
  mutate(population_factor = ifelse(n > 0, n / n[year == 2019], 1)) %>%
  ungroup() %>%
  select(-n)

# Ad-hoc level adjustments applied at every year 2020+ to hit CBO targets.
ad_hoc_factors = list(
  'salt_inc_sales' = 0.97,
  'salt_prop'      = 0.97,
  'first_mort_int' = 0.97
)

#-------------------------------------
# Helpers: compute weights + extensive factors per year
#-------------------------------------

# Per-year weight vector; pure function over the Phase-1 tax_units + source
# tables. Returns a tibble (id, weight, year).
compute_weights_for_year = function(y) {
  if (y == 2017L) {
    return(tax_units %>% select(id, weight) %>% mutate(year = 2017L))
  }

  if (y <= 2019L) {
    return(
      tax_units %>%
        mutate(year = y,
               age_group = case_when(
                 age1 < 26 ~ 1,
                 age1 < 35 ~ 2,
                 age1 < 45 ~ 3,
                 age1 < 55 ~ 4,
                 age1 < 65 ~ 5,
                 TRUE      ~ 6)) %>%
        left_join(irs_growth_factors_demog,
                  by = c('year', 'filing_status', 'age_group')) %>%
        mutate(weight = weight * if_else(filer == 1, population_factor, 1)) %>%
        select(-population_factor) %>%
        mutate(married = as.integer(filing_status == 2)) %>%
        left_join(population_factors_2018_19,
                  by = c('year', 'married')) %>%
        mutate(weight = weight * if_else(filer == 0, population_factor, 1)) %>%
        select(id, weight) %>%
        mutate(year = y)
    )
  }

  # y >= 2020: start from 2019 weights (which implicitly carry the 2018-19
  # demographic adjustment). Ages are NOT aged by project_puf — they stay at
  # their 2017 values in tax_units, which matches the legacy behavior.
  w2019 = compute_weights_for_year(2019L) %>% select(id, w2019 = weight)

  tax_units %>%
    mutate(married_flag = as.integer(filing_status == 2)) %>%
    select(id, married_flag, age1, age2, starts_with('dep_age')) %>%
    left_join(w2019, by = 'id') %>%
    pivot_longer(cols         = -c(id, married_flag, w2019),
                 names_prefix = 'age',
                 names_to     = 'person',
                 values_to    = 'age') %>%
    filter(!is.na(age)) %>%
    mutate(married = if_else(person == '1' | person == '2', married_flag, 0L)) %>%
    left_join(population_factors_2020plus %>% filter(year == y),
              by = c('married', 'age')) %>%
    mutate(weight = w2019 * population_factor * if_else(age < 18, 0.99, 1)) %>%
    group_by(id) %>%
    summarise(weight = mean(weight),
              .groups = 'drop') %>%
    mutate(year = y)
}

#-------------------------------------
# Build weight_ledger
#-------------------------------------

t0 = Sys.time()
weight_ledger = map_dfr(2017L:2097L, compute_weights_for_year) %>%
  arrange(year, id)
cat(sprintf('project_puf.R: weight_ledger built (%d rows, %.1fs)\n',
            nrow(weight_ledger), as.numeric(Sys.time() - t0, units = 'secs')))

#-------------------------------------
# Build factor_ledger
#-------------------------------------

# Resolved grow_with map (handles split variables like 'wages1' → 'wages').
var_growth_map = variable_guide %>%
  filter(!is.na(grow_with), !(variable %in% vars_to_ignore)) %>%
  select(variable, grow_with_vg = grow_with) %>%
  mutate(grow_with_resolved = if_else(
    str_sub(variable, end = -2) %in% c('wages', 'sole_prop', 'farm', 'part_se'),
    str_sub(variable, end = -2),
    grow_with_vg
  ))

# 2018-2019 factors (cumulative-from-2017 via irs_growth_factors_income).
fl_2018_2019 = var_growth_map %>%
  tidyr::crossing(year = 2018L:2019L) %>%
  left_join(irs_growth_factors_income %>%
              select(year, variable, income_factor) %>%
              rename(grow_with_resolved = variable),
            by = c('year', 'grow_with_resolved')) %>%
  transmute(year,
            variable,
            factor = income_factor,
            source = paste0('irs:', grow_with_resolved))

factor_2019_per_var = fl_2018_2019 %>%
  filter(year == 2019L) %>%
  select(variable, factor_2019 = factor)

# 2020-2097 factors. Reconstructs the legacy computation:
#   applied(V, Y) = intensive_factor(grow_with_resolved(V), Y) × ad_hoc(V)
#   intensive_factor = income_factors[Y, g] / extensive_factor(V, Y)
#   extensive_factor = sum((V != 0) × weight_Y) / sum((V != 0) × weight_2019)
#
# Cumulative-from-2017: factor_2019 × applied.
build_factor_rows_2020plus = function(y) {
  nw_y = weight_ledger %>% filter(year == y) %>% select(id, nw = weight)
  w19  = weight_ledger %>% filter(year == 2019L) %>% select(id, w19 = weight)

  ext_df = tax_units %>%
    select(id, all_of(var_growth_map$variable)) %>%
    left_join(w19, by = 'id') %>%
    left_join(nw_y, by = 'id') %>%
    summarise(across(.cols = all_of(var_growth_map$variable),
                     .fns  = ~ sum((. != 0) * nw, na.rm = TRUE) /
                              sum((. != 0) * w19, na.rm = TRUE))) %>%
    mutate(across(everything(), ~ if_else(is.nan(.), 1, .))) %>%
    pivot_longer(cols      = everything(),
                 names_to  = 'variable',
                 values_to = 'extensive_factor')

  income_y = income_factors %>%
    filter(year == y) %>%
    select(grow_with_resolved = variable, income_factor_y = income_factor)

  var_growth_map %>%
    left_join(ext_df, by = 'variable') %>%
    left_join(income_y, by = 'grow_with_resolved') %>%
    mutate(intensive_factor = income_factor_y / extensive_factor,
           ad_hoc_val = vapply(variable, function(v) {
             if (v %in% names(ad_hoc_factors)) ad_hoc_factors[[v]] else 1.0
           }, numeric(1)),
           applied = intensive_factor * ad_hoc_val) %>%
    left_join(factor_2019_per_var, by = 'variable') %>%
    transmute(year = y,
              variable,
              factor = factor_2019 * applied,
              source = 'income_factors × ad_hoc')
}

t0 = Sys.time()
fl_2020_2097 = map_dfr(2020L:2097L, build_factor_rows_2020plus)
cat(sprintf('project_puf.R: factor_ledger 2020-2097 built (%d rows, %.1fs)\n',
            nrow(fl_2020_2097), as.numeric(Sys.time() - t0, units = 'secs')))

# Special case: kg_lt_basis uses its own cycle-aware basis adjustment.
fl_kg_lt_basis = tibble(
  year     = 2018L:2097L,
  variable = 'kg_lt_basis',
  factor   = vapply(2018:2097, function(y)
                     basis_adjustment_factors[[as.character(y)]],
                    numeric(1)),
  source   = 'basis_adjustment'
)

factor_ledger = bind_rows(
  fl_2018_2019 %>% filter(variable != 'kg_lt_basis'),
  fl_2020_2097 %>% filter(variable != 'kg_lt_basis'),
  fl_kg_lt_basis
) %>%
  arrange(year, variable)

cat(sprintf('project_puf.R: factor_ledger final (%d rows)\n', nrow(factor_ledger)))

#-------------------------------------
# Cache (diagnostic; main artifact is in-memory for Phase 4)
#-------------------------------------

write_rds(factor_ledger, file.path(output_path, 'factor_ledger.rds'))
write_rds(weight_ledger, file.path(output_path, 'weight_ledger.rds'))
