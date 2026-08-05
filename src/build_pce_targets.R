#---------------------------------------------------
# build_pce_targets.R
#
# Parses BEA NIPA Table 2.4.5U (Personal Consumption
# Expenditures by Type of Product) into two resource files:
#
#   resources/pce_targets_2017.csv  - 20 fine categories,
#                                     2017 values, same schema
#                                     as pce_targets_2023.csv
#   resources/pce_historical.csv    - 8 collapsed categories
#                                     (matching pce_collapse_map
#                                     in src/pce_benchmark.R)
#                                     plus total PCE (gdp_c),
#                                     annual 2017 to latest year
#
# Input: resources/nipa_2_4_5u.csv, downloaded as-is from BEA
# iTable (Table 2.4.5U, annual, millions of dollars).
#---------------------------------------------------

library(tidyverse)


#--------------------------
# Read raw NIPA 2.4.5U CSV
#--------------------------

# File has 3 preamble rows, 2 header rows (duplicated),
# then data. Columns: line_no, label, year_2017 ... year_YYYY.
# Trailing legend rows are dropped by the integer-line filter.
raw_header = read_lines('resources/nipa_2_4_5u.csv', n_max = 5)[4] %>%
  str_split(',', simplify = TRUE) %>%
  as.character()
years = raw_header[raw_header != '' & raw_header != 'Line'] %>% as.integer()

nipa_long = read_csv(
  'resources/nipa_2_4_5u.csv',
  skip            = 5,
  col_names       = c('line', 'label', paste0('y', years)),
  col_types       = cols(line = col_character(), label = col_character(), .default = col_double()),
  show_col_types  = FALSE
) %>%
  filter(str_detect(line, '^\\d+$')) %>%
  mutate(line = as.integer(line)) %>%
  pivot_longer(
    cols            = starts_with('y'),
    names_to        = 'year',
    names_transform = ~ as.integer(str_sub(., 2)),
    values_to       = 'value_millions'
  ) %>%
  mutate(value_billions = value_millions / 1000)


#------------------------------------
# Fine-category lookup (20 PCE rows)
#------------------------------------

# Mirrors the schema and contents of resources/pce_targets_2023.csv.
# 'other_services' is a RESIDUAL: line 280 (NIPA 'Other services')
# minus its child lines 281 (communication), 290 (education), and
# 333 (net foreign travel). Verified against the 2023 file: e.g.,
# 1582.924 - 302.719 - 338.573 - 45.710 = 895.922 ~ 896.0.
fine_lookup = tribble(
  ~category,             ~pce_name,                                                                             ~nipa_line, ~in_cex,
  'clothing',            'Clothing and footwear',                                                               104L,       TRUE,
  'motor_vehicles',      'Motor vehicles and parts',                                                            4L,         TRUE,
  'other_durables',      'Other durable goods',                                                                 62L,        TRUE,
  'furnishings',         'Furnishings and durable household equipment',                                         23L,        TRUE,
  'rec_goods',           'Recreational goods and vehicles',                                                     38L,        TRUE,
  'other_nondurables',   'Other nondurable goods',                                                              120L,       TRUE,
  'food_off_premises',   'Food and beverages purchased for off-premises consumption',                           73L,        TRUE,
  'communication',       'Communication',                                                                       281L,       TRUE,
  'npish',               'Final consumption expenditures of NPISH',                                             342L,       FALSE,
  'other_services',      'Other services (residual after removing communication/education/net foreign travel)', 280L,       TRUE,
  'transport_services',  'Transportation services',                                                             190L,       TRUE,
  'rec_services',        'Recreation services',                                                                 209L,       TRUE,
  'net_foreign_travel',  'Net foreign travel',                                                                  333L,       FALSE,
  'food_accommodations', 'Food services and accommodations',                                                    234L,       TRUE,
  'health_care',         'Health care',                                                                         172L,       TRUE,
  'utilities',           'Household utilities',                                                                 165L,       TRUE,
  'gasoline',            'Gasoline and other energy goods',                                                     113L,       TRUE,
  'education',           'Education',                                                                           290L,       TRUE,
  'financial_insurance', 'Financial services and insurance',                                                    252L,       TRUE,
  'housing',             'Housing',                                                                             153L,       TRUE
)


#-----------------------------------------
# Build fine-category x year value table
#-----------------------------------------

fine_yearly = fine_lookup %>%
  left_join(
    nipa_long %>% select(nipa_line = line, year, value_billions),
    by           = 'nipa_line',
    relationship = 'one-to-many'
  ) %>%
  # Override 'other_services' as residual (280 - 281 - 290 - 333)
  group_by(year) %>%
  mutate(
    v_280 = value_billions[category == 'other_services'],
    v_281 = value_billions[category == 'communication'],
    v_290 = value_billions[category == 'education'],
    v_333 = value_billions[category == 'net_foreign_travel']
  ) %>%
  ungroup() %>%
  mutate(value_billions = if_else(
    category == 'other_services',
    v_280 - v_281 - v_290 - v_333,
    value_billions
  )) %>%
  select(year, category, pce_name, nipa_line, in_cex, value_billions)

stopifnot(!any(is.na(fine_yearly$value_billions)))


#---------------------------------------
# Output 1: pce_targets_2017.csv (fine)
#---------------------------------------

fine_yearly %>%
  filter(year == 2017) %>%
  transmute(
    category,
    pce_name,
    pce_billions = round(value_billions, 1),
    nipa_line,
    in_cex
  ) %>%
  write_csv('resources/pce_targets_2017.csv')


#-----------------------------------------------
# Output 2: pce_historical.csv (8 collapsed + gdp_c)
#-----------------------------------------------

# Mirrors pce_collapse_map in src/pce_benchmark.R. Keep these in sync.
collapse_map = list(
  c_clothing              = 'clothing',
  c_motor_vehicles        = 'motor_vehicles',
  c_durables              = c('other_durables', 'furnishings', 'rec_goods'),
  c_other_nondurables     = 'other_nondurables',
  c_food_off_premises     = 'food_off_premises',
  c_gasoline              = 'gasoline',
  c_housing_utilities     = c('housing', 'utilities'),
  c_other_services_health = c('communication', 'npish', 'other_services',
                              'transport_services', 'rec_services',
                              'net_foreign_travel', 'food_accommodations',
                              'health_care', 'education', 'financial_insurance')
)

collapsed = imap_dfr(collapse_map, function(fine_cats, c_name) {
  fine_yearly %>%
    filter(category %in% fine_cats) %>%
    group_by(year) %>%
    summarise(value_billions = sum(value_billions), .groups = 'drop') %>%
    mutate(variable = c_name)
})

# gdp_c: total personal consumption expenditures, NIPA line 1
gdp_c = nipa_long %>%
  filter(line == 1) %>%
  transmute(year, value_billions, variable = 'gdp_c')

bind_rows(collapsed, gdp_c) %>%
  mutate(value_billions = round(value_billions, 3)) %>%
  pivot_wider(names_from = variable, values_from = value_billions) %>%
  arrange(year) %>%
  write_csv('resources/pce_historical.csv')


cat('wrote resources/pce_targets_2017.csv\n')
cat('wrote resources/pce_historical.csv\n')
