#--------------------------------------
# pce_benchmark.R
#
# Benchmarks consumption imputations to
# NIPA PCE control totals and computes
# expenditure shares for distributional
# price impact analysis.
#--------------------------------------


# 8 collapsed consumption categories for tariff analysis
pce_categories = c('clothing', 'motor_vehicles', 'durables', 'other_nondurables',
                   'food_off_premises', 'gasoline', 'housing_utilities', 'other_services_health')

# Mapping from collapsed categories to the fine 20 BEA PCE categories in the targets CSV.
# Used to aggregate fine NIPA targets to collapsed targets.
pce_collapse_map = list(
  clothing              = 'clothing',
  motor_vehicles        = 'motor_vehicles',
  durables              = c('other_durables', 'furnishings', 'rec_goods'),
  other_nondurables     = 'other_nondurables',
  food_off_premises     = 'food_off_premises',
  gasoline              = 'gasoline',
  housing_utilities     = c('housing', 'utilities'),
  other_services_health = c('communication', 'npish', 'other_services', 'transport_services',
                            'rec_services', 'net_foreign_travel', 'food_accommodations',
                            'health_care', 'education', 'financial_insurance')
)


#' Benchmark consumption categories to NIPA PCE control totals
#'
#' Reads fine (20-category) NIPA targets, aggregates to collapsed (8-category)
#' targets, then scales each category by the ratio of NIPA target to weighted total.
#' After scaling, caps extreme consumption-to-income ratios at cap_pctile.
#'
#' @param data       Data frame with columns for each collapsed category, plus
#'                   weight and income columns
#' @param weight_col Name of the weight column (default: FINLWT21)
#' @param income_col Name of the income column (default: income)
#' @param target_file Path to CSV with columns: category, pce_billions (fine 20-cat)
#' @param cap_pctile  Percentile cap for consumption-to-income ratio (default: 0.95)
#' @param annualize   Multiplier to annualize data (1 if data is already annual,
#'                    4 for quarterly data)
#' @return List with:
#'   $data            - benchmarked data frame
#'   $scale_factors   - named vector of per-category scaling factors
#'   $diagnostics     - data frame comparing pre/post totals to NIPA targets
benchmark_to_pce = function(data,
                            weight_col  = 'FINLWT21',
                            income_col  = 'income',
                            target_file = 'resources/pce_targets_2023.csv',
                            cap_pctile  = 0.95,
                            annualize   = 1) {

  # Read fine NIPA targets and aggregate to collapsed categories
  fine_targets = fread(target_file)
  fine_map     = setNames(fine_targets$pce_billions, fine_targets$category)
  target_map   = sapply(pce_collapse_map, function(fine_cats) sum(fine_map[fine_cats], na.rm = TRUE))
  w            = data[[weight_col]]

  # Step 1: Benchmark each collapsed category
  current = sapply(pce_categories, function(cat) {
    sum(data[[cat]] * annualize * w, na.rm = TRUE) / 1e9
  })

  sf = target_map[pce_categories] / current
  sf[!is.finite(sf)] = 1

  for (cat in pce_categories) {
    data[[cat]] = data[[cat]] * sf[cat]
  }

  # Recompute total consumption
  data$total_consumption = rowSums(data[pce_categories], na.rm = TRUE)

  # Step 2: Cap consumption-to-income ratio at cap_pctile
  inc = data[[income_col]]
  pos = inc > 0 & data$total_consumption > 0

  if (any(pos)) {
    ratios = data$total_consumption[pos] / inc[pos]
    cap    = quantile(ratios, cap_pctile, na.rm = TRUE)
    over   = pos & (data$total_consumption / inc > cap)

    if (any(over)) {
      shrink = (cap * inc[over]) / data$total_consumption[over]
      for (cat in pce_categories) {
        data[[cat]][over] = data[[cat]][over] * shrink
      }
      data$total_consumption[over] = cap * inc[over]
    }
  }

  # Diagnostics
  post = sapply(pce_categories, function(cat) {
    sum(data[[cat]] * annualize * w, na.rm = TRUE) / 1e9
  })

  diagnostics = data.frame(
    category       = pce_categories,
    nipa_target    = target_map[pce_categories],
    pre_benchmark  = round(current, 1),
    scale_factor   = round(sf, 2),
    post_benchmark = round(post, 1),
    cex_coverage   = paste0(round(current / target_map[pce_categories] * 100, 1), '%')
  )

  list(data = data, scale_factors = sf, diagnostics = diagnostics)
}


#' Compute expenditure shares for each category
#'
#' Adds _share columns: each category as a fraction of total consumption.
#'
#' @param data Data frame with category columns and total_consumption
#' @return Same data frame with additional _share columns appended
compute_expenditure_shares = function(data) {
  for (cat in pce_categories) {
    share_col = paste0(cat, '_share')
    data[[share_col]] = ifelse(
      data$total_consumption > 0,
      data[[cat]] / data$total_consumption,
      0
    )
  }
  data
}
