#--------------------------------------
# pce_benchmark.R
#
# Benchmarks consumption imputations to
# NIPA PCE control totals and computes
# expenditure shares for distributional
# price impact analysis.
#--------------------------------------


# All 20 BEA PCE major categories
pce_categories = c(
  'clothing', 'motor_vehicles', 'other_durables', 'furnishings',
  'rec_goods', 'other_nondurables', 'food_off_premises', 'communication',
  'npish', 'other_services', 'transport_services', 'rec_services',
  'net_foreign_travel', 'food_accommodations', 'health_care', 'utilities',
  'gasoline', 'education', 'financial_insurance', 'housing'
)

# Categories with no direct CEX source -- distributed proportional to
# total CEX-observed consumption (neutral distribution, following BLS)
pce_neutral = c('npish', 'net_foreign_travel')

# CEX-observed categories (the other 18)
pce_observed = setdiff(pce_categories, pce_neutral)


#' Benchmark consumption categories to NIPA PCE control totals
#'
#' For CEX-observed categories: scales by ratio of NIPA target to weighted total.
#' For neutral-distribution categories (NPISH, net foreign travel): allocates
#' the NIPA target proportional to each unit's share of total CEX consumption.
#' After scaling, caps extreme consumption-to-income ratios at cap_pctile.
#'
#' @param data       Data frame with columns for each PCE category, plus
#'                   weight and income columns
#' @param weight_col Name of the weight column (default: FINLWT21)
#' @param income_col Name of the income column (default: income)
#' @param target_file Path to CSV with columns: category, pce_billions
#' @param cap_pctile  Percentile cap for consumption-to-income ratio (default: 0.95)
#' @param annualize   Multiplier to annualize data (4 for quarterly CEX CQ data,
#'                    1 if data is already annual)
#' @return List with:
#'   $data            - benchmarked data frame
#'   $scale_factors   - named vector of per-category scaling factors
#'   $diagnostics     - data frame comparing pre/post totals to NIPA targets
benchmark_to_pce = function(data,
                            weight_col  = 'FINLWT21',
                            income_col  = 'income',
                            target_file = 'resources/pce_targets_2023.csv',
                            cap_pctile  = 0.95,
                            annualize   = 4) {

  targets    = fread(target_file)
  target_map = setNames(targets$pce_billions, targets$category)
  w          = data[[weight_col]]

  # Step 1: Benchmark CEX-observed categories
  current = sapply(pce_observed, function(cat) {
    sum(data[[cat]] * annualize * w, na.rm = TRUE) / 1e9
  })

  sf = target_map[pce_observed] / current
  sf[!is.finite(sf)] = 1

  for (cat in pce_observed) {
    data[[cat]] = data[[cat]] * sf[cat]
  }

  # Step 2: Allocate neutral-distribution categories proportional to
  # total CEX-observed consumption (each unit gets its share of the NIPA target)
  cex_total = rowSums(data[pce_observed], na.rm = TRUE)
  weighted_total = sum(cex_total * annualize * w, na.rm = TRUE) / 1e9

  for (cat in pce_neutral) {
    nipa_target = target_map[cat]
    # Each unit's share of total consumption * NIPA target (in quarterly $)
    data[[cat]] = ifelse(
      weighted_total > 0,
      cex_total * (nipa_target / weighted_total),
      0
    )
  }

  # Recompute total consumption (all 20 categories)
  data$total_consumption = rowSums(data[pce_categories], na.rm = TRUE)

  # Step 3: Cap consumption-to-income ratio at cap_pctile
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

  pre_all = c(current, setNames(rep(0, length(pce_neutral)), pce_neutral))

  diagnostics = data.frame(
    category       = pce_categories,
    nipa_target    = target_map[pce_categories],
    pre_benchmark  = round(pre_all[pce_categories], 1),
    scale_factor   = round(ifelse(pre_all[pce_categories] > 0,
                                  target_map[pce_categories] / pre_all[pce_categories],
                                  NA), 2),
    post_benchmark = round(post, 1),
    cex_coverage   = ifelse(pre_all[pce_categories] > 0,
                            paste0(round(pre_all[pce_categories] / target_map[pce_categories] * 100, 1), '%'),
                            'neutral')
  )

  list(data = data, scale_factors = sf, diagnostics = diagnostics)
}


#' Compute expenditure shares for each PCE category
#'
#' Adds _share columns: each category as a fraction of total consumption.
#'
#' @param data Data frame with PCE category columns and total_consumption
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
