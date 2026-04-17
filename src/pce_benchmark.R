#--------------------------------------
# pce_benchmark.R
#
# Benchmarks consumption imputations to
# NIPA PCE control totals and computes
# expenditure shares for distributional
# price impact analysis.
#--------------------------------------


# 8 collapsed consumption categories for tariff analysis
pce_categories = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
                   'c_food_off_premises', 'c_gasoline', 'c_housing_utilities', 'c_other_services_health')

# Mapping from collapsed categories to the fine 20 BEA PCE categories in the targets CSV.
# Used to aggregate fine NIPA targets to collapsed targets.
pce_collapse_map = list(
  c_clothing              = 'clothing',
  c_motor_vehicles        = 'motor_vehicles',
  c_durables              = c('other_durables', 'furnishings', 'rec_goods'),
  c_other_nondurables     = 'other_nondurables',
  c_food_off_premises     = 'food_off_premises',
  c_gasoline              = 'gasoline',
  c_housing_utilities     = c('housing', 'utilities'),
  c_other_services_health = c('communication', 'npish', 'other_services', 'transport_services',
                              'rec_services', 'net_foreign_travel', 'food_accommodations',
                              'health_care', 'education', 'financial_insurance')
)


#' Benchmark consumption categories to NIPA PCE control totals
#'
#' Two-stage scaling:
#'   Stage 1 (capped multiplicative): each category is multiplied by
#'     min(T_k / A_k, sf_cap). Categories with raw scale factor at or below
#'     sf_cap hit their NIPA target exactly. Categories above sf_cap are
#'     only scaled up to sf_cap; the remainder is reallocated in Stage 2.
#'   Stage 2 (deficit allocation): the per-category deficit T_k - A_k^1
#'     is distributed across households proportional to each household's
#'     post-Stage-1 total consumption. Equivalently, capped categories
#'     borrow population-share signal from the rest of the data rather
#'     than assuming CEX's within-category composition is uniformly
#'     scalable up to 11x.
#'
#' Post-benchmark aggregates match NIPA targets exactly by construction.
#'
#' @param data       Data frame with columns for each collapsed category, plus
#'                   a weight column
#' @param weight_col Name of the weight column (default: FINLWT21)
#' @param target_file Path to CSV with columns: category, pce_billions (fine 20-cat)
#' @param sf_cap      Maximum allowable per-category scale factor. Categories
#'                    with raw scale > sf_cap get capped at sf_cap and the
#'                    shortfall is reallocated proportional to HH total C.
#'                    Set to Inf to disable (== pure category matching).
#'                    Default 2.
#' @param annualize   Multiplier to annualize data (1 if data is already annual,
#'                    4 for quarterly data)
#' @return List with:
#'   $data            - benchmarked data frame
#'   $scale_factors   - named vector of per-category applied scaling factors
#'   $diagnostics     - data frame with raw and capped scale factors,
#'                      pre/post totals, and per-category deficit allocations
benchmark_to_pce = function(data,
                            weight_col  = 'FINLWT21',
                            target_file = 'resources/pce_targets_2023.csv',
                            sf_cap      = 2,
                            annualize   = 1) {

  # Read fine NIPA targets and aggregate to collapsed categories
  fine_targets = fread(target_file)
  fine_map     = setNames(fine_targets$pce_billions, fine_targets$category)
  target_map   = sapply(pce_collapse_map, function(fine_cats) sum(fine_map[fine_cats], na.rm = TRUE))
  w            = data[[weight_col]]

  # Use plain unnamed vectors keyed by pce_categories position to avoid
  # name-propagation gotchas in pmin/pmax/subsetting.
  K       = length(pce_categories)
  tgts    = as.numeric(target_map[pce_categories])
  current = vapply(pce_categories,
                   function(cat) sum(data[[cat]] * annualize * w, na.rm = TRUE) / 1e9,
                   numeric(1), USE.NAMES = FALSE)

  sf_raw = tgts / current
  sf_raw[!is.finite(sf_raw)] = 1
  sf     = pmin(sf_raw, sf_cap)

  # Stage 1: capped multiplicative scaling
  for (k in seq_len(K)) {
    data[[pce_categories[k]]] = data[[pce_categories[k]]] * sf[k]
  }

  # Stage 1 aggregates (billions)
  stage1 = vapply(pce_categories,
                  function(cat) sum(data[[cat]] * annualize * w, na.rm = TRUE) / 1e9,
                  numeric(1), USE.NAMES = FALSE)

  # Stage 2: allocate per-category deficit proportional to HH post-Stage-1 total C.
  # Δc_{i,k} = (deficit_k_in_$) × (C_i / Σ_j w_j C_j)  , with conversion from
  # billion-dollar deficit to per-row storage units via /annualize.
  deficit      = pmax(0, tgts - stage1)
  total_hh     = rowSums(data[pce_categories], na.rm = TRUE)
  agg_total_hh = sum(total_hh * w, na.rm = TRUE)

  if (agg_total_hh > 0 && any(deficit > 0)) {
    for (k in seq_len(K)) {
      if (!is.na(deficit[k]) && deficit[k] > 0) {
        data[[pce_categories[k]]] = data[[pce_categories[k]]] +
          (deficit[k] * 1e9 / annualize) * (total_hh / agg_total_hh)
      }
    }
  }

  # Recompute total consumption
  data$total_consumption = rowSums(data[pce_categories], na.rm = TRUE)

  # Post-benchmark aggregates (should equal targets by construction)
  post = vapply(pce_categories,
                function(cat) sum(data[[cat]] * annualize * w, na.rm = TRUE) / 1e9,
                numeric(1), USE.NAMES = FALSE)

  diagnostics = data.frame(
    category             = pce_categories,
    nipa_target          = round(tgts, 1),
    pre_benchmark        = round(current, 1),
    scale_factor_raw     = round(sf_raw, 2),
    scale_factor_applied = round(sf, 2),
    deficit_allocated    = round(deficit, 1),
    post_benchmark       = round(post, 1),
    cex_coverage         = paste0(round(current / tgts * 100, 1), '%')
  )

  list(data = data, scale_factors = setNames(sf, pce_categories), diagnostics = diagnostics)
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
