#---------------------------------------------
# forbes_splice.R
#
# Weight-only Forbes billionaire top-tail splice.
#
# Core invariants:
#   - Existing PUF record variables are never edited.
#   - Forbes records are appended as synthetic tax units.
#   - Existing-record weights are calibrated so key income categories,
#     total count, and non-Forbes net worth are preserved within tolerance.
#---------------------------------------------

source('src/imputations/wealth_schema.R')


forbes_metadata_vars = c(
  'forbes_flag', 'forbes_source_year', 'forbes_rank', 'forbes_name',
  'forbes_source_category', 'forbes_rank_group', 'forbes_donor_id',
  'forbes_net_worth', 'forbes_fiscal_income'
)

forbes_income_categories = list(
  capital_gains = c(kg_lt = 1, kg_st = 1, other_gains = 1),
  dividends = c(div_ord = 1, div_pref = 1),
  interest = c(txbl_int = 1, exempt_int = 1),
  business = c(
    sole_prop = 1, farm = 1,
    scorp_active = 1, scorp_passive = 1,
    part_active = 1, part_passive = 1,
    scorp_active_loss = -1, scorp_passive_loss = -1, scorp_179 = -1,
    part_active_loss = -1, part_passive_loss = -1, part_179 = -1
  ),
  rent_estate_other = c(
    rent = 1, estate = 1, other_inc = 1,
    rent_loss = -1, estate_loss = -1
  ),
  wages_pensions = c(wages = 1, gross_pens_dist = 1)
)


coalesce_col = function(df, col, default = 0) {
  if (!(col %in% names(df))) return(rep(default, nrow(df)))
  x = df[[col]]
  x[is.na(x)] = default
  x
}


forbes_net_worth = function(df) {
  assets = rowSums(df[, intersect(wealth_value_asset_vars, names(df)),
                     drop = FALSE], na.rm = TRUE)
  debts = rowSums(df[, intersect(wealth_value_debt_vars, names(df)),
                    drop = FALSE], na.rm = TRUE)
  assets - debts
}


forbes_category_value = function(df, category) {
  signs = forbes_income_categories[[category]]
  if (is.null(signs)) stop('Unknown Forbes income category: ', category)
  out = rep(0, nrow(df))
  for (v in names(signs)) {
    out = out + unname(signs[[v]]) * coalesce_col(df, v, 0)
  }
  out
}


forbes_category_matrix = function(df,
                                  categories = names(forbes_income_categories)) {
  out = as.data.frame(lapply(categories, function(cat) {
    forbes_category_value(df, cat)
  }))
  names(out) = categories
  out
}


forbes_fiscal_income = function(df) {
  rowSums(forbes_category_matrix(df), na.rm = TRUE)
}


add_forbes_metadata_defaults = function(df) {
  defaults = list(
    forbes_flag = 0L,
    forbes_source_year = NA_integer_,
    forbes_rank = NA_integer_,
    forbes_name = NA_character_,
    forbes_source_category = NA_character_,
    forbes_rank_group = NA_character_,
    forbes_donor_id = NA_real_,
    forbes_net_worth = NA_real_,
    forbes_fiscal_income = NA_real_
  )
  for (v in names(defaults)) {
    if (!(v %in% names(df))) df[[v]] = defaults[[v]]
  }
  df
}


read_forbes_input = function(path = 'resources/forbes/forbes_billionaires_2022_2025.csv') {
  if (!file.exists(path)) {
    warning('Forbes input not found: ', path, '. Skipping Forbes splice.')
    return(tibble::tibble())
  }
  out = readr::read_csv(path, show_col_types = FALSE)
  if (nrow(out) == 0L) return(out)
  required = c('year', 'rank', 'name', 'net_worth', 'source_category')
  missing = setdiff(required, names(out))
  if (length(missing) > 0L) {
    stop('Forbes input missing required columns: ',
         paste(missing, collapse = ', '))
  }
  out %>%
    dplyr::mutate(
      year = as.integer(year),
      rank = as.integer(rank),
      net_worth = as.numeric(net_worth)
    )
}


read_bsyz_params = function(path = 'resources/forbes/bsyz_fiscal_income_params.csv') {
  if (!file.exists(path)) {
    stop('BSYZ parameter file not found: ', path)
  }
  params = readr::read_csv(path, show_col_types = FALSE)
  required = c('rank_group', 'rank_min', 'rank_max',
               'fiscal_income_to_wealth', 'category', 'share')
  missing = setdiff(required, names(params))
  if (length(missing) > 0L) {
    stop('BSYZ params missing required columns: ',
         paste(missing, collapse = ', '))
  }
  if (!all(params$category %in% names(forbes_income_categories))) {
    stop('BSYZ params contain unknown categories: ',
         paste(setdiff(unique(params$category),
                       names(forbes_income_categories)), collapse = ', '))
  }
  params %>%
    dplyr::mutate(
      rank_min = as.integer(rank_min),
      rank_max = as.integer(rank_max),
      fiscal_income_to_wealth = as.numeric(fiscal_income_to_wealth),
      share = as.numeric(share)
    ) %>%
    dplyr::group_by(rank_group) %>%
    dplyr::mutate(share = share / sum(share)) %>%
    dplyr::ungroup()
}


assign_bsyz_rank_group = function(rank, params) {
  groups = params %>%
    dplyr::distinct(rank_group, rank_min, rank_max) %>%
    dplyr::arrange(rank_min)
  out = character(length(rank))
  for (i in seq_along(rank)) {
    hit = which(rank[i] >= groups$rank_min & rank[i] <= groups$rank_max)
    if (length(hit) == 0L) {
      stop('No BSYZ rank group for Forbes rank ', rank[i])
    }
    out[i] = groups$rank_group[hit[1]]
  }
  out
}


forbes_target_categories = function(forbes_df, params) {
  if (nrow(forbes_df) == 0L) return(tibble::tibble())
  forbes_df = forbes_df %>%
    dplyr::mutate(rank_group = assign_bsyz_rank_group(rank, params))
  params_key = params %>%
    dplyr::select(rank_group, category, fiscal_income_to_wealth, share)
  forbes_df %>%
    dplyr::select(year, rank, name, net_worth, rank_group) %>%
    dplyr::left_join(params_key, by = 'rank_group') %>%
    dplyr::mutate(target = net_worth * fiscal_income_to_wealth * share) %>%
    dplyr::select(year, rank, name, rank_group, category, target)
}


hybrid_top_tail_score = function(df) {
  nw = forbes_net_worth(df)
  fiscal = forbes_fiscal_income(df)
  cap = forbes_category_value(df, 'capital_gains') +
        forbes_category_value(df, 'dividends') +
        forbes_category_value(df, 'interest')
  rank01 = function(x) {
    if (length(x) <= 1L) return(rep(1, length(x)))
    rank(x, ties.method = 'average', na.last = 'keep') / length(x)
  }
  rank01(nw) + rank01(abs(fiscal)) + rank01(abs(cap))
}


forbes_rank01 = function(x) {
  x[is.na(x)] = -Inf
  if (length(x) <= 1L) return(rep(1, length(x)))
  rank(x, ties.method = 'average') / length(x)
}


forbes_income_signal = function(df) {
  rowSums(abs(forbes_category_matrix(df)), na.rm = TRUE)
}


choose_forbes_composition_donors = function(base_df,
                                            n_donors = 500L,
                                            billionaire_threshold = 1e9) {
  nw = forbes_net_worth(base_df)
  ok = !is.na(nw) & nw > 0 & nw < billionaire_threshold &
       coalesce_col(base_df, 'weight', 0) > 0
  if (!any(ok)) stop('No positive-net-worth non-billionaire donor rows found.')
  cand = base_df[ok, , drop = FALSE]
  cand$.__score = hybrid_top_tail_score(cand)
  cand %>%
    dplyr::arrange(dplyr::desc(.__score)) %>%
    dplyr::select(-.__score) %>%
    utils::head(n_donors)
}


category_default_shares = function(donor_pool, category, target_sign) {
  signs = forbes_income_categories[[category]]
  eligible = names(signs)[signs == target_sign]
  if (length(eligible) == 0L) return(numeric(0))
  totals = sapply(eligible, function(v) {
    sum(pmax(coalesce_col(donor_pool, v, 0), 0) *
          coalesce_col(donor_pool, 'weight', 1), na.rm = TRUE)
  })
  if (sum(totals) <= 0) totals[] = 1
  totals / sum(totals)
}


set_category_on_row = function(row, target, category, donor_pool,
                                floor = 1e-6) {
  signs = forbes_income_categories[[category]]
  vars = names(signs)
  for (v in vars) if (!(v %in% names(row))) row[[v]] = 0

  current = forbes_category_value(row, category)
  if (abs(target) < floor) {
    for (v in vars) row[[v]] = 0
    return(row)
  }

  if (abs(current) >= floor && sign(current) == sign(target)) {
    factor = target / current
    for (v in vars) row[[v]] = coalesce_col(row, v, 0) * factor
    return(row)
  }

  target_sign = if (target >= 0) 1 else -1
  shares = category_default_shares(donor_pool, category, target_sign)
  for (v in vars) row[[v]] = 0
  for (v in names(shares)) row[[v]] = abs(target) * shares[[v]]
  row
}


scale_wealth_to_net_worth = function(row, target_net_worth) {
  current_nw = forbes_net_worth(row)
  wealth_vars = intersect(wealth_output_vars, names(row))

  if (!is.na(current_nw) && abs(current_nw) > 1e-6) {
    # Scale donor values to the Forbes net-worth target.
    factor = target_net_worth / current_nw
    for (v in wealth_vars) row[[v]] = coalesce_col(row, v, 0) * factor
  }

  # If the donor had no usable wealth vector, or if all wealth vars were
  # absent, fall back to a simple public-equity-only allocation.
  if (length(wealth_vars) == 0L || abs(forbes_net_worth(row) - target_net_worth) > 1e-3) {
    for (v in wealth_output_vars) if (!(v %in% names(row))) row[[v]] = 0
    row[, wealth_output_vars] = 0
    row[['value.equities']] = target_net_worth
    row[['basis.equities']] = 0.2 * target_net_worth
    row[['accruals.equities']] = 0.118 * target_net_worth
  }
  row
}


make_forbes_id = function(year, rank, max_existing_id) {
  candidate = as.numeric(year) * 1e6 + as.numeric(rank)
  ifelse(candidate > max_existing_id, candidate,
         max_existing_id + as.numeric(year) * 1e3 + as.numeric(rank))
}


build_forbes_rows_for_year = function(base_df, forbes_year_df, params,
                                      billionaire_threshold = 1e9) {
  if (nrow(forbes_year_df) == 0L) return(tibble::tibble())
  donor_pool = choose_forbes_composition_donors(
    base_df, n_donors = max(500L, nrow(forbes_year_df)),
    billionaire_threshold = billionaire_threshold)
  targets = forbes_target_categories(forbes_year_df, params)
  max_id = max(base_df$id, na.rm = TRUE)
  out = vector('list', nrow(forbes_year_df))

  for (i in seq_len(nrow(forbes_year_df))) {
    f = forbes_year_df[i, , drop = FALSE]
    donor = donor_pool[((i - 1L) %% nrow(donor_pool)) + 1L, , drop = FALSE]
    row = donor
    row = add_forbes_metadata_defaults(row)
    row = scale_wealth_to_net_worth(row, f$net_worth)

    t_i = targets %>% dplyr::filter(rank == f$rank)
    for (cat in names(forbes_income_categories)) {
      target = t_i$target[t_i$category == cat]
      if (length(target) == 0L) target = 0
      row = set_category_on_row(row, target, cat, donor_pool)
    }

    fiscal = sum(t_i$target)
    if ('E00100' %in% names(row)) row$E00100 = fiscal
    if ('txbl_pens_dist' %in% names(row) && 'gross_pens_dist' %in% names(row)) {
      row$txbl_pens_dist = row$gross_pens_dist
    }
    for (v in intersect(c('ui', 'gross_ss', 'state_ref', 'alimony',
                          'txbl_ira_dist'), names(row))) {
      row[[v]] = 0
    }

    row$id = make_forbes_id(f$year, f$rank, max_id)
    row$weight = if ('weight' %in% names(f) && !is.na(f$weight)) {
      as.numeric(f$weight)
    } else 1
    row$forbes_flag = 1L
    row$forbes_source_year = as.integer(f$year)
    row$forbes_rank = as.integer(f$rank)
    row$forbes_name = as.character(f$name)
    row$forbes_source_category = as.character(f$source_category)
    row$forbes_rank_group = assign_bsyz_rank_group(f$rank, params)
    row$forbes_donor_id = donor$id
    row$forbes_net_worth = f$net_worth
    row$forbes_fiscal_income = fiscal
    out[[i]] = row
  }

  dplyr::bind_rows(out)
}


build_splice_pools = function(base_df,
                              source_n = 5000L,
                              receiver_n = 10000L,
                              billionaire_threshold = 1e9) {
  nw = forbes_net_worth(base_df)
  ok = coalesce_col(base_df, 'weight', 0) > 0 & !is.na(nw) & nw > 0 &
       nw < billionaire_threshold
  cand = base_df[ok, , drop = FALSE]
  if (nrow(cand) < 2L) {
    stop('Forbes splice calibration needs at least two candidate rows.')
  }

  source_take = min(source_n, max(1L, floor(nrow(cand) / 2L)))
  receiver_take = min(receiver_n, nrow(cand) - source_take)

  income_signal = forbes_income_signal(cand)
  nw_cand = nw[ok]
  cand$.__income_density = income_signal / pmax(nw_cand, 1)
  cand$.__wealth_density = nw_cand / pmax(income_signal, 1)

  source = cand %>%
    dplyr::mutate(
      .__score = forbes_rank01(abs(forbes_fiscal_income(cand))) +
                 forbes_rank01(.__income_density)
    ) %>%
    dplyr::arrange(dplyr::desc(.__score)) %>%
    utils::head(source_take)

  receiver = cand %>%
    dplyr::mutate(
      .__score = forbes_rank01(nw_cand) +
                 forbes_rank01(.__wealth_density)
    ) %>%
    dplyr::filter(!(id %in% source$id)) %>%
    dplyr::arrange(dplyr::desc(.__score)) %>%
    utils::head(receiver_take)

  list(source_ids = source$id, receiver_ids = receiver$id)
}


solve_forbes_weight_calibration = function(base_df, forbes_rows,
                                           source_ids, receiver_ids,
                                           rel_tol = 0.001,
                                           income_abs_tol = 1e3,
                                           count_abs_tol = 1e-6,
                                           wealth_abs_tol = 1e6,
                                           receiver_max_factor = 10,
                                           receiver_penalty = 1.1) {
  if (nrow(forbes_rows) == 0L) {
    return(list(
      weights = tibble::tibble(),
      constraints = tibble::tibble(),
      status = 'empty'
    ))
  }
  if (!requireNamespace('lpSolveAPI', quietly = TRUE)) {
    stop('lpSolveAPI is required for Forbes splice calibration.')
  }

  source = base_df[match(source_ids, base_df$id), , drop = FALSE]
  receiver = base_df[match(receiver_ids, base_df$id), , drop = FALSE]
  source = source[!is.na(source$id), , drop = FALSE]
  receiver = receiver[!is.na(receiver$id), , drop = FALSE]
  n_s = nrow(source)
  n_r = nrow(receiver)
  n_vars = n_s + n_r
  if (n_vars == 0L) stop('Forbes splice calibration has no candidate rows.')

  constraint_names = c('count', 'net_worth', names(forbes_income_categories))
  x_source = list(
    count = rep(1, n_s),
    net_worth = forbes_net_worth(source)
  )
  x_receiver = list(
    count = rep(1, n_r),
    net_worth = forbes_net_worth(receiver)
  )
  x_forbes = list(
    count = rep(1, nrow(forbes_rows)),
    net_worth = forbes_net_worth(forbes_rows)
  )
  for (cat in names(forbes_income_categories)) {
    x_source[[cat]] = forbes_category_value(source, cat)
    x_receiver[[cat]] = forbes_category_value(receiver, cat)
    x_forbes[[cat]] = forbes_category_value(forbes_rows, cat)
  }

  targets = sapply(constraint_names, function(nm) {
    if (nm == 'net_worth') 0 else -sum(forbes_rows$weight * x_forbes[[nm]])
  })
  abs_tol = sapply(constraint_names, function(nm) {
    floor = if (nm == 'count') count_abs_tol else if (nm == 'net_worth') wealth_abs_tol else income_abs_tol
    max(abs(targets[[nm]]) * rel_tol, floor)
  })

  lprw = lpSolveAPI::make.lp(0, n_vars)
  lpSolveAPI::set.objfn(lprw, c(rep(1, n_s), rep(receiver_penalty, n_r)))
  upper = c(source$weight, receiver$weight * (receiver_max_factor - 1))
  lpSolveAPI::set.bounds(lprw, lower = rep(0, n_vars),
                         upper = upper, columns = seq_len(n_vars))

  for (nm in constraint_names) {
    coef = c(-x_source[[nm]], x_receiver[[nm]])
    lpSolveAPI::add.constraint(lprw, coef, '<=', targets[[nm]] + abs_tol[[nm]])
    lpSolveAPI::add.constraint(lprw, coef, '>=', targets[[nm]] - abs_tol[[nm]])
  }

  solution = solve(lprw)
  if (solution != 0) {
    stop('Forbes splice calibration failed with lpSolve status ', solution)
  }
  sol = lpSolveAPI::get.variables(lprw)
  d_source = sol[seq_len(n_s)]
  d_receiver = sol[n_s + seq_len(n_r)]

  weight_rows = dplyr::bind_rows(
    tibble::tibble(id = source$id, old_weight = source$weight,
                   delta_weight = -d_source, role = 'source'),
    tibble::tibble(id = receiver$id, old_weight = receiver$weight,
                   delta_weight = d_receiver, role = 'receiver')
  ) %>%
    dplyr::filter(abs(delta_weight) > 1e-10) %>%
    dplyr::mutate(new_weight = old_weight + delta_weight,
                  weight_factor = new_weight / old_weight)

  constraint_rows = lapply(constraint_names, function(nm) {
    achieved = sum(-d_source * x_source[[nm]]) +
               sum( d_receiver * x_receiver[[nm]])
    tibble::tibble(
      constraint = nm,
      target_delta = targets[[nm]],
      achieved_delta = achieved,
      gap = achieved - targets[[nm]],
      tolerance = abs_tol[[nm]],
      ok = abs(achieved - targets[[nm]]) <= abs_tol[[nm]] + 1e-8
    )
  }) %>% dplyr::bind_rows()

  list(weights = weight_rows,
       constraints = constraint_rows,
       status = 'solved')
}


purge_scf_billionaires = function(scf_tax_units, threshold = 1e9) {
  nw = rowSums(scf_tax_units[, wealth_asset_vars, drop = FALSE], na.rm = TRUE) -
       rowSums(scf_tax_units[, wealth_debt_vars, drop = FALSE], na.rm = TRUE)
  drop = !is.na(nw) & nw >= threshold
  diagnostics = tibble::tibble(
    threshold = threshold,
    rows_before = nrow(scf_tax_units),
    rows_dropped = sum(drop),
    rows_after = sum(!drop),
    weighted_count_dropped = sum(scf_tax_units$weight[drop], na.rm = TRUE),
    net_worth_dropped = sum(scf_tax_units$weight[drop] * nw[drop], na.rm = TRUE)
  )
  list(scf_tax_units = scf_tax_units[!drop, , drop = FALSE],
       diagnostics = diagnostics)
}


build_forbes_splice = function(base,
                               factor_ledger,
                               weight_ledger,
                               module_deltas,
                               bucketed_factors,
                               record_bucket,
                               years = 2022L:2025L,
                               forbes_path = 'resources/forbes/forbes_billionaires_2022_2025.csv',
                               params_path = 'resources/forbes/bsyz_fiscal_income_params.csv',
                               billionaire_threshold = 1e9) {
  forbes_input = read_forbes_input(forbes_path)
  params = read_bsyz_params(params_path)
  if (nrow(forbes_input) == 0L) {
    return(list(
      rows = tibble::tibble(),
      weight_adjustments = tibble::tibble(),
      constraints = tibble::tibble(),
      diagnostics = tibble::tibble(status = 'empty_forbes_input'),
      years = years
    ))
  }

  row_list = list()
  weight_list = list()
  constraint_list = list()
  diag_list = list()

  for (y in years) {
    puf_y = materialize(y, base, factor_ledger, weight_ledger,
                        module_deltas,
                        bucketed_factors = bucketed_factors,
                        record_bucket = record_bucket)
    puf_y = add_forbes_metadata_defaults(puf_y)
    f_y = forbes_input %>% dplyr::filter(year == y)
    if (nrow(f_y) == 0L) next

    rows_y = build_forbes_rows_for_year(
      puf_y, f_y, params,
      billionaire_threshold = billionaire_threshold)
    pools = build_splice_pools(puf_y,
                               billionaire_threshold = billionaire_threshold)
    calib = solve_forbes_weight_calibration(
      puf_y, rows_y, pools$source_ids, pools$receiver_ids)

    rows_y$splice_year = y
    weights_y = calib$weights %>% dplyr::mutate(year = y)
    constraints_y = calib$constraints %>% dplyr::mutate(year = y)
    diag_y = tibble::tibble(
      year = y,
      forbes_rows = nrow(rows_y),
      forbes_weight = sum(rows_y$weight),
      forbes_net_worth = sum(rows_y$weight * forbes_net_worth(rows_y)),
      forbes_fiscal_income = sum(rows_y$weight * forbes_fiscal_income(rows_y)),
      source_pool_n = length(pools$source_ids),
      receiver_pool_n = length(pools$receiver_ids),
      calibration_status = calib$status
    )

    row_list[[as.character(y)]] = rows_y
    weight_list[[as.character(y)]] = weights_y
    constraint_list[[as.character(y)]] = constraints_y
    diag_list[[as.character(y)]] = diag_y
  }

  list(
    rows = dplyr::bind_rows(row_list),
    weight_adjustments = dplyr::bind_rows(weight_list),
    constraints = dplyr::bind_rows(constraint_list),
    diagnostics = dplyr::bind_rows(diag_list),
    years = years
  )
}


factor_ratio = function(tbl, variable, target_year, source_year,
                        bucket = NULL) {
  if (is.null(tbl) || nrow(tbl) == 0L || !(variable %in% tbl$variable)) return(NA_real_)
  rows = tbl[tbl$variable == variable, , drop = FALSE]
  if (!is.null(bucket) && 'bucket' %in% names(rows)) {
    rows = rows[rows$bucket == bucket, , drop = FALSE]
  }
  f_t = rows$factor[match(target_year, rows$year)]
  f_s = rows$factor[match(source_year, rows$year)]
  if (length(f_t) != 1L || is.na(f_t)) return(NA_real_)
  if (length(f_s) != 1L || is.na(f_s)) f_s = 1
  if (f_s == 0) return(NA_real_)
  f_t / f_s
}


project_forbes_rows = function(rows, target_year, source_year,
                               factor_ledger = NULL,
                               bucketed_factors = NULL,
                               wealth_bucket = 'pct99to100') {
  if (target_year == source_year || nrow(rows) == 0L) return(rows)
  numeric_vars = names(rows)[vapply(rows, is.numeric, logical(1))]
  for (v in numeric_vars) {
    if (v %in% c('id', 'weight', forbes_metadata_vars, 'splice_year')) next
    r = factor_ratio(bucketed_factors, v, target_year, source_year,
                     bucket = wealth_bucket)
    if (is.na(r)) r = factor_ratio(factor_ledger, v, target_year, source_year)
    if (!is.na(r)) rows[[v]] = rows[[v]] * r
  }
  rows$forbes_net_worth = forbes_net_worth(rows)
  rows$forbes_fiscal_income = forbes_fiscal_income(rows)
  if ('E00100' %in% names(rows)) rows$E00100 = rows$forbes_fiscal_income
  rows
}


apply_forbes_splice_to_materialized = function(out, target_year,
                                               forbes_splice = NULL,
                                               factor_ledger = NULL,
                                               bucketed_factors = NULL) {
  out = add_forbes_metadata_defaults(out)
  if (is.null(forbes_splice) ||
      is.null(forbes_splice$rows) ||
      nrow(forbes_splice$rows) == 0L ||
      target_year < min(forbes_splice$years)) {
    return(out)
  }

  splice_year = if (target_year %in% forbes_splice$years) {
    target_year
  } else {
    max(forbes_splice$years)
  }

  adj = forbes_splice$weight_adjustments %>%
    dplyr::filter(year == splice_year)
  if (nrow(adj) > 0L) {
    idx = match(adj$id, out$id)
    ok = !is.na(idx)
    out$weight[idx[ok]] = out$weight[idx[ok]] * adj$weight_factor[ok]
  }

  rows = forbes_splice$rows[
    forbes_splice$rows$splice_year == splice_year, , drop = FALSE]
  rows = project_forbes_rows(rows, target_year, splice_year,
                             factor_ledger, bucketed_factors)

  if (all(c('q_death1', 'forbes_donor_id') %in% names(rows)) &&
      'q_death1' %in% names(out)) {
    didx = match(rows$forbes_donor_id, out$id)
    rows$q_death1 = out$q_death1[didx]
    if ('q_death2' %in% names(rows) && 'q_death2' %in% names(out)) {
      rows$q_death2 = out$q_death2[didx]
    }
  }

  missing_out = setdiff(names(rows), names(out))
  for (v in missing_out) out[[v]] = NA
  missing_rows = setdiff(names(out), names(rows))
  for (v in missing_rows) rows[[v]] = NA
  rows = rows[, names(out), drop = FALSE]
  dplyr::bind_rows(out, rows)
}


if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(readr)
    library(lpSolveAPI)
  })

  cat('--- forbes_splice.R tests ---\n')

  scf_fx = tibble(
    weight = c(1, 1),
    cash = c(1e6, 2e9), equities = 0, bonds = 0, dc = 0, db = 0,
    life_ins = 0, annuities = 0, trusts = 0, other_fin = 0,
    pass_throughs = 0, primary_home = 0, other_home = 0, re_fund = 0,
    other_nonfin = 0,
    primary_mortgage = 0, other_mortgage = 0, credit_lines = 0,
    credit_cards = 0, installment_debt = 0, other_debt = 0
  )
  purged = purge_scf_billionaires(scf_fx, threshold = 1e9)
  stopifnot(nrow(purged$scf_tax_units) == 1L,
            purged$diagnostics$rows_dropped == 1L)
  cat('  [PASS] SCF billionaire purge\n')

  params_fx = tibble(
    rank_group = rep(c('top100', 'next300'), each = 6),
    rank_min = rep(c(1L, 101L), each = 6),
    rank_max = rep(c(100L, 400L), each = 6),
    fiscal_income_to_wealth = rep(c(0.02, 0.03), each = 6),
    category = rep(names(forbes_income_categories), 2),
    share = rep(c(0.6, 0.2, 0.1, -0.05, 0.1, 0.05), 2)
  ) %>% group_by(rank_group) %>% mutate(share = share / sum(share)) %>% ungroup()
  stopifnot(assign_bsyz_rank_group(c(1L, 250L), params_fx)[1] == 'top100')
  cat('  [PASS] rank group assignment\n')

  base_fx = tibble(
    id = 1:4,
    weight = c(10, 10, 10, 10),
    E00100 = c(100, 50, 0, 0),
    q_death1 = c(0.01, 0.02, 0.03, 0.04),
    q_death2 = c(0.02, 0.03, 0.04, 0.05),
    kg_lt = c(100, 50, 0, 0), kg_st = 0, other_gains = 0,
    div_ord = 0, div_pref = 0, txbl_int = 0, exempt_int = 0,
    sole_prop = 0, farm = 0, scorp_active = 0, scorp_passive = 0,
    part_active = 0, part_passive = 0,
    scorp_active_loss = 0, scorp_passive_loss = 0, scorp_179 = 0,
    part_active_loss = 0, part_passive_loss = 0, part_179 = 0,
    rent = 0, estate = 0, other_inc = 0, rent_loss = 0, estate_loss = 0,
    wages = 0, gross_pens_dist = 0,
    `value.equities` = c(100, 200, 10, 10),
    `value.cash` = 0, `value.bonds` = 0, `value.dc` = 0, `value.db` = 0,
    `value.life_ins` = 0, `value.annuities` = 0, `value.trusts` = 0,
    `value.other_fin` = 0, `value.pass_throughs` = 0,
    `value.primary_home` = 0, `value.other_home` = 0, `value.re_fund` = 0,
    `value.other_nonfin` = 0,
    `value.primary_mortgage` = 0, `value.other_mortgage` = 0,
    `value.credit_lines` = 0, `value.credit_cards` = 0,
    `value.installment_debt` = 0, `value.other_debt` = 0
  )
  for (v in wealth_basis_vars) base_fx[[v]] = 0
  for (v in wealth_accrual_vars) base_fx[[v]] = 0

  pools_fx = build_splice_pools(base_fx, source_n = 1, receiver_n = 1)
  stopifnot(pools_fx$source_ids == 1L,
            pools_fx$receiver_ids == 2L)
  cat('  [PASS] calibration pools separate income and wealth roles\n')

  forbes_input_fx = tibble(
    year = 2022L,
    rank = 1L,
    name = 'Fixture Billionaire',
    net_worth = 1e9,
    source_category = 'technology'
  )
  forbes_rows_fx = build_forbes_rows_for_year(
    base_fx, forbes_input_fx, params_fx)
  stopifnot(nrow(forbes_rows_fx) == 1L,
            forbes_rows_fx$forbes_flag == 1L,
            abs(forbes_net_worth(forbes_rows_fx) - 1e9) < 1,
            abs(forbes_fiscal_income(forbes_rows_fx) - 2e7) < 1,
            abs(forbes_category_value(forbes_rows_fx, 'business') + 1e6) < 1,
            abs(forbes_rows_fx$E00100 - 2e7) < 1)
  cat('  [PASS] Forbes row construction hits wealth and income targets\n')

  proj_ledger_fx = tibble(
    year = 2023L,
    variable = c('kg_lt', 'value.equities'),
    factor = c(2, 1.25),
    source = 'fixture'
  )
  projected_fx = project_forbes_rows(
    forbes_rows_fx, 2023L, 2022L,
    factor_ledger = proj_ledger_fx)
  stopifnot(projected_fx$forbes_fiscal_income >
              forbes_rows_fx$forbes_fiscal_income,
            projected_fx$forbes_net_worth >
              forbes_rows_fx$forbes_net_worth,
            abs(projected_fx$E00100 -
                  projected_fx$forbes_fiscal_income) < 1)
  cat('  [PASS] projected Forbes metadata recomputes from aged values\n')

  forbes_rows_fx$splice_year = 2022L
  splice_fx = list(
    rows = forbes_rows_fx,
    weight_adjustments = tibble(
      year = 2022L,
      id = 1L,
      old_weight = 10,
      delta_weight = -5,
      role = 'source',
      new_weight = 5,
      weight_factor = 0.5
    ),
    years = 2022L:2025L
  )
  applied_fx = apply_forbes_splice_to_materialized(
    base_fx, 2022L, splice_fx)
  appended_fx = applied_fx %>% dplyr::filter(forbes_flag == 1L)
  stopifnot(nrow(applied_fx) == nrow(base_fx) + 1L,
            applied_fx$weight[applied_fx$id == 1L] == 5,
            nrow(appended_fx) == 1L,
            appended_fx$q_death1 ==
              base_fx$q_death1[base_fx$id == appended_fx$forbes_donor_id])
  cat('  [PASS] materialized output applies weights and appends Forbes rows\n')

  forbes_fx = base_fx[1, , drop = FALSE]
  forbes_fx$id = 2025000001
  forbes_fx$weight = 1
  forbes_fx$kg_lt = 150
  forbes_fx$`value.equities` = 1e9
  calib = solve_forbes_weight_calibration(
    base_fx, forbes_fx,
    source_ids = 1,
    receiver_ids = 2,
    receiver_max_factor = 2)
  stopifnot(calib$status == 'solved',
            all(calib$constraints$ok))
  cat('  [PASS] category/count/net-worth LP calibration\n')

  cat('\nAll tests passed.\n')
}
