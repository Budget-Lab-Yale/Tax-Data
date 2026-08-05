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


# Profile fields the v2 assembler consumes, with their post-read types. The
# assembler does the demographic MAPPING (birth_date->age, marital->filing
# status, etc.); read_forbes_input only joins and coerces, leaving the raw
# Forbes values intact. Missing-profile coverage is handled (with documented
# defaults) in the assembler, not silently dropped here.
forbes_profile_vars = c('birth_date', 'gender', 'marital_status', 'children',
                        'family', 'self_made', 'self_made_type',
                        'residence_state', 'industry', 'n_assets',
                        'public_equity_interactive', 'public_equity_all')


read_forbes_input = function(path = 'resources/forbes/forbes_billionaires_2022_2025.csv',
                             profiles_path = 'resources/forbes/forbes_profiles.csv') {
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
  out = out %>%
    dplyr::mutate(
      year = as.integer(year),
      rank = as.integer(rank),
      net_worth = as.numeric(net_worth)
    )

  # v2 enrichment: left-join per-profile demographics + asset composition by
  # uri (one profile row per uri; the per-year base repeats a uri across
  # years). Absent profiles file -> v1-style behavior: the assembler sees the
  # columns as NA and falls back to documented demographic/composition
  # defaults. We WARN rather than fail so the pipeline degrades gracefully.
  if (file.exists(profiles_path) && 'forbes_uri' %in% names(out)) {
    profiles = readr::read_csv(profiles_path, show_col_types = FALSE) %>%
      dplyr::distinct(uri, .keep_all = TRUE) %>%
      dplyr::mutate(
        children                  = suppressWarnings(as.integer(children)),
        n_assets                  = suppressWarnings(as.integer(n_assets)),
        family                    = as.logical(family),
        self_made                 = as.logical(self_made),
        public_equity_interactive = suppressWarnings(as.numeric(public_equity_interactive)),
        public_equity_all         = suppressWarnings(as.numeric(public_equity_all))
      )
    out = out %>% dplyr::left_join(profiles, by = c('forbes_uri' = 'uri'))

    # Reconcile the public-equity figure: prefer the interactive (live-tracker
    # direct stake); fall back to the all-holdings sum when interactive is 0
    # (some inherited holders carry their entire stake as a single
    # interactive=false row — see resources/forbes/README.md). NA -> 0.
    pei = coalesce_col(out, 'public_equity_interactive', 0)
    pea = coalesce_col(out, 'public_equity_all', 0)
    out$public_equity_value = ifelse(pei > 0, pei, pea)
  } else {
    warning('Forbes profiles not found: ', profiles_path,
            '. v2 assembler will use demographic/composition defaults.')
    for (v in c(forbes_profile_vars, 'public_equity_value')) out[[v]] = NA
  }
  out
}


# BSYZ params are stored WIDE: one row per rank_group, with a column per
# income category holding that category's share of fiscal income. Wide
# format makes the rank_group join in forbes_target_categories provably
# many-to-one (each billionaire -> its single group row), so there is no
# fan-out and no risk of a stray duplicate row double-counting a category —
# the failure mode a long (one-row-per-category) layout invited.
read_bsyz_params = function(path = 'resources/forbes/bsyz_fiscal_income_params.csv') {
  if (!file.exists(path)) {
    stop('BSYZ parameter file not found: ', path)
  }
  params = readr::read_csv(path, show_col_types = FALSE)
  cats = names(forbes_income_categories)
  required = c('rank_group', 'rank_min', 'rank_max',
               'fiscal_income_to_wealth', cats)
  missing = setdiff(required, names(params))
  if (length(missing) > 0L) {
    stop('BSYZ params missing required columns: ',
         paste(missing, collapse = ', '))
  }
  if (any(duplicated(params$rank_group))) {
    stop('BSYZ params has duplicate rank_group row(s): ',
         paste(unique(params$rank_group[duplicated(params$rank_group)]),
               collapse = ', '))
  }
  params = params %>%
    dplyr::mutate(
      rank_min = as.integer(rank_min),
      rank_max = as.integer(rank_max),
      fiscal_income_to_wealth = as.numeric(fiscal_income_to_wealth)
    )
  # Normalize each group's category shares to sum to 1 (raw BSYZ shares sum
  # to ~1.01 and include a negative business share). Row-wise across the
  # category columns.
  share_mat = as.matrix(params[, cats])
  params[, cats] = share_mat / rowSums(share_mat)
  params
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


# Returns long (year, rank, name, rank_group, category, target): one row per
# (billionaire × category). Internally the join is many-to-one on rank_group
# (wide params, one row per group); the long shape the solver consumes is
# built explicitly from the per-category columns rather than via a
# many-to-many fan-out join.
forbes_target_categories = function(forbes_df, params) {
  if (nrow(forbes_df) == 0L) return(tibble::tibble())
  cats = names(forbes_income_categories)
  fdf = forbes_df %>%
    dplyr::mutate(rank_group = assign_bsyz_rank_group(rank, params)) %>%
    dplyr::select(year, rank, name, net_worth, rank_group) %>%
    dplyr::left_join(
      params %>% dplyr::select(rank_group, fiscal_income_to_wealth,
                               dplyr::all_of(cats)),
      by = 'rank_group')
  dplyr::bind_rows(lapply(cats, function(cat) {
    tibble::tibble(
      year       = fdf$year,
      rank       = fdf$rank,
      name       = fdf$name,
      rank_group = fdf$rank_group,
      category   = cat,
      target     = fdf$net_worth * fdf$fiscal_income_to_wealth * fdf[[cat]]
    )
  }))
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


# v2 repurpose: this is no longer a wholesale-copy donor pool. The v2
# assembler builds each synthetic record from sourced components and never
# copies a PUF row. We keep this top-of-distribution selection only as the
# reference pool from which category_default_shares derives AGGREGATE
# within-category income shares (the user-chosen "PUF-top aggregate shares"
# allocation): e.g. how capital_gains splits across kg_lt / kg_st /
# other_gains among the wealthiest filers. No per-record identity is carried.
choose_forbes_share_pool = function(base_df,
                                    n_records = 500L,
                                    billionaire_threshold = 1e9) {
  nw = forbes_net_worth(base_df)
  ok = !is.na(nw) & nw > 0 & nw < billionaire_threshold &
       coalesce_col(base_df, 'weight', 0) > 0
  if (!any(ok)) stop('No positive-net-worth non-billionaire records for share pool.')
  cand = base_df[ok, , drop = FALSE]
  cand$.__score = hybrid_top_tail_score(cand)
  cand %>%
    dplyr::arrange(dplyr::desc(.__score)) %>%
    dplyr::select(-.__score) %>%
    utils::head(n_records)
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


# ===========================================================================
# v2 assembler: build each synthetic record from sourced components.
#
# v1 copied a donor PUF row wholesale and overwrote its wealth + 6 income
# aggregates, leaving demographics, basis, deductions, mortality, and the
# split-sum components donor-inherited (wrong in load-bearing ways for the
# deemed-realization / estate / wealth-tax policies this feeds). v2 starts
# from a zeroed schema template and populates every load-bearing field from
# Forbes /info, /assets, the founder model, BSYZ, and PUF-top relationships.
# ===========================================================================

# Zero/NA template carrying base_df's EXACT schema (names + types). Only the
# column STRUCTURE is borrowed (not values), so appended rows stay schema-
# compatible for the downstream bind_rows. The long tail (credits, AMT,
# consumption c_*, ...) is left at its light default — 0 — negligible for the
# wealth / estate / income-at-top policies this feeds (documented choice).
make_forbes_template = function(base_df) {
  tmpl = base_df[1, , drop = FALSE]
  for (v in names(tmpl)) {
    tmpl[[v]] = if (is.numeric(tmpl[[v]])) 0 else NA
  }
  tmpl
}


forbes_birth_year = function(birth_date) {
  suppressWarnings(as.integer(substr(as.character(birth_date), 1, 4)))
}


# Demographics from Forbes /info. age1 from birthDate; male1 from gender;
# filing_status from maritalStatus (MARS: 1 single, 2 MFJ, 4 HoH); spouse
# (age2/male2) assumed for MFJ (opposite sex, same age — Forbes gives the
# principal only). Coverage gaps default to married male, no dependents
# (user-confirmed: billionaires skew married/male). Dependents: children are
# overwhelmingly adult at billionaire ages, so deps are claimed only for a
# young (<45) principal, capped at 3 at plausible minor ages — negligible
# either way for the target policies.
set_forbes_demographics = function(row, f, list_year) {
  by = forbes_birth_year(f$birth_date)
  age1 = if (length(by) == 1L && !is.na(by)) list_year - by else 65L
  if (is.na(age1) || age1 < 18L || age1 > 100L) age1 = 65L
  row$age1 = as.integer(age1)

  g = tolower(as.character(f$gender))
  row$male1 = if (length(g) == 1L && g %in% c('m', 'male')) 1L else
              if (length(g) == 1L && g %in% c('w', 'f', 'female')) 0L else 1L

  ms = tolower(as.character(f$marital_status))
  married = length(ms) != 1L || is.na(ms) || ms == '' ||
            grepl('married|remarried', ms)        # "Widowed, Remarried" -> married
  nch = suppressWarnings(as.integer(f$children)); if (is.na(nch)) nch = 0L
  n_dep = if (row$age1 < 45L) min(nch, 3L) else 0L

  row$filing_status = if (married) 2L else if (n_dep > 0L) 4L else 1L
  if ('filer' %in% names(row)) row$filer = 1L
  if ('dep_status' %in% names(row)) row$dep_status = 0L
  if ('blind1' %in% names(row)) row$blind1 = 0L
  if ('blind2' %in% names(row)) row$blind2 = 0L

  if (row$filing_status == 2L) {
    row$age2 = row$age1
    row$male2 = 1L - row$male1
  } else {
    row$age2 = NA_integer_
    row$male2 = NA_integer_
  }

  row$n_dep = as.integer(n_dep)
  dep_ages = c(12L, 9L, 6L)
  for (k in 1:3) {
    col = paste0('dep_age', k)
    if (col %in% names(row)) row[[col]] = if (k <= n_dep) dep_ages[k] else NA_integer_
  }
  if ('n_dep_ctc' %in% names(row))  row$n_dep_ctc  = as.integer(n_dep)
  if ('n_dep_eitc' %in% names(row)) row$n_dep_eitc = as.integer(n_dep)
  row
}


# Wealth composition from Forbes /assets + industry. Public ticker'd holdings
# -> value.equities; the residual (net_worth - public) -> value.pass_throughs,
# or value.re_fund when the industry is real estate. Reconciled so value.*
# sums EXACTLY to net_worth. Debts are left at 0: Forbes net worth is already
# net, so we model gross assets ~ net worth (documented simplification).
set_forbes_wealth = function(row, net_worth, public_equity_value, industry) {
  wv = intersect(wealth_output_vars, names(row))
  for (v in wealth_output_vars) if (!(v %in% names(row))) row[[v]] = 0
  row[, wealth_output_vars] = 0

  nw = max(as.numeric(net_worth), 0)
  pub = as.numeric(public_equity_value); if (is.na(pub)) pub = 0
  pub = min(max(pub, 0), nw)                       # cap public at net worth
  residual = nw - pub

  row[['value.equities']] = pub
  ind = tolower(as.character(industry))
  is_re = length(ind) == 1L && !is.na(ind) && grepl('real.?estate', ind)
  if (is_re) row[['value.re_fund']] = residual
  else       row[['value.pass_throughs']] = residual
  row
}


# Founder basis model keyed on selfMade.type. Self-made (and unknown ->
# treated self-made: only ~1.5% unknown, and zero basis is the higher-tax-base
# default for deemed realization) carry ~0 basis on the APPRECIATING founding
# assets (equities, pass_throughs, re_fund) -> deemed-realization base ~ full
# value. Inherited carry stepped-up basis ~ value. Homes keep basis ~ value in
# both (not the founding gain). basis.* mirror wealth.R::to_output_schema.
set_forbes_basis = function(row, self_made_type) {
  t = tolower(as.character(self_made_type))
  inherited = length(t) == 1L && !is.na(t) && grepl('inherit', t)
  founder = !inherited
  vv = function(col) coalesce_col(row, col, 0)
  row[['basis.equities']]      = if (founder) 0 else vv('value.equities')
  row[['basis.pass_throughs']] = if (founder) 0 else vv('value.pass_throughs')
  row[['basis.re_fund']]       = if (founder) 0 else vv('value.re_fund')
  row[['basis.primary_home']]  = vv('value.primary_home')
  row[['basis.other_home']]    = vv('value.other_home')
  row
}


# Split-sum identities. Tax-Simulator depends on x1 + x2 == x to machine
# precision (see CLAUDE.md). v1 left these donor-inherited while overwriting
# the aggregates, breaking the identity on synthetic rows. v2 assigns all to
# the primary filer (x1 = x, x2 = 0): exact by construction, and billionaires'
# wage/SE content is near-zero anyway. wagebill_* / sstb_* are W-2-paid /
# SSTB-flag fields, not split-pair identities; left at their template 0.
forbes_split_pairs = list(
  c('wages', 'wages1', 'wages2'),
  c('ot', 'ot1', 'ot2'),
  c('tips', 'tips1', 'tips2'),
  c('sole_prop', 'sole_prop1', 'sole_prop2'),
  c('part_se', 'part_se1', 'part_se2'),
  c('farm', 'farm1', 'farm2')
)

set_business_splits_primary = function(row) {
  for (p in forbes_split_pairs) {
    if (all(p %in% names(row))) {
      row[[p[2]]] = coalesce_col(row, p[1], 0)
      row[[p[3]]] = 0
    }
  }
  row
}


# Deduction estimation on PUF-top relationships (user-confirmed approach).
# Charity is the high-leverage, flagged piece: fit log(char_total) ~
# log(net_worth) on the PUF top (net worth >= top_q quantile) with positive
# charity, predict at billionaire wealth, then CAP at cap_frac x net_worth to
# stop lognormal overshoot. Cash/noncash split by the top's weighted ratio.
# salt_prop ~ a flat share of wealth (capped at $10k downstream anyway).
# Mortgage interest ~ 0 (billionaires pay cash). All flagged for review.
fit_forbes_deduction_model = function(base_df, top_q = 0.999, cap_frac = 0.05) {
  nw = forbes_net_worth(base_df)
  w  = coalesce_col(base_df, 'weight', 0)
  ok = !is.na(nw) & nw > 0 & w > 0
  if (!any(ok)) return(list(b0 = NA, b1 = NA, cash_share = 0.5,
                            cap_frac = cap_frac, salt_prop_rate = 0))
  thr = stats::quantile(nw[ok], top_q, names = FALSE)
  top = ok & nw >= thr
  char_cash = coalesce_col(base_df, 'char_cash', 0)
  char_nc   = coalesce_col(base_df, 'char_noncash', 0)
  char_tot  = char_cash + char_nc

  reg = top & char_tot > 0
  b0 = NA; b1 = NA
  if (sum(reg) >= 30L) {
    fit = stats::lm(log(char_tot[reg]) ~ log(nw[reg]), weights = w[reg])
    cf = stats::coef(fit); b0 = unname(cf[1]); b1 = unname(cf[2])
  }
  den_c = sum(w[top] * char_tot[top])
  cash_share = if (den_c > 0) sum(w[top] * char_cash[top]) / den_c else 0.5
  sp = coalesce_col(base_df, 'salt_prop', 0)
  den_w = sum(w[top] * nw[top])
  salt_prop_rate = if (den_w > 0) sum(w[top] * sp[top]) / den_w else 0

  list(b0 = b0, b1 = b1, cash_share = cash_share,
       cap_frac = cap_frac, salt_prop_rate = salt_prop_rate)
}

set_forbes_deductions = function(row, model, net_worth) {
  nw = max(as.numeric(net_worth), 0)
  char_tot = 0
  if (!is.na(model$b0) && !is.na(model$b1) && nw > 0) {
    char_tot = min(exp(model$b0 + model$b1 * log(nw)), model$cap_frac * nw)
  }
  if ('char_cash' %in% names(row))    row$char_cash    = char_tot * model$cash_share
  if ('char_noncash' %in% names(row)) row$char_noncash = char_tot * (1 - model$cash_share)
  if ('salt_prop' %in% names(row))    row$salt_prop    = model$salt_prop_rate * nw
  # Mortgage interest ~ 0: first/second_mort_* stay at template 0.
  row
}


make_forbes_id = function(year, rank, max_existing_id) {
  candidate = as.numeric(year) * 1e6 + as.numeric(rank)
  ifelse(candidate > max_existing_id, candidate,
         max_existing_id + as.numeric(year) * 1e3 + as.numeric(rank))
}


# Assemble one synthetic record per Forbes billionaire from sourced
# components. share_pool feeds the within-category income allocation (PUF-top
# aggregate shares); dedn_model is the fitted PUF-top deduction model. No
# donor row is copied — each field is set explicitly. q_death1/q_death2 are
# left at the template default here and filled by build_forbes_mortality_ledger
# (pinned p100) via apply_forbes_splice_to_materialized.
build_forbes_rows_for_year = function(base_df, forbes_year_df, params,
                                      share_pool, dedn_model,
                                      billionaire_threshold = 1e9) {
  if (nrow(forbes_year_df) == 0L) return(tibble::tibble())
  targets = forbes_target_categories(forbes_year_df, params)
  max_id = max(base_df$id, na.rm = TRUE)
  template = make_forbes_template(base_df)
  list_year = as.integer(forbes_year_df$year[1])
  out = vector('list', nrow(forbes_year_df))

  for (i in seq_len(nrow(forbes_year_df))) {
    f = forbes_year_df[i, , drop = FALSE]
    row = add_forbes_metadata_defaults(template)

    # --- demographics, wealth composition, founder basis (from /info, /assets) ---
    row = set_forbes_demographics(row, f, list_year)
    pev = if ('public_equity_value' %in% names(f)) f$public_equity_value else NA
    ind = if ('industry' %in% names(f)) f$industry else NA
    smt = if ('self_made_type' %in% names(f)) f$self_made_type else NA
    row = set_forbes_wealth(row, f$net_worth, pev, ind)
    row = set_forbes_basis(row, smt)

    # --- BSYZ income, allocated within-category from PUF-top aggregate shares ---
    t_i = targets %>% dplyr::filter(rank == f$rank)
    for (cat in names(forbes_income_categories)) {
      target = t_i$target[t_i$category == cat]
      if (length(target) == 0L) target = 0
      row = set_category_on_row(row, target, cat, share_pool)
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

    # --- split-sum identities (all to primary), then deductions ---
    row = set_business_splits_primary(row)
    row = set_forbes_deductions(row, dedn_model, f$net_worth)

    # --- identifiers + metadata ---
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
    row$forbes_donor_id = NA_real_           # v2: no donor copied
    row$forbes_net_worth = f$net_worth
    row$forbes_fiscal_income = fiscal
    out[[i]] = row
  }

  out_rows = dplyr::bind_rows(out)

  # accruals.* = Z1-rate x value, reusing the tested compute_accruals (DC and
  # trusts are 0 for billionaires, so the DC equity-share blend is moot).
  # Guarded: in the standalone test block accruals.R is not sourced, and these
  # are consumed by nothing downstream (verified), so 0 is harmless there.
  if (exists('compute_accruals') && nrow(out_rows) > 0L) {
    n = nrow(out_rows)
    acc = compute_accruals(out_rows,
                           donor_dc_eq_share = rep(NA_real_, n),
                           donor_dc_total    = rep(0, n),
                           age_older         = out_rows$age1)
    for (v in names(acc)) out_rows[[v]] = acc[[v]]
  }
  out_rows
}


# Per-record synthetic mortality, pinned to national p100. build_chetty_pctile
# ranks WITHIN its input, so running it on billionaires-only would spread them
# across percentiles instead of pinning the top; we hardcode pctile = 100 and
# go straight to build_record_modifier -> build_mortality_ledger. The age/sex
# gradient still flows through q_baseline + R_marital. Returns (year, id,
# q_death1, q_death2), keyed by synthetic id across the projection range.
build_forbes_mortality_ledger = function(rows, years = 2017L:2097L) {
  if (nrow(rows) == 0L) return(tibble::tibble())
  # weight is needed by renormalize_johnson_for_puf (inside build_record_modifier)
  # to make the marital layer mean-preserving on this population's composition.
  tu = rows %>%
    dplyr::select(id, weight, age1, age2, male1, male2, filing_status)
  pctile = tibble::tibble(id = rows$id, pctile1 = 100L, pctile2 = 100L)
  build_mortality_ledger(tax_units = tu, chetty_pctile = pctile, years = years)
}


# Build the source (weights lowered) and receiver (weights raised) pools for
# the weight calibration.
#
# Source pool = (largest source_n/2 by income) UNION (largest source_n/2 by
# net worth). The income half supplies wage/income content to offset the
# billionaires' income injection; the wealth half supplies the CAPITAL-income
# content (dividends, capital gains, interest) that a pure income-density
# pool lacks. A pure income-density source pool was structurally thin on
# dividends — billionaires inject more dividends than such a pool held — so
# the dividend constraint was infeasible. Pulling in the wealthiest records
# (where capital income concentrates) gives the LP something to shrink.
build_splice_pools = function(base_df,
                              source_n = 20000L,
                              receiver_n = 10000L,
                              billionaire_threshold = 1e9) {
  nw = forbes_net_worth(base_df)
  ok = coalesce_col(base_df, 'weight', 0) > 0 & !is.na(nw) & nw > 0 &
       nw < billionaire_threshold
  cand = base_df[ok, , drop = FALSE]
  if (nrow(cand) < 2L) {
    stop('Forbes splice calibration needs at least two candidate rows.')
  }
  cand$.__nw  = nw[ok]
  cand$.__inc = abs(forbes_fiscal_income(cand))

  # Source = top half by income  ∪  top half by net worth.
  half = min(as.integer(ceiling(source_n / 2)), nrow(cand))
  by_income  = cand$id[order(cand$.__inc, decreasing = TRUE)][seq_len(half)]
  by_wealth  = cand$id[order(cand$.__nw,  decreasing = TRUE)][seq_len(half)]
  source_ids = unique(c(by_income, by_wealth))

  # Receiver = wealth-density records not already sourced; weights raised to
  # add net worth back, holding the net-worth constraint when sources shrink.
  receiver_take = max(min(receiver_n, nrow(cand) - length(source_ids)), 0L)
  receiver = cand %>%
    dplyr::filter(!(id %in% source_ids)) %>%
    dplyr::mutate(.__wealth_density = .__nw / pmax(.__inc, 1),
                  .__score = forbes_rank01(.__nw) +
                             forbes_rank01(.__wealth_density)) %>%
    dplyr::arrange(dplyr::desc(.__score)) %>%
    utils::head(receiver_take)

  list(source_ids = source_ids, receiver_ids = receiver$id)
}


# Weight calibration: nudge existing-record weights so that appending the
# Forbes billionaires preserves count + the six income-category totals while
# net worth is allowed to grow by exactly the spliced wealth.
#
# Primary method is a RIDGE QP (osqp): minimize sum (delta_i / w_i)^2 — the
# squared relative weight change — so the adjustment SPREADS gently across
# many records instead of the sparse corner solution an L1 objective produces
# (which zeroed ~1,300 records and pinned a dozen at the cap). Every source
# weight is floored at `factor_lo` x its original (no record loses more than
# (1 - factor_lo) of its weight; no silent deletions); receivers are capped at
# `receiver_max_factor`x.
#
# If osqp is unavailable or fails to certify every constraint within tolerance
# (a single year can fail to converge), we WARN and fall back to the L1 LP
# (lpSolveAPI), which is exact but sparse — so the pipeline always gets a
# valid calibration rather than crashing or shipping an uncalibrated splice.
#
# Tolerances are fractions of a meaningful scale, not fixed absolute floors
# (those were sized for the unit-test fixture and are meaningless against real
# trillion-dollar aggregates): count + income to `rel_tol` of their own
# injection; net worth (target 0) to `wealth_rel_tol` of the spliced wealth.
solve_forbes_weight_calibration = function(base_df, forbes_rows,
                                           source_ids, receiver_ids,
                                           rel_tol = 0.005,
                                           wealth_rel_tol = 0.001,
                                           factor_lo = 0.1,
                                           receiver_max_factor = 10,
                                           receiver_ridge = 1.0,
                                           max_iter = 200000L,
                                           receiver_penalty = 1.1) {
  if (nrow(forbes_rows) == 0L) {
    return(list(weights = tibble::tibble(), constraints = tibble::tibble(),
                status = 'empty', method = 'none'))
  }

  source = base_df[match(source_ids, base_df$id), , drop = FALSE]
  receiver = base_df[match(receiver_ids, base_df$id), , drop = FALSE]
  source = source[!is.na(source$id), , drop = FALSE]
  receiver = receiver[!is.na(receiver$id), , drop = FALSE]
  n_s = nrow(source); n_r = nrow(receiver); n_vars = n_s + n_r
  if (n_vars == 0L) stop('Forbes splice calibration has no candidate rows.')
  ws = source$weight; wr = receiver$weight

  constraint_names = c('count', 'net_worth', names(forbes_income_categories))
  x_source   = list(count = rep(1, n_s), net_worth = forbes_net_worth(source))
  x_receiver = list(count = rep(1, n_r), net_worth = forbes_net_worth(receiver))
  x_forbes   = list(count = rep(1, nrow(forbes_rows)),
                    net_worth = forbes_net_worth(forbes_rows))
  for (cat in names(forbes_income_categories)) {
    x_source[[cat]]   = forbes_category_value(source, cat)
    x_receiver[[cat]] = forbes_category_value(receiver, cat)
    x_forbes[[cat]]   = forbes_category_value(forbes_rows, cat)
  }
  targets = sapply(constraint_names, function(nm)
    if (nm == 'net_worth') 0 else -sum(forbes_rows$weight * x_forbes[[nm]]))
  spliced_nw = sum(forbes_rows$weight * x_forbes[['net_worth']])
  abs_tol = sapply(constraint_names, function(nm)
    if (nm == 'net_worth') wealth_rel_tol * abs(spliced_nw)
    else max(abs(targets[[nm]]) * rel_tol, 1))

  # Assemble the standard result list from a (d_source, d_receiver) solution.
  # weight_factor relative slack on `ok`: the optimizer parks binding
  # constraints on the tolerance edge, and recomputing the gap carries FP
  # noise of order (magnitude x 1e-15) that swamps a fixed absolute slack.
  build_result = function(d_source, d_receiver, method) {
    weight_rows = dplyr::bind_rows(
      tibble::tibble(id = source$id,   old_weight = ws,
                     delta_weight = -d_source, role = 'source'),
      tibble::tibble(id = receiver$id, old_weight = wr,
                     delta_weight =  d_receiver, role = 'receiver')
    ) %>%
      dplyr::filter(abs(delta_weight) > 1e-10) %>%
      dplyr::mutate(new_weight = pmax(old_weight + delta_weight, 0),
                    weight_factor = new_weight / old_weight)
    constraint_rows = lapply(constraint_names, function(nm) {
      achieved = sum(-d_source * x_source[[nm]]) + sum(d_receiver * x_receiver[[nm]])
      tibble::tibble(constraint = nm, target_delta = targets[[nm]],
                     achieved_delta = achieved, gap = achieved - targets[[nm]],
                     tolerance = abs_tol[[nm]],
                     ok = abs(achieved - targets[[nm]]) <= abs_tol[[nm]] * (1 + 1e-6))
    }) %>% dplyr::bind_rows()
    list(weights = weight_rows, constraints = constraint_rows,
         status = 'solved', method = method)
  }

  # ---- Primary: ridge QP via osqp ------------------------------------------
  # Change of variables u = delta / w (relative change) + per-row tolerance
  # scaling condition the QP: bounds become O(1) and the ridge objective is a
  # clean diagonal. u_source in [0, 1-factor_lo], u_receiver in [0, maxf-1].
  ridge = tryCatch({
    if (!requireNamespace('osqp', quietly = TRUE) ||
        !requireNamespace('Matrix', quietly = TRUE))
      stop('osqp/Matrix not installed')
    A_agg = matrix(0, length(constraint_names), n_vars)
    for (i in seq_along(constraint_names)) {
      nm = constraint_names[i]
      A_agg[i, ] = c(-ws * x_source[[nm]], wr * x_receiver[[nm]])
    }
    A_agg = sweep(A_agg, 1, abs_tol, '/')
    A  = Matrix::rbind2(Matrix::Diagonal(n_vars),
                        Matrix::Matrix(A_agg, sparse = TRUE))
    lo = c(rep(0, n_vars),                       targets / abs_tol - 1)
    up = c(rep(1 - factor_lo, n_s),
           rep(receiver_max_factor - 1, n_r),    targets / abs_tol + 1)
    P  = Matrix::Diagonal(n_vars,
                          x = 2 * c(rep(1, n_s), rep(receiver_ridge, n_r)))
    m = osqp::osqp(P, rep(0, n_vars), A, lo, up,
                   pars = osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-7,
                                             eps_rel = 1e-7, max_iter = max_iter,
                                             polish = TRUE))
    s = m$Solve(); uu = s$x; uu[uu < 0] = 0
    list(d_source = uu[seq_len(n_s)] * ws,
         d_receiver = uu[n_s + seq_len(n_r)] * wr,
         osqp_status = s$info$status)
  }, error = function(e) {
    warning('Forbes ridge calibration errored: ', conditionMessage(e)); NULL
  })

  if (!is.null(ridge)) {
    res = build_result(ridge$d_source, ridge$d_receiver, 'ridge')
    if (all(res$constraints$ok)) return(res)
    warning(sprintf(
      'Forbes ridge calibration did not certify all constraints (osqp: %s); falling back to L1.',
      ridge$osqp_status))
  }

  # ---- Fallback: L1 LP via lpSolveAPI (exact, sparse) ----------------------
  if (!requireNamespace('lpSolveAPI', quietly = TRUE))
    stop('lpSolveAPI required for the L1 fallback calibration.')
  lprw = lpSolveAPI::make.lp(0, n_vars)
  lpSolveAPI::set.objfn(lprw, c(rep(1, n_s), rep(receiver_penalty, n_r)))
  # L1 honors the same source floor + receiver cap as the ridge path.
  upper = c(ws * (1 - factor_lo), wr * (receiver_max_factor - 1))
  lpSolveAPI::set.bounds(lprw, lower = rep(0, n_vars), upper = upper,
                         columns = seq_len(n_vars))
  for (nm in constraint_names) {
    coef = c(-x_source[[nm]], x_receiver[[nm]])
    lpSolveAPI::add.constraint(lprw, coef, '<=', targets[[nm]] + abs_tol[[nm]])
    lpSolveAPI::add.constraint(lprw, coef, '>=', targets[[nm]] - abs_tol[[nm]])
  }
  solution = solve(lprw)
  if (solution != 0)
    stop('Forbes splice L1 fallback failed with lpSolve status ', solution)
  sol = lpSolveAPI::get.variables(lprw)
  build_result(sol[seq_len(n_s)], sol[n_s + seq_len(n_r)], 'l1_fallback')
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

    # Per-year income-share pool (within-category allocation) and deduction
    # model, both estimated from this year's materialized PUF top.
    share_pool = choose_forbes_share_pool(
      puf_y, n_records = max(500L, nrow(f_y)),
      billionaire_threshold = billionaire_threshold)
    dedn_model = fit_forbes_deduction_model(puf_y)

    rows_y = build_forbes_rows_for_year(
      puf_y, f_y, params, share_pool, dedn_model,
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

  all_rows = dplyr::bind_rows(row_list)

  # Synthetic ids must be unique among themselves and disjoint from the base
  # PUF ids — Tax-Simulator joins on id, so a collision would silently merge
  # or overwrite records downstream. make_forbes_id's fallback branch makes
  # collisions improbable but not impossible; assert rather than hope.
  if (nrow(all_rows) > 0L) {
    dup_within = unique(all_rows$id[duplicated(all_rows$id)])
    if (length(dup_within) > 0L) {
      stop('Forbes splice produced duplicate synthetic ids: ',
           paste(utils::head(dup_within, 5L), collapse = ', '))
    }
    clash = intersect(all_rows$id, base$id)
    if (length(clash) > 0L) {
      stop('Forbes synthetic ids collide with base PUF ids: ',
           paste(utils::head(clash, 5L), collapse = ', '))
    }
  }

  # Per-record synthetic mortality, pinned to national p100 (billionaires are
  # unambiguously the top). Keyed by synthetic id across the full projection
  # range; consumed by apply_forbes_splice_to_materialized. build_*_ledger is
  # available because main.R sources mortality_ledger.R before this runs.
  forbes_mortality = if (nrow(all_rows) > 0L && exists('build_mortality_ledger')) {
    build_forbes_mortality_ledger(all_rows)
  } else tibble::tibble()

  list(
    rows = all_rows,
    weight_adjustments = dplyr::bind_rows(weight_list),
    constraints = dplyr::bind_rows(constraint_list),
    diagnostics = dplyr::bind_rows(diag_list),
    mortality = forbes_mortality,
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

  # Forbes data ends at max(years) (2025). Each year 2022-2025 carries its
  # own per-year weight calibration (solved independently in
  # build_forbes_splice); 2026+ intentionally freezes at the 2025 solution —
  # there is no Forbes list beyond 2025 to recalibrate against — and ages the
  # 2025 synthetic rows forward via project_forbes_rows.
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

  # Mortality: look up the synthetic rows' OWN q_death (pinned-p100 ledger,
  # keyed by synthetic id x target_year) rather than copying a donor's. v1
  # copied the donor's q_death — driven by the donor's age, not the
  # billionaire's. The ledger covers freeze years (2026+) too, keyed by the
  # 2025 ids that are reused there.
  ml = forbes_splice$mortality
  if (!is.null(ml) && nrow(ml) > 0L && 'q_death1' %in% names(rows)) {
    ml_y = ml[ml$year == target_year, , drop = FALSE]
    mi = match(rows$id, ml_y$id)
    rows$q_death1 = ml_y$q_death1[mi]
    if ('q_death2' %in% names(rows)) rows$q_death2 = ml_y$q_death2[mi]
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

  # Wide fixture: one row per rank_group, a column per income category.
  # Shares already sum to 1.0 (so they equal what read_bsyz_params would
  # return after row-normalization); this fixture is built inline and does
  # not pass through read_bsyz_params.
  params_fx = tibble(
    rank_group              = c('top100', 'next300'),
    rank_min                = c(1L, 101L),
    rank_max                = c(100L, 400L),
    fiscal_income_to_wealth = c(0.02, 0.03),
    capital_gains           = c(0.6, 0.6),
    dividends               = c(0.2, 0.2),
    interest                = c(0.1, 0.1),
    business                = c(-0.05, -0.05),
    rent_estate_other       = c(0.1, 0.1),
    wages_pensions          = c(0.05, 0.05)
  )
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
  # v2 assembler reads/writes these (split-sum pairs, demographics, deduction
  # inputs); present them on the template fixture, zeroed.
  for (v in c('age1', 'male1', 'age2', 'male2', 'filing_status', 'n_dep',
              'dep_age1', 'dep_age2', 'dep_age3', 'filer', 'dep_status',
              'blind1', 'blind2', 'n_dep_ctc', 'n_dep_eitc',
              'wages1', 'wages2', 'ot', 'ot1', 'ot2', 'tips', 'tips1', 'tips2',
              'sole_prop1', 'sole_prop2', 'part_se', 'part_se1', 'part_se2',
              'farm1', 'farm2', 'txbl_pens_dist', 'ui', 'gross_ss',
              'state_ref', 'alimony', 'txbl_ira_dist',
              'char_cash', 'char_noncash', 'salt_prop')) {
    base_fx[[v]] = 0
  }

  # Source pool = top half by income (id 1, highest fiscal income) UNION top
  # half by net worth (id 2, highest value.equities). Receiver = wealthiest
  # remaining (id 3 or 4). source_n = 2 -> one record per half.
  pools_fx = build_splice_pools(base_fx, source_n = 2, receiver_n = 1)
  stopifnot(setequal(pools_fx$source_ids, c(1L, 2L)),
            pools_fx$receiver_ids %in% c(3L, 4L))
  cat('  [PASS] source pool spans top-income AND top-wealth records\n')

  # v2 enriched input: /info demographics + /assets public-equity value +
  # selfMade.type. Self-made tech founder, $1B net worth, $0.6B public.
  forbes_input_fx = tibble(
    year = 2022L, rank = 1L, name = 'Fixture Billionaire',
    net_worth = 1e9, source_category = 'technology',
    birth_date = '1960-01-01', gender = 'm', marital_status = 'Married',
    children = 2L, self_made_type = 'self-made', industry = 'technology',
    public_equity_value = 6e8
  )
  share_pool_fx = choose_forbes_share_pool(base_fx, n_records = 4)
  dedn_fx = fit_forbes_deduction_model(base_fx)
  forbes_rows_fx = build_forbes_rows_for_year(
    base_fx, forbes_input_fx, params_fx, share_pool_fx, dedn_fx)
  stopifnot(nrow(forbes_rows_fx) == 1L,
            forbes_rows_fx$forbes_flag == 1L,
            is.na(forbes_rows_fx$forbes_donor_id),                # v2: no donor
            abs(forbes_net_worth(forbes_rows_fx) - 1e9) < 1,
            abs(forbes_fiscal_income(forbes_rows_fx) - 2e7) < 1,
            abs(forbes_category_value(forbes_rows_fx, 'business') + 1e6) < 1,
            abs(forbes_rows_fx$E00100 - 2e7) < 1)
  cat('  [PASS] assembled row hits wealth and income targets (no donor)\n')

  # Wealth composition: public ticker'd -> value.equities; residual ->
  # value.pass_throughs (tech, not real estate); sums exactly to net worth.
  stopifnot(abs(forbes_rows_fx$`value.equities`     - 6e8) < 1,
            abs(forbes_rows_fx$`value.pass_throughs` - 4e8) < 1,
            abs(forbes_rows_fx$`value.re_fund`)       < 1)
  cat('  [PASS] wealth composition splits public vs private residual\n')

  # Founder basis: self-made -> ~0 basis on the appreciating founding assets.
  stopifnot(forbes_rows_fx$`basis.equities`      == 0,
            forbes_rows_fx$`basis.pass_throughs`  == 0)
  # Inherited counterpart -> stepped-up basis ~ value.
  inh_input = forbes_input_fx; inh_input$self_made_type = 'inherited'
  inh_row = build_forbes_rows_for_year(
    base_fx, inh_input, params_fx, share_pool_fx, dedn_fx)
  stopifnot(abs(inh_row$`basis.equities`     - inh_row$`value.equities`)     < 1,
            abs(inh_row$`basis.pass_throughs` - inh_row$`value.pass_throughs`) < 1)
  cat('  [PASS] founder basis ~0; inherited basis ~ value (stepped-up)\n')

  # Demographics: birthDate->age, gender->male1, Married->MFJ + assumed
  # spouse (opposite sex, same age), older principal -> no dependents.
  stopifnot(forbes_rows_fx$age1 == 2022L - 1960L,
            forbes_rows_fx$male1 == 1L,
            forbes_rows_fx$filing_status == 2L,
            forbes_rows_fx$age2 == forbes_rows_fx$age1,
            forbes_rows_fx$male2 == 0L,
            forbes_rows_fx$n_dep == 0L)
  cat('  [PASS] demographics map from /info (age, sex, MFJ, spouse)\n')

  # Split-sum identities must hold to MACHINE PRECISION (Tax-Simulator depends
  # on these). v2 sets all to the primary filer.
  for (p in forbes_split_pairs) {
    if (all(p %in% names(forbes_rows_fx))) {
      lhs = forbes_rows_fx[[p[2]]] + forbes_rows_fx[[p[3]]]
      stopifnot(identical(lhs, forbes_rows_fx[[p[1]]]),
                forbes_rows_fx[[p[3]]] == 0)
    }
  }
  cat('  [PASS] split-sum identities exact on synthetic row (all-to-primary)\n')

  # Negative business target (BSYZ top-100 business share is negative —
  # billionaires report net business losses). On a zeroed assembler row,
  # set_category_on_row rebuilds the category from the loss/179 components and
  # must land exactly on the negative target.
  neg_row  = base_fx[3, , drop = FALSE]; neg_row[, names(neg_row)] = 0
  neg_out  = set_category_on_row(neg_row, -5e5, 'business', base_fx)
  neg_val  = forbes_category_value(neg_out, 'business')
  pos_part = neg_out$sole_prop + neg_out$farm +
             neg_out$scorp_active + neg_out$scorp_passive +
             neg_out$part_active + neg_out$part_passive
  stopifnot(abs(neg_val - (-5e5)) < 1, neg_val < 0, pos_part == 0)
  cat('  [PASS] negative business target builds net-negative composition\n')

  # Deduction model: charity ~ wealth fit on a synthetic PUF top, capped.
  set.seed(1)
  n_d = 400L
  nw_d = exp(rnorm(n_d, log(5e7), 1))
  char_d = pmax(0, 0.01 * nw_d * exp(rnorm(n_d, 0, 0.3)))
  dedn_base = tibble(
    weight = 1, char_cash = 0.6 * char_d, char_noncash = 0.4 * char_d,
    salt_prop = 1e4, `value.equities` = nw_d
  )
  for (v in setdiff(wealth_output_vars, 'value.equities')) dedn_base[[v]] = 0
  dm = fit_forbes_deduction_model(dedn_base, top_q = 0.9, cap_frac = 0.05)
  ded_row = set_forbes_deductions(forbes_rows_fx, dm, net_worth = 1e9)
  char_tot = ded_row$char_cash + ded_row$char_noncash
  stopifnot(!is.na(dm$b1), char_tot > 0, char_tot <= 0.05 * 1e9 + 1,
            abs(ded_row$char_cash / char_tot - 0.6) < 0.05)
  cat('  [PASS] deduction model fits charity ~ wealth and caps the tail\n')

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
  # v2 mortality: looked up from the synthetic ledger by (year, synthetic id),
  # NOT copied from a donor. Fixture ledger pins q_death for the synthetic id.
  splice_fx = list(
    rows = forbes_rows_fx,
    weight_adjustments = tibble(
      year = 2022L, id = 1L, old_weight = 10, delta_weight = -5,
      role = 'source', new_weight = 5, weight_factor = 0.5
    ),
    mortality = tibble(
      year = 2022L, id = forbes_rows_fx$id,
      q_death1 = 0.05, q_death2 = 0.04
    ),
    years = 2022L:2025L
  )
  applied_fx = apply_forbes_splice_to_materialized(
    base_fx, 2022L, splice_fx)
  appended_fx = applied_fx %>% dplyr::filter(forbes_flag == 1L)
  stopifnot(nrow(applied_fx) == nrow(base_fx) + 1L,
            applied_fx$weight[applied_fx$id == 1L] == 5,
            nrow(appended_fx) == 1L,
            appended_fx$q_death1 == 0.05,        # own pinned-p100 mortality
            appended_fx$q_death2 == 0.04)
  cat('  [PASS] materialized output applies weights, appends rows, sets q_death\n')

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
