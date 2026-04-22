#--------------------------------------
# test_wealth_X_ablation_validation.R
#
# Extended validation of the X-ablation
# results, oriented to the downstream
# uses of the Tax-Simulator integration:
#
#   (i)   capital-gains policy (tax
#         unrealized, change realization
#         rate) — need the joint of
#         (realized income × unrealized
#         KG by asset type) to be right
#   (ii)  wealth-tax simulation — need
#         top tail of NW correct
#   (iii) economic-income distribution —
#         NW by income decile must match
#   (iv)  joint ownership × filing —
#         ownership rates and holdings
#         must match by filing status ×
#         age × income decile
#
# Produces, per spec (small/medium/large)
# vs SCF truth:
#
#   (A) top wealth shares: NW share of
#       top 10/1/0.1%, Gini coefficient
#   (B) 10×10 joint (income dec × NW dec)
#       mass + relative-error heatmaps
#   (C) ownership rates by income decile
#       for each of 13 asset categories
#   (D) unrealized-KG distribution by
#       realized-income decile (mean, P90)
#   (E) NW quantile thresholds (P50/P90/
#       P99) by income decile
#   (F) rank-correlation of NW with each
#       composition variable — diagnoses
#       where X contains wealth signal
#   (G) wealth-tax policy counters: how
#       many filers / aggregate NW above
#       thresholds ($1M, $10M, $50M, $1B)
#
# Inputs:
#   resources/cache/wealth_ablation.rds
#     (from postproc; has puf_by_spec)
#   resources/cache/scf_tax_units.rds
#     (for SCF truth)
#--------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(Hmisc)
})

plot_dir = 'plots/wealth_ablation'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

#---------------------------------------------------------------------------
# Load cached postproc output + SCF tax units.
#---------------------------------------------------------------------------

pp = read_rds('resources/cache/wealth_ablation.rds')
scf_raw = read_rds('resources/cache/scf_tax_units.rds')

spec_names = pp$spec_names
puf_by_spec = pp$puf_by_spec
stopifnot(!is.null(spec_names), !is.null(puf_by_spec))

wealth_asset_vars = c(
  'cash', 'equities', 'bonds', 'retirement', 'life_ins', 'annuities',
  'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
  're_fund', 'other_nonfin'
)
wealth_debt_vars = c(
  'primary_mortgage', 'other_mortgage', 'credit_lines',
  'credit_cards', 'installment_debt', 'other_debt'
)
wealth_kg_vars = c(
  'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other'
)
wealth_y_vars = c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)

# Build SCF-native frame with net_worth etc. (stage1 income column = SCFP
# INCOME, which we also use as the rank basis on PUF).
scf_native = scf_raw %>%
  mutate(
    total_assets = rowSums(across(all_of(wealth_asset_vars))),
    total_debts  = rowSums(across(all_of(wealth_debt_vars))),
    total_kg     = rowSums(across(all_of(wealth_kg_vars))),
    net_worth    = total_assets - total_debts
  )

# Helper: compute within-dataset decile (1..10) on a positive-income basis.
# Rows with non-positive income land in decile 0.
wtd_decile = function(x, w) {
  pos = x > 0
  if (!any(pos)) return(rep(0L, length(x)))
  brks = wtd.quantile(x[pos], w[pos], probs = seq(0, 1, 0.1))
  brks = sort(brks)
  bin = findInterval(x, brks, all.inside = TRUE)
  ifelse(pos, bin, 0L)
}

# NW decile — net worth can be negative, so rank the full (signed) series.
wtd_nw_decile = function(x, w) {
  brks = sort(wtd.quantile(x, w, probs = seq(0, 1, 0.1)))
  findInterval(x, brks, all.inside = TRUE)
}

# Decile-assign on SCF first (truth); same approach applied per-spec below.
scf_native = scf_native %>%
  mutate(inc_dec = wtd_decile   (income,    weight),
         nw_dec  = wtd_nw_decile(net_worth, weight))

#---------------------------------------------------------------------------
# (A) Top wealth shares + Gini
#
# Share of aggregate NW held by top-X% of *weighted* tax units, ranked by
# NW within each dataset / spec. Also Gini coefficient.
#---------------------------------------------------------------------------

weighted_top_share = function(x, w, top_frac) {
  o = order(-x)
  cw = cumsum(w[o]) / sum(w)
  keep = cw <= top_frac
  # include the bracket-crossing row too, so shares are continuous
  keep[which.max(cw > top_frac)] = TRUE
  sum(x[o][keep] * w[o][keep]) / sum(x * w)
}

weighted_gini = function(x, w) {
  # Standard weighted Gini via Lorenz integral. Works on nonnegative and
  # signed x — for NW with negatives, the "Gini" is interpretable but
  # exceeds 1 when negative values are present; report as-is.
  o = order(x)
  x = x[o]; w = w[o]
  cw = cumsum(w) / sum(w)
  cx = cumsum(x * w) / sum(x * w)
  # Trapezoidal integration of Lorenz: G = 1 - 2 * integral(L dp)
  1 - 2 * sum((cw - c(0, head(cw, -1))) * (cx + c(0, head(cx, -1))) / 2)
}

top_shares = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]]
  tibble(
    source     = nm,
    top10_share  = weighted_top_share(d$net_worth, d$weight, 0.10),
    top1_share   = weighted_top_share(d$net_worth, d$weight, 0.01),
    top01_share  = weighted_top_share(d$net_worth, d$weight, 0.001),
    bottom50_share = weighted_top_share(-d$net_worth, d$weight, 0.50),  # flip sign to grab bottom 50 via "top 50 of -x"
    gini       = weighted_gini(d$net_worth, d$weight),
    total_nw_tn = sum(d$net_worth * d$weight) / 1e12
  )
}) %>%
  bind_rows(tibble(
    source = 'SCF (truth)',
    top10_share  = weighted_top_share(scf_native$net_worth, scf_native$weight, 0.10),
    top1_share   = weighted_top_share(scf_native$net_worth, scf_native$weight, 0.01),
    top01_share  = weighted_top_share(scf_native$net_worth, scf_native$weight, 0.001),
    bottom50_share = weighted_top_share(-scf_native$net_worth, scf_native$weight, 0.50),
    gini       = weighted_gini(scf_native$net_worth, scf_native$weight),
    total_nw_tn = sum(scf_native$net_worth * scf_native$weight) / 1e12
  ))

cat('\n==========================================================\n')
cat('  (A) Top wealth shares + Gini\n')
cat('==========================================================\n\n')
print(top_shares, n = 100)

write_csv(top_shares, file.path(plot_dir, 'top_shares.csv'))

#---------------------------------------------------------------------------
# (G) Wealth-tax policy counters
#
# Thresholds drawn from recent proposals: $1M/$10M/$50M (Warren-style) and
# $1B. Report fraction of filers and aggregate NW above each.
#---------------------------------------------------------------------------

thresholds = c(1e6, 1e7, 5e7, 1e9)
names(thresholds) = c('1M', '10M', '50M', '1B')

wealth_tax_base = function(x, w, thr) {
  over = x > thr
  tibble(
    share_filers = sum(w[over]) / sum(w),
    n_filers_m   = sum(w[over]) / 1e6,
    agg_nw_tn    = sum(x[over] * w[over]) / 1e12,
    taxable_base_tn = sum((x[over] - thr) * w[over]) / 1e12
  )
}

wt_tbl = map_dfr(names(thresholds), function(thr_nm) {
  thr = thresholds[thr_nm]
  per_spec = map_dfr(spec_names, function(nm) {
    d = puf_by_spec[[nm]]
    wealth_tax_base(d$net_worth, d$weight, thr) %>%
      mutate(source = nm, threshold = thr_nm)
  })
  truth = wealth_tax_base(scf_native$net_worth, scf_native$weight, thr) %>%
    mutate(source = 'SCF (truth)', threshold = thr_nm)
  bind_rows(per_spec, truth)
})

cat('\n==========================================================\n')
cat('  (G) Wealth-tax policy counters\n')
cat('==========================================================\n\n')
print(wt_tbl %>% select(threshold, source, share_filers, n_filers_m,
                        agg_nw_tn, taxable_base_tn) %>%
        arrange(threshold, source),
      n = 100)

write_csv(wt_tbl, file.path(plot_dir, 'wealth_tax_counters.csv'))

#---------------------------------------------------------------------------
# (B) 10 × 10 joint (income decile × NW decile) mass heatmap
#
# For each spec, compute % of weighted tax units in each (inc_dec, nw_dec)
# cell. Compare to SCF-native mass. A well-behaved imputation has joint
# mass matching SCF up to definitional noise. Report relative error per cell.
#---------------------------------------------------------------------------

joint_mass = function(d) {
  d %>%
    count(inc_dec, nw_dec, wt = weight, name = 'w') %>%
    mutate(share = w / sum(w))
}

scf_joint = joint_mass(scf_native) %>% rename(scf_share = share) %>% select(-w)

puf_joint = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]] %>%
    mutate(inc_dec = wtd_decile(income, weight),
           nw_dec  = wtd_nw_decile(net_worth, weight))
  joint_mass(d) %>% mutate(spec = nm)
}) %>%
  rename(puf_share = share) %>%
  select(-w)

joint_cmp = puf_joint %>%
  left_join(scf_joint, by = c('inc_dec', 'nw_dec')) %>%
  mutate(scf_share = replace_na(scf_share, 0),
         rel_err = (puf_share - scf_share) / pmax(scf_share, 1e-6))

write_csv(joint_cmp, file.path(plot_dir, 'joint_income_nw_mass.csv'))

p_joint = joint_cmp %>%
  ggplot(aes(factor(inc_dec), factor(nw_dec),
             fill = pmin(pmax(100 * rel_err, -200), 200))) +
  geom_tile(color = 'white') +
  geom_text(aes(label = ifelse(abs(rel_err) > 0.1 & scf_share > 0.001,
                               sprintf('%+.0f%%', 100 * rel_err), '')),
            size = 2.3) +
  scale_fill_gradient2(low = '#2166ac', mid = 'white', high = '#b2182b',
                       midpoint = 0, name = 'PUF − SCF\n(% of SCF mass)',
                       limits = c(-200, 200), oob = scales::squish) +
  facet_wrap(~ spec, ncol = 3) +
  labs(title = 'Joint (income × NW) mass: PUF vs SCF, per spec',
       subtitle = 'Within-dataset deciles. Green near zero = joint dist is well-recovered.',
       x = 'Income decile', y = 'Net worth decile') +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, '07_joint_income_nw_mass.png'),
       p_joint, width = 13, height = 5, dpi = 140)

#---------------------------------------------------------------------------
# (C) Ownership rates by income decile, per asset category
#---------------------------------------------------------------------------

ownership_by_decile = function(d, asset_vars, decile_col = 'inc_dec') {
  d %>%
    pivot_longer(all_of(asset_vars), names_to = 'asset', values_to = 'v') %>%
    group_by(.data[[decile_col]], asset) %>%
    summarise(share_own = sum(weight[v > 0]) / sum(weight),
              .groups = 'drop') %>%
    rename(decile = !!decile_col)
}

own_scf = ownership_by_decile(scf_native, wealth_asset_vars) %>%
  mutate(source = 'SCF (truth)')

own_puf = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]] %>%
    mutate(inc_dec = wtd_decile(income, weight))
  ownership_by_decile(d, wealth_asset_vars) %>% mutate(source = nm)
})

own_df = bind_rows(own_puf, own_scf) %>%
  mutate(asset = factor(asset, levels = wealth_asset_vars))

write_csv(own_df, file.path(plot_dir, 'ownership_by_decile.csv'))

p_own = own_df %>%
  filter(decile >= 1) %>%
  ggplot(aes(decile, share_own, color = source, linetype = source)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c(small = '#4daf4a', medium = '#377eb8', large_binv2 = '#984ea3',
                                large = '#e41a1c', `SCF (truth)` = 'black')) +
  scale_linetype_manual(values = c(small = 'solid', medium = 'solid', large_binv2 = 'dotdash',
                                    large = 'solid',
                                    `SCF (truth)` = 'dashed')) +
  facet_wrap(~ asset, ncol = 4, scales = 'free_y') +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  labs(title = 'Asset-ownership rate by income decile',
       subtitle = 'Share with holdings > 0. SCF (dashed) = truth.',
       x = 'Income decile', y = 'Share with asset > 0') +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, '08_ownership_by_decile.png'),
       p_own, width = 13, height = 9, dpi = 140)

#---------------------------------------------------------------------------
# (D) Unrealized KG by realized-income decile
#
# For cap-gains realization/unrealized-tax policy. Report mean and P90 of
# each kg_* category within each realized-income decile.
#---------------------------------------------------------------------------

kg_by_decile = function(d, decile_col = 'inc_dec') {
  d %>%
    pivot_longer(all_of(wealth_kg_vars), names_to = 'kg_cat', values_to = 'v') %>%
    group_by(.data[[decile_col]], kg_cat) %>%
    summarise(
      mean_kg = weighted.mean(v, weight),
      p90_kg  = wtd.quantile (v, weight, 0.90),
      total_kg_bn = sum(v * weight) / 1e9,
      .groups = 'drop'
    ) %>%
    rename(decile = !!decile_col)
}

kg_scf = kg_by_decile(scf_native) %>% mutate(source = 'SCF (truth)')
kg_puf = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]] %>%
    mutate(inc_dec = wtd_decile(income, weight))
  kg_by_decile(d) %>% mutate(source = nm)
})
kg_df = bind_rows(kg_puf, kg_scf) %>%
  mutate(kg_cat = factor(kg_cat, levels = wealth_kg_vars))

write_csv(kg_df, file.path(plot_dir, 'unrealized_kg_by_decile.csv'))

p_kg = kg_df %>%
  filter(decile >= 1) %>%
  ggplot(aes(decile, total_kg_bn, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c(small = '#4daf4a', medium = '#377eb8', large_binv2 = '#984ea3',
                               large = '#e41a1c', `SCF (truth)` = 'black')) +
  facet_wrap(~ kg_cat, ncol = 2, scales = 'free_y') +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  labs(title = 'Unrealized capital gains by realized-income decile',
       subtitle = 'Aggregate $B of unrealized KG per decile, by imputation spec vs SCF.',
       x = 'Income decile', y = 'Unrealized KG ($B)') +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, '09_unrealized_kg_by_decile.png'),
       p_kg, width = 11, height = 8, dpi = 140)

#---------------------------------------------------------------------------
# (E) NW quantile thresholds by income decile
#---------------------------------------------------------------------------

nw_quantiles_by_inc = function(d, decile_col = 'inc_dec') {
  d %>%
    group_by(.data[[decile_col]]) %>%
    summarise(
      p50_NW = wtd.quantile(net_worth, weight, 0.5),
      p90_NW = wtd.quantile(net_worth, weight, 0.9),
      p99_NW = wtd.quantile(net_worth, weight, 0.99),
      .groups = 'drop'
    ) %>% rename(decile = !!decile_col)
}

nw_q_scf = nw_quantiles_by_inc(scf_native) %>% mutate(source = 'SCF (truth)')
nw_q_puf = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]] %>%
    mutate(inc_dec = wtd_decile(income, weight))
  nw_quantiles_by_inc(d) %>% mutate(source = nm)
})

nw_q = bind_rows(nw_q_puf, nw_q_scf) %>%
  pivot_longer(starts_with('p'), names_to = 'quantile', values_to = 'nw')

write_csv(nw_q, file.path(plot_dir, 'nw_quantiles_by_inc_decile.csv'))

p_nwq = nw_q %>%
  filter(decile >= 1) %>%
  ggplot(aes(decile, nw / 1000, color = source, linetype = source)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 10, base = 10),
                     breaks = c(-100, -10, 0, 10, 100, 1000, 10000, 100000),
                     labels = scales::label_comma()) +
  scale_color_manual(values = c(small = '#4daf4a', medium = '#377eb8', large_binv2 = '#984ea3',
                                large = '#e41a1c', `SCF (truth)` = 'black')) +
  scale_linetype_manual(values = c(small = 'solid', medium = 'solid', large_binv2 = 'dotdash',
                                    large = 'solid',
                                    `SCF (truth)` = 'dashed')) +
  facet_wrap(~ quantile, ncol = 3) +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  labs(title = 'NW quantile thresholds by income decile',
       subtitle = 'P50/P90/P99 of net worth within each income decile. Pseudo-log y.',
       x = 'Income decile',
       y = 'Net worth threshold ($ thousands, pseudo-log)') +
  theme_minimal(base_size = 10)

ggsave(file.path(plot_dir, '10_nw_quantiles_by_inc_decile.png'),
       p_nwq, width = 13, height = 5, dpi = 140)

#---------------------------------------------------------------------------
# (F) Rank-correlation of NW with each demographic + composition signal
#
# Diagnoses how much of the "NW predictable from X" signal each spec picks
# up. The DRF imputation cannot exceed SCF-native correlations in
# expectation — so spec-vs-SCF deltas bound the attainable match.
#---------------------------------------------------------------------------

spearman_w = function(x, y, w) {
  # Weighted Spearman: correlate weighted ranks
  rx = rank(x); ry = rank(y)
  cov.wt(cbind(rx, ry), wt = w, cor = TRUE)$cor[1, 2]
}

rho_scf = tibble(
  source  = 'SCF (truth)',
  rho_nw_income   = spearman_w(scf_native$net_worth, scf_native$income,
                               scf_native$weight),
  rho_nw_wages    = spearman_w(scf_native$net_worth, scf_native$wages_scf,
                               scf_native$weight),
  rho_nw_business = spearman_w(scf_native$net_worth, scf_native$business_scf,
                               scf_native$weight),
  rho_nw_intdiv   = spearman_w(scf_native$net_worth, scf_native$int_div_scf,
                               scf_native$weight),
  rho_nw_capgains = spearman_w(scf_native$net_worth, scf_native$capital_gains_scf,
                               scf_native$weight)
)

# For PUF: NW is imputed; other vars come from puf_context. Need composition
# from the underlying per-spec frame — but puf_by_spec rows retain income &
# pctile-of-business/int_div/capgains etc. directly? Actually the postproc
# saves id + weight + a few demographics + Y. We'd need to merge on tax_units
# composition columns — but those aren't in puf_by_spec. Use the PUF income
# + has-flag versions as proxies (they are saved). Correlate NW with income
# and has_* flags.
rho_puf = map_dfr(spec_names, function(nm) {
  d = puf_by_spec[[nm]]
  tibble(
    source = nm,
    rho_nw_income   = spearman_w(d$net_worth, d$income, d$weight),
    rho_nw_wages    = NA_real_,  # not saved per-spec
    rho_nw_business = spearman_w(d$net_worth, d$has_business, d$weight),
    rho_nw_intdiv   = NA_real_,  # has_int_div saved
    rho_nw_capgains = spearman_w(d$net_worth, d$has_capital_gains, d$weight)
  )
})

rho_df = bind_rows(rho_puf, rho_scf)
cat('\n==========================================================\n')
cat('  (F) Spearman rank-correlation of NW with covariates\n')
cat('==========================================================\n\n')
print(rho_df)
write_csv(rho_df, file.path(plot_dir, 'rank_corr_nw.csv'))

#---------------------------------------------------------------------------
# (H, I, J, K) — per-asset concentration & composition at top-X% income.
# Requested as a per-asset extension of the top-wealth metrics:
#
#   (H) Total $ by asset type, SCF vs imputed PUF
#       — same data as agg_long but displayed per-asset.
#   (I) Share of each asset $ held by top 10 / top 1 / top 0.1 percent of
#       INCOME. Concentration-by-income.
#   (J) Absolute $ of each asset in top 10 / top 1 / top 0.1 income.
#   (K) Composition — within top 10 / 1 / 0.1 income, how total assets
#       split across the 13 asset categories (shares sum to 1).
#
# "Top X% of income" uses within-dataset weighted quantile cutoff on
# `income`, so each spec and SCF are ranked on their own income variable.
#---------------------------------------------------------------------------

top_fracs = c(top10 = 0.10, top1 = 0.01, top01 = 0.001)

per_asset_top_income = function(d, src_label) {
  map_dfr(names(top_fracs), function(tn) {
    tf = top_fracs[[tn]]
    thresh = Hmisc::wtd.quantile(d$income, d$weight, 1 - tf)
    in_top = d$income > thresh
    total_assets_in_top = sum(d$total_assets[in_top] * d$weight[in_top])
    map_dfr(wealth_asset_vars, function(v) {
      dollars_all = sum(d[[v]]          * d$weight)         / 1e9
      dollars_top = sum(d[[v]][in_top]  * d$weight[in_top]) / 1e9
      tibble(
        source = src_label,
        top    = tn,
        asset  = v,
        dollars_total_bn = dollars_all,
        dollars_top_bn   = dollars_top,
        share_of_asset_in_top = dollars_top / dollars_all,
        composition_in_top    = (dollars_top * 1e9) / total_assets_in_top
      )
    })
  })
}

# PUF specs
top_inc_puf = map_dfr(spec_names, function(nm) {
  per_asset_top_income(puf_by_spec[[nm]], nm)
})
# SCF truth
top_inc_scf = per_asset_top_income(scf_native, 'SCF (truth)')
top_inc_all = bind_rows(top_inc_puf, top_inc_scf) %>%
  mutate(asset = factor(asset, levels = wealth_asset_vars),
         top   = factor(top,   levels = names(top_fracs)))

write_csv(top_inc_all, file.path(plot_dir, 'per_asset_top_income.csv'))

cat('\n==========================================================\n')
cat('  (H) Total $ by asset type — PUF specs vs SCF ($B)\n')
cat('==========================================================\n\n')
print(top_inc_all %>% filter(top == 'top10') %>%
        select(asset, source, dollars_total_bn) %>%
        pivot_wider(names_from = source, values_from = dollars_total_bn),
      n = length(wealth_asset_vars))

cat('\n==========================================================\n')
cat('  (I) Share of each asset $ held by top-X% of income\n')
cat('==========================================================\n\n')
for (tn in names(top_fracs)) {
  cat(sprintf('\n-- %s of income (income > P%.1f) --\n',
              tn, 100 * (1 - top_fracs[[tn]])))
  print(top_inc_all %>% filter(top == tn) %>%
          select(asset, source, share_of_asset_in_top) %>%
          pivot_wider(names_from = source, values_from = share_of_asset_in_top) %>%
          mutate(across(-asset, ~ sprintf('%.0f%%', 100 * .x))),
        n = length(wealth_asset_vars))
}

cat('\n==========================================================\n')
cat('  (J) Absolute $ of each asset in top-X% income ($B)\n')
cat('==========================================================\n\n')
for (tn in names(top_fracs)) {
  cat(sprintf('\n-- %s of income --\n', tn))
  print(top_inc_all %>% filter(top == tn) %>%
          select(asset, source, dollars_top_bn) %>%
          pivot_wider(names_from = source, values_from = dollars_top_bn),
        n = length(wealth_asset_vars))
}

cat('\n==========================================================\n')
cat('  (K) Asset composition within top-X% income\n')
cat('      (each column sums to ~100% across the 13 asset rows)\n')
cat('==========================================================\n\n')
for (tn in names(top_fracs)) {
  cat(sprintf('\n-- %s of income --\n', tn))
  print(top_inc_all %>% filter(top == tn) %>%
          select(asset, source, composition_in_top) %>%
          pivot_wider(names_from = source, values_from = composition_in_top) %>%
          mutate(across(-asset, ~ sprintf('%.1f%%', 100 * .x))),
        n = length(wealth_asset_vars))
}

# Plots — (I): share-by-income concentration, faceted by asset
p_share_top = top_inc_all %>%
  ggplot(aes(top, 100 * share_of_asset_in_top, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c(small = '#4daf4a', medium = '#377eb8',
                               large = '#e41a1c', large_binv2 = '#984ea3',
                               `SCF (truth)` = 'black')) +
  facet_wrap(~ asset, ncol = 4, scales = 'free_y') +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  labs(title = 'Share of each asset $ held by top-X% of income',
       subtitle = 'Concentration-of-asset by income percentile. Compare PUF specs to SCF truth.',
       x = NULL, y = 'Share of aggregate asset $ (%)') +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = 'bold'))

ggsave(file.path(plot_dir, '11_share_asset_by_top_income.png'),
       p_share_top, width = 13, height = 9, dpi = 140)

# Plot (K): composition within top-X% — stacked bars per (spec, top)
p_composition = top_inc_all %>%
  ggplot(aes(source, 100 * composition_in_top, fill = asset)) +
  geom_col(position = 'stack', color = 'white', linewidth = 0.15) +
  facet_wrap(~ top, ncol = 3) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  labs(title = 'Asset-composition within top-X% of income',
       subtitle = '$ share of each asset category in total assets of top-X%-income filers',
       x = NULL, y = 'Composition of assets (%)', fill = 'Asset') +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(plot_dir, '12_asset_composition_top_income.png'),
       p_composition, width = 13, height = 7, dpi = 140)

#---------------------------------------------------------------------------
# Console summary: compact table with the headline numbers
#---------------------------------------------------------------------------

cat('\n\n==========================================================\n')
cat('  SUMMARY — headline validation numbers per spec vs SCF\n')
cat('==========================================================\n\n')

headline = top_shares %>%
  select(source, total_nw_tn, top10_share, top1_share, top01_share, gini) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(headline)

cat('\nWealth-tax base at $50M threshold:\n')
print(wt_tbl %>% filter(threshold == '50M') %>%
        select(source, n_filers_m, taxable_base_tn))

cat(sprintf('\n--- plots + CSVs written to %s/ ---\n', plot_dir))
cat('  07_joint_income_nw_mass.png    — 10×10 joint mass heatmap\n')
cat('  08_ownership_by_decile.png     — per-asset ownership-rate lines\n')
cat('  09_unrealized_kg_by_decile.png — KG aggregate by income decile\n')
cat('  10_nw_quantiles_by_inc_decile.png — NW thresholds by income decile\n')
cat('  11_share_asset_by_top_income.png — share of each asset in top-X% income\n')
cat('  12_asset_composition_top_income.png — asset mix within top-X% income\n')
cat('  per_asset_top_income.csv — tables (H,I,J,K) per-asset × top_frac × source\n')
cat('  top_shares.csv\n')
cat('  wealth_tax_counters.csv\n')
cat('  joint_income_nw_mass.csv\n')
cat('  ownership_by_decile.csv\n')
cat('  unrealized_kg_by_decile.csv\n')
cat('  nw_quantiles_by_inc_decile.csv\n')
cat('  rank_corr_nw.csv\n')

cat('\nDone.\n')
