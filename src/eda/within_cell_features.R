#--------------------------------------
# within_cell_features.R
#
# Why does the DRF assign ~2x SCF's
# per-household mean NW to PUF inside
# cell 4|mar|65+|biz0 even under the
# large spec?
#
# Hypothesis: within this decomp cell,
# PUF's finer DRF feature values
# (pctile_income, pctile_int_div,
# pctile_capital_gains, pctile_ss_pens,
# pctile_wages, pctile_business,
# age_older, age_younger) are
# distributed differently than SCF's —
# in a direction the DRF reads as
# "more wealthy".
#
# This script computes weighted
# quantiles of each DRF feature WITHIN
# the target cell, on each side, and
# prints side-by-side comparison.
#
# Inputs:
#   resources/cache/scf_tax_units.rds
#   resources/cache/consumption_analysis.rds
#   resources/cache/wealth_analysis.rds
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/imputations/helpers.R')  # compute_percentile

report_path = 'plots/wealth_decomp/within_cell_features_report.txt'
plot_dir    = 'plots/wealth_decomp'

scf      = read_rds('resources/cache/scf_tax_units.rds')
puf_dem  = read_rds('resources/cache/wealth_analysis.rds')
puf_cons = read_rds('resources/cache/consumption_analysis.rds')

sink(report_path, split = TRUE)
cat('Within-cell DRF feature distributions — 4|mar|65+|biz0\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Build SCF frame with large-spec features.
# Mirrors the ablation script's feature engineering (age_older/age_younger,
# unfloored income, dual-binary income, per-component percentiles).
#---------------------------------------------------------------------------

scf_h = scf %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    income_unfloored = wages_scf + business_scf + int_div_scf +
                       capital_gains_scf + rent_scf + ss_pens_scf + ui_other_scf,
    pctile_income   = compute_percentile(income_unfloored, weight),
    pctile_wages    = compute_percentile(wages_scf,         weight),
    pctile_business = compute_percentile(business_scf,      weight),
    pctile_int_div  = compute_percentile(int_div_scf,       weight),
    pctile_capital_gains = compute_percentile(capital_gains_scf, weight),
    pctile_ss_pens  = compute_percentile(ss_pens_scf,       weight)
  )

# Income quintile on scf_h (positive income only)
pos = scf_h$income_unfloored > 0
brks_scf = Hmisc::wtd.quantile(scf_h$income_unfloored[pos], scf_h$weight[pos],
                               probs = seq(0, 1, 0.2))
scf_h$pct_q = findInterval(scf_h$income_unfloored, brks_scf, all.inside = TRUE)
scf_h$pct_q[!pos] = 0L

# Cell membership
scf_cell = scf_h %>%
  filter(pct_q == 4L, married == 1L, age1 >= 65L, business_scf <= 0)

#---------------------------------------------------------------------------
# Build PUF frame with large-spec features.
#---------------------------------------------------------------------------

puf = puf_cons %>%
  transmute(
    id, weight, age1, age2, married,
    wages_p         = wages,
    business_p      = sole_prop + farm +
                      scorp_active  - scorp_active_loss  - scorp_179 +
                      scorp_passive - scorp_passive_loss +
                      part_active   - part_active_loss   - part_179 +
                      part_passive  - part_passive_loss,
    int_div_p       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains_p = kg_lt + kg_st,
    ss_pens_p       = gross_ss + gross_pens_dist,
    rent_p          = rent - rent_loss + estate - estate_loss,
    ui_other_p      = ui
  ) %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    income_p = wages_p + business_p + int_div_p + capital_gains_p +
               rent_p + ss_pens_p + ui_other_p,
    pctile_income   = compute_percentile(income_p, weight),
    pctile_wages    = compute_percentile(wages_p, weight),
    pctile_business = compute_percentile(business_p, weight),
    pctile_int_div  = compute_percentile(int_div_p, weight),
    pctile_capital_gains = compute_percentile(capital_gains_p, weight),
    pctile_ss_pens  = compute_percentile(ss_pens_p, weight)
  )

pos_p = puf$income_p > 0
brks_puf = Hmisc::wtd.quantile(puf$income_p[pos_p], puf$weight[pos_p],
                               probs = seq(0, 1, 0.2))
puf$pct_q = findInterval(puf$income_p, brks_puf, all.inside = TRUE)
puf$pct_q[!pos_p] = 0L

puf_cell = puf %>%
  filter(pct_q == 4L, married == 1L, age1 >= 65L, business_p <= 0)

#---------------------------------------------------------------------------
# Report.
#---------------------------------------------------------------------------

cat(sprintf('SCF cell: %d rows, weighted = %.2fM households\n',
            nrow(scf_cell), sum(scf_cell$weight) / 1e6))
cat(sprintf('PUF cell: %d rows, weighted = %.2fM households\n',
            nrow(puf_cell), sum(puf_cell$weight) / 1e6))

# Weighted quantile helper
wq = function(x, w, probs) {
  v = Hmisc::wtd.quantile(x, w, probs = probs)
  as.numeric(v)
}
probs = c(0.10, 0.25, 0.50, 0.75, 0.90)

compare_feature = function(fname) {
  s = scf_cell[[fname]]; sw = scf_cell$weight
  p = puf_cell[[fname]]; pw = puf_cell$weight
  s_q = wq(s, sw, probs)
  p_q = wq(p, pw, probs)
  tibble(
    feature = fname,
    side    = c('SCF', 'PUF', 'Δ'),
    p10 = c(s_q[1], p_q[1], p_q[1] - s_q[1]),
    p25 = c(s_q[2], p_q[2], p_q[2] - s_q[2]),
    p50 = c(s_q[3], p_q[3], p_q[3] - s_q[3]),
    p75 = c(s_q[4], p_q[4], p_q[4] - s_q[4]),
    p90 = c(s_q[5], p_q[5], p_q[5] - s_q[5]),
    mean = c(sum(s*sw)/sum(sw), sum(p*pw)/sum(pw),
             sum(p*pw)/sum(pw) - sum(s*sw)/sum(sw))
  )
}

features_to_compare = c(
  'pctile_income', 'pctile_wages', 'pctile_int_div',
  'pctile_capital_gains', 'pctile_ss_pens', 'pctile_business',
  'age_older', 'age_younger'
)

cat('\n\nFeature distributions, weighted quantiles\n')
cat('(Δ row = PUF − SCF; positive means PUF shifted toward higher values)\n\n')

all_compare = map_dfr(features_to_compare, compare_feature)

for (f in features_to_compare) {
  sub = all_compare %>% filter(feature == f)
  cat(sprintf('  %-22s  %-4s  %7s %7s %7s %7s %7s  %7s\n',
              f, 'side',
              'p10', 'p25', 'p50', 'p75', 'p90', 'mean'))
  for (i in seq_len(nrow(sub))) {
    fmt = if (grepl('^age', f)) '%7.1f' else '%7.2f'
    cat(sprintf('  %-22s  %-4s  ', '', sub$side[i]))
    for (col in c('p10', 'p25', 'p50', 'p75', 'p90', 'mean')) {
      cat(sprintf(fmt, sub[[col]][i]))
      cat(' ')
    }
    cat('\n')
  }
  cat('\n')
}

# Positivity shares for the non-pctile features — what fraction of cell has > 0
cat('Positivity shares within cell (share with component > 0)\n\n')
comp_vars_scf = c(pctile_int_div = 'int_div_scf',
                  pctile_capital_gains = 'capital_gains_scf',
                  pctile_ss_pens = 'ss_pens_scf',
                  pctile_wages = 'wages_scf')
comp_vars_puf = c(pctile_int_div = 'int_div_p',
                  pctile_capital_gains = 'capital_gains_p',
                  pctile_ss_pens = 'ss_pens_p',
                  pctile_wages = 'wages_p')

pos_tbl = map_dfr(names(comp_vars_scf), function(k) {
  s = scf_cell[[comp_vars_scf[[k]]]]
  p = puf_cell[[comp_vars_puf[[k]]]]
  tibble(
    component = k,
    scf_pos_share = sum(scf_cell$weight[s > 0]) / sum(scf_cell$weight),
    puf_pos_share = sum(puf_cell$weight[p > 0]) / sum(puf_cell$weight),
    delta_pp      = 100 * (sum(puf_cell$weight[p > 0]) / sum(puf_cell$weight) -
                           sum(scf_cell$weight[s > 0]) / sum(scf_cell$weight))
  )
})

cat(sprintf('  %-22s  %10s  %10s  %8s\n',
            'component', 'SCF pos%', 'PUF pos%', 'Δ pp'))
for (i in seq_len(nrow(pos_tbl))) {
  cat(sprintf('  %-22s  %9.1f%%  %9.1f%%  %+7.1f\n',
              pos_tbl$component[i],
              100 * pos_tbl$scf_pos_share[i],
              100 * pos_tbl$puf_pos_share[i],
              pos_tbl$delta_pp[i]))
}

write_csv(all_compare, file.path(plot_dir, 'within_cell_4mar65_biz0_features.csv'))
write_csv(pos_tbl,     file.path(plot_dir, 'within_cell_4mar65_biz0_positivity.csv'))

cat('\n\nArtifacts:\n')
cat('  within_cell_4mar65_biz0_features.csv\n')
cat('  within_cell_4mar65_biz0_positivity.csv\n')
cat('  within_cell_features_report.txt\n')

sink()
