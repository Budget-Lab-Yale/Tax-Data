#---------------------------------------------
# wealth_diagnosis.R
#
# Root-cause diagnosis for the PUF vs SCF per-
# income-quintile wealth-share mismatch (pre-tilt).
#
# Four questions, each addressed by a self-
# contained test:
#
#   Q1. Within each income-rank bin, do PUF and
#       SCF have the same JOINT feature
#       distribution? (Same-rank-different-
#       population is a real possibility in rank
#       space: both datasets have p0-20 = "bottom
#       20% of their own weighted income," but the
#       composition of that bottom 20% may differ.
#       Dollar cutoffs are irrelevant here — we're
#       asking whether the forest, which operates
#       on features, is seeing similar inputs.)
#
#   Q2. Is the DRF self-consistent?
#       → Apply the trained forest to SCF records
#         (using their own X features), draw
#         donors, and check whether SCF-on-SCF
#         aggregates by income quintile match SCF
#         truth. If yes, the forest is fine; the
#         problem is PUF-specific X-shift. If no,
#         the forest has an inherent bias.
#
#   Q3. Where does PUF's imputed wealth come from?
#       → For each PUF record, look up its
#         pre-tilt donor's SCF income quintile.
#         Cross-tab PUF_quintile × donor_quintile.
#         Under a perfect match, this is the
#         identity matrix. Deviations reveal
#         exactly where the wealth "slosh" goes.
#
#   Q4. Is donor usage concentrated?
#       → Count how many times each original SCF
#         tax unit appears as a donor across PUF
#         records. A few donors picked often
#         indicates heavy-tail contamination.
#
# Requires a pipeline output directory with
# tax_units_2022.csv (for the pre-wealth PUF).
#
# Usage:
#   Rscript src/eda/wealth_diagnosis.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript src/eda/wealth_diagnosis.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

# estimate_models = 0 for cached forest; takes ~1 min total per call
estimate_models = 0
do_lp           = 0

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')


#--- Load inputs ----------------------------------------------------------

cat('Loading scf_tax_units + puf_2022...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')
puf_2022_csv  = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                          show_col_types = FALSE)
for (v in wealth_y_vars) puf_2022_csv[[v]] = NA_real_
puf_2022 = puf_2022_csv

# Collapse SCF raw → 23 Y-vars if needed.
source('src/imputations/wealth.R')   # also defines scf_to_y at module scope


#--- Run wealth imputation with debug_output = TRUE -----------------------

cat('Running run_wealth_imputation (debug mode)...\n')
t0 = Sys.time()
set.seed(76)
result = run_wealth_imputation(puf_2022, scf_tax_units, debug_output = TRUE)
cat(sprintf('  took %.1fs\n', as.numeric(Sys.time() - t0, units = 'secs')))

pre_tilt_donor_boot_idx = result$pre_tilt_donor_boot_idx
boot_idx                = result$boot_idx


#--- Recompute incomes on both sides ---------------------------------------

scf_tax_units$income = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)

puf_age2 = ifelse(is.na(puf_2022$age2), 0L, puf_2022$age2)
puf_2022$income = with(puf_2022,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st +
  gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)

#---------------------------------------------------------------------------
# Q1: Within-rank-bin joint feature-distribution comparison
#---------------------------------------------------------------------------

cat('\n==============================================================\n')
cat('Q1. Within-rank-bin composition: PUF vs SCF\n')
cat('==============================================================\n\n')

# Need percentile bins on both datasets' own weighted income distribution.
scf_pctile_tmp = compute_percentile(scf_tax_units$income, scf_tax_units$weight,
                                    seq(0.01, 0.99, 0.01))
puf_pctile_tmp = compute_percentile(puf_2022$income, puf_2022$weight,
                                    seq(0.01, 0.99, 0.01))

to_bin_tmp = function(pct) {
  cut(pct, breaks = c(-1, 19, 39, 59, 79, 89, 98, 100),
      labels = c('p0-20','p20-40','p40-60','p60-80',
                 'p80-90','p90-99','top1'),
      include.lowest = TRUE, right = TRUE)
}
scf_bin_tmp = to_bin_tmp(scf_pctile_tmp)
puf_bin_tmp = to_bin_tmp(puf_pctile_tmp)

# Compute composition stats per bin, side-by-side.
scf_ageolder = pmax(pmin(80L, scf_tax_units$age1),
                    pmin(80L, ifelse(is.na(scf_tax_units$age2), 0L,
                                      scf_tax_units$age2)))
puf_ageolder = pmax(pmin(80L, puf_2022$age1),
                    pmin(80L, ifelse(is.na(puf_2022$age2), 0L, puf_2022$age2)))

puf_business = with(puf_2022, sole_prop + farm +
                    scorp_active + scorp_passive + part_active + part_passive)
puf_int_div  = with(puf_2022, txbl_int + exempt_int + div_ord + div_pref)
puf_kg       = with(puf_2022, kg_lt + kg_st)
puf_ss_pens  = with(puf_2022, gross_ss + gross_pens_dist)

wmean = function(x, w) sum(x * w) / sum(w)

composition = function(df_w, age_older, income, wages, business, int_div,
                       kg, ss_pens, bin, label) {
  bins = levels(bin)
  purrr::map_dfr(bins, function(b) {
    s = bin == b
    w = df_w[s]
    tibble(
      source       = label,
      bin          = b,
      n_unwt       = sum(s),
      n_wt_m       = sum(w) / 1e6,
      mean_age     = wmean(age_older[s], w),
      frac_senior  = wmean(age_older[s] >= 65, w),
      frac_neg_inc = wmean(income[s] < 0, w),
      frac_pos_wages    = wmean(wages[s] > 0, w),
      frac_pos_business = wmean(business[s] > 0, w),
      frac_pos_int_div  = wmean(int_div[s] > 0, w),
      frac_pos_kg       = wmean(kg[s] > 0, w),
      frac_pos_ss_pens  = wmean(ss_pens[s] > 0, w)
    )
  })
}

scf_comp = composition(scf_tax_units$weight, scf_ageolder,
                       scf_tax_units$income,
                       scf_tax_units$wages_scf,
                       scf_tax_units$business_scf,
                       scf_tax_units$int_div_scf,
                       scf_tax_units$capital_gains_scf,
                       scf_tax_units$ss_pens_scf,
                       scf_bin_tmp, 'SCF')
puf_comp = composition(puf_2022$weight, puf_ageolder,
                       puf_2022$income,
                       puf_2022$wages,
                       puf_business, puf_int_div, puf_kg, puf_ss_pens,
                       puf_bin_tmp, 'PUF')

cat('Composition within each income rank bin (SCF over PUF per bin):\n\n')
combined = bind_rows(scf_comp, puf_comp) %>%
  mutate(across(c(mean_age), round, 1),
         across(c(n_wt_m), round, 1),
         across(c(frac_senior, frac_neg_inc, frac_pos_wages,
                  frac_pos_business, frac_pos_int_div, frac_pos_kg,
                  frac_pos_ss_pens),
                ~ round(.x, 3))) %>%
  arrange(bin, source)
print(combined, n = Inf, width = Inf)


#---------------------------------------------------------------------------
# Q3. Donor-origin cross-tab
# (Do this before Q2 because it uses the already-computed result.)
#---------------------------------------------------------------------------

cat('\n\n==============================================================\n')
cat('Q3. Where does each PUF record\'s donor come from (by SCF quintile)?\n')
cat('==============================================================\n\n')

# PUF record i's donor is scf_boot[pre_tilt_donor_boot_idx[i]], which
# corresponds to original SCF row boot_idx[pre_tilt_donor_boot_idx[i]].
puf_donor_scf_row = boot_idx[pre_tilt_donor_boot_idx]
stopifnot(length(puf_donor_scf_row) == nrow(puf_2022))

# Bin both PUF records and SCF donors with the SAME percentile breakpoints
# (each dataset uses its own weighted income distribution to define its
# bins). The donor's bin is defined by SCF's percentile system (the
# training-data perspective).
scf_pctile = compute_percentile(scf_tax_units$income, scf_tax_units$weight,
                                 seq(0.01, 0.99, 0.01))
puf_pctile = compute_percentile(puf_2022$income, puf_2022$weight,
                                 seq(0.01, 0.99, 0.01))

to_bin = function(pct) {
  cut(pct, breaks = c(-1, 19, 39, 59, 79, 89, 98, 100),
      labels = c('p0-20','p20-40','p40-60','p60-80',
                 'p80-90','p90-99','top1'),
      include.lowest = TRUE, right = TRUE)
}
scf_bin   = to_bin(scf_pctile)
puf_bin   = to_bin(puf_pctile)
donor_bin = scf_bin[puf_donor_scf_row]

xtab = tibble(puf_bin = puf_bin, donor_bin = donor_bin,
              weight = puf_2022$weight) %>%
  group_by(puf_bin, donor_bin) %>%
  summarise(wt = sum(weight), .groups = 'drop') %>%
  group_by(puf_bin) %>%
  mutate(share_of_puf_bin = wt / sum(wt)) %>%
  ungroup()

cat('Share of PUF records in each bin whose donor came from SCF bin X:\n')
cat('(rows: PUF income bin; columns: donor SCF income bin. ',
    'Identity = perfect match.)\n\n', sep = '')
wide = xtab %>%
  select(puf_bin, donor_bin, share_of_puf_bin) %>%
  pivot_wider(names_from = donor_bin, values_from = share_of_puf_bin,
              values_fill = 0) %>%
  mutate(across(-puf_bin, ~ round(.x, 3)))
print(wide, n = Inf)

# Also the reverse: where do SCF's p0-20 donors go?
cat('\nWeighted total NW (trillions) attached to each (PUF bin, donor bin) cell:\n')
scf_y = scf_to_y(scf_tax_units)
scf_nw = rowSums(scf_y[, wealth_asset_vars]) - rowSums(scf_y[, wealth_debt_vars])
puf_nw_per_record = scf_nw[puf_donor_scf_row]
wt_nw_tbl = tibble(puf_bin = puf_bin, donor_bin = donor_bin,
                   wt_nw = puf_2022$weight * puf_nw_per_record) %>%
  group_by(puf_bin, donor_bin) %>%
  summarise(wt_nw_trillion = sum(wt_nw) / 1e12, .groups = 'drop') %>%
  pivot_wider(names_from = donor_bin, values_from = wt_nw_trillion,
              values_fill = 0) %>%
  mutate(across(-puf_bin, ~ round(.x, 2)))
print(wt_nw_tbl, n = Inf)


#---------------------------------------------------------------------------
# Q2. Forest self-consistency — apply forest to SCF itself
#---------------------------------------------------------------------------

cat('\n\n==============================================================\n')
cat('Q2. Is the DRF self-consistent? (forest on SCF → SCF truth?)\n')
cat('==============================================================\n\n')

# Re-engineer SCF features exactly as run_wealth_imputation does, then
# walk the forest with those features. This is "SCF-on-SCF" imputation.
# If the forest reproduces SCF's own aggregate by quintile, it's fine.

make_has = function(x) case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)
features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'age_younger', 'n_dep_hh',
             'pctile_wages', 'has_wages',
             'pctile_business', 'has_business_act', 'has_business_pos',
             'pctile_int_div', 'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act', 'has_capital_gains_pos',
             'pctile_ss_pens', 'has_ss_pens')

scf_features_tbl = scf_tax_units %>%
  mutate(
    n_dep_hh    = n_dep,
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    income_recompute = wages_scf + business_scf + int_div_scf + capital_gains_scf +
                       rent_scf + ss_pens_scf + ui_other_scf,
    has_income_act        = as.integer(income_recompute != 0),
    has_income_pos        = as.integer(income_recompute >  0),
    has_negative_income   = as.integer(income_recompute <  0),
    pctile_income         = compute_percentile(income_recompute, weight, FINE_PCTILE_PROBS),
    has_wages             = make_has(wages_scf),
    pctile_wages          = compute_percentile(wages_scf, weight, FINE_PCTILE_PROBS),
    has_business_act      = as.integer(business_scf != 0),
    has_business_pos      = as.integer(business_scf >  0),
    pctile_business       = compute_percentile(business_scf, weight, FINE_PCTILE_PROBS),
    has_int_div           = make_has(int_div_scf),
    pctile_int_div        = compute_percentile(int_div_scf, weight, FINE_PCTILE_PROBS),
    has_capital_gains_act = as.integer(capital_gains_scf != 0),
    has_capital_gains_pos = as.integer(capital_gains_scf >  0),
    pctile_capital_gains  = compute_percentile(capital_gains_scf, weight, FINE_PCTILE_PROBS),
    has_ss_pens           = make_has(ss_pens_scf),
    pctile_ss_pens        = compute_percentile(ss_pens_scf, weight, FINE_PCTILE_PROBS)
  )

scf_X = as.matrix(scf_features_tbl[features])

# Walk forest for SCF — replicate wealth.R's walk logic.
wealth_drf = read_rds('resources/cache/qrf/wealth_drf.rds')
n_trees = wealth_drf[['_num_trees']]
root    = sapply(wealth_drf[['_root_nodes']], identity)
child_L = lapply(wealth_drf[['_child_nodes']], `[[`, 1L)
child_R = lapply(wealth_drf[['_child_nodes']], `[[`, 2L)
split_v = wealth_drf[['_split_vars']]
split_s = wealth_drf[['_split_values']]
leaves  = wealth_drf[['_leaf_samples']]

walk_to_leaf = function(t, x_row) {
  L = child_L[[t]]; R = child_R[[t]]
  v = split_v[[t]]; s = split_s[[t]]
  ll = leaves[[t]]
  node = root[t] + 1L
  repeat {
    if (length(ll[[node]]) > 0L) return(node)
    xv = x_row[v[node] + 1L]; sv = s[node]
    node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L else R[node] + 1L
  }
}

set.seed(77)   # separate seed stream for scf-self-test
n_scf_q = nrow(scf_X)
scf_donor_boot_idx = integer(n_scf_q)
tree_pick_scf      = sample.int(n_trees, size = n_scf_q, replace = TRUE)
for (i in seq_len(n_scf_q)) {
  t  = tree_pick_scf[i]
  nd = walk_to_leaf(t, scf_X[i, ])
  lr = leaves[[t]][[nd]] + 1L
  scf_donor_boot_idx[i] = lr[sample.int(length(lr), 1L)]
}
scf_donor_scf_row = boot_idx[scf_donor_boot_idx]
scf_scf_donor_nw  = scf_nw[scf_donor_scf_row]

# Aggregate by SCF income quintile (using SCF's own quintile assignment)
cat('SCF truth vs forest-applied-to-SCF (both aggregated by SCF income quintile):\n')
agg = tibble(bin = scf_bin, weight = scf_tax_units$weight,
             scf_truth_nw = scf_nw,
             scf_forest_nw = scf_scf_donor_nw) %>%
  group_by(bin) %>%
  summarise(
    scf_truth_total    = sum(weight * scf_truth_nw) / 1e12,
    scf_forest_total   = sum(weight * scf_forest_nw) / 1e12,
    diff_pct           = 100 * (scf_forest_total - scf_truth_total) / scf_truth_total,
    .groups = 'drop'
  ) %>%
  mutate(across(c(scf_truth_total, scf_forest_total), ~ round(.x, 2)),
         diff_pct = round(diff_pct, 1))
print(agg, n = Inf)

# Big picture: total NW via forest on SCF vs SCF truth
cat(sprintf('\nTotal NW:  SCF truth $%.2fT   forest-on-SCF $%.2fT   diff %.1f%%\n',
            sum(scf_tax_units$weight * scf_nw) / 1e12,
            sum(scf_tax_units$weight * scf_scf_donor_nw) / 1e12,
            100 * (sum(scf_tax_units$weight * scf_scf_donor_nw) -
                   sum(scf_tax_units$weight * scf_nw)) /
                   sum(scf_tax_units$weight * scf_nw)))


#---------------------------------------------------------------------------
# Q4. Donor usage concentration
#---------------------------------------------------------------------------

cat('\n\n==============================================================\n')
cat('Q4. Donor usage concentration\n')
cat('==============================================================\n\n')

# For each original SCF tax unit, how many PUF records picked it as donor?
# Weighted by PUF weight to get "effective weight this donor contributes."
donor_counts = tibble(
  scf_row  = puf_donor_scf_row,
  puf_wt   = puf_2022$weight
) %>%
  group_by(scf_row) %>%
  summarise(n_picks = dplyr::n(),
            eff_weight = sum(puf_wt), .groups = 'drop') %>%
  left_join(
    tibble(scf_row = seq_len(nrow(scf_tax_units)),
           scf_weight = scf_tax_units$weight,
           scf_nw    = scf_nw,
           scf_inc   = scf_tax_units$income),
    by = 'scf_row'
  ) %>%
  mutate(weight_ratio = eff_weight / scf_weight)

cat('Top 15 most-used SCF donors (by effective PUF weight):\n')
print(donor_counts %>%
      arrange(desc(eff_weight)) %>%
      select(scf_row, n_picks, eff_weight, scf_weight, weight_ratio,
             scf_inc, scf_nw) %>%
      mutate(eff_weight = round(eff_weight),
             scf_weight = round(scf_weight),
             weight_ratio = round(weight_ratio, 2),
             scf_inc = round(scf_inc),
             scf_nw  = round(scf_nw)) %>%
      head(15))

cat('\nTop 15 highest-NW SCF donors and their PUF usage:\n')
top_nw = tibble(
  scf_row = seq_len(nrow(scf_tax_units)),
  scf_nw = scf_nw, scf_inc = scf_tax_units$income,
  scf_weight = scf_tax_units$weight) %>%
  arrange(desc(scf_nw)) %>% head(15)
for_tp = donor_counts %>% right_join(top_nw, by = 'scf_row',
                                      suffix = c('', '_y')) %>%
  select(scf_row, scf_nw = scf_nw_y, scf_inc = scf_inc_y,
         scf_weight = scf_weight_y, n_picks, eff_weight, weight_ratio)
for_tp$n_picks[is.na(for_tp$n_picks)] = 0L
for_tp$eff_weight[is.na(for_tp$eff_weight)] = 0
for_tp$weight_ratio[is.na(for_tp$weight_ratio)] = 0
print(for_tp %>%
      mutate(scf_nw = round(scf_nw), scf_inc = round(scf_inc),
             scf_weight = round(scf_weight),
             eff_weight = round(eff_weight),
             weight_ratio = round(weight_ratio, 2)),
      n = Inf)


cat('\nDone.\n')
