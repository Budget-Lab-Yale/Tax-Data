#---------------------------------------------
# training_reweight_1d_test.R
#
# 1D test of training-time reweighting as a
# count-matching mechanism. Targets the worst-
# matched (cell × age × CATEGORY) bucket:
# pct99.9to100 × senior × equities, where SCF
# count is 28.8k and forest gives 55.7k (+93%).
# Varies a single multiplier α applied to the
# bootstrap weight of SCF rows where
# (age >= 65 AND equities > 0).
# Retrains the pct99.9to100 forest for each α;
# runs leaf walk + uniform draw on PUF rows in
# that cell; tallies imputed equities count
# and aggregate for the senior subset.
#
# Outputs a curve of α vs (count, aggregate)
# vs SCF target.
#
# Hypothesis: if α ≈ 0.5 brings PUF count in
# line with SCF, training reweighting works as
# a count-matching mechanism. If we need
# α << 0.1 or convergence is unstable, the
# leaf-restriction may be the binding constraint
# even at training time.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(drf); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

estimate_models = 0L
do_lp           = 0L

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
# wealth.R defines scf_to_y at top-level (before run_wealth_imputation).
source('src/imputations/wealth.R')


CELL          = 'pct99.9to100'
SENIOR_AGE    = 65L
N_BOOT_CELL   = 150000L
ALPHAS        = c(1.0, 0.7, 0.5, 0.3, 0.15, 0.05)
TRAIN_SEED    = 108L  # matches wealth.R cell idx 8 seed
WALK_SEED     = 308L

# Target category — the wealth Y-var to count holders of. We pick the
# worst-matched bucket from the verification: equities in pct99.9to100
# senior (SCF count 28.8k, post-Stage-3 forest 55.7k = +93%).
TARGET_CAT    = 'equities'


# Mirror features list from wealth.R run_wealth_imputation interior.
features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'age_younger', 'n_dep_hh',
             'pctile_wages',         'has_wages',
             'pctile_business',      'has_business_act',
                                     'has_business_pos',
             'pctile_int_div',       'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act',
                                     'has_capital_gains_pos',
             'pctile_ss_pens',       'has_ss_pens')

make_has = function(x) {
  case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)
}


#--- Load + prep SCF training frame ------------------------------------

scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

scf_tax_units = scf_tax_units %>%
  mutate(
    n_dep_hh    = n_dep,
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    has_income_act        = as.integer(income != 0),
    has_income_pos        = as.integer(income >  0),
    has_negative_income   = as.integer(income <  0),
    pctile_income         = compute_percentile(income, weight, FINE_PCTILE_PROBS),
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

scf_training = scf_tax_units %>%
  scf_to_y() %>%
  mutate(income = wages_scf + business_scf + int_div_scf +
                  capital_gains_scf + rent_scf + ss_pens_scf +
                  ui_other_scf) %>%
  select(weight, income, all_of(features), all_of(wealth_y_vars))

# Cell assignment (matches wealth.R)
scf_cells = assign_calibration_cells(
  data.frame(weight = scf_training$weight),
  scf_training$income, scf_training$age_older, scf_training$weight
)$cell_income

scf_in_cell    = which(scf_cells == CELL)
scf_w_in_cell  = scf_training$weight[scf_in_cell]
scf_age_in_cell = scf_training$age_older[scf_in_cell]
scf_y_in_cell  = scf_training[scf_in_cell, wealth_y_vars]
scf_target_val_cell = scf_y_in_cell[[TARGET_CAT]]

is_target = (scf_age_in_cell >= SENIOR_AGE) & (scf_target_val_cell > 0)
cat(sprintf('SCF cell %s: %d rows, %d senior+%s>0 (%.1f%% of cell rows)\n',
            CELL, length(scf_in_cell), sum(is_target),
            TARGET_CAT, 100 * mean(is_target)))

scf_count_target = sum(scf_w_in_cell[is_target])
scf_total_target = sum(scf_w_in_cell[is_target] * scf_target_val_cell[is_target])
cat(sprintf('SCF target (cell × senior × %s>0): count = %.3fM, total = $%.3fT\n',
            TARGET_CAT, scf_count_target / 1e6, scf_total_target / 1e12))


#--- Load + prep PUF features (mirrors wealth.R 250-336) ---------------

snap_path = file.path(output_dir, 'puf_2022_snapshot.rds')
if (file.exists(snap_path)) {
  puf_2022 = read_rds(snap_path)
} else {
  puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                      show_col_types = FALSE)
  for (v in wealth_y_vars) puf_2022[[v]] = NA_real_
}

puf = puf_2022 %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped), age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped), 0L),
    married = as.integer(!is.na(male2)),
    n_dep_hh = (!is.na(dep_age1) & dep_age1 < 18) +
               (!is.na(dep_age2) & dep_age2 < 18) +
               (!is.na(dep_age3) & dep_age3 < 18),
    income  = wages + sole_prop + farm +
              scorp_active  - scorp_active_loss  - scorp_179 +
              scorp_passive - scorp_passive_loss +
              part_active   - part_active_loss   - part_179 +
              part_passive  - part_passive_loss +
              txbl_int + exempt_int + div_ord + div_pref +
              kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
              rent - rent_loss + estate - estate_loss,
    wages_puf         = wages,
    business_puf      = sole_prop + farm +
                        scorp_active  - scorp_active_loss  - scorp_179 +
                        scorp_passive - scorp_passive_loss +
                        part_active   - part_active_loss   - part_179 +
                        part_passive  - part_passive_loss,
    int_div_puf_raw   = txbl_int + exempt_int + div_ord + div_pref,
    int_div_puf       = if_else(int_div_puf_raw > 0 & int_div_puf_raw <= 100,
                                0, int_div_puf_raw),
    capital_gains_puf = kg_lt + kg_st,
    ss_pens_puf       = gross_ss + gross_pens_dist,
    has_income_act      = as.integer(income != 0),
    has_income_pos      = as.integer(income >  0),
    has_negative_income = as.integer(income <  0),
    pctile_income       = compute_percentile(income, weight, FINE_PCTILE_PROBS),
    has_wages             = make_has(wages_puf),
    pctile_wages          = compute_percentile(wages_puf, weight, FINE_PCTILE_PROBS),
    has_business_act      = as.integer(business_puf != 0),
    has_business_pos      = as.integer(business_puf >  0),
    pctile_business       = compute_percentile(business_puf, weight, FINE_PCTILE_PROBS),
    has_int_div           = make_has(int_div_puf),
    pctile_int_div        = compute_percentile(int_div_puf, weight, FINE_PCTILE_PROBS),
    has_capital_gains_act = as.integer(capital_gains_puf != 0),
    has_capital_gains_pos = as.integer(capital_gains_puf >  0),
    pctile_capital_gains  = compute_percentile(capital_gains_puf, weight, FINE_PCTILE_PROBS),
    has_ss_pens           = make_has(ss_pens_puf),
    pctile_ss_pens        = compute_percentile(ss_pens_puf, weight, FINE_PCTILE_PROBS)
  ) %>%
  select(id, weight, income, age_older, all_of(features))

puf_cells = assign_calibration_cells(
  data.frame(weight = puf$weight),
  puf$income, puf$age_older, puf$weight
)$cell_income

puf_in_cell    = which(puf_cells == CELL)
puf_w_in_cell  = puf$weight[puf_in_cell]
puf_age_in_cell = puf$age_older[puf_in_cell]
puf_is_senior  = puf_age_in_cell >= SENIOR_AGE
X_puf_in_cell  = as.matrix(puf[puf_in_cell, features])

cat(sprintf('PUF cell %s: %d rows, pop %.3fM (senior: %.3fM)\n\n', CELL,
            length(puf_in_cell), sum(puf_w_in_cell) / 1e6,
            sum(puf_w_in_cell[puf_is_senior]) / 1e6))


#--- Walk forest + uniform-draw helper ----------------------------------

walk_to_leaf = function(f, t, x_row) {
  L  = f$child_L[[t]]; R  = f$child_R[[t]]
  v  = f$split_v[[t]]; s  = f$split_s[[t]]
  ll = f$leaves[[t]]
  node = f$root[t] + 1L
  repeat {
    if (length(ll[[node]]) > 0L) return(node)
    xv = x_row[v[node] + 1L]; sv = s[node]
    node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L else R[node] + 1L
  }
}

extract_forest = function(m) list(
  n_trees = m[['_num_trees']],
  root    = sapply(m[['_root_nodes']], identity),
  child_L = lapply(m[['_child_nodes']], `[[`, 1L),
  child_R = lapply(m[['_child_nodes']], `[[`, 2L),
  split_v = m[['_split_vars']],
  split_s = m[['_split_values']],
  leaves  = m[['_leaf_samples']]
)


#--- Main loop over alpha values ----------------------------------------

results = list()

for (alpha in ALPHAS) {
  cat(sprintf('\n=== alpha = %.3f ===\n', alpha))
  t0 = Sys.time()

  modified_w = scf_w_in_cell
  modified_w[is_target] = modified_w[is_target] * alpha

  set.seed(TRAIN_SEED)
  boot_in_cell = sample.int(length(scf_in_cell), size = N_BOOT_CELL,
                             replace = TRUE, prob = modified_w)
  boot_scf_row = scf_in_cell[boot_in_cell]
  scf_boot     = scf_training[boot_scf_row, , drop = FALSE]

  # Diagnostic: target subset's share of bootstrap
  boot_is_target = is_target[boot_in_cell]
  cat(sprintf('  Bootstrap composition: %.2f%% in target subset (vs %.2f%% if no reweight)\n',
              100 * mean(boot_is_target),
              100 * sum(scf_w_in_cell[is_target]) / sum(scf_w_in_cell)))

  drf_cell = drf::drf(
    X = as.matrix(scf_boot[features]),
    Y = as.matrix(scf_boot[wealth_y_vars]),
    num.trees      = 500,
    splitting.rule = 'FourierMMD',
    num.features   = 50,
    mtry           = 10,
    min.node.size  = 20,
    honesty        = TRUE,
    response.scaling = FALSE,
    num.threads    = n_threads()
  )
  cat(sprintf('  DRF training time: %.1fs\n',
              as.numeric(Sys.time() - t0, units = 'secs')))

  f      = extract_forest(drf_cell)
  Y_boot = as.matrix(scf_boot[wealth_y_vars])

  # PUF leaf walk + uniform draw of donor index
  set.seed(WALK_SEED)
  tree_pick = sample.int(f$n_trees, size = length(puf_in_cell), replace = TRUE)
  imputed_target_val = numeric(length(puf_in_cell))
  imputed_holds      = logical(length(puf_in_cell))
  for (j in seq_along(puf_in_cell)) {
    t   = tree_pick[j]
    nd  = walk_to_leaf(f, t, X_puf_in_cell[j, ])
    lr  = f$leaves[[t]][[nd]] + 1L
    don = lr[sample.int(length(lr), 1L)]
    val = Y_boot[don, TARGET_CAT]
    imputed_target_val[j] = val
    imputed_holds[j]      = val > 0
  }

  count_imputed = sum(puf_w_in_cell[puf_is_senior & imputed_holds])
  total_imputed = sum(puf_w_in_cell[puf_is_senior] * imputed_target_val[puf_is_senior])

  cat(sprintf('  PUF imputed (senior, %s>0): count = %.3fM   total = $%.3fT\n',
              TARGET_CAT, count_imputed / 1e6, total_imputed / 1e12))
  cat(sprintf('  vs SCF target            : count = %.3fM   total = $%.3fT\n',
              scf_count_target / 1e6, scf_total_target / 1e12))
  cat(sprintf('  count_err = %+.1f%%   total_err = %+.1f%%\n',
              100 * (count_imputed / scf_count_target - 1),
              100 * (total_imputed / scf_total_target - 1)))

  results[[as.character(alpha)]] = tibble(
    alpha          = alpha,
    boot_target_share = mean(boot_is_target),
    count_imputed_M = count_imputed / 1e6,
    total_imputed_T = total_imputed / 1e12,
    count_err      = count_imputed / scf_count_target - 1,
    total_err      = total_imputed / scf_total_target - 1
  )
}


#--- Summary -------------------------------------------------------------

cat('\n\n=== Response curve (α vs PUF imputed count) ===\n')
cat(sprintf('SCF target: count = %.3fM, total = $%.3fT\n\n',
            scf_count_target / 1e6, scf_total_target / 1e12))

summary_tbl = bind_rows(results) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(summary_tbl), row.names = FALSE)

cat('\nDone.\n')
