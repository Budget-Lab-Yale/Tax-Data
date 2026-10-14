#--------------------------------------------------------------------------
# d_drf.R  (piece D, method 1: the production wealth method, refit)
#
# A copy, outside the pipeline, of Stage 2 of src/imputations/wealth.R:
# the per-income-cell distributional random forest and its donor draw,
# WITHOUT the Stage 3 tilt and rescale to SCF aggregates. Nothing in src/
# is sourced or edited, and nothing is written to resources/cache/: the
# fits go to $S/comp/<dataset>/drf_cache/, so no production fit can be
# overwritten.
#
# What it does, as wealth.R does:
#   1. Features on both sides from the seven income components (weighted
#      percentile of each within the dataset, sign flags) plus married,
#      age and dependents.
#   2. Training (donor) records are split into 8 income cells by weighted
#      rank of total income (0-20, 20-40, 40-60, 60-80, 80-90, 90-99,
#      99-99.9, 99.9-100). In each cell, a weighted bootstrap (rows drawn
#      with probability proportional to the survey weight) of 100,000 rows
#      (150,000 in the top cell) is drawn, and an unweighted distributional
#      random forest is fit to it, predicting all 24 wealth amounts jointly.
#      A distributional random forest is a random forest whose splits are
#      chosen to separate records with different joint distributions of
#      the 24 outcomes, rather than different means.
#   3. Each target record is assigned to an income cell by its weighted
#      rank within the target set, picks one tree of that cell's forest at
#      random, follows the tree to a leaf, and copies the full 24-variable
#      wealth vector of one training record drawn uniformly from the leaf.
#
# Deviations from production (all also in COMPARATORS_REPORT.md):
#   - Features: production uses the exact age of the older and the younger
#     spouse; here only the shared age range of the unit head exists, so
#     `age_older` is the age-range code and `age_younger` is dropped
#     (19 features instead of 20).
#   - Five draws per record (five independent tree-and-donor picks), each
#     seeded from (seed, draw), instead of one draw keyed by record id.
#   - Training seeds 1, 2, 3 change the bootstrap and the forest seed.
#   - Split-off SCF units (wealth unobserved in this test, John 9 Oct) are
#     not donors; production keeps them with zero wealth. They stay in the
#     rank base, so donors and targets are ranked among all filers.
#   - Targets with no age range (PUF records outside the demographic file)
#     get one drawn from donors in the same income cell and marital status.
#   - No Stage 3 tilt, no Step B rescale, no accruals or basis columns, no
#     Forbes purge.
#   - Experiment B: the two SCF waves are pooled as production pools 2019
#     and 2022: the older wave (2013) is reflated to 2016 dollars per column
#     by the ratio of weighted totals, and each wave is scaled to the same
#     population and halved. Completed values for 2013 targets are deflated
#     back by the same per-column factor; PUF records at 2015 are scaled by
#     the factor to the power -1/3 (log-linear between the waves).
#
# Usage (through sbatch only):
#   Rscript d_drf.R <dataset> <seed> [draws=5] [n_boot_scale=1] [num_trees=500]
#--------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(Hmisc)
  library(jsonlite)
})

args      = commandArgs(trailingOnly = TRUE)
dataset   = args[1]
seed      = as.integer(args[2])
n_draws   = if (length(args) >= 3) as.integer(args[3]) else 5L
boot_scale = if (length(args) >= 4) as.numeric(args[4]) else 1
num_trees = if (length(args) >= 5) as.integer(args[5]) else 500L
stopifnot(!is.na(seed), n_draws >= 1L)

S        = '/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint'
comp_dir = file.path(S, 'comp', dataset)
in_dir   = file.path(comp_dir, 'drf_input')
cache_dir = file.path(comp_dir, 'drf_cache')
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
n_threads = as.integer(Sys.getenv('SLURM_CPUS_PER_TASK', '4'))
t_start = Sys.time()

#---------------------------------------------------------------------------
# Copied from src/imputations/wealth_schema.R, helpers.R, stage3_target_qc.R
#---------------------------------------------------------------------------
wealth_asset_vars = c(
  'cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities',
  'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
  're_fund', 'other_nonfin')
wealth_debt_vars = c(
  'primary_mortgage', 'other_mortgage', 'credit_lines',
  'credit_cards', 'installment_debt', 'other_debt')
wealth_kg_vars = c(
  'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other')
wealth_y_vars = c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)
income_vars = c('wages', 'business', 'int_div', 'capital_gains', 'rent',
                'ss_pens', 'ui_other')

compute_percentile = function(x, weight, probs = seq(0.01, 0.99, 0.01)) {
  breaks = sort(wtd.quantile(x[x > 0], weight[x > 0], probs))
  pct = findInterval(x, breaks) + 1L
  pct = pmin(pct, length(probs) + 1L)
  pct[x <= 0 | is.na(x)] = 0L
  as.integer(pct)
}
FINE_PCTILE_PROBS = c(seq(0.01, 0.99, 0.01), seq(0.991, 0.999, 0.001), 0.9999)

CALIB_INCOME_BUCKETS = c('pct00to20', 'pct20to40', 'pct40to60',
                         'pct60to80', 'pct80to90', 'pct90to99',
                         'pct99to99.9', 'pct99.9to100')
CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
assign_income_cell = function(income, weight) {
  ord        = order(income)
  cum_w      = cumsum(weight[ord]) / sum(weight)
  rank_0_100 = numeric(length(income))
  rank_0_100[ord] = 100 * cum_w
  idx = findInterval(rank_0_100, CALIB_INCOME_EDGES,
                     rightmost.closed = TRUE, all.inside = TRUE)
  CALIB_INCOME_BUCKETS[idx]
}

# Production feature list minus age_younger (not a shared variable).
features = c('has_income_act', 'has_income_pos', 'has_negative_income',
             'pctile_income',
             'married', 'age_older', 'n_dep_hh',
             'pctile_wages',         'has_wages',
             'pctile_business',      'has_business_act',
                                     'has_business_pos',
             'pctile_int_div',       'has_int_div',
             'pctile_capital_gains', 'has_capital_gains_act',
                                     'has_capital_gains_pos',
             'pctile_ss_pens',       'has_ss_pens')

make_has = function(x) case_when(x > 0 ~ 1L, x == 0 ~ 0L, TRUE ~ -1L)

# Same engineering as wealth.R lines 239-267 (SCF side) and 398-452 (PUF
# side), applied to the shared income definitions. Percentiles are
# weighted within the table passed in.
engineer = function(df) {
  df %>% mutate(
    n_dep_hh  = n_dep,
    age_older = age_range,
    income = wages + business + int_div + capital_gains + rent + ss_pens +
             ui_other,
    has_income_act        = as.integer(income != 0),
    has_income_pos        = as.integer(income >  0),
    has_negative_income   = as.integer(income <  0),
    pctile_income         = compute_percentile(income, weight, FINE_PCTILE_PROBS),
    has_wages             = make_has(wages),
    pctile_wages          = compute_percentile(wages, weight, FINE_PCTILE_PROBS),
    has_business_act      = as.integer(business != 0),
    has_business_pos      = as.integer(business >  0),
    pctile_business       = compute_percentile(business, weight, FINE_PCTILE_PROBS),
    has_int_div           = make_has(int_div),
    pctile_int_div        = compute_percentile(int_div, weight, FINE_PCTILE_PROBS),
    has_capital_gains_act = as.integer(capital_gains != 0),
    has_capital_gains_pos = as.integer(capital_gains >  0),
    pctile_capital_gains  = compute_percentile(capital_gains, weight, FINE_PCTILE_PROBS),
    has_ss_pens           = make_has(ss_pens),
    pctile_ss_pens        = compute_percentile(ss_pens, weight, FINE_PCTILE_PROBS)
  )
}

#---------------------------------------------------------------------------
# Inputs
#---------------------------------------------------------------------------
train   = as.data.frame(read_parquet(file.path(in_dir, 'train.parquet')))
# D_TARGETS / D_OUT (optional, diagnostics only): complete another target
# file and write the raw draws elsewhere, reusing the cached forests.
targets_path = Sys.getenv('D_TARGETS', file.path(in_dir, 'targets.parquet'))
out_dir = Sys.getenv('D_OUT', comp_dir)
targets = as.data.frame(read_parquet(targets_path))

# The experiment A variants alter only half 1's incomes, so their training
# records (half 2) are the same as expA_clean's. If DRF_CACHE_FROM names a
# dataset whose training input is identical in every value, its fitted
# forests are reused instead of refitting the same forests again.
cache_from = Sys.getenv('DRF_CACHE_FROM')
if (nzchar(cache_from) && cache_from != dataset) {
  other = as.data.frame(read_parquet(file.path(S, 'comp', cache_from,
                                               'drf_input', 'train.parquet')))
  if (isTRUE(all.equal(other, train, check.attributes = FALSE, tolerance = 0))) {
    cache_dir = file.path(S, 'comp', cache_from, 'drf_cache')
    cat(sprintf('d_drf.R: training records identical to %s; reusing its forests\n',
                cache_from))
  } else {
    cat(sprintf('d_drf.R: training records differ from %s; fitting afresh\n',
                cache_from))
  }
}
cat(sprintf('d_drf.R: dataset %s seed %d: %d training rows, %d target rows\n',
            dataset, seed, nrow(train), nrow(targets)))
# train.parquet holds every SCF unit of the training split: the rank base.
# Only rows with in_training (wealth observed) are donors; split-off units
# (wealth unobserved since A's 13:19 change) are kept only so that the
# training records are ranked among all filers, as the targets are.
if (!('in_training' %in% names(train))) train$in_training = TRUE
train$in_training = as.logical(train$in_training)
cat(sprintf('d_drf.R: rank base %d SCF rows, of which %d training records\n',
            nrow(train), sum(train$in_training)))
stopifnot(!anyNA(train[, c('weight', 'married', 'n_dep', 'age_range', income_vars)]))
stopifnot(!anyNA(train[train$in_training, wealth_y_vars]))
stopifnot(!anyNA(targets[, c('weight', 'married', 'n_dep', income_vars)]))

# Pool SCF waves as production pools 2019 and 2022 (pool_scf_donors.R).
train_years = sort(unique(train$year))
base_year   = max(train_years)
reflate = list()
if (length(train_years) > 1L) {
  stopifnot(length(train_years) == 2L)
  old_year = min(train_years)
  is_old = train$year == old_year
  vars = c(wealth_y_vars, income_vars)
  # Wealth totals over training records (split-off units hold zero wealth
  # in production's donor table, so this is the same total); income totals
  # over all units.
  use = function(v) if (v %in% wealth_y_vars) train$in_training else rep(TRUE, nrow(train))
  num = sapply(vars, function(v) { u = use(v) & !is_old; sum(train$weight[u] * train[[v]][u]) })
  den = sapply(vars, function(v) { u = use(v) &  is_old; sum(train$weight[u] * train[[v]][u]) })
  bad = vars[!is.finite(den) | den == 0 | sign(num) != sign(den)]
  if (length(bad) > 0L)
    stop('reflation factor undefined or negative for: ', paste(bad, collapse = ', '))
  gf = num / den
  for (v in vars) train[[v]][is_old] = train[[v]][is_old] * gf[[v]]
  p_target = sum(train$weight[!is_old])
  train$weight[is_old]  = train$weight[is_old]  * (p_target / sum(train$weight[is_old]))  / 2
  train$weight[!is_old] = train$weight[!is_old] * (p_target / sum(train$weight[!is_old])) / 2
  reflate = list(old_year = old_year, base_year = base_year, factors = as.list(gf))
  cat('d_drf.R: reflation factors (old wave to base wave dollars):\n')
  print(round(gf, 3))
}

train = engineer(train)
train$cell = assign_income_cell(train$income, train$weight)
train = train[train$in_training, ]   # donors: wealth observed only

#---------------------------------------------------------------------------
# Per-cell bootstrap + DRF fit (wealth.R lines 296-358)
#---------------------------------------------------------------------------
min_node_size = 50L
n_boot_cell = round(boot_scale * c(
  'pct00to20'    = 100000, 'pct20to40'    = 100000,
  'pct40to60'    = 100000, 'pct60to80'    = 100000,
  'pct80to90'    = 100000, 'pct90to99'    = 100000,
  'pct99to99.9'  = 100000, 'pct99.9to100' = 150000))

extract_forest = function(m) list(
  n_trees = m[['_num_trees']],
  root    = sapply(m[['_root_nodes']], identity),
  child_L = lapply(m[['_child_nodes']], `[[`, 1L),
  child_R = lapply(m[['_child_nodes']], `[[`, 2L),
  split_v = m[['_split_vars']],
  split_s = m[['_split_values']],
  leaves  = m[['_leaf_samples']]
)

boot_list = list(); forest_list = list(); cell_log = list()
for (ci_idx in seq_along(CALIB_INCOME_BUCKETS)) {
  ci = CALIB_INCOME_BUCKETS[ci_idx]
  rows_in_cell = which(train$cell == ci)
  if (length(rows_in_cell) == 0L) stop('no training rows in cell ', ci)
  # D_MODE=sw (diagnostic variant, NOT production): no bootstrap; the forest
  # is fit to the cell's own rows with drf's sample.weights, and the donor is
  # drawn from the leaf with probability proportional to the survey weight.
  sw_mode = Sys.getenv('D_MODE') == 'sw'
  if (sw_mode) {
    boot_rows = rows_in_cell
  } else {
    set.seed(seed * 1000L + 100L + ci_idx)
    boot_in_cell = sample.int(length(rows_in_cell), size = n_boot_cell[[ci]],
                              replace = TRUE, prob = train$weight[rows_in_cell])
    boot_rows = rows_in_cell[boot_in_cell]
  }
  boot_list[[ci]] = boot_rows
  cache_path = file.path(cache_dir, sprintf('drf_%s_seed%02d_boot%g_trees%d%s.rds',
                                            ci, seed, boot_scale, num_trees,
                                            if (sw_mode) '_sw' else ''))
  t0 = Sys.time()
  if (file.exists(cache_path)) {
    fit = readRDS(cache_path)
  } else {
    fit = drf::drf(X = as.matrix(train[boot_rows, features]),
                   Y = as.matrix(train[boot_rows, wealth_y_vars]),
                   sample.weights = if (sw_mode) train$weight[boot_rows] else NULL,
                   num.trees = num_trees, splitting.rule = 'FourierMMD',
                   num.features = 50, mtry = 10L,
                   min.node.size = min_node_size, honesty = TRUE,
                   response.scaling = FALSE, num.threads = n_threads,
                   seed = seed * 1000L + 500L + ci_idx)
    saveRDS(fit, cache_path)
  }
  forest_list[[ci]] = extract_forest(fit)
  rm(fit); gc(verbose = FALSE)
  secs = as.numeric(Sys.time() - t0, units = 'secs')
  cell_log[[ci]] = list(training_rows = length(rows_in_cell),
                        distinct_households = length(unique(train$hh_id[rows_in_cell])),
                        bootstrap = n_boot_cell[[ci]], seconds = round(secs, 1))
  cat(sprintf('d_drf.R: cell %-12s training rows %6d, bootstrap %6d, fit %.0fs\n',
              ci, length(rows_in_cell), n_boot_cell[[ci]], secs))
  flush.console()
}

#---------------------------------------------------------------------------
# Target features, cells and donor draws (wealth.R lines 398-509, 712-722)
# Each target year is ranked within its own records, as production ranks
# the PUF within itself.
#---------------------------------------------------------------------------
# Production zeroes PUF interest and dividends of $100 or less, to match
# the SCF's share reporting them (wealth.R, int_div_puf); done here too.
is_puf = targets$source == 'PUF'
n_intdiv_zeroed = sum(is_puf & targets$int_div > 0 & targets$int_div <= 100)
targets$int_div[is_puf & targets$int_div > 0 & targets$int_div <= 100] = 0
if (any(is_puf)) cat(sprintf('d_drf.R: PUF interest+dividends of $100 or less set to zero for %d records\n', n_intdiv_zeroed))
targets$grp = paste(targets$source, targets$year)
joint_rank = Sys.getenv('D_RANK') == 'joint'   # diagnostic only, not production
tf = lapply(split(seq_len(nrow(targets)), targets$grp), function(ix) {
  if (joint_rank) {
    # Rank targets on the training records' scale: append them with
    # negligible weight so the breakpoints are the training records'.
    cols = c('weight', 'age_range', 'married', 'n_dep', income_vars)
    tt = targets[ix, cols]; tt$weight = tt$weight * 1e-9
    e = engineer(rbind(train[, cols], tt))
    e$cell = assign_income_cell(e$income, e$weight)
    e = e[nrow(train) + seq_along(ix), ]
    d = cbind(targets[ix, setdiff(names(targets), names(e))], e)
    d$weight = targets$weight[ix]
    return(transform(d, .ix = ix))
  }
  d = engineer(targets[ix, ])
  d$cell = assign_income_cell(d$income, d$weight)
  d$.ix = ix
  d
})
tf = bind_rows(tf)
tf = tf[order(tf$.ix), ]
stopifnot(identical(tf$.ix, seq_len(nrow(targets))))

# Targets with no age range (PUF records outside the demographic file).
# Production has an imputed age for every PUF record; here the age range is
# drawn, once per record, from the training records in the same income cell
# and marital status, with probability proportional to the survey weight.
age_na = which(is.na(tf$age_older))
n_age_imputed = length(age_na)
if (n_age_imputed > 0L) {
  set.seed(seed * 1000L + 777L)
  for (ci in CALIB_INCOME_BUCKETS) for (mm in c(0, 1)) {
    ix = age_na[tf$cell[age_na] == ci & tf$married[age_na] == mm]
    if (length(ix) == 0L) next
    pool = which(train$cell == ci & train$married == mm)
    if (length(pool) == 0L) pool = which(train$cell == ci)
    pick = pool[sample.int(length(pool), length(ix), replace = TRUE,
                           prob = train$weight[pool])]
    tf$age_older[ix] = train$age_range[pick]
  }
  stopifnot(!anyNA(tf$age_older))
  cat(sprintf('d_drf.R: age range drawn for %d target records without one\n',
              n_age_imputed))
}
X_t = as.matrix(tf[, features])

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

n_t = nrow(targets)
donor = matrix(NA_integer_, n_t, n_draws)   # row in `train`
leaf_size = matrix(NA_integer_, n_t, n_draws)
n_retry = 0L
for (d in seq_len(n_draws)) {
  set.seed(seed * 1000L + 900L + d)
  u_tree = runif(n_t); u_leaf = runif(n_t)
  for (ci in CALIB_INCOME_BUCKETS) {
    f = forest_list[[ci]]
    rows = which(tf$cell == ci)
    for (i in rows) {
      t = 1L + floor(u_tree[i] * f$n_trees)
      nd = walk_to_leaf(f, t, X_t[i, ])
      lr = f$leaves[[t]][[nd]] + 1L
      tries = 0L
      while (length(lr) == 0L) {           # not expected with honesty pruning
        tries = tries + 1L; n_retry = n_retry + 1L
        if (tries > 50L) stop('50 empty leaves in a row for target ', i)
        t = (t %% f$n_trees) + 1L
        nd = walk_to_leaf(f, t, X_t[i, ])
        lr = f$leaves[[t]][[nd]] + 1L
      }
      if (sw_mode) {
        cw = cumsum(train$weight[boot_list[[ci]][lr]])
        pick = lr[pmin(length(lr), 1L + findInterval(u_leaf[i] * cw[length(cw)], cw))]
      } else {
        pick = lr[pmin(length(lr), 1L + floor(u_leaf[i] * length(lr)))]
      }
      donor[i, d] = boot_list[[ci]][pick]
      leaf_size[i, d] = length(lr)
    }
  }
}
stopifnot(!anyNA(donor))
cat(sprintf('d_drf.R: draws done; empty-leaf retries %d; median leaf size %d\n',
            n_retry, as.integer(median(leaf_size))))

#---------------------------------------------------------------------------
# Output: one row per (target, draw), donor wealth in the target year's
# dollars.
#---------------------------------------------------------------------------
out = do.call(rbind, lapply(seq_len(n_draws), function(d) {
  y = train[donor[, d], wealth_y_vars]
  data.frame(row = targets$row, draw = d - 1L, donor_row = donor[, d],
             donor_hh = train$hh_id[donor[, d]],
             donor_year = train$year[donor[, d]],
             leaf_size = leaf_size[, d], cell = tf$cell,
             target_year = targets$year, y, check.names = FALSE)
}))
if (length(reflate) > 0L) {
  for (v in wealth_y_vars) {
    f = reflate$factors[[v]]
    m_old = out$target_year == reflate$old_year
    out[[v]][m_old] = out[[v]][m_old] / f
    m_mid = out$target_year != reflate$old_year & out$target_year != reflate$base_year
    if (any(m_mid)) {
      expo = (reflate$base_year - out$target_year[m_mid]) /
             (reflate$base_year - reflate$old_year)
      out[[v]][m_mid] = out[[v]][m_mid] * f^(-expo)
    }
  }
}
method_name = if (Sys.getenv('D_MODE') == 'sw') 'drfsw' else 'drf'
write_parquet(out, file.path(out_dir, sprintf('%s_raw_seed%02d.parquet', method_name, seed)))
run = list(dataset = dataset, seed = seed, draws = n_draws,
           features = features, min_node_size = min_node_size,
           mtry = 10L, num_features = 50L, num_trees = num_trees,
           boot_scale = boot_scale, cells = cell_log, reflate = reflate,
           empty_leaf_retries = n_retry, age_range_imputed = n_age_imputed,
           seconds = round(as.numeric(Sys.time() - t_start, units = 'secs')))
write_json(run, file.path(out_dir, sprintf('run_%s_seed%02d.json', method_name, seed)),
           auto_unbox = TRUE, pretty = TRUE, digits = NA)
cat(sprintf('d_drf.R: done in %.0fs\n', run$seconds))
