#--------------------------------------
# test_wealth_X_ablation.R
#
# Per-spec worker for the SCF wealth-
# imputation X-ablation. Run once per
# spec; each invocation trains one DRF
# and writes donor draws to
#   resources/cache/wealth_ablation_<spec>.rds
# A separate postproc script
# (src/eda/test_wealth_X_ablation_postproc.R)
# reads the three outputs and produces
# the diagnostic tables + plots.
#
# Usage:
#   Rscript src/eda/test_wealth_X_ablation.R <spec>
#   where <spec> ∈ {small, medium, large}
#
# Spec menu:
#   small  — demographics + pooled-income
#            rank only (production-as-of-
#            this-commit, minus male1).
#   medium — small + wealth-informative
#            composition triad (wages,
#            business, int_div) as
#            within-dataset percentile
#            plus has-flag.
#   large  — small + all 7 SCFP↔PUF
#            candidates from
#            src/eda/scf_puf_coverage.R
#            (wages, business, int_div,
#            capital_gains, rent, ss_pens,
#            ui_other), each as pctile +
#            has-flag.
#
# Fixed across specs:
#   n_boot         = 250,000
#   num.trees      = 500
#   min.node.size  = 20
#   honesty        = TRUE
#   splitting.rule = FourierMMD
#   bootstrap seed = 1337 (identical
#     boot_idx across spec jobs)
#
# Varies with spec:
#   features set
#   mtry = ncol(X)  (DRF default; an
#     artifact of X-dim, not a tuning
#     knob — reported in the output log)
#--------------------------------------

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1)
  stop('Usage: Rscript test_wealth_X_ablation.R <spec>  (small|medium|large)')
valid_specs = c('small', 'medium', 'large', 'large_no_intdiv',
                'small_plus_wages', 'small_plus_biz', 'small_plus_intdiv',
                'small_plus_cg', 'small_plus_sspens',
                paste0('ladder', 1:10),
                'ladder10_no_ndep', 'large_no_ndep')
SPEC = args[1]
if (!(SPEC %in% valid_specs))
  stop('Unknown spec: ', SPEC,
       '. Choose one of: ', paste(valid_specs, collapse = ', '), '.')
cat(sprintf('\n====== test_wealth_X_ablation — SPEC = %s ======\n\n', SPEC))

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')
estimate_models = 1
do_lp = 0

# Seeds. TRAIN_SEED controls bootstrap row selection and DRF fit. DONOR_SEED
# controls per-PUF-row donor sampling after the DRF is fit. Separated so we
# can measure training MC variance vs donor-sampling MC variance
# independently. Both default to 1337 to preserve legacy behaviour.
TRAIN_SEED = as.integer(Sys.getenv('TRAIN_SEED', unset = '1337'))
DONOR_SEED = as.integer(Sys.getenv('DONOR_SEED', unset = '1337'))
cat(sprintf('seeds: TRAIN_SEED=%d, DONOR_SEED=%d\n', TRAIN_SEED, DONOR_SEED))
set.seed(TRAIN_SEED)

#---------------------------------------------------------------------------
# Minimal pipeline up to demographics + ages (matches test_wealth.R).
#---------------------------------------------------------------------------

source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
source('./src/impute_nonfilers.R')

source('./src/imputations/helpers.R')
source('./src/imputations/demographics.R')
source('./src/imputations/ages.R')

# Stage 1 — uses cache if available; rebuilds if raw SCF is newer.
source('./src/imputations/stage1_scf_tax_units.R')

stopifnot(exists('scf_tax_units'))
stopifnot(all(c('wages_scf', 'business_scf', 'int_div_scf',
                'capital_gains_scf', 'rent_scf',
                'ss_pens_scf', 'ui_other_scf') %in% names(scf_tax_units)))

#---------------------------------------------------------------------------
# Y schema — copied verbatim from src/imputations/wealth.R.
#---------------------------------------------------------------------------

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

scf_to_y = function(df) {
  if (all(wealth_y_vars %in% names(df))) return(df)
  df %>% mutate(
    cash             = LIQ + CDS,
    equities         = STOCKS + STMUTF + COMUTF,
    bonds            = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    retirement       = IRAKH + THRIFT + FUTPEN + CURRPEN,
    life_ins         = CASHLI,
    annuities        = ANNUIT,
    trusts           = TRUSTS,
    other_fin        = OTHFIN + OMUTF,
    pass_throughs    = BUS,
    primary_home     = HOUSES,
    other_home       = ORESRE,
    re_fund          = NNRESRE,
    other_nonfin     = VEHIC + OTHNFIN,
    primary_mortgage = MRTHEL,
    other_mortgage   = RESDBT,
    credit_lines     = OTHLOC,
    credit_cards     = CCBAL,
    installment_debt = INSTALL,
    other_debt       = ODEBT,
    kg_primary_home  = KGHOUSE,
    kg_other_re      = KGORE,
    kg_pass_throughs = KGBUS,
    kg_other         = KGSTMF
  )
}

#---------------------------------------------------------------------------
# Feature engineering: composition pctile + has-flag on BOTH sides.
#
# has_* mirrors the production has_income coding (1/0/-1 for pos/zero/neg).
# pctile_* uses compute_percentile, which returns 0 for non-positive and
# NA and 1..100 on positives. The pair redundantly encodes the zero mass
# so DRF can split on either.
#---------------------------------------------------------------------------

make_has = function(x) {
  case_when(x >  0 ~ 1L,
            x == 0 ~ 0L,
            TRUE   ~ -1L)
}

# --- SCF side ---
# has_income / pctile_income are not in the stage1 transmute, so compute them
# fresh here using the same definitions as src/imputations/wealth.R.
scf_tax_units = scf_tax_units %>%
  mutate(
    # SCF n_dep is already household-kids-only (KIDS_u from roster, per
    # stage1_scf_tax_units.R:496). Alias to n_dep_hh to match the PUF side's
    # kids-only reconstruction below. This aligns the dependent-count
    # concept across datasets — the raw PUF n_dep (= XOCAH + XOCAWH +
    # XOPAR + XOODEP) includes adult parents and other adult dependents
    # that SCF's roster-based KIDS does not capture, producing a huge
    # density mismatch at 55-64 and 65+ households that drives the
    # aggregate wealth gap.
    n_dep_hh = n_dep,
    # Cap ages at 80 (PUF topcode). Then recode as older/younger to remove
    # the PUF primary-filer vs SCF respondent arbitrariness — whichever
    # spouse is "age1" vs "age2" is a labeling accident and DRF shouldn't
    # learn patterns that depend on it. Singles get age_younger = 0 as a
    # sentinel (cleanly distinguishes from married via a single split).
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped),
                          age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped),
                          0L),
    # Unfloor SCF income: stage1's `income` column inherits SCFP's published
    # INCOME (which floors sum-of-components at 0). Reconstruct from the
    # carried-through component columns so negatives (~0.2% of SCF tax units
    # with net business/cap-gains losses exceeding other income) come
    # through, matching PUF's ~1% negative after the income-expression fix.
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    # Dual-binary for income: has_income_act separates {=0} from {≠0}; pos
    # distinguishes negatives from positives within activity. Parallel to
    # has_business encoding.
    has_income_act  = as.integer(income != 0),
    has_income_pos  = as.integer(income >  0),
    pctile_income   = compute_percentile(income, weight),
    has_wages         = make_has(wages_scf),
    has_business      = make_has(business_scf),
    has_int_div       = make_has(int_div_scf),
    has_capital_gains = make_has(capital_gains_scf),
    has_rent          = make_has(rent_scf),
    has_ss_pens       = make_has(ss_pens_scf),
    has_ui_other      = make_has(ui_other_scf),
    # Dual-binary for business + capital_gains (were in large_binv2 only;
    # now available across specs).
    has_business_act      = as.integer(business_scf      != 0),
    has_business_pos      = as.integer(business_scf      >  0),
    has_capital_gains_act = as.integer(capital_gains_scf != 0),
    has_capital_gains_pos = as.integer(capital_gains_scf >  0),
    pctile_wages         = compute_percentile(wages_scf,         weight),
    pctile_business      = compute_percentile(business_scf,      weight),
    pctile_int_div       = compute_percentile(int_div_scf,       weight),
    pctile_capital_gains = compute_percentile(capital_gains_scf, weight),
    pctile_rent          = compute_percentile(rent_scf,          weight),
    pctile_ss_pens       = compute_percentile(ss_pens_scf,       weight),
    pctile_ui_other      = compute_percentile(ui_other_scf,      weight)
  )

# --- PUF side ---
# Confirm the raw inputs are present & non-NA at this pipeline stage.
# Filers get them from process_puf (raw PUF codes); nonfilers get 0-fill
# from the remaining_vars loop in src/impute_nonfilers.R:135-142 for
# anything not set explicitly in the DINA summarise. If a NA ever slips
# through, this stopifnot will flag the cause (per CLAUDE.md: never drop
# NAs silently).
puf_raw_inputs = c(
  'wages', 'sole_prop', 'farm',
  'scorp_active', 'scorp_active_loss', 'scorp_179',
  'scorp_passive', 'scorp_passive_loss',
  'part_active', 'part_active_loss', 'part_179',
  'part_passive', 'part_passive_loss',
  'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
  'kg_lt', 'kg_st',
  'rent', 'rent_loss', 'estate', 'estate_loss',
  'gross_ss', 'gross_pens_dist', 'ui'
)
missing_cols = setdiff(puf_raw_inputs, names(tax_units))
if (length(missing_cols) > 0)
  stop('Missing PUF composition inputs on tax_units: ',
       paste(missing_cols, collapse = ', '))
na_report = sapply(puf_raw_inputs, function(v) sum(is.na(tax_units[[v]])))
if (any(na_report > 0)) {
  cat('NA counts by column:\n'); print(na_report[na_report > 0])
  stop('Unexpected NAs in PUF composition inputs. Investigate before ',
       'proceeding — do not silently coalesce.')
}

# Income concept: mirror SCFP INCOME =
#   WAGEINC + BUSSEFARMINC + INTDIVINC + KGINC + SSRETINC + TRANSFOTHINC
#   + RENTINC
# so PUF rank-on-income is computed on the same concept the DRF's SCF side
# is using (stage1's `income = INCOME`). This adds farm, kg_lt + kg_st,
# estate - estate_loss, exempt_int, and ui relative to the prior wealth.R /
# consumption.R income expression — which is what lets PUF income go
# legitimately negative for filers with large realized capital losses or
# farm losses, matching SCF's ~5% non-positive share.
# Business concept (mirrors SCFP BUSSEFARMINC): sole_prop + farm +
#   active/passive partnership (net of loss, Section 179) +
#   active/passive S-corp (net of loss, Section 179).
# Rent concept (mirrors SCFP RENTINC): rent/royalty (net) + estate/trust
#   (net). variable_guide lists rent as "rent/royalty" so royalty is
#   already bundled on the PUF side.
tax_units = tax_units %>%
  mutate(
    # Household-kids-only dependent count. The raw PUF n_dep (from
    # process_puf.R:126) = XOCAH + XOCAWH + XOPAR + XOODEP — includes
    # adult parents (XOPAR) and other adult dependents (XOODEP) that
    # SCF's household-roster KIDS does not capture. Reconstruct by
    # counting dep_age1/2/3 slots with age < 18, matching the pattern
    # in src/imputations/mortgage.R:78-80 and similar.
    n_dep_hh = (!is.na(dep_age1) & dep_age1 < 18) +
               (!is.na(dep_age2) & dep_age2 < 18) +
               (!is.na(dep_age3) & dep_age3 < 18),
    # Age harmonization + labeling-invariant recoding (see SCF-side note).
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped),
                          pmax(age1_capped, age2_capped),
                          age1_capped),
    age_younger = if_else(!is.na(age2_capped),
                          pmin(age1_capped, age2_capped),
                          0L),
    married = as.integer(!is.na(male2)),
    income  = wages +
              # BUSSEFARMINC
              sole_prop + farm +
              scorp_active - scorp_active_loss - scorp_179 +
              scorp_passive - scorp_passive_loss +
              part_active - part_active_loss - part_179 +
              part_passive - part_passive_loss +
              # INTDIVINC
              txbl_int + exempt_int + div_ord + div_pref +
              # KGINC
              kg_lt + kg_st +
              # SSRETINC
              gross_ss + gross_pens_dist +
              # TRANSFOTHINC (PUF only has UI on this bundle)
              ui +
              # RENTINC
              rent - rent_loss + estate - estate_loss,
    has_income    = make_has(income),
    pctile_income = compute_percentile(income, weight),
    wages_puf         = wages,
    business_puf      = sole_prop + farm +
                        scorp_active  - scorp_active_loss  - scorp_179 +
                        scorp_passive - scorp_passive_loss +
                        part_active   - part_active_loss   - part_179 +
                        part_passive  - part_passive_loss,
    int_div_puf       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains_puf = kg_lt + kg_st,
    rent_puf          = rent - rent_loss + estate - estate_loss,
    ss_pens_puf       = gross_ss + gross_pens_dist,
    ui_other_puf      = ui,
    has_wages         = make_has(wages_puf),
    has_business      = make_has(business_puf),
    has_int_div       = make_has(int_div_puf),
    has_capital_gains = make_has(capital_gains_puf),
    has_rent          = make_has(rent_puf),
    has_ss_pens       = make_has(ss_pens_puf),
    has_ui_other      = make_has(ui_other_puf),
    # Dual-binary for income (parallel to SCF side).
    has_income_act    = as.integer(income       != 0),
    has_income_pos    = as.integer(income       >  0),
    # Dual-binary for business / capital_gains.
    has_business_act      = as.integer(business_puf      != 0),
    has_business_pos      = as.integer(business_puf      >  0),
    has_capital_gains_act = as.integer(capital_gains_puf != 0),
    has_capital_gains_pos = as.integer(capital_gains_puf >  0),
    pctile_wages         = compute_percentile(wages_puf,         weight),
    pctile_business      = compute_percentile(business_puf,      weight),
    pctile_int_div       = compute_percentile(int_div_puf,       weight),
    pctile_capital_gains = compute_percentile(capital_gains_puf, weight),
    pctile_rent          = compute_percentile(rent_puf,          weight),
    pctile_ss_pens       = compute_percentile(ss_pens_puf,       weight),
    pctile_ui_other      = compute_percentile(ui_other_puf,      weight)
  )

#---------------------------------------------------------------------------
# Feature specs.
#---------------------------------------------------------------------------

small_feats  = c('has_income_act', 'has_income_pos', 'pctile_income',
                 'married', 'age_older', 'age_younger', 'n_dep_hh')

medium_feats = c(small_feats,
                 'pctile_wages',    'has_wages',
                 'pctile_business', 'has_business_act', 'has_business_pos',
                 'pctile_int_div',  'has_int_div')

large_feats  = c(small_feats,
                 'pctile_wages',         'has_wages',
                 'pctile_business',      'has_business_act',
                                         'has_business_pos',
                 'pctile_int_div',       'has_int_div',
                 'pctile_capital_gains', 'has_capital_gains_act',
                                         'has_capital_gains_pos',
                 'pctile_ss_pens',       'has_ss_pens')
# rent + ui_other dropped on positivity-gap grounds:
#   rent: SCFP cash-basis vs PUF tax-basis — only ~6.4% PUF vs 29.3% SCF
#     tax units with rental activity (depreciation absorbs cash rent on
#     tax returns; indirect holdings route through pass-throughs).
#   ui_other: SCFP TRANSFOTHINC = UI + welfare + alimony + child support +
#     other, PUF ui is Schedule 1 UI only. Positivity 21.7% vs 3.4%.
#
# Encoding note (supersedes the earlier large_binv2 experiment):
# All specs use dual-binary (has_X_act + has_X_pos) for income, business,
# and capital_gains — variables that can go negative — because the 3-level
# ordinal encoding was forcing DRF splits to lump {neg, zero} or {zero, pos}
# and missed the wealth signal in loss-holders. wages / int_div / ss_pens
# can't go negative, so single has_* (ordinal) suffices.

# large_no_intdiv: large spec with pctile_int_div and has_int_div removed,
# used to directly test whether removing the int_div features reduces the
# aggregate gap (user's empirical request).
large_no_intdiv_feats = setdiff(large_feats,
                                c('pctile_int_div', 'has_int_div'))

# Forward-stepwise single-feature-group additions to small, for marginal
# contribution analysis. Each adds one income-composition feature group.
small_plus_wages_feats   = c(small_feats, 'pctile_wages', 'has_wages')
small_plus_biz_feats     = c(small_feats, 'pctile_business',
                             'has_business_act', 'has_business_pos')
small_plus_intdiv_feats  = c(small_feats, 'pctile_int_div', 'has_int_div')
small_plus_cg_feats      = c(small_feats, 'pctile_capital_gains',
                             'has_capital_gains_act', 'has_capital_gains_pos')
small_plus_sspens_feats  = c(small_feats, 'pctile_ss_pens', 'has_ss_pens')

# Ladder: from-the-ground-up stepwise feature build-up. Each rung adds one
# feature group to the previous. ladder10 is identical in feature set to
# the `large` spec.
ladder1  = c('married')
ladder2  = c(ladder1, 'age_older', 'age_younger')
ladder3  = c(ladder2, 'n_dep_hh')
ladder4  = c(ladder3, 'has_income_act', 'has_income_pos')
ladder5  = c(ladder4, 'pctile_income')
ladder6  = c(ladder5, 'pctile_wages', 'has_wages')
ladder7  = c(ladder6, 'pctile_business', 'has_business_act', 'has_business_pos')
ladder8  = c(ladder7, 'pctile_int_div', 'has_int_div')
ladder9  = c(ladder8, 'pctile_capital_gains',
             'has_capital_gains_act', 'has_capital_gains_pos')
ladder10 = c(ladder9, 'pctile_ss_pens', 'has_ss_pens')

# ladder10_no_ndep: same features as ladder10 but with n_dep_hh removed —
# direct test of whether the dependent-count feature is a gap driver after
# it's been properly restricted to household kids only.
ladder10_no_ndep = setdiff(ladder10, 'n_dep_hh')
large_no_ndep    = setdiff(large_feats, 'n_dep_hh')

specs = list(small              = small_feats,
             medium             = medium_feats,
             large              = large_feats,
             large_no_intdiv    = large_no_intdiv_feats,
             small_plus_wages   = small_plus_wages_feats,
             small_plus_biz     = small_plus_biz_feats,
             small_plus_intdiv  = small_plus_intdiv_feats,
             small_plus_cg      = small_plus_cg_feats,
             small_plus_sspens  = small_plus_sspens_feats,
             ladder1  = ladder1,  ladder2  = ladder2,
             ladder3  = ladder3,  ladder4  = ladder4,
             ladder5  = ladder5,  ladder6  = ladder6,
             ladder7  = ladder7,  ladder8  = ladder8,
             ladder9  = ladder9,  ladder10 = ladder10,
             ladder10_no_ndep = ladder10_no_ndep,
             large_no_ndep    = large_no_ndep)
feats = specs[[SPEC]]

cat(sprintf('spec %-7s: %2d feats — %s\n\n',
            SPEC, length(feats), paste(feats, collapse = ', ')))

#---------------------------------------------------------------------------
# Bootstrap sample of SCF. Seed = 1337 (fixed at script start) and identical
# across all three spec jobs, so every job draws the same boot_idx and
# therefore trains on the same 250k row selection.
#---------------------------------------------------------------------------

scf_mapped = scf_to_y(scf_tax_units)

n_boot     = 250000L
boot_probs = scf_mapped$weight / sum(scf_mapped$weight)
boot_idx   = sample.int(nrow(scf_mapped), size = n_boot,
                        replace = TRUE, prob = boot_probs)
scf_boot   = scf_mapped[boot_idx, ]
Y_mat      = as.matrix(scf_boot[wealth_y_vars])

cat(sprintf('bootstrap: %d rows from %d SCF tax units; seed = 1337\n',
            nrow(scf_boot), nrow(scf_mapped)))

stopifnot(all(feats %in% names(scf_boot)))
stopifnot(all(feats %in% names(tax_units)))

X_mat = as.matrix(scf_boot[feats])

#---------------------------------------------------------------------------
# walk_to_leaf (copied from wealth.R). Closures over the fitted forest.
#---------------------------------------------------------------------------

walk_to_leaf_factory = function(fit) {
  n_trees = fit[['_num_trees']]
  root    = sapply(fit[['_root_nodes']], identity)
  child_L = lapply(fit[['_child_nodes']], `[[`, 1L)
  child_R = lapply(fit[['_child_nodes']], `[[`, 2L)
  split_v = fit[['_split_vars']]
  split_s = fit[['_split_values']]
  leaves  = fit[['_leaf_samples']]

  walk = function(t, x_row) {
    L  = child_L[[t]]; R  = child_R[[t]]
    v  = split_v[[t]]; s  = split_s[[t]]
    ll = leaves [[t]]
    node = root[t] + 1L
    repeat {
      if (length(ll[[node]]) > 0L) return(node)
      xv = x_row[v[node] + 1L]; sv = s[node]
      node = if (is.na(xv) || is.na(sv) || xv <= sv) L[node] + 1L
             else R[node] + 1L
    }
  }
  list(walk = walk, leaves = leaves, n_trees = n_trees)
}

#---------------------------------------------------------------------------
# Train DRF for this spec, draw donors, save.
#---------------------------------------------------------------------------

DRF_NUM_TREES     = 500L
DRF_MIN_NODE_SIZE = 20L
DRF_HONESTY       = TRUE
# num.features = dimension of the random Fourier basis for FourierMMD
# approximation. drf default is 10; higher reduces approximation variance
# at ~1/√D rate. Expose via env var so we can sweep it.
DRF_NUM_FEATURES  = as.integer(Sys.getenv('NUM_FEATURES', unset = '10'))

cat(sprintf('\ntraining DRF: %d trees, min.node.size = %d, honesty = %s, mtry = %d, num.features = %d\n',
            DRF_NUM_TREES, DRF_MIN_NODE_SIZE, DRF_HONESTY,
            ncol(X_mat), DRF_NUM_FEATURES))

t_fit = Sys.time()
# Cache tag includes TRAIN_SEED and NUM_FEATURES so sweeps don't collide.
fit = train_or_load_drf(
  name          = paste0('wealth_drf_ablation_', SPEC, '_t', TRAIN_SEED,
                         '_f', DRF_NUM_FEATURES),
  X             = X_mat,
  Y             = Y_mat,
  num.trees     = DRF_NUM_TREES,
  min.node.size = DRF_MIN_NODE_SIZE,
  honesty       = DRF_HONESTY,
  num.features  = DRF_NUM_FEATURES
)
fit_min = as.numeric(Sys.time() - t_fit, units = 'mins')
cat(sprintf('  fit time: %.2f min\n', fit_min))

wtl = walk_to_leaf_factory(fit)

# Reset seed for donor sampling so training-seed variance and donor-seed
# variance can be measured independently.
set.seed(DONOR_SEED)

X_puf     = as.matrix(tax_units[, feats])
n_pred    = nrow(X_puf)
tree_pick = sample.int(wtl$n_trees, size = n_pred, replace = TRUE)
donors    = integer(n_pred)
leaf_size = integer(n_pred)

t_samp = Sys.time()
for (i in seq_len(n_pred)) {
  tr       = tree_pick[i]
  nd       = wtl$walk(tr, X_puf[i, ])
  lr       = wtl$leaves[[tr]][[nd]] + 1L
  leaf_size[i] = length(lr)
  donors[i]    = as.integer(lr[sample.int(length(lr), 1L)])
}
samp_sec = as.numeric(Sys.time() - t_samp, units = 'secs')
cat(sprintf('  donor-sampling time: %.1f s\n', samp_sec))
cat(sprintf('  leaf size: min=%d p10=%d p50=%d p90=%d max=%d\n',
            min(leaf_size),
            as.integer(quantile(leaf_size, 0.10)),
            as.integer(quantile(leaf_size, 0.50)),
            as.integer(quantile(leaf_size, 0.90)),
            max(leaf_size)))

donor_y = Y_mat[donors, , drop = FALSE]
colnames(donor_y) = wealth_y_vars

#---------------------------------------------------------------------------
# Save per-spec artifact. Postproc will merge by `id` across the three.
#---------------------------------------------------------------------------

out_path = if (TRAIN_SEED == 1337L && DONOR_SEED == 1337L && DRF_NUM_FEATURES == 10L) {
  # Default knobs preserve the legacy canonical cache path so downstream
  # postproc / diagnostic scripts keep working without changes.
  sprintf('resources/cache/wealth_ablation_%s.rds', SPEC)
} else {
  sprintf('resources/cache/wealth_ablation_%s_t%d_d%d_f%d.rds',
          SPEC, TRAIN_SEED, DONOR_SEED, DRF_NUM_FEATURES)
}
write_rds(
  list(
    spec        = SPEC,
    feats       = feats,
    donor_y     = donor_y,
    leaf_size   = leaf_size,
    id          = tax_units$id,
    n_feats     = length(feats),
    knobs       = list(n_boot = n_boot,
                       num.trees = DRF_NUM_TREES,
                       min.node.size = DRF_MIN_NODE_SIZE,
                       honesty = DRF_HONESTY,
                       train_seed = TRAIN_SEED,
                       donor_seed = DONOR_SEED),
    fit_minutes = fit_min,
    samp_sec    = samp_sec,
    boot_idx    = boot_idx
  ),
  out_path
)
cat(sprintf('\nwrote %s  (donor_y: %d × %d, leaf_size: %d)\n',
            out_path, nrow(donor_y), ncol(donor_y), length(leaf_size)))
cat('Done.\n')
