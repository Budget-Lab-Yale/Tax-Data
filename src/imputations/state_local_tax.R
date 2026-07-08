#--------------------------------------
# state_local_tax.R
#
# Imputes a combined state + local tax
# RATE onto each PUF tax unit from DINA
# 2019, using a ranger quantile forest.
#--------------------------------------

# Rationale ----------------------------------------------------------------
# Downstream (Tax-Simulator) reports effective average tax rates by income
# group for federal taxes only. To let those curves absorb the more
# regressive state & local pieces, we borrow the joint distribution of
#   ditas      = state personal income tax
#   salestax   = sales + excise taxes (ALL levels; includes ~18% federal
#                excise + customs that DINA does not separate out)
#   proprestax = residential property tax
# from DINA (Piketty-Saez-Zucman) and impute a single combined rate
#   r = (ditas + salestax + proprestax) / income
# onto the PUF by rank-matching on income and demographics. r is scale-free
# and stored as a held-constant column (grow_with = NA); Tax-Simulator
# multiplies it by whatever income concept it uses ("ratio-to-income" aging).
#
# This is deliberately low-precision (used only to shift group ATRs):
#   - DINA is national -> the rate is a national average conditional on
#     income & demographics; there is no state variation (PUF has no state).
#   - PUF and DINA income definitions differ; rank-matching neutralizes the
#     level gap (same assumption as the CEX consumption imputation).
#   - Denominator is broad income incl. KG, SS and UI (the PUF's
#     compute_broad_income; DINA fiinc + SS + UI); document this so
#     downstream applies the rate to the same income concept.

# Feature set (identical construction on DINA train and PUF predict sides):
#   pctile_income  income rank, finer than 1% at the very top
#   has_income     sign of income {-1, 0, 1}
#   married        joint return (DINA `married` / PUF filing_status == 2)
#   filer          filer flag (covers appended DINA non-filers)
#   age_bucket     DINA's 3 age bins: <=44 / 45-64 / 65+  ->  1 / 2 / 3
#   n_dep          # dependent children, capped at 3 to match DINA xkidspop
features = c('pctile_income', 'has_income', 'married', 'filer', 'age_bucket', 'n_dep')

# Income-rank cutpoints: 1% bins through the 99th, then 99.5 and 99.9. Finer
# than plain percentiles at the top (to catch the state-income progressivity
# that keeps climbing into the top 0.1%), but not so fine that DINA's thin
# top (~47k tax units total) leaves empty cells. Coarser than FINE_PCTILE_PROBS.
SALT_PCTILE_PROBS = c(seq(0.01, 0.99, 0.01), 0.995, 0.999)

#---------------------------------------------------------------------------
# Build DINA 2019 tax-unit training frame
#---------------------------------------------------------------------------

# DINA is an adult-level file; `id` identifies the tax unit. Aggregate to the
# tax unit, respecting how each variable is stored (established in
# src/eda/dina_salt_aggregation.R and validate_salt_impute.R):
#   ditas, salestax  split per spouse (vary within id)            -> SUM
#   fiinc            fiscal income incl. KG, per-adult            -> SUM
#   ss, ui           split EQUALLY between spouses (0 within-id    -> SUM
#                    variation, but sum matches OASDI/UI totals)
#   proprestax       replicated FULL on each spouse               -> FIRST
#                    (summing would double-count married units ~2x)
# Income denominator is aligned to the PUF's compute_broad_income: DINA fiinc
# (wages+pensions+business+rent+int+div+KG) omits Social Security and UI,
# which compute_broad_income includes (gross_ss + ui), so we add them here.
# Otherwise rate = SALT/fiinc applied to broad income would overstate SALT
# for SS-heavy retirees. Weight is the tax-unit weight (dweghttaxu / 1e5).
dina = file.path(interface_paths$DINA, 'usdina2019.dta') %>%
  read_dta()

dina_tu = dina %>%
  group_by(id) %>%
  summarise(
    weight     = mean(dweghttaxu) / 1e5,
    filer      = max(filer),
    married    = max(married),
    ageprim    = max(ageprim),
    xkidspop   = mean(xkidspop),
    ditas      = sum(ditas),
    salestax   = sum(salestax),
    proprestax = first(proprestax),
    fiinc_only = sum(fiinc),
    ss         = sum(ssinc_oa + ssinc_di),
    ui         = sum(uiinc),
    .groups    = 'drop'
  ) %>%
  mutate(
    income     = fiinc_only + ss + ui,
    salt_total = ditas + salestax + proprestax,
    # Feature construction — must mirror the PUF side below exactly
    pctile_income = compute_percentile(income, weight, SALT_PCTILE_PROBS),
    has_income = case_when(income >  0 ~  1L,
                           income == 0 ~  0L,
                           T           ~ -1L),
    married    = as.integer(married),
    filer      = as.integer(filer),
    age_bucket = case_when(ageprim <= 44 ~ 1L,
                           ageprim <= 64 ~ 2L,
                           T             ~ 3L),
    n_dep      = pmin(as.integer(xkidspop), 3L)
  )

stopifnot(!any(is.na(dina_tu[c(features, 'salt_total', 'income', 'weight')])))

# The rate is only defined where income is positive; ~3% of units have
# income <= 0 (business losses, zero-fiscal-income) and are dropped from
# TRAINING (they still receive a predicted rate on the PUF side via their
# bottom rank + sign flag).
dina_train = dina_tu %>%
  filter(income > 0) %>%
  # Floor at 0: a few near-zero-income units net a state refund (ditas < 0)
  # larger than their sales+property tax, giving a negative rate. Left in the
  # leaves, these negative draws pull imputed rates below 0 for low-income PUF
  # units routed there. A negative combined burden rate isn't meaningful, so
  # clip it. Negatives are rare and small, so the group gradient is unaffected.
  mutate(rate = pmax(salt_total / income, 0))

# Winsorize the top of the training rate. Near-zero-income units produce
# enormous ratios (sales/property tax >> tiny fiscal income); left uncapped, a
# single such record in a leaf would inflate predictions for every PUF unit
# routed there. Cap at the weighted 99.5th percentile of positive rates. The
# bottom DINA deciles legitimately show high rates (~30% of income), so this
# only clips the extreme individual outliers, not the group gradient.
rate_cap = wtd.quantile(dina_train$rate[dina_train$rate > 0],
                        dina_train$weight[dina_train$rate > 0], 0.995)
n_capped = sum(dina_train$rate > rate_cap)
dina_train = dina_train %>% mutate(rate = pmin(rate, rate_cap))

cat(sprintf(paste0('state_local_tax.R: %d DINA tax units (%d with income>0 for ',
                   'training); combined SALT $%.0fB; rate cap = %.2f (clipped %d recs)\n'),
            nrow(dina_tu), nrow(dina_train),
            sum(dina_tu$salt_total * dina_tu$weight) / 1e9,
            rate_cap, n_capped))

#---------------------------------------------------------------------------
# Train the quantile forest: rate | X
#---------------------------------------------------------------------------

salt_rate_rf = train_or_load_ranger(
  name          = 'state_local_tax_qrf',
  formula       = rate ~ .,
  data          = dina_train[c(features, 'rate')],
  case_weights  = dina_train$weight,
  mtry          = length(features),
  min_node_size = 20
)

#---------------------------------------------------------------------------
# Predict a stochastic rate onto every PUF tax unit
#---------------------------------------------------------------------------

# Broad income incl. KG, matched to DINA fiinc (compute_broad_income is the
# pipeline-canonical concept; all components are PUF-native and non-NA here,
# 0-filled for appended non-filers by impute_nonfilers.R). Compute in a
# standalone step to avoid the magrittr `.`-in-mutate ambiguity.
puf = tax_units
puf$income = compute_broad_income(puf)
puf = puf %>%
  mutate(
    pctile_income = compute_percentile(income, weight, SALT_PCTILE_PROBS),
    has_income    = case_when(income >  0 ~  1L,
                              income == 0 ~  0L,
                              T           ~ -1L),
    married       = as.integer(filing_status == 2),
    filer         = as.integer(filer),
    age_bucket    = case_when(age1 <= 44 ~ 1L,
                              age1 <= 64 ~ 2L,
                              T          ~ 3L),
    n_dep         = pmin(as.integer(n_dep), 3L)
  ) %>%
  select(id, weight, all_of(features))

stopifnot(!any(is.na(puf[features])))

# One random conditional-quantile draw per unit (draw-from-conditional, like
# consumption.R). Floor at 0: DINA `ditas` can be negative (net state refunds),
# so a few training rates are < 0; a negative combined state+local burden rate
# is not meaningful for the ATR use, so clip it away.
puf$dina_other_taxes_rate = pmax(predict_ranger_draw(salt_rate_rf, puf[features]), 0)

cat(sprintf(paste0('state_local_tax.R: imputed rate onto %d PUF units; ',
                   'weighted mean rate = %.3f, median = %.3f\n'),
            nrow(puf),
            weighted.mean(puf$dina_other_taxes_rate, puf$weight),
            median(puf$dina_other_taxes_rate)))

tax_units %<>% left_join(puf %>% select(id, dina_other_taxes_rate), by = 'id')

# Save diagnostics for the validation script (only during test runs)
if (exists('save_salt_diagnostics') && save_salt_diagnostics) {
  write_rds(list(dina_tu = dina_tu, rate_cap = rate_cap,
                 features = features, probs = SALT_PCTILE_PROBS),
            'resources/cache/state_local_tax_diagnostics.rds')
}

rm(dina, dina_tu, dina_train, salt_rate_rf, puf, rate_cap, n_capped,
   features, SALT_PCTILE_PROBS)
