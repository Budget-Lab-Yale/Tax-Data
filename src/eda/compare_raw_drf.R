#---------------------------------------------
# compare_raw_drf.R
#
# Three-column comparison: SCF | raw_DRF (old PUF) |
# raw_DRF (new PUF, post age fix). Same Tables 1-4
# as report_v3.R but stripped to these three sources.
#
# Usage (sbatch):
#   Rscript src/eda/compare_raw_drf.R <new_output_dir> <old_diag_rds>
# Defaults: new = current 2026042712 baseline; old = v3 saved RDS.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
new_dir  = if (length(args) >= 1L) args[1] else
  '/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026042712/baseline'
old_rds  = if (length(args) >= 2L) args[2] else
  '/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline/wealth_harness_tilt_diag_v3.rds'

source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
source('src/imputations/helpers.R')

scf_to_y_local = function(df) {
  if (all(wealth_y_vars %in% names(df))) return(df)
  df %>% mutate(
    cash             = LIQ + CDS,
    equities         = STOCKS + STMUTF + COMUTF,
    bonds            = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    retirement       = IRAKH + THRIFT + FUTPEN + CURRPEN,
    life_ins         = CASHLI, annuities = ANNUIT, trusts = TRUSTS,
    other_fin        = OTHFIN + OMUTF, pass_throughs = BUS,
    primary_home     = HOUSES, other_home = ORESRE, re_fund = NNRESRE,
    other_nonfin     = VEHIC + OTHNFIN,
    primary_mortgage = MRTHEL, other_mortgage = RESDBT,
    credit_lines     = OTHLOC, credit_cards = CCBAL,
    installment_debt = INSTALL, other_debt = ODEBT,
    kg_primary_home  = KGHOUSE, kg_other_re = KGORE,
    kg_pass_throughs = KGBUS, kg_other = KGSTMF
  )
}

#--- Load -----------------------------------------------------------------

cat(sprintf('Loading SCF...\n'))
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_age_older = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_y    = scf_to_y_local(scf_tax_units)
scf_cv   = compute_category_values(scf_y)
scf = tibble(weight = scf_y$weight, age_older = scf_age_older, income = scf_inc)
for (cn in colnames(scf_cv)) scf[[cn]] = scf_cv[[cn]]


# --- raw_DRF OLD --------------------------------------------------------
cat(sprintf('Loading OLD raw_DRF: %s\n', old_rds))
old_diag = readRDS(old_rds)
stopifnot(!is.null(old_diag$y_pre_tilt))
# Old PUF X is in the old baseline dir. Read tax_units_2022.csv there for
# weight/age/income.
old_puf_dir = sub('wealth_harness_tilt_diag.*\\.rds$', '', old_rds)
# fallback: derive from path prefix
old_csv = file.path(dirname(old_rds), 'tax_units_2022.csv')
if (!file.exists(old_csv)) {
  cand = list.files(dirname(old_rds), 'tax_units_2022.csv', full.names = TRUE)
  if (length(cand) > 0L) old_csv = cand[1]
}
cat(sprintf('  reading old PUF X from %s\n', old_csv))
old_puf = read_csv(old_csv, show_col_types = FALSE)
puf_age2 = ifelse(is.na(old_puf$age2), 0L, old_puf$age2)
old_age_older = pmax(pmin(80L, old_puf$age1), pmin(80L, puf_age2))
old_inc = with(old_puf,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)
old_meta = tibble(id = old_puf$id, weight = old_puf$weight,
                  age_older = old_age_older, income = old_inc,
                  dep_status = old_puf$dep_status) %>%
  filter(dep_status == 0L)
old_y  = old_diag$y_pre_tilt %>% inner_join(old_meta %>% select(id), by = 'id')
old_cv = compute_category_values(old_y)
raw_old = old_meta %>% inner_join(old_y %>% select(id), by = 'id')
for (cn in colnames(old_cv)) raw_old[[cn]] = old_cv[[cn]]


# --- raw_DRF NEW --------------------------------------------------------
new_rds = file.path(new_dir, 'diagnose_age_income.rds')
cat(sprintf('Loading NEW raw_DRF: %s\n', new_rds))
new_diag = readRDS(new_rds)
new_y = new_diag$result$y_pre_tilt
# New PUF X — read from puf_2022_snapshot.rds
new_snap = read_rds(file.path(new_dir, 'puf_2022_snapshot.rds'))
new_age2 = ifelse(is.na(new_snap$age2), 0L, new_snap$age2)
new_age_older = pmax(pmin(80L, new_snap$age1), pmin(80L, new_age2))
new_inc = with(new_snap,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)
new_meta = tibble(id = new_snap$id, weight = new_snap$weight,
                  age_older = new_age_older, income = new_inc,
                  dep_status = new_snap$dep_status) %>%
  filter(dep_status == 0L)
new_y2 = new_y %>% inner_join(new_meta %>% select(id), by = 'id')
new_cv = compute_category_values(new_y2)
raw_new = new_meta %>% inner_join(new_y2 %>% select(id), by = 'id')
for (cn in colnames(new_cv)) raw_new[[cn]] = new_cv[[cn]]


cat(sprintf('\nSCF:        %d rows  weighted %.1fM\n',
            nrow(scf), sum(scf$weight) / 1e6))
cat(sprintf('raw_DRF old: %d rows  weighted %.1fM\n',
            nrow(raw_old), sum(raw_old$weight) / 1e6))
cat(sprintf('raw_DRF new: %d rows  weighted %.1fM\n',
            nrow(raw_new), sum(raw_new$weight) / 1e6))


#--- Helpers --------------------------------------------------------------

weighted_quantile = function(x, w, probs) {
  o = order(x); x = x[o]; w = w[o]
  p = cumsum(w) / sum(w); approx(p, x, xout = probs, rule = 2)$y
}

income_bin = function(income, weight) {
  pct = compute_percentile(income, weight)
  cut(pct, breaks = c(-Inf, 20, 40, 60, 80, 90, 99, 99.9, Inf),
      labels = c('p0-20','p20-40','p40-60','p60-80','p80-90',
                 'p90-99','p99-99.9','top0.1%'),
      include.lowest = TRUE, right = FALSE)
}

age_bin = function(age) {
  cut(age, breaks = c(-Inf, 24, 34, 44, 54, 64, 74, Inf),
      labels = c('18-24','25-34','35-44','45-54','55-64','65-74','75+'),
      include.lowest = TRUE, right = TRUE)
}

wealth_bin = function(nw, weight) {
  o = order(nw); cw = cumsum(weight[o]) / sum(weight)
  pct = numeric(length(nw)); pct[o] = 100 * cw
  cut(pct, breaks = c(-Inf, 50, 90, 99, 99.9, Inf),
      labels = c('bot50','p50-90','p90-99','p99-99.9','top0.1%'),
      include.lowest = TRUE, right = TRUE)
}

fmt_T = function(x) sprintf('$%.2fT', x / 1e12)
fmt_M = function(x) sprintf('$%.2fM', x / 1e6)

frames = list(SCF = scf, raw_DRF_old = raw_old, raw_DRF_new = raw_new)

#--- Table 1 — NW $ by income percentile -----------------------------------

cat('\n=== Table 1 — Net worth ($T) by income percentile ===\n')
t1 = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  f$bin = income_bin(f$income, f$weight)
  f %>% group_by(bin) %>%
    summarise(nw = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(source = src)
})) %>%
  pivot_wider(names_from = source, values_from = nw) %>%
  bind_rows(tibble(
    bin = factor('TOTAL', levels = c(levels(.$bin), 'TOTAL')),
    SCF         = sum(.$SCF, na.rm = TRUE),
    raw_DRF_old = sum(.$raw_DRF_old, na.rm = TRUE),
    raw_DRF_new = sum(.$raw_DRF_new, na.rm = TRUE)
  )) %>%
  mutate(across(c(SCF, raw_DRF_old, raw_DRF_new), fmt_T))
print(t1, n = Inf, width = Inf)


#--- Table 2 — NW $ by age group ------------------------------------------

cat('\n=== Table 2 — Net worth ($T) by age group ===\n')
t2 = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  f$bin = age_bin(f$age_older)
  f %>% group_by(bin) %>%
    summarise(nw = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(source = src)
})) %>%
  pivot_wider(names_from = source, values_from = nw) %>%
  bind_rows(tibble(
    bin = factor('TOTAL', levels = c(levels(.$bin), 'TOTAL')),
    SCF         = sum(.$SCF, na.rm = TRUE),
    raw_DRF_old = sum(.$raw_DRF_old, na.rm = TRUE),
    raw_DRF_new = sum(.$raw_DRF_new, na.rm = TRUE)
  )) %>%
  mutate(across(c(SCF, raw_DRF_old, raw_DRF_new), fmt_T))
print(t2, n = Inf, width = Inf)


#--- Table 3 — NW shares + thresholds by wealth percentile ----------------

cat('\n=== Table 3 — Net worth shares + thresholds by wealth percentile ===\n')
t3_share = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  f$bin = wealth_bin(f$cat_nw, f$weight)
  total = sum(f$weight * f$cat_nw)
  f %>% group_by(bin) %>%
    summarise(nw = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(share = nw / total, source = src) %>%
    select(bin, source, share)
})) %>%
  pivot_wider(names_from = source, values_from = share)

threshold_probs = c(0, 0.50, 0.90, 0.99, 0.999)
t3_thr = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  q = weighted_quantile(f$cat_nw, f$weight, threshold_probs)
  tibble(bin = c('bot50','p50-90','p90-99','p99-99.9','top0.1%'),
         source = src, lower_threshold = q)
})) %>%
  pivot_wider(names_from = source, values_from = lower_threshold,
              names_prefix = 'thr_')

t3 = t3_share %>%
  mutate(across(c(SCF, raw_DRF_old, raw_DRF_new),
                ~ sprintf('%.3f', .))) %>%
  inner_join(t3_thr %>%
               mutate(across(starts_with('thr_'), fmt_M)),
             by = 'bin')
print(t3, n = Inf, width = Inf)


#--- Table 4 — counts + amounts by wealth category ------------------------

cat('\n=== Table 4 — Counts (M) and amounts ($T) by wealth category ===\n')
cat_cols = paste0('cat_', CALIB_CATEGORIES)
t4_amt = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  vals = vapply(cat_cols, function(c) sum(f$weight * f[[c]]), numeric(1))
  tibble(category = CALIB_CATEGORIES, source = src, amount = vals)
})) %>%
  pivot_wider(names_from = source, values_from = amount,
              names_prefix = 'amt_')

t4_cnt = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  vals = vapply(cat_cols, function(c) sum(f$weight * (f[[c]] > 0)),
                numeric(1))
  tibble(category = CALIB_CATEGORIES, source = src, count = vals)
})) %>%
  pivot_wider(names_from = source, values_from = count,
              names_prefix = 'cnt_')

fmt_Mc = function(x) sprintf('%.1fM', x / 1e6)
t4 = t4_cnt %>% inner_join(t4_amt, by = 'category') %>%
  mutate(across(starts_with('cnt_'), fmt_Mc),
         across(starts_with('amt_'), fmt_T))
print(t4, n = Inf, width = Inf)

cat('\nDone.\n')
