#---------------------------------------------
# report_v3.R
#
# Builds the v3 tilt-imputation report from the
# saved microdata. Three tables with 4 columns:
#   SCF | raw_DRF | post_tilt | post_rescale
#
#   Table 1: NW $ by income percentile (with total row)
#   Table 2: NW $ by age group (with total row)
#   Table 3: NW shares + thresholds by wealth percentile
#            (bot50, 50-90, 90-99, 99-99.9, top0.1%)
#
# Microdata sources (preserved -- can re-cut without
# re-running the harness):
#   - wealth_harness_tilt_diag.rds  (3 y matrices)
#   - tax_units_2022.csv             (weight, age, income)
#   - resources/cache/scf_tax_units.rds (SCF target side)
#
# Usage (sbatch):
#   Rscript src/eda/report_v3.R <output_dir>
# Writes: AM_STATUS.md (overwrites) and AM_STATUS.html
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: Rscript report_v3.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
source('src/imputations/helpers.R')
# scf_to_y is module-scope in wealth.R; pull just that helper.
# (Sourcing all of wealth.R is heavy and pulls run_wealth_imputation.)
scf_to_y_local = function(df) {
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

#--- Load microdata --------------------------------------------------------

cat(sprintf('Loading scf_tax_units...\n'))
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')

cat(sprintf('Loading puf_2022 from %s...\n', output_dir))
# Prefer the Phase-3 snapshot RDS (no wealth columns yet — exactly what
# we want for X-side joins). Fall back to tax_units_2022.csv (Phase-4
# output) which has wealth columns we'd ignore.
snap_path = file.path(output_dir, 'puf_2022_snapshot.rds')
csv_path  = file.path(output_dir, 'tax_units_2022.csv')
puf_2022 = if (file.exists(snap_path)) {
  cat(sprintf('  using snapshot: %s\n', snap_path))
  read_rds(snap_path)
} else if (file.exists(csv_path)) {
  cat(sprintf('  using CSV: %s\n', csv_path))
  read_csv(csv_path, show_col_types = FALSE)
} else {
  stop('Neither puf_2022_snapshot.rds nor tax_units_2022.csv found in ', output_dir)
}

diag_path = file.path(output_dir, 'wealth_harness_tilt_diag.rds')
cat(sprintf('Loading tilt diagnostics from %s...\n', diag_path))
diag = readRDS(diag_path)
stopifnot(!is.null(diag$y_pre_tilt),
          !is.null(diag$y_post_tilt_pre_rescale),
          !is.null(diag$y_post_rescale))

#--- Build per-record frames ----------------------------------------------

# PUF age + income (matching wealth.R's conventions).
puf_age2 = ifelse(is.na(puf_2022$age2), 0L, puf_2022$age2)
puf_age_older = pmax(pmin(80L, puf_2022$age1), pmin(80L, puf_age2))
puf_inc = with(puf_2022,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st +
  gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)

puf_meta = tibble(
  id        = puf_2022$id,
  weight    = puf_2022$weight,
  age_older = puf_age_older,
  income    = puf_inc
)

# Helper: for a stage's y tibble, compute all 8 cat_* values and join
# puf_meta. Returns a tibble with weight, age_older, income, cat_nw,
# cat_equities, cat_bonds, cat_homes, cat_retirement, cat_business,
# cat_other, cat_debt.
build_frame = function(y_tbl) {
  cv = compute_category_values(y_tbl)
  out = puf_meta %>% inner_join(y_tbl %>% select(id), by = 'id')
  for (cn in colnames(cv)) out[[cn]] = cv[[cn]]
  out
}

raw_frame      = build_frame(diag$y_pre_tilt)
tilt_frame     = build_frame(diag$y_post_tilt_pre_rescale)
rescale_frame  = build_frame(diag$y_post_rescale)

# SCF frame (target).
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_age_older = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_y    = scf_to_y_local(scf_tax_units)
scf_cv   = compute_category_values(scf_y)
scf_frame = tibble(
  weight    = scf_y$weight,
  age_older = scf_age_older,
  income    = scf_inc
)
for (cn in colnames(scf_cv)) scf_frame[[cn]] = scf_cv[[cn]]

frames = list(SCF = scf_frame, raw_DRF = raw_frame,
              post_tilt = tilt_frame, post_rescale = rescale_frame)

#--- Helpers ---------------------------------------------------------------

weighted_quantile = function(x, w, probs) {
  o = order(x); x = x[o]; w = w[o]
  p = cumsum(w) / sum(w)
  approx(p, x, xout = probs, rule = 2)$y
}

# Within-frame income percentile bin label.
income_bin = function(income, weight) {
  pct = compute_percentile(income, weight)   # 0..100 integer
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

# Within-frame wealth percentile bin label.
wealth_bin = function(nw, weight) {
  o = order(nw); cw = cumsum(weight[o]) / sum(weight)
  pct = numeric(length(nw)); pct[o] = 100 * cw
  cut(pct, breaks = c(-Inf, 50, 90, 99, 99.9, Inf),
      labels = c('bot50','p50-90','p90-99','p99-99.9','top0.1%'),
      include.lowest = TRUE, right = TRUE)
}

# $T formatter.
fmt_T = function(x) sprintf('$%.2fT', x / 1e12)
# $M formatter for thresholds.
fmt_M = function(x) sprintf('$%.2fM', x / 1e6)
# Rescale factor formatter — post_rescale / post_tilt. Returns "—" when
# post_tilt is near zero (catastrophic-collapse case where the ratio is
# meaningless).
fmt_factor = function(post_rescale, post_tilt) {
  ifelse(abs(post_tilt) < 1e6, '—',
         sprintf('%.3f', post_rescale / post_tilt))
}

#--- Table 1: NW $ by income percentile -----------------------------------

t1 = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  f$bin = income_bin(f$income, f$weight)
  f %>% group_by(bin) %>%
    summarise(nw_total = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(source = src)
})) %>%
  pivot_wider(names_from = source, values_from = nw_total) %>%
  bind_rows(tibble(
    bin = factor('TOTAL', levels = c(levels(.$bin), 'TOTAL')),
    SCF          = sum(.$SCF, na.rm = TRUE),
    raw_DRF      = sum(.$raw_DRF, na.rm = TRUE),
    post_tilt    = sum(.$post_tilt, na.rm = TRUE),
    post_rescale = sum(.$post_rescale, na.rm = TRUE)
  ))

t1_fmt = t1 %>%
  mutate(rescale_factor = fmt_factor(post_rescale, post_tilt)) %>%
  mutate(across(c(SCF, raw_DRF, post_tilt, post_rescale), fmt_T))

#--- Table 2: NW $ by age group -------------------------------------------

t2 = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  f$bin = age_bin(f$age_older)
  f %>% group_by(bin) %>%
    summarise(nw_total = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(source = src)
})) %>%
  pivot_wider(names_from = source, values_from = nw_total) %>%
  bind_rows(tibble(
    bin = factor('TOTAL', levels = c(levels(.$bin), 'TOTAL')),
    SCF          = sum(.$SCF, na.rm = TRUE),
    raw_DRF      = sum(.$raw_DRF, na.rm = TRUE),
    post_tilt    = sum(.$post_tilt, na.rm = TRUE),
    post_rescale = sum(.$post_rescale, na.rm = TRUE)
  ))

t2_fmt = t2 %>%
  mutate(rescale_factor = fmt_factor(post_rescale, post_tilt)) %>%
  mutate(across(c(SCF, raw_DRF, post_tilt, post_rescale), fmt_T))

#--- Table 3: NW shares + thresholds by wealth percentile -----------------

t3_share = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  f$bin = wealth_bin(f$cat_nw, f$weight)
  total = sum(f$weight * f$cat_nw)
  f %>% group_by(bin) %>%
    summarise(nw_total = sum(weight * cat_nw), .groups = 'drop') %>%
    mutate(share = nw_total / total, source = src) %>%
    select(bin, source, share)
})) %>%
  pivot_wider(names_from = source, values_from = share)

# Thresholds: lower edge of each bin, in dollars.
threshold_probs = c(0, 0.50, 0.90, 0.99, 0.999)
t3_thr = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  q = weighted_quantile(f$cat_nw, f$weight, threshold_probs)
  tibble(bin = c('bot50','p50-90','p90-99','p99-99.9','top0.1%'),
         source = src,
         lower_threshold = q)
})) %>%
  pivot_wider(names_from = source, values_from = lower_threshold,
              names_prefix = 'thr_')

t3 = t3_share %>%
  mutate(across(c(SCF, raw_DRF, post_tilt, post_rescale),
                ~ sprintf('%.3f', .))) %>%
  inner_join(t3_thr %>%
               mutate(across(c(thr_SCF, thr_raw_DRF, thr_post_tilt,
                              thr_post_rescale), fmt_M)),
             by = 'bin')

#--- Table 4: 8 categories × {count, amount} × 4 stages -------------------

# Categories per CALIB_CATEGORIES (matches stage3_target_qc.R):
#   nw, equities, bonds, homes, retirement, business, other, debt
# count[c]  = sum_i weight[i] * 1{cat_c[i] > 0}    (weighted households)
# amount[c] = sum_i weight[i] * cat_c[i]            (weighted dollars)
cat_cols = paste0('cat_', CALIB_CATEGORIES)

t4_amount = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  vals = vapply(cat_cols, function(c) sum(f$weight * f[[c]]), numeric(1))
  tibble(category = CALIB_CATEGORIES, source = src, amount = vals)
})) %>%
  pivot_wider(names_from = source, values_from = amount,
              names_prefix = 'amt_')

t4_count = bind_rows(lapply(names(frames), function(src) {
  f = frames[[src]]
  vals = vapply(cat_cols, function(c) sum(f$weight * (f[[c]] > 0)),
                numeric(1))
  tibble(category = CALIB_CATEGORIES, source = src, count = vals)
})) %>%
  pivot_wider(names_from = source, values_from = count,
              names_prefix = 'cnt_')

# Format: counts in millions of households, amounts in $T.
fmt_M_count = function(x) sprintf('%.1fM', x / 1e6)

t4 = t4_count %>%
  inner_join(t4_amount, by = 'category') %>%
  mutate(cnt_rescale_factor = fmt_factor(cnt_post_rescale, cnt_post_tilt),
         amt_rescale_factor = fmt_factor(amt_post_rescale, amt_post_tilt)) %>%
  mutate(across(starts_with('cnt_') & !ends_with('rescale_factor'),
                fmt_M_count),
         across(starts_with('amt_') & !ends_with('rescale_factor'),
                fmt_T)) %>%
  # Reorder so each rescale_factor sits right after its post_rescale.
  select(category,
         cnt_SCF, cnt_raw_DRF, cnt_post_tilt, cnt_post_rescale,
         cnt_rescale_factor,
         amt_SCF, amt_raw_DRF, amt_post_tilt, amt_post_rescale,
         amt_rescale_factor)

#--- Microdata-saving location note ---------------------------------------

microdata_paths = c(
  sprintf('  - 3-stage record-level Y matrices: %s', diag_path),
  sprintf('     (y_pre_tilt = raw DRF leaf draw; y_post_tilt_pre_rescale ='),
  sprintf('      after tilt, before Step B; y_post_rescale = final.'),
  sprintf('     Each is a tibble with id + 23 wealth Y vars.)'),
  sprintf('  - PUF X-side covariates: %s/tax_units_2022.csv',
          output_dir),
  sprintf('  - SCF target side: resources/cache/scf_tax_units.rds')
)

#--- Build markdown -------------------------------------------------------

md_lines = c(
  '# Tilt Wealth Imputation — v4 Report',
  '',
  'v4 changes vs v3: `lambda_max=5` per-component cap on the tilt + Step',
  'B `fallback_uniform` branch disabled (it inflated counts when tilt',
  'collapsed in infeasible cells; now Step B falls back to `skip` if PUF',
  'total is near zero, but in v4 no cells hit that — every (cell × cat)',
  'has positive PUF mass after the constrained tilt). Plus the upstream',
  'age fix that tightened cell-pop ratios.',
  '',
  'See `slurm_tilt_v4.out` for full per-bucket diagnostics.',
  '',
  '## Method',
  '',
  'For each PUF tax unit, we ask the per-(income-cell) DRF for forest-',
  'averaged donor probabilities `p_ij` over the cell\'s SCF bootstrap, then',
  'tilt them by `q_ij ∝ p_ij · exp(Z_jᵀ λ)` where `Z_j` are donor-side',
  'wealth target features (8 categories × {amount, count}) and `λ` is',
  'optimized per (income × age) bucket against SCF cell aggregates via',
  'BFGS. Solver runs on a normalized basis (`Z_n = Z / max|Z|` per column)',
  'so condition number stays in check across count vs amount targets.',
  'One donor per record is then sampled from `q_ij` with top-K=300',
  'sparsification.',
  '',
  'A residual rescale (Step B) closes any per-(cell × cat) dollar gap',
  'that the tilt couldn\'t reach. Three modes:',
  '',
  '- `multiplicative` — `factor = SCF_total / PUF_total` when both alive',
  '- `fallback_uniform` — when tilt collapsed donors to zero in a',
  '  category (PUF total ≈ 0 but SCF target > 0), distribute SCF',
  '  aggregate uniformly across PUF records in the cell',
  '- `skip` — when SCF target ≈ 0 (don\'t fabricate amounts)',
  '',
  'DRF `min.node.size = 50`. Per-cell forests cached at',
  '`resources/cache/qrf/wealth_percell_<ci>_mns50.rds`.',
  '',
  '---',
  '',
  '## Table 1 — Net worth ($T) by income percentile',
  '',
  'Income percentile is computed within-frame (each source ranks its own',
  'records). NW = sum(13 assets) − sum(6 debts). Excludes `kg_*`',
  'unrealized capital gains (consistent with `cat_nw` definition in',
  '`stage3_target_qc.R`).',
  '',
  knitr::kable(t1_fmt, format = 'markdown',
               align = c('l', rep('r', 5))) %>%
    paste(collapse = '\n'),
  '',
  '## Table 2 — Net worth ($T) by age group',
  '',
  'Age group from `age_older` = max(age1, age2), capped at 80. SCF and PUF',
  'use the same definition.',
  '',
  knitr::kable(t2_fmt, format = 'markdown',
               align = c('l', rep('r', 5))) %>%
    paste(collapse = '\n'),
  '',
  '## Table 3 — Net worth shares + thresholds by wealth percentile',
  '',
  'Wealth percentile is computed within-frame on net worth. Share is',
  'fraction of total NW. Threshold is the lower-edge wealth value at the',
  'bin boundary (e.g., `p50-90` lower threshold = the median NW; `top0.1%`',
  'lower threshold = 99.9th percentile NW).',
  '',
  knitr::kable(t3, format = 'markdown',
               col.names = c('bin', 'SCF', 'raw_DRF', 'post_tilt',
                             'post_rescale',
                             'SCF thr', 'raw_DRF thr', 'post_tilt thr',
                             'post_rescale thr'),
               align = c('l', rep('r', 8))) %>%
    paste(collapse = '\n'),
  '',
  '## Table 4 — Counts and amounts by wealth category',
  '',
  'Eight categories: total NW, plus the seven dollar-target categories',
  '(equities, bonds, homes [primary residence], retirement [IRA + 401k +',
  'pensions], business [pass-throughs], other [residual assets], debt).',
  'Count = weighted M-households with `cat > 0`; amount = weighted $T sum.',
  'NW count is `cat_nw > 0` (positive net worth).',
  '',
  knitr::kable(t4, format = 'markdown',
               col.names = c('category',
                             'SCF cnt', 'raw_DRF cnt', 'post_tilt cnt',
                             'post_rescale cnt', 'rescale_factor cnt',
                             'SCF amt', 'raw_DRF amt', 'post_tilt amt',
                             'post_rescale amt', 'rescale_factor amt'),
               align = c('l', rep('r', 10))) %>%
    paste(collapse = '\n'),
  '',
  '---',
  '',
  '## Microdata for re-cuts',
  '',
  'All record-level microdata is saved — you can build new tables without',
  're-running the harness:',
  '',
  microdata_paths,
  '',
  'Re-run this report after editing `src/eda/report_v3.R`:',
  '',
  '```',
  'sbatch slurm_render_status.sh   # re-renders HTML from AM_STATUS.md',
  '```',
  '',
  '(or call `Rscript src/eda/report_v3.R <output_dir>` via sbatch to',
  'rebuild AM_STATUS.md from microdata first, then re-render HTML.)'
)

writeLines(md_lines, 'AM_STATUS.md')
cat(sprintf('Wrote AM_STATUS.md (%d lines)\n', length(md_lines)))

# Print to stdout for the slurm job log too.
cat('\n', paste(md_lines, collapse = '\n'), '\n', sep = '')

cat('\nDone.\n')
