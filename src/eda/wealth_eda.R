#--------------------------------------
# wealth_eda.R
#
# Side-by-side EDA of the imputed PUF
# wealth vs the SCF tax-unit wealth
# (from Stage 1). Compares:
#   - top-level aggregates
#   - median/mean NW by income percentile
#   - median/mean NW by age band
#   - median/mean NW by filing status
#   - top-share concentration
#   - per-category weighted aggregates
#
# Outputs a plain-text report to
# plots/wealth_eda/eda_report.txt
# and two comparison plots.
#
# Inputs:
#   resources/cache/scf_tax_units.rds
#     (Stage 1: 29,290 tax units)
#   resources/cache/wealth_analysis.rds
#     (PUF post-wealth-imputation,
#      cached by test_wealth.R)
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)

report_path = 'plots/wealth_eda/eda_report.txt'
plot_dir    = 'plots/wealth_eda'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

#---------------------------------------------------------------------------
# Load
#---------------------------------------------------------------------------

scf_path = 'resources/cache/scf_tax_units.rds'
puf_path = 'resources/cache/wealth_analysis.rds'

if (!file.exists(scf_path))
  stop('SCF tax-unit cache missing: ', scf_path,
       ' — run src/eda/test_stage1.R first.')
if (!file.exists(puf_path))
  stop('PUF wealth-analysis cache missing: ', puf_path,
       ' — run src/eda/test_wealth.R first (with the write_rds added).')

scf = read_rds(scf_path)
puf = read_rds(puf_path)

sink(report_path, split = TRUE)

cat('Wealth imputation EDA — PUF imputed vs SCF tax units (Stage 1)\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Harmonize both frames into a common schema: {weight, age1, income,
# filing_status, asset_vars..., debt_vars..., kg_vars..., total_assets,
# total_debts, net_worth}. SCF's filestat is the Stage-1 FILESTAT column;
# PUF's is filing_status (MARS-coded) — both use 1=Single, 2=MFJ, 3=MFS,
# 4=HoH.
#---------------------------------------------------------------------------

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')
kg_vars    = c('kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other')
y_vars     = c(asset_vars, debt_vars, kg_vars)

scf_h = scf %>%
  transmute(
    weight = weight, age1 = age1, income = income,
    filing_status = filestat,
    !!!setNames(lapply(y_vars, function(v) rlang::sym(v)), y_vars),
    total_assets = !!rlang::parse_expr(paste(asset_vars, collapse = ' + ')),
    total_debts  = !!rlang::parse_expr(paste(debt_vars,  collapse = ' + ')),
    net_worth    = total_assets - total_debts
  )

puf_h = puf %>%
  transmute(weight, age1, income, filing_status,
            !!!setNames(lapply(y_vars, function(v) rlang::sym(v)), y_vars),
            total_assets, total_debts, net_worth)

#---------------------------------------------------------------------------
# Helpers
#---------------------------------------------------------------------------

w_sum   = function(x, w) sum(x * w)
w_med   = function(x, w) as.numeric(Hmisc::wtd.quantile(x, w, 0.5, na.rm = TRUE))
w_mean  = function(x, w) weighted.mean(x, w, na.rm = TRUE)
w_share = function(flag, w) sum(w[flag]) / sum(w)

# Standard within-dataset income percentile (1..100)
rank_pct = function(x, w) {
  pos = x > 0
  if (!any(pos)) return(rep(0L, length(x)))
  brks = Hmisc::wtd.quantile(x[pos], w[pos], probs = seq(0, 1, 0.01))
  bin  = findInterval(x, brks, all.inside = TRUE)
  ifelse(pos, bin, 0L)
}

scf_h$pctile = rank_pct(scf_h$income, scf_h$weight)
puf_h$pctile = rank_pct(puf_h$income, puf_h$weight)

#---------------------------------------------------------------------------
# 1. Top-level aggregates
#---------------------------------------------------------------------------

cat('1. Top-level aggregates\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

tl = tibble(
  metric = c(
    'weighted tax units (M)',
    'total assets ($T)',
    'total debts ($T)',
    'net worth ($T)',
    'mean NW per unit ($K)',
    'median NW per unit ($)',
    'share NW <= 0 (%)',
    'share NW >= $1M (%)',
    'share NW >= $10M (%)'
  ),
  scf = c(
    sum(scf_h$weight) / 1e6,
    w_sum(scf_h$total_assets, scf_h$weight) / 1e12,
    w_sum(scf_h$total_debts,  scf_h$weight) / 1e12,
    w_sum(scf_h$net_worth,    scf_h$weight) / 1e12,
    w_mean(scf_h$net_worth, scf_h$weight) / 1e3,
    w_med (scf_h$net_worth, scf_h$weight),
    100 * w_share(scf_h$net_worth <= 0,       scf_h$weight),
    100 * w_share(scf_h$net_worth >= 1e6,     scf_h$weight),
    100 * w_share(scf_h$net_worth >= 1e7,     scf_h$weight)
  ),
  puf = c(
    sum(puf_h$weight) / 1e6,
    w_sum(puf_h$total_assets, puf_h$weight) / 1e12,
    w_sum(puf_h$total_debts,  puf_h$weight) / 1e12,
    w_sum(puf_h$net_worth,    puf_h$weight) / 1e12,
    w_mean(puf_h$net_worth, puf_h$weight) / 1e3,
    w_med (puf_h$net_worth, puf_h$weight),
    100 * w_share(puf_h$net_worth <= 0,       puf_h$weight),
    100 * w_share(puf_h$net_worth >= 1e6,     puf_h$weight),
    100 * w_share(puf_h$net_worth >= 1e7,     puf_h$weight)
  )
) %>%
  mutate(gap_pct = 100 * (puf / scf - 1))

cat(sprintf('  %-26s | %12s %12s %10s\n',
            'metric', 'SCF', 'PUF', 'gap %'))
cat(paste0(rep('-', 72), collapse = ''), '\n')
for (i in seq_len(nrow(tl))) {
  cat(sprintf('  %-26s | %12.2f %12.2f %+10.1f\n',
              tl$metric[i], tl$scf[i], tl$puf[i], tl$gap_pct[i]))
}

#---------------------------------------------------------------------------
# 2. NW by income percentile group
#---------------------------------------------------------------------------

cat('\n\n2. Net worth by income percentile group\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

groups = list(
  'P1-P25'    = c(1, 25),
  'P25-P50'   = c(25, 50),
  'P50-P75'   = c(50, 75),
  'P75-P90'   = c(75, 90),
  'P90-P95'   = c(90, 95),
  'P95-P99'   = c(95, 99),
  'P99+'      = c(99, 100)
)

by_pct = function(d, g) {
  s = d %>% filter(pctile > g[1], pctile <= g[2])
  if (nrow(s) == 0) return(rep(NA_real_, 4))
  c(
    n_k    = sum(s$weight) / 1e3,                # weighted HHs in thousands
    med_NW = w_med (s$net_worth, s$weight),
    mean_NW = w_mean(s$net_worth, s$weight),
    med_A  = w_med (s$total_assets, s$weight)
  )
}

cat(sprintf('  %-8s | %12s %12s %12s %12s | %12s %12s %12s %12s\n',
            'group',
            'SCF N(K)', 'SCF medNW', 'SCF meanNW', 'SCF medA',
            'PUF N(K)', 'PUF medNW', 'PUF meanNW', 'PUF medA'))
cat(paste0(rep('-', 116), collapse = ''), '\n')
for (nm in names(groups)) {
  s = by_pct(scf_h, groups[[nm]])
  p = by_pct(puf_h, groups[[nm]])
  cat(sprintf('  %-8s | %12.1f %12.0f %12.0f %12.0f | %12.1f %12.0f %12.0f %12.0f\n',
              nm, s[1], s[2], s[3], s[4],
                  p[1], p[2], p[3], p[4]))
}

#---------------------------------------------------------------------------
# 3. NW by age band
#---------------------------------------------------------------------------

cat('\n\n3. Net worth by primary age band\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

age_bands = list(
  '<35'    = c(-Inf, 35),
  '35-54'  = c(35, 55),
  '55-64'  = c(55, 65),
  '65+'    = c(65, Inf)
)
by_age = function(d, r) {
  s = d %>% filter(!is.na(age1), age1 >= r[1], age1 < r[2])
  if (nrow(s) == 0) return(rep(NA_real_, 4))
  c(
    n_k     = sum(s$weight) / 1e3,
    med_NW  = w_med (s$net_worth, s$weight),
    mean_NW = w_mean(s$net_worth, s$weight),
    shareTA = sum(s$total_assets * s$weight) /
              w_sum(d$total_assets, d$weight)
  )
}
cat(sprintf('  %-8s | %12s %12s %12s %12s | %12s %12s %12s %12s\n',
            'age',
            'SCF N(K)', 'SCF medNW', 'SCF meanNW', 'SCF %A',
            'PUF N(K)', 'PUF medNW', 'PUF meanNW', 'PUF %A'))
cat(paste0(rep('-', 116), collapse = ''), '\n')
for (nm in names(age_bands)) {
  s = by_age(scf_h, age_bands[[nm]])
  p = by_age(puf_h, age_bands[[nm]])
  cat(sprintf('  %-8s | %12.1f %12.0f %12.0f %12.3f | %12.1f %12.0f %12.0f %12.3f\n',
              nm, s[1], s[2], s[3], s[4],
                  p[1], p[2], p[3], p[4]))
}

#---------------------------------------------------------------------------
# 4. NW by filing status
#---------------------------------------------------------------------------

cat('\n\n4. Net worth by filing status (1=Single, 2=MFJ, 3=MFS, 4=HoH)\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

fs_by = function(d, fs) {
  s = d %>% filter(filing_status == fs)
  if (nrow(s) == 0) return(rep(NA_real_, 3))
  c(
    n_k     = sum(s$weight) / 1e3,
    med_NW  = w_med (s$net_worth, s$weight),
    mean_NW = w_mean(s$net_worth, s$weight)
  )
}
cat(sprintf('  %-3s | %12s %12s %12s | %12s %12s %12s\n',
            'fs',
            'SCF N(K)', 'SCF medNW', 'SCF meanNW',
            'PUF N(K)', 'PUF medNW', 'PUF meanNW'))
cat(paste0(rep('-', 92), collapse = ''), '\n')
for (fs in 1:4) {
  s = fs_by(scf_h, fs)
  p = fs_by(puf_h, fs)
  cat(sprintf('  %-3d | %12.1f %12.0f %12.0f | %12.1f %12.0f %12.0f\n',
              fs, s[1], s[2], s[3], p[1], p[2], p[3]))
}

#---------------------------------------------------------------------------
# 5. Top-share concentration (by income percentile, share of TOTAL wealth)
#---------------------------------------------------------------------------

cat('\n\n5. Top-share concentration (share of total NW held by income top X%)\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

top_share = function(d, cutoff) {
  total = sum(d$net_worth * d$weight)
  top   = d %>% filter(pctile > cutoff)
  sum(top$net_worth * top$weight) / total
}
cutoffs = c(50, 90, 95, 99)
cat(sprintf('  %-8s | %10s %10s\n', 'top', 'SCF %', 'PUF %'))
cat(paste0(rep('-', 36), collapse = ''), '\n')
for (c in cutoffs) {
  s = 100 * top_share(scf_h, c)
  p = 100 * top_share(puf_h, c)
  cat(sprintf('  top %2d%%  | %10.1f %10.1f\n', 100 - c, s, p))
}

#---------------------------------------------------------------------------
# 6. Per-category weighted aggregates ($B)
#---------------------------------------------------------------------------

cat('\n\n6. Per-category weighted aggregates ($ billions)\n')
cat(paste0(rep('-', 72), collapse = ''), '\n')

cat_table = tibble(
  variable = y_vars,
  scf_bn   = sapply(y_vars, function(v) w_sum(scf_h[[v]], scf_h$weight)) / 1e9,
  puf_bn   = sapply(y_vars, function(v) w_sum(puf_h[[v]], puf_h$weight)) / 1e9
) %>%
  mutate(gap_pct = 100 * (puf_bn / scf_bn - 1))

cat(sprintf('  %-18s | %10s %10s %10s\n',
            'variable', 'SCF $B', 'PUF $B', 'gap %'))
cat(paste0(rep('-', 56), collapse = ''), '\n')
for (i in seq_len(nrow(cat_table))) {
  cat(sprintf('  %-18s | %10.0f %10.0f %+10.1f\n',
              cat_table$variable[i],
              cat_table$scf_bn[i], cat_table$puf_bn[i],
              cat_table$gap_pct[i]))
}

#---------------------------------------------------------------------------
# Plots
#---------------------------------------------------------------------------

# NW by percentile overlay
nw_by_p = function(d, src) {
  d %>%
    filter(pctile >= 1) %>%
    group_by(pctile) %>%
    summarise(
      med_NW   = w_med (net_worth, weight),
      mean_NW  = w_mean(net_worth, weight),
      .groups = 'drop'
    ) %>%
    mutate(source = src)
}
p_df = bind_rows(nw_by_p(scf_h, 'SCF'), nw_by_p(puf_h, 'PUF'))

p1 = p_df %>%
  ggplot(aes(pctile, med_NW / 1000, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c('SCF' = 'steelblue', 'PUF' = 'firebrick')) +
  labs(title = 'Median net worth by within-dataset income percentile',
       x = 'Income percentile', y = 'Median NW ($ thousands)') +
  theme_minimal(base_size = 10)
ggsave(file.path(plot_dir, '01_median_nw_by_pctile.png'), p1,
       width = 9, height = 5, dpi = 140)

p2 = p_df %>%
  ggplot(aes(pctile, mean_NW / 1000, color = source)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c('SCF' = 'steelblue', 'PUF' = 'firebrick')) +
  scale_y_log10() +
  labs(title = 'Mean net worth by within-dataset income percentile (log y)',
       x = 'Income percentile', y = 'Mean NW ($ thousands, log)') +
  theme_minimal(base_size = 10)
ggsave(file.path(plot_dir, '02_mean_nw_by_pctile.png'), p2,
       width = 9, height = 5, dpi = 140)

# Per-category gap bar
p3 = cat_table %>%
  mutate(bucket = case_when(variable %in% asset_vars ~ 'asset',
                            variable %in% debt_vars  ~ 'debt',
                            TRUE                       ~ 'kg'),
         variable = factor(variable, levels = variable[order(gap_pct)])) %>%
  ggplot(aes(variable, gap_pct, fill = bucket)) +
  geom_col() +
  geom_hline(yintercept = 0, color = 'grey40') +
  scale_fill_manual(values = c('asset' = '#4daf4a',
                                'debt'  = '#e41a1c',
                                'kg'    = '#377eb8')) +
  coord_flip() +
  labs(title = 'Per-category aggregate gap: PUF imputed vs SCF tax-unit',
       subtitle = 'Positive = PUF higher than SCF',
       x = NULL, y = 'PUF / SCF − 1 (%)') +
  theme_minimal(base_size = 9)
ggsave(file.path(plot_dir, '03_per_category_gap.png'), p3,
       width = 9, height = 6, dpi = 140)

cat(sprintf('\nPlots saved to %s/\n', plot_dir))
cat('  01_median_nw_by_pctile.png  — median NW by income percentile, SCF vs PUF\n')
cat('  02_mean_nw_by_pctile.png    — mean NW by income percentile (log y)\n')
cat('  03_per_category_gap.png     — per-category $ aggregate gap\n')
cat('\nReport saved to ', report_path, '\n', sep = '')

sink()
