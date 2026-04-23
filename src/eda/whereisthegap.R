#--------------------------------------
# whereisthegap.R
#
# Stop narrating, start measuring. Three
# tests that cleanly separate possible
# sources of the ~$30T PUF-SCF NW gap.
#
# Test 1. Small-spec null-mode: cells =
#   (pct_q × mar × age × n_dep × has_inc)
#   — all 5 production small-spec features
#   binned. Compute SCF cell means, assign
#   to PUF by cell, aggregate.
#   If this recovers ~$30T: gap is
#   population mismatch at small-spec X,
#   not any DRF-specific mechanism.
#   If it doesn't: DRF is doing something
#   at finer-than-cell resolution.
#
# Test 2. Small-spec null-mode vs actual
#   DRF output. Measures how much the DRF
#   adds above cell means under the 5
#   small-spec features specifically.
#
# Test 3. Within-cell n_dep distribution
#   for the canonical 4|mar|65+|biz0 cell.
#   Fills the one small-spec feature we
#   haven't measured.
#
# Inputs (all cached):
#   resources/cache/scf_tax_units.rds
#   resources/cache/wealth_analysis.rds
#   resources/cache/consumption_analysis.rds
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/imputations/helpers.R')

report_path = 'plots/wealth_decomp/whereisthegap.txt'
plot_dir    = 'plots/wealth_decomp'

scf      = read_rds('resources/cache/scf_tax_units.rds')
puf_dem  = read_rds('resources/cache/wealth_analysis.rds')  # has actual imputed NW
puf_cons = read_rds('resources/cache/consumption_analysis.rds')

sink(report_path, split = TRUE)
cat('Where is the gap — direct measurement\n')
cat('Generated: ', as.character(Sys.time()), '\n', sep = '')
cat(paste0(rep('=', 72), collapse = ''), '\n\n')

#---------------------------------------------------------------------------
# Harmonize. Compute net_worth on each side. Use the 5 small-spec features
# mirroring production wealth.R:102.
#---------------------------------------------------------------------------

asset_vars = c('cash', 'equities', 'bonds', 'retirement', 'life_ins',
               'annuities', 'trusts', 'other_fin', 'pass_throughs',
               'primary_home', 'other_home', 're_fund', 'other_nonfin')
debt_vars  = c('primary_mortgage', 'other_mortgage', 'credit_lines',
               'credit_cards', 'installment_debt', 'other_debt')

scf_h = scf %>%
  transmute(
    weight, age1, income, married = as.integer(married),
    n_dep = as.integer(n_dep),
    business_scf,
    has_income = case_when(income > 0 ~ 1L, income == 0 ~ 0L, TRUE ~ -1L),
    net_worth = !!rlang::parse_expr(paste(asset_vars, collapse = ' + ')) -
                !!rlang::parse_expr(paste(debt_vars,  collapse = ' + '))
  )
# Within-dataset income percentile
scf_h$pctile_income = compute_percentile(scf_h$income, scf_h$weight)

puf_h = puf_dem %>%
  select(id, weight, age1, married, n_dep, income, net_worth) %>%
  mutate(
    married = as.integer(married),
    n_dep   = as.integer(n_dep),
    has_income = case_when(income > 0 ~ 1L, income == 0 ~ 0L, TRUE ~ -1L),
    pctile_income = compute_percentile(income, weight)
  )

#---------------------------------------------------------------------------
# Binning — 5-dim small-spec cells
#---------------------------------------------------------------------------

bin_small = function(d) {
  pos = d$income > 0
  brks = Hmisc::wtd.quantile(d$income[pos], d$weight[pos], probs = seq(0, 1, 0.2))
  d$pct_q = findInterval(d$income, brks, all.inside = TRUE)
  d$pct_q[!pos] = 0L
  d$age_b = as.character(cut(d$age1, c(-Inf, 35, 55, 65, Inf),
                             labels = c('<35', '35-54', '55-64', '65+'), right = FALSE))
  d$mar_b = ifelse(d$married == 1L, 'mar', 'sin')
  d$dep_b = as.character(factor(pmin(as.integer(d$n_dep), 3L),
                                levels = 0:3, labels = c('d0', 'd1', 'd2', 'd3+')))
  d$has_b = ifelse(d$has_income > 0, 'hi+',
             ifelse(d$has_income < 0, 'hi-', 'hi0'))
  d$cell = paste(d$pct_q, d$mar_b, d$age_b, d$dep_b, d$has_b, sep = '|')
  d
}
scf_h = bin_small(scf_h)
puf_h = bin_small(puf_h)

#---------------------------------------------------------------------------
# TEST 1 + 2: null-mode total at small-spec cells
#---------------------------------------------------------------------------

sc = scf_h %>% group_by(cell) %>%
  summarise(W_scf = sum(weight),
            NW_scf_tn = sum(net_worth * weight) / 1e12,
            mu_scf = NW_scf_tn * 1e12 / W_scf, .groups = 'drop')
pc = puf_h %>% group_by(cell) %>%
  summarise(W_puf = sum(weight),
            NW_puf_tn = sum(net_worth * weight) / 1e12,
            mu_puf = NW_puf_tn * 1e12 / W_puf, .groups = 'drop')

j = full_join(sc, pc, by = 'cell') %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         null_tn = W_puf * mu_scf / 1e12,
         actual_tn = NW_puf_tn)

scf_total  = sum(j$NW_scf_tn)
puf_total  = sum(j$NW_puf_tn)
null_total = sum(j$null_tn)

cat('=== TEST 1: Small-spec null-mode (5D cells: pct_q × mar × age × n_dep × has_income) ===\n\n')
cat(sprintf('  Cells defined: %d\n', nrow(j)))
cat(sprintf('  Cells populated on both sides: %d\n', sum(j$W_scf > 0 & j$W_puf > 0)))
cat(sprintf('  Total PUF weight covered: %.1f%%\n\n',
            100 * sum(j$W_puf[j$W_scf > 0]) / sum(j$W_puf)))

cat(sprintf('  SCF total NW:                       $%.2fT\n', scf_total))
cat(sprintf('  PUF total NW (actual, DRF output):  $%.2fT\n', puf_total))
cat(sprintf('  PUF total NW (null = W_PUF × μ_SCF): $%.2fT\n', null_total))
cat('\n')
cat(sprintf('  Gap from pop mismatch alone (null - SCF):     %+.2fT\n',
            null_total - scf_total))
cat(sprintf('  Gap from DRF within-cell variation (actual - null): %+.2fT\n',
            puf_total - null_total))
cat(sprintf('  Total gap (actual - SCF):                     %+.2fT\n',
            puf_total - scf_total))

#---------------------------------------------------------------------------
# How much of the null-mode gap comes from cells where PUF has extra weight?
# Rank cells by |null - SCF contribution|, show top 15.
#---------------------------------------------------------------------------

j2 = j %>%
  mutate(null_vs_scf = null_tn - NW_scf_tn) %>%
  arrange(desc(abs(null_vs_scf)))

cat('\n  Top 15 cells by |null contribution to gap| (null-mode, small-spec):\n\n')
cat(sprintf('  %-36s %8s %8s %10s %10s %8s\n',
            'cell', 'W_scf(M)', 'W_puf(M)', 'μ_scf($K)', 'null($T)', 'null-scf'))
cat('  ', strrep('-', 88), '\n', sep = '')
for (i in seq_len(min(15, nrow(j2)))) {
  cat(sprintf('  %-36s %8.2f %8.2f %10.0f %+10.2f %+8.2f\n',
              j2$cell[i], j2$W_scf[i] / 1e6, j2$W_puf[i] / 1e6,
              j2$mu_scf[i] / 1e3, j2$null_tn[i] - j2$NW_scf_tn[i], j2$null_vs_scf[i]))
}

#---------------------------------------------------------------------------
# TEST 3: within-cell n_dep distribution for 4|mar|65+|biz0
#---------------------------------------------------------------------------

cat('\n\n=== TEST 3: within-cell n_dep distribution, 4|mar|65+|biz0 ===\n\n')

# Custom filter — business status from raw components on each side
puf_biz = puf_cons %>%
  transmute(id,
            business_p = sole_prop + farm +
                         scorp_active  - scorp_active_loss  - scorp_179 +
                         scorp_passive - scorp_passive_loss +
                         part_active   - part_active_loss   - part_179 +
                         part_passive  - part_passive_loss)
puf_with_biz = puf_h %>% left_join(puf_biz, by = 'id') %>%
  filter(!is.na(business_p))

scf_cell = scf_h %>% filter(pct_q == 4L, married == 1L, age1 >= 65L,
                            business_scf <= 0)
puf_cell = puf_with_biz %>% filter(pct_q == 4L, married == 1L, age1 >= 65L,
                                   business_p <= 0)

cat(sprintf('  SCF: %.2fM weighted\n  PUF: %.2fM weighted\n\n',
            sum(scf_cell$weight) / 1e6, sum(puf_cell$weight) / 1e6))

cat('  n_dep distribution (weighted shares):\n')
cat(sprintf('  %6s  %8s  %8s  %8s\n', 'n_dep', 'SCF', 'PUF', 'Δ'))
for (k in c(0, 1, 2, 3)) {
  tag = if (k == 3) '3+' else as.character(k)
  s_share = sum(scf_cell$weight[pmin(scf_cell$n_dep, 3L) == k]) / sum(scf_cell$weight)
  p_share = sum(puf_cell$weight[pmin(puf_cell$n_dep, 3L) == k]) / sum(puf_cell$weight)
  cat(sprintf('  %6s  %7.1f%%  %7.1f%%  %+7.1f\n',
              tag, 100 * s_share, 100 * p_share, 100 * (p_share - s_share)))
}

#---------------------------------------------------------------------------
# TEST 4: Super-simple sanity check — is it PUF has too many weighted
# households vs SCF period? Compare the raw household counts by pct_q.
#---------------------------------------------------------------------------

cat('\n\n=== TEST 4: Weighted population by pct_q, raw comparison ===\n\n')
cat(sprintf('  %6s %10s %10s %+8s\n', 'pct_q', 'W_scf(M)', 'W_puf(M)', 'Δ'))
for (q in 0:5) {
  ws = sum(scf_h$weight[scf_h$pct_q == q]) / 1e6
  wp = sum(puf_h$weight[puf_h$pct_q == q]) / 1e6
  cat(sprintf('  %6d %10.2f %10.2f %+8.2f\n', q, ws, wp, wp - ws))
}

cat('\n\nArtifacts: plots/wealth_decomp/whereisthegap.txt\n')
sink()
