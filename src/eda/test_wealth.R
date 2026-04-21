#--------------------------------------
# test_wealth.R
#
# Smoke test for src/imputations/wealth.R.
# Runs the pipeline up through
# consumption, attaches an scf_tax_units
# object (from Stage 1 if available,
# otherwise a household-level stub read
# directly from SCFP2022.csv), runs the
# wealth imputation, and prints/plots
# per-category diagnostics.
#
# The stub path is explicitly marked so
# results from smoke tests run before
# Stage 1 lands are not mistaken for
# validated output: with the stub, each
# SCF household is treated as one tax
# unit, which over-attributes wealth at
# PEU-spanning incomes and does not
# exercise the Stage-1 PEU split.
#
# Mirrors src/eda/test_consumption.R.
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')
estimate_models = 1
save_wealth_diagnostics = TRUE
# Skip the reweight LP solver and read cached weight_deltas.rds.
# This is the slow step (~30 min) and its output is the same as long as
# the PUF source and SOI targets are unchanged. If you change either,
# set this back to 1 once to regenerate the cache.
do_lp = 0
set.seed(76)

#---------------------------------------------------------------------------
# Minimal pipeline up to consumption (wealth runs right after)
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
source('./src/imputations/ssn.R')
source('./src/imputations/earnings_split.R')
source('./src/imputations/qbi.R')
source('./src/imputations/mobility.R')
source('./src/imputations/tips.R')
source('./src/imputations/overtime.R')
source('./src/imputations/auto_loan.R')
source('./src/imputations/childcare.R')
source('./src/imputations/mortgage.R')
source('./src/imputations/consumption.R')

#---------------------------------------------------------------------------
# Provide scf_tax_units
#
# Preferred: Stage 1 produces this upstream. Until it does, we read raw
# SCFP2022.csv and treat each household as one tax unit. The stub is
# clearly flagged in the diagnostics so results are not mistaken for
# validated output.
#---------------------------------------------------------------------------

stage1_cache = 'resources/cache/scf_tax_units.rds'
if (file.exists(stage1_cache)) {
  cat('test_wealth.R: loading Stage-1 scf_tax_units from ', stage1_cache, '\n')
  scf_tax_units = read_rds(stage1_cache)
  scf_source = 'stage1'
} else {
  cat('test_wealth.R: Stage 1 output not found at ', stage1_cache, '\n')
  cat('               falling back to household-level SCFP2022 stub.\n')
  cat('               RESULTS ARE NOT VALIDATED — each SCF HH treated as one tax unit.\n')

  scf_tax_units = interface_paths$SCF %>%
    file.path('SCFP2022.csv') %>%
    fread() %>%
    tibble() %>%
    transmute(
      weight = WGT,
      income = INCOME,
      age1   = AGE,
      age2   = NA_integer_,
      n_dep  = pmin(KIDS, 3L),
      male1  = as.integer(HHSEX == 1),
      male2  = NA_integer_,
      married = as.integer(MARRIED == 1),
      # All raw SCFP fields used by scf_to_y() in wealth.R:
      LIQ, CDS, STOCKS, STMUTF, COMUTF, BOND, SAVBND, TFBMUTF, GBMUTF, OBMUTF,
      IRAKH, THRIFT, FUTPEN, CURRPEN, CASHLI, ANNUIT, TRUSTS, OTHFIN, OMUTF,
      BUS, HOUSES, ORESRE, NNRESRE, VEHIC, OTHNFIN,
      MRTHEL, RESDBT, OTHLOC, CCBAL, INSTALL, ODEBT,
      KGHOUSE, KGORE, KGBUS, KGSTMF
    )
  scf_source = 'stub'
}

cat(sprintf('scf_tax_units: %d rows, source = %s\n', nrow(scf_tax_units), scf_source))

#---------------------------------------------------------------------------
# Run wealth imputation
#---------------------------------------------------------------------------

cat('\n========== Running wealth imputation ==========\n\n')
source('./src/imputations/wealth.R')
cat('\n========== Wealth imputation complete ==========\n\n')

#---------------------------------------------------------------------------
# Build analysis dataset
#---------------------------------------------------------------------------

wealth_y_vars = c(
  'cash', 'equities', 'bonds', 'retirement', 'life_ins', 'annuities',
  'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
  're_fund', 'other_nonfin',
  'primary_mortgage', 'other_mortgage', 'credit_lines',
  'credit_cards', 'installment_debt', 'other_debt',
  'kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other'
)

asset_vars = wealth_y_vars[1:13]
debt_vars  = wealth_y_vars[14:19]
kg_vars    = wealth_y_vars[20:23]

if (!all(wealth_y_vars %in% names(tax_units))) {
  stop('Wealth vars missing from tax_units; imputation may have been skipped.')
}

d = tax_units %>%
  mutate(
    income = wages + sole_prop + part_active + part_passive - part_active_loss -
      part_passive_loss - part_179 + scorp_active + scorp_passive -
      scorp_active_loss - scorp_passive_loss - scorp_179 + gross_ss +
      txbl_int + div_ord + div_pref + gross_pens_dist +
      rent - rent_loss,
    total_assets = rowSums(across(all_of(asset_vars))),
    total_debts  = rowSums(across(all_of(debt_vars))),
    net_worth    = total_assets - total_debts,
    total_kg     = rowSums(across(all_of(kg_vars)))
  )

# Percentile bins (PUF-weighted on positive income)
pctiles = Hmisc::wtd.quantile(d$income[d$income > 0], d$weight[d$income > 0],
                              probs = seq(0, 1, 0.01))
d$pctile = findInterval(d$income, pctiles[-101]) + 1L
d$pctile = pmin(d$pctile, 100L)

#---------------------------------------------------------------------------
# Aggregate sanity table — per category, weighted total on PUF vs SCF
#---------------------------------------------------------------------------

diag_path = 'resources/cache/wealth_diagnostics.rds'
if (!file.exists(diag_path)) stop('Wealth diagnostics cache missing: ', diag_path)
wd = read_rds(diag_path)
Y_train = wd$Y_mat  # bootstrap-resampled SCF rows; equal-weighted

puf_agg = sapply(wealth_y_vars, function(v) sum(d[[v]] * d$weight)) / 1e9
scf_agg = colSums(Y_train) * (sum(scf_tax_units$weight) / nrow(Y_train)) / 1e9

agg_tbl = tibble(
  variable = wealth_y_vars,
  puf_bn   = puf_agg,
  scf_bn   = scf_agg,
  rel_gap  = puf_bn / scf_bn - 1
)

cat('==========================================================\n')
cat('  Weighted aggregates: imputed PUF vs SCF training ($B)\n')
cat('  (SCF source: ', scf_source, ')\n', sep = '')
cat('==========================================================\n\n')
cat(sprintf('%-18s | %10s %10s %8s\n', 'variable', 'puf $B', 'scf $B', 'rel gap'))
cat(paste0(rep('-', 55), collapse = ''), '\n')
for (i in seq_len(nrow(agg_tbl))) {
  cat(sprintf('%-18s | %10.1f %10.1f %+7.1f%%\n',
              agg_tbl$variable[i], agg_tbl$puf_bn[i], agg_tbl$scf_bn[i],
              100 * agg_tbl$rel_gap[i]))
}

#---------------------------------------------------------------------------
# Net worth by income percentile group
#---------------------------------------------------------------------------

groups = list(
  'P1-P25'    = c(1, 25),
  'P25-P50'   = c(25, 50),
  'P50-P75'   = c(50, 75),
  'P75-P90'   = c(75, 90),
  'P90-P99'   = c(90, 99),
  'P99+'      = c(99, 100)
)

cat('\n==========================================================\n')
cat('  Net worth by PUF income percentile group\n')
cat('==========================================================\n\n')
cat(sprintf('%-10s | %10s %10s %10s %10s | %8s\n',
            'Group', 'med_NW', 'mean_NW', 'med_A', 'med_D', 'N'))
cat(paste0(rep('-', 75), collapse = ''), '\n')
for (nm in names(groups)) {
  rng = groups[[nm]]
  s = d %>% filter(pctile > rng[1], pctile <= rng[2])
  if (nrow(s) == 0) next
  cat(sprintf('%-10s | %10.0f %10.0f %10.0f %10.0f | %8d\n',
              nm,
              Hmisc::wtd.quantile(s$net_worth,    s$weight, 0.5),
              weighted.mean  (s$net_worth,        s$weight),
              Hmisc::wtd.quantile(s$total_assets, s$weight, 0.5),
              Hmisc::wtd.quantile(s$total_debts,  s$weight, 0.5),
              nrow(s)))
}

cat(sprintf('\nTotal weighted net worth: $%.1f trillion\n',
            sum(d$net_worth * d$weight) / 1e12))
cat(sprintf('Total weighted assets:    $%.1f trillion\n',
            sum(d$total_assets * d$weight) / 1e12))
cat(sprintf('Total weighted debts:     $%.1f trillion\n',
            sum(d$total_debts * d$weight) / 1e12))
cat(sprintf('Share with NW <= 0:       %.1f%%\n',
            100 * sum(d$weight[d$net_worth <= 0]) / sum(d$weight)))

#---------------------------------------------------------------------------
# Plots
#---------------------------------------------------------------------------

plot_dir = 'plots/wealth'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# --- Plot 1: aggregate-gap bar ---
pAG = agg_tbl %>%
  mutate(variable = factor(variable, levels = wealth_y_vars),
         bucket   = case_when(variable %in% asset_vars ~ 'asset',
                              variable %in% debt_vars  ~ 'debt',
                              variable %in% kg_vars    ~ 'kg')) %>%
  ggplot(aes(variable, 100 * rel_gap, fill = bucket)) +
  geom_col() +
  geom_hline(yintercept = 0, color = 'grey40') +
  scale_fill_manual(values = c('asset' = '#4daf4a',
                                'debt'  = '#e41a1c',
                                'kg'    = '#377eb8')) +
  coord_flip() +
  labs(title    = sprintf('Per-category aggregate gap: PUF vs SCF-training (%s)',
                          scf_source),
       subtitle = 'Lower is better. 0% = perfect aggregate match.',
       x = NULL, y = 'PUF / SCF \u2212 1 (%)') +
  theme_minimal(base_size = 10)

ggsave(file.path(plot_dir, '01_aggregate_gap.png'),
       pAG, width = 9, height = 0.35 * nrow(agg_tbl) + 2, dpi = 140)

# --- Plot 2: median net worth by percentile ---
nw_by_p = d %>%
  group_by(pctile) %>%
  summarise(
    med_NW = Hmisc::wtd.quantile(net_worth, weight, 0.5),
    med_A  = Hmisc::wtd.quantile(total_assets, weight, 0.5),
    med_D  = Hmisc::wtd.quantile(total_debts, weight, 0.5),
    n = n(),
    .groups = 'drop'
  )

pNW = nw_by_p %>%
  select(pctile, med_NW, med_A, med_D) %>%
  pivot_longer(-pctile, names_to = 'stat', values_to = 'val') %>%
  ggplot(aes(pctile, val / 1000, color = stat)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c('med_NW' = 'black',
                                'med_A'  = '#4daf4a',
                                'med_D'  = '#e41a1c')) +
  labs(title    = sprintf('Median net worth, assets, and debts by income percentile (%s)',
                          scf_source),
       x = 'Income percentile', y = 'Median ($ thousands, 2022)') +
  theme_minimal(base_size = 10)

ggsave(file.path(plot_dir, '02_nw_by_pctile.png'),
       pNW, width = 9, height = 5, dpi = 140)

# --- Plot 3: per-category marginal QQ (imputed PUF vs SCF-training) ---
qq_df = map_dfr(wealth_y_vars, function(v) {
  puf_q = Hmisc::wtd.quantile(d[[v]],   d$weight, probs = seq(0.01, 0.99, 0.01))
  scf_q = quantile(Y_train[, v],                   probs = seq(0.01, 0.99, 0.01))
  tibble(variable = v, p = seq(0.01, 0.99, 0.01),
         q_puf = as.numeric(puf_q), q_scf = as.numeric(scf_q))
})

pQQ = qq_df %>%
  mutate(variable = factor(variable, levels = wealth_y_vars)) %>%
  ggplot(aes(q_scf / 1000, q_puf / 1000)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = 'grey60') +
  geom_point(size = 0.5, alpha = 0.6, color = 'steelblue') +
  facet_wrap(~ variable, scales = 'free', ncol = 5) +
  labs(title    = sprintf('Per-category QQ: imputed PUF vs SCF training (%s)',
                          scf_source),
       subtitle = 'Each point is one percentile p \u2208 {0.01, ..., 0.99}. 45\u00b0 = perfect match.',
       x = 'SCF quantile ($ thousand)', y = 'PUF quantile ($ thousand)') +
  theme_minimal(base_size = 8) +
  theme(strip.text = element_text(size = 7))

ggsave(file.path(plot_dir, '03_marginal_qq.png'),
       pQQ, width = 12, height = 10, dpi = 140)

# --- Plot 4: joint structure — correlation matrices ---
cor_puf = cor(as.matrix(d[wealth_y_vars]))
cor_scf = cor(Y_train)

cor_long = bind_rows(
  as_tibble(cor_puf, rownames = 'row') %>%
    pivot_longer(-row, names_to = 'col', values_to = 'cor') %>%
    mutate(source = 'PUF'),
  as_tibble(cor_scf, rownames = 'row') %>%
    pivot_longer(-row, names_to = 'col', values_to = 'cor') %>%
    mutate(source = 'SCF')
) %>%
  mutate(row = factor(row, levels = wealth_y_vars),
         col = factor(col, levels = wealth_y_vars))

pCor = ggplot(cor_long, aes(col, row, fill = cor)) +
  geom_tile(color = 'white') +
  scale_fill_gradient2(low = '#2166ac', mid = 'white', high = '#b2182b',
                       midpoint = 0, limits = c(-1, 1),
                       name = 'Pearson r') +
  facet_wrap(~ source, ncol = 2) +
  labs(title    = sprintf('Pairwise correlation: PUF imputed vs SCF training (%s)',
                          scf_source),
       subtitle = 'Joint structure should be similar across the two panels.',
       x = NULL, y = NULL) +
  theme_minimal(base_size = 8) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(plot_dir, '04_joint_correlation.png'),
       pCor, width = 14, height = 7, dpi = 140)

cat(sprintf('\nPlots saved to %s/\n', plot_dir))
cat('  01_aggregate_gap.png       \u2014 per-category $ aggregate gap\n')
cat('  02_nw_by_pctile.png        \u2014 median NW/A/D by income percentile\n')
cat('  03_marginal_qq.png         \u2014 per-category QQ: PUF vs SCF\n')
cat('  04_joint_correlation.png   \u2014 pairwise corr: PUF imputed vs SCF training\n')

cat('\nDone.\n')
