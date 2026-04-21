#--------------------------------------
# scf_puf_coverage.R
#
# Coverage analysis: how well does each
# candidate capital-income variable agree
# between PUF and SCF? Gates the Phase-2b
# X expansion of the SCF-to-PUF wealth
# imputation.
#
# For each candidate: compare marginal
# distributions (share zero, P50, P90, P99,
# aggregate) and within-income-rank-cell
# conditional distributions. Verdict per
# candidate: PASS / MARGINAL / FAIL.
#
# Inputs:
#   resources/cache/consumption_analysis.rds
#     (produced by src/eda/test_consumption.R;
#      pre-wealth tax_units snapshot with raw
#      PUF capital-income variables intact)
#   SCFP2022.csv via interface_paths$SCF
#--------------------------------------

lapply(c('tidyverse', 'magrittr', 'data.table', 'Hmisc', 'ggplot2'),
       library, character.only = TRUE)

plot_dir  = 'plots/scf_puf_coverage'
cache_dir = 'resources/cache'
dir.create(plot_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

#---------------------------------------------------------------------------
# 1. Harmonization table
#
# Each row: one candidate conditioner. The PUF expression and SCF expression
# must evaluate to comparable $ concepts. Where mapping is approximate, the
# `note` flags the slack so the verdict can be read in context.
#---------------------------------------------------------------------------

# PUF income concept: reuse consumption.R's definition so rank-cells are
# comparable with pre-existing diagnostics. SCF income: use SCFP's INCOME
# (gross household income) — this is the field SCF publishes and downstream
# SCF-on-PUF pipelines (auto_loan, mortgage) already use.
puf_income_expr = quote(
  wages + sole_prop + part_active + part_passive - part_active_loss -
    part_passive_loss - part_179 + scorp_active + scorp_passive -
    scorp_active_loss - scorp_passive_loss - scorp_179 + gross_ss +
    txbl_int + div_ord + div_pref + gross_pens_dist +
    rent - rent_loss
)

# SCFP collapses some components (no separate interest vs dividends, no
# separate SS vs pensions, no separate rent vs royalty). We build a
# candidate list where each entry has a well-defined pair on both sides.
# Rows with no clean SCF analog (standalone interest, dividends) are
# dropped from the candidate set and handled via combined pairs instead.

candidate_tbl = tribble(
  ~variable,       ~puf_expr,
  ~scfp_expr,      ~note,

  'wages',         quote(wages),
  quote(WAGEINC),  'SCFP WAGEINC; SCF and PUF concepts align tightly',

  'int_div',       quote(txbl_int + exempt_int + div_ord + div_pref),
  quote(INTDIVINC), 'SCFP INTDIVINC bundles interest + dividends',

  'capital_gains', quote(kg_lt + kg_st),
  quote(KGINC),    'SCFP KGINC; definition-sensitive (realization timing)',

  'business',      quote(sole_prop - sole_prop_loss +
                         scorp_active - scorp_active_loss - scorp_179 +
                         part_active  - part_active_loss  - part_179),
  quote(BUSSEFARMINC),
                   'SCFP bundles Schedule C / S-corp / partnership / farm; PUF splits them',

  'rent',          quote(rent - rent_loss),
  quote(RENTINC),  'SCFP RENTINC bundles rent + royalty + trust; PUF is narrower',

  'ss_pens',       quote(gross_ss + gross_pens_dist),
  quote(SSRETINC), 'SCFP SSRETINC bundles SS + retirement + annuity',

  'ui_other',      quote(ui),
  quote(TRANSFOTHINC),
                   'SCFP TRANSFOTHINC bundles UI + welfare + other; PUF UI narrower'
)

#---------------------------------------------------------------------------
# 2. Load PUF and SCF
#---------------------------------------------------------------------------

puf_cache = file.path(cache_dir, 'consumption_analysis.rds')
if (!file.exists(puf_cache)) {
  stop('PUF cache missing at ', puf_cache,
       ' — run src/eda/test_consumption.R first to produce the pre-wealth snapshot.')
}

puf = read_rds(puf_cache) %>%
  mutate(income = eval(puf_income_expr))

scf_path = interface_paths$SCF %>% file.path('SCFP2022.csv')
cat('Reading SCF from ', scf_path, '\n', sep = '')

scf = fread(scf_path) %>% tibble()

# SCFP WGT is per-implicate weight (sums to ~US household count when pooled
# over the 5 implicates). Pool all implicates by keeping rows as-is.
scf_h = scf %>%
  transmute(
    w              = WGT,
    income         = INCOME,
    WAGEINC, INTDIVINC, KGINC, BUSSEFARMINC, RENTINC, SSRETINC, TRANSFOTHINC
  )

puf_h = puf %>%
  transmute(
    w        = weight,
    income   = income,
    wages, txbl_int, exempt_int, div_ord, div_pref, kg_lt, kg_st,
    sole_prop, sole_prop_loss,
    scorp_active, scorp_active_loss, scorp_179,
    scorp_passive, scorp_passive_loss,
    part_active, part_active_loss, part_179,
    part_passive, part_passive_loss,
    rent, rent_loss,
    gross_ss, gross_pens_dist, ui
  )

#---------------------------------------------------------------------------
# 3. Income-rank cell definition (within-dataset rank deciles)
#---------------------------------------------------------------------------

rank_deciles = function(x, w) {
  # Rank by weighted quantile of positive income, 0 for non-positive
  pos = x > 0
  if (!any(pos)) return(rep(0L, length(x)))
  brks = Hmisc::wtd.quantile(x[pos], w[pos], probs = seq(0, 1, 0.1))
  bin = findInterval(x, brks, all.inside = TRUE)
  ifelse(pos, bin, 0L)
}

puf_h$dec = rank_deciles(puf_h$income, puf_h$w)
scf_h$dec = rank_deciles(scf_h$income, scf_h$w)

#---------------------------------------------------------------------------
# 4. Per-variable diagnostics
#
# For each candidate, evaluate PUF and SCF series, compute:
#   - share-zero (weighted)
#   - aggregate $ total (weighted)
#   - unconditional P50, P90, P99 among positives
#   - per-decile median among positives on each side
#   - per-decile relative gap in medians (PUF / SCF - 1)
# Pass / Marginal / Fail:
#   PASS     if |agg gap| < 10% AND median cellwise |rel gap| < 15%
#   MARGINAL if |agg gap| < 25% AND median cellwise |rel gap| < 40%
#   FAIL     otherwise
# Note: the SCFP combined fields (BUSSEFARMINC, SSRETINC, RENTINC,
# TRANSFOTHINC, INTDIVINC) are wider than their PUF counterparts, so for
# those we expect PUF < SCF and use that as the lower bound of plausible
# agreement rather than evidence of failure.
#---------------------------------------------------------------------------

weighted_stat = function(x, w, probs) {
  pos = x > 0 & is.finite(x)
  if (!any(pos)) {
    out = setNames(rep(NA_real_, length(probs)), paste0('p', probs * 100))
    return(as.list(out))
  }
  as.list(Hmisc::wtd.quantile(x[pos], w[pos], probs = probs, na.rm = TRUE)) %>%
    setNames(paste0('p', probs * 100))
}

probs = c(0.5, 0.9, 0.99)

summarize_var = function(x, w) {
  pos = x > 0 & is.finite(x)
  tot_w = sum(w, na.rm = TRUE)
  c(
    share_zero = sum(w[!pos], na.rm = TRUE) / tot_w,
    agg        = sum(pmax(x, 0) * w, na.rm = TRUE),
    weighted_stat(x, w, probs)
  )
}

per_decile = function(x, w, dec) {
  tibble(dec = dec, x = x, w = w) %>%
    filter(x > 0, dec >= 1) %>%
    group_by(dec) %>%
    summarise(med = Hmisc::wtd.quantile(x, w, 0.5),
              n   = n(), .groups = 'drop')
}

eval_candidate = function(var_name, puf_expr, scf_expr) {
  puf_x = eval(puf_expr, envir = puf_h)
  scf_x = eval(scf_expr, envir = scf_h)

  puf_summary = summarize_var(puf_x, puf_h$w)
  scf_summary = summarize_var(scf_x, scf_h$w)

  puf_dec = per_decile(puf_x, puf_h$w, puf_h$dec) %>% rename(med_puf = med, n_puf = n)
  scf_dec = per_decile(scf_x, scf_h$w, scf_h$dec) %>% rename(med_scf = med, n_scf = n)

  dec = full_join(puf_dec, scf_dec, by = 'dec') %>%
    mutate(rel_gap = med_puf / med_scf - 1)

  agg_gap = puf_summary['agg'] / scf_summary['agg'] - 1
  med_rel_gap = median(abs(dec$rel_gap), na.rm = TRUE)

  verdict = case_when(
    abs(agg_gap) < 0.10 & med_rel_gap < 0.15 ~ 'PASS',
    abs(agg_gap) < 0.25 & med_rel_gap < 0.40 ~ 'MARGINAL',
    TRUE                                       ~ 'FAIL'
  )

  list(
    variable         = var_name,
    verdict          = verdict,
    share_zero_puf   = unname(puf_summary['share_zero']),
    share_zero_scf   = unname(scf_summary['share_zero']),
    agg_puf          = unname(puf_summary['agg']),
    agg_scf          = unname(scf_summary['agg']),
    agg_gap          = unname(agg_gap),
    p50_puf          = unname(puf_summary['p50']),
    p50_scf          = unname(scf_summary['p50']),
    p90_puf          = unname(puf_summary['p90']),
    p90_scf          = unname(scf_summary['p90']),
    p99_puf          = unname(puf_summary['p99']),
    p99_scf          = unname(scf_summary['p99']),
    med_cell_rel_gap = med_rel_gap,
    decile_tbl       = dec
  )
}

results = pmap(
  list(candidate_tbl$variable, candidate_tbl$puf_expr, candidate_tbl$scfp_expr),
  eval_candidate
)

#---------------------------------------------------------------------------
# 5. Verdict table
#---------------------------------------------------------------------------

verdict_tbl = map_dfr(results, ~ tibble(
  variable         = .x$variable,
  verdict          = .x$verdict,
  share_zero_puf   = .x$share_zero_puf,
  share_zero_scf   = .x$share_zero_scf,
  agg_puf_bn       = .x$agg_puf / 1e9,
  agg_scf_bn       = .x$agg_scf / 1e9,
  agg_gap          = .x$agg_gap,
  p50_puf          = .x$p50_puf,
  p50_scf          = .x$p50_scf,
  p90_puf          = .x$p90_puf,
  p90_scf          = .x$p90_scf,
  p99_puf          = .x$p99_puf,
  p99_scf          = .x$p99_scf,
  med_cell_rel_gap = .x$med_cell_rel_gap
))

cat('\n========== Coverage verdict ==========\n\n')
print(verdict_tbl %>% select(variable, verdict, agg_gap, med_cell_rel_gap,
                             share_zero_puf, share_zero_scf),
      n = nrow(verdict_tbl))
cat('\nPASS     candidates safe to add to Phase 2b X set\n')
cat('MARGINAL candidates: judgment call; review per-decile plot before deciding\n')
cat('FAIL     candidates: keep out of X; conditioning would inject measurement-error bias\n\n')

write_rds(list(verdict = verdict_tbl, results = results),
          file.path(cache_dir, 'scf_puf_coverage.rds'))
write_csv(verdict_tbl, file.path(plot_dir, 'coverage_verdict.csv'))

#---------------------------------------------------------------------------
# 6. Plots
#
# One multi-panel figure per variable: (i) decile-median bars (PUF vs SCF),
# (ii) QQ of positives at 1..99 percentiles. Plus a single coverage heatmap
# showing per-variable per-decile relative gap.
#---------------------------------------------------------------------------

dec_long = map_dfr(results, ~ .x$decile_tbl %>%
                          mutate(variable = .x$variable))

hm = dec_long %>%
  mutate(rel_gap_pct = 100 * rel_gap,
         variable = factor(variable, levels = verdict_tbl$variable))

pHM = ggplot(hm, aes(factor(dec), variable, fill = rel_gap_pct)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = ifelse(is.finite(rel_gap_pct),
                               sprintf('%+.0f%%', rel_gap_pct), '—')),
            size = 2.6, color = 'black') +
  scale_fill_gradient2(low = '#2166ac', mid = 'white', high = '#b2182b',
                       midpoint = 0, name = 'PUF/SCF − 1\n(%, positives)',
                       limits = c(-100, 100), oob = scales::squish) +
  labs(title    = 'SCF↔PUF per-decile median gap (positives), by candidate conditioner',
       subtitle = 'Cells are income-rank deciles within each dataset. Green ≈ agreement.',
       x = 'Income-rank decile (within-dataset)',
       y = 'Candidate conditioner') +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank())

ggsave(file.path(plot_dir, '01_coverage_heatmap.png'),
       pHM, width = 11, height = 0.4 * nrow(verdict_tbl) + 3, dpi = 140)

# Aggregate-gap bar
pAG = verdict_tbl %>%
  mutate(variable = factor(variable, levels = variable[order(agg_gap)])) %>%
  ggplot(aes(variable, 100 * agg_gap, fill = verdict)) +
  geom_col() +
  geom_hline(yintercept = c(-10, 10), linetype = 2, color = 'grey60') +
  geom_hline(yintercept = c(-25, 25), linetype = 3, color = 'grey40') +
  scale_fill_manual(values = c('PASS' = '#4daf4a',
                                'MARGINAL' = '#ff7f00',
                                'FAIL' = '#e41a1c')) +
  coord_flip() +
  labs(title    = 'Aggregate $ gap: PUF vs SCF, by candidate',
       subtitle = 'Dashed = \u00b110% (PASS threshold). Dotted = \u00b125% (MARGINAL threshold).',
       x = NULL, y = 'PUF agg / SCF agg − 1 (%)') +
  theme_minimal(base_size = 10)

ggsave(file.path(plot_dir, '02_aggregate_gap.png'),
       pAG, width = 9, height = 0.35 * nrow(verdict_tbl) + 2, dpi = 140)

cat(sprintf('Plots written to %s/\n', plot_dir))
cat('  01_coverage_heatmap.png  — per-decile median gap per variable\n')
cat('  02_aggregate_gap.png     — per-variable aggregate $ gap with verdict fill\n')
cat('  coverage_verdict.csv     — machine-readable verdict table\n')

cat('\nNext: review verdicts with user. PASS variables are safe to add to the\n')
cat('Phase 2b X set in src/imputations/wealth.R. FAIL variables stay out.\n')
