#--------------------------------------
# validate_top_tail.R
#
# Top-tail validation for the power-law adjustment in consumption.R:
#   For PUF units with income > Y_ref (CEX P98 median income), imputed C
#   is scaled by (income / Y_ref) ^ beta_prod. CEX has no support above
#   Y_ref, so this needs a standalone sanity check.
#
# Three diagnostics:
#   (1) Continuity of C(Y) at Y_ref — PUF scatter in $250k-$450k,
#       overlaid LHS LOESS and RHS power-law. Large visible jump at
#       Y_ref would mean the QRF fit near Y_ref and the extrapolated
#       power-law curve disagree.
#   (2) β sensitivity — refit log(C) ~ log(Y) on CEX at P50+, P70+,
#       P80+ (production), P90+, P95+. If β is stable in a tight band,
#       the P80+ choice is innocuous; if it drifts, pick something
#       more robust.
#   (3) Aggregate top-C sanity — weighted sum of C above Y_ref, weighted
#       mean C at top 1% / top 0.1%, and counterfactual with β=0 (no
#       adjustment) to quantify what the adjustment contributes.
#
# Inputs (from a prior test_consumption.R run):
#   resources/cache/consumption_analysis.rds    — PUF w/ C, income, weight, pctile
#   resources/cache/consumption_diagnostics.rds — elasticity_beta, y_ref
#   resources/cache/cex_training_cached.rds     — CEX w/ total_consumption, income
#
# Outputs:
#   plots/20_top_tail_continuity.png
#   plots/21_top_tail_beta_sensitivity.png
#   Console report (tables 1-3).
#--------------------------------------

lapply(c('tidyverse', 'magrittr', 'Hmisc', 'ggplot2'),
       library, character.only = TRUE)

plot_dir = 'plots'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# ---------- Load cached artifacts ----------
diag      = read_rds('resources/cache/consumption_diagnostics.rds')
beta_prod = diag$elasticity_beta
y_ref     = diag$y_ref

puf = read_rds('resources/cache/consumption_analysis.rds')   # pre-filtered: income>0, C>0
cex = read_rds('resources/cache/cex_training_cached.rds')$cex

cat(sprintf('\nProduction params: beta = %.4f, Y_ref = $%.0f\n',
            beta_prod, y_ref))
cat(sprintf('PUF (income>0, C>0): %d rows\n', nrow(puf)))
cat(sprintf('CEX training:        %d rows\n\n', nrow(cex)))


#==============================================================================
# (1) Continuity of C(Y) at Y_ref
#==============================================================================

win = puf %>% filter(income >= 250e3, income <= 450e3, C > 0)
cat('========== (1) Continuity at Y_ref ==========\n')
cat(sprintf('Window ($250k-$450k): %d PUF units\n', nrow(win)))

lhs = win %>% filter(income <  y_ref)
rhs = win %>% filter(income >= y_ref)

# Anchor c_ref = median C in narrow band JUST BELOW y_ref (still QRF-native,
# not yet adjusted). This is what the power-law extrapolates from.
band_lhs = puf %>% filter(income > y_ref * 0.95, income <= y_ref, C > 0)
c_ref = median(band_lhs$C)

# LHS LOESS at y_ref (smoothed QRF value)
lhs_loess     = loess(C ~ income, data = lhs, weights = lhs$weight, span = 0.5)
c_lhs_at_yref = as.numeric(predict(lhs_loess,
                                   newdata = data.frame(income = y_ref)))

gap_pct = (c_ref - c_lhs_at_yref) / c_lhs_at_yref * 100
cat(sprintf('LHS LOESS at Y_ref:                      $%.0f\n', c_lhs_at_yref))
cat(sprintf('Band median C (just below Y_ref):        $%.0f\n', c_ref))
cat(sprintf('Gap (band median - LOESS)/LOESS:         %+.1f%%\n', gap_pct))

# Where does the PUF RHS actually land relative to the power-law?
y_grid   = seq(y_ref, 450e3, length.out = 200)
pl_grid  = c_ref * (y_grid / y_ref) ^ beta_prod

if (nrow(rhs) > 50) {
  rhs_loess   = loess(C ~ income, data = rhs, weights = rhs$weight, span = 0.5)
  pl_at_350   = c_ref * (350e3 / y_ref) ^ beta_prod
  rhs_at_350  = as.numeric(predict(rhs_loess,
                                   newdata = data.frame(income = 350e3)))
  cat(sprintf('At $350k:   power-law = $%.0f,  PUF LOESS = $%.0f  (%+.1f%%)\n',
              pl_at_350, rhs_at_350,
              (rhs_at_350 - pl_at_350) / pl_at_350 * 100))
}
cat('\n')

# ---------- Plot 20 ----------
set.seed(42)
samp = win %>% sample_n(min(5000, nrow(win)))

p1 = ggplot() +
  geom_point(data = samp, aes(income / 1e3, C / 1e3),
             alpha = 0.15, size = 0.6, color = 'grey30') +
  geom_vline(xintercept = y_ref / 1e3, linetype = 2, color = 'red',
             linewidth = 0.6) +
  geom_line(data = data.frame(income = y_grid, C = pl_grid),
            aes(income / 1e3, C / 1e3), color = '#d7191c', linewidth = 1.1) +
  geom_smooth(data = lhs, aes(income / 1e3, C / 1e3), method = 'loess',
              se = FALSE, color = '#2c7bb6', linewidth = 1.1, span = 0.5) +
  geom_smooth(data = rhs, aes(income / 1e3, C / 1e3), method = 'loess',
              se = FALSE, color = '#1a9850', linewidth = 1.1, span = 0.5,
              linetype = 'longdash') +
  annotate('text', x = y_ref / 1e3 + 3,
           y = quantile(samp$C, 0.95, na.rm = TRUE) / 1e3,
           label = sprintf('Y_ref = $%dk', round(y_ref / 1e3)),
           hjust = 0, color = 'red', size = 3.3) +
  labs(title    = '(1) Top-tail continuity: PUF C vs income around Y_ref',
       subtitle = sprintf('LHS LOESS at Y_ref = $%.0fk  |  anchor c_ref = $%.0fk  |  gap = %+.1f%%  |  beta = %.3f',
                          c_lhs_at_yref / 1e3, c_ref / 1e3, gap_pct, beta_prod),
       caption  = 'Blue: LHS LOESS (QRF-native).  Red: power-law extrapolation.  Green dashed: PUF RHS LOESS (should hug red if OK).',
       x = 'Income ($k, 2017)', y = 'Imputed C ($k, 2017)') +
  theme_minimal(base_size = 11)

ggsave(file.path(plot_dir, '20_top_tail_continuity.png'), p1,
       width = 9.5, height = 5.5, dpi = 140)


#==============================================================================
# (2) beta sensitivity on CEX
#==============================================================================

cex_h = cex %>% filter(has_income == 1, income > 0, total_consumption > 0)

fit_beta = function(pct_floor) {
  sub = cex_h %>% filter(pctile_income >= pct_floor)
  m   = lm(log(total_consumption) ~ log(income), data = sub,
           weights = sub$WT_ANNUAL)
  tibble(pct_floor = pct_floor,
         beta      = unname(coef(m)['log(income)']),
         se        = sqrt(vcov(m)['log(income)', 'log(income)']),
         n         = nrow(sub),
         r2        = summary(m)$r.squared)
}

beta_tbl = bind_rows(lapply(c(50, 70, 80, 90, 95), fit_beta))

cat('========== (2) beta sensitivity (CEX) ==========\n')
cat(sprintf('%-11s %8s %8s %10s %8s\n',
            'pct_floor', 'beta', 'se', 'n', 'r2'))
cat(paste0(rep('-', 52), collapse = ''), '\n')
for (i in seq_len(nrow(beta_tbl))) {
  cat(sprintf('P%-10d %8.4f %8.4f %10d %8.3f\n',
              beta_tbl$pct_floor[i], beta_tbl$beta[i], beta_tbl$se[i],
              beta_tbl$n[i], beta_tbl$r2[i]))
}
cat(sprintf('\nProduction beta (P80+): %.4f\n', beta_prod))
cat(sprintf('Range across specs:     [%.3f, %.3f]\n',
            min(beta_tbl$beta), max(beta_tbl$beta)))
cat(sprintf('Max |deviation| from P80+: %.3f\n\n',
            max(abs(beta_tbl$beta - beta_prod))))

p2 = ggplot(beta_tbl, aes(factor(pct_floor), beta)) +
  geom_hline(yintercept = beta_prod, linetype = 2, color = 'grey30') +
  geom_linerange(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
                 color = '#2c7bb6', linewidth = 0.8) +
  geom_point(size = 3.5, color = '#2c7bb6') +
  geom_text(aes(label = sprintf('%.3f', beta)), nudge_x = 0.22, size = 3.2) +
  labs(title    = '(2) Elasticity beta across CEX percentile floors',
       subtitle = sprintf('Dashed line: production beta (P80+) = %.3f.  Lines: +/- 1.96 * SE.',
                          beta_prod),
       caption  = 'If beta drifts materially across floors, the P80+ choice is not robust.',
       x = 'Percentile floor', y = expression(beta)) +
  theme_minimal(base_size = 11)

ggsave(file.path(plot_dir, '21_top_tail_beta_sensitivity.png'), p2,
       width = 7.5, height = 4.5, dpi = 140)


#==============================================================================
# (3) Aggregate top-C sanity
#==============================================================================

# 2017 NIPA PCE total: ~$13.3T. PUF here is pre-benchmarking, so we only
# check order of magnitude.
nipa_2017 = 13.3e12

agg_total = sum(puf$C * puf$weight)
cat('========== (3) Aggregate top-C sanity (PUF, pre-benchmark) ==========\n')
cat(sprintf('Aggregate C (all filtered PUF): $%.0f B\n', agg_total / 1e9))
cat(sprintf('As fraction of 2017 NIPA PCE:   %.1f%%  (expected < 100%% pre-benchmark)\n\n',
            agg_total / nipa_2017 * 100))

slices = list(
  c('All',             expression(rep(TRUE, nrow(puf)))),
  c('P50+',            expression(pctile >= 50)),
  c('P80+',            expression(pctile >= 80)),
  c('P90+',            expression(pctile >= 90)),
  c('P95+',            expression(pctile >= 95)),
  c('P99+',            expression(pctile >= 99)),
  c('P99.9+',          expression(pctile_fine > 102)),
  c('income > Y_ref',  expression(income >  y_ref))
)

cat(sprintf('%-18s %10s %12s %14s %12s\n',
            'Slice', 'N (M)', 'Agg C ($B)', 'Mean C ($k)', 'Share (%)'))
cat(paste0(rep('-', 72), collapse = ''), '\n')
for (s in slices) {
  idx = eval(s[[2]], envir = puf)
  w   = puf$weight[idx]
  C   = puf$C[idx]
  agg = sum(C * w)
  cat(sprintf('%-18s %10.2f %12.0f %14.0f %12.1f\n',
              s[[1]], sum(w) / 1e6, agg / 1e9,
              agg / sum(w) / 1e3,
              agg / agg_total * 100))
}

# Counterfactual: if beta = 0 (no top-tail adjustment), what would aggregate C be?
# Backward inversion: C_QRF = C_observed / (income / Y_ref)^beta for adjusted units.
cat('\n---- Counterfactual: what does the adjustment add? ----\n')
adj  = puf$income > y_ref
cf_C = puf$C
cf_C[adj] = cf_C[adj] / (puf$income[adj] / y_ref) ^ beta_prod
cf_agg = sum(cf_C * puf$weight)
delta  = agg_total - cf_agg

cat(sprintf('Aggregate C with production beta = %.3f:  $%.0f B\n',
            beta_prod, agg_total / 1e9))
cat(sprintf('Aggregate C with beta = 0 (no adjustment): $%.0f B\n', cf_agg / 1e9))
cat(sprintf('Adjustment contributes:                    $%.0f B  (+%.1f%%)\n',
            delta / 1e9, delta / cf_agg * 100))

# Also report: within the adjusted set, mean adj factor (weighted)
adj_fac = (puf$income[adj] / y_ref) ^ beta_prod
cat(sprintf('\nWithin adjusted set (%d units, %.1f M weighted):\n',
            sum(adj), sum(puf$weight[adj]) / 1e6))
cat(sprintf('  Weighted mean adj factor:   %.3f\n',
            weighted.mean(adj_fac, puf$weight[adj])))
cat(sprintf('  Median adj factor:          %.3f\n', median(adj_fac)))
cat(sprintf('  P95 adj factor:             %.3f\n',
            quantile(adj_fac, 0.95)))
cat(sprintf('  Max adj factor (income = $%.1f M): %.3f\n',
            max(puf$income[adj]) / 1e6, max(adj_fac)))

# Quick SCF-ballpark check
p99_C = Hmisc::wtd.quantile(puf$C[puf$pctile >= 99], puf$weight[puf$pctile >= 99], 0.5)
top1_mean = weighted.mean(puf$C[puf$pctile >= 99], puf$weight[puf$pctile >= 99])
cat(sprintf('\nPUF top 1%% mean C: $%.0fk  (SCF ballpark 2017: ~$200-300k)\n',
            top1_mean / 1e3))
cat(sprintf('PUF top 1%% med C:  $%.0fk\n', p99_C / 1e3))

cat('\n========== Done ==========\n')
cat(sprintf('Plots: %s/{20_top_tail_continuity.png, 21_top_tail_beta_sensitivity.png}\n',
            plot_dir))
