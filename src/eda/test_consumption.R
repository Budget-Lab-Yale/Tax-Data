#--------------------------------------
# test_consumption.R
#
# Runs the minimal pipeline up through
# consumption imputation, saves data,
# and generates diagnostic plots
#--------------------------------------

# Load packages and configure
lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')
estimate_models = 1  # re-estimate DRF with mtry fix
save_consumption_diagnostics = TRUE
set.seed(76)

# Run minimal pipeline up to consumption
source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
source('./src/impute_nonfilers.R')

# Imputation modules needed before consumption
source('./src/imputations/helpers.R')
source('./src/imputations/demographics.R')
source('./src/imputations/ages.R')

# Run consumption imputation
cat("\n========== Running consumption imputation ==========\n\n")
source('./src/imputations/consumption.R')
cat("\n========== Consumption imputation complete ==========\n\n")

# Load back the saved diagnostics (y_ref and elasticity_beta got rm'd)
diag = read_rds('resources/cache/consumption_diagnostics.rds')
elasticity_beta = diag$elasticity_beta
y_ref = diag$y_ref

cat(sprintf("elasticity_beta = %.4f, y_ref = $%.0f\n\n", elasticity_beta, y_ref))

# Build analysis dataset. C is not a stored field on tax_units (production
# drops total C after benchmarking); reconstruct as sum of the 8 c_*
# categories for diagnostic purposes.
d = tax_units %>%
  mutate(
    income = wages + sole_prop + part_active + part_passive - part_active_loss -
      part_passive_loss - part_179 + scorp_active + scorp_passive -
      scorp_active_loss - scorp_passive_loss - scorp_179 + gross_ss +
      txbl_int + div_ord + div_pref + gross_pens_dist +
      rent - rent_loss,
    C = c_clothing + c_motor_vehicles + c_durables + c_other_nondurables +
        c_food_off_premises + c_gasoline + c_housing_utilities + c_other_services_health
  ) %>%
  filter(income > 0, C > 0) %>%
  mutate(CY = C / income)

# Income percentiles (PUF-weighted)
pctiles = Hmisc::wtd.quantile(d$income, d$weight, probs = seq(0, 1, 0.01))
d$pctile = findInterval(d$income, pctiles[-101]) + 1L
d$pctile = pmin(d$pctile, 100L)

# Fine percentiles for top
pctiles_fine = Hmisc::wtd.quantile(d$income, d$weight,
  probs = c(seq(0, 0.99, 0.01), 0.995, 0.999, 1))
d$pctile_fine = findInterval(d$income, pctiles_fine[-length(pctiles_fine)]) + 1L

# Save analysis data for quick re-plotting
write_rds(d, 'resources/cache/consumption_analysis.rds')

#---------------------------------------------------------------------------
# Summary table
#---------------------------------------------------------------------------

groups = list(
  "P1-P25"     = c(1, 25),
  "P25-P50"    = c(25, 50),
  "P50-P75"    = c(50, 75),
  "P75-P90"    = c(75, 90),
  "P90-P95"    = c(90, 95),
  "P95-P99"    = c(95, 99),
  "P99-P99.5"  = c(99, 99.5),
  "P99.5-P100" = c(99.5, 100)
)

cat("==========================================================\n")
cat("  C/Y Ratios by PUF Income Percentile Group\n")
cat("==========================================================\n\n")

cat(sprintf("%-14s | %10s %10s %10s %10s %10s | %8s\n",
            "Group", "med_Y", "med_C", "med_CY", "mean_CY", "wtd_C_tot", "N"))
cat(paste0(rep("-", 85), collapse = ""), "\n")

for (nm in names(groups)) {
  rng = groups[[nm]]
  if (rng[2] <= 99) {
    s = d %>% filter(pctile > rng[1], pctile <= rng[2])
  } else if (rng[1] == 99 & rng[2] == 99.5) {
    s = d %>% filter(pctile_fine > 99, pctile_fine <= 101)
  } else {
    s = d %>% filter(pctile_fine > 101)
  }
  if (nrow(s) == 0) next
  cy = s$CY
  cat(sprintf("%-14s | %10.0f %10.0f %10.3f %10.3f %10.1f | %8d\n",
              nm,
              Hmisc::wtd.quantile(s$income, s$weight, 0.5),
              Hmisc::wtd.quantile(s$C, s$weight, 0.5),
              Hmisc::wtd.quantile(cy, s$weight, 0.5),
              weighted.mean(cy, s$weight),
              sum(s$C * s$weight) / 1e9,
              nrow(s)))
}

total_C = sum(d$C * d$weight) / 1e9
cat(sprintf("\nTotal weighted consumption (positive income): $%.1f billion\n\n", total_C))

#---------------------------------------------------------------------------
# Top-tail spot checks
#---------------------------------------------------------------------------

cat("==========================================================\n")
cat("  Top-tail spot checks (top 20 by income)\n")
cat("==========================================================\n\n")

cat(sprintf("Y_ref = $%.0f, elasticity_beta = %.4f\n\n", y_ref, elasticity_beta))

top = d %>% arrange(desc(income)) %>% head(20)
top$adj_factor = ifelse(top$income > y_ref, (top$income / y_ref) ^ elasticity_beta, 1.0)

cat(sprintf("%-10s %12s %12s %8s %10s\n", "ID", "Income", "C", "C/Y", "Adj Factor"))
cat(paste0(rep("-", 60), collapse = ""), "\n")
for (i in 1:nrow(top)) {
  cat(sprintf("%-10s %12.0f %12.0f %8.3f %10.2f\n",
              top$id[i], top$income[i], top$C[i], top$CY[i], top$adj_factor[i]))
}

#---------------------------------------------------------------------------
# PLOTS
#---------------------------------------------------------------------------

plot_dir = 'plots'
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# --- Plot 1: Median C and C/Y by percentile ---
pct_summary = d %>%
  group_by(pctile) %>%
  summarise(
    med_Y  = Hmisc::wtd.quantile(income, weight, 0.5),
    med_C  = Hmisc::wtd.quantile(C, weight, 0.5),
    med_CY = Hmisc::wtd.quantile(CY, weight, 0.5),
    mean_CY = weighted.mean(CY, weight),
    n = n(),
    .groups = 'drop'
  )

png(file.path(plot_dir, '01_median_consumption_by_pctile.png'), width = 900, height = 500)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))

plot(pct_summary$pctile, pct_summary$med_C / 1000, type = 'l', lwd = 2, col = 'steelblue',
     xlab = 'Income Percentile', ylab = 'Median Consumption ($k, 2017)',
     main = 'Median Consumption by Income Percentile')
abline(v = which.min(abs(pctiles - y_ref)), lty = 2, col = 'red')
text(which.min(abs(pctiles - y_ref)) + 2, max(pct_summary$med_C/1000) * 0.9,
     paste0('Y_ref = $', round(y_ref/1000), 'k'), col = 'red', adj = 0, cex = 0.8)

plot(pct_summary$pctile, pct_summary$med_CY, type = 'l', lwd = 2, col = 'darkred',
     xlab = 'Income Percentile', ylab = 'Median C/Y Ratio',
     main = 'Median C/Y Ratio by Income Percentile', ylim = c(0, 2))
abline(h = 1, lty = 3, col = 'grey50')
abline(v = which.min(abs(pctiles - y_ref)), lty = 2, col = 'red')

dev.off()

# --- Plot 2: Log-log scatter with top-tail adjustment ---
set.seed(42)
samp = d %>% sample_n(min(20000, nrow(d)))

png(file.path(plot_dir, '02_loglog_consumption_income.png'), width = 800, height = 600)
par(mar = c(5, 5, 3, 2))

plot(log10(samp$income), log10(samp$C),
     pch = '.', col = rgb(0.2, 0.4, 0.7, 0.15), cex = 2,
     xlab = 'Log10(Income)', ylab = 'Log10(Consumption)',
     main = 'Consumption vs Income (log-log scale)',
     xlim = c(3, 7.5), ylim = c(3, 6.5))

# Add 45-degree line (C = Y)
abline(0, 1, lty = 2, col = 'grey50')
text(6.5, 6.7, 'C = Y', col = 'grey50', cex = 0.8)

# Add Y_ref line
abline(v = log10(y_ref), lty = 2, col = 'red', lwd = 1.5)
text(log10(y_ref) + 0.05, 6.3, paste0('Y_ref\n$', round(y_ref/1000), 'k'),
     col = 'red', adj = 0, cex = 0.8)

# Add parametric model line for top tail
y_grid = 10^seq(log10(y_ref), 7.5, length.out = 100)
# Get approximate C_hat at y_ref from data
c_at_ref = median(d$C[d$income > y_ref * 0.9 & d$income < y_ref * 1.1])
c_grid = c_at_ref * (y_grid / y_ref) ^ elasticity_beta
lines(log10(y_grid), log10(c_grid), col = 'red', lwd = 2)
text(6.8, log10(c_grid[length(c_grid)]) + 0.1,
     paste0('C ~ Y^', round(elasticity_beta, 2)), col = 'red', cex = 0.8)

dev.off()

# --- Plot 3: C/Y ratio vs income (top half) ---
top_half = d %>% filter(pctile >= 50)

png(file.path(plot_dir, '03_cy_ratio_vs_income_top.png'), width = 800, height = 500)
par(mar = c(5, 5, 3, 2))

set.seed(42)
samp2 = top_half %>% sample_n(min(15000, nrow(top_half)))

plot(log10(samp2$income), samp2$CY,
     pch = '.', col = rgb(0.6, 0.2, 0.2, 0.2), cex = 2,
     xlab = 'Log10(Income)', ylab = 'C/Y Ratio',
     main = 'Consumption-to-Income Ratio (P50+)',
     ylim = c(0, 2))

# Smoothed median line by percentile
lines(log10(pct_summary$med_Y[pct_summary$pctile >= 50]),
      pct_summary$med_CY[pct_summary$pctile >= 50],
      col = 'darkred', lwd = 3)

abline(h = 1, lty = 3, col = 'grey50')
abline(v = log10(y_ref), lty = 2, col = 'red')
text(log10(y_ref) + 0.05, 1.8, paste0('Y_ref = $', round(y_ref/1000), 'k'),
     col = 'red', adj = 0, cex = 0.8)

dev.off()

# --- Plot 4: Consumption composition by income group ---
pce_cats = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
             'c_food_off_premises', 'c_gasoline', 'c_housing_utilities', 'c_other_services_health')

comp = d %>%
  mutate(inc_group = case_when(
    pctile <= 25 ~ 'P0-25',
    pctile <= 50 ~ 'P25-50',
    pctile <= 75 ~ 'P50-75',
    pctile <= 90 ~ 'P75-90',
    pctile <= 99 ~ 'P90-99',
    TRUE         ~ 'P99+'
  )) %>%
  mutate(inc_group = factor(inc_group, levels = c('P0-25','P25-50','P50-75','P75-90','P90-99','P99+'))) %>%
  group_by(inc_group) %>%
  summarise(across(all_of(pce_cats), ~ weighted.mean(.x / C, weight)), .groups = 'drop')

shares_mat = as.matrix(comp[pce_cats])
rownames(shares_mat) = comp$inc_group

png(file.path(plot_dir, '04_consumption_composition.png'), width = 900, height = 500)
par(mar = c(5, 5, 3, 10), xpd = TRUE)

cols = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3',
         '#ff7f00', '#a65628', '#f781bf', '#999999')

barplot(t(shares_mat), col = cols, names.arg = rownames(shares_mat),
        xlab = 'Income Group', ylab = 'Share of Consumption',
        main = 'Consumption Composition by Income Group',
        legend.text = gsub('_', ' ', pce_cats),
        args.legend = list(x = 'right', inset = c(-0.35, 0), cex = 0.7, bty = 'n'))

dev.off()

# --- Plot 5: Adjustment factor distribution ---
d$adj_factor = ifelse(d$income > y_ref, (d$income / y_ref) ^ elasticity_beta, 1.0)
adjusted = d %>% filter(adj_factor > 1)

png(file.path(plot_dir, '05_adjustment_factor.png'), width = 800, height = 500)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))

hist(adjusted$adj_factor, breaks = 50, col = 'steelblue', border = 'white',
     main = paste0('Top-Tail Adjustment Factor\n(', nrow(adjusted), ' units above Y_ref)'),
     xlab = 'Adjustment Factor', ylab = 'Count')
abline(v = 1, lty = 2, col = 'red')

plot(log10(adjusted$income), adjusted$adj_factor, pch = '.', cex = 2,
     col = rgb(0.2, 0.4, 0.7, 0.3),
     xlab = 'Log10(Income)', ylab = 'Adjustment Factor',
     main = 'Adjustment Factor vs Income')
abline(h = 1, lty = 2, col = 'grey50')

dev.off()

cat(sprintf("\nPlots saved to %s/\n", plot_dir))
cat("Done.\n")
