#--------------------------------------
# Compare CEX raw composition vs DRF-imputed PUF composition
#--------------------------------------

lapply(c('tidyverse', 'magrittr', 'data.table', 'Hmisc'), library, character.only = TRUE)

pce_cats = c('c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables',
             'c_food_off_premises', 'c_gasoline', 'c_housing_utilities', 'c_other_services_health')

plot_dir = 'plots'

#---------------------------------------------------------------------------
# Part 1: Raw CEX composition by income group
#---------------------------------------------------------------------------

source('src/cex.R')
cex = build_cex_training()

cex_pos = cex %>%
  filter(has_income == 1, income > 0, total_consumption > 0) %>%
  mutate(
    pctile = findInterval(income,
      wtd.quantile(income, WT_ANNUAL, probs = seq(0, 1, 0.01))[-101]) + 1L,
    pctile = pmin(pctile, 100L),
    inc_group = case_when(
      pctile <= 25 ~ 'P0-25',
      pctile <= 50 ~ 'P25-50',
      pctile <= 75 ~ 'P50-75',
      pctile <= 90 ~ 'P75-90',
      pctile <= 99 ~ 'P90-99',
      TRUE         ~ 'P99+'
    ),
    inc_group = factor(inc_group, levels = c('P0-25','P25-50','P50-75','P75-90','P90-99','P99+'))
  )

# Weighted mean shares from raw CEX
cex_comp = cex_pos %>%
  group_by(inc_group) %>%
  summarise(
    across(all_of(pce_cats), ~ weighted.mean(.x / total_consumption, WT_ANNUAL)),
    n = n(),
    .groups = 'drop'
  )

cat("==========================================================\n")
cat("  Raw CEX consumption shares by income group\n")
cat("==========================================================\n\n")

cat(sprintf("%-10s", "Group"))
for (cat_name in pce_cats) cat(sprintf(" %10s", substr(cat_name, 1, 10)))
cat(sprintf(" %6s\n", "N"))
cat(paste0(rep("-", 100), collapse = ""), "\n")

for (i in 1:nrow(cex_comp)) {
  cat(sprintf("%-10s", cex_comp$inc_group[i]))
  for (cat_name in pce_cats) cat(sprintf(" %10.3f", cex_comp[[cat_name]][i]))
  cat(sprintf(" %6d\n", cex_comp$n[i]))
}

# Now load PUF results
puf = read_rds('resources/cache/consumption_analysis.rds')

puf_comp = puf %>%
  mutate(
    inc_group = case_when(
      pctile <= 25 ~ 'P0-25',
      pctile <= 50 ~ 'P25-50',
      pctile <= 75 ~ 'P50-75',
      pctile <= 90 ~ 'P75-90',
      pctile <= 99 ~ 'P90-99',
      TRUE         ~ 'P99+'
    ),
    inc_group = factor(inc_group, levels = c('P0-25','P25-50','P50-75','P75-90','P90-99','P99+'))
  ) %>%
  group_by(inc_group) %>%
  summarise(
    across(all_of(pce_cats), ~ weighted.mean(.x / C, weight)),
    n = n(),
    .groups = 'drop'
  )

cat("\n==========================================================\n")
cat("  DRF-imputed PUF consumption shares by income group\n")
cat("==========================================================\n\n")

cat(sprintf("%-10s", "Group"))
for (cat_name in pce_cats) cat(sprintf(" %10s", substr(cat_name, 1, 10)))
cat(sprintf(" %6s\n", "N"))
cat(paste0(rep("-", 100), collapse = ""), "\n")

for (i in 1:nrow(puf_comp)) {
  cat(sprintf("%-10s", puf_comp$inc_group[i]))
  for (cat_name in pce_cats) cat(sprintf(" %10.3f", puf_comp[[cat_name]][i]))
  cat(sprintf(" %6d\n", puf_comp$n[i]))
}

#---------------------------------------------------------------------------
# Plots: side-by-side CEX raw vs PUF DRF
#---------------------------------------------------------------------------

cex_mat = as.matrix(cex_comp[pce_cats])
rownames(cex_mat) = cex_comp$inc_group
puf_mat = as.matrix(puf_comp[pce_cats])
rownames(puf_mat) = puf_comp$inc_group

cols = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3',
         '#ff7f00', '#a65628', '#f781bf', '#999999')
nice_names = c('Clothing', 'Motor Vehicles', 'Durables', 'Other Nondurables',
               'Food (off-prem)', 'Gasoline', 'Housing+Utilities', 'Other Services+Health')

png(file.path(plot_dir, '06_composition_cex_vs_puf.png'), width = 1100, height = 500)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 1), oma = c(0, 0, 0, 10))

barplot(t(cex_mat), col = cols, names.arg = rownames(cex_mat),
        xlab = 'Income Group (CEX percentile)', ylab = 'Share of Consumption',
        main = 'Raw CEX Data', ylim = c(0, 1), las = 2, cex.names = 0.8)

barplot(t(puf_mat), col = cols, names.arg = rownames(puf_mat),
        xlab = 'Income Group (PUF percentile)', ylab = '',
        main = 'DRF-Imputed PUF', ylim = c(0, 1), las = 2, cex.names = 0.8)

par(xpd = TRUE)
legend('right', inset = c(-0.45, 0), legend = rev(nice_names), fill = rev(cols),
       cex = 0.7, bty = 'n')

dev.off()

# --- Plot: difference (PUF - CEX) ---
diff_mat = puf_mat - cex_mat

png(file.path(plot_dir, '07_composition_diff.png'), width = 900, height = 500)
par(mar = c(6, 5, 3, 10), xpd = FALSE)

barplot(t(diff_mat), beside = TRUE, col = cols,
        names.arg = rownames(diff_mat),
        xlab = 'Income Group', ylab = 'Share Difference (PUF - CEX)',
        main = 'Composition Difference: DRF-Imputed PUF minus Raw CEX',
        las = 2, cex.names = 0.8)
abline(h = 0, lty = 1, col = 'black')

par(xpd = TRUE)
legend('right', inset = c(-0.25, 0), legend = nice_names, fill = cols,
       cex = 0.65, bty = 'n')

dev.off()

# --- Plot: just CEX shares as lines by percentile (finer granularity) ---
cex_fine = cex_pos %>%
  mutate(pctile_5 = 5 * (pctile %/% 5)) %>%
  group_by(pctile_5) %>%
  summarise(
    across(all_of(pce_cats), ~ weighted.mean(.x / total_consumption, WT_ANNUAL)),
    med_Y = wtd.quantile(income, WT_ANNUAL, 0.5),
    .groups = 'drop'
  ) %>%
  filter(pctile_5 > 0)

png(file.path(plot_dir, '08_cex_shares_by_pctile.png'), width = 900, height = 600)
par(mar = c(5, 5, 3, 10), xpd = FALSE)

plot(cex_fine$pctile_5, cex_fine[[pce_cats[1]]], type = 'n',
     ylim = c(0, 0.45), xlab = 'CEX Income Percentile (5-pt bins)',
     ylab = 'Share of Total Consumption',
     main = 'Raw CEX: Expenditure Category Shares by Income Percentile')

for (j in seq_along(pce_cats)) {
  lines(cex_fine$pctile_5, cex_fine[[pce_cats[j]]], col = cols[j], lwd = 2)
}

par(xpd = TRUE)
legend('right', inset = c(-0.28, 0), legend = nice_names, col = cols,
       lwd = 2, cex = 0.7, bty = 'n')

dev.off()

cat("\nPlots saved.\n")
