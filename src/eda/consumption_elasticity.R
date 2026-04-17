#--------------------------------------
# consumption_elasticity.R
#
# EDA: estimate consumption-income
# elasticity on CEX subsamples
#--------------------------------------

lapply(c('tidyverse', 'magrittr', 'data.table', 'Hmisc'), library, character.only = TRUE)
source('src/cex.R')

cat("Building CEX training data...\n")
cex = build_cex_training()

# Restrict to positive income and positive consumption
d = cex %>%
  filter(has_income == 1, income > 0, total_consumption > 0) %>%
  mutate(
    log_C = log(total_consumption),
    log_Y = log(income)
  )

cat(sprintf("\nFull sample (positive income & consumption): %d obs\n\n", nrow(d)))

# Weighted quantiles of income for subsetting
pctiles = wtd.quantile(d$income, d$WT_ANNUAL, probs = seq(0, 1, 0.01))

d = d %>%
  mutate(
    pctile = findInterval(income, pctiles[-101]) + 1L,
    pctile = pmin(pctile, 100L)
  )

# Define subsets to estimate on
subsets = list(
  "Full sample"  = d,
  "P20-P100"     = d %>% filter(pctile >= 20),
  "P50-P100"     = d %>% filter(pctile >= 50),
  "P70-P100"     = d %>% filter(pctile >= 70),
  "P80-P100"     = d %>% filter(pctile >= 80),
  "P80-P98"      = d %>% filter(pctile >= 80, pctile <= 98),
  "P90-P100"     = d %>% filter(pctile >= 90),
  "P90-P98"      = d %>% filter(pctile >= 90, pctile <= 98),
  "P95-P100"     = d %>% filter(pctile >= 95)
)

# Estimate log(C) = a + b*log(Y), both OLS and weighted
cat("==========================================================\n")
cat("  Consumption-Income Elasticity: log(C) = a + b*log(Y)\n")
cat("==========================================================\n\n")
cat(sprintf("%-14s | %6s %6s %7s %7s | %6s %6s %7s %7s | %8s %8s\n",
            "Subset", "b_ols", "se", "R2", "N",
            "b_wt", "se_wt", "R2_wt", "N_wt",
            "med_Y", "med_C"))
cat(paste0(rep("-", 110), collapse = ""), "\n")

for (nm in names(subsets)) {
  s = subsets[[nm]]
  n = nrow(s)

  # OLS (unweighted)
  fit_ols = lm(log_C ~ log_Y, data = s)
  b_ols  = coef(fit_ols)['log_Y']
  se_ols = summary(fit_ols)$coefficients['log_Y', 'Std. Error']
  r2_ols = summary(fit_ols)$r.squared

  # Weighted
  fit_wt = lm(log_C ~ log_Y, data = s, weights = s$WT_ANNUAL)
  b_wt  = coef(fit_wt)['log_Y']
  se_wt = summary(fit_wt)$coefficients['log_Y', 'Std. Error']
  r2_wt = summary(fit_wt)$r.squared

  med_Y = wtd.quantile(s$income, s$WT_ANNUAL, probs = 0.5)
  med_C = wtd.quantile(s$total_consumption, s$WT_ANNUAL, probs = 0.5)

  cat(sprintf("%-14s | %6.3f %6.4f %7.3f %7d | %6.3f %6.4f %7.3f %7d | %8.0f %8.0f\n",
              nm, b_ols, se_ols, r2_ols, n,
              b_wt, se_wt, r2_wt, n, med_Y, med_C))
}

cat("\n\n")

# Also show mean and median C/Y ratio by decile at the top
cat("==========================================================\n")
cat("  C/Y Ratio by Income Percentile Group (weighted)\n")
cat("==========================================================\n\n")

top_groups = list(
  "P1-P25"   = c(1, 25),
  "P25-P50"  = c(25, 50),
  "P50-P75"  = c(50, 75),
  "P75-P90"  = c(75, 90),
  "P90-P95"  = c(90, 95),
  "P95-P98"  = c(95, 98),
  "P98-P100" = c(98, 100)
)

cat(sprintf("%-12s | %8s %8s %8s %8s %8s | %6s\n",
            "Group", "mean_CY", "med_CY", "med_Y", "med_C", "p75_CY", "N"))
cat(paste0(rep("-", 75), collapse = ""), "\n")

for (nm in names(top_groups)) {
  rng = top_groups[[nm]]
  s = d %>% filter(pctile > rng[1], pctile <= rng[2])
  if (nrow(s) == 0) next

  cy = s$total_consumption / s$income

  mean_cy = weighted.mean(cy, s$WT_ANNUAL)
  med_cy  = wtd.quantile(cy, s$WT_ANNUAL, probs = 0.5)
  p75_cy  = wtd.quantile(cy, s$WT_ANNUAL, probs = 0.75)
  med_Y   = wtd.quantile(s$income, s$WT_ANNUAL, probs = 0.5)
  med_C   = wtd.quantile(s$total_consumption, s$WT_ANNUAL, probs = 0.5)

  cat(sprintf("%-12s | %8.3f %8.3f %8.0f %8.0f %8.3f | %6d\n",
              nm, mean_cy, med_cy, med_Y, med_C, p75_cy, nrow(s)))
}

cat("\n\n")
cat("==========================================================\n")
cat("  Y_ref candidates: weighted income at top of CEX\n")
cat("==========================================================\n\n")

ref_groups = list(
  "P90+"  = d %>% filter(pctile >= 90),
  "P95+"  = d %>% filter(pctile >= 95),
  "P98+"  = d %>% filter(pctile >= 98),
  "P99+"  = d %>% filter(pctile >= 99)
)

cat(sprintf("%-8s | %10s %10s %10s | %6s\n",
            "Group", "wt_median", "wt_mean", "wt_p75", "N"))
cat(paste0(rep("-", 55), collapse = ""), "\n")

for (nm in names(ref_groups)) {
  s = ref_groups[[nm]]
  med  = wtd.quantile(s$income, s$WT_ANNUAL, probs = 0.5)
  mn   = weighted.mean(s$income, s$WT_ANNUAL)
  p75  = wtd.quantile(s$income, s$WT_ANNUAL, probs = 0.75)
  cat(sprintf("%-8s | %10.0f %10.0f %10.0f | %6d\n", nm, med, mn, p75, nrow(s)))
}

cat("\n\n")
cat("==========================================================\n")
cat("  In-sample fit: log-log model vs actual C/Y by percentile\n")
cat("  Model: log(C) = a + b*log(Y), estimated on P80-P100\n")
cat("==========================================================\n\n")

# Estimate on P80+
p80 = d %>% filter(pctile >= 80)
fit = lm(log_C ~ log_Y, data = p80, weights = p80$WT_ANNUAL)
alpha = coef(fit)['(Intercept)']
beta  = coef(fit)['log_Y']
cat(sprintf("Estimated: alpha = %.4f, beta = %.4f\n\n", alpha, beta))

# Predicted vs actual by percentile group
pgroups = list(
  "P80-P85"  = c(80, 85),
  "P85-P90"  = c(85, 90),
  "P90-P95"  = c(90, 95),
  "P95-P98"  = c(95, 98),
  "P98-P100" = c(98, 100)
)

cat(sprintf("%-12s | %8s %8s %8s | %8s %8s %8s\n",
            "Group", "med_Y", "act_C", "act_CY",
            "pred_C", "pred_CY", "ratio"))
cat(paste0(rep("-", 80), collapse = ""), "\n")

for (nm in names(pgroups)) {
  rng = pgroups[[nm]]
  s = d %>% filter(pctile > rng[1], pctile <= rng[2])
  if (nrow(s) == 0) next

  med_Y   = wtd.quantile(s$income, s$WT_ANNUAL, probs = 0.5)
  act_C   = wtd.quantile(s$total_consumption, s$WT_ANNUAL, probs = 0.5)
  act_CY  = act_C / med_Y
  pred_C  = exp(alpha + beta * log(med_Y))
  pred_CY = pred_C / med_Y

  cat(sprintf("%-12s | %8.0f %8.0f %8.3f | %8.0f %8.3f %8.3f\n",
              nm, med_Y, act_C, act_CY, pred_C, pred_CY, pred_C / act_C))
}

# Now show what it predicts out of sample
cat("\n\nOut-of-sample extrapolation:\n\n")
cat(sprintf("%-12s | %12s %12s %8s\n", "Income", "Pred C", "Pred C/Y", "Note"))
cat(paste0(rep("-", 55), collapse = ""), "\n")
oos_incomes = c(500000, 1000000, 5000000, 10000000, 50000000)
for (y in oos_incomes) {
  pred_c = exp(alpha + beta * log(y))
  cat(sprintf("$%10.0f | $%10.0f %12.3f\n", y, pred_c, pred_c / y))
}

cat("\nDone.\n")
