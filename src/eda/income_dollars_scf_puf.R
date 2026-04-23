#--------------------------------------
# income_dollars_scf_puf.R
#
# Dollar-level diagnostic on the 6 income
# conditioning variables used by the
# wealth DRF (large spec):
#   income, wages, business, int_div,
#   capital_gains, ss_pens
# plus rent + ui_other (dropped on
# concept-gap grounds but carried through
# stage1 — included here for completeness).
#
# SCF 2022 values are deflated to 2017$
# using CPI-U (2022→2017: /1.17443,
# matches src/cex.R:34).
#
# Outputs a weighted-quantile table
# (p5, p25, p50, p75, p90, p95, p99,
# p99.9) plus mean, total, and
# positivity shares to
#   plots/wealth_ablation/income_dollars_scf_puf.csv
#
# Usage:
#   sbatch src/eda/income_dollars_scf_puf.sh
#--------------------------------------

lapply(readLines('requirements.txt'), library, character.only = TRUE)
source('./src/configure.R')
estimate_models = 1
do_lp = 0
set.seed(1337)

# Same minimal pipeline as test_wealth_X_ablation.R to materialize PUF
# tax_units with raw income components.
source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
source('./src/impute_nonfilers.R')

source('./src/imputations/helpers.R')
source('./src/imputations/demographics.R')
source('./src/imputations/ages.R')
source('./src/imputations/stage1_scf_tax_units.R')

stopifnot(exists('scf_tax_units'), exists('tax_units'))
stopifnot(all(c('wages_scf', 'business_scf', 'int_div_scf',
                'capital_gains_scf', 'rent_scf',
                'ss_pens_scf', 'ui_other_scf') %in% names(scf_tax_units)))

# CPI-U deflator: 2022$ → 2017$ (matches src/cex.R:34 exactly).
CPI_2022_TO_2017 = 1.17443

# Reconstruct the `income` aggregate on SCF side using the un-floored
# sum-of-components (mirrors the ablation's logic).
scf = scf_tax_units %>%
  transmute(
    weight,
    wages         = wages_scf,
    business      = business_scf,
    int_div       = int_div_scf,
    capital_gains = capital_gains_scf,
    rent          = rent_scf,
    ss_pens       = ss_pens_scf,
    ui_other      = ui_other_scf,
    income        = wages + business + int_div + capital_gains +
                    rent + ss_pens + ui_other
  ) %>%
  mutate(across(c(wages, business, int_div, capital_gains,
                  rent, ss_pens, ui_other, income),
                ~ .x / CPI_2022_TO_2017))

# PUF-side: use the same definitions as the ablation's income construction.
puf = tax_units %>%
  transmute(
    weight,
    wages         = wages,
    business      = sole_prop + farm +
                    scorp_active  - scorp_active_loss  - scorp_179 +
                    scorp_passive - scorp_passive_loss +
                    part_active   - part_active_loss   - part_179 +
                    part_passive  - part_passive_loss,
    int_div       = txbl_int + exempt_int + div_ord + div_pref,
    capital_gains = kg_lt + kg_st,
    rent          = rent - rent_loss + estate - estate_loss,
    ss_pens       = gross_ss + gross_pens_dist,
    ui_other      = ui,
    income        = wages + business + int_div + capital_gains +
                    rent + ss_pens + ui_other
  )

income_vars = c('income', 'wages', 'business', 'int_div',
                'capital_gains', 'ss_pens', 'rent', 'ui_other')

probs = c(0.05, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 0.999)

summarize_one = function(d, source_label) {
  map_dfr(income_vars, function(v) {
    x = d[[v]]
    w = d$weight
    qs = as.numeric(Hmisc::wtd.quantile(x, w, probs = probs, na.rm = FALSE))
    tibble(
      source   = source_label,
      variable = v,
      p5       = qs[1],
      p25      = qs[2],
      p50      = qs[3],
      p75      = qs[4],
      p90      = qs[5],
      p95      = qs[6],
      p99      = qs[7],
      p999     = qs[8],
      mean     = sum(x * w) / sum(w),
      total_bn = sum(x * w) / 1e9,
      pos_share  = sum(w[x >  0]) / sum(w),
      zero_share = sum(w[x == 0]) / sum(w),
      neg_share  = sum(w[x <  0]) / sum(w)
    )
  })
}

out = bind_rows(
  summarize_one(scf, 'SCF 2022 (in 2017$)'),
  summarize_one(puf, 'PUF 2017')
) %>%
  mutate(variable = factor(variable, levels = income_vars)) %>%
  arrange(variable, source)

out_dir = 'plots/wealth_ablation'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
csv_path = file.path(out_dir, 'income_dollars_scf_puf.csv')
write_csv(out, csv_path)

cat('\n==========================================================\n')
cat('  Income conditioning vars: PUF 2017 vs SCF 2022 (in 2017$)\n')
cat(sprintf('  CPI-U deflator applied to SCF: /%.5f\n', CPI_2022_TO_2017))
cat('==========================================================\n')

fmt_dollars = function(x) sprintf('%12s', formatC(round(x), big.mark = ',',
                                                  format = 'd'))

# Quantiles + mean table, one section per variable.
for (v in income_vars) {
  cat(sprintf('\n--- %s (dollars) ---\n', v))
  sub = out %>% filter(variable == v) %>%
    select(source, p5, p25, p50, p75, p90, p95, p99, p999, mean) %>%
    mutate(across(where(is.numeric), fmt_dollars))
  print(sub, n = 2)
}

cat('\n--- Aggregates & positivity ---\n')
agg = out %>%
  select(variable, source, total_bn, pos_share, zero_share, neg_share) %>%
  mutate(total_bn  = sprintf('%10.1f', total_bn),
         pos_share = sprintf('%6.1f%%', 100 * pos_share),
         zero_share = sprintf('%6.1f%%', 100 * zero_share),
         neg_share  = sprintf('%6.1f%%', 100 * neg_share))
print(agg, n = Inf)

cat(sprintf('\nwrote %s\n', csv_path))
cat('Done.\n')
