#---------------------------------------------------------------
# nonfiler_intdiv_probe.R
#
# Two-part focused probe for the status doc open questions:
#
#   (A) Is the PUF-SCF tax-unit count gap driven by DINA
#       nonfilers, and does it concentrate in 18-24?
#
#   (B) If we threshold PUF int_div at some $ amount, how close
#       does PUF's share-positive come to SCF's, and what does
#       the aggregate look like? Tests whether small-dollar
#       int_div can be zeroed for training-feature purposes.
#
# Usage:  Rscript src/eda/nonfiler_intdiv_probe.R <output_dir>
# Needs:  tax_units_2022.csv (with the `filer` column) and
#         resources/cache/scf_tax_units.rds
#---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
output_dir = args[1]

AGE_BINS  = c(-1, 24, 34, 44, 54, 64, 74, 80)
AGE_LABS  = c('18-24','25-34','35-44','45-54','55-64','65-74','75+')

#--- SCF ---
scf = read_rds('resources/cache/scf_tax_units.rds') %>%
  mutate(
    a1 = pmin(as.integer(age1), 80L),
    a2 = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older = if_else(!is.na(a2), pmax(a1, a2), a1),
    age_bin   = cut(age_older, AGE_BINS, AGE_LABS, include.lowest = TRUE)
  )

#--- PUF ---
puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
               show_col_types = FALSE) %>%
  mutate(
    a1 = pmin(as.integer(age1), 80L),
    a2 = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older = if_else(!is.na(a2), pmax(a1, a2), a1),
    age_bin   = cut(age_older, AGE_BINS, AGE_LABS, include.lowest = TRUE),
    int_div   = txbl_int + exempt_int + div_ord + div_pref
  )

stopifnot('filer' %in% names(puf))

cat('===============================================================\n')
cat('(A) PUF population decomposition: filers vs nonfilers\n')
cat('===============================================================\n\n')

totals = puf %>% group_by(filer) %>%
  summarise(pop_M = sum(weight) / 1e6, .groups = 'drop') %>%
  mutate(filer = c('0 nonfiler','1 filer')[filer + 1])
cat('PUF totals:\n')
print(as.data.frame(totals %>% mutate(pop_M = round(pop_M, 2))),
      row.names = FALSE)
puf_total_M = sum(puf$weight) / 1e6
scf_total_M = sum(scf$weight) / 1e6
cat(sprintf('\nSCF total: %.2fM   PUF total: %.2fM   gap: %+.2fM\n',
            scf_total_M, puf_total_M, puf_total_M - scf_total_M))

cat('\nAge distribution — SCF vs PUF filers vs PUF nonfilers (millions):\n\n')
scf_age     = scf %>% group_by(age_bin) %>%
  summarise(scf = sum(weight) / 1e6, .groups = 'drop')
puf_f_age   = puf %>% filter(filer == 1) %>% group_by(age_bin) %>%
  summarise(puf_filer = sum(weight) / 1e6, .groups = 'drop')
puf_nf_age  = puf %>% filter(filer == 0) %>% group_by(age_bin) %>%
  summarise(puf_nonfiler = sum(weight) / 1e6, .groups = 'drop')

age_tbl = scf_age %>%
  full_join(puf_f_age,  by = 'age_bin') %>%
  full_join(puf_nf_age, by = 'age_bin') %>%
  mutate(across(c(scf, puf_filer, puf_nonfiler), ~ round(replace_na(., 0), 2)),
         puf_total = round(puf_filer + puf_nonfiler, 2),
         gap_total = round(puf_total - scf, 2),
         nonfiler_share_of_gap =
           round(pmin(pmax(puf_nonfiler / pmax(gap_total, 1e-9), 0), 2), 2))
print(as.data.frame(age_tbl), row.names = FALSE)

cat('\n18-24 bracket deep-dive:\n')
f18 = puf %>% filter(filer == 1, age_bin == '18-24') %>%
  summarise(w = sum(weight) / 1e6) %>% pull(w)
nf18 = puf %>% filter(filer == 0, age_bin == '18-24') %>%
  summarise(w = sum(weight) / 1e6) %>% pull(w)
s18 = scf %>% filter(age_bin == '18-24') %>%
  summarise(w = sum(weight) / 1e6) %>% pull(w)
cat(sprintf('  SCF 18-24:          %.2fM\n', s18))
cat(sprintf('  PUF 18-24 filers:   %.2fM\n', f18))
cat(sprintf('  PUF 18-24 nonfilers:%.2fM\n', nf18))
cat(sprintf('  PUF 18-24 total:    %.2fM  (SCF gap: %+.2fM)\n',
            f18 + nf18, f18 + nf18 - s18))
cat(sprintf('  Fraction of PUF 18-24 that are nonfilers: %.1f%%\n',
            100 * nf18 / (f18 + nf18)))
cat(sprintf('  Census ACS 2022 18-24 population: ~30M\n'))

cat('\n===============================================================\n')
cat('(B) int_div threshold experiment\n')
cat('===============================================================\n\n')

# Overall aggregates
scf_id_pos = scf$int_div_scf > 0
puf_id_pos = puf$int_div > 0

scf_tot_B = sum(scf$weight * scf$int_div_scf) / 1e9
puf_tot_B = sum(puf$weight * puf$int_div) / 1e9
scf_share = sum(scf$weight[scf_id_pos]) / sum(scf$weight)
puf_share = sum(puf$weight[puf_id_pos]) / sum(puf$weight)

cat(sprintf('SCF aggregate: $%.1fB   share positive: %.1f%%\n',
            scf_tot_B, 100 * scf_share))
cat(sprintf('PUF aggregate: $%.1fB   share positive: %.1f%%\n\n',
            puf_tot_B, 100 * puf_share))

# Quantiles of the positive-mass PUF distribution
cat('PUF int_div quantiles among holders (int_div > 0):\n')
pos_vals = puf$int_div[puf_id_pos]
pos_wts  = puf$weight[puf_id_pos]
qs = c(0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99)
q_vals = wtd.quantile(pos_vals, weights = pos_wts, probs = qs)
print(round(q_vals, 0))

cat('\nSCF int_div quantiles among holders:\n')
s_pos_vals = scf$int_div_scf[scf_id_pos]
s_pos_wts  = scf$weight[scf_id_pos]
sq_vals = wtd.quantile(s_pos_vals, weights = s_pos_wts, probs = qs)
print(round(sq_vals, 0))

# Threshold sweep: for a menu of thresholds, what share-positive does PUF have?
cat('\nThreshold sweep: PUF int_div threshold -> share_positive, aggregate, mean|pos\n')
thresh = c(0, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 5000)
sweep = tibble(threshold = thresh) %>%
  rowwise() %>%
  mutate(
    share_pos = sum(puf$weight[puf$int_div > threshold]) / sum(puf$weight),
    agg_B     = sum(puf$weight[puf$int_div > threshold] *
                     puf$int_div[puf$int_div > threshold]) / 1e9,
    mean_pos  = agg_B * 1e9 / sum(puf$weight[puf$int_div > threshold])
  ) %>% ungroup() %>%
  mutate(
    share_pos = round(share_pos, 3),
    agg_B     = round(agg_B, 1),
    mean_pos  = round(mean_pos, 0),
    agg_pct_loss = round(100 * (1 - agg_B / puf_tot_B), 2),
    delta_from_scf_share_pp = round(100 * (share_pos - scf_share), 1)
  )
print(as.data.frame(sweep), row.names = FALSE)

# What threshold lands PUF share_positive closest to SCF's?
target = scf_share
closest_idx = which.min(abs(sweep$share_pos - target))
cat(sprintf('\nClosest match to SCF share-positive (%.1f%%):\n',
            100 * target))
cat(sprintf('  threshold = $%.0f  -> share_pos %.1f%%, agg $%.1fB (%.1f%% loss vs unthreshold), mean|pos $%.0f\n',
            sweep$threshold[closest_idx],
            100 * sweep$share_pos[closest_idx],
            sweep$agg_B[closest_idx],
            sweep$agg_pct_loss[closest_idx],
            sweep$mean_pos[closest_idx]))
cat(sprintf('  (SCF reference: agg $%.1fB, mean|pos $%.0f)\n',
            scf_tot_B,
            sum(s_pos_wts * s_pos_vals) / sum(s_pos_wts)))

cat('\nDone.\n')
