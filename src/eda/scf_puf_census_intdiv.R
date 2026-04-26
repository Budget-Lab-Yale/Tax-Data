#---------------------------------------------
# scf_puf_census_intdiv.R
#
# Three-part diagnostic to inform "is the X-shift
# between SCF and PUF real population difference
# or a construction artifact":
#
#   (A) Weighted age distribution: SCF vs PUF.
#       Presented in shapes that can be compared
#       against external ACS/Census figures.
#
#   (B) Interest/dividend aggregate + share
#       positive: total across dataset, and by
#       (income cell × age bin).
#
#   (C) [note only; not computed here] The
#       188M vs 170M pop-scale gap between PUF
#       and SCF probably comes from PUF's
#       DINA-nonfiler addition. To investigate
#       later.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
output_dir = args[1]

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')

AGE_BINS_10YR  = c(-1, 24, 34, 44, 54, 64, 74, 80)
AGE_LABELS_10  = c('18-24','25-34','35-44','45-54','55-64','65-74','75+')

#--- SCF prep ---
scf = read_rds('resources/cache/scf_tax_units.rds') %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    age_bin = cut(age_older, breaks = AGE_BINS_10YR, labels = AGE_LABELS_10,
                   include.lowest = TRUE)
  )

ord = order(scf$income); cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank_scf = numeric(nrow(scf)); rank_scf[ord] = 100 * cum_w
scf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

#--- PUF prep ---
puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                show_col_types = FALSE) %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income = wages + sole_prop + farm +
             scorp_active  - scorp_active_loss  - scorp_179 +
             scorp_passive - scorp_passive_loss +
             part_active   - part_active_loss   - part_179 +
             part_passive  - part_passive_loss +
             txbl_int + exempt_int + div_ord + div_pref +
             kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
             rent - rent_loss + estate - estate_loss,
    int_div_puf = txbl_int + exempt_int + div_ord + div_pref,
    age_bin = cut(age_older, breaks = AGE_BINS_10YR, labels = AGE_LABELS_10,
                   include.lowest = TRUE)
  )

puf_ord = order(puf$income); puf_cum_w = cumsum(puf$weight[puf_ord]) / sum(puf$weight)
rank_puf = numeric(nrow(puf)); rank_puf[puf_ord] = 100 * puf_cum_w
puf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

#--- (A) Age distribution comparison ---
cat('=============================================\n')
cat('(A) Weighted tax-unit population by age_older (10-year bins)\n')
cat('=============================================\n\n')

scf_age = scf %>% group_by(age_bin) %>%
  summarise(scf_pop_M = sum(weight)/1e6, .groups = 'drop')
puf_age = puf %>% group_by(age_bin) %>%
  summarise(puf_pop_M = sum(weight)/1e6, .groups = 'drop')

age_tbl = full_join(scf_age, puf_age, by = 'age_bin') %>%
  mutate(diff_M = round(puf_pop_M - scf_pop_M, 1),
         pct_diff = round(100 * (puf_pop_M - scf_pop_M) / scf_pop_M, 1),
         scf_pop_M = round(scf_pop_M, 1),
         puf_pop_M = round(puf_pop_M, 1))
cat(sprintf('Total tax units: SCF %.1fM  PUF %.1fM  diff %+.1fM (%+.1f%%)\n\n',
            sum(scf_age$scf_pop_M), sum(puf_age$puf_pop_M),
            sum(puf_age$puf_pop_M) - sum(scf_age$scf_pop_M),
            100 * (sum(puf_age$puf_pop_M) - sum(scf_age$scf_pop_M)) /
                  sum(scf_age$scf_pop_M)))
print(as.data.frame(age_tbl), row.names = FALSE)

cat('\nShare of total (within each dataset):\n')
age_share = full_join(
  scf %>% group_by(age_bin) %>% summarise(scf_share = sum(weight) / sum(scf$weight)),
  puf %>% group_by(age_bin) %>% summarise(puf_share = sum(weight) / sum(puf$weight)),
  by = 'age_bin') %>%
  mutate(diff_pp = round(100 * (puf_share - scf_share), 2),
         scf_share = round(scf_share, 3),
         puf_share = round(puf_share, 3))
print(as.data.frame(age_share), row.names = FALSE)


#--- (B) Interest/dividend comparison ---
cat('\n\n=============================================\n')
cat('(B) Interest/dividend comparison (total $, share positive)\n')
cat('=============================================\n\n')

cat('Overall totals:\n')
scf_total_id = sum(scf$weight * scf$int_div_scf)
puf_total_id = sum(puf$weight * puf$int_div_puf)
scf_share_pos = sum(scf$weight[scf$int_div_scf > 0]) / sum(scf$weight)
puf_share_pos = sum(puf$weight[puf$int_div_puf > 0]) / sum(puf$weight)
scf_mean_if_pos = sum(scf$weight[scf$int_div_scf > 0] *
                      scf$int_div_scf[scf$int_div_scf > 0]) /
                  sum(scf$weight[scf$int_div_scf > 0])
puf_mean_if_pos = sum(puf$weight[puf$int_div_puf > 0] *
                      puf$int_div_puf[puf$int_div_puf > 0]) /
                  sum(puf$weight[puf$int_div_puf > 0])
overall = tibble(
  source            = c('SCF', 'PUF'),
  total_int_div_B   = round(c(scf_total_id, puf_total_id) / 1e9, 1),
  share_positive    = round(c(scf_share_pos, puf_share_pos), 3),
  mean_amount_if_pos = round(c(scf_mean_if_pos, puf_mean_if_pos), 0)
)
print(as.data.frame(overall), row.names = FALSE)

cat(sprintf('\nAggregate int_div ratio PUF/SCF: %.2fx\n',
            puf_total_id / scf_total_id))

# By cell × age
cat('\n\nShare of tax units with int_div > 0, by (cell × age):\n\n')
scf_share_ca = scf %>% group_by(cell, age_bin) %>%
  summarise(scf_share_pos = sum(weight * (int_div_scf > 0)) / sum(weight),
            .groups = 'drop')
puf_share_ca = puf %>% group_by(cell, age_bin) %>%
  summarise(puf_share_pos = sum(weight * (int_div_puf > 0)) / sum(weight),
            .groups = 'drop')

share_tbl = full_join(scf_share_ca, puf_share_ca, by = c('cell', 'age_bin')) %>%
  mutate(diff_pp = round(100 * (puf_share_pos - scf_share_pos), 1),
         scf_share_pos = round(scf_share_pos, 3),
         puf_share_pos = round(puf_share_pos, 3),
         cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell, age_bin)
# Compact pivot
share_wide = share_tbl %>%
  mutate(ratio = sprintf('%.2f→%.2f(%+.0fpp)',
                         scf_share_pos, puf_share_pos, diff_pp)) %>%
  select(cell, age_bin, ratio) %>%
  pivot_wider(names_from = age_bin, values_from = ratio, values_fill = '-')
print(as.data.frame(share_wide), row.names = FALSE)

# Total int_div amount by (cell × age), in billions
cat('\n\nTotal int_div ($B weighted), SCF vs PUF, by (cell × age):\n\n')
scf_total_ca = scf %>% group_by(cell, age_bin) %>%
  summarise(scf_B = sum(weight * int_div_scf) / 1e9, .groups = 'drop')
puf_total_ca = puf %>% group_by(cell, age_bin) %>%
  summarise(puf_B = sum(weight * int_div_puf) / 1e9, .groups = 'drop')

tot_tbl = full_join(scf_total_ca, puf_total_ca, by = c('cell', 'age_bin')) %>%
  mutate(ratio = puf_B / pmax(scf_B, 1e-9),
         cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell, age_bin)
tot_wide_ratio = tot_tbl %>%
  mutate(val = sprintf('%.0f/%.0f=%.1fx', puf_B, scf_B, ratio)) %>%
  select(cell, age_bin, val) %>%
  pivot_wider(names_from = age_bin, values_from = val, values_fill = '-')
cat('Values: PUF_$B / SCF_$B = ratio\n\n')
print(as.data.frame(tot_wide_ratio), row.names = FALSE)

cat('\n\nDone.\n')
