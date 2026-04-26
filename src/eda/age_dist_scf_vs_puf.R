#---------------------------------------------
# age_dist_scf_vs_puf.R
#
# Weighted age distribution: SCF tax units vs
# PUF. Overall and per 8-cell income bucket.
# Age = `age_older` = pmax(age1, age2) with both
# capped at 80 (matches wealth.R construction).
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

source('src/imputations/helpers.R')

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')

#--- SCF
scf = read_rds('resources/cache/scf_tax_units.rds') %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    age_younger = if_else(!is.na(age2_capped), pmin(age1_capped, age2_capped), 0L),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf
  )

ord = order(scf$income); cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank_scf = numeric(nrow(scf)); rank_scf[ord] = 100 * cum_w
scf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

#--- PUF
puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                show_col_types = FALSE) %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    age_younger = if_else(!is.na(age2_capped), pmin(age1_capped, age2_capped), 0L),
    income = wages + sole_prop + farm +
             scorp_active  - scorp_active_loss  - scorp_179 +
             scorp_passive - scorp_passive_loss +
             part_active   - part_active_loss   - part_179 +
             part_passive  - part_passive_loss +
             txbl_int + exempt_int + div_ord + div_pref +
             kg_lt + kg_st +
             gross_ss + gross_pens_dist + ui +
             rent - rent_loss + estate - estate_loss
  )

ord_p = order(puf$income); cum_wp = cumsum(puf$weight[ord_p]) / sum(puf$weight)
rank_puf = numeric(nrow(puf)); rank_puf[ord_p] = 100 * cum_wp
puf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]


#--- Overall weighted distribution (5-year bins) -------------------------

cat('SCF: ', nrow(scf), ' rows, pop ', round(sum(scf$weight)/1e6, 1), 'M\n', sep='')
cat('PUF: ', nrow(puf), ' rows, pop ', round(sum(puf$weight)/1e6, 1), 'M\n\n', sep='')

# Bin age_older
age_bins = c(-1, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 80)
age_labels = c('0-24','25-29','30-34','35-39','40-44','45-49','50-54',
               '55-59','60-64','65-69','70-74','75-79','80 (topcode)')

scf$age_bin = cut(scf$age_older, breaks = age_bins, labels = age_labels, right = TRUE)
puf$age_bin = cut(puf$age_older, breaks = age_bins, labels = age_labels, right = TRUE)

scf_hist = scf %>% group_by(age_bin) %>%
  summarise(scf_pop_M = sum(weight) / 1e6, .groups = 'drop') %>%
  mutate(scf_share = scf_pop_M / sum(scf_pop_M))
puf_hist = puf %>% group_by(age_bin) %>%
  summarise(puf_pop_M = sum(weight) / 1e6, .groups = 'drop') %>%
  mutate(puf_share = puf_pop_M / sum(puf_pop_M))

cat('Overall age_older distribution (weighted):\n')
hist_tbl = full_join(scf_hist, puf_hist, by = 'age_bin') %>%
  mutate(diff = round(puf_share - scf_share, 4),
         scf_pop_M = round(scf_pop_M, 1),
         puf_pop_M = round(puf_pop_M, 1),
         scf_share = round(scf_share, 3),
         puf_share = round(puf_share, 3))
print(as.data.frame(hist_tbl), row.names = FALSE)


#--- Weighted quantiles of age_older -------------------------------------

cat('\nWeighted quantiles of age_older (years):\n')
q_probs = c(0.10, 0.25, 0.50, 0.75, 0.90)
scf_q = wtd.quantile(scf$age_older, scf$weight, probs = q_probs)
puf_q = wtd.quantile(puf$age_older, puf$weight, probs = q_probs)
qtbl = tibble(
  probs = paste0('p', q_probs * 100),
  scf   = round(as.numeric(scf_q), 1),
  puf   = round(as.numeric(puf_q), 1),
  diff  = round(as.numeric(puf_q) - as.numeric(scf_q), 1)
)
print(as.data.frame(qtbl), row.names = FALSE)

cat(sprintf('\nWeighted mean age_older:  SCF %.1f, PUF %.1f  (diff %+.1f)\n',
            sum(scf$weight * scf$age_older) / sum(scf$weight),
            sum(puf$weight * puf$age_older) / sum(puf$weight),
            sum(puf$weight * puf$age_older) / sum(puf$weight) -
            sum(scf$weight * scf$age_older) / sum(scf$weight)))


#--- Per-cell age moments ------------------------------------------------

cat('\n\nPer-cell age_older weighted mean + p50 + p90 (SCF vs PUF):\n')
per_cell = purrr::map_dfr(CALIB_INCOME_BUCKETS, function(cn) {
  s = scf %>% filter(cell == cn)
  p = puf %>% filter(cell == cn)
  tibble(
    cell        = cn,
    scf_mean    = round(sum(s$weight * s$age_older) / sum(s$weight), 1),
    puf_mean    = round(sum(p$weight * p$age_older) / sum(p$weight), 1),
    mean_diff   = round(sum(p$weight * p$age_older) / sum(p$weight) -
                        sum(s$weight * s$age_older) / sum(s$weight), 1),
    scf_p50     = round(as.numeric(wtd.quantile(s$age_older, s$weight, 0.5)), 1),
    puf_p50     = round(as.numeric(wtd.quantile(p$age_older, p$weight, 0.5)), 1),
    scf_p90     = round(as.numeric(wtd.quantile(s$age_older, s$weight, 0.9)), 1),
    puf_p90     = round(as.numeric(wtd.quantile(p$age_older, p$weight, 0.9)), 1)
  )
})
print(as.data.frame(per_cell), row.names = FALSE)


#--- Same for age_younger ------------------------------------------------

cat('\n\nOverall age_younger weighted distribution:\n')
scf$ay_bin = cut(scf$age_younger, breaks = c(-1, 0, 24, 34, 44, 54, 64, 80),
                  labels = c('0-single','1-24','25-34','35-44','45-54',
                             '55-64','65+'))
puf$ay_bin = cut(puf$age_younger, breaks = c(-1, 0, 24, 34, 44, 54, 64, 80),
                  labels = c('0-single','1-24','25-34','35-44','45-54',
                             '55-64','65+'))

scf_ay = scf %>% group_by(ay_bin) %>%
  summarise(scf_pop_M = sum(weight)/1e6, .groups = 'drop') %>%
  mutate(scf_share = scf_pop_M / sum(scf_pop_M))
puf_ay = puf %>% group_by(ay_bin) %>%
  summarise(puf_pop_M = sum(weight)/1e6, .groups = 'drop') %>%
  mutate(puf_share = puf_pop_M / sum(puf_pop_M))
ay_tbl = full_join(scf_ay, puf_ay, by = 'ay_bin') %>%
  mutate(diff = round(puf_share - scf_share, 4),
         scf_pop_M = round(scf_pop_M, 1),
         puf_pop_M = round(puf_pop_M, 1),
         scf_share = round(scf_share, 3),
         puf_share = round(puf_share, 3))
print(as.data.frame(ay_tbl), row.names = FALSE)

cat('\nDone.\n')
