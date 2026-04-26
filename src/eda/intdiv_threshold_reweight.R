#---------------------------------------------------------------
# intdiv_threshold_reweight.R
#
# For each candidate PUF int_div threshold T (in dollars),
# rebuild has_int_div with the threshold applied only to PUF
# (SCF stays at threshold 0 — its small-dollar mass is already
# suppressed by survey methodology), then run the (age × int_div)
# joint reweight from wealth_reweight_scf.R.
#
# Headline question: does thresholding PUF's has_int_div at
# $100/$200/$500 meaningfully reduce the $180.6T reweight
# aggregate toward the pop-scaled SCF target ($153.7T)?
#---------------------------------------------------------------

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

AGE_BINS   = c(-1, 34, 44, 54, 64, 74, 80)
AGE_LABELS = c('<35','35-44','45-54','55-64','65-74','75+')

# SCF stays at raw zero threshold; the categorical buckets match the
# reference reweight script (pos/zero/neg).
classify_int_div = function(x, threshold = 0) {
  case_when(x >  threshold ~ 'pos',
            x == 0          ~ 'zero',
            x <  0          ~ 'neg',
            TRUE            ~ 'zero')  # small positive below threshold -> treated as zero
}

#--- SCF ---
scf = read_rds('resources/cache/scf_tax_units.rds') %>%
  mutate(
    a1 = pmin(as.integer(age1), 80L),
    a2 = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older = if_else(!is.na(a2), pmax(a1, a2), a1),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    has_int_div = classify_int_div(int_div_scf, threshold = 0),
    age_bin = cut(age_older, AGE_BINS, AGE_LABELS, include.lowest = TRUE)
  )
scf$nw = rowSums(scf[, wealth_asset_vars]) - rowSums(scf[, wealth_debt_vars])

ord = order(scf$income); cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank_scf = numeric(nrow(scf)); rank_scf[ord] = 100 * cum_w
scf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

#--- PUF ---
puf = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
               show_col_types = FALSE) %>%
  mutate(
    a1 = pmin(as.integer(age1), 80L),
    a2 = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older = if_else(!is.na(a2), pmax(a1, a2), a1),
    income = wages + sole_prop + farm +
             scorp_active  - scorp_active_loss  - scorp_179 +
             scorp_passive - scorp_passive_loss +
             part_active   - part_active_loss   - part_179 +
             part_passive  - part_passive_loss +
             txbl_int + exempt_int + div_ord + div_pref +
             kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
             rent - rent_loss + estate - estate_loss,
    int_div = txbl_int + exempt_int + div_ord + div_pref,
    age_bin = cut(age_older, AGE_BINS, AGE_LABELS, include.lowest = TRUE)
  )

puf_ord = order(puf$income); puf_cum_w = cumsum(puf$weight[puf_ord]) / sum(puf$weight)
rank_puf = numeric(nrow(puf)); rank_puf[puf_ord] = 100 * puf_cum_w
puf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

cat(sprintf('SCF: pop %.2fM  PUF: pop %.2fM\n', sum(scf$weight)/1e6, sum(puf$weight)/1e6))
scf_truth = sum(scf$weight * scf$nw) / 1e12
scf_pop_scaled = scf_truth * sum(puf$weight) / sum(scf$weight)
cat(sprintf('SCF truth: $%.2fT; pop-scaled to PUF: $%.2fT\n\n',
            scf_truth, scf_pop_scaled))

#--- Reweight function (matches wealth_reweight_scf.R) ---
reweight_aggregate = function(scf, puf, bin_cols) {
  key = c('cell', bin_cols)
  scf_sub = scf %>% group_by(across(all_of(key))) %>%
    summarise(scf_pop = sum(weight),
              scf_mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop')
  scf_cell_mean = scf %>% group_by(cell) %>%
    summarise(cell_mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop')
  puf_sub = puf %>% group_by(across(all_of(key))) %>%
    summarise(puf_pop = sum(weight), .groups = 'drop')
  joint = puf_sub %>%
    left_join(scf_sub, by = key) %>%
    left_join(scf_cell_mean, by = 'cell') %>%
    mutate(
      scf_mean_nw = if_else(is.na(scf_mean_nw) | is.na(scf_pop) | scf_pop == 0,
                            cell_mean_nw, scf_mean_nw),
      contribution = puf_pop * scf_mean_nw
    )
  sum(joint$contribution) / 1e12
}

#--- Threshold sweep ---
thresholds = c(0, 50, 100, 200, 300, 500, 1000)

cat('Threshold sweep: PUF has_int_div threshold -> reweight aggregate\n')
cat('(SCF uses threshold 0 throughout; this only changes PUF side)\n\n')

results = tibble(threshold = thresholds) %>%
  mutate(puf_share_pos = NA_real_,
         rw_age = NA_real_, rw_id = NA_real_, rw_both = NA_real_)

for (i in seq_along(thresholds)) {
  T_i = thresholds[i]
  puf_i = puf %>% mutate(has_int_div = classify_int_div(int_div, threshold = T_i))
  share_pos = sum(puf_i$weight[puf_i$has_int_div == 'pos']) / sum(puf_i$weight)
  results$puf_share_pos[i] = share_pos
  results$rw_age[i]  = reweight_aggregate(scf, puf_i, c('age_bin'))
  results$rw_id[i]   = reweight_aggregate(scf, puf_i, c('has_int_div'))
  results$rw_both[i] = reweight_aggregate(scf, puf_i, c('age_bin', 'has_int_div'))
}

# Reference: SCF has_int_div share_positive at threshold 0
scf_share_pos = sum(scf$weight[scf$has_int_div == 'pos']) / sum(scf$weight)
cat(sprintf('SCF share_positive (threshold 0): %.1f%%\n\n', 100 * scf_share_pos))

print(as.data.frame(results %>%
  mutate(
    puf_share_pos = round(100 * puf_share_pos, 1),
    delta_share_pp = round(100 * (puf_share_pos/100 - scf_share_pos), 1),
    rw_age  = round(rw_age,  2),
    rw_id   = round(rw_id,   2),
    rw_both = round(rw_both, 2),
    gap_closed_age_id_T = round(results$rw_both[1] - rw_both, 2)
  ) %>%
  rename(
    `threshold` = threshold,
    `PUF_share_pos_%` = puf_share_pos,
    `delta_vs_SCF_pp` = delta_share_pp,
    `rw_age_$T` = rw_age,
    `rw_intdiv_$T` = rw_id,
    `rw_both_$T` = rw_both,
    `gap_closed_$T` = gap_closed_age_id_T
  )), row.names = FALSE)

cat(sprintf('\nPop-scaled SCF target (if X-shift were all closed): $%.2fT\n',
            scf_pop_scaled))
cat(sprintf('Reweight at threshold 0 (baseline):                $%.2fT\n',
            results$rw_both[1]))
cat(sprintf('Gap from threshold-0 reweight to pop-scaled SCF:   $%.2fT\n',
            results$rw_both[1] - scf_pop_scaled))

cat('\nInterpretation:\n')
cat(' - rw_both is the (age × has_int_div) joint reweight aggregate.\n')
cat(' - gap_closed_$T shows $T closed vs threshold-0 baseline.\n')
cat(' - If gap_closed substantially >$0 at T=$100-$200 with minimal cost,\n')
cat('   the int_div definitional gap is indeed a big contributor and a\n')
cat('   threshold fix will meaningfully help.\n')

cat('\nDone.\n')
