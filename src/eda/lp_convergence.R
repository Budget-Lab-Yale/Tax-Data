#---------------------------------------------------------------------
# lp_convergence.R
#
# The LP returns "solved" (status 0) but post-LP, the band-1 marginal
# is at 0.872× SOI — 13% under, far outside the 0.5% per-constraint
# tolerance. So either:
#
#   (a) the per-(fs × ag × AGI) constraints aren't all hit, despite
#       LP returning optimal — possible if the LP weight-delta bounds
#       (eps=0.66) clamp some records before they reach their target;
#   (b) target_value computation gives a smaller target than the SOI
#       row sum we're comparing against — i.e., the LP saw a different
#       target than we're benchmarking against;
#
# This script:
#   1) Loads cached weight_deltas.rds and reports its distribution.
#      Records pinned at 1 ± eps mean the LP wanted to push them
#      further but couldn't.
#   2) Re-derives the targets tibble used by reweight_lp by sourcing
#      process_targets.R + the bits of process_puf.R needed to compute
#      get_target_value(). Reports sum of target_values for a few
#      probe constraints (e.g., fs=all + ag=1 across AGI bins) vs
#      SOI row totals to see if (b) is real.
#---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(stringr); library(magrittr)
})

CACHE = 'resources/cache/lp/weight_deltas.rds'

#--- 1. weight_deltas distribution

cat('Loading cached weight_deltas ...\n')
wd = readRDS(CACHE)
stopifnot(is.numeric(wd))
cat(sprintf('  %d records\n', length(wd)))

cat('\n--- weight_deltas distribution ---\n')
cat(sprintf('  min    : %.4f\n', min(wd)))
cat(sprintf('  q01    : %.4f\n', quantile(wd, 0.01)))
cat(sprintf('  q10    : %.4f\n', quantile(wd, 0.10)))
cat(sprintf('  q25    : %.4f\n', quantile(wd, 0.25)))
cat(sprintf('  median : %.4f\n', median(wd)))
cat(sprintf('  q75    : %.4f\n', quantile(wd, 0.75)))
cat(sprintf('  q90    : %.4f\n', quantile(wd, 0.90)))
cat(sprintf('  q99    : %.4f\n', quantile(wd, 0.99)))
cat(sprintf('  max    : %.4f\n', max(wd)))
cat(sprintf('  mean   : %.4f\n', mean(wd)))

EPS = 0.66
LO  = 1 - EPS
HI  = 1 + EPS

cat(sprintf('\n--- bound-hitting (eps = %.2f) ---\n', EPS))
cat(sprintf('  at lower bound (delta <= %.3f + 0.001) : %d (%.1f%%)\n',
            LO, sum(wd <= LO + 1e-3), 100 * mean(wd <= LO + 1e-3)))
cat(sprintf('  at upper bound (delta >= %.3f - 0.001) : %d (%.1f%%)\n',
            HI, sum(wd >= HI - 1e-3), 100 * mean(wd >= HI - 1e-3)))

#--- 2. Reload SOI 1.6 row sums for a few probe (fs, ag) cells

cat('\n--- SOI 2017 1.6 by (fs × ag) and AGI-bin sum ---\n')
soi = read_csv(
  '/nfs/roberts/project/pi_nrs36/shared/model_data/Compiled-SOI-Tables/v2/2023100323/historical/table_1_6.csv',
  show_col_types = FALSE
) %>% filter(year == 2017)

# Long form
soi_long = soi %>%
  tidyr::pivot_longer(-c(year, filing_status, age_group),
                      names_to = 'agi_min_chr', values_to = 'count') %>%
  mutate(agi_min = as.numeric(agi_min_chr),
         count   = replace(count, is.na(count), 0))

# Probe: ag=1 (under 26) total across all fs × all AGI
probe_ag1_all_fs = soi_long %>%
  filter(age_group == 1) %>%
  summarise(target_M = sum(count) / 1e6)
cat(sprintf('  ag=1, fs=all (sum across all AGI bins): %.3fM\n',
            probe_ag1_all_fs$target_M))

# By individual filing status × ag=1
cat('  ag=1 by filing_status (SOI):\n')
print(soi_long %>%
        filter(age_group == 1) %>%
        group_by(filing_status) %>%
        summarise(soi_M = sum(count) / 1e6, .groups = 'drop'))

# By AGI bin × ag=1 × fs=all
cat('\n  ag=1, fs=all, by AGI bin (SOI):\n')
print(soi_long %>%
        filter(age_group == 1) %>%
        group_by(agi_min) %>%
        summarise(soi_M = sum(count) / 1e6, .groups = 'drop') %>%
        arrange(agi_min),
      n = Inf)

# Same for fs=1 (single)
cat('\n  ag=1, fs=1 (single), by AGI bin (SOI):\n')
print(soi_long %>%
        filter(age_group == 1, filing_status == 1) %>%
        arrange(agi_min) %>%
        select(agi_min, count_M = count) %>%
        mutate(count_M = count_M / 1e6),
      n = Inf)

cat('\nDone.\n')
