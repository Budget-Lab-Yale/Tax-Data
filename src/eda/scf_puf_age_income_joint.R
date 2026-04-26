#---------------------------------------------
# scf_puf_age_income_joint.R
#
# Joint (age × income-cell) tax-unit count
# distribution: SCF vs PUF. Reports:
#   (1) Full joint table — pop-M and shares
#   (2) PUF − SCF gaps in M tax units
#   (3) PUF gap split by filer / nonfiler
#       (uses PUF's `filer` column)
#   (4) Per-cell summary: total pop, gap,
#       and concentration of gap by age band
#
# Income definition matches the SCFP-INCOME
# bundling used in age_dist_scf_vs_puf.R and
# the production wealth.R.
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

AGE_BINS   = c(-1, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 80)
AGE_LABELS = c('18-24','25-29','30-34','35-39','40-44','45-49','50-54',
               '55-59','60-64','65-69','70-74','75-80')

#--- SCF -----------------------------------------------------------------
scf = read_rds('resources/cache/scf_tax_units.rds') %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    age_bin = cut(age_older, breaks = AGE_BINS, labels = AGE_LABELS,
                   right = TRUE, include.lowest = TRUE)
  )

ord = order(scf$income); cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank_scf = numeric(nrow(scf)); rank_scf[ord] = 100 * cum_w
scf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

#--- PUF -----------------------------------------------------------------
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
    age_bin = cut(age_older, breaks = AGE_BINS, labels = AGE_LABELS,
                   right = TRUE, include.lowest = TRUE)
  )

ord_p = order(puf$income); cum_wp = cumsum(puf$weight[ord_p]) / sum(puf$weight)
rank_puf = numeric(nrow(puf)); rank_puf[ord_p] = 100 * cum_wp
puf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

cat(sprintf('SCF: %d rows, pop %.1fM\n', nrow(scf), sum(scf$weight)/1e6))
cat(sprintf('PUF: %d rows, pop %.1fM  (filers %.1fM + nonfilers %.1fM)\n\n',
            nrow(puf), sum(puf$weight)/1e6,
            sum(puf$weight[puf$filer == 1])/1e6,
            sum(puf$weight[puf$filer == 0])/1e6))


#--- (1) Full joint pop counts (M) ---------------------------------------

scf_jt = scf %>%
  group_by(cell, age_bin) %>%
  summarise(scf_pop_M = sum(weight) / 1e6, .groups = 'drop')

puf_jt = puf %>%
  group_by(cell, age_bin) %>%
  summarise(puf_pop_M = sum(weight) / 1e6,
            puf_filer_M = sum(weight * (filer == 1)) / 1e6,
            puf_nonfiler_M = sum(weight * (filer == 0)) / 1e6,
            .groups = 'drop')

joint = full_join(scf_jt, puf_jt, by = c('cell','age_bin')) %>%
  mutate(across(ends_with('_M'), ~ replace_na(.x, 0)),
         gap_M = puf_pop_M - scf_pop_M,
         filer_gap_M = puf_filer_M - scf_pop_M)

joint = joint %>%
  mutate(cell = factor(cell, levels = CALIB_INCOME_BUCKETS),
         age_bin = factor(age_bin, levels = AGE_LABELS)) %>%
  arrange(cell, age_bin)

cat('--- (1) Full joint tax-unit pop (millions) ---\n\n')
cat('SCF pop_M by (cell × age_bin):\n')
scf_wide = joint %>% select(cell, age_bin, scf_pop_M) %>%
  pivot_wider(names_from = age_bin, values_from = scf_pop_M, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(scf_wide), row.names = FALSE)

cat('\nPUF pop_M by (cell × age_bin):\n')
puf_wide = joint %>% select(cell, age_bin, puf_pop_M) %>%
  pivot_wider(names_from = age_bin, values_from = puf_pop_M, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(puf_wide), row.names = FALSE)

cat('\nGAP_M = PUF − SCF (millions of tax units):\n')
gap_wide = joint %>% select(cell, age_bin, gap_M) %>%
  pivot_wider(names_from = age_bin, values_from = gap_M, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(gap_wide), row.names = FALSE)


#--- (2) Filer-only gap (excludes DINA-nonfilers) ------------------------

cat('\n--- (2) Filer-only gap = PUF_filers − SCF (millions) ---\n')
cat('(Subtracts DINA-nonfiler imputation; isolates Moore-port deficit)\n\n')
filer_gap_wide = joint %>% select(cell, age_bin, filer_gap_M) %>%
  pivot_wider(names_from = age_bin, values_from = filer_gap_M, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(filer_gap_wide), row.names = FALSE)

cat('\nNonfiler-only pop_M (where DINA places them):\n')
nf_wide = joint %>% select(cell, age_bin, puf_nonfiler_M) %>%
  pivot_wider(names_from = age_bin, values_from = puf_nonfiler_M, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(nf_wide), row.names = FALSE)


#--- (3) Marginal totals --------------------------------------------------

cat('\n--- (3) Age-marginal pop totals ---\n\n')
age_marg = joint %>% group_by(age_bin) %>%
  summarise(scf_M = sum(scf_pop_M),
            puf_M = sum(puf_pop_M),
            puf_filer_M = sum(puf_filer_M),
            puf_nonfiler_M = sum(puf_nonfiler_M), .groups = 'drop') %>%
  mutate(gap_M = puf_M - scf_M,
         filer_gap_M = puf_filer_M - scf_M,
         scf_share = scf_M / sum(scf_M),
         puf_share = puf_M / sum(puf_M)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(as.data.frame(age_marg), row.names = FALSE)

cat('\n--- Cell-marginal pop totals ---\n\n')
cell_marg = joint %>% group_by(cell) %>%
  summarise(scf_M = sum(scf_pop_M),
            puf_M = sum(puf_pop_M),
            puf_filer_M = sum(puf_filer_M),
            puf_nonfiler_M = sum(puf_nonfiler_M), .groups = 'drop') %>%
  mutate(gap_M = puf_M - scf_M,
         filer_gap_M = puf_filer_M - scf_M) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(cell_marg), row.names = FALSE)


#--- (4) Gap concentration: where is the 17.9M gap? ----------------------

cat('\n--- (4) Top 10 (cell × age) cells by absolute gap ---\n\n')
top_gap = joint %>%
  arrange(desc(abs(gap_M))) %>%
  slice_head(n = 10) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  select(cell, age_bin, scf_pop_M, puf_pop_M, gap_M,
         puf_filer_M, puf_nonfiler_M, filer_gap_M)
print(as.data.frame(top_gap), row.names = FALSE)

total_gap_M = sum(joint$gap_M)
total_filer_gap_M = sum(joint$filer_gap_M)
cat(sprintf('\nTotal gap          : %+.2fM tax units\n', total_gap_M))
cat(sprintf('Total filer-gap    : %+.2fM tax units\n', total_filer_gap_M))
cat(sprintf('Total nonfiler-gap : %+.2fM tax units (DINA addition)\n',
            sum(joint$puf_nonfiler_M)))

cat('\nDone.\n')
