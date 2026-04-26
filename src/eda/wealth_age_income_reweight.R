#---------------------------------------------
# wealth_age_income_reweight.R
#
# Quantify how much of the +$62T PUF wealth
# overshoot is attributable to (age × income)
# tax-unit-distribution mismatch alone, holding
# Y|X = SCF.
#
# Method: aggregate = Σ_{cell × age_bin}
#   PUF_pop_here × SCF_mean_nw_here
# Empty subcells fall back to SCF cell-mean NW.
#
# Variants:
#   (a) age × income-cell joint reweight
#   (b) age-only reweight  (within-cell scaling)
#   (c) PUF-FILER × age × cell reweight
#       (excludes DINA-nonfilers from PUF
#        side; isolates the Moore-port-deficit
#        contribution)
#
# Compare to:
#   - SCF truth (no reweight)            ~ $139T
#   - Pure pop-scaling (no X-shift)      ~ $154T
#   - age × int_div reweight (existing)  ~ $181T
#   - Forest aggregate                   ~ $201T
#
# If (a) closes most of the $46T residual
# left after pop-scaling, age × income IS the
# X-shift driver. If little of it closes, the
# wealth gap is being driven by other features
# (capital_gains, business, ss_pens) interacting
# with cell, not by tax-unit count.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')

AGE_BINS   = c(-1, 24, 34, 44, 54, 64, 74, 80)
AGE_LABELS = c('18-24','25-34','35-44','45-54','55-64','65-74','75-80')


#--- SCF ------------------------------------------------------------------
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
scf$nw = rowSums(scf[, wealth_asset_vars]) - rowSums(scf[, wealth_debt_vars])

ord = order(scf$income); cum_w = cumsum(scf$weight[ord]) / sum(scf$weight)
rank_scf = numeric(nrow(scf)); rank_scf[ord] = 100 * cum_w
scf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_scf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

#--- PUF ------------------------------------------------------------------
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

puf_ord = order(puf$income); puf_cum_w = cumsum(puf$weight[puf_ord]) / sum(puf$weight)
rank_puf = numeric(nrow(puf)); rank_puf[puf_ord] = 100 * puf_cum_w
puf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

cat(sprintf('SCF: %d rows, pop %.1fM\n', nrow(scf), sum(scf$weight)/1e6))
cat(sprintf('PUF: %d rows, pop %.1fM  (filers %.1fM + nonfilers %.1fM)\n\n',
            nrow(puf), sum(puf$weight)/1e6,
            sum(puf$weight[puf$filer == 1])/1e6,
            sum(puf$weight[puf$filer == 0])/1e6))


#--- Core reweight function ----------------------------------------------

reweight_aggregate = function(scf, puf_use, bin_cols, label) {
  # Σ_{cell × bin_cols} PUF_pop × SCF_mean_NW.
  # Empty SCF subcells fall back to SCF cell-mean NW.
  key = c('cell', bin_cols)

  scf_sub = scf %>%
    group_by(across(all_of(key))) %>%
    summarise(scf_pop = sum(weight),
              scf_mean_nw = sum(weight * nw) / sum(weight),
              .groups = 'drop')

  scf_cell_mean = scf %>%
    group_by(cell) %>%
    summarise(cell_mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop')

  puf_sub = puf_use %>%
    group_by(across(all_of(key))) %>%
    summarise(puf_pop = sum(weight), .groups = 'drop')

  joint = puf_sub %>%
    left_join(scf_sub, by = key) %>%
    left_join(scf_cell_mean, by = 'cell') %>%
    mutate(
      scf_mean_nw = if_else(is.na(scf_mean_nw) | is.na(scf_pop) | scf_pop == 0,
                            cell_mean_nw, scf_mean_nw),
      contribution = puf_pop * scf_mean_nw
    )

  per_cell = joint %>% group_by(cell) %>%
    summarise(rw_T = sum(contribution) / 1e12, .groups = 'drop')

  agg = sum(joint$contribution) / 1e12
  cat(sprintf('  [%s] aggregate = $%.2fT\n', label, agg))
  list(total = agg, per_cell = per_cell, label = label)
}


#--- Anchors --------------------------------------------------------------

scf_truth_total = sum(scf$weight * scf$nw) / 1e12
pop_only_T = scf_truth_total * sum(puf$weight) / sum(scf$weight)

cat(sprintf('SCF truth aggregate (no reweight):           $%.2fT\n', scf_truth_total))
cat(sprintf('SCF × PUF/SCF pop ratio (pop-only):          $%.2fT\n', pop_only_T))
cat('\n')


#--- Variant (a): age × cell joint reweight ------------------------------

cat('--- (a) age_bin × income-cell joint reweight ---\n')
rw_full = reweight_aggregate(scf, puf, c('age_bin'), 'age × cell  (PUF all)')


#--- Variant (b): age-only marginal (cell ignored) -----------------------

cat('\n--- (b) age-only marginal (no cell stratification) ---\n')
rw_age_only = local({
  key = c('age_bin')
  scf_sub = scf %>% group_by(across(all_of(key))) %>%
    summarise(scf_pop = sum(weight), scf_mean_nw = sum(weight * nw) / sum(weight),
              .groups = 'drop')
  puf_sub = puf %>% group_by(across(all_of(key))) %>%
    summarise(puf_pop = sum(weight), .groups = 'drop')
  joint = puf_sub %>% left_join(scf_sub, by = key) %>%
    mutate(scf_mean_nw = if_else(is.na(scf_mean_nw), 0, scf_mean_nw),
           contribution = puf_pop * scf_mean_nw)
  agg = sum(joint$contribution) / 1e12
  cat(sprintf('  [age only, ignore cell]  aggregate = $%.2fT\n', agg))
  agg
})


#--- Variant (c): same as (a) but PUF-filers only ------------------------

cat('\n--- (c) age × cell reweight, PUF FILERS only ---\n')
puf_filers = puf %>% filter(filer == 1)
rw_filers = reweight_aggregate(scf, puf_filers, c('age_bin'), 'age × cell  (filers only)')


#--- Variant (d): cell-only (no age) -------------------------------------
# Sanity: this should equal pop-scaled SCF when SCF cell pop sums to PUF cell pop
# only after rescale. In practice differs because of within-cell pop differences.

cat('\n--- (d) cell-only (within-cell pop rescale; no age) ---\n')
rw_cell_only = local({
  key = c('cell')
  scf_sub = scf %>% group_by(across(all_of(key))) %>%
    summarise(scf_mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop')
  puf_sub = puf %>% group_by(across(all_of(key))) %>%
    summarise(puf_pop = sum(weight), .groups = 'drop')
  joint = puf_sub %>% left_join(scf_sub, by = key) %>%
    mutate(contribution = puf_pop * scf_mean_nw)
  agg = sum(joint$contribution) / 1e12
  cat(sprintf('  [cell only]  aggregate = $%.2fT\n', agg))
  agg
})


#--- Summary --------------------------------------------------------------

cat('\n\n--- Summary table ---\n\n')
summary_tbl = tibble(
  variant = c(
    'SCF truth (no reweight)',
    'SCF × PUF/SCF pop ratio (pop-only)',
    'cell-only (PUF cell pops, SCF cell mean NW)',
    'age-only (PUF age pops, SCF age mean NW)',
    'age × cell joint (PUF all)',
    'age × cell joint (PUF filers only)',
    'age × int_div joint  (existing, ref: $180.64T)',
    'Pre-swap forest      (existing, ref: $201.06T)',
    'Pre-swap forest w/ $100 int_div thresh (ref: $187.23T)',
    'SCF truth target     (ref: $139.12T)'
  ),
  agg_T = c(scf_truth_total,
            pop_only_T,
            rw_cell_only,
            rw_age_only,
            rw_full$total,
            rw_filers$total,
            NA, NA, NA, NA)
) %>% mutate(agg_T = round(agg_T, 2))
print(as.data.frame(summary_tbl), row.names = FALSE)

cat('\n--- Gap closure (relative to PUF $201T forest, SCF $139T truth) ---\n')
gap_total = 201.06 - scf_truth_total
cat(sprintf('Total gap to close                      : $%.1fT\n', gap_total))
cat(sprintf('Pop-only closure                        : $%.1fT  (%.0f%%)\n',
            pop_only_T - scf_truth_total,
            100 * (pop_only_T - scf_truth_total) / gap_total))
cat(sprintf('age × cell (all)   closure              : $%.1fT  (%.0f%%)\n',
            rw_full$total - scf_truth_total,
            100 * (rw_full$total - scf_truth_total) / gap_total))
cat(sprintf('age × cell (filers) closure             : $%.1fT  (%.0f%%)\n',
            rw_filers$total - scf_truth_total,
            100 * (rw_filers$total - scf_truth_total) / gap_total))


#--- Per-cell breakdown ---------------------------------------------------

cat('\n--- Per-cell aggregate under each reweight ($T) ---\n\n')
scf_cell_truth = scf %>% group_by(cell) %>%
  summarise(scf_truth_T = sum(weight * nw) / 1e12, .groups = 'drop')

per_cell_tbl = scf_cell_truth %>%
  left_join(rw_full$per_cell %>% rename(rw_full_T = rw_T), by = 'cell') %>%
  left_join(rw_filers$per_cell %>% rename(rw_filers_T = rw_T), by = 'cell') %>%
  mutate(cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(per_cell_tbl), row.names = FALSE)

cat('\nDone.\n')
