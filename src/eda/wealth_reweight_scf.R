#---------------------------------------------
# wealth_reweight_scf.R
#
# Reweight SCF households within each income
# cell so their (age × int_div) joint marginal
# matches PUF's, then compute aggregate NW using
# SCF's native NW values. Compare to SCF truth
# and to the forest's PUF-imputed output.
#
# Question: if we accept PUF's X distribution as
# truthful about its population and map through
# SCF's Y|X, how much wealth does that imply?
# If close to SCF's $139T → the X-shift doesn't
# move aggregates; the forest's $201T must be
# artifact. If close to $201T → the X-shift
# genuinely implies more wealth, Stage 3 rescale
# is a calibration choice not a bug fix.
#
# Method: aggregate = Σ_{cell × age × int_div}
#   PUF_pop_here × SCF_mean_nw_here
# Empty subcells fall back to SCF cell-mean NW.
#
# Also reports age-only and int_div-only
# reweighting, to isolate each feature's
# contribution to the shift.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/wealth.R')

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')

AGE_BINS   = c(-1, 34, 44, 54, 64, 74, 80)
AGE_LABELS = c('<35','35-44','45-54','55-64','65-74','75+')

classify_int_div = function(x) {
  case_when(x > 0  ~ 'pos',
            x == 0 ~ 'zero',
            TRUE   ~ 'neg')
}

#--- SCF ------------------------------------------------------------------
scf_raw = read_rds('resources/cache/scf_tax_units.rds')
scf = scf_raw %>%
  mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income = wages_scf + business_scf + int_div_scf + capital_gains_scf +
             rent_scf + ss_pens_scf + ui_other_scf,
    has_int_div = classify_int_div(int_div_scf),
    age_bin = cut(age_older, breaks = AGE_BINS, labels = AGE_LABELS,
                   include.lowest = TRUE)
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
    int_div_puf = txbl_int + exempt_int + div_ord + div_pref,
    has_int_div = classify_int_div(int_div_puf),
    age_bin = cut(age_older, breaks = AGE_BINS, labels = AGE_LABELS,
                   include.lowest = TRUE)
  )

puf_ord = order(puf$income); puf_cum_w = cumsum(puf$weight[puf_ord]) / sum(puf$weight)
rank_puf = numeric(nrow(puf)); rank_puf[puf_ord] = 100 * puf_cum_w
puf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]

cat(sprintf('SCF: %d rows, pop %.1fM\n', nrow(scf), sum(scf$weight)/1e6))
cat(sprintf('PUF: %d rows, pop %.1fM\n\n', nrow(puf), sum(puf$weight)/1e6))

scf_truth_total = sum(scf$weight * scf$nw) / 1e12
cat(sprintf('SCF truth aggregate:                       $%.2fT\n', scf_truth_total))
cat(sprintf('SCF truth × PUF/SCF pop ratio:             $%.2fT  (pop-only scaling)\n',
            scf_truth_total * sum(puf$weight) / sum(scf$weight)))


#--- Core: reweight on a set of marginal bins -----------------------------

reweight_aggregate = function(scf, puf, bin_cols, label) {
  # For each (cell × [bin_cols]) compute PUF pop + SCF mean NW.
  # If no SCF rows for a subcell, fall back to the cell mean NW.
  key = c('cell', bin_cols)

  scf_sub = scf %>%
    group_by(across(all_of(key))) %>%
    summarise(scf_pop = sum(weight),
              scf_mean_nw = sum(weight * nw) / sum(weight),
              .groups = 'drop')

  scf_cell_mean = scf %>%
    group_by(cell) %>%
    summarise(cell_mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop')

  puf_sub = puf %>%
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
  cat(sprintf('Reweight SCF to PUF on [%s]:  aggregate = $%.2fT\n',
              paste(bin_cols, collapse = ' × '), agg))
  list(total = agg, per_cell = per_cell, label = label)
}


#--- Run 3 variants --------------------------------------------------------

cat('\n--- Reweighting experiments ---\n\n')

rw_both = reweight_aggregate(scf, puf, c('age_bin', 'has_int_div'), 'both')
rw_age  = reweight_aggregate(scf, puf, c('age_bin'),                'age only')
rw_id   = reweight_aggregate(scf, puf, c('has_int_div'),            'int_div only')


#--- Consolidated comparison ----------------------------------------------

cat('\n\n--- Summary ---\n\n')
cat(sprintf('SCF truth (native):                        $%.2fT   (SCF pop %.1fM)\n',
            scf_truth_total, sum(scf$weight)/1e6))
cat(sprintf('SCF × PUF/SCF pop ratio (no X shift):     $%.2fT   (PUF pop %.1fM)\n',
            scf_truth_total * sum(puf$weight) / sum(scf$weight),
            sum(puf$weight)/1e6))
cat(sprintf('Reweight on age only:                      $%.2fT\n', rw_age$total))
cat(sprintf('Reweight on int_div only:                  $%.2fT\n', rw_id$total))
cat(sprintf('Reweight on age × int_div (joint):         $%.2fT\n', rw_both$total))
cat('Forest on PUF (from percell harness):      $201.06T\n')

cat('\n--- Per-cell (PUF-pop scale) ---\n')
scf_truth_cell = scf %>% group_by(cell) %>%
  summarise(scf_truth = sum(weight * nw) / 1e12, .groups = 'drop')
puf_pop_cell = puf %>% group_by(cell) %>%
  summarise(puf_pop_M = sum(weight) / 1e6, .groups = 'drop')
scf_pop_scaled = scf_truth_cell %>%
  left_join(puf_pop_cell, by = 'cell') %>%
  left_join(scf %>% group_by(cell) %>%
            summarise(scf_pop_M = sum(weight) / 1e6, .groups = 'drop'),
            by = 'cell') %>%
  mutate(scf_pop_scaled_T = scf_truth * puf_pop_M / scf_pop_M)

full_tbl = scf_truth_cell %>%
  left_join(scf_pop_scaled %>% select(cell, scf_pop_scaled_T), by = 'cell') %>%
  left_join(rw_age$per_cell %>% rename(rw_age_T = rw_T), by = 'cell') %>%
  left_join(rw_id$per_cell  %>% rename(rw_id_T  = rw_T), by = 'cell') %>%
  left_join(rw_both$per_cell %>% rename(rw_both_T = rw_T), by = 'cell') %>%
  mutate(cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(full_tbl), row.names = FALSE)

cat('\nDone.\n')
