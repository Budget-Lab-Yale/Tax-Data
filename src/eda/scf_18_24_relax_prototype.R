#---------------------------------------------
# scf_18_24_relax_prototype.R
#
# Prototype option (b): relax Moore's NPEU
# rule for 18-24 roster members tagged
# `findep == 1`. Add them as new NPEU tax
# units with placeholder income = 0 and zero
# wealth (consistent with NPEU treatment in
# stage1).
#
# Goal: quantify how much SCF closes toward
# PUF's age × cell distribution, and how the
# wealth aggregate under (age × cell) reweight
# changes.
#
# Caveats:
#   - This is a counterfactual data tweak, not
#     a stage1 refactor. New rows have income=0,
#     so they all land in pct00to20.
#   - 2022 Sabelhaus (TPC) intentionally folds
#     these into parent PEUs as dependents.
#     2025 Gale/Hall/Sabelhaus methodology
#     keeps this same exclusion. So this is a
#     deliberate frame-shift from Sabelhaus to
#     PUF-IRS frame, not a "fix".
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(haven)
  library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: ... <output_dir>')
output_dir = args[1]

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')

SCF_RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2022/historical/p22i6.dta'

CALIB_INCOME_EDGES   = c(0, 20, 40, 60, 80, 90, 99, 99.9, 100)
CALIB_INCOME_BUCKETS = c('pct00to20','pct20to40','pct40to60','pct60to80',
                         'pct80to90','pct90to99','pct99to99.9','pct99.9to100')
AGE_BINS   = c(-1, 24, 34, 44, 54, 64, 74, 80)
AGE_LABELS = c('18-24','25-34','35-44','45-54','55-64','65-74','75-80')


#--- Existing SCF tax units ----------------------------------------------

scf_tu = read_rds('resources/cache/scf_tax_units.rds')
cat(sprintf('Baseline SCF tax units: %d rows, pop %.1fM\n',
            nrow(scf_tu), sum(scf_tu$weight) / 1e6))


#--- Raw SCF: enumerate 18-24 fd==1 roster members -----------------------

cat('Loading raw SCF...\n')
raw = haven::read_dta(SCF_RAW)
names(raw) = toupper(names(raw))

# 10 roster slots (matching stage1)
slot_age = paste0('X', c('110','116','122','128','134','204','210','216','222','228'))
slot_rel = paste0('X', c('108','114','120','126','132','202','208','214','220','226'))
slot_lw  = paste0('X', c('112','118','124','130','136','206','212','218','224','230'))
slot_fd  = paste0('X', c('113','119','125','131','137','207','213','219','225','231'))

raw = raw %>% mutate(weight_imp = X42001 / 5)

# Build long-format roster, filter to 18-24 with lw in 1..4 and fd == 1
roster_long = bind_rows(lapply(seq_along(slot_age), function(i) {
  tibble(
    Y1     = raw$Y1, YY1 = raw$YY1, weight = raw$weight_imp,
    slot   = i,
    age    = as.integer(raw[[slot_age[i]]]),
    rel    = as.integer(raw[[slot_rel[i]]]),
    lw     = as.integer(raw[[slot_lw[i]]]),
    fd     = as.integer(raw[[slot_fd[i]]])
  )
})) %>% filter(!is.na(age), age >= 0)

# SCF disclosure-rounds roster ages: actual 18-25 → coded 25,
# actual 26+ rounded to nearest 5 (so coded 25 covers actuals 18-27).
# Reference Stata uses publicuseagerecode (not ported here) to randomly
# assign specific ages within each band. For this prototype we treat
# coded age==25 as the candidate band and assign new NPEU rows age=21.

cat('\nFd-code mass by raw age value (roster only, pop M):\n')
age_probe = roster_long %>%
  filter(age > 0) %>%
  group_by(age, fd) %>%
  summarise(pop_M = round(sum(weight) / 1e6, 2), .groups = 'drop') %>%
  pivot_wider(names_from = fd, values_from = pop_M, values_fill = 0,
              names_prefix = 'fd=') %>%
  arrange(age)
print(as.data.frame(age_probe), row.names = FALSE)

# Candidates: rounded age=25 (actuals 18-27 inclusive), lw in 1..4,
# fd == 1 (financially-part-of-PEU; the dependents Sabelhaus folds in)
NEW_NPEU_AGE = 21L  # midpoint of 18-25; falls in our 18-24 bin

new_npeu = roster_long %>%
  filter(age == 25L, lw %in% 1:4, fd == 1L)

cat(sprintf('Roster members 18-24 with lw in 1..4 and fd==1:\n'))
cat(sprintf('  rows = %d, pop = %.2fM\n', nrow(new_npeu),
            sum(new_npeu$weight) / 1e6))

# Decompose by relationship
rel_split = new_npeu %>%
  mutate(rel_label = case_when(
    rel %in% c(4, 13)  ~ 'child of head/spouse',
    rel %in% c(7, 8)   ~ 'sibling',
    rel %in% c(28, 29) ~ 'in-law/partner',
    rel == 36          ~ 'foster child',
    TRUE                ~ paste0('rel=', rel)
  )) %>%
  group_by(rel_label) %>%
  summarise(pop_M = round(sum(weight) / 1e6, 2),
            n_rows = n(), .groups = 'drop') %>%
  arrange(desc(pop_M))
cat('\nRelationship breakdown of new candidates:\n')
print(as.data.frame(rel_split), row.names = FALSE)


#--- Construct synthetic NPEU tax-unit rows ------------------------------

# Match the schema of scf_tax_units. Income components all 0; wealth all 0.
# taxunitid = Y1 * 100 + (10 + slot) so it doesn't collide with existing
# PEU/NPEU IDs (existing NPEU uses 3..12; we use 11..20 to be safe).

zero_wealth_cols = wealth_y_vars  # 13 assets + 6 debts + 4 kg
income_zero_cols = c('income','wages_scf','business_scf','int_div_scf',
                     'capital_gains_scf','rent_scf','ss_pens_scf','ui_other_scf')

new_rows = new_npeu %>%
  mutate(
    taxunitid    = as.integer(Y1 * 100L + 10L + slot),
    scf_hh_id    = YY1,
    implicate    = as.integer(Y1 - 10L * YY1),
    age1         = NEW_NPEU_AGE,
    age2         = NA_integer_,
    n_dep        = 0L,
    married      = 0L,
    male1        = NA_integer_,
    male2        = NA_integer_,
    filestat     = 1L,
    taxunit_code = as.integer(10L + slot)
  ) %>%
  select(taxunitid, scf_hh_id, implicate, weight, age1, age2,
         n_dep, married, male1, male2, filestat, taxunit_code)

# Add zeroed income + wealth columns
for (v in income_zero_cols)   new_rows[[v]] = 0
for (v in zero_wealth_cols)   new_rows[[v]] = 0

# Reorder columns to match scf_tu exactly
new_rows = new_rows[, names(scf_tu)]

scf_tu_alt = bind_rows(scf_tu, new_rows)
cat(sprintf('\nAlt SCF tax units: %d rows (+%d), pop %.1fM (+%.1fM)\n',
            nrow(scf_tu_alt), nrow(new_rows),
            sum(scf_tu_alt$weight) / 1e6,
            sum(new_rows$weight) / 1e6))


#--- Common: build (age × cell) frames for both baseline and alt SCF ----

build_scf_frame = function(scf) {
  scf %>% mutate(
    age1_capped = pmin(as.integer(age1), 80L),
    age2_capped = if_else(!is.na(age2), pmin(as.integer(age2), 80L), NA_integer_),
    age_older   = if_else(!is.na(age2_capped), pmax(age1_capped, age2_capped),
                          age1_capped),
    income_calc = wages_scf + business_scf + int_div_scf + capital_gains_scf +
                  rent_scf + ss_pens_scf + ui_other_scf,
    age_bin = cut(age_older, breaks = AGE_BINS, labels = AGE_LABELS,
                   right = TRUE, include.lowest = TRUE)
  ) %>%
  (function(d) {
    d$nw = rowSums(d[, wealth_asset_vars]) - rowSums(d[, wealth_debt_vars])
    ord = order(d$income_calc)
    cum_w = cumsum(d$weight[ord]) / sum(d$weight)
    rank_v = numeric(nrow(d)); rank_v[ord] = 100 * cum_w
    d$cell = CALIB_INCOME_BUCKETS[findInterval(rank_v, CALIB_INCOME_EDGES,
                                                rightmost.closed = TRUE,
                                                all.inside = TRUE)]
    d
  })()
}

scf_base = build_scf_frame(scf_tu)
scf_alt  = build_scf_frame(scf_tu_alt)


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
puf_ord = order(puf$income); puf_cum_w = cumsum(puf$weight[puf_ord]) / sum(puf$weight)
rank_puf = numeric(nrow(puf)); rank_puf[puf_ord] = 100 * puf_cum_w
puf$cell = CALIB_INCOME_BUCKETS[findInterval(rank_puf, CALIB_INCOME_EDGES,
                                              rightmost.closed = TRUE,
                                              all.inside = TRUE)]


#--- Compare (age × cell) tax-unit pop ------------------------------------

cat('\n--- Age × cell pop totals (M tax units) ---\n')
counts_by = function(d) d %>% group_by(cell, age_bin) %>%
  summarise(pop_M = sum(weight) / 1e6, .groups = 'drop')

base_ct = counts_by(scf_base) %>% rename(scf_base_M = pop_M)
alt_ct  = counts_by(scf_alt)  %>% rename(scf_alt_M  = pop_M)
puf_ct  = counts_by(puf)      %>% rename(puf_M      = pop_M)

joint = full_join(puf_ct, base_ct, by = c('cell','age_bin')) %>%
  full_join(alt_ct, by = c('cell','age_bin')) %>%
  mutate(across(ends_with('_M'), ~ replace_na(.x, 0)),
         base_gap = puf_M - scf_base_M,
         alt_gap  = puf_M - scf_alt_M,
         gap_closed_M = base_gap - alt_gap)

cat('\n18-24 row only (where the prototype acts):\n')
youth = joint %>% filter(age_bin == '18-24') %>%
  select(cell, scf_base_M, scf_alt_M, puf_M, base_gap, alt_gap, gap_closed_M) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)),
         cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell)
print(as.data.frame(youth), row.names = FALSE)

cat('\nTotal pop comparison:\n')
totals = tibble(
  source = c('SCF baseline','SCF alt (relaxed)','PUF (filers + nonfilers)','PUF filers only'),
  pop_M  = c(sum(scf_base$weight)/1e6,
             sum(scf_alt$weight)/1e6,
             sum(puf$weight)/1e6,
             sum(puf$weight[puf$filer == 1])/1e6)
) %>% mutate(pop_M = round(pop_M, 2))
print(as.data.frame(totals), row.names = FALSE)


#--- Reweight aggregates --------------------------------------------------

cat('\n--- Wealth aggregate under (age × cell) reweight ---\n')

reweight_aggregate = function(scf_use, puf_use) {
  key = c('cell', 'age_bin')
  scf_sub = scf_use %>% group_by(across(all_of(key))) %>%
    summarise(scf_pop = sum(weight),
              scf_mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop')
  scf_cell_mean = scf_use %>% group_by(cell) %>%
    summarise(cell_mean_nw = sum(weight * nw) / sum(weight), .groups = 'drop')
  puf_sub = puf_use %>% group_by(across(all_of(key))) %>%
    summarise(puf_pop = sum(weight), .groups = 'drop')
  joint = puf_sub %>% left_join(scf_sub, by = key) %>%
    left_join(scf_cell_mean, by = 'cell') %>%
    mutate(scf_mean_nw = if_else(is.na(scf_mean_nw) | is.na(scf_pop) | scf_pop == 0,
                                  cell_mean_nw, scf_mean_nw),
           contribution = puf_pop * scf_mean_nw)
  list(total = sum(joint$contribution) / 1e12,
       per_cell = joint %>% group_by(cell) %>%
         summarise(rw_T = sum(contribution) / 1e12, .groups = 'drop'))
}

rw_base = reweight_aggregate(scf_base, puf)
rw_alt  = reweight_aggregate(scf_alt,  puf)

scf_truth_base = sum(scf_base$weight * scf_base$nw) / 1e12
scf_truth_alt  = sum(scf_alt$weight  * scf_alt$nw)  / 1e12

cat(sprintf('\nSCF truth (baseline)              : $%.2fT\n', scf_truth_base))
cat(sprintf('SCF truth (alt, +zero-NW rows)    : $%.2fT  (unchanged: new rows have 0 NW)\n',
            scf_truth_alt))
cat(sprintf('age × cell reweight (baseline)    : $%.2fT\n', rw_base$total))
cat(sprintf('age × cell reweight (alt)         : $%.2fT  (delta %+.2fT)\n',
            rw_alt$total, rw_alt$total - rw_base$total))

cat('\nPer-cell reweight comparison ($T):\n')
cmp = rw_base$per_cell %>% rename(rw_base_T = rw_T) %>%
  full_join(rw_alt$per_cell %>% rename(rw_alt_T = rw_T), by = 'cell') %>%
  mutate(delta_T = rw_alt_T - rw_base_T,
         cell = factor(cell, levels = CALIB_INCOME_BUCKETS)) %>%
  arrange(cell) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
print(as.data.frame(cmp), row.names = FALSE)


#--- Diagnostic: SCF mean NW in (pct00to20, 18-24) before/after ---------

cat('\n--- (pct00to20, 18-24) cell mean NW: baseline vs alt ---\n')
for (frame_name in c('baseline','alt')) {
  d = if (frame_name == 'baseline') scf_base else scf_alt
  s = d %>% filter(cell == 'pct00to20', age_bin == '18-24')
  if (nrow(s) > 0) {
    cat(sprintf('  %-9s pop=%6.2fM  mean_nw=$%9.0f  total=$%5.2fT\n',
                frame_name, sum(s$weight)/1e6, sum(s$weight*s$nw)/sum(s$weight),
                sum(s$weight*s$nw)/1e12))
  }
}

cat('\nDone.\n')
