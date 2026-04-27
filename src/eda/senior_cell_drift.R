#---------------------------------------------------------------------
# senior_cell_drift.R
#
# Two diagnostics:
#
# 1) PUF 2017 vs SOI 2017 cell drift: every (filing_status × age_group)
#    cell. Identifies whether senior over-count is just MFJ band 6 or
#    also in single / HoH band 6.
#
# 2) LP marginal check: the constraints the LP actually targets
#    (filing_status totals, age_group totals, AGI-pool totals). Are they
#    being hit? If yes, then drift is "permitted" by the marginal-only
#    spec; if no, the LP is failing to converge to its own targets.
#
# Plus a quick look at the impute_nonfilers age_group assignment, since
# we found 2.64M non-filer MFJ band-6 units in PUF 2017 and the rule in
# impute_nonfilers.R:92-96 is suspiciously coarse.
#---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(tibble)
})

PUF_2017 = '/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline/tax_units_2017.csv'
SOI_TBL  = '/nfs/roberts/project/pi_nrs36/shared/model_data/Compiled-SOI-Tables/v2/2023100323/historical/table_1_6.csv'

band_of = function(age) case_when(
  age < 26 ~ 1L, age < 35 ~ 2L, age < 45 ~ 3L,
  age < 55 ~ 4L, age < 65 ~ 5L, TRUE     ~ 6L
)

#-------------------- 1. Load and tag PUF 2017 ------------------------

puf_all = read_csv(PUF_2017, show_col_types = FALSE) %>%
  select(id, weight, filing_status, age1, age2, filer, dep_status) %>%
  mutate(age_group = band_of(age1),
         is_filer  = filer == 1L)

stopifnot(!any(is.na(puf_all$age1)),
          !any(is.na(puf_all$weight)),
          !any(is.na(puf_all$filing_status)),
          !any(is.na(puf_all$filer)),
          !any(is.na(puf_all$dep_status)))

cat(sprintf('PUF 2017 ALL records: %.2fM tax units\n',
            sum(puf_all$weight) / 1e6))
cat(sprintf('  filers (filer==1, all dep_status): %.2fM   (%.2fM dep + %.2fM non-dep)\n',
            sum(puf_all$weight[puf_all$is_filer]) / 1e6,
            sum(puf_all$weight[puf_all$is_filer & puf_all$dep_status == 1L]) / 1e6,
            sum(puf_all$weight[puf_all$is_filer & puf_all$dep_status == 0L]) / 1e6))
cat(sprintf('  non-filers (filer==0)              : %.2fM\n',
            sum(puf_all$weight[!puf_all$is_filer]) / 1e6))
cat(sprintf('  SOI 2017 total returns (incl dep)  : 152.90M\n'))

# For cell-drift comparison, "filers" means filer==1, regardless of dep_status,
# since SOI 1.6 includes dependent returns (it's "All Returns").
puf = puf_all

#-------------------- 2. Load SOI 2017 long ---------------------------

soi = read_csv(SOI_TBL, show_col_types = FALSE) %>%
  filter(year == 2017) %>%
  select(-year)

# Sum across AGI bin columns to get cell totals.
soi_cells = soi %>%
  rowwise() %>%
  mutate(soi_count = sum(c_across(-c(filing_status, age_group)),
                         na.rm = TRUE)) %>%
  ungroup() %>%
  select(filing_status, age_group, soi_count)

#-------------------- 3. PUF cells (filers only) ----------------------

puf_cells_filers = puf %>%
  filter(is_filer) %>%
  group_by(filing_status, age_group) %>%
  summarise(puf_filers = sum(weight), .groups = 'drop')

puf_cells_nonfilers = puf %>%
  filter(!is_filer) %>%
  group_by(filing_status, age_group) %>%
  summarise(puf_nonfilers = sum(weight), .groups = 'drop')

cell = soi_cells %>%
  full_join(puf_cells_filers,    by = c('filing_status', 'age_group')) %>%
  full_join(puf_cells_nonfilers, by = c('filing_status', 'age_group')) %>%
  mutate(across(c(soi_count, puf_filers, puf_nonfilers),
                ~ replace_na(., 0)),
         ratio_filers = puf_filers / soi_count)

cat('\n--- PUF 2017 vs SOI 2017 by (filing_status × age_group) cell (millions) ---\n')
cat('  filing_status: 1=single, 2=MFJ, 3=MFS, 4=HoH\n\n')

cell_pretty = cell %>%
  mutate(across(c(soi_count, puf_filers, puf_nonfilers), ~ . / 1e6),
         across(c(soi_count, puf_filers, puf_nonfilers), ~ round(., 2)),
         ratio_filers = round(ratio_filers, 2)) %>%
  arrange(filing_status, age_group)

print(cell_pretty, n = Inf)

#-------------------- 4. LP marginal constraints ----------------------
#
# Per config/target_info/baseline.csv, `returns` is constrained:
#   filing_status = "1 2 3 4" pooled, age_group = "1 2 3 4 5 6" pooled,
#   per AGI bin.
#
# Plus there are rows for each filing_status separately (age_group
# pooled) and each age_group separately (filing_status pooled). We
# check the top-level marginals here.

cat('\n--- LP marginal: filing_status totals, PUF filers vs SOI ---\n')
fs_marg = puf_cells_filers %>%
  group_by(filing_status) %>%
  summarise(puf_filers = sum(puf_filers), .groups = 'drop') %>%
  full_join(soi_cells %>%
              group_by(filing_status) %>%
              summarise(soi_count = sum(soi_count), .groups = 'drop'),
            by = 'filing_status') %>%
  mutate(across(c(puf_filers, soi_count), ~ . / 1e6),
         ratio = puf_filers / soi_count) %>%
  arrange(filing_status)
print(fs_marg %>% mutate(across(everything(), ~ if (is.numeric(.)) round(., 3) else .)))

cat('\n--- LP marginal: age_group totals, PUF filers vs SOI ---\n')
ag_marg = puf_cells_filers %>%
  group_by(age_group) %>%
  summarise(puf_filers = sum(puf_filers), .groups = 'drop') %>%
  full_join(soi_cells %>%
              group_by(age_group) %>%
              summarise(soi_count = sum(soi_count), .groups = 'drop'),
            by = 'age_group') %>%
  mutate(across(c(puf_filers, soi_count), ~ . / 1e6),
         ratio = puf_filers / soi_count) %>%
  arrange(age_group)
print(ag_marg %>% mutate(across(everything(), ~ if (is.numeric(.)) round(., 3) else .)))

cat('\n--- LP marginal: grand total returns (all fs × all ag) ---\n')
cat(sprintf('  PUF filers : %.2fM\n', sum(puf$weight[puf$is_filer]) / 1e6))
cat(sprintf('  SOI 2017   : %.2fM\n', sum(soi_cells$soi_count) / 1e6))
cat(sprintf('  ratio      : %.3fx\n',
            sum(puf$weight[puf$is_filer]) / sum(soi_cells$soi_count)))

#-------------------- 5. AGI-bin marginal -----------------------------
# (Skipped: tax_units_2017 output does not carry a single AGI column
# downstream — AGI is computed in Tax-Simulator. The fs/ag marginals
# above are sufficient to identify whether the LP is hitting its
# constraints at the level we care about.)

#-------------------- 6. Non-filer senior contribution ----------------

cat('\n--- Non-filer (impute_nonfilers.R) by (filing_status × age_group) ---\n')
nf = puf %>%
  filter(!is_filer) %>%
  group_by(filing_status, age_group) %>%
  summarise(weight_M = sum(weight) / 1e6, .groups = 'drop') %>%
  arrange(filing_status, age_group)
print(nf, n = Inf)

cat(sprintf('\nNon-filer total      : %.2fM\n',
            sum(puf$weight[!puf$is_filer]) / 1e6))
cat(sprintf('Non-filer band-6     : %.2fM\n',
            sum(puf$weight[!puf$is_filer & puf$age_group == 6L]) / 1e6))
cat(sprintf('Non-filer band-6 share of all non-filers: %.1f%%\n',
            100 * sum(puf$weight[!puf$is_filer & puf$age_group == 6L]) /
                  sum(puf$weight[!puf$is_filer])))

cat('\nDone.\n')
