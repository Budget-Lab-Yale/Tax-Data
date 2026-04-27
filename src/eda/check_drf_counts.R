#---------------------------------------------
# check_drf_counts.R
#
# How badly are raw-DRF counts off from SCF, per
# (cell_income x cell_age x category)? Quick diagnostic
# to inform whether dropping count targets in
# pct00to20 (or other cells) is justified.
#
# Usage (sbatch):
#   Rscript src/eda/check_drf_counts.R <output_dir>
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
output_dir = if (length(args) >= 1L) args[1] else
  '/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline'

source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')
source('src/imputations/helpers.R')

scf_to_y_local = function(df) {
  if (all(wealth_y_vars %in% names(df))) return(df)
  df %>% mutate(
    cash             = LIQ + CDS,
    equities         = STOCKS + STMUTF + COMUTF,
    bonds            = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    retirement       = IRAKH + THRIFT + FUTPEN + CURRPEN,
    life_ins         = CASHLI, annuities = ANNUIT, trusts = TRUSTS,
    other_fin        = OTHFIN + OMUTF, pass_throughs = BUS,
    primary_home     = HOUSES, other_home = ORESRE, re_fund = NNRESRE,
    other_nonfin     = VEHIC + OTHNFIN,
    primary_mortgage = MRTHEL, other_mortgage = RESDBT,
    credit_lines     = OTHLOC, credit_cards = CCBAL,
    installment_debt = INSTALL, other_debt = ODEBT,
    kg_primary_home  = KGHOUSE, kg_other_re = KGORE,
    kg_pass_throughs = KGBUS, kg_other = KGSTMF
  )
}

scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')
puf_2022 = read_csv(file.path(output_dir, 'tax_units_2022.csv'),
                    show_col_types = FALSE)
diag = readRDS(file.path(output_dir, 'wealth_harness_tilt_diag.rds'))

# Build SCF and raw_DRF (= y_pre_tilt) frames with cell assignments.
puf_age2 = ifelse(is.na(puf_2022$age2), 0L, puf_2022$age2)
puf_age_older = pmax(pmin(80L, puf_2022$age1), pmin(80L, puf_age2))
puf_inc = with(puf_2022,
  wages + sole_prop + farm +
  scorp_active  - scorp_active_loss  - scorp_179 +
  scorp_passive - scorp_passive_loss +
  part_active   - part_active_loss   - part_179 +
  part_passive  - part_passive_loss +
  txbl_int + exempt_int + div_ord + div_pref +
  kg_lt + kg_st + gross_ss + gross_pens_dist + ui +
  rent - rent_loss + estate - estate_loss)

puf_meta = data.frame(weight = puf_2022$weight,
                      age_older = puf_age_older,
                      income = puf_inc)
puf_meta = assign_calibration_cells(puf_meta, puf_meta$income,
                                    puf_meta$age_older, puf_meta$weight)

# Match diag$y_pre_tilt's id order to puf_meta order via id.
puf_meta$id = puf_2022$id
raw_y = diag$y_pre_tilt %>% inner_join(puf_meta, by = 'id')
raw_cv = compute_category_values(raw_y)
raw = bind_cols(raw_y[, c('id', 'weight', 'age_older', 'income',
                           'cell_income', 'cell_age')], raw_cv)

# SCF frame.
scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_age_older = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_y = scf_to_y_local(scf_tax_units)
scf_meta = data.frame(weight = scf_y$weight,
                      age_older = scf_age_older,
                      income = scf_inc)
scf_meta = assign_calibration_cells(scf_meta, scf_meta$income,
                                    scf_meta$age_older, scf_meta$weight)
scf_cv = compute_category_values(scf_y)
scf = bind_cols(scf_meta, scf_cv)

# Per (cell × cat) weighted count.
cells = expand.grid(cell_income = CALIB_INCOME_BUCKETS,
                    cell_age = CALIB_AGE_BUCKETS,
                    stringsAsFactors = FALSE)

count_per_cell_cat = function(df, label) {
  bind_rows(lapply(seq_len(nrow(cells)), function(i) {
    ci = cells$cell_income[i]; ca = cells$cell_age[i]
    mask = df$cell_income == ci & df$cell_age == ca
    if (sum(mask) == 0) return(NULL)
    w = df$weight[mask]
    n = sum(w)
    out = tibble(cell_income = ci, cell_age = ca, source = label,
                 n_records = n)
    for (cn in CALIB_CATEGORIES) {
      out[[cn]] = sum(w * (df[[paste0('cat_', cn)]][mask] > 0))
    }
    out
  }))
}

scf_counts = count_per_cell_cat(scf, 'SCF')
raw_counts = count_per_cell_cat(raw, 'raw_DRF')

# Combined comparison: per (cell, category), SCF count vs raw count, ratio.
# Drop n_records before pivoting so SCF and raw rows merge cleanly on
# (cell_income, cell_age, category).
cmp = bind_rows(scf_counts, raw_counts) %>%
  select(-n_records) %>%
  pivot_longer(cols = all_of(CALIB_CATEGORIES),
               names_to = 'category', values_to = 'count') %>%
  pivot_wider(names_from = source, values_from = count) %>%
  left_join(scf_counts %>%
              transmute(cell_income, cell_age, scf_pop = n_records),
            by = c('cell_income', 'cell_age')) %>%
  left_join(raw_counts %>%
              transmute(cell_income, cell_age, raw_pop = n_records),
            by = c('cell_income', 'cell_age')) %>%
  mutate(scf_share  = SCF     / scf_pop,
         raw_share  = raw_DRF / raw_pop,
         ratio_raw_to_scf = raw_DRF / pmax(SCF, 1))

# Pop sizes per cell (SCF vs PUF).
pop_cmp = bind_rows(
  scf_counts %>% select(cell_income, cell_age, source, n_records),
  raw_counts %>% select(cell_income, cell_age, source, n_records)
) %>% pivot_wider(names_from = source, values_from = n_records,
                  names_prefix = 'pop_') %>%
  mutate(pop_ratio_puf_to_scf = pop_raw_DRF / pop_SCF)

cat('=== Cell population sizes (PUF vs SCF) ===\n')
print(pop_cmp %>% mutate(across(starts_with('pop_'),
                                ~ sprintf('%.1fM', . / 1e6)),
                          pop_ratio_puf_to_scf = sprintf('%.2f',
                            pmax(0, as.numeric(gsub("M","",
                              gsub("\\.\\d+M","", pop_raw_DRF)))) / 1)),
      n = Inf, width = Inf)

# Just print the simple version.
cat('\n=== Cell pops (raw_DRF M weighted / SCF M weighted, ratio) ===\n')
ptab = pop_cmp %>%
  transmute(cell_income, cell_age,
            scf_M = pop_SCF / 1e6,
            puf_M = pop_raw_DRF / 1e6,
            ratio = pop_raw_DRF / pop_SCF) %>%
  mutate(across(c(scf_M, puf_M), ~ sprintf('%.1f', .)),
         ratio = sprintf('%.2f', as.numeric(ratio)))
print(ptab, n = Inf, width = Inf)

cat('\n=== Per (cell × cat) count: SCF vs raw_DRF (M households, ratio raw/SCF) ===\n')
out_tbl = cmp %>%
  transmute(cell_income, cell_age, category,
            scf_M = sprintf('%.2f', SCF / 1e6),
            raw_M = sprintf('%.2f', raw_DRF / 1e6),
            scf_pct = sprintf('%.1f%%', 100 * scf_share),
            raw_pct = sprintf('%.1f%%', 100 * raw_share),
            ratio = sprintf('%.2f', ratio_raw_to_scf))
print(out_tbl, n = Inf, width = Inf)

cat('\n=== pct00to20 only — count comparison ===\n')
print(out_tbl %>% filter(cell_income == 'pct00to20'),
      n = Inf, width = Inf)

cat('\n=== Worst-off raw_DRF cells (|log2 ratio| > 0.5) ===\n')
print(cmp %>% filter(SCF > 1e3,
                     abs(log2(pmax(ratio_raw_to_scf, 1e-3))) > 0.5) %>%
        arrange(desc(abs(log2(pmax(ratio_raw_to_scf, 1e-3))))) %>%
        transmute(cell_income, cell_age, category,
                  scf_M = sprintf('%.2f', SCF / 1e6),
                  raw_M = sprintf('%.2f', raw_DRF / 1e6),
                  ratio = sprintf('%.2f', ratio_raw_to_scf)),
      n = Inf, width = Inf)

cat('\nDone.\n')
