#---------------------------------------------
# diagnose_age_income.R
#
# Compares SCF (target) vs PUF (post-upstream-fix)
# at the (income × age) cell level, and runs the
# Stage-2 leaf walk to produce raw-DRF counts and
# amounts WITHOUT running the tilt. Tells us:
#
#   1. Cell pop ratio — is the frame mismatch fixed?
#   2. Raw-DRF count vs SCF count per (cell × cat) —
#      where would the tilt have to push counts?
#   3. Raw-DRF amount vs SCF amount per (cell × cat) —
#      where does aggregate $ already match?
#
# Usage (sbatch):
#   Rscript src/eda/diagnose_age_income.R <output_dir>
#
# Reads:
#   <output_dir>/puf_2022_snapshot.rds
#   resources/cache/scf_tax_units.rds
# Writes:
#   <output_dir>/diagnose_age_income.rds
#   stdout: human-readable tables
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble); library(Hmisc)
})

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop('Usage: diagnose_age_income.R <output_dir>')
output_dir = args[1]
stopifnot(dir.exists(output_dir))

estimate_models = 0L     # use cached forests
do_lp           = 0

source('src/imputations/helpers.R')
source('src/imputations/wealth_schema.R')
source('src/imputations/stage3_target_qc.R')

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

#--- Load ------------------------------------------------------------------

cat('Loading SCF + PUF snapshot...\n')
scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')
puf_2022 = read_rds(file.path(output_dir, 'puf_2022_snapshot.rds'))
cat(sprintf('  SCF tax units: %d  weighted: %.1fM\n',
            nrow(scf_tax_units), sum(scf_tax_units$weight) / 1e6))
cat(sprintf('  PUF 2022 rows: %d  weighted: %.1fM\n',
            nrow(puf_2022), sum(puf_2022$weight) / 1e6))

#--- Cell assignment on both sides ----------------------------------------

scf_age2 = ifelse(is.na(scf_tax_units$age2), 0L, scf_tax_units$age2)
scf_age_older = pmax(pmin(80L, scf_tax_units$age1), pmin(80L, scf_age2))
scf_inc  = with(scf_tax_units,
  wages_scf + business_scf + int_div_scf + capital_gains_scf +
  rent_scf + ss_pens_scf + ui_other_scf)
scf_y    = scf_to_y_local(scf_tax_units)
scf_cv   = compute_category_values(scf_y)
scf = bind_cols(
  data.frame(weight = scf_y$weight, age_older = scf_age_older,
             income = scf_inc),
  scf_cv)
scf = assign_calibration_cells(scf, scf$income, scf$age_older, scf$weight)

# PUF income definition matching wealth.R.
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

#--- (1) Cell pop comparison ----------------------------------------------

scf_pop_per_cell = scf %>%
  group_by(cell_income, cell_age) %>%
  summarise(scf_pop = sum(weight), .groups = 'drop')

puf_meta = data.frame(weight = puf_2022$weight,
                      age_older = puf_age_older,
                      income = puf_inc,
                      dep_status = puf_2022$dep_status)
puf_meta = assign_calibration_cells(puf_meta, puf_meta$income,
                                    puf_meta$age_older, puf_meta$weight)

# Restrict PUF to non-dependents (the wealth-imputation universe).
puf_nd = puf_meta %>% filter(dep_status == 0L)
puf_pop_per_cell = puf_nd %>%
  group_by(cell_income, cell_age) %>%
  summarise(puf_pop = sum(weight), .groups = 'drop')

pop_cmp = full_join(scf_pop_per_cell, puf_pop_per_cell,
                    by = c('cell_income', 'cell_age')) %>%
  mutate(scf_M = scf_pop / 1e6,
         puf_M = puf_pop / 1e6,
         ratio = puf_pop / scf_pop) %>%
  arrange(cell_income, cell_age)

cat('\n=== (1) Cell pop comparison: PUF (non-dep) / SCF ===\n')
print(pop_cmp %>% mutate(scf_M = sprintf('%.2f', scf_M),
                         puf_M = sprintf('%.2f', puf_M),
                         ratio = sprintf('%.2f', ratio)) %>%
        select(cell_income, cell_age, scf_M, puf_M, ratio),
      n = Inf, width = Inf)

#--- (2) Stage 2 leaf walk on new PUF X (no tilt) -------------------------

# To get raw-DRF counts/amounts, we need to run the per-cell DRF leaf
# walk that wealth.R Stage 2 uses. That requires the same SCF training
# pipeline (bootstrap + DRF train_or_load) plus the per-PUF leaf draw.
#
# Cheapest path: rebuild the parts of run_wealth_imputation through
# Stage 2 only. Source wealth.R and run a stripped variant inline.

cat('\nRunning Stage 2 leaf walk on new PUF X (cached forests)...\n')

# Source wealth.R for `run_wealth_imputation` and `scf_to_y`, then
# stop after Stage 2. The cleanest hack: call run_wealth_imputation
# with a tilt_options that triggers the early "no viable targets"
# branch in every bucket — i.e., use a target spec that's empty so
# every bucket falls back to pre_tilt_donors. NOT GENERIC; kept here.
#
# Alternative: just run run_wealth_imputation as-is. Tilt will run on
# new data, and we report raw_DRF (= y_pre_tilt) from its result. Cost
# is tilt compute (~50min) but we DO want to see it anyway.
# NOTE: do NOT source stage1_scf_tax_units.R — it needs interface_paths
# from configure.R, which we don't run. We already loaded scf_tax_units
# from cache above.
source('src/imputations/wealth.R')

t0 = Sys.time()
result = run_wealth_imputation(puf_2022, scf_tax_units, skip_tilt = TRUE)
cat(sprintf('Stage 2 only run: %.1f min\n',
            as.numeric(Sys.time() - t0, units = 'mins')))

#--- (3) Per-(cell × cat) count + amount comparison -----------------------

# Build raw_DRF frame (= y_pre_tilt) with cell assignments + cat values.
raw_y  = result$y_pre_tilt
raw_cv = compute_category_values(raw_y)
raw = bind_cols(
  raw_y %>% select(id) %>%
    inner_join(puf_meta %>% mutate(id = puf_2022$id) %>%
                 select(id, weight, age_older, income, cell_income,
                        cell_age, dep_status),
               by = 'id'),
  raw_cv) %>% filter(dep_status == 0L)

# Counts per (cell × cat).
count_amount_per_cell_cat = function(df, lab) {
  out = df %>%
    group_by(cell_income, cell_age) %>%
    summarise(across(starts_with('cat_'),
                     list(cnt = ~ sum(weight * (. > 0)),
                          amt = ~ sum(weight * .)),
                     .names = '{.col}_{.fn}'),
              .groups = 'drop') %>%
    mutate(source = lab)
  out
}

scf_ca = count_amount_per_cell_cat(scf, 'SCF')
raw_ca = count_amount_per_cell_cat(raw, 'raw_DRF')

# Long-format gap per (cell × cat × {cnt, amt}).
ca_long = bind_rows(scf_ca, raw_ca) %>%
  pivot_longer(cols = starts_with('cat_'),
               names_to = c('category', 'metric'),
               names_pattern = 'cat_(.+)_(cnt|amt)',
               values_to = 'value') %>%
  pivot_wider(names_from = source, values_from = value) %>%
  mutate(ratio = raw_DRF / pmax(SCF, 1))

cat('\n=== (2) Raw-DRF vs SCF — counts (M households) per (cell × cat) ===\n')
cnt_tbl = ca_long %>% filter(metric == 'cnt') %>%
  transmute(cell_income, cell_age, category,
            scf_M = sprintf('%.2f', SCF / 1e6),
            raw_M = sprintf('%.2f', raw_DRF / 1e6),
            ratio = sprintf('%.2f', ratio))
print(cnt_tbl, n = Inf, width = Inf)

cat('\n=== (3) Raw-DRF vs SCF — amounts ($T) per (cell × cat) ===\n')
amt_tbl = ca_long %>% filter(metric == 'amt') %>%
  transmute(cell_income, cell_age, category,
            scf_T = sprintf('%.2f', SCF / 1e12),
            raw_T = sprintf('%.2f', raw_DRF / 1e12),
            ratio = sprintf('%.2f', ratio))
print(amt_tbl, n = Inf, width = Inf)

cat('\n=== (4) Worst-off cells: |log2 ratio| > 0.3 (>= 1.23x or <= 0.81x) ===\n')
worst = ca_long %>%
  filter(SCF > 1e3, abs(log2(pmax(ratio, 1e-3))) > 0.3) %>%
  arrange(desc(abs(log2(pmax(ratio, 1e-3))))) %>%
  transmute(cell_income, cell_age, category, metric,
            scf = ifelse(metric == 'cnt',
                         sprintf('%.2fM', SCF / 1e6),
                         sprintf('$%.2fT', SCF / 1e12)),
            raw = ifelse(metric == 'cnt',
                         sprintf('%.2fM', raw_DRF / 1e6),
                         sprintf('$%.2fT', raw_DRF / 1e12)),
            ratio = sprintf('%.2f', ratio))
print(worst, n = Inf, width = Inf)

#--- Save ------------------------------------------------------------------

out_path = file.path(output_dir, 'diagnose_age_income.rds')
saveRDS(list(
  pop_cmp     = pop_cmp,
  count_table = ca_long %>% filter(metric == 'cnt'),
  amount_table = ca_long %>% filter(metric == 'amt'),
  result      = result   # full wealth_result so we can do v4 follow-up too
), out_path)
cat(sprintf('\nSaved diagnostic dump: %s\n', out_path))

cat('\nDone.\n')
