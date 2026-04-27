#---------------------------------------------
# senior_pop.R
#
# Senior counts (PUF vs SCF), non-dependent only,
# at both tax-unit and individual-population level.
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble)
})

scf_tax_units = read_rds('resources/cache/scf_tax_units.rds')
puf_2022 = read_csv(
  '/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline/tax_units_2022.csv',
  show_col_types = FALSE)

cat(sprintf('PUF rows: %d\n', nrow(puf_2022)))
cat(sprintf('PUF non-dep rows: %d\n', sum(puf_2022$dep_status == 0)))
cat(sprintf('SCF tax units: %d\n', nrow(scf_tax_units)))

# PUF: filter to dep_status == 0
puf_nd = puf_2022 %>% filter(dep_status == 0L)

# Senior tax-unit (older person >= 65) — capping at 80 NOT applied here
# since we want raw senior status. age1 / age2 raw.
puf_age1 = puf_nd$age1
puf_age2 = puf_nd$age2  # NA for singles
puf_age_older = pmax(puf_age1, puf_age2, na.rm = FALSE)
# pmax with NA on age2 returns NA — so handle singles:
puf_age_older = ifelse(is.na(puf_age2), puf_age1, pmax(puf_age1, puf_age2))

is_senior_taxunit_puf = puf_age_older >= 65

# Individual-level: count age1 >= 65 + count age2 >= 65 (not NA)
puf_seniors_indiv = sum(puf_nd$weight[puf_age1 >= 65], na.rm = TRUE) +
  sum(puf_nd$weight[!is.na(puf_age2) & puf_age2 >= 65], na.rm = TRUE)

cat('\n=== PUF (non-dependent, dep_status==0) ===\n')
cat(sprintf('Total tax units (weighted): %.2fM\n',
            sum(puf_nd$weight) / 1e6))
cat(sprintf('Senior tax units (older spouse >= 65): %.2fM\n',
            sum(puf_nd$weight[is_senior_taxunit_puf]) / 1e6))
cat(sprintf('Senior individuals (count each spouse): %.2fM\n',
            puf_seniors_indiv / 1e6))

# Joint senior status (both spouses >= 65 if married)
both_senior_puf = !is.na(puf_age2) & puf_age1 >= 65 & puf_age2 >= 65
one_senior_puf  = is_senior_taxunit_puf & !both_senior_puf
cat(sprintf('  ...of which both spouses senior: %.2fM tax units\n',
            sum(puf_nd$weight[both_senior_puf]) / 1e6))
cat(sprintf('  ...of which only one senior:     %.2fM tax units\n',
            sum(puf_nd$weight[one_senior_puf]) / 1e6))

# SCF
scf_age1 = scf_tax_units$age1
scf_age2 = scf_tax_units$age2  # NA for singles
scf_age_older = ifelse(is.na(scf_age2), scf_age1, pmax(scf_age1, scf_age2))
is_senior_taxunit_scf = scf_age_older >= 65

scf_seniors_indiv = sum(scf_tax_units$weight[scf_age1 >= 65], na.rm = TRUE) +
  sum(scf_tax_units$weight[!is.na(scf_age2) & scf_age2 >= 65], na.rm = TRUE)

both_senior_scf = !is.na(scf_age2) & scf_age1 >= 65 & scf_age2 >= 65
one_senior_scf  = is_senior_taxunit_scf & !both_senior_scf

cat('\n=== SCF (split tax units, all non-dependent by construction) ===\n')
cat(sprintf('Total tax units (weighted): %.2fM\n',
            sum(scf_tax_units$weight) / 1e6))
cat(sprintf('Senior tax units (older spouse >= 65): %.2fM\n',
            sum(scf_tax_units$weight[is_senior_taxunit_scf]) / 1e6))
cat(sprintf('Senior individuals (count each spouse): %.2fM\n',
            scf_seniors_indiv / 1e6))
cat(sprintf('  ...of which both spouses senior: %.2fM tax units\n',
            sum(scf_tax_units$weight[both_senior_scf]) / 1e6))
cat(sprintf('  ...of which only one senior:     %.2fM tax units\n',
            sum(scf_tax_units$weight[one_senior_scf]) / 1e6))

# Comparison
cat('\n=== Ratio (PUF / SCF) ===\n')
cat(sprintf('Total tax units:     %.2fx\n',
            sum(puf_nd$weight) / sum(scf_tax_units$weight)))
cat(sprintf('Senior tax units:    %.2fx\n',
            sum(puf_nd$weight[is_senior_taxunit_puf]) /
            sum(scf_tax_units$weight[is_senior_taxunit_scf])))
cat(sprintf('Senior individuals:  %.2fx\n',
            puf_seniors_indiv / scf_seniors_indiv))
cat(sprintf('Non-senior tax units: %.2fx\n',
            sum(puf_nd$weight[!is_senior_taxunit_puf]) /
            sum(scf_tax_units$weight[!is_senior_taxunit_scf])))

cat('\n=== Census 2022 reference (rough): age 65+ = ~58M individuals nationally ===\n')

cat('\nDone.\n')
