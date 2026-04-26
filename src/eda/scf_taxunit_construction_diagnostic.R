#---------------------------------------------
# scf_taxunit_construction_diagnostic.R
#
# Internal anatomy of Moore's SAS port
# (src/imputations/stage1_scf_tax_units.R).
# Diagnostic targets:
#
# (1) Tax-units-per-household ratio
#     overall + by HH composition.
# (2) PEU-split rate (TAXUNIT=0 vs 1+2),
#     broken down by marital/partnership state.
# (3) NPEU enumeration: how often does
#     X7050 ≥ 1 fire, what's the age
#     distribution of NPEU members emitted,
#     and how many roster members aged 18-30
#     are *eligible* but not made NPEU
#     (because lw or findep didn't qualify).
# (4) Age × roster-position cross-tab of
#     SCF members who are 18-30 but folded
#     into the PEU as kids (relationship
#     code ∈ {4, 13}) — these are the
#     candidate "missing tax units" if Moore's
#     port is dropping young filers.
# (5) Filing-status distribution vs IRS SOI
#     2022 filer counts (~163.1M total returns;
#     SOI 2022 split: ~78M Single, ~57M MFJ,
#     ~3M MFS, ~24M HoH; ~1.5M widowed).
#---------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(haven)
})

# Hardcode SCF raw path to avoid sourcing configure.R (has side effects).
# Resolves to interface_paths$SCF in src/configure.R.
SCF_RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2022/historical/p22i6.dta'

cat('Loading scf_tax_units.rds + raw SCF (p22i6.dta)...\n')

scf_tu = read_rds('resources/cache/scf_tax_units.rds')
raw = haven::read_dta(SCF_RAW)
names(raw) = toupper(names(raw))

cat(sprintf('  scf_tax_units: %d rows, %d distinct households\n',
            nrow(scf_tu), n_distinct(scf_tu$scf_hh_id)))
cat(sprintf('  raw SCF      : %d rows (5 implicates × ~6,000 HHs)\n', nrow(raw)))


#--- (1) Tax-units-per-household -----------------------------------------

cat('\n--- (1) Tax units per SCF household ---\n\n')

# scf_hh_id collapses across implicates already; weight is per implicate / 5
hh_taxunit_ct = scf_tu %>%
  group_by(scf_hh_id) %>%
  summarise(n_tu = n(),
            hh_weight = sum(weight) / first(n()) * 5,  # avg-per-row × 5 ≈ HH weight
            .groups = 'drop')

cat('Tax-unit-count distribution across HHs (combined across 5 implicates):\n')
ct_dist = hh_taxunit_ct %>%
  group_by(n_tu) %>%
  summarise(n_HHs = n(),
            share = round(n() / nrow(hh_taxunit_ct), 4),
            .groups = 'drop')
print(as.data.frame(ct_dist), row.names = FALSE)

# More useful: per-implicate (one HH = 5 rows in scf_hh_id terms; n_tu sums all 5)
# The Moore "avg tax units per HH" computed in stage1 is mean(n_tu) / 5
mean_tu_per_hh = mean(hh_taxunit_ct$n_tu) / 5
cat(sprintf('\nAvg tax units per household (across implicates): %.3f\n', mean_tu_per_hh))


#--- (2) PEU split breakdown ---------------------------------------------

cat('\n--- (2) PEU split classification ---\n')
cat('taxunit_code: 0=single PEU TU; 1,2=split PEU; 3+=NPEU\n\n')

tu_class = scf_tu %>%
  mutate(class = case_when(
    taxunit_code == 0L           ~ '0 (single PEU)',
    taxunit_code %in% c(1L, 2L)  ~ '1/2 (split PEU)',
    taxunit_code >= 3L           ~ '3+ (NPEU)',
    TRUE                          ~ 'NA'
  )) %>%
  group_by(class) %>%
  summarise(n_rows = n(),
            pop_M  = sum(weight) / 1e6,
            share_pop = sum(weight) / sum(scf_tu$weight),
            .groups = 'drop') %>%
  mutate(share_pop = round(share_pop, 4),
         pop_M = round(pop_M, 2))
print(as.data.frame(tu_class), row.names = FALSE)


#--- (3) NPEU emission anatomy -------------------------------------------

cat('\n--- (3) NPEU emission anatomy ---\n')
cat('NPEU trigger conditions in stage1: X7050 >= 1 AND PEU-TAXUNIT in (0,1)\n\n')

# Per-row NPEU-eligible flag
raw_eligible = raw %>%
  transmute(Y1, X7050,
            npeu_trigger_pop = X42001 / 5)

cat(sprintf('SCF HHs with X7050 >= 1 (pop): %.1fM (%.1f%% of HHs by pop)\n',
            sum(raw_eligible$npeu_trigger_pop[raw_eligible$X7050 >= 1]) / 1e6,
            100 * sum(raw_eligible$npeu_trigger_pop[raw_eligible$X7050 >= 1]) /
                  sum(raw_eligible$npeu_trigger_pop)))

# Of those, how many actually produced NPEU rows in scf_tax_units?
npeu_rows = scf_tu %>% filter(taxunit_code >= 3L)
cat(sprintf('NPEU rows emitted              : %d  (pop %.1fM)\n',
            nrow(npeu_rows), sum(npeu_rows$weight) / 1e6))

# Age distribution of NPEU members emitted
cat('\nAge distribution of emitted NPEU tax-unit rows:\n')
npeu_age_dist = npeu_rows %>%
  mutate(age_bin = cut(age1, breaks = c(-1, 17, 24, 29, 39, 49, 64, 80),
                        labels = c('<18','18-24','25-29','30-39','40-49',
                                   '50-64','65+'))) %>%
  group_by(age_bin) %>%
  summarise(n_rows = n(), pop_M = round(sum(weight) / 1e6, 2),
            .groups = 'drop')
print(as.data.frame(npeu_age_dist), row.names = FALSE)


#--- (4) Folded-young-roster diagnostic ---------------------------------

cat('\n--- (4) Roster members aged 18-30 by livewith × findep ---\n')
cat('Did Moore\'s port make them NPEU TUs, or fold them into the parent PEU?\n\n')

# 10 roster slots: ages X110, X116, X122, X128, X134, X204, X210, X216, X222, X228
# Relationship X108..X226. livewith X112..X230. findep X113..X231.
slot_age   = paste0('X', c('110','116','122','128','134','204','210','216','222','228'))
slot_rel   = paste0('X', c('108','114','120','126','132','202','208','214','220','226'))
slot_lw    = paste0('X', c('112','118','124','130','136','206','212','218','224','230'))
slot_fd    = paste0('X', c('113','119','125','131','137','207','213','219','225','231'))

# Long-format roster
roster_long = bind_rows(lapply(seq_along(slot_age), function(i) {
  tibble(
    Y1     = raw$Y1,
    weight = raw$X42001 / 5,  # implicate weight
    slot   = i,
    age    = raw[[slot_age[i]]],
    rel    = raw[[slot_rel[i]]],
    lw     = raw[[slot_lw[i]]],
    fd     = raw[[slot_fd[i]]]
  )
})) %>% filter(age >= 0)  # drop empty slots

# 18-30 only
young = roster_long %>% filter(age >= 18, age <= 30)

cat(sprintf('Total roster members aged 18-30: %.1fM (pop)\n',
            sum(young$weight) / 1e6))

cat('\nBy livewith × findep code (NPEU criterion: lw in 1-4 AND fd in 2-5):\n')
young_lw_fd = young %>%
  mutate(npeu_eligible = (lw %in% 1:4) & (fd %in% 2:5)) %>%
  group_by(lw, fd, npeu_eligible) %>%
  summarise(pop_M = round(sum(weight) / 1e6, 2),
            n_rows = n(), .groups = 'drop') %>%
  arrange(desc(pop_M))
print(as.data.frame(young_lw_fd %>% slice_head(n = 20)), row.names = FALSE)

cat('\nNPEU-eligible vs ineligible split (age 18-30):\n')
young_split = young %>%
  mutate(npeu_eligible = (lw %in% 1:4) & (fd %in% 2:5),
         is_kid_rel    = rel %in% c(4, 13)) %>%
  group_by(npeu_eligible, is_kid_rel) %>%
  summarise(pop_M = round(sum(weight) / 1e6, 2), .groups = 'drop')
print(as.data.frame(young_split), row.names = FALSE)

# Of the NPEU-eligible 18-30, how many actually made it through to scf_tax_units?
# Cross-check: NPEU emitted in 18-30 from (3)
npeu_18_30 = npeu_rows %>% filter(age1 >= 18, age1 <= 30) %>%
  summarise(pop_M = round(sum(weight) / 1e6, 2)) %>% pull(pop_M)

cat(sprintf('\nNPEU-eligible 18-30 (any kid_rel): %.2fM\n',
            sum((young$lw %in% 1:4) & (young$fd %in% 2:5) * young$weight) / 1e6))
cat(sprintf('NPEU emitted 18-30                : %.2fM\n', npeu_18_30))


#--- (5) Filing-status distribution vs IRS SOI 2022 ---------------------

cat('\n--- (5) Filing-status distribution: SCF tax units vs IRS SOI 2022 ---\n')
cat('Note: SCF includes filers + nonfilers; SOI = filers only.\n\n')

filestat_dist = scf_tu %>%
  mutate(fs = case_when(
    filestat == 1L ~ 'Single',
    filestat == 2L ~ 'MFJ',
    filestat == 3L ~ 'MFS',
    filestat == 4L ~ 'HoH',
    TRUE           ~ as.character(filestat)
  )) %>%
  group_by(fs) %>%
  summarise(scf_pop_M = round(sum(weight) / 1e6, 2), .groups = 'drop')

# IRS SOI 2022 filer totals (Pub 1304, Table 1.2)
soi_2022 = tibble(
  fs = c('Single','MFJ','MFS','HoH','Widow(er)'),
  soi_M = c(85.43, 56.41, 2.66, 22.43, 0.07)
)
soi_2022 = soi_2022 %>%
  mutate(fs = if_else(fs == 'Widow(er)', 'MFJ', fs)) %>%
  group_by(fs) %>% summarise(soi_M = sum(soi_M), .groups = 'drop')

filestat_cmp = filestat_dist %>%
  full_join(soi_2022, by = 'fs') %>%
  mutate(across(c(scf_pop_M, soi_M), ~ round(.x, 2)),
         scf_minus_soi = round(scf_pop_M - soi_M, 2))
print(as.data.frame(filestat_cmp), row.names = FALSE)

cat(sprintf('\nSCF total tax units : %.1fM\n',
            sum(scf_tu$weight) / 1e6))
cat(sprintf('SOI 2022 returns    : %.1fM (filers only)\n',
            sum(soi_2022$soi_M)))


cat('\nDone.\n')
