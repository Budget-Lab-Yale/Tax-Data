#---------------------------------------------------------------------
# dina_ageprim_probe.R
#
# impute_nonfilers.R routes DINA non-filers into PUF age_groups via:
#
#   age_group = case_when(
#     ageprim == 20 ~ floor(runif(., 1, 4)),  # bands 1-3
#     ageprim == 45 ~ floor(runif(., 4, 6)),  # bands 4-5
#     T             ~ 6                        # all else → band 6 (65+)
#   )
#
# We found 10.86M non-filers in PUF band 6 (41.5% of all non-filers).
# Question: how much of that is genuinely 65+ vs miscategorized 50-64?
# That depends on the actual values DINA's `ageprim` takes among
# non-filers and what those values mean.
#---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(haven); library(tibble)
})

DINA = '/nfs/roberts/project/pi_nrs36/shared/raw_data/DINA/v1/2023082913/historical/usdina2017.dta'

cat('Loading DINA 2017 ...\n')
dina = read_dta(DINA)
cat(sprintf('  %d DINA rows\n', nrow(dina)))

# Sanity: filer flag distribution
cat('\n--- DINA filer flag (record count, weighted) ---\n')
print(dina %>%
        group_by(filer) %>%
        summarise(n = n(),
                  weight_M = sum(dweghttaxu) / 1e5 / 1e6) %>%
        ungroup())

# ageprim raw distribution among non-filers (per-record, since
# impute_nonfilers groups by id and takes max(ageprim); we look at
# records first to see what raw values exist)
nf_raw = dina %>% filter(filer == 0)
cat(sprintf('\nDINA non-filer rows: %d\n', nrow(nf_raw)))

cat('\n--- Raw ageprim values, weighted (DINA non-filer rows) ---\n')
agetab_raw = nf_raw %>%
  group_by(ageprim) %>%
  summarise(records = n(),
            weight_M = sum(dweghttaxu) / 1e5 / 1e6,
            .groups = 'drop') %>%
  arrange(ageprim)
print(agetab_raw, n = Inf)

# Now apply the impute_nonfilers pre-aggregation: group by id, max(ageprim).
# This is what actually gets routed into PUF.
nf_units = dina %>%
  filter(filer == 0) %>%
  group_by(id) %>%
  summarise(weight  = mean(dweghttaxu) / 1e5,
            ageprim = max(ageprim),
            married = mean(married),
            xkidspop = mean(xkidspop),
            .groups = 'drop')

cat(sprintf('\nDINA non-filer tax units (after id agg): %d records, %.2fM weighted\n',
            nrow(nf_units), sum(nf_units$weight) / 1e6))

cat('\n--- Per-tax-unit ageprim distribution (post id agg) ---\n')
agetab_units = nf_units %>%
  group_by(ageprim) %>%
  summarise(records = n(),
            weight_M = sum(weight) / 1e6,
            .groups = 'drop') %>%
  arrange(ageprim)
print(agetab_units, n = Inf)

# Simulate the imputation rule and report what fraction of non-filer
# weight would land in each PUF band.
nf_units = nf_units %>%
  mutate(
    routed_band = case_when(
      ageprim == 20 ~ NA_integer_,    # → bands 1-3 uniformly (avg 2)
      ageprim == 45 ~ NA_integer_,    # → bands 4-5 uniformly (avg 4.5)
      TRUE          ~ 6L
    ),
    routed_label = case_when(
      ageprim == 20 ~ '1-3 (uniform)',
      ageprim == 45 ~ '4-5 (uniform)',
      TRUE          ~ '6 (forced)'
    )
  )

cat('\n--- Routing of DINA non-filers into PUF age_group ---\n')
print(nf_units %>%
        group_by(routed_label, ageprim) %>%
        summarise(weight_M = sum(weight) / 1e6, .groups = 'drop') %>%
        arrange(routed_label, ageprim),
      n = Inf)

# Summary: what fraction of weight ends up in band 6, and what ageprim
# values does that correspond to?
band6_weight = nf_units %>%
  filter(routed_label == '6 (forced)') %>%
  group_by(ageprim) %>%
  summarise(weight_M = sum(weight) / 1e6, .groups = 'drop') %>%
  arrange(ageprim)

cat('\n--- Forced-band-6 mass by raw ageprim (i.e., the 10.86M PUF non-filer band-6) ---\n')
print(band6_weight, n = Inf)
cat(sprintf('  total forced-to-band-6: %.2fM\n',
            sum(band6_weight$weight_M)))

cat('\nDone.\n')
