#------------------------------------------------------------------------------
# impute_nonfilers.R
#
# Appends the constructed non-filing population to the 2017 PUF -- every
# published year of it (Block E, design A: the union base).
#
# Was (through 2026-09-11): read the 2017 pool only and let project_puf.R
# rescale those records for every later year, which froze composition at
# 2017 and put the annual rebuild S18(c) asked for out of reach.
#
# Now: every pool year the pinned ASEC-Nonfilers vintage publishes (2017-2023)
# is appended, each record tagged with the year it was built for (`base_year`)
# and its own weight in that year (`weight_own`). Two consequences:
#
#   * The record set is the SAME in every output year. Tax-Simulator samples
#     ids from the 2017 file and binds precomputed random numbers to records
#     by ROW POSITION (run.R:349-357), so the union is what lets composition
#     change annually without re-randomising anything downstream. A record
#     carries positive weight only in the year(s) it represents:
#     weight_ledger gives the 2017 pool 2017, the 2018 pool 2018, ..., and
#     the 2023 pool 2023 onward (grown on the S18(b) band series). Every
#     other year it is a zero-weight row.
#   * Pools reuse ids (each numbers from 1000001), so the year-y pool is moved
#     into its own id block: id + (y - 2017) * 1e6. Filers stay below 1e6.
#
# `weight` on the appended rows is the 2017 weight: the 2017 pool's own, zero
# for every later pool. Phase 1 imputations therefore see a 2017-weighted
# population -- percentile cut points, calibration targets and the PCE
# benchmark are unaffected by the later pools -- while every record still
# receives a value (cut points are weighted, records are ranked). Known
# second-order limitation, recorded rather than fixed: later pools carry their
# own year's nominal dollars, so a 2023 record ranks against 2017 cut points
# with 2023 wages. The deflate-at-append fix needs the income factors before
# the append; see research/state_weights/plan.md.
#
# Values are aged by materialize() as factor(y) / factor(base_year), so a
# record materialized at its own year reproduces the published file exactly.
#
# Records carry EXPECTED weights -- weight x P(does not file) under the
# calibrated filing model -- rather than being drawn. Deterministic, and
# there is no RNG anywhere in the builder.
#
# What the pool supplies and what the imputation modules overwrite is
# documented in src/nonfiler_contract.R (the schema contract this script and
# src/tests/test_nonfiler_contract.R share).
#------------------------------------------------------------------------------

pool_dir = interface_paths$`ASEC-Nonfilers`

# The manifest says what was published. Years come from it, not from a
# hard-coded list, so a vintage that adds a year is consumed without an edit
# here -- and one that drops a year fails below rather than silently.
manifest_file = file.path(pool_dir, 'manifest.csv')
if (!file.exists(manifest_file)) {
  stop('ASEC-Nonfilers manifest not found: ', manifest_file)
}
nonfiler_manifest = read_csv(manifest_file, show_col_types = F)
NONFILER_POOL_YEARS     = sort(unique(as.integer(nonfiler_manifest$tax_year)))
NONFILER_LAST_POOL_YEAR = max(NONFILER_POOL_YEARS)
stopifnot(min(NONFILER_POOL_YEARS) == NONFILER_BASE_YEAR,
          identical(NONFILER_POOL_YEARS,
                    seq(NONFILER_BASE_YEAR, NONFILER_LAST_POOL_YEAR)))

# Helper columns the union needs and the output never carries (not in the
# variable guide, so write_outputs.R drops them). Filers are 2017-native.
nonfiler_helper_cols = c('base_year', 'weight_own')
puf_2017 = puf_2017 %>%
  mutate(base_year  = NONFILER_BASE_YEAR,
         weight_own = weight)
puf_cols_contract = setdiff(colnames(puf_2017), nonfiler_helper_cols)

ids_so_far = puf_2017$id
pools = list()
for (y in NONFILER_POOL_YEARS) {
  f = file.path(pool_dir, sprintf('nonfiler_pool_%d.csv.gz', y))
  if (!file.exists(f)) {
    stop('ASEC-Nonfilers pool listed in the manifest but not found: ', f)
  }
  raw = read_csv(f, show_col_types = F)
  if (!('tax_year' %in% colnames(raw)) || !all(raw$tax_year == y)) {
    stop('nonfiler_pool_', y, '.csv.gz does not declare tax_year == ', y)
  }

  pool = raw %>%
    offset_nonfiler_ids(tax_year = y) %>%
    validate_nonfiler_pool(puf_cols = puf_cols_contract,
                           puf_ids  = ids_so_far,
                           label    = sprintf('impute_nonfilers.R [%d]', y)) %>%
    mutate(base_year  = as.integer(y),
           weight_own = weight,
           # 2017 weight: the 2017 pool's own; a later pool has none in 2017
           weight     = if_else(base_year == NONFILER_BASE_YEAR, weight, 0))

  # Published adults must be what arrived: the manifest is the producer's
  # statement of the file, and a mismatch means a truncated or stale copy.
  adults_here = sum(pool$weight_own * (1 + (pool$filing_status == 2)))
  adults_pub  = nonfiler_manifest$adults[nonfiler_manifest$tax_year == y]
  stopifnot(length(adults_pub) == 1,
            abs(adults_here / adults_pub - 1) < 1e-6)

  ids_so_far = c(ids_so_far, pool$id)
  pools[[as.character(y)]] = pool
}
nonfilers = bind_rows(pools)

# Add to PUF
tax_units = puf_2017 %>%
  bind_rows(nonfilers)

# D4: the append must not disturb the filer records or their order, and the
# union must be one record set with unique ids -- the property Tax-Simulator's
# positional random-number binding depends on.
stopifnot(identical(tax_units$id[seq_len(nrow(puf_2017))], puf_2017$id),
          !any(duplicated(tax_units$id)),
          nrow(tax_units) == nrow(puf_2017) + nrow(nonfilers),
          all(tax_units$base_year[tax_units$filer == 1] == NONFILER_BASE_YEAR))

cat(sprintf(paste0('impute_nonfilers.R: %d pool years appended (%d-%d), ',
                   '%s non-filer records; weight in the base year is the %d ',
                   'pool\'s (%.2fM units), later pools carry zero until their ',
                   'own year\n'),
            length(NONFILER_POOL_YEARS), NONFILER_BASE_YEAR,
            NONFILER_LAST_POOL_YEAR, format(nrow(nonfilers), big.mark = ','),
            NONFILER_BASE_YEAR,
            sum(nonfilers$weight) / 1e6))
print(nonfilers %>%
        group_by(base_year) %>%
        summarise(records = n(),
                  units_M  = round(sum(weight_own) / 1e6, 3),
                  adults_M = round(sum(weight_own * (1 + (filing_status == 2))) / 1e6, 3),
                  .groups = 'drop'))
rm(pools, raw, pool, ids_so_far, adults_here, adults_pub)
