#------------------------------------------------------------------------------
# impute_nonfilers.R
#
# Appends the constructed non-filing population to the 2017 PUF.
#
# Was: read DINA microdata, aggregate to tax units, and IMPUTE the demographics
# it does not carry -- ages drawn with runif() inside three coarse buckets,
# dependent ages sampled from hard-coded probabilities, sex left NA.
#
# Now: read a file that already carries those as observed columns. The
# population is built in Tax-Simulator (branch state-tax,
# research/state_weights/nonfiler_pool) from the CPS ASEC, with the
# group-quarters share backfilled from the ACS, and published to the
# ASEC-Nonfilers interface in this schema. So the script is a read, a
# validation, and a bind. The contract itself lives in src/nonfiler_contract.R
# so that the test suite exercises the same code this does.
#
# Records carry EXPECTED weights -- weight x P(does not file) under the
# calibrated filing model -- rather than being drawn. Deterministic, and there
# is no RNG anywhere in the builder, which removes a class of silent
# re-randomisation this file used to introduce.
#
# What changes downstream, and is worth knowing before reading diffs:
#   * ages are real to the BAND: age_group is observed, and ages.R then draws
#     within band. The pool's exact age1 is dropped, because ages.R runs later
#     and overwrites it -- see src/nonfiler_contract.R.
#   * dependents are real, so dep_age_group1-3 and the credit-qualifying counts
#     are no longer sampled from a hard-coded age distribution
#   * GENDER is OBSERVED (S14), which is what lets demographics.R stop
#     targeting the DINA sex split on the filer = 0 cells
#   * interest, dividends and capital gains are present. DINA carried exactly
#     0.0% receipt on the first two, so any aggregate that sums over records
#     with a non-zero value CHANGES -- see the note on project_puf.R's
#     extensive factor.
#------------------------------------------------------------------------------

# The published pool.
#
# The file is checked to exist before it is read. read_csv() on a zero-length
# path returns a 0x0 tibble rather than erroring, and every contract assertion
# would then pass VACUOUSLY, so a misconfigured interface appended nothing and
# reported success. configure.R now rejects an unresolved interface; this
# rejects a resolved interface whose file is absent.
nonfiler_file = interface_paths$`ASEC-Nonfilers` %>%
  file.path('nonfiler_pool_2017.csv.gz')
if (!file.exists(nonfiler_file)) {
  stop('ASEC-Nonfilers pool not found: ', nonfiler_file)
}

nonfilers_2017 = read_csv(nonfiler_file, show_col_types = F) %>%
  validate_nonfiler_pool(puf_cols = colnames(puf_2017),
                         puf_ids  = puf_2017$id,
                         label    = 'impute_nonfilers.R')

# Add to PUF
ids_before = puf_2017$id
tax_units = puf_2017 %>%
  bind_rows(nonfilers_2017)

# D4: the append must not disturb the filer records or their order.
stopifnot(identical(tax_units$id[seq_along(ids_before)], ids_before),
          nrow(tax_units) == nrow(puf_2017) + nrow(nonfilers_2017))
