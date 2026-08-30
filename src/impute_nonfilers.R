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
# ASEC-Nonfilers interface in this schema. So the script is a read, a set of
# assertions, and a bind.
#
# Records carry EXPECTED weights -- weight x P(does not file) under the
# calibrated filing model -- rather than being drawn. Deterministic, and there
# is no RNG anywhere in the builder, which removes a class of silent
# re-randomisation this file used to introduce.
#
# What changes downstream, and is worth knowing before reading diffs:
#   * ages are real, so the runif() draws and their fixed bucket boundaries go
#   * dependents are real, so dep_age_group1-3 and the credit-qualifying counts
#     are no longer sampled from a hard-coded age distribution
#   * GENDER is OBSERVED (S14), which is what lets demographics.R stop
#     targeting the DINA sex split on the filer = 0 cells
#   * interest, dividends and capital gains are present. DINA carried exactly
#     0.0% receipt on the first two, so any aggregate that sums over records
#     with a non-zero value CHANGES -- see the note on project_puf.R's
#     extensive factor.
#------------------------------------------------------------------------------

# The published pool, in PUF schema already
nonfilers_2017 = interface_paths$`ASEC-Nonfilers` %>%
  file.path('nonfiler_pool_2017.csv.gz') %>%
  read_csv(show_col_types = F)

#-------------------------------------------------------------
# Assertions. This file is produced by another repo on another
# schedule, so its contract is checked here rather than assumed.
#-------------------------------------------------------------

# 1. It is a non-filer file, and every record says so.
stopifnot(all(nonfilers_2017$filer == 0),
          all(nonfilers_2017$dep_status == 0))

# 2. Ids are disjoint from the PUF's. `run.R` binds random numbers
#    POSITIONALLY, so a collision here is silent and catastrophic.
stopifnot(min(nonfilers_2017$id) >= 1e6,
          !any(nonfilers_2017$id %in% puf_2017$id),
          !any(duplicated(nonfilers_2017$id)))

# 3. Weights are positive and finite. Expected weights are fractional by
#    construction, so do NOT test for integers.
stopifnot(all(is.finite(nonfilers_2017$weight)),
          all(nonfilers_2017$weight > 0))

# 4. The demographics that used to be imputed are actually populated. If the
#    upstream file ever regresses to NA here, the old behaviour would return
#    silently as "zero-filled".
stopifnot(!any(is.na(nonfilers_2017$age1)),
          !any(is.na(nonfilers_2017$male1)),
          all(nonfilers_2017$age1 >= 0))

# 5. Schema. Anything the PUF has and the pool lacks is zero-filled, exactly as
#    the DINA append did -- but the set of such columns is REPORTED, because a
#    column silently appearing in that list is how a real variable gets zeroed.
missing_vars = setdiff(colnames(puf_2017), colnames(nonfilers_2017))
extra_vars   = setdiff(colnames(nonfilers_2017), colnames(puf_2017))
if (length(extra_vars) > 0) {
  stop('ASEC-Nonfilers carries columns the PUF does not: ',
       paste(extra_vars, collapse = ', '))
}
cat(sprintf(paste('impute_nonfilers.R: %s records, %.2fM weighted units,',
                  '%d columns zero-filled\n'),
            format(nrow(nonfilers_2017), big.mark = ','),
            sum(nonfilers_2017$weight) / 1e6, length(missing_vars)))

nonfilers_2017 %<>%
  bind_cols(
    rep(0, length(missing_vars)) %>%
      set_names(missing_vars) %>%
      map_df(.f = ~ 0)
  ) %>%
  select(all_of(colnames(puf_2017)))

# Add to PUF
ids_before = puf_2017$id
tax_units = puf_2017 %>%
  bind_rows(nonfilers_2017)

# D4: the append must not disturb the filer records or their order.
stopifnot(identical(tax_units$id[seq_along(ids_before)], ids_before),
          nrow(tax_units) == nrow(puf_2017) + nrow(nonfilers_2017))
