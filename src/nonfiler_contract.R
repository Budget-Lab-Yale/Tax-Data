#------------------------------------------------------------------------------
# nonfiler_contract.R
#
# The ASEC-Nonfilers producer/consumer contract, in one function.
#
# The pool is built in another repository (Tax-Simulator branch state-tax,
# research/state_weights/nonfiler_pool) on another schedule, so its shape is
# CHECKED here rather than assumed. This lives in a function so that
# impute_nonfilers.R and src/tests/test_nonfiler_contract.R exercise the same
# code -- a test that restated these rules would drift from them, which is the
# failure it exists to prevent.
#
# Returns the pool conformed to `puf_cols`: allowed extras dropped, missing
# columns zero-filled, column order matched. Stops on any real violation.
#------------------------------------------------------------------------------

# Impute-module columns. The pool carries these as OBSERVED values, but
# imputations/ages.R and imputations/demographics.R own them, run after the
# append, and overwrite every record -- so they are dropped here. The loss is
# real and worth naming: for non-filers `age1` is redrawn within its observed
# band rather than taken as observed. Band membership itself survives, via
# `age_group`, whose cut points match ages.R (<26/<35/<45/<55/<65/65+).
NONFILER_DROPPED_IMPUTED_VARS = c('age1', 'age2', 'male1', 'male2')

# Provenance describes how the record was constructed, not its tax situation.
# Dropped rather than rejected; the per-file facts (vintage, scenario, years,
# row counts) travel in the published manifest.csv.
NONFILER_PROVENANCE_VARS = c('source', 'tax_year')

# The economic variables the pool exists to supply. DINA carried exact zeros for
# interest and dividends, so any of these arriving zero-filled -- which is what
# a producer-side rename causes -- would look just like the file being replaced
# and would pass every other check here.
NONFILER_POPULATED_ECONOMIC_VARS = c('wages', 'txbl_int', 'div_pref', 'gross_ss')


validate_nonfiler_pool = function(pool, puf_cols, puf_ids, label = 'ASEC-Nonfilers') {

  # 0. Not empty. This guards every check below, all of which are VACUOUSLY
  #    true on zero rows: all(logical(0)) is TRUE and min(NULL) is Inf. A
  #    misconfigured interface used to resolve to character(0), and
  #    read_csv(character(0)) returns a 0x0 tibble -- so the append silently
  #    added nothing and reported success.
  if (nrow(pool) == 0) {
    stop(label, ': pool has zero rows. Every assertion below would pass ',
         'vacuously, so this is rejected rather than appended.')
  }

  # 1. Required columns are present before anything reads them.
  required = c('id', 'weight', 'filer', 'dep_status', 'filing_status',
               'age_group', 'GENDER')
  absent = setdiff(required, colnames(pool))
  if (length(absent) > 0) {
    stop(label, ': required columns absent: ', paste(absent, collapse = ', '))
  }

  # 2. It is a non-filer file, and every record says so.
  stopifnot(all(pool$filer == 0), all(pool$dep_status == 0))

  # 3. Ids are disjoint from the PUF's, and unique. run.R binds precomputed
  #    random numbers POSITIONALLY, so a collision here is silent and
  #    catastrophic.
  stopifnot(min(pool$id) >= 1e6,
            !any(pool$id %in% puf_ids),
            !any(duplicated(pool$id)))

  # 4. Weights are positive and finite. Expected weights are fractional by
  #    construction, so do NOT test for integers.
  stopifnot(all(is.finite(pool$weight)), all(pool$weight > 0))

  # 5. The demographics that used to be imputed are populated.
  #
  #    Assert on what this pipeline actually CONSUMES -- `age_group` and
  #    `GENDER` -- not `age1`/`male1`. At this point in main.R neither age1 nor
  #    male1 exists on the PUF: both are `source = imputed, name_puf = NA` in
  #    config/variable_guide/baseline.csv and are created later. Asserting on
  #    them here while check 6 rejects any column the PUF lacks made the two
  #    mutually unsatisfiable: no correctly-shaped pool could pass both.
  stopifnot(!any(is.na(pool$age_group)),
            all(pool$age_group %in% 1:6),
            !any(is.na(pool$GENDER)))

  # 6. Schema, in both directions.
  missing_vars = setdiff(puf_cols, colnames(pool))
  extra_vars   = setdiff(colnames(pool), puf_cols)

  allowed_extra = c(NONFILER_DROPPED_IMPUTED_VARS, NONFILER_PROVENANCE_VARS)
  unexpected    = setdiff(extra_vars, allowed_extra)
  if (length(unexpected) > 0) {
    stop(label, ' carries columns the PUF does not: ',
         paste(unexpected, collapse = ', '),
         '. If one of these is a rename of a live variable, fix the name in ',
         'the PRODUCER (Tax-Simulator research/state_weights/nonfiler_pool/',
         '05_emit_pool.R) -- adding it to the drop list here would zero-fill ',
         'the variable it was meant to populate.')
  }

  zeroed = intersect(NONFILER_POPULATED_ECONOMIC_VARS, missing_vars)
  if (length(zeroed) > 0) {
    stop(label, ' is missing economic variables it is required to populate, ',
         'so they would be silently zero-filled: ',
         paste(zeroed, collapse = ', '), '. Check for a producer-side rename.')
  }

  dropped = intersect(extra_vars, NONFILER_DROPPED_IMPUTED_VARS)
  if (length(dropped) > 0) {
    cat(label, ': dropping observed ', paste(dropped, collapse = ', '),
        ' -- the imputation modules that own these columns run later and ',
        'overwrite every record\n', sep = '')
  }

  cat(sprintf('%s: %s records, %.2fM weighted units, %d columns zero-filled\n',
              label, format(nrow(pool), big.mark = ','),
              sum(pool$weight) / 1e6, length(missing_vars)))

  # 7. Conform: zero-fill what the PUF has and the pool lacks, then take the
  #    PUF's columns in the PUF's order. This is where allowed extras go.
  if (length(missing_vars) > 0) {
    pool = pool %>%
      bind_cols(
        rep(0, length(missing_vars)) %>%
          set_names(missing_vars) %>%
          map_df(.f = ~ 0)
      )
  }
  pool = pool %>% select(all_of(puf_cols))

  stopifnot(identical(colnames(pool), puf_cols))
  pool
}
