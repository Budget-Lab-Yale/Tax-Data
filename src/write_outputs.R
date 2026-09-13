#---------------------------------------------------
# write_outputs.R  (Phase 4: materialize + write)
#
# Materializes each year 2017..2097 from
# (base_puf=tax_units, factor_ledger, weight_ledger,
# module_deltas) and writes tax_units_{year}.csv.
# Also writes the variable_guide as supplemental
# output. Module_deltas is empty when no donor-year
# imputations have run yet; Phase 3 will populate it.
#---------------------------------------------------

source('src/materialize.R')
source('src/forbes_splice.R')

# Default: empty module_deltas when Phase 3 has not produced any.
if (!exists('module_deltas')) {
  module_deltas = list()
}

# Bucketed-factor path: non-NULL iff Phase 3 built both objects.
# build_wealth_bucketed_factors + build_record_bucket are responsible for
# producing them. When absent (legacy call sites), materialize() falls
# through to the uniform-ledger-only path.
bucketed_factors = if (exists('bucketed_factor_ledger')) bucketed_factor_ledger else NULL
rb               = if (exists('record_bucket'))         record_bucket         else NULL
ml               = if (exists('mortality_ledger'))      mortality_ledger      else NULL
fs               = if (exists('forbes_splice'))         forbes_splice         else NULL

# Columns to emit per year: variable_guide vars minus vars_to_ignore. The
# 2018-19 legacy loop didn't apply this select (wrote all helper cols too);
# the 2020+ legacy loop did. We apply it consistently now.
out_cols = variable_guide$variable[!(variable_guide$variable %in% vars_to_ignore)]

# Design C emit rule. Default FALSE = design A, every record in every year.
emit_live_records_only = as.logical(as.integer(
  Sys.getenv('TAXDATA_EMIT_LIVE_ONLY', unset = '0')))
stopifnot(!is.na(emit_live_records_only))

emit_manifest = list()

cat(sprintf('Phase 4: materializing and writing per-year CSVs (emit %s)...\n',
            if (emit_live_records_only) 'live records only' else 'all records'))
t0 = Sys.time()
for (y in 2017L:2097L) {
  out = materialize(y, tax_units, factor_ledger, weight_ledger,
                    module_deltas,
                    bucketed_factors = bucketed_factors,
                    record_bucket    = rb,
                    mortality_ledger = ml)
  out = apply_forbes_splice_to_materialized(
    out, y, fs,
    factor_ledger    = factor_ledger,
    bucketed_factors = bucketed_factors)

  # Design C: emit only the records that are live in this year. The union
  # base carries every pool year, so a record from the 2019 pool sits in the
  # 2022 file with weight 0 -- about 73% of every file. Every aggregate the
  # consumer builds is weighted (`sum(. * weight)`, `weighted.mean(., weight)`
  # in Tax-Simulator's summary_stats.R), so dropping them is arithmetically
  # neutral; it is the row count, the file size and the consumer's per-year
  # work that fall. No PUF filer is ever zero-weight -- verified across
  # 2017..2097 on vintage 2026091119 -- so this only ever drops pool records.
  #
  # Off by default: this changes the record set Tax-Simulator sees year to
  # year, which is the fixed-id contract, so it is a decision, not a tidy-up.
  # See research/state_weights/nonfiler_design_c_scope.md.
  n_all = nrow(out)
  if (emit_live_records_only) {
    out = out[out$weight > 0, , drop = FALSE]
  }
  emit_manifest[[length(emit_manifest) + 1L]] = tibble(
    year        = y,
    rows_all    = n_all,
    rows_live   = nrow(out),
    n_filers    = sum(out$filer == 1),
    n_nonfilers = sum(out$filer == 0)
  )

  out = out[, intersect(out_cols, names(out)), drop = FALSE]
  write_csv(out, file.path(output_path, paste0('tax_units_', y, '.csv')))
}
cat(sprintf('Phase 4: wrote 81 per-year CSVs (%.1fs)\n',
            as.numeric(Sys.time() - t0, units = 'secs')))

# Supplemental: variable guide as written alongside output.
write_csv(variable_guide, file.path(output_path, 'variable_guide.csv'))

# What was actually emitted, per year, so the consumer asserts rather than
# infers. Records the emit rule in force.
emit_manifest = bind_rows(emit_manifest) %>%
  mutate(emit_rule = if (emit_live_records_only) 'live_only' else 'all')
write_csv(emit_manifest, file.path(output_path, 'emit_manifest.csv'))
