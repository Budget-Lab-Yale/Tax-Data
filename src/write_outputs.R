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

# Default: empty module_deltas when Phase 3 has not produced any.
if (!exists('module_deltas')) {
  module_deltas = list()
}

# Columns to emit per year: variable_guide vars minus vars_to_ignore. The
# 2018-19 legacy loop didn't apply this select (wrote all helper cols too);
# the 2020+ legacy loop did. We apply it consistently now.
out_cols = variable_guide$variable[!(variable_guide$variable %in% vars_to_ignore)]

cat('Phase 4: materializing and writing per-year CSVs...\n')
t0 = Sys.time()
for (y in 2017L:2097L) {
  out = materialize(y, tax_units, factor_ledger, weight_ledger, module_deltas)
  out = out[, intersect(out_cols, names(out)), drop = FALSE]
  write_csv(out, file.path(output_path, paste0('tax_units_', y, '.csv')))
}
cat(sprintf('Phase 4: wrote 81 per-year CSVs (%.1fs)\n',
            as.numeric(Sys.time() - t0, units = 'secs')))

# Supplemental: variable guide as written alongside output.
write_csv(variable_guide, file.path(output_path, 'variable_guide.csv'))
