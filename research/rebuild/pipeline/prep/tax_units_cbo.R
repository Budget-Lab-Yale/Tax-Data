#-------------------------------------------------------------------------------
# cbo_tax_units(x, yr, roster_path): the tax units tax_units.py built, in the form
# production's ASEC builder returns, so that production's filing requirement and
# build_cps.R read them unchanged. x is the cleaned ASEC person table without
# group quarters; the unit table comes from build_asec_unit_table(), with CBO's
# filing status (joint with a spouse, head of household with a dependent, else
# single).
#-------------------------------------------------------------------------------
cbo_tax_units = function(x, yr, roster_path) {
  ro = fread(roster_path)
  stopifnot(all(ro$year == yr), !anyNA(ro))
  ro = ro[, .(SERIAL = hh, PERNUM = pn, unit_head = as.integer(tu_id), tu_rel, dep_type, unit_fs)]
  p = merge(copy(x), ro, by = c('SERIAL', 'PERNUM'), all.x = TRUE)
  miss = p[is.na(unit_head)]
  if (nrow(miss)) stop(sprintf('%d people in the ASEC frame have no unit in %s', nrow(miss), roster_path))
  stopifnot(nrow(ro) == nrow(p))
  p[, role := c('primary', 'spouse', 'dependent', 'dependent')[tu_rel]]
  p[, is_qc := tu_rel == 3L]
  p[, unit_id := as.numeric(SERIAL) * 100 + unit_head]
  p[, is_dependent := role == 'dependent']
  chk = p[role == 'primary', .N, by = unit_id]
  stopifnot(all(chk$N == 1), uniqueN(p$unit_id) == nrow(chk))
  units = build_asec_unit_table(p, yr)
  fs = unique(p[, .(unit_id, unit_fs)])
  stopifnot(!anyDuplicated(fs$unit_id))
  units[fs, on = 'unit_id', cbo_fs := i.unit_fs]
  stopifnot(!anyNA(units[unit_type == 'nondependent', cbo_fs]))
  # the builder's rule and CBO's differ only for unmarried heads whose dependents are all qualifying relatives
  cat(sprintf('  filing status: builder rule and CBO rule differ on %d units (%.2f m weighted)\n',
              units[unit_type == 'nondependent' & filing_status != cbo_fs, .N],
              units[unit_type == 'nondependent' & filing_status != cbo_fs, sum(weight)] / 1e6))
  units[unit_type == 'nondependent', filing_status := cbo_fs]
  units[, cbo_fs := NULL]
  list(persons = p[], units = units)
}
