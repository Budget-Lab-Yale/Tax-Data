#-------------------------------------------------------------------------------
# tax_units_cbo.R: the CBO-rule tax units (pipeline/tax_units.py) in the form production's builder returns, so the
# filing model (src/nonfilers/filing_model.R) and the pool's calibration score them unchanged.
#
# cbo_tax_units(x, yr, roster_path) takes the cleaned ASEC person table x (as build_cps.R prepares it, group
# quarters already left out) and the roster tax_units.py wrote (roster_<year>_<variant>_nogq.csv.gz: one row per
# person, with the head's person number tu_id, the role tu_rel 1 head, 2 spouse, 3 qualifying child, 4 qualifying
# relative, the dependent's type and the unit's filing status). It returns list(persons, units) with the columns of
# build_asec_tax_units(): the unit table is made by production's own build_asec_unit_table() (head and spouse
# income summed; a scoring row for every dependent aged 15 and over), and the filing status is then set to CBO's:
# joint with a spouse, head of household with any dependent, single otherwise.
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
