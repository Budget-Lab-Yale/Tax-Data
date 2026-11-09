#-------------------------------------------------------------------------------
# cbo_tax_units(x, yr, roster_path) puts the tax units that tax_units.py built
# into the form that production's ASEC builder (src/nonfilers/asec_tax_units.R)
# returns. Production's filing requirement and build_cps.R then read them as
# they are. x is the cleaned ASEC person table, without people in group
# quarters. roster_path is the roster that tax_units.save_cps wrote: each
# person's unit and role. Returns the persons and a unit table. The unit table
# comes from production's build_asec_unit_table(), with the filing status set
# by CBO's rule: joint with a spouse, head of household with a dependent, and
# single otherwise.
#-------------------------------------------------------------------------------
cbo_tax_units = function(x, yr, roster_path) {
  # 1. Attach each person's unit and role. Every person must have one.
  ro = fread(roster_path)
  stopifnot(all(ro$year == yr), !anyNA(ro))
  ro = ro[, .(SERIAL = hh, PERNUM = pn, unit_head = as.integer(tu_id), tu_rel, dep_type, unit_fs)]
  p = merge(copy(x), ro, by = c('SERIAL', 'PERNUM'), all.x = TRUE)
  miss = p[is.na(unit_head)]
  if (nrow(miss)) stop(sprintf('%d people in the ASEC frame have no unit in %s', nrow(miss), roster_path))
  stopifnot(nrow(ro) == nrow(p))
  # 2. The roles in production's terms. A unit's id is the household serial x 100 plus its head's person number.
  p[, role := c('primary', 'spouse', 'dependent', 'dependent')[tu_rel]]
  p[, is_qc := tu_rel == 3L]
  p[, unit_id := as.numeric(SERIAL) * 100 + unit_head]
  p[, is_dependent := role == 'dependent']
  chk = p[role == 'primary', .N, by = unit_id]
  stopifnot(all(chk$N == 1), uniqueN(p$unit_id) == nrow(chk))
  # 3. Build production's unit table and give each unit CBO's filing status.
  units = build_asec_unit_table(p, yr)
  fs = unique(p[, .(unit_id, unit_fs)])
  stopifnot(!anyDuplicated(fs$unit_id))
  units[fs, on = 'unit_id', cbo_fs := i.unit_fs]
  stopifnot(!anyNA(units[unit_type == 'nondependent', cbo_fs]))
  # The builder's rule and CBO's differ only for unmarried heads whose dependents are all qualifying relatives.
  cat(sprintf('  filing status: builder rule and CBO rule differ on %d units (%.2f m weighted)\n',
              units[unit_type == 'nondependent' & filing_status != cbo_fs, .N],
              units[unit_type == 'nondependent' & filing_status != cbo_fs, sum(weight)] / 1e6))
  units[unit_type == 'nondependent', filing_status := cbo_fs]
  units[, cbo_fs := NULL]
  list(persons = p[], units = units)
}
