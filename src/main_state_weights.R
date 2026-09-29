#---------------------------------------------------------------------------
# main_state_weights.R
#
# The State-Weights producer: fits each record's split of its national weight
# across the 53 jurisdictions (51 states with DC, plus PR and OA as no-tax
# buckets) and publishes state_weights_{year}.csv.gz as a versioned interface
# Tax-Simulator reads (research/state_weights/notes/phase4_state_weights_
# interface.md; S42).
#
# Its own entry point, not a stage of main.R, because the fit stratifies the
# filer targets by an AGI only Tax-Simulator computes (S30): it reads a
# Tax-Simulator baseline RUN ON the Tax-Data vintage being split. Both pins
# come from config/runscripts/state_weights.yaml and are checked
# (src/state_weights_contract.R): the baseline's own dependencies.csv must
# name this Tax-Data vintage, and its detail must carry every record.
#
# Per fit year (HT2 through TY2023):
#   1. records from the Tax-Data vintage, agi/eitc joined from the baseline
#   2. build_weight_inputs(): HT2 stub x series targets and prior for filers,
#      state adult anchors and the ACS prior for non-filers
#   3. filers by the runscript's arm (S39: C_10_warmB = A(2017) -> B chain ->
#      C joint 2018-2023 warm from B); non-filers by exact IPF, per year
#   4. emit_split(): long (id, state, weight), zero cells dropped, sum over
#      states = national weight to 1e-8 relative -- asserted on the emitted
#      weights, per year, before anything is written
# Every later emit year carries the last fit year's shares: W_y = w_y x P_last
# by id (D2) -- well-defined under Design C because every live 2024+ record is
# a 2023 record -- except records new to a later year (Forbes list years
# 2024+), which take the last fit year's Forbes rows' weighted-mean split and
# are counted in the manifest.
#
# Environment (all optional):
#   TAXDATA_VINTAGE                 the vintage name (default: the hour stamp)
#   TAXDATA_ALLOW_VINTAGE_OVERWRITE=1  write into a non-empty vintage
#   SW_ARM, SW_FIT_YEARS, SW_EMIT_YEARS  override the runscript for a dry run,
#       e.g. SW_ARM=B SW_FIT_YEARS=2022:2023 SW_EMIT_YEARS=2022:2025 (D4)
#   TAXDATA_SCRATCH                 where the fitted share matrices go
#---------------------------------------------------------------------------

lapply(readLines('requirements.txt'), library, character.only = T)
source('./src/resolve_interfaces.R')
source('./src/state_weights_contract.R')
source('./src/nonfilers/state_weights.R')   # the engine: build_weight_inputs(), the chains, emit_split()

#-----------------
# Configuration
#-----------------

runscript_id       = 'state_weights'
runscript          = read_yaml(file.path('./config/runscripts', paste0(runscript_id, '.yaml')))
output_roots       = read_yaml('./config/interfaces/output_roots.yaml')
interface_versions = read_yaml('./config/interfaces/interface_versions.yaml')
OWN = 'State-Weights'

parse_years = function(x) { r = as.integer(strsplit(x, ':')[[1]]); if (length(r) == 2) r[1]:r[2] else r }
arm        = Sys.getenv('SW_ARM',        unset = runscript$runtime_options$arm)
fit_years  = if (nzchar(Sys.getenv('SW_FIT_YEARS'))) parse_years(Sys.getenv('SW_FIT_YEARS')) else
               do.call(seq, as.list(runscript$runtime_options$fit_years))
emit_years = if (nzchar(Sys.getenv('SW_EMIT_YEARS'))) parse_years(Sys.getenv('SW_EMIT_YEARS')) else 2017L:2097L
stopifnot(arm %in% c('A', 'B', 'C_10_warmB'), length(fit_years) >= 1,
          all(diff(fit_years) == 1), min(emit_years) >= min(fit_years), all(diff(emit_years) == 1))
carried_years = setdiff(emit_years, fit_years)
stopifnot(all(carried_years > max(fit_years)))   # carry forward only, never back

vintage = Sys.getenv('TAXDATA_VINTAGE', unset = format(Sys.time(), '%Y%m%d%H'))
stopifnot(grepl('^[A-Za-z0-9_]+$', vintage))

resolved = resolve_interfaces(runscript, runscript_id, interface_versions, output_roots,
                              own_interface = OWN, pinned_anywhere = interfaces_pinned_anywhere())
tax_data_dir = resolved$paths$`Tax-Data`
baseline_dir = resolved$paths$`Tax-Simulator`
tax_data_vintage = runscript$dependency_info$`Tax-Data`$vintage
check_baseline_pin(baseline_dir, tax_data_vintage)

output_root = if (isTRUE(runscript$runtime_options$write_locally)) {
  file.path(output_roots$local, runscript$runtime_options$user_id)
} else output_roots$production
vintage_dir = file.path(output_root, interface_versions[[OWN]]$type, OWN,
                        paste0('v', interface_versions[[OWN]]$version), vintage)
output_path = file.path(vintage_dir, 'baseline')
if (dir.exists(output_path) && length(list.files(output_path)) > 0 &&
    Sys.getenv('TAXDATA_ALLOW_VINTAGE_OVERWRITE', unset = '0') != '1') {
  stop('output vintage already has content: ', output_path,
       '\n  Name another with TAXDATA_VINTAGE, or set TAXDATA_ALLOW_VINTAGE_OVERWRITE=1.')
}
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
write_csv(resolved$dependencies, file.path(vintage_dir, 'dependencies.csv'))

fits_dir = file.path(scratch_root(), 'state_weights_build', vintage)
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf('main_state_weights.R: vintage %s, arm %s, fit years %d-%d, emit years %d-%d\n',
            vintage, arm, min(fit_years), max(fit_years), min(emit_years), max(emit_years)))
cat(sprintf('  Tax-Data %s; Tax-Simulator baseline %s (pin checked)\n', tax_data_vintage, basename(dirname(baseline_dir))))

#-----------------------------------------
# Inputs per fit year: records + targets
#-----------------------------------------

read_records = function(year, cols = WEIGHT_INPUT_COLS) {
  fread(file.path(tax_data_dir, sprintf('tax_units_%d.csv', year)), select = cols)
}

t0 = Sys.time()
inputs = list(); records = list()
for (y in fit_years) {
  tu = read_records(y)
  stopifnot(all(tu$weight > 0))                       # Design C emits live records only
  det = validate_baseline_detail(read_baseline_detail(baseline_dir, y), tu, y)
  tu[, `:=`(agi = det$agi, eitc = det$eitc)]           # det is in tu's order
  inp = build_weight_inputs(tu, y)
  inp$filers$ids        = tu$id[inp$filers$idx]
  inp$filers$jurisdictions    = inp$jurisdictions
  inp$nonfilers$ids     = tu$id[inp$nonfilers$idx]
  inp$nonfilers$jurisdictions = inp$jurisdictions
  stopifnot(length(inp$filers$idx) + length(inp$nonfilers$idx) == nrow(tu))
  inputs[[as.character(y)]]  = inp
  records[[as.character(y)]] = tu[, .(id, weight, filer)]
  cat(sprintf('  TY%d: %d records (%d filers, %d non-filers), %d jurisdictions, %d filer targets (%.0fs)\n',
              y, nrow(tu), length(inp$filers$idx), length(inp$nonfilers$idx), length(inp$jurisdictions),
              length(inp$filers$targets), as.numeric(Sys.time() - t0, units = 'secs')))
}

#-----------------------------------------
# Fits: filers by arm, non-filers by IPF
#-----------------------------------------

as_fit = function(f, p) list(P = f$P, theta = f$theta, ids = p$ids, jurisdictions = p$jurisdictions)
filer_ps = lapply(inputs, `[[`, 'filers')
t0 = Sys.time()
filer_fits = switch(arm,
  A = lapply(filer_ps, function(p) as_fit(fit_filers_config7(p, verbose = TRUE), p)),
  B = fit_filers_chain_B(filer_ps, verbose = TRUE),
  C_10_warmB = {
    # A(first year) -> B over all fit years -> C joint over the years with a
    # common jurisdiction set (2018+: PR splits out of OA that year), warm from B
    b = fit_filers_chain_B(filer_ps, verbose = TRUE)
    c_years = which(fit_years >= 2018L)
    if (length(c_years) >= 2) {
      cc = fit_filers_chain_C(filer_ps[c_years], gamma_mult = 10, warm = b[c_years], verbose = TRUE)
      b[c_years] = cc$fits
      saveRDS(cc$loss_hist, file.path(fits_dir, 'C_10_warmB_loss_hist.rds'))
    } else message('  fewer than two years from 2018: arm C stage skipped, arm B fits kept')
    b
  })
names(filer_fits) = names(inputs)
cat(sprintf('  filer fits done (%.0fs)\n', as.numeric(Sys.time() - t0, units = 'secs')))

nonfiler_fits = lapply(inputs, function(inp) {
  p = inp$nonfilers
  f = fit_calibration(p$w, p$P0, p$targets)
  as_fit(f, p)
})
for (y in names(inputs)) {
  saveRDS(filer_fits[[y]],    file.path(fits_dir, sprintf('fit_filers_%s.rds', y)))
  saveRDS(nonfiler_fits[[y]], file.path(fits_dir, sprintf('fit_nonfilers_%s.rds', y)))
}

#-----------------------------------------
# Emit: fit years, then carried years
#-----------------------------------------

manifest = list()
write_year = function(year, split, fitted, source_year, n_carried_new = 0L) {
  setorder(split, id, state)
  f = sprintf('state_weights_%d.csv.gz', year)
  fwrite(split, file.path(output_path, f), compress = 'gzip')
  manifest[[length(manifest) + 1]] <<- data.table(
    interface = OWN, version = interface_versions[[OWN]]$version, vintage = vintage, scenario = 'baseline',
    tax_year = year, file = f, n_records = uniqueN(split$id), n_states = uniqueN(split$state),
    sum_weight = sum(split$weight), fitted = fitted, arm = arm, source_year = source_year,
    n_carried_new = n_carried_new)
  cat(sprintf('  wrote %s: %d records, %d states, %.1fM total weight%s\n', f, uniqueN(split$id),
              uniqueN(split$state), sum(split$weight) / 1e6,
              if (fitted) '' else sprintf(' (carried from TY%d, %d new records)', source_year, n_carried_new)))
}

# Every live record of the year must be in the emitted split, once
assert_covers = function(split, tu) {
  ids = unique(split$id)
  stopifnot(setequal(ids, tu$id), length(ids) == nrow(tu))
}

for (y in fit_years) {
  ys = as.character(y); inp = inputs[[ys]]; tu = records[[ys]]
  split = rbind(
    emit_split(filer_fits[[ys]]$P,    inp$filers$w,    inp$filers$ids,    inp$jurisdictions),
    emit_split(nonfiler_fits[[ys]]$P, inp$nonfilers$w, inp$nonfilers$ids, inp$jurisdictions))
  assert_covers(split, tu)
  write_year(y, split, fitted = TRUE, source_year = y)
}

if (length(carried_years)) {
  last = as.character(max(fit_years))
  P_last  = rbind(filer_fits[[last]]$P, nonfiler_fits[[last]]$P)
  ids_last = c(filer_fits[[last]]$ids, nonfiler_fits[[last]]$ids)
  J = filer_fits[[last]]$jurisdictions
  stopifnot(!anyDuplicated(ids_last), identical(nonfiler_fits[[last]]$jurisdictions, J))
  # records new to a later year (Forbes list years 2024+): the last fit year's
  # Forbes rows' weighted-mean split
  forbes_last = ids_last >= 9e6
  fallback = if (any(forbes_last)) {
    w = records[[last]]$weight[match(ids_last[forbes_last], records[[last]]$id)]
    colSums(P_last[forbes_last, , drop = FALSE] * w) / sum(w)
  } else colSums(P_last * records[[last]]$weight[match(ids_last, records[[last]]$id)]) / sum(records[[last]]$weight)
  for (y in carried_years) {
    tu = read_records(y, cols = c('id', 'weight', 'filer'))
    stopifnot(all(tu$weight > 0))
    rows = match(tu$id, ids_last)
    new  = is.na(rows)
    stopifnot(all(tu$id[new] >= 9e6),                   # only Forbes synthetics are new
              mean(new) < 0.005)
    P = matrix(NA_real_, nrow(tu), length(J))
    P[!new, ] = P_last[rows[!new], ]
    if (any(new)) P[new, ] = matrix(fallback, sum(new), length(J), byrow = TRUE)
    split = emit_split(P, tu$weight, tu$id, J)
    assert_covers(split, tu)
    write_year(y, split, fitted = FALSE, source_year = as.integer(last), n_carried_new = sum(new))
  }
}

#-----------------
# Manifest, README
#-----------------

manifest = rbindlist(manifest)
fwrite(manifest, file.path(output_path, 'manifest.csv'))
writeLines(c(
  '# State-Weights -- the per-year state split of each record\'s national weight',
  '',
  sprintf('Vintage `%s`, built %s by Tax-Data `src/main_state_weights.R` (arm `%s`).',
          vintage, format(Sys.time(), '%Y-%m-%d %H:%M'), arm),
  sprintf('Records: Tax-Data `%s`. Calculated AGI and EITC: Tax-Simulator baseline `%s`, run on that vintage (checked).',
          tax_data_vintage, basename(dirname(baseline_dir))),
  '',
  '## Files',
  '',
  '`state_weights_{year}.csv.gz`: long `id, state, weight`, `weight > 0`, sorted by id and state.',
  'For every record the weights over all jurisdictions sum to its national weight in the',
  'Tax-Data file for that year (asserted to 1e-8 relative before writing). `state` is the',
  '2-letter code with `DC`; `PR` and `OA` (Other Areas) are no-tax buckets. TY2017 has no',
  '`PR` column because HT2 2017 has no Puerto Rico row: its returns sit in `OA`.',
  '',
  sprintf('Fit years %d-%d (IRS Historic Table 2 targets for filers, netted residual state anchors for',
          min(fit_years), max(fit_years)),
  'non-filers). Later years carry the last fit year\'s shares: W_y = w_y x P_last by record id;',
  'records new to a later year (Forbes synthetics) take the last fit year\'s Forbes rows\'',
  'weighted-mean split (`n_carried_new` in the manifest).',
  '',
  '`manifest.csv`: one row per year. `dependencies.csv` (one level up): the two pins.',
  '',
  'Method of record: Tax-Data `research/state_weights/notes/phase4_state_weights_interface.md`;',
  'the fit: `research/state_weights/state_weights_phase1_summary.md` and `plan.md` Phase 3 (S39).'
), file.path(output_path, 'README.md'))

cat(sprintf('STATE_WEIGHTS_DONE %s: %d years written to %s\n', vintage, nrow(manifest), output_path))
