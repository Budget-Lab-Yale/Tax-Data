#---------------------------------------------
# main.R
#
# Entry point into Tax-Data processing module.
# Four explicit phases:
#   1. Base construction  — 2017-native PUF + imputations.
#   2. Projection ledger  — project_puf.R produces factor_ledger +
#                            weight_ledger, no per-year tibble mutation.
#   3. Donor-year imputations — modules with base_year > 2017 run at
#                            their native year, producing module_deltas.
#                            (Empty until wealth migration lands.)
#   4. Materialize + write — for each year 2017..2097, materialize via
#                            (base, factor_ledger, weight_ledger,
#                            module_deltas) and write tax_units_{year}.csv.
#---------------------------------------------


#-----------------------
# Bootstrap
#-----------------------

# Load required packages
lapply(readLines('requirements.txt'), library, character.only = T)

# Read runtime configuration params and set filepaths
source('./src/configure.R')

# Set random seed
set.seed(76)


#-----------------------
# Phase 1: base construction
#-----------------------

source('./src/process_targets.R')
source('./src/process_puf.R')
source('./src/reweight.R')
source('./src/summary.R')
source('./src/create_2017_puf.R')
source('./src/nonfiler_contract.R')
source('./src/impute_nonfilers.R')
source('./src/impute_variables.R')


#-----------------------
# Phase 2: projection ledger
#-----------------------

source('./src/project_puf.R')


#-----------------------
# Phase 3: donor-year imputations
#-----------------------

# Modules with base_year > 2017 attach their outputs to module_deltas.
# Each module: (i) materializes the PUF at its base_year using the ledger
# built in Phase 2, (ii) imputes its variables against that state, (iii)
# stores the base-year values in module_deltas.
#
source('./src/materialize.R')
source('./src/record_bucket.R')
source('./src/dfa_factors.R')
module_deltas = list()

# Wealth — pooled (2019+2022) SCF donor, runs at 2022 base.
source('./src/imputations/stage1_scf_tax_units.R')
source('./src/imputations/wealth.R')
source('./src/forbes_splice.R')
source('./src/imputations/pool_scf_donors.R')

# Remove SCF billionaire-like donors before baseline wealth imputation only
# when explicit Forbes records are available to add back. The resource file in
# the repo is a schema template, so an empty input must preserve the legacy
# wealth path.
forbes_input_preview = read_forbes_input()
if (nrow(forbes_input_preview) > 0L) {
  scf_purge_result = purge_scf_billionaires(scf_tax_units, threshold = 1e9)
  scf_tax_units_wealth = scf_purge_result$scf_tax_units
} else {
  scf_purge_result = list(
    scf_tax_units = scf_tax_units,
    diagnostics = tibble(
      threshold = 1e9,
      rows_before = nrow(scf_tax_units),
      rows_dropped = 0L,
      rows_after = nrow(scf_tax_units),
      weighted_count_dropped = 0,
      net_worth_dropped = 0,
      status = 'skipped_empty_forbes_input'
    )
  )
  scf_tax_units_wealth = scf_tax_units
}

# Pooled donor set (SCF 2019 reflated to 2022$ ⊕ SCF 2022), built to de-lump
# the imputed wealth top tail (DRF is empirical-support, so a thin SCF top
# clones donors across high-weight PUF records). The pool feeds ONLY the
# donor side of run_wealth_imputation; calibration targets stay pinned to
# 2022 via target_scf = scf_tax_units_wealth (the full-weight, purged 2022
# table built above). The pool's own equal-population reweighting must NOT
# reach the targets — see pool_scf_donors.R. Billionaires are purged from
# the pool under the same Forbes-input gate as the 2022 path.
# (TODO: once validated, retire the SCF-2022-only donor path entirely.)
pooled_donors = build_pooled_scf_donors(scf_2022 = scf_tax_units)
pooled_donors_wealth = if (nrow(forbes_input_preview) > 0L) {
  purge_scf_billionaires(pooled_donors, threshold = 1e9)$scf_tax_units
} else {
  pooled_donors
}

puf_2022 = materialize(2022L, tax_units, factor_ledger, weight_ledger,
                       module_deltas)

# Snapshot the pre-wealth 2022 PUF so src/eda/wealth_harness.R can
# iterate on Stage 3 designs in ~1-2 min without rerunning Phase 1/2.
write_rds(puf_2022, file.path(output_path, 'puf_2022_snapshot.rds'))

# Freeze per-record income bucket at 2022. Used by Phase 4 together with
# bucketed_factor_ledger to age wealth Y-vars by DFA income percentile.
record_bucket = build_record_bucket(puf_2022)
write_rds(record_bucket, file.path(output_path, 'record_bucket.rds'))

# Mortality module — sourced here so build_chetty_pctile is available
# alongside the other puf_2022-derived rank artifacts.
source('./src/mortality_ledger.R')

# Freeze per-person within-(sex, age) income percentile at 2022 for the
# Chetty 2016 income gradient match in mortality_ledger.R. Within-cell
# rank (not the global rank used by DFA) — Chetty's percentiles are
# constructed within (gender × age × year), so a within-cell match is
# the only one that preserves the SSA marginal exactly. Output is
# wide: (id, pctile1, pctile2). Bespoke to mortality; not used elsewhere.
chetty_pctile = build_chetty_pctile(puf_2022)
write_rds(chetty_pctile, file.path(output_path, 'chetty_pctile.rds'))

# Bucketed growth factors for wealth Y-vars: DFA 2023..last_dfa_year, then
# per-household GDP compounding 2026+ on top of each bucket's final DFA
# cumulative. All 23 wealth Y-vars live in this ledger (none in
# factor_ledger) — the invariant in materialize() guarantees no double-
# multiplication.
bucketed_factor_ledger = build_wealth_bucketed_factors(
  weight_ledger, record_bucket, macro_projections)
write_rds(bucketed_factor_ledger,
          file.path(output_path, 'bucketed_factor_ledger.rds'))
cat(sprintf('main.R: bucketed_factor_ledger built (%d rows)\n',
            nrow(bucketed_factor_ledger)))

# Donor pool = pooled 2019+2022 (de-lumped). Target source = full-weight,
# purged 2022 (target_scf). cache_tag keeps the pooled per-cell DRF forests
# in their own cache namespace. chunk_size halves the tilt's peak donor-
# weight matrix (the pooled bottom cell has ~2x the donors); memory only,
# no effect on results.
wealth_result = run_wealth_imputation(
  puf_2022, pooled_donors_wealth,
  target_scf   = scf_tax_units_wealth,
  cache_tag    = 'pool1922',
  tilt_options = list(chunk_size = 1000L))
module_deltas[['wealth']] = list(base_year = 2022L, values = wealth_result$y)

# Forbes billionaire splice. If the Forbes input file is empty or absent, this
# returns an empty splice object and Phase 4 writes the baseline wealth output.
forbes_splice = build_forbes_splice(
  base             = tax_units,
  factor_ledger    = factor_ledger,
  weight_ledger    = weight_ledger,
  module_deltas    = module_deltas,
  bucketed_factors = bucketed_factor_ledger,
  record_bucket    = record_bucket,
  years            = 2022L:2025L
)

# Mortality ledger: per-(year, id) q_death1/q_death2. Built once over
# the full projection range; consumed by materialize() in Phase 4 like
# weight_ledger. See src/mortality_ledger.R for the layered functional
# form and the methodology resolutions (esp. the male-only marital
# adjustment, which departs from Ricco 2020 PWBM §4.2).
# (mortality_ledger.R already sourced above for build_chetty_pctile.)
t0 = Sys.time()
mortality_ledger = build_mortality_ledger(
  tax_units     = tax_units,
  chetty_pctile = chetty_pctile,
  years         = 2017L:2097L
)
write_rds(mortality_ledger, file.path(output_path, 'mortality_ledger.rds'))
cat(sprintf('main.R: mortality_ledger built (%d rows, %.1fs)\n',
            nrow(mortality_ledger),
            as.numeric(Sys.time() - t0, units = 'secs')))

# Diagnostic artifacts for downstream analysis: the pre-swap (Stage 2
# only, uniform leaf draw) donors, the QC report, and the per-(cell ×
# category) rescale factors from Step B.
write_rds(wealth_result$y_pre_tilt,
          file.path(output_path, 'wealth_pre_tilt.rds'))
write_rds(wealth_result$qc_report,
          file.path(output_path, 'stage3_qc_report.rds'))
write_rds(wealth_result$rescale_factors,
          file.path(output_path, 'rescale_factors.rds'))
write_rds(scf_purge_result$diagnostics,
          file.path(output_path, 'forbes_scf_purge_diagnostics.rds'))
write_rds(forbes_splice$rows,
          file.path(output_path, 'forbes_splice_rows.rds'))
write_rds(forbes_splice$weight_adjustments,
          file.path(output_path, 'forbes_weight_adjustments.rds'))
write_rds(forbes_splice$constraints,
          file.path(output_path, 'forbes_splice_constraints.rds'))
write_rds(forbes_splice$diagnostics,
          file.path(output_path, 'forbes_splice_diagnostics.rds'))

rm(puf_2022, wealth_result, scf_purge_result, scf_tax_units_wealth,
   forbes_input_preview, pooled_donors, pooled_donors_wealth)


#-----------------------
# Phase 4: materialize + write
#-----------------------

source('./src/write_outputs.R')
