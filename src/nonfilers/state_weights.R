# =============================================================================
# state_weights.R  --  loader for the split state-weights engine
#
# Split state weights make the national PUF-based microdata state-representative:
# each record's national weight is divided across 53 jurisdictions (50 states +
# DC, plus PR and OA as no-tax buckets) under the per-record constraint
# sum_state W[i, state] = weight_i, so every national total is preserved.
#
# Records are PARTITIONED by the exogenous `filer` flag:
#   filers     -> IRS SOI Historic Table 2 (state x AGI stub), share-normalised
#   non-filers -> ACS state x age x income population margins
#
# Until 2026-09-23 this was one 1,714-line file. F4 split it by job into five
# modules, moved verbatim; this file now only sources them, so every script that
# sources state_weights.R gets exactly the functions it did before. Source a
# single module directly when you need only part -- sw_common.R carries no
# dependency on the fit engine.
#
#   sw_common.R    jurisdictions, FIPS map, path roots, age bands, income tiers,
#                  group-quarters classes
#   sw_soi.R       HT2, Pub 1304, the IRA age split, the filing threshold
#   sw_acs.R       ACS extract and population margins; ACS-vs-IRS comparisons
#   sw_external.R  SSA, QWI and LODES readers
#   sw_fit.R       target assembly, both engines, build_split_weights
#
# build_state_weights(), the runtime dispatcher, was deleted here: it was a
# byte-identical copy of the one in Tax-Simulator, which owns it
# (research/state_weights/plan.md section 5a, finding 3).
#
# Method: research/state_weights/state_weights_phase1_summary.md
# =============================================================================

# local = TRUE: define everything in whatever environment this loader is itself
# evaluated in, exactly as the single file did. A plain source() would always
# write to the global environment, so a caller sourcing this file locally would
# silently get nothing.
for (.sw_module in c('sw_common', 'sw_soi', 'sw_acs', 'sw_external', 'sw_fit')) {
  source(file.path('src/nonfilers', paste0(.sw_module, '.R')), local = TRUE)
}
rm(.sw_module)
