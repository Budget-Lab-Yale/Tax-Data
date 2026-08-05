#--------------------------------------
# pool_scf_donors.R
#
# Build a POOLED SCF donor set (2019 + 2022, both on 2022 dollars and one
# combined population) to de-lump the wealth imputation's top tail.
#
# Why: downstream estate scoring spikes because the thin SCF 2022 top
# (~104 distinct households, 25 elderly, in the $15-20M band) forces DRF —
# an empirical-support estimator — to clone a handful of donor vectors
# across many high-weight PUF records. Bin-level calibration to SCF is
# already excellent; the problem is *within-bin granularity*. Reflating and
# pooling SCF 2019 roughly doubles top support without changing the targets
# (see src/eda/scf_2019_vs_2022_top.R for the household-level diagnostic).
#
# Contract: returns a tibble that is a superset of the stage1
# scf_tax_units schema plus a `donor_wave` column. It feeds the DONOR side
# of run_wealth_imputation (bootstrap / per-cell DRF / tilt / accruals).
# The TARGET side stays pinned to 2022 by passing
#   target_scf = pooled[pooled$donor_wave == 2022, ]
# to run_wealth_imputation (see its `target_scf` arg).
#
# Depends (must be sourced first by the caller):
#   - src/imputations/wealth_schema.R   (wealth_y_vars, asset/debt/kg vars)
#   - src/imputations/stage1_scf_tax_units.R (build_scf_tax_units,
#                                             default_stage1_config)
#--------------------------------------

# 2019 SCF raw inputs. Pinned directly for the prototype (the production
# interface_paths$SCF currently points only at the 2022 vintage).
SCF_2019_DIR  = '/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/2019/historical'
SCF_2019_RAW  = file.path(SCF_2019_DIR, 'p19i6.dta')
SCF_2019_SCFP = file.path(SCF_2019_DIR, 'SCFP2019.csv')

# Income-composition fields carried out of stage1 (the 8 reflated income
# columns). The 24 wealth columns to reflate are wealth_y_vars.
scf_income_comp_vars = c('wages_scf', 'business_scf', 'int_div_scf',
                         'capital_gains_scf', 'rent_scf', 'ss_pens_scf',
                         'ui_other_scf', 'income')

#---------------------------------------------------------------------------
# scf_raw_xcodes_required()
#
# The set of raw `X*` fields build_scf_tax_units() reads from the .dta,
# sourced FROM THE FUNCTION BODY so it can't drift out of sync. X7370 is
# excluded because the function *derives* it (X7370 = SYEAR - X8005) rather
# than reading it — keeping it in would false-stop the verification gate.
#---------------------------------------------------------------------------

scf_raw_xcodes_required = function() {
  body_txt = paste(deparse(body(build_scf_tax_units)), collapse = '\n')
  codes    = regmatches(body_txt, gregexpr('\\bX[0-9]+\\b', body_txt))[[1]]
  setdiff(sort(unique(toupper(codes))), 'X7370')
}

#---------------------------------------------------------------------------
# DC retirement allocation flags. build_scf_tax_units explicitly TOLERATES
# their absence (stage1_scf_tax_units.R ~L260-270 sets any missing flag to NA
# and compute_accruals falls back to the lifecycle glide). They drive only
# dc_equity_share_scf — a portfolio-mix flag, not a dollar amount — so their
# absence does not corrupt wealth levels. Hence OPTIONAL in the gate:
# absence is reported, not fatal. Every other X-code has no fallback.
scf_alloc_optional_xcodes = c('X3631', 'X3635', 'X3637',
                              'X11036', 'X11136', 'X11236',
                              'X11436', 'X11536', 'X11636')

# verify_scf_xcodes(raw_path)
#
# Gate (CLAUDE.md: no silent NA / wrong-codebook garbage). Reads only the
# column names of `raw_path`. Hard-stops with the missing list if any
# genuinely-required X-code is absent; WARNs (does not stop) for the
# optional DC-allocation codes, mirroring stage1's own graceful handling.
#---------------------------------------------------------------------------

verify_scf_xcodes = function(raw_path) {
  all_read = scf_raw_xcodes_required()
  required = setdiff(all_read, scf_alloc_optional_xcodes)
  hdr      = haven::read_dta(raw_path, n_max = 0)
  present  = toupper(names(hdr))

  miss_req = setdiff(required, present)
  if (length(miss_req) > 0L) {
    stop(sprintf(
      paste0('pool_scf_donors: %d required X-code(s) absent from %s — ',
             'build_scf_tax_units would read NA. Missing:\n  %s'),
      length(miss_req), raw_path, paste(miss_req, collapse = ', ')))
  }

  miss_opt = setdiff(intersect(scf_alloc_optional_xcodes, all_read), present)
  if (length(miss_opt) > 0L) {
    cat(sprintf(
      paste0('pool_scf_donors: WARN — %d optional DC-allocation code(s) ',
             'absent from %s; affected slots -> NA, lifecycle-glide fallback ',
             'in accruals (by design). Missing: %s\n'),
      length(miss_opt), basename(raw_path), paste(miss_opt, collapse = ', ')))
  }

  cat(sprintf('pool_scf_donors: X-code gate passed (%d/%d required present in %s)\n',
              length(required), length(required), basename(raw_path)))
  invisible(TRUE)
}

#---------------------------------------------------------------------------
# reflate_2019(scf_2019, scf_2022, reflate_vars)
#
# Per-category SCF-internal growth, computed DIRECTLY on the stage1 output
# columns (resolved design — SCFP summary categories don't map 1:1 to the
# y-vars and omit the income fields entirely). For each column v:
#   gf[v] = sum(w22 * y22[[v]]) / sum(w19 * y19[[v]])
# and y19[[v]] <- y19[[v]] * gf[v]. This self-aligns the 2019 weighted
# marginals to 2022, putting 2019 donor amounts on 2022 dollars.
#
# Note: factors are total-based (matching the validated household
# diagnostic), so each carries a small population-growth component on top of
# per-capita growth — acceptable and consistent with scf_2019_vs_2022_top.R.
# Guards against zero/NA denominators with a hard stop on the offender.
#---------------------------------------------------------------------------

reflate_2019 = function(scf_2019, scf_2022, reflate_vars) {
  stopifnot(all(reflate_vars %in% names(scf_2019)),
            all(reflate_vars %in% names(scf_2022)))

  num19 = sapply(reflate_vars, function(v) sum(scf_2022$weight * scf_2022[[v]]))
  den19 = sapply(reflate_vars, function(v) sum(scf_2019$weight * scf_2019[[v]]))

  bad = reflate_vars[!is.finite(den19) | den19 == 0]
  if (length(bad) > 0L) {
    stop(sprintf(
      'pool_scf_donors: 2019 weighted total is zero/non-finite for: %s',
      paste(bad, collapse = ', ')))
  }

  gf = num19 / den19
  for (v in reflate_vars) scf_2019[[v]] = scf_2019[[v]] * gf[v]

  cat('pool_scf_donors: per-column reflation factors (2019 -> 2022$):\n')
  print(round(gf, 3))
  attr(scf_2019, 'reflation_factors') = gf
  scf_2019
}

#---------------------------------------------------------------------------
# build_pooled_scf_donors(scf_2022 = NULL)
#
# Returns the combined 2019+2022 donor tibble (stage1 schema + donor_wave).
#   - scf_2022: the 2022 stage1 tax units. Defaults to the existing stage1
#     cache so the pool reuses the production 2022 build unchanged.
#---------------------------------------------------------------------------

build_pooled_scf_donors = function(
    scf_2022 = read_rds('resources/cache/scf_tax_units.rds')) {

  reflate_vars = c(wealth_y_vars, scf_income_comp_vars)
  stopifnot(all(reflate_vars %in% names(scf_2022)))

  # --- 1. Build 2019 (after the X-code gate) ---
  verify_scf_xcodes(SCF_2019_RAW)
  cat('pool_scf_donors: building 2019 tax units...\n')
  scf_2019 = build_scf_tax_units(
    config    = default_stage1_config(),
    raw_path  = SCF_2019_RAW,
    scfp_path = SCF_2019_SCFP,
    syear     = 2019
  )

  # Backstop the no-silent-NA contract on exactly the columns we reflate:
  # a dropped SCFP join column would surface here as an all-NA y-var rather
  # than corrupting weighted totals downstream.
  na_cols = reflate_vars[sapply(reflate_vars, function(v) anyNA(scf_2019[[v]]))]
  if (length(na_cols) > 0L) {
    stop(sprintf(
      'pool_scf_donors: NA in 2019 reflate column(s) after build: %s',
      paste(na_cols, collapse = ', ')))
  }

  # --- 2. Reflate 2019 to 2022 dollars ---
  scf_2019 = reflate_2019(scf_2019, scf_2022, reflate_vars)

  # --- 3. Equal-population weighting ---
  # Scale each wave to the same total population (use 2022's), then halve so
  # the combined pool represents one population. Sets sampling frequency
  # only; 2019 still contributes its extra distinct top rows (the point).
  p_target = sum(scf_2022$weight)
  scf_2019$weight = scf_2019$weight * (p_target / sum(scf_2019$weight)) / 2
  scf_2022$weight = scf_2022$weight * (p_target / sum(scf_2022$weight)) / 2

  # --- 4. Tag and bind ---
  scf_2019$donor_wave = 2019L
  scf_2022$donor_wave = 2022L
  pooled = dplyr::bind_rows(scf_2022, scf_2019)

  cat(sprintf(
    paste0('pool_scf_donors: pooled donor set — %d rows ',
           '(%d from 2022, %d from 2019); combined wtd pop = %.3fM ',
           '(target %.3fM)\n'),
    nrow(pooled), sum(pooled$donor_wave == 2022L),
    sum(pooled$donor_wave == 2019L),
    sum(pooled$weight) / 1e6, p_target / 1e6))

  pooled
}
