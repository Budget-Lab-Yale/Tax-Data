#--------------------------------------
# accruals.R
#
# Per-record annual unrealized-gain accrual flows for the 7
# appreciation-bearing wealth categories. Anchored to Z.1 long-run
# revaluation means; design in docs/wealth_accruals_design.md.
#
# Public interface:
#   compute_accruals(value_tbl,
#                    donor_dc_eq_share, donor_dc_total,
#                    age_older)
#     -> tibble of length nrow(value_tbl) with the 7 accruals.* cols.
#
# Constants:
#   Z1_RATES        — Federal Reserve Z.1 long-run mean revaluation
#                     rates by asset class, 1990–2024 vintage.
#   TRUST_PORTFOLIO — assumed equity/bond mix for trusts; documented
#                     here so the trust accrual rate is computed, not
#                     hardcoded.
#--------------------------------------

# Z.1 long-run revaluation means, 1990–2024. See
# docs/wealth_accruals_design.md "Anchor data" table.
Z1_RATES = c(
  equities       = 0.118,
  pass_throughs  = 0.052,
  real_estate    = 0.045,
  bonds          = 0.0     # zero by assumption (Z.1 mean −0.08%, rounded to 0)
)

# Trust portfolio: 60/40 equity/bond. Computed, not hardcoded.
TRUST_PORTFOLIO = c(equities = 0.60, bonds = 0.40)
TRUST_RATE = sum(TRUST_PORTFOLIO * Z1_RATES[names(TRUST_PORTFOLIO)])
# = 0.60 × 0.118 + 0.40 × 0 = 0.0708


#' Lifecycle equity-share glide for DC retirement accounts.
#'
#' Standard target-date heuristic: equity share = 1.10 − age/100, capped
#' to [0.20, 0.95]. At age 30 → 0.80; age 50 → 0.60; age 70 → 0.40;
#' age 95+ → 0.20 floor.
lifecycle_equity_share = function(age) {
  pmin(pmax(1.10 - age / 100, 0.20), 0.95)
}


#' Compute per-record annual accrual flows.
#'
#' @param value_tbl tibble (or data.frame) with columns value.equities,
#'                   value.pass_throughs, value.primary_home,
#'                   value.other_home, value.re_fund, value.dc,
#'                   value.trusts. Extra columns are ignored.
#' @param donor_dc_eq_share numeric ∈ [0,1] or NA, length nrow(value_tbl).
#'                   Donor-side SCF-reported DC equity share. NA when
#'                   donor has no DC accounts or no allocation flag.
#' @param donor_dc_total numeric, length nrow(value_tbl). Donor's total
#'                   DC balance (used to detect "donor has no DC" so we
#'                   don't blend a meaningless flag).
#' @param age_older integer, length nrow(value_tbl). PUF record's older
#'                   filer age (used in lifecycle glide).
#'
#' @return tibble with columns accruals.{equities, pass_throughs,
#'         primary_home, other_home, re_fund, dc, trusts}.
compute_accruals = function(value_tbl,
                            donor_dc_eq_share, donor_dc_total,
                            age_older) {
  n = nrow(value_tbl)
  stopifnot(length(donor_dc_eq_share) == n,
            length(donor_dc_total)    == n,
            length(age_older)         == n)

  needed = paste0('value.',
                  c('equities', 'pass_throughs', 'primary_home',
                    'other_home', 're_fund', 'dc', 'trusts'))
  missing = setdiff(needed, names(value_tbl))
  if (length(missing) > 0L) {
    stop('compute_accruals: value_tbl missing required columns: ',
         paste(missing, collapse = ', '))
  }

  # Per-record DC equity share. 50/50 blend of donor SCF report and
  # PUF-age lifecycle glide. Fall back to lifecycle alone when the donor
  # has no DC (NA share or zero DC total) — a meaningless flag should
  # not pollute the blend.
  glide   = lifecycle_equity_share(age_older)
  use_scf = !is.na(donor_dc_eq_share) & donor_dc_total > 0
  dc_eq   = if_else(use_scf,
                    0.5 * donor_dc_eq_share + 0.5 * glide,
                    glide)

  # Constants extracted to scalars so the row-wise multiplications below
  # don't trip on names() carry-over in vectors.
  r_eq     = unname(Z1_RATES['equities'])
  r_pass   = unname(Z1_RATES['pass_throughs'])
  r_re     = unname(Z1_RATES['real_estate'])
  r_bonds  = unname(Z1_RATES['bonds'])
  r_trusts = unname(TRUST_RATE)

  tibble(
    accruals.equities      = r_eq    * value_tbl[['value.equities']],
    accruals.pass_throughs = r_pass  * value_tbl[['value.pass_throughs']],
    accruals.primary_home  = r_re    * value_tbl[['value.primary_home']],
    accruals.other_home    = r_re    * value_tbl[['value.other_home']],
    accruals.re_fund       = r_re    * value_tbl[['value.re_fund']],
    accruals.dc            = (dc_eq * r_eq + (1 - dc_eq) * r_bonds) *
                              value_tbl[['value.dc']],
    accruals.trusts        = r_trusts * value_tbl[['value.trusts']]
  )
}


#---------------------------------------------------------------------------
# Standalone tests. Run via:
#   Rscript src/imputations/accruals.R
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(tibble)
  })

  cat('--- accruals.R tests ---\n')

  # Test A: lifecycle_equity_share endpoints + middle.
  stopifnot(abs(lifecycle_equity_share(30) - 0.80) < 1e-12)
  stopifnot(abs(lifecycle_equity_share(50) - 0.60) < 1e-12)
  stopifnot(abs(lifecycle_equity_share(70) - 0.40) < 1e-12)
  stopifnot(abs(lifecycle_equity_share(20) - 0.90) < 1e-12)
  # Cap at 0.95 (e.g. age 5)
  stopifnot(abs(lifecycle_equity_share(5)  - 0.95) < 1e-12)
  # Floor at 0.20 (e.g. age 95)
  stopifnot(abs(lifecycle_equity_share(95) - 0.20) < 1e-12)
  cat('  [PASS] lifecycle_equity_share endpoints + cap/floor\n')

  # Test B: TRUST_RATE is the 60/40 blend with bonds at 0.
  stopifnot(abs(TRUST_RATE - (0.60 * 0.118 + 0.40 * 0)) < 1e-12)
  cat(sprintf('  [PASS] TRUST_RATE = %.4f (computed from documented 60/40 mix)\n',
              TRUST_RATE))

  # Test C: per-category accruals on a single-row fixture with NA DC
  # share → DC accrual uses pure lifecycle glide.
  v = tibble(
    value.equities      = 100,
    value.pass_throughs = 100,
    value.primary_home  = 100,
    value.other_home    = 100,
    value.re_fund       = 100,
    value.dc            = 100,
    value.trusts        = 100
  )
  acc = compute_accruals(
    value_tbl         = v,
    donor_dc_eq_share = NA_real_,
    donor_dc_total    = 0,
    age_older         = 50L
  )
  stopifnot(abs(acc$accruals.equities      - 11.8 ) < 1e-9)
  stopifnot(abs(acc$accruals.pass_throughs -  5.2 ) < 1e-9)
  stopifnot(abs(acc$accruals.primary_home  -  4.5 ) < 1e-9)
  stopifnot(abs(acc$accruals.other_home    -  4.5 ) < 1e-9)
  stopifnot(abs(acc$accruals.re_fund       -  4.5 ) < 1e-9)
  # DC at 50: glide=0.60, NA donor share → blend = glide = 0.60.
  # DC accrual = (0.60*0.118 + 0.40*0) * 100 = 7.08.
  stopifnot(abs(acc$accruals.dc            -  7.08) < 1e-9)
  stopifnot(abs(acc$accruals.trusts        -  7.08) < 1e-9)
  cat('  [PASS] per-category rates × value (NA donor → lifecycle fallback)\n')

  # Test D: with donor share present, DC blend is 50/50.
  acc2 = compute_accruals(
    value_tbl         = v,
    donor_dc_eq_share = 1.0,    # donor reports all stocks
    donor_dc_total    = 50000,
    age_older         = 50L
  )
  # blend = 0.5*1.0 + 0.5*0.60 = 0.80; DC accrual = 0.80 * 0.118 * 100 = 9.44.
  stopifnot(abs(acc2$accruals.dc - 9.44) < 1e-9)
  cat('  [PASS] DC blend uses 50/50 of donor share + lifecycle\n')

  # Test E: donor_dc_total == 0 forces lifecycle fallback even with
  # a non-NA donor share.
  acc3 = compute_accruals(
    value_tbl         = v,
    donor_dc_eq_share = 1.0,
    donor_dc_total    = 0,      # donor has no DC despite a stale flag
    age_older         = 50L
  )
  stopifnot(abs(acc3$accruals.dc - 7.08) < 1e-9)   # same as Test C
  cat('  [PASS] zero donor DC total forces lifecycle fallback\n')

  # Test F: vectorized — three records, mixed donor presence.
  v3 = tibble(
    value.equities      = c(100, 200, 300),
    value.pass_throughs = c(  0,   0,   0),
    value.primary_home  = c(  0,   0,   0),
    value.other_home    = c(  0,   0,   0),
    value.re_fund       = c(  0,   0,   0),
    value.dc            = c(100, 200, 300),
    value.trusts        = c(  0,   0,   0)
  )
  acc4 = compute_accruals(
    value_tbl         = v3,
    donor_dc_eq_share = c(1.0, NA_real_, 0.0),
    donor_dc_total    = c(50000, 0, 30000),
    age_older         = c(30L, 50L, 70L)
  )
  # rec 1 (age 30, donor 1.0): glide=0.80; blend=0.5*1.0+0.5*0.80=0.90
  #         DC accrual = 0.90 * 0.118 * 100 = 10.62
  # rec 2 (age 50, donor NA): blend = glide = 0.60
  #         DC accrual = 0.60 * 0.118 * 200 = 14.16
  # rec 3 (age 70, donor 0.0): glide=0.40; blend=0.5*0.0+0.5*0.40=0.20
  #         DC accrual = 0.20 * 0.118 * 300 =  7.08
  stopifnot(abs(acc4$accruals.dc[1] - 10.62) < 1e-9)
  stopifnot(abs(acc4$accruals.dc[2] - 14.16) < 1e-9)
  stopifnot(abs(acc4$accruals.dc[3] -  7.08) < 1e-9)
  cat('  [PASS] vectorized DC blend across mixed donor presence\n')

  # Test G: missing required column → informative error.
  bad = v %>% select(-value.dc)
  err = tryCatch(
    compute_accruals(bad, NA_real_, 0, 50L),
    error = function(e) conditionMessage(e)
  )
  stopifnot(grepl('value.dc', err))
  cat('  [PASS] missing required column raises informative error\n')

  cat('\nAll tests passed.\n')
}
