#---------------------------------------------
# record_bucket.R
#
# Builds a frozen per-record income bucket at a
# reference year (2022 for the current pipeline)
# to pair with bucketed_factors in materialize().
#
# A record's bucket is assigned at the reference
# year and reused for all target years — this
# matches how DFA-style distributional aging is
# defined (a household's position in the income
# distribution is fixed at a reference year, not
# re-ranked every year).
#
# The income definition matches the wealth.R
# forest feature (unfloored sum; losses pass
# through). The bucket boundaries match DFA's
# published income slice in dfa_totals.csv — six
# unequal bins.
#---------------------------------------------


# DFA income-slice labels and percentile edges. Six bins covering
# [0, 100]. Inclusive-left, exclusive-right except the top bucket is
# closed on the right.
dfa_income_buckets = c('pct00to20',  'pct20to40',  'pct40to60',
                       'pct60to80',  'pct80to99',  'pct99to100')
dfa_income_edges   = c(0, 20, 40, 60, 80, 99, 100)


#' Build per-record (id, bucket) from a reference-year PUF tibble.
#'
#' Income is reconstructed as the same unfloored sum used in the wealth
#' forest feature at src/imputations/wealth.R (lines ~212–222). Bucket
#' assignment uses a continuous weighted rank over the full income
#' distribution (zeros and losses included), matching the rule in
#' assign_calibration_cells() at src/imputations/stage3_target_qc.R. Same
#' percentile method ensures the DFA aging bucket is a deterministic
#' coarsening of the imputation cell — a record's calibration cell maps
#' uniquely to a DFA bucket since dfa_income_edges is a subset of
#' CALIB_INCOME_EDGES. Records with negative or zero income fall to the
#' bottom of the weighted rank and land in pct00to20 only if they are
#' in the bottom 20% by weight; an asset-rich household with a one-year
#' loss no longer gets permanently routed to the bottom bucket.
#'
#' @param puf_ref  PUF tibble at the reference year. Must carry `id`,
#'                 `weight`, and the income composition columns listed
#'                 below.
#' @return         Tibble (id, bucket). bucket is a character column
#'                 whose values are a subset of `dfa_income_buckets`.
build_record_bucket = function(puf_ref) {
  required = c(
    'id', 'weight',
    'wages',
    'sole_prop', 'farm',
    'scorp_active',  'scorp_active_loss',  'scorp_179',
    'scorp_passive', 'scorp_passive_loss',
    'part_active',   'part_active_loss',   'part_179',
    'part_passive',  'part_passive_loss',
    'txbl_int', 'exempt_int', 'div_ord', 'div_pref',
    'kg_lt', 'kg_st',
    'gross_ss', 'gross_pens_dist',
    'ui',
    'rent', 'rent_loss', 'estate', 'estate_loss'
  )
  missing = setdiff(required, names(puf_ref))
  if (length(missing) > 0L) {
    stop('build_record_bucket(): missing columns in puf_ref: ',
         paste(missing, collapse = ', '))
  }

  income = with(puf_ref,
    wages +
    sole_prop + farm +
    scorp_active  - scorp_active_loss  - scorp_179 +
    scorp_passive - scorp_passive_loss +
    part_active   - part_active_loss   - part_179 +
    part_passive  - part_passive_loss +
    txbl_int + exempt_int + div_ord + div_pref +
    kg_lt + kg_st +
    gross_ss + gross_pens_dist +
    ui +
    rent - rent_loss + estate - estate_loss
  )

  # NA guard — income should be fully defined at the reference year.
  # Surface counts rather than silently dropping.
  if (any(is.na(income))) {
    stop('build_record_bucket(): income computation produced ',
         sum(is.na(income)), ' NA(s). Check upstream imputations.')
  }

  ord = order(income)
  cum_w = cumsum(puf_ref$weight[ord]) / sum(puf_ref$weight)
  rank_0_100 = numeric(length(income))
  rank_0_100[ord] = 100 * cum_w
  bucket_idx = findInterval(rank_0_100, dfa_income_edges,
                            rightmost.closed = TRUE,
                            all.inside       = TRUE)

  tibble::tibble(
    id     = puf_ref$id,
    bucket = dfa_income_buckets[bucket_idx]
  )
}


#---------------------------------------------------------------------------
# Standalone tests. Run with:
#   Rscript src/record_bucket.R
#---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(Hmisc)   # wtd.quantile
  })
  source('src/imputations/helpers.R')

  # Build a small fixture with known income ranks. The bucket assignment
  # uses continuous weighted rank over the FULL distribution (zeros and
  # losses included, no special routing) — same rule as
  # assign_calibration_cells in src/imputations/stage3_target_qc.R, with
  # a coarser set of edges.
  make_row = function(id, wages_val, weight_val = 1) {
    tibble(
      id     = id,   weight = weight_val,
      wages  = wages_val,
      sole_prop = 0, farm = 0,
      scorp_active = 0, scorp_active_loss = 0, scorp_179 = 0,
      scorp_passive = 0, scorp_passive_loss = 0,
      part_active = 0, part_active_loss = 0, part_179 = 0,
      part_passive = 0, part_passive_loss = 0,
      txbl_int = 0, exempt_int = 0, div_ord = 0, div_pref = 0,
      kg_lt = 0, kg_st = 0,
      gross_ss = 0, gross_pens_dist = 0,
      ui = 0,
      rent = 0, rent_loss = 0, estate = 0, estate_loss = 0
    )
  }

  cat('--- record_bucket.R tests ---\n')

  # Test 1: monotone assignment. Ranks distribute across buckets correctly.
  puf = bind_rows(lapply(1:100, function(i) make_row(i, i)))
  rb  = build_record_bucket(puf)
  stopifnot(nrow(rb) == 100)
  stopifnot(all(rb$bucket %in% dfa_income_buckets))
  # Lowest-income rows should be in pct00to20, highest in pct99to100.
  stopifnot(rb$bucket[rb$id == 1]   == 'pct00to20')
  stopifnot(rb$bucket[rb$id == 100] == 'pct99to100')
  cat('  [PASS] monotone rank -> correct extreme buckets\n')

  # Test 2: monotonicity across the negative/zero/positive boundary. The
  # bucket label is determined by weighted rank, so id=1 (-100) ≤ id=2 (0)
  # ≤ id=3 ≤ id=4 by dfa_income_buckets order. With 4 equal-weight records
  # the negative-income one sits at rank 25, NOT pct00to20 — that's the
  # design (no special routing of non-positive income).
  puf_neg = bind_rows(
    make_row(1, -100),
    make_row(2, 0),
    make_row(3, 50000),
    make_row(4, 100000)
  )
  rb_neg = build_record_bucket(puf_neg)
  stopifnot(match(rb_neg$bucket[rb_neg$id == 1], dfa_income_buckets) <=
            match(rb_neg$bucket[rb_neg$id == 2], dfa_income_buckets))
  stopifnot(match(rb_neg$bucket[rb_neg$id == 2], dfa_income_buckets) <=
            match(rb_neg$bucket[rb_neg$id == 3], dfa_income_buckets))
  stopifnot(match(rb_neg$bucket[rb_neg$id == 3], dfa_income_buckets) <=
            match(rb_neg$bucket[rb_neg$id == 4], dfa_income_buckets))
  cat('  [PASS] monotone bucket assignment across negative/zero/positive\n')

  # Test 2b: when a negative-income record is firmly in the bottom 20% by
  # weight, it lands in pct00to20. Heavy weights on the positive-income
  # records push the loser into the bottom rank slice.
  puf_heavy = bind_rows(
    make_row(1, -100, weight_val = 1),
    make_row(2,  100, weight_val = 100),
    make_row(3, 1000, weight_val = 100)
  )
  rb_heavy = build_record_bucket(puf_heavy)
  stopifnot(rb_heavy$bucket[rb_heavy$id == 1] == 'pct00to20')
  cat('  [PASS] negative income at bottom of weighted rank -> pct00to20\n')

  # Test 3: missing required column errors explicitly.
  puf_short = puf[, setdiff(names(puf), 'ui')]
  err = tryCatch(build_record_bucket(puf_short),
                 error = function(e) conditionMessage(e))
  stopifnot(grepl('ui', err))
  cat('  [PASS] missing column errors with column name\n')

  # Test 4: NA in any income component errors (never silent).
  puf_na = puf
  puf_na$wages[5] = NA_real_
  err_na = tryCatch(build_record_bucket(puf_na),
                    error = function(e) conditionMessage(e))
  stopifnot(grepl('NA', err_na))
  cat('  [PASS] NA in income component errors\n')

  # Test 5: losses pull a record's bucket down. id=3 starts with wages=100
  # but a scorp_active_loss=200 leaves it with the lowest income of the
  # three, so it lands at the bottom of the weighted rank.
  puf_loss = bind_rows(
    make_row(1, 10),
    make_row(2, 20),
    make_row(3, 30)
  )
  puf_loss$wages[3]             = 100
  puf_loss$scorp_active_loss[3] = 200
  rb_loss = build_record_bucket(puf_loss)
  stopifnot(match(rb_loss$bucket[rb_loss$id == 3], dfa_income_buckets) <=
            match(rb_loss$bucket[rb_loss$id == 1], dfa_income_buckets))
  cat('  [PASS] losses pass through unfloored, push bucket down\n')

  cat('\nAll tests passed.\n')
}
