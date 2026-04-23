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
#' forest feature at src/imputations/wealth.R (lines ~212–222). Weighted
#' percentile uses compute_percentile() from src/imputations/helpers.R,
#' which assigns percentile 0 to non-positive income; those records land
#' in `pct00to20` — consistent with DFA's treatment of the bottom of the
#' income distribution.
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

  pctile = compute_percentile(income, puf_ref$weight)
  # findInterval with rightmost.closed=TRUE maps pctile=100 into the last
  # bucket; all.inside=TRUE guards against edge cases (shouldn't occur
  # given compute_percentile's 0..100 range but cheap belt-and-braces).
  bucket_idx = findInterval(pctile, dfa_income_edges,
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

  # Build a small fixture with known income ranks. 10 records uniform on
  # weight 1, income = 1..10; quantiles will be at 1, 2, …, 10 so pctile
  # equals 10 * rank (approximately). Zero-income rows should land in
  # pct00to20.
  make_row = function(id, wages_val) {
    tibble(
      id     = id,   weight = 1,
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

  # Test 2: non-positive income routes to pct00to20.
  puf_neg = bind_rows(
    make_row(1, -100),  # losses
    make_row(2, 0),     # zero
    make_row(3, 50000),
    make_row(4, 100000)
  )
  rb_neg = build_record_bucket(puf_neg)
  stopifnot(rb_neg$bucket[rb_neg$id == 1] == 'pct00to20')
  stopifnot(rb_neg$bucket[rb_neg$id == 2] == 'pct00to20')
  cat('  [PASS] non-positive income -> pct00to20\n')

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

  # Test 5: losses partially offset wages in the unfloored definition.
  # A record with wages=100 but scorp_active_loss=200 has income=-100 → pct=0.
  puf_loss = bind_rows(
    make_row(1, 10),
    make_row(2, 20),
    make_row(3, 30)
  )
  puf_loss$wages[3]             = 100
  puf_loss$scorp_active_loss[3] = 200
  rb_loss = build_record_bucket(puf_loss)
  stopifnot(rb_loss$bucket[rb_loss$id == 3] == 'pct00to20')
  cat('  [PASS] losses drive income negative -> pct00to20\n')

  cat('\nAll tests passed.\n')
}
