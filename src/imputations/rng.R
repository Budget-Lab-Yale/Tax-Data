#--------------------------------------------------------------------------
# rng.R  (decision S23)
#
# Random draws keyed by RECORD ID, not by row position or record count.
#
# WHY. Every imputation module used to draw with `runif(nrow(.))` or
# `sample_n()` under one global `set.seed(76)`. The number of draws each
# module consumes is then proportional to the number of records, so every
# module after the first sees a shifted stream whenever the record set
# changes size -- and a record's imputed value depends on who else is in the
# file. The federal validation battery failed on exactly this (2026-09-12):
# appending six more non-filer pool years moved 46 of 188 columns for the
# SAME 207,692 filers, the QBI sole-proprietor wage bill by -3.9% and
# childcare expenses by +2.0%, which flowed into AGI and moved 187 of 209
# filer-gated 1040 lines. See
# research/state_weights/nonfiler_federal_validation_findings.md.
#
# WHAT THIS GIVES. `draw_by_id(ids, stream)` returns the same value for the
# same (id, stream) pair no matter which other ids are present, how many
# there are, or what order they are in. Two consequences worth stating:
#
#   * a data change is attributable. Adding records changes the records you
#     added and nothing else, so "non-filer only" becomes a testable claim
#     rather than a hope.
#   * the pipeline stops depending on module ORDER for its values. Inserting
#     or removing an imputation no longer perturbs every later one.
#
# HOW. Each stream draws the uniform vector for a FIXED id space once, then
# indexes it by id. Fixed is the whole point: `runif(max(ids))` would make
# the draw depend on the largest id present, which is the bug again. The
# cost is one 80MB temporary per stream (`runif(1e7)`, about 0.3s), released
# immediately; only the drawn subset is kept.
#
# The global RNG state is SAVED AND RESTORED around every call, so these
# helpers are pure with respect to the rest of the pipeline: model training
# and any remaining positional draw see exactly the stream they would have
# seen without them.
#
# ADDING A STREAM. Put it in RNG_STREAMS with a new integer. Never reuse or
# renumber an index -- that silently rewrites history for every record on
# that stream. Never derive a stream index from a hash of the name either;
# an explicit table is what makes a collision impossible and a rename
# visible in review.
#
# SUB-KEYS. Some frames carry several rows per record and need an
# INDEPENDENT draw on each -- qbi.R pivots to one row per (record, form),
# and keying on the record alone would make a record's three forms perfectly
# correlated, which is a modelling change, not an RNG fix. `sub` supplies a
# small integer per row; each distinct value gets its own seeded vector.
# Stream indices are multiplied by 1000 in the seed so sub in [0, 999]
# cannot collide with the next stream.
#--------------------------------------------------------------------------

# The id space the draws are defined over. Fixed by decision, not by data.
# Tax-Data ids: filers < 1e6; non-filer pool year y occupies
# [1e6*(y-2016)+1, 1e6*(y-2015)], so the seven pools 2017-2023 reach
# 7,168,785. Raising this is a breaking change -- every draw moves -- so it
# is set with headroom once rather than tracked to the data.
RNG_ID_SPACE = 10000000L

# The base seed. 76 is the repo's long-standing global seed; keeping it
# means the FIRST stream reproduces nothing in particular, which is the
# honest position: S23 changes every imputed value once, by design.
RNG_BASE_SEED = 76L

# One index per stream. Explicit and append-only.
RNG_STREAMS = c(
  # demographics.R
  blind1                  = 1L,
  blind2                  = 2L,
  gender_joint            = 3L,
  gender_nonjoint         = 4L,
  male2_same_sex          = 5L,
  # ages.R
  age1                    = 10L,
  age2_gap                = 11L,
  dep_age1                = 12L,
  dep_age2                = 13L,
  dep_age3                = 14L,
  dep_ctc1                = 15L,
  dep_ctc2                = 16L,
  dep_ctc3                = 17L,
  # ssn.R -- one stream: SSN status is assumed perfectly correlated within a
  # tax unit, so dep_ssn1-3 are copies of `ssn` rather than separate draws
  ssn                     = 20L,
  # earnings_split.R
  wage_split              = 30L,
  # qbi.R
  qbi_sstb                = 40L,
  qbi_employer            = 41L,
  qbi_wagebill_noise      = 42L,
  # mobility.R
  mobility                = 50L,
  # tips.R
  tips_quantile           = 60L,
  tips_year               = 61L,
  tips_receipt            = 62L,
  # overtime.R
  ot_jitter_pctile        = 71L,
  ot_quantile             = 72L,
  ot_receipt              = 73L,
  # auto_loan.R
  auto_quantile           = 82L,
  auto_receipt            = 83L,
  # mortgage.R
  mortgage_quantile       = 91L,
  # childcare.R
  childcare_quantile      = 100L,
  # capital_gains.R
  kg_holding_period       = 110L,
  kg_bucket               = 111L,
  # consumption (cex.R / consumption.R)
  consumption_quantile    = 120L,
  # wealth.R -- the per-record donor pick inside the Stage 3 tilt
  wealth_donor            = 130L
)
stopifnot(!anyDuplicated(RNG_STREAMS), !anyDuplicated(names(RNG_STREAMS)))


#' Uniform draw per record id, invariant to the record set.
#'
#' @param ids     integer/numeric record ids, 1..RNG_ID_SPACE. May repeat:
#'                repeated ids get the SAME draw, which is what makes a
#'                per-id draw meaningful in a joined (many-rows-per-id) frame.
#' @param stream  a name in RNG_STREAMS.
#' @param min,max range, as for runif.
#' @return        numeric vector, one per element of `ids`.
draw_by_id = function(ids, stream, min = 0, max = 1, sub = 0L) {
  if (!(stream %in% names(RNG_STREAMS))) {
    stop('draw_by_id(): unknown stream "', stream, '". Add it to RNG_STREAMS ',
         'with a new index; never reuse one.', call. = FALSE)
  }
  if (anyNA(ids)) {
    stop('draw_by_id(): ', sum(is.na(ids)), ' NA ids on stream "', stream,
         '". An NA id has no draw -- fix the join that produced it.',
         call. = FALSE)
  }
  if (any(ids < 1) || any(ids > RNG_ID_SPACE)) {
    stop('draw_by_id(): ids outside [1, ', RNG_ID_SPACE, '] on stream "',
         stream, '" (range ', min(ids), '..', max(ids), '). Raising ',
         'RNG_ID_SPACE moves every draw; see the note there.', call. = FALSE)
  }
  sub = as.integer(sub)
  if (anyNA(sub) || any(sub < 0L) || any(sub > 999L)) {
    stop('draw_by_id(): `sub` must be an integer in [0, 999] on stream "',
         stream, '"', call. = FALSE)
  }
  if (length(sub) != 1L && length(sub) != length(ids)) {
    stop('draw_by_id(): `sub` must be length 1 or length(ids)', call. = FALSE)
  }

  # Pure with respect to the global stream: save, use, restore.
  if (exists('.Random.seed', envir = globalenv())) {
    saved = get('.Random.seed', envir = globalenv())
    on.exit(assign('.Random.seed', saved, envir = globalenv()), add = TRUE)
  }
  base = RNG_BASE_SEED + RNG_STREAMS[[stream]] * 1000L
  if (length(sub) == 1L) {
    set.seed(base + sub)
    return(runif(RNG_ID_SPACE, min = min, max = max)[ids])
  }
  out = numeric(length(ids))
  for (s_val in sort(unique(sub))) {
    set.seed(base + s_val)
    u = runif(RNG_ID_SPACE, min = min, max = max)
    hit = which(sub == s_val)
    out[hit] = u[ids[hit]]
  }
  out
}


#' One row per id, chosen with probability proportional to a weight column.
#'
#' The id-keyed replacement for `group_by(id) %>% sample_n(1, weight = p)`.
#' Within each id the rows are ordered as given, the weights are normalised
#' to a CDF, and the id's own uniform draw selects the row. Ties and
#' zero-weight rows behave as in the weighted sample they replace.
#'
#' @param df      a data frame with one or more rows per id.
#' @param stream  a name in RNG_STREAMS.
#' @param id_col,weight_col  column names (strings).
#' @return        df with exactly one row per id, in first-appearance order.
sample_one_by_id = function(df, stream, id_col = 'id', weight_col = 'p') {
  stopifnot(is.data.frame(df), id_col %in% names(df), weight_col %in% names(df))
  ids = df[[id_col]]
  w   = df[[weight_col]]
  if (anyNA(w)) {
    stop('sample_one_by_id(): NA in `', weight_col, '` on stream "', stream,
         '" -- an NA weight is not a probability. ', sum(is.na(w)),
         ' rows affected.', call. = FALSE)
  }
  if (any(w < 0)) {
    stop('sample_one_by_id(): negative `', weight_col, '` on stream "',
         stream, '".', call. = FALSE)
  }

  ord  = order(ids)                      # group rows without reordering output
  ids_s = ids[ord]; w_s = w[ord]
  grp   = cumsum(!duplicated(ids_s))     # 1..n_groups along the sorted vector
  tot   = as.numeric(tapply(w_s, grp, sum))[grp]
  if (any(tot <= 0)) {
    bad = unique(ids_s[tot <= 0])
    stop('sample_one_by_id(): ', length(bad), ' id(s) have zero total `',
         weight_col, '` on stream "', stream, '", so no row can be drawn; ',
         'first: ', bad[1], call. = FALSE)
  }
  cdf = unlist(lapply(split(w_s, grp), function(x) cumsum(x) / sum(x)),
               use.names = FALSE)

  u_by_group = draw_by_id(ids_s[!duplicated(ids_s)], stream)
  u = u_by_group[grp]

  # first row in the group whose CDF reaches u
  pick_s = !duplicated(grp[cdf >= u])
  sel_s  = which(cdf >= u)[pick_s]
  df[ord[sel_s], , drop = FALSE]
}


#' Pick one column index per row, uniformly, keyed by id.
#'
#' For `predict_*_draw()` helpers that hold a per-record grid of quantile
#' predictions and take one at random. Replaces `sample(n_col, 1)` per row.
#'
#' @param ids     record ids, one per row of the prediction matrix.
#' @param n_col   number of columns in the grid.
#' @param stream  a name in RNG_STREAMS.
#' @return        integer vector in 1..n_col, one per id.
column_by_id = function(ids, n_col, stream, sub = 0L) {
  stopifnot(length(n_col) == 1L, n_col >= 1L)
  pmin(n_col, 1L + floor(draw_by_id(ids, stream, sub = sub) * n_col))
}


#' One ensemble member per record from a quantregForest, chosen by id.
#'
#' The id-keyed replacement for `predict(m, newdata, what = function(x)
#' sample(x, 1))`. EXACT equivalence, not an approximation: quantregForest
#' applies `what` to each record's ensemble of `ntree` values, so asking for
#' the whole ensemble (`what = function(x) x`) and then choosing one column
#' per record draws from the same set with the same uniform probability --
#' only the source of the uniform changes.
#'
#' Why not keep `sample(x, 1)` and feed it an id-keyed draw: `what` receives
#' only the values, never the record, and the package probes it twice beyond
#' the row count and not in row order (measured 2026-09-13: 27 calls for 25
#' rows), so a counter inside the callback cannot identify the record.
#'
#' @param model   a quantregForest.
#' @param newdata prediction frame, rows aligned to `ids`.
#' @param ids     record ids, one per row of `newdata`.
#' @param stream  a name in RNG_STREAMS.
#' @param offset  added to every ensemble value before the pick, for the call
#'                sites that draw from `x - 1`.
predict_qrf_draw_by_id = function(model, newdata, ids, stream, offset = 0,
                                  sub = 0L) {
  ens = predict(model, newdata = newdata, what = function(x) x)
  if (!is.matrix(ens)) ens = matrix(ens, nrow = length(ids))
  if (nrow(ens) != length(ids)) {
    stop('predict_qrf_draw_by_id(): ', nrow(ens), ' prediction rows for ',
         length(ids), ' ids on stream "', stream, '"', call. = FALSE)
  }
  (ens + offset)[cbind(seq_along(ids),
                       column_by_id(ids, ncol(ens), stream, sub = sub))]
}


#--------------------------------------------------------------------------
# Standalone tests. Run with:  Rscript src/imputations/rng.R
# Exits 0 on success. Not part of the pipeline.
#--------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  cat('--- rng.R tests ---\n')

  # 1. The property the decision exists for: a record's draw does not depend
  #    on which other records are present, how many, or in what order.
  a = draw_by_id(c(5L, 17L, 1000003L), 'age1')
  b = draw_by_id(c(1L:100L, 1000003L, 17L, 5L), 'age1')
  stopifnot(identical(a[1], b[length(b)]),
            identical(a[2], b[length(b) - 1L]),
            identical(a[3], b[length(b) - 2L]))
  cat('  [PASS] draw is invariant to the surrounding record set\n')

  # 1b. Sub-keys are independent, and invariant the same way.
  s_a = draw_by_id(c(4L, 9L), 'qbi_sstb', sub = c(1L, 1L))
  s_b = draw_by_id(c(4L, 9L), 'qbi_sstb', sub = c(2L, 2L))
  stopifnot(all(s_a != s_b))
  mixed = draw_by_id(c(4L, 9L, 4L), 'qbi_sstb', sub = c(1L, 2L, 2L))
  stopifnot(mixed[1] == s_a[1], mixed[2] == s_b[2], mixed[3] == s_b[1])
  stopifnot(identical(draw_by_id(4L, 'qbi_sstb', sub = 1L),
                      draw_by_id(c(1L, 4L, 77L), 'qbi_sstb', sub = 1L)[2]))
  cat('  [PASS] sub-keys are independent and invariant\n')

  # 2. Same id, different streams, different draws.
  stopifnot(draw_by_id(42L, 'age1') != draw_by_id(42L, 'ssn'))
  cat('  [PASS] streams are independent\n')

  # 3. Repeated ids get the same draw (needed in joined frames).
  r = draw_by_id(c(7L, 7L, 8L), 'ssn')
  stopifnot(r[1] == r[2], r[1] != r[3])
  cat('  [PASS] repeated ids draw once\n')

  # 4. Purity: the global stream is untouched.
  set.seed(999); before = runif(3)
  set.seed(999); invisible(draw_by_id(1:10, 'tips_receipt')); after = runif(3)
  stopifnot(identical(before, after))
  cat('  [PASS] global RNG state is preserved\n')

  # 5. Range, and uniformity to a loose tolerance.
  u = draw_by_id(1:200000, 'wage_split')
  stopifnot(all(u >= 0), all(u < 1), abs(mean(u) - 0.5) < 0.01)
  v = draw_by_id(1:1000, 'dep_age1', min = 5, max = 13)
  stopifnot(all(v >= 5), all(v < 13))
  cat('  [PASS] range and mean\n')

  # 6. Guards.
  for (bad in list(
    function() draw_by_id(1L, 'not_a_stream'),
    function() draw_by_id(c(1L, NA), 'ssn'),
    function() draw_by_id(RNG_ID_SPACE + 1, 'ssn'),
    function() draw_by_id(0L, 'ssn'))) {
    stopifnot(inherits(try(bad(), silent = TRUE), 'try-error'))
  }
  cat('  [PASS] unknown stream, NA id and out-of-range id all stop\n')

  # 7. sample_one_by_id: one row per id, invariant to the other ids present,
  #    and it honours the weights.
  mk = function(ids) data.frame(
    id = rep(ids, each = 3),
    v  = rep(c('a', 'b', 'c'), times = length(ids)),
    p  = rep(c(0.2, 0.3, 0.5), times = length(ids)),
    stringsAsFactors = FALSE)
  s1 = sample_one_by_id(mk(c(11L, 12L, 13L)), 'mobility')
  s2 = sample_one_by_id(mk(c(90L:99L, 13L, 12L, 11L)), 'mobility')
  stopifnot(nrow(s1) == 3L, identical(sort(s1$id), c(11L, 12L, 13L)))
  m = merge(s1, s2, by = 'id')
  stopifnot(identical(m$v.x, m$v.y))
  cat('  [PASS] sample_one_by_id: one row per id, invariant to the rest\n')

  big = sample_one_by_id(mk(1:20000), 'mobility')
  sh  = prop.table(table(big$v))
  stopifnot(abs(sh[['a']] - 0.2) < 0.02, abs(sh[['b']] - 0.3) < 0.02,
            abs(sh[['c']] - 0.5) < 0.02)
  cat('  [PASS] sample_one_by_id: selection frequencies match the weights\n')

  z = mk(1:5); z$p = 0
  stopifnot(inherits(try(sample_one_by_id(z, 'mobility'), silent = TRUE), 'try-error'))
  cat('  [PASS] sample_one_by_id: a zero-weight id stops rather than silently dropping\n')

  # 8. column_by_id: in range, invariant, and roughly uniform.
  ci = column_by_id(1:50000, 99L, 'tips_quantile')
  stopifnot(all(ci >= 1L), all(ci <= 99L),
            identical(column_by_id(c(3L, 9L), 99L, 'tips_quantile'),
                      ci[c(3L, 9L)]))
  cat('  [PASS] column_by_id: range and invariance\n')

  cat('\nAll tests passed.\n')
}
