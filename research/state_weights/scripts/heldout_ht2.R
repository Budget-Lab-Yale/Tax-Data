#------------------------------------------------------------------------------
# research/state_weights/scripts/heldout_ht2.R  (sourced, not run)
#
# The held-out HT2 series: IRS state x AGI-stub cells the filer fit never
# targets, share-normalized exactly like the targeted set, so an untargeted
# miss reads on the same scale as a targeted one. Shared by
# validate_state_weights.R --untargeted and phase3_score.R.
#
# A00900 Schedule C net income; A01700 taxable pensions/annuities; A02500
# taxable social security; A06500 income tax after credits. kg_amt (A01000),
# excluded from calibration for mixed sign, is scored on its positive part.
#------------------------------------------------------------------------------

#' The held-out cells for one year: one row per (series, stub, state) with the
#' records it covers, their x-vector and the share-normalized target.
#' @param tu_f the filer partition's records, in p$idx order, with baseline
#'        agi, txbl_ss and liab_iit joined
heldout_cells <- function(year, p, tu_f, jur) {
  stopifnot(nrow(tu_f) == length(p$w), all(abs(tu_f$weight - p$w) < 1e-9))
  stub <- assign_ht2_stub(tu_f$agi, year)
  HELD_OUT <- list(
    n_bus       = list(code = 'N00900', x = as.numeric(tu_f$sole_prop != 0)),
    bus_amt     = list(code = 'A00900', x = tu_f$sole_prop),
    n_pens      = list(code = 'N01700', x = as.numeric(tu_f$txbl_pens_dist != 0)),
    pens_amt    = list(code = 'A01700', x = tu_f$txbl_pens_dist),
    n_ss        = list(code = 'N02500', x = as.numeric(tu_f$txbl_ss > 0)),
    ss_amt      = list(code = 'A02500', x = tu_f$txbl_ss),
    n_fedtax    = list(code = 'N06500', x = as.numeric(tu_f$liab_iit > 0)),
    fedtax_amt  = list(code = 'A06500', x = pmax(tu_f$liab_iit, 0)),
    kg_amt_pos  = list(code = 'A01000', x = pmax(tu_f$kg_st + tu_f$kg_lt, 0))
  )

  # Raw HT2 cells, same reader conventions as read_ht2
  d <- fread(cmd = paste('zcat', shQuote(ht2_path(year))), colClasses = 'character')
  setnames(d, toupper(names(d)))
  parse_num <- function(x) as.numeric(str_replace_all(x, ',', ''))
  codes <- unique(vapply(HELD_OUT, `[[`, '', 'code'))
  ht2 <- d[, c('STATE', 'AGI_STUB', codes), with = FALSE]
  ht2 <- ht2[STATE %in% jur & AGI_STUB != '0']
  for (cc in codes) ht2[, (cc) := parse_num(get(cc)) * ifelse(grepl('^A', cc), 1000, 1)]
  ht2[, stub := as.integer(AGI_STUB)]

  cells <- list()
  for (ser in names(HELD_OUT)) {
    ho <- HELD_OUT[[ser]]
    for (sb in sort(unique(ht2$stub))) {
      rows <- which(stub == sb)
      if (sum(ho$x[rows] != 0) == 0) next
      cl <- ht2[stub == sb & get(ho$code) > 0]
      tgt_natl <- sum(p$w[rows] * ho$x[rows])             # PUF national for the stub
      if (tgt_natl <= 0) next
      shares <- cl[[ho$code]] / sum(cl[[ho$code]])
      cells[[length(cells) + 1]] <- list(series = ser, stub = sb, state = cl$STATE,
                                         s = match(cl$STATE, jur), rows = rows,
                                         x = ho$x[rows], target = tgt_natl * shares)
    }
  }
  cells
}

#' Score one share matrix on the held-out cells: data.table(series, stub,
#' state, target, that, err)
score_heldout <- function(cells, w, P) {
  rbindlist(lapply(cells, function(cl) {
    that <- as.vector(crossprod(w[cl$rows] * cl$x, P[cl$rows, cl$s, drop = FALSE]))
    data.table(series = cl$series, stub = cl$stub, state = cl$state,
               target = cl$target, that = that)
  }))[, err := abs(that / target - 1)]
}
