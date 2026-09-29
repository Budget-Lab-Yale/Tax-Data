#------------------------------------------------------------------------------
# state_weights_contract.R
#
# The consumer-side contract for the Tax-Simulator baseline the state-weights
# split reads its calculated AGI and EITC from (the two-pass dependency, S30;
# research/state_weights/notes/phase4_state_weights_interface.md, 2.1).
#
# The split stratifies every filer target by an AGI that only Tax-Simulator
# computes, so it reads a pinned baseline vintage's per-record detail. Two
# things can go silently wrong, and this file makes both loud: the baseline
# was run on a DIFFERENT Tax-Data vintage than the one being split (the ids
# would mostly still match, and the join would mostly succeed), or it was run
# on a subsample (weights already divided by pct_sample, ids missing).
#
# One function per check, sourced by src/main_state_weights.R and by
# src/tests/test_state_weights_contract.R, so the test cannot drift from the
# rule it tests (the nonfiler_contract.R pattern).
#------------------------------------------------------------------------------

BASELINE_DETAIL_COLS = c('id', 'weight', 'filer', 'agi', 'eitc')

# The Tax-Data vintage a Tax-Simulator baseline was run on, from the
# dependencies.csv Tax-Simulator writes one level above the scenario directory
# (<...>/Tax-Simulator/v1/<vintage>/dependencies.csv; the interface path ends
# in /<vintage>/<scenario>).
baseline_pinned_tax_data = function(baseline_dir) {
  dep_file = file.path(dirname(baseline_dir), 'dependencies.csv')
  if (!file.exists(dep_file)) {
    stop('Tax-Simulator baseline at ', baseline_dir, ' has no dependencies.csv ',
         'beside its scenario directory; cannot tell which Tax-Data vintage it was run on.')
  }
  deps = read_csv(dep_file, show_col_types = FALSE)
  pin = deps %>% filter(interface == 'Tax-Data', ID == basename(baseline_dir))
  if (nrow(pin) != 1) {
    stop('Tax-Simulator baseline ', dep_file, ': expected one Tax-Data row for scenario "',
         basename(baseline_dir), '", found ', nrow(pin))
  }
  pin$vintage
}

# Refuse a baseline that was not run on the vintage being split
check_baseline_pin = function(baseline_dir, tax_data_vintage) {
  pinned = baseline_pinned_tax_data(baseline_dir)
  if (pinned != tax_data_vintage) {
    stop('Tax-Simulator baseline ', baseline_dir, ' was run on Tax-Data vintage ', pinned,
         ', but the records being split are vintage ', tax_data_vintage,
         '. The split needs a baseline run on the same vintage ',
         '(Tax-Simulator config/runscripts/tests/state_weights_baseline.csv).')
  }
  invisible(pinned)
}

# One year's per-record baseline detail: the columns the split uses
read_baseline_detail = function(baseline_dir, year) {
  f = file.path(baseline_dir, 'static', 'detail', paste0(year, '.csv'))
  if (!file.exists(f)) stop('Tax-Simulator baseline has no detail file for TY', year, ': ', f)
  fread(f, select = BASELINE_DETAIL_COLS)
}

# The detail must describe exactly the records being split: same ids, same
# national weights (so the baseline ran at full sample -- Tax-Simulator divides
# `weight` by pct_sample), and an AGI and EITC for every one. Returns the
# detail as (id, agi, eitc) in the order of `tax_units`.
validate_baseline_detail = function(detail, tax_units, year, label = 'Tax-Simulator baseline') {
  detail = as_tibble(detail)
  if (nrow(detail) == 0) stop(label, ' TY', year, ': detail has zero rows')
  missing_cols = setdiff(BASELINE_DETAIL_COLS, names(detail))
  if (length(missing_cols)) stop(label, ' TY', year, ': detail lacks ', paste(missing_cols, collapse = ', '))
  if (anyDuplicated(detail$id)) stop(label, ' TY', year, ': duplicate ids in the detail')

  only_detail = setdiff(detail$id, tax_units$id)
  only_units  = setdiff(tax_units$id, detail$id)
  if (length(only_detail) || length(only_units)) {
    stop(label, ' TY', year, ': id sets differ -- ', length(only_units), ' records have no detail row, ',
         length(only_detail), ' detail rows have no record. The baseline was run on a different ',
         'vintage or a subsample.')
  }
  d = detail[match(tax_units$id, detail$id), ]
  if (!isTRUE(all.equal(d$weight, tax_units$weight, tolerance = 1e-9))) {
    stop(label, ' TY', year, ': detail weights differ from the records\' national weights ',
         '(largest relative gap ', signif(max(abs(d$weight / tax_units$weight - 1)), 3),
         '). The baseline was not run at full sample.')
  }
  if (anyNA(d$agi) || anyNA(d$eitc)) {
    stop(label, ' TY', year, ': ', sum(is.na(d$agi)), ' NA agi and ', sum(is.na(d$eitc)), ' NA eitc')
  }
  d %>% select(id, agi, eitc)
}
