#------------------------------------------------------------------------------
# test_state_weights_contract.R
#
# The consumer-side contract for the Tax-Simulator baseline the state-weights
# split reads AGI and EITC from (src/state_weights_contract.R).
#
# Part 1 runs anywhere in seconds on a fixture: the well-formed case passes;
# a missing record, a stray detail row, a subsampled weight and an NA AGI each
# stop with the message that names the cause. Part 2 runs only where the
# shared filer is mounted: the published P1 baseline passes the pin check
# against the vintage it was run on and fails against the earlier one.
#
#   Rscript src/tests/test_state_weights_contract.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
source('./src/state_weights_contract.R')

expect_stop = function(expr, pattern, what) {
  msg = tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(msg)) stop('FAIL ', what, ': did not stop')
  if (!grepl(pattern, msg)) stop('FAIL ', what, ': stopped with an unexpected message: ', msg)
  cat('ok  ', what, '\n')
}

#------------------------------------------------------------------------------
# Part 1: the fixture
#------------------------------------------------------------------------------

units  = tibble(id = c(3, 1, 2, 1000001), weight = c(10.5, 220.25, 7, 1500))
detail = tibble(id = c(1, 2, 3, 1000001), weight = c(220.25, 7, 10.5, 1500),
                filer = c(1L, 1L, 1L, 0L), agi = c(50000, -1200, 8e6, 0), eitc = c(0, 0, 0, 3200))

out = validate_baseline_detail(detail, units, 2022L)
stopifnot(identical(out$id, units$id), identical(names(out), c('id', 'agi', 'eitc')),
          out$agi[1] == 8e6)
cat('ok   well-formed detail passes and is returned in the records\' order\n')

expect_stop(validate_baseline_detail(detail[-2, ], units, 2022L), 'id sets differ -- 1 records have no detail row',
            'a record without a detail row stops')
expect_stop(validate_baseline_detail(bind_rows(detail, tibble(id = 99, weight = 1, filer = 1L, agi = 1, eitc = 0)), units, 2022L),
            '1 detail rows have no record', 'a stray detail row stops')
expect_stop(validate_baseline_detail(detail %>% mutate(weight = weight / 0.1), units, 2022L),
            'not run at full sample', 'subsampled weights stop')
expect_stop(validate_baseline_detail(detail %>% mutate(agi = replace(agi, 2, NA)), units, 2022L),
            '1 NA agi', 'an NA agi stops')
expect_stop(validate_baseline_detail(detail %>% select(-eitc), units, 2022L), 'lacks eitc',
            'a missing column stops')
expect_stop(validate_baseline_detail(detail[0, ], units, 2022L), 'zero rows', 'an empty detail stops')

#------------------------------------------------------------------------------
# Part 2: the published baselines, where mounted
#------------------------------------------------------------------------------

root = file.path(yaml::read_yaml('./config/interfaces/output_roots.yaml')$production,
                 'model_data', 'Tax-Simulator', 'v1')
p1   = file.path(root, 'sw_base_2026092720_filer_refit_v2', 'baseline')
s33  = file.path(root, 'phase0_s33', 'baseline')
if (dir.exists(p1) && dir.exists(s33)) {
  stopifnot(check_baseline_pin(p1, '2026092720_filer_refit_v2') == '2026092720_filer_refit_v2')
  cat('ok   P1 baseline pins the vintage it was run on\n')
  expect_stop(check_baseline_pin(s33, '2026092720_filer_refit_v2'), 'was run on Tax-Data vintage 2026092220_s33_scratch',
              'a baseline run on another vintage is refused')
  d = read_baseline_detail(p1, 2023L)
  stopifnot(identical(names(d), BASELINE_DETAIL_COLS), nrow(d) > 3e5)
  cat('ok   P1 detail reads with the contract columns (', nrow(d), ' rows)\n', sep = '')
} else {
  cat('skip published-baseline checks (shared filer not mounted)\n')
}

cat('ALL PASSED\n')
