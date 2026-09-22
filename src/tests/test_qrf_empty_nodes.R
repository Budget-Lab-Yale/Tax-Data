#---------------------------------------------------------------------------
# Unit test: predict_qrf_draw_by_id() never returns NA (S33)
#
# On this project's data quantregForest leaves `valuesNodes` NA at a share of
# a regression forest's terminal nodes, so an id-keyed draw that lands in one
# returned NA. auto_loan.R let that NA reach AGI and every federal total came
# out NA; childcare, overtime, tips and mortgage masked it with replace_na /
# if_else, silently turning an in-universe imputation into $0 or 100%.
#
# Synthetic data does not produce empty nodes on its own, so this test INJECTS
# them into a small forest's valuesNodes and checks the draw survives.
#
# Run with:  Rscript src/tests/test_qrf_empty_nodes.R
#---------------------------------------------------------------------------

suppressPackageStartupMessages(library(quantregForest))
source('src/imputations/rng.R')

n_passed = 0
n_failed = 0
assert = function(desc, condition) {
  if (isTRUE(condition)) { cat(sprintf('  PASS: %s\n', desc)); n_passed <<- n_passed + 1 }
  else                   { cat(sprintf('  FAIL: %s\n', desc)); n_failed <<- n_failed + 1 }
}

set.seed(11)
N = 1500
x = data.frame(a = runif(N), b = sample(0:3, N, TRUE))
y = x$a * 10 + rnorm(N)
set.seed(12)
m = quantregForest(x = x, y = y, nthreads = 1, mtry = 2, nodesize = 5)

newdata = x[1:600, ]
ids     = sample(1:5e6, 600)
STREAM  = 'auto_quantile'

# Empty ~20% of terminal nodes, as the production mortgage forest has ~28%
term = which(m$forest$nodestatus == -1)
set.seed(13)
m$valuesNodes[sample(term, floor(0.2 * length(term)))] = NA

cat('\nTest: a forest with empty terminal nodes still yields a value for every record\n')
ens = predict(m, newdata = newdata, what = function(z) z)
raw = ens[cbind(seq_along(ids), column_by_id(ids, ncol(ens), STREAM))]
assert(sprintf('the injected forest really does hand back NA first choices (%d)', sum(is.na(raw))),
       sum(is.na(raw)) > 0)
after = predict_qrf_draw_by_id(m, newdata, ids, STREAM)
assert('predict_qrf_draw_by_id returns no NA', !anyNA(after))

cat('\nTest: records whose first choice was valid keep exactly that value\n')
ok = !is.na(raw)
assert('every valid first choice is unchanged', identical(after[ok], raw[ok]))

cat('\nTest: the re-draw is keyed by id, so it is deterministic\n')
assert('same ids -> identical draws', identical(after, predict_qrf_draw_by_id(m, newdata, ids, STREAM)))

cat('\nTest: filled values come from the record\'s own non-empty ensemble members\n')
filled_ok = vapply(which(!ok), function(i) after[i] %in% ens[i, !is.na(ens[i, ])], logical(1))
assert('each filled value is one of that record\'s non-empty members', all(filled_ok))

cat('\nTest: a record with EVERY member empty fails loudly rather than returning NA\n')
m2 = m; m2$valuesNodes[] = NA
err = tryCatch({ predict_qrf_draw_by_id(m2, newdata[1:3, ], ids[1:3], STREAM); 'no error' },
               error = function(e) conditionMessage(e))
assert('stops with a named error', grepl('every ensemble member is empty', err))

cat(sprintf('\n%d passed, %d failed\n', n_passed, n_failed))
if (n_failed > 0) stop(sprintf('%d test(s) failed', n_failed))
