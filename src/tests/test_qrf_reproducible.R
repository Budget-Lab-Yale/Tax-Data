#---------------------------------------------------------------------------
# Unit test: forked quantregForest fits must be reproducible (S31)
#
# quantregForest splits training across `nthreads` workers with
# parallel::mclapply. Those forks do not inherit the default Mersenne-Twister
# stream, so a plain set.seed() does NOT make the fit reproducible -- two
# from-scratch rebuilds of the same commit disagreed on every
# quantregForest-imputed column. L'Ecuyer-CMRG is the RNG mclapply can split
# deterministically, so with_model_seed(parallel = TRUE) restores
# reproducibility without giving up the parallel speedup.
#
# This test fails if a quantregForest fit loses `parallel = TRUE`.
#
# Run with:  Rscript src/tests/test_qrf_reproducible.R
#---------------------------------------------------------------------------

suppressPackageStartupMessages(library(quantregForest))
source('src/imputations/rng.R')

n_passed = 0
n_failed = 0
assert = function(desc, condition) {
  if (isTRUE(condition)) { cat(sprintf('  PASS: %s\n', desc)); n_passed <<- n_passed + 1 }
  else                   { cat(sprintf('  FAIL: %s\n', desc)); n_failed <<- n_failed + 1 }
}

set.seed(42)
N = 2000
x = data.frame(wages = runif(N, 0, 2e5), married = rbinom(N, 1, 0.5), age = runif(N, 18, 80))
y = x$wages / 1e5 + rnorm(N)
w = runif(N, 1, 5000)
NTHREADS = 2   # forked, but light enough to run anywhere

fit = function(parallel) {
  f = with_model_seed('test_qrf', parallel = parallel,
                      expr = quantregForest(x = x, y = y, nthreads = NTHREADS,
                                            weights = w, mtry = 2, nodesize = 5))
  predict(f, newdata = x[1:150, ], what = c(0.1, 0.5, 0.9))
}

cat('\nTest: a forked fit is reproducible under parallel = TRUE\n')
assert('two forked fits are identical',
       isTRUE(all.equal(fit(TRUE), fit(TRUE), tolerance = 0)))

cat('\nTest: the default stream does NOT survive a fork (this is why the switch exists)\n')
assert('two forked fits under parallel = FALSE differ',
       !isTRUE(all.equal(fit(FALSE), fit(FALSE), tolerance = 0)))

cat('\nTest: with_model_seed leaves the global stream and RNG kind as it found them\n')
RNGkind('Mersenne-Twister'); set.seed(1); before_kind = RNGkind(); before_seed = .Random.seed
invisible(fit(TRUE))
assert('RNG kind restored',   identical(RNGkind(), before_kind))
assert('random stream restored', identical(.Random.seed, before_seed))

cat('\nTest: every quantregForest fit in src/ is marked as forking\n')
src  = c(readLines('src/imputations/helpers.R'), readLines('src/imputations/mortgage.R'))
code = src[!grepl('^\\s*#', src)]                       # comments discuss these names
fits = grep('quantregForest\\(', code, value = TRUE)
assert(sprintf('found %d quantregForest fit call(s)', length(fits)), length(fits) >= 2)
assert('every fit is wrapped with parallel = TRUE',
       length(grep('parallel\\s*=\\s*TRUE', code)) >= length(fits))
assert('no fit hardcodes nthreads = 1 (that was the slow fix, superseded)',
       !any(grepl('nthreads\\s*=\\s*1\\b', code)))

cat(sprintf('\n%d passed, %d failed\n', n_passed, n_failed))
if (n_failed > 0) stop(sprintf('%d test(s) failed', n_failed))
