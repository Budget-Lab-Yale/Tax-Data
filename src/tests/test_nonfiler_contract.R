#------------------------------------------------------------------------------
# test_nonfiler_contract.R
#
# The producer/consumer contract test for the ASEC-Nonfilers interface.
#
# Exercises src/nonfiler_contract.R -- the same function impute_nonfilers.R
# calls -- against a fixture pool. Needs no cluster mounts, no PUF, and no
# published artifact, so it runs anywhere in seconds.
#
# The fixture deliberately contains the cases that broke or nearly broke this
# interface: a joint unit, dependents, positive AND negative business income,
# retirement income, Social Security, dividends, and a group-quarters record.
#
#   Rscript src/tests/test_nonfiler_contract.R
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse); library(magrittr)
})

source('./src/nonfiler_contract.R')

#------------------------------------------------------------------------------
# Fixture: a well-formed pool, in the producer's emitted schema
#------------------------------------------------------------------------------

make_fixture = function() {
  base = tibble(
    id            = 1e6 + 1:6,
    tax_year      = 2017L,
    weight        = c(1.5, 220.7, 90.25, 3300.1, 12.75, 640.5),
    filer         = 0L,
    dep_status    = 0L,
    #                single joint  hoh    single single single(GQ)
    filing_status = c(1L,    2L,   4L,    1L,    1L,    1L),
    GENDER        = c(1L,    1L,   2L,    2L,    1L,    2L),
    male1         = c(1L,    1L,   0L,    0L,    1L,    0L),
    male2         = c(NA,    0L,   NA,    NA,    NA,    NA),
    age1          = c(23L,   41L,  37L,   68L,   55L,   81L),
    age2          = c(NA,    39L,  NA,    NA,    NA,    NA),
    age_group     = c(1L,    3L,   3L,    6L,    5L,    6L),
    EARNSPLIT     = NA_integer_,
    n_dep          = c(0L,   2L,   1L,    0L,    0L,    0L),
    dep_age_group1 = c(NA,   1L,   3L,    NA,    NA,    NA),
    dep_age_group2 = c(NA,   3L,   NA,    NA,    NA,    NA),
    dep_age_group3 = NA_integer_,
    n_dep_ctc      = c(0L,   2L,   1L,    0L,    0L,    0L),
    n_dep_eitc     = c(0L,   2L,   1L,    0L,    0L,    0L),
    wages          = c(4200, 18500, 9100, 0,     2400,  0),
    wages1         = c(4200, 11000, 9100, 0,     2400,  0),
    wages2         = c(0,    7500,  0,    0,     0,     0),
    txbl_int       = c(12,   340,   0,    1850,  75,    420),
    div_pref       = c(0,    260,   0,    2100,  0,     180),
    # positive AND negative business income: a loss must survive the contract
    sole_prop      = c(0,    5200, -3400, 0,     -900,  0),
    sole_prop1     = c(0,    5200, -3400, 0,     -900,  0),
    sole_prop2     = 0,
    kg_lt          = 0,
    gross_pens_dist = c(0,   0,     0,    14200, 3100,  0),
    txbl_pens_dist  = c(0,   0,     0,    14200, 3100,  0),
    rent           = c(0,    0,     0,    600,   0,     0),
    rent_loss      = c(0,    0,     0,    0,     250,   0),
    ui             = c(0,    1200,  0,    0,     0,     0),
    gross_ss       = c(0,    0,     0,    19800, 0,     11400),
    other_inc      = c(0,    0,     450,  0,     0,     0),
    source         = c(rep('asec_household', 5), 'acs_gq_institutional')
  )
  base
}


#------------------------------------------------------------------------------
# The PUF's column set AS IT EXISTS when impute_nonfilers.R runs.
#
# Stated explicitly rather than derived from the variable guide. The guide
# cannot answer this question: presence depends on WHEN a column is created,
# not on its metadata. `weight`, `wages1` and `wages2` are all
# `source = imputed, name_puf = NA` -- the same metadata as age1/male1 -- yet
# they exist here because process_puf.R / create_2017_puf.R / reweight.R build
# them before the append, while imputations/ages.R (age1, age2) and
# imputations/demographics.R (male1, male2) run after it.
#
# So the two invariants the contract depends on are asserted directly below,
# and the rest of this list is a fixture: enough real column names to exercise
# conforming and zero-filling.
#------------------------------------------------------------------------------

allowed_extras = c(NONFILER_DROPPED_IMPUTED_VARS, NONFILER_PROVENANCE_VARS)

# Everything the fixture supplies that the PUF genuinely has at append time...
puf_cols = c(
  setdiff(names(make_fixture()), allowed_extras),
  # ...plus real PUF columns the pool does NOT supply, which must be zero-filled
  'div_ord', 'kg_st', 'part_active', 'part_passive', 'scorp', 'farm',
  'char_cash', 'txbl_ira_dist', 'blind1', 'blind2'
) %>% unique()

# The invariant that made the old assertion pair unsatisfiable: these columns
# are NOT on the PUF when the append runs, so a contract that both requires and
# forbids them cannot be satisfied.
stopifnot(!any(c('age1', 'age2', 'male1', 'male2') %in% puf_cols))

# The invariant a producer-side rename breaks: these must be present, or the
# pool's economic content is silently zero-filled.
stopifnot(all(c('div_pref', 'wages', 'txbl_int', 'gross_ss') %in% puf_cols))

puf_ids = 1:500


#------------------------------------------------------------------------------
# Harness
#------------------------------------------------------------------------------

passed = 0L
failed = 0L

expect_ok = function(desc, expr) {
  res = tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
  if (is.null(res)) {
    cat('  ok   ', desc, '\n', sep = '')
    passed <<- passed + 1L
  } else {
    cat('  FAIL ', desc, '\n         unexpected error: ', res, '\n', sep = '')
    failed <<- failed + 1L
  }
}

expect_stop = function(desc, expr, pattern) {
  res = tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
  if (is.null(res)) {
    cat('  FAIL ', desc, '\n         expected an error, got none\n', sep = '')
    failed <<- failed + 1L
  } else if (!grepl(pattern, res, fixed = TRUE)) {
    cat('  FAIL ', desc, '\n         error did not mention "', pattern,
        '": ', res, '\n', sep = '')
    failed <<- failed + 1L
  } else {
    cat('  ok   ', desc, '\n', sep = '')
    passed <<- passed + 1L
  }
}

validate = function(pool) {
  validate_nonfiler_pool(pool, puf_cols = puf_cols, puf_ids = puf_ids,
                         label = 'fixture')
}


#------------------------------------------------------------------------------
# Positive case
#------------------------------------------------------------------------------

cat('\nA well-formed pool\n')

expect_ok('the fixture passes the contract', validate(make_fixture()))

out = validate(make_fixture())

expect_ok('output carries exactly the PUF columns, in order',
          stopifnot(identical(colnames(out), puf_cols)))

expect_ok('allowed extras are dropped',
          stopifnot(!any(c('age1', 'age2', 'male1', 'male2', 'source',
                           'tax_year') %in% colnames(out))))

expect_ok('row count is preserved',
          stopifnot(nrow(out) == 6))

expect_ok('dividend mass survives as div_pref, not zero-filled',
          stopifnot(abs(sum(out$weight * out$div_pref) -
                        sum(make_fixture()$weight * make_fixture()$div_pref))
                    < 1e-8,
                    sum(out$weight * out$div_pref) > 0))

expect_ok('a business LOSS survives with its sign',
          stopifnot(any(out$sole_prop < 0),
                    min(out$sole_prop) == -3400))

expect_ok('Social Security and pension mass survive',
          stopifnot(sum(out$weight * out$gross_ss) > 0,
                    sum(out$weight * out$txbl_pens_dist) > 0))

expect_ok('observed age_group is unchanged',
          stopifnot(identical(out$age_group, make_fixture()$age_group)))

expect_ok('observed GENDER is unchanged',
          stopifnot(identical(out$GENDER, make_fixture()$GENDER)))


#------------------------------------------------------------------------------
# Negative cases -- each is a real failure this interface has hit
#------------------------------------------------------------------------------

cat('\nViolations the contract must reject\n')

# The bug that made this branch unrunnable: the producer wrote the DINA-era
# name, which is not a Tax-Data variable.
expect_stop('a renamed economic column (qual_div for div_pref)',
            validate(make_fixture() %>% rename(qual_div = div_pref)),
            'qual_div')

expect_stop('a required economic column simply absent',
            validate(make_fixture() %>% select(-div_pref)),
            'silently zero-filled')

# The failure mode that made a misconfigured interface a silent no-op.
expect_stop('an empty pool',
            validate(make_fixture() %>% filter(FALSE)),
            'zero rows')

expect_stop('an unknown extra column',
            validate(make_fixture() %>% mutate(experimental_var = 1)),
            'experimental_var')

expect_stop('a required column absent',
            validate(make_fixture() %>% select(-age_group)),
            'required columns absent')

expect_stop('a filer record in a non-filer file',
            validate(make_fixture() %>% mutate(filer = c(0L, 1L, rep(0L, 4)))),
            'filer')

expect_stop('an id colliding with the PUF',
            validate(make_fixture() %>% mutate(id = c(42L, id[-1]))),
            'id')

expect_stop('a duplicated id',
            validate(make_fixture() %>% mutate(id = c(1e6 + 1, 1e6 + 1,
                                                      id[3:6]))),
            'duplicated')

expect_stop('a zero weight',
            validate(make_fixture() %>% mutate(weight = c(0, weight[-1]))),
            'weight')

expect_stop('an NA in observed GENDER',
            validate(make_fixture() %>%
                       mutate(GENDER = c(NA_integer_, GENDER[-1]))),
            'GENDER')

expect_stop('an out-of-range age_group',
            validate(make_fixture() %>%
                       mutate(age_group = c(9L, age_group[-1]))),
            'age_group')


#------------------------------------------------------------------------------

cat(sprintf('\n%d passed, %d failed\n', passed, failed))
if (failed > 0) quit(status = 1)
cat('PASS\n')
