#---------------------------------------------------------------
# Tests for the state-weights data layer (src/nonfilers/state_weights.R)
#
# Defines functions only (this file is sourced by main.R's recursive
# walk; side effects at source time would break model startup).
#
# Synthetic frames only -- nothing here reads the IPUMS extract, which
# OOMs the login node. The extract-backed checks live in
# research/state_weights/nonfiler_residual/03_diagnose_current_nonfilers.R.
#
# Run manually (defining the file runs nothing; call the function):
#   module load R/4.4.2-gfbf-2024a
#   Rscript -e 'library(data.table); source("src/nonfilers/state_weights.R");
#               source("src/nonfilers/asec_tax_units.R");
#               source("src/tests/test_state_weights.R"); test_state_weights()'
#---------------------------------------------------------------


test_state_weights = function() {

  #----------------------------------------------------------------------------
  # Runs all state-weights data-layer tests, stopping on first failure.
  #
  # Returns: TRUE invisibly if all tests pass (throws otherwise).
  #----------------------------------------------------------------------------

  test_gq_classification()
  test_gq_differentiation()
  test_gq_guards()
  test_adult_x_vector()
  test_nonfiler_targets_count_adults()
  test_fit_gradient_panel()
  message('test_state_weights: ALL TESTS PASSED')
  invisible(TRUE)
}


#-------------------------------------------------------------------------------
# Synthetic ACS person frame.
#
# One California household plus five group-quarters singles, all survey year
# 2022. Deliberately covers every branch of classify_gq() and both sides of the
# dorm-student income test:
#
#   p1/p2  married couple 40/38, $60k joint      -> filer household unit
#   p3     their child, 10                       -> dependent
#   p4     prisoner 30, $0, GQ=3                 -> non-filer unit, RETAINED
#   p5     dorm student 20, $2k, GQ=4 SCHOOL=2   -> RECLASSIFIED out of the units
#   p6     dorm student 21, $30k, GQ=4 SCHOOL=2  -> above threshold, stays a FILER
#   p7     barracks 22, $40k, GQ=4 SCHOOL=1      -> income test, FILER
#   p8     nursing home 80, $9k, GQ=3            -> non-filer unit, RETAINED
#
# Every person carries PERWT = 100, so weighted counts are readable by eye.
#-------------------------------------------------------------------------------
synthetic_acs_frame = function() {

  person = function(pernum, age, inc, gq, school, marst = 6, sploc = 0,
                    momloc = 0, poploc = 0, serial = 1) {
    data.table::data.table(
      YEAR = 2022L, STATEFIP = 6L, PERWT = 100, PERNUM = pernum,
      SERIAL = serial, SAMPLE = 202201L, AGE = age, MARST = marst,
      SPLOC = sploc, MOMLOC = momloc, POPLOC = poploc, INCTOT = inc,
      GQ = gq, SCHOOL = school)
  }

  data.table::rbindlist(list(
    person(1, 40, 35000, 1, 1, marst = 1, sploc = 2),
    person(2, 38, 25000, 1, 1, marst = 1, sploc = 1),
    person(3, 10,     0, 1, 2, momloc = 2, poploc = 1),
    person(1, 30,     0, 3, 1, serial = 2),
    person(1, 20,  2000, 4, 2, serial = 3),
    person(1, 21, 30000, 4, 2, serial = 4),
    person(1, 22, 40000, 4, 1, serial = 5),
    person(1, 80,  9000, 3, 1, serial = 6)))
}


test_gq_classification = function() {

  #----------------------------------------------------------------------------
  # classify_gq() assigns the four types on the documented IPUMS codes.
  #
  # The dorm predicate (GQ==4 & in school & 18-24) is shared with the Stage D
  # T7 tabulation; on the TY2022 extract it reproduces F6 exactly -- 3.61M
  # institutional, 2.81M dorm students, 1.74M other. Verified 2026-08-24.
  #----------------------------------------------------------------------------

  got = classify_gq(GQ     = c(1L, 2L, 5L, 3L, 4L, 4L, 4L, 4L),
                    SCHOOL = c(1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L),
                    AGE    = c(30L, 30L, 30L, 30L, 20L, 30L, 20L, 17L))

  stopifnot(identical(got, c('household', 'household', 'household',
                             'institutional',
                             'dorm_student',            # in school, 18-24
                             'other_noninstitutional',  # in school but 30
                             'other_noninstitutional',  # 18-24 but not in school
                             'other_noninstitutional'))) # in school but 17

  message('  test_gq_classification: PASSED')
}


test_gq_differentiation = function() {

  #----------------------------------------------------------------------------
  # build_acs_margins() differentiated GQ treatment (task B1, decision D4).
  #
  # Four properties, each one a way the treatment could go wrong:
  #   1. state_pop is IDENTICAL across treatments. The anchors are PEP resident
  #      adults with no GQ subtraction (design memo 3.0), so reclassifying a
  #      dorm student must change who heads a unit and NOT how many people live
  #      in the state. This is the invariant the whole design rests on.
  #   2. Only the below-threshold dorm student leaves the non-filer margin.
  #   3. Institutional residents are retained as own-state non-filer units.
  #   4. Filer counts are untouched -- the reclassified population is, by
  #      construction, below the filing threshold.
  #----------------------------------------------------------------------------

  acs = synthetic_acs_frame()
  v0  = build_acs_margins(acs, 2022, gq_treatment = 'v0')
  dif = build_acs_margins(acs, 2022, gq_treatment = 'differentiated')

  # 1. the population invariant
  stopifnot(isTRUE(all.equal(v0$state_pop, dif$state_pop)),
            dif$state_pop$pop == 800)

  # v0 emits no GQ objects; differentiated does, and both label themselves
  stopifnot(is.null(v0$gq_composition), is.null(v0$gq_reclassified),
            v0$gq_treatment == 'v0', dif$gq_treatment == 'differentiated')

  # 2. exactly one student reclassified -- the $2k one, not the $30k one
  stopifnot(nrow(dif$gq_reclassified) == 1,
            dif$gq_reclassified$persons == 100,
            sum(v0$nonfiler_margins$n_units) -
              sum(dif$nonfiler_margins$n_units) == 100)

  # composition covers all three GQ types on the full frame
  comp = dif$gq_composition
  stopifnot(comp[gq_type == 'institutional',          sum(persons)] == 200,
            comp[gq_type == 'dorm_student',           sum(persons)] == 200,
            comp[gq_type == 'other_noninstitutional', sum(persons)] == 100)

  # 3. both institutional residents survive as non-filer units, in their own
  #    state and their own age bands
  nf = dif$nonfiler_margins
  stopifnot(nrow(nf) == 2,
            all(nf$state == 'CA'),
            setequal(as.character(nf$age_band), c('26_34', '75p')),
            sum(nf$n_units) == 200)

  # 4. filers unchanged: the household, the $30k student, the barracks resident
  stopifnot(sum(v0$filer_units$n_units) == sum(dif$filer_units$n_units),
            sum(dif$filer_units$n_units) == 300)

  message('  test_gq_differentiation: PASSED')
}


test_gq_guards = function() {

  #----------------------------------------------------------------------------
  # The two loud failures. Both exist because the silent version is worse:
  # margins built without GQ would look complete and quietly carry the v0
  # composition, and an unrecognized GQ code would be swept into 'household'.
  #----------------------------------------------------------------------------

  acs = synthetic_acs_frame()

  missing_cols = try(
    build_acs_margins(acs[, !c('GQ', 'SCHOOL')], 2022), silent = TRUE)
  stopifnot(inherits(missing_cols, 'try-error'),
            grepl('needs GQ, SCHOOL', missing_cols))

  bad_code = data.table::copy(acs)
  bad_code[1, GQ := 6L]
  unknown_gq = try(build_acs_margins(bad_code, 2022), silent = TRUE)
  stopifnot(inherits(unknown_gq, 'try-error'),
            grepl('unexpected IPUMS GQ code', unknown_gq))

  message('  test_gq_guards: PASSED')
}


test_adult_x_vector = function() {

  #----------------------------------------------------------------------------
  # The D5 adult x-vector: 1 + (filing_status == 2).
  #
  # Kept distinct from n_indiv, which is HT2's "individuals" concept and adds
  # dependents. Confusing the two would silently inflate every non-filer target
  # by the dependent count, so the test asserts they differ where dependents
  # exist.
  #----------------------------------------------------------------------------

  tu = data.table::data.table(
    filing_status = c(1L, 2L, 4L, 2L, 1L),
    n_dep         = c(0L, 0L, 2L, 3L, 1L))

  adults = puf_series_x(tu, 'n_adults')
  indiv  = puf_series_x(tu, 'n_indiv')

  stopifnot(
    # single/HoH count one adult, joint counts two -- dependents never counted
    identical(as.numeric(adults), c(1, 2, 1, 2, 1)),
    # n_indiv adds dependents, so the two agree ONLY with no dependents
    identical(as.numeric(indiv), c(1, 2, 3, 5, 2)),
    all(adults == indiv | tu$n_dep > 0),
    # MFS (status 3) is one adult, like single
    puf_series_x(data.table::data.table(filing_status = 3L, n_dep = 0L),
                 'n_adults') == 1)

  message('  test_adult_x_vector: PASSED')
}


test_nonfiler_targets_count_adults = function() {

  #----------------------------------------------------------------------------
  # build_weight_inputs() must target ADULTS on the non-filer partition: one
  # target per state (S37), whose values sum to the PUF's ADULT total and split
  # it by the anchor's shares -- not by the ACS prior's unit shares, which now
  # only seed P0.
  #
  # Runs on synthetic PUF records, margins and anchor, so no HT2, ACS or
  # anchor-file read.
  #----------------------------------------------------------------------------

  # Two non-filer records in one cell: one single, one joint => 3 adults, 2 units
  tu = data.table::data.table(
    id = 1:3, weight = c(100, 100, 100), filer = c(0L, 0L, 1L),
    filing_status = c(1L, 2L, 1L), n_dep = 0L, age1 = c(30L, 30L, 40L),
    agi = c(0, 0, 50000),
    wages = c(0,0,50000), txbl_int = 0, exempt_int = 0, div_ord = 0, div_pref = 0,
    kg_st = 0, kg_lt = 0, txbl_pens_dist = 0, txbl_ira_dist = 0, gross_ss = 0,
    sole_prop = 0, eitc = 0)

  # The ACS prior puts the cell 60/40 across two states in UNITS
  margins = list(nonfiler_margins = data.table::data.table(
    state       = c('CA', 'NY'),
    age_band    = factor('26_34', levels = AGE_BANDS),
    income_tier = factor('neg_zero',
                    levels = c('neg_zero','1_10k','10_25k','25_50k','50k_plus')),
    n_units     = c(60, 40),
    n_adults    = c(120, 40)))

  ht2 = data.table::data.table(
    state = rep(c('CA','NY'), each = 10), agi_stub = rep(1:10, 2),
    variable = 'n_returns', value = 1000, year = 2022L)

  # ... and the anchor 3:1 in ADULTS
  anchor = data.table::data.table(state = c('CA', 'NY'), anchor = c(300, 100))

  inp = build_weight_inputs(tu, 2022, ht2 = ht2, acs_margins = margins,
                            nonfiler_anchor = anchor, verbose = FALSE)
  tn = inp$nonfilers$targets
  stopifnot(length(tn) == 2,
            all(vapply(tn, `[[`, character(1), 'series') == 'n_adults_state'))

  # the adult x-vector reached the target, per record
  stopifnot(identical(as.numeric(tn[[1]]$x), c(1, 2)))

  # PUF adults = 100*1 + 100*2 = 300, split 3:1 by the anchor -> 225 / 75.
  # A unit-denominated target would split 200 units; the prior's 60/40 would
  # give 180 / 120. Both are distinguishable from this.
  got = vapply(tn, `[[`, numeric(1), 'target')
  names(got) = vapply(tn, `[[`, character(1), 'state_code')
  stopifnot(isTRUE(all.equal(got[c('CA', 'NY')], c(CA = 225, NY = 75))))

  # the prior is still the ACS unit shares
  ca = match('CA', inp$jurisdictions)
  stopifnot(isTRUE(all.equal(inp$nonfilers$P0[, ca], c(0.6, 0.6))))

  message('  test_nonfiler_targets_count_adults: PASSED')
}


test_fit_gradient_panel = function() {

  #----------------------------------------------------------------------------
  # fit_gradient_panel() (Phase 3 arm C) on a two-year synthetic panel:
  #   1. gamma = 0 is the two independent fit_gradient() runs, bit for bit
  #   2. at convergence the finite-difference gradient of the full objective,
  #      written out independently here, is ~0 -- a wrong analytic gradient
  #      would converge somewhere else
  #   3. a large gamma pins the persistent records' splits across years
  #----------------------------------------------------------------------------

  S = 3
  mk_year = function(w, ids, tg) {
    N = length(w)
    list(w = w, ids = ids, P0 = matrix(1 / S, N, S),
         targets = lapply(seq_along(tg), function(s) list(
           rows = seq_len(N), state = s, x = rep(1, N), target = tg[s], lambda = 1)))
  }
  # records 2-4 persist; 1 leaves after year 1, 5 enters in year 2
  years = list(mk_year(c(10, 20, 5, 8), c(1, 2, 3, 4), c(20, 15, 8)),
               mk_year(c(22, 6, 9, 12), c(2, 3, 4, 5), c(10, 25, 14)))

  # 1. gamma = 0 == independent fits
  p0 = fit_gradient_panel(years, gamma = 0, beta = 1e-2, lr = 0.1, n_steps = 300)
  for (y in 1:2) {
    ind = fit_gradient(years[[y]]$w, years[[y]]$P0, years[[y]]$targets,
                       beta = 1e-2, lr = 0.1, n_steps = 300)
    stopifnot(identical(p0$theta[[y]], ind$theta))
  }

  # 2. stationarity of the full objective at convergence
  gamma = 0.05; beta = 1e-2
  fit = fit_gradient_panel(years, gamma = gamma, beta = beta, lr = 0.05,
                           n_steps = 20000, lr_schedule = 'cosine')
  objective = function(th) {
    P = lapply(th, function(z) { e = exp(z - apply(z, 1, max)); e / rowSums(e) })
    f = 0
    for (y in 1:2) {
      yr = years[[y]]
      for (t in yr$targets) f = f + ((sum(yr$w * P[[y]][, t$state]) - t$target) / t$target)^2
      f = f + beta * sum(P[[y]] * log(P[[y]] / yr$P0))
    }
    cur = match(c(2, 3, 4), years[[2]]$ids); prev = match(c(2, 3, 4), years[[1]]$ids)
    f + gamma * sum(P[[2]][cur, ] * log(P[[2]][cur, ] / P[[1]][prev, ]))
  }
  h = 1e-6; gmax = 0
  for (y in 1:2) for (i in seq_len(nrow(fit$theta[[y]]))) for (s in 1:S) {
    up = fit$theta; dn = fit$theta
    up[[y]][i, s] = up[[y]][i, s] + h; dn[[y]][i, s] = dn[[y]][i, s] - h
    gmax = max(gmax, abs((objective(up) - objective(dn)) / (2 * h)))
  }
  stopifnot(gmax < 1e-4)

  # 3. large gamma: persistent records hold their split
  big = fit_gradient_panel(years, gamma = 50, beta = beta, lr = 0.05,
                           n_steps = 5000, lr_schedule = 'cosine')
  d0 = max(abs(p0$P[[2]][1:3, ] - p0$P[[1]][2:4, ]))
  d1 = max(abs(big$P[[2]][1:3, ] - big$P[[1]][2:4, ]))
  stopifnot(d1 < 0.1 * d0)

  message(sprintf('  test_fit_gradient_panel: PASSED (stationarity %.1e; split drift %.3f -> %.4f)',
                  gmax, d0, d1))
}
