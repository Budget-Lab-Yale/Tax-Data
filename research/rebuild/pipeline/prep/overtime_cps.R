#-------------------------------------------------------------------------------
# Each CPS ASEC person's overtime pay in the tax year, by Ernie Tedeschi's rules
# (Reports/2024/Overtime/src/overtime_PUBLIC.do, The Budget Lab, 2024), with each
# tax year's salary thresholds. build_cps.R sources this file and calls
# cps_overtime(yr, x_keys), which returns one row per person.
#
# The method in brief. The Fair Labor Standards Act requires time and a half for
# hours over 40 a week, for workers it covers and does not exempt. A worker is
# not covered in some sectors and classes of work, and is exempt in some named
# occupations and industries. A salaried worker paid at least the salary
# threshold is also exempt if their duties are executive, administrative or
# professional (the "duties test"). The CPS does not ask about duties. Each such
# worker passes the test with a chance that depends on their occupation's class
# in the Labor Department's table and on their pay, and one random draw per
# person decides. Overtime pay is then time and a half for the hours over
# 40, times the weeks worked, and at most the person's wages.
#
# Pay and hours are asked only of households in their 4th and 8th month in the
# survey. Everyone else's overtime is missing (NA), not zero. Occupations are
# read on IPUMS's harmonized 2010 codes (OCC2010) in every year. FEATURES.md,
# "The CPS", has the method.
#-------------------------------------------------------------------------------

OT_VARS = c('SERIAL', 'PERNUM', 'MISH', 'EARNWT', 'ELIGORG', 'PAIDHOUR', 'EARNWEEK2', 'HOURWAGE2', 'UHRSWORK1',
            'AHRSWORKT', 'OCC2010', 'IND', 'CLASSWKR', 'EMPSTAT', 'STATEFIP', 'CBSASZ', 'WKSWORK1', 'INCWAGE', 'GQ')
# The industry codes the rules name; build_cps.R prints how many people have each, to show the codes exist
OT_IND_NAMED = c(170, 180, 280, 290, 570, 4670, 4680, 6090, 6170, 6370, 6770, 7860, 7870, 8170, 8370, 9290)

# Reads the variables vars of one IPUMS CPS sample, without IPUMS's labels.
read_ot_sample = function(dir, sample, vars) {
  dd = file.path(dir, sample)
  ddi = ipumsr::read_ipums_ddi(list.files(dd, '\\.xml$', full.names = TRUE))
  x = as.data.table(ipumsr::read_ipums_micro(ddi, vars = tidyselect::any_of(vars), verbose = FALSE))
  x[, lapply(.SD, function(v) { attributes(v) <- NULL; v })]
}

# The March basic monthly file's answers for the people of one ASEC year: weekly
# pay, hourly wage, and hours last week at the main job. Returns NULL when the
# settings name no basic file or it lacks the year. People are linked by
# MARBASECIDP, IPUMS's person link between the ASEC and the March basic file, or
# by CPSIDP where a sample lacks it (the 2026 basic sample does). Returns
# SERIAL, PERNUM and the basic file's b_week, b_hour and b_lw1. A duplicated
# link stops the run.
march_basic = function(yr) {
  root = settings$paths$raw$cps_march_basic
  if (is.null(root)) return(NULL)
  sb = sprintf('cps%d_03b', yr + 1); sa = sprintf('cps%d_03s', yr + 1)
  if (!dir.exists(file.path(root, sb)) || !dir.exists(file.path(root, sa))) return(NULL)
  a = read_ot_sample(root, sa, c('SERIAL', 'PERNUM', 'MARBASECIDP', 'CPSIDP'))
  b = read_ot_sample(root, sb, c('MARBASECIDP', 'CPSIDP', 'EARNWEEK2', 'HOURWAGE2', 'AHRSWORK1'))
  stopifnot(!anyNA(a), !anyNA(b), 'AHRSWORK1' %in% names(b))
  key = if ('MARBASECIDP' %in% names(a) && 'MARBASECIDP' %in% names(b)) 'MARBASECIDP' else 'CPSIDP'
  a = a[get(key) > 0]; b = b[get(key) > 0]
  stopifnot(!anyDuplicated(b[[key]]), !anyDuplicated(a[[key]]))
  b = b[, .(lk = get(key), b_week = EARNWEEK2, b_hour = HOURWAGE2, b_lw1 = AHRSWORK1)]
  m = merge(a[, .(SERIAL, PERNUM, lk = get(key))], b, by = 'lk')
  cat(sprintf('  March basic %d: %d ASEC people linked by %s\n', yr + 1, nrow(m), key))
  m[, .(SERIAL, PERNUM, b_week, b_hour, b_lw1)]
}

# Each person's overtime pay for tax year yr, with the steps' intermediate
# columns. x_keys holds the SERIAL and PERNUM of the common extract build_cps.R
# reads; the earnings extract must have the same people.
cps_overtime = function(yr, x_keys) {
  # 1. Read the year's settings and the earnings extract, and check it has the same people as the common extract.
  #    People in group quarters are left out.
  OT = settings$cps$overtime
  thr = as.numeric(OT$salary_threshold[[as.character(yr)]])
  hce = as.numeric(OT$hce_annual[[as.character(yr)]])
  if (!length(thr) || !length(hce)) stop(sprintf('settings cps.overtime has no salary threshold or HCE line for %d', yr))
  comp_hourly = as.numeric(OT$computer_hourly)
  x = read_ot_sample(settings$paths$raw$cps_earnings, sprintf('cps%d_03s', yr + 1), OT_VARS)
  miss = setdiff(OT_VARS, names(x))
  if (length(miss)) stop(sprintf('the earnings extract for tax year %d lacks %s', yr, paste(miss, collapse = ', ')))
  stopifnot(!anyNA(x))
  setkey(x, SERIAL, PERNUM)
  k = x_keys[order(SERIAL, PERNUM), .(SERIAL, PERNUM)]
  if (nrow(k) != nrow(x) || !all(k$SERIAL == x$SERIAL) || !all(k$PERNUM == x$PERNUM))
    stop(sprintf('tax year %d: the earnings extract and the common extract have different records', yr))
  x = x[GQ != 2]

  # 2. Who is asked pay and hours: the earnings group (EARNWT > 0), in a month-in-sample group whose pay is in this
  #    year's file. ELIGORG is also 1 on the ASEC's extra sample, which is never asked (EARNWT 0). A month group in
  #    which fewer than half of the workers asked have any pay has lost its pay in this file. Its pay is taken from
  #    the March basic file where that file links; otherwise its overtime is missing.
  x[, grp := EARNWT > 0]
  stopifnot(all(x[grp == TRUE, MISH] %in% c(4, 8)))
  x[, has_pay := EARNWEEK2 > 0 | (HOURWAGE2 > 0 & HOURWAGE2 < 999)]
  gs = x[grp & ELIGORG == 1, .(workers = .N, with_pay = mean(has_pay)), by = MISH][order(MISH)]
  cat('  earnings group, workers asked their pay, share with pay, by month in sample:\n'); print(gs)
  lost = gs[with_pay < 0.5, MISH]
  x[, `:=`(basic_pay = FALSE, b_lw1 = NA_real_)]
  bp = march_basic(yr)
  if (is.null(bp)) {
    cat('  no March basic file for this year: hours last week at all jobs (AHRSWORKT)\n')
    if (length(lost)) cat(sprintf('  month group(s) %s without pay in this file: overtime missing there\n', paste(lost, collapse = ', ')))
  } else {
    x[bp, on = c('SERIAL', 'PERNUM'), `:=`(b_week = i.b_week, b_hour = i.b_hour, b_lw1 = as.numeric(i.b_lw1))]
    linked = x[grp & ELIGORG == 1, mean(!is.na(b_lw1))]
    cat(sprintf('  workers asked their pay linked to the basic file: %.2f%%\n', 100 * linked))
    if (linked < 0.99) stop(sprintf('tax year %d: only %.1f%% of workers asked their pay link to the March basic file', yr, 100 * linked))
    if (length(lost)) {
      fix = x$MISH %in% lost & x$ELIGORG == 1 & !x$has_pay & !is.na(x$b_week) &
        (x$b_week > 0 | (x$b_hour > 0 & x$b_hour < 999))
      x[fix, `:=`(EARNWEEK2 = b_week, HOURWAGE2 = b_hour, has_pay = TRUE, basic_pay = TRUE)]
      cat(sprintf('  pay taken from the March basic file for %d workers of month group(s) %s\n', sum(fix),
                  paste(lost, collapse = ', ')))
      lost = x[grp & ELIGORG == 1, .(with_pay = mean(has_pay)), by = MISH][with_pay < 0.5, MISH]
    }
    x[, c('b_week', 'b_hour') := NULL]
  }
  x[, asked := grp & !(MISH %in% lost)]

  # 3. Who the Act does not cover: federal workers, the armed forces, the self-employed, unpaid family workers and
  #    clergy. Three groups of federal workers are covered again: the postal service, federal workers in electric
  #    power in the Tennessee Valley Authority's states, and the Library of Congress.
  emp = x$EMPSTAT %in% c(10, 12)
  covered_cow = x$CLASSWKR %in% c(21, 22, 23, 27, 28)
  x[, inelig := emp & (!covered_cow | OCC2010 %in% c(2040, 2050, 2060))]
  x[emp & (IND == 6370 | (CLASSWKR == 25 & IND == 570 & STATEFIP %in% c(21, 47, 28, 1, 13, 37, 51)) |
             (CLASSWKR == 25 & IND == 6770 & STATEFIP == 11)), inelig := FALSE]
  # 4. Pay and hours. Weekly pay, usual weekly hours, and hours last week. Hours last week are at the main job from
  #    the basic file (Tedeschi's hourslw1) where linked, and otherwise at all jobs. The hourly wage is the reported
  #    one for hourly workers, and weekly pay over hours for the others.
  x[, hourly := PAIDHOUR == 2]
  x[, weekpay := fifelse(EARNWEEK2 > 0, as.numeric(EARNWEEK2), NA_real_)]
  usual = fifelse(x$UHRSWORK1 > 0 & x$UHRSWORK1 < 997, as.numeric(x$UHRSWORK1), NA_real_)
  lw = fifelse(!is.na(x$b_lw1), x$b_lw1, as.numeric(x$AHRSWORKT))
  lastwk = fifelse(lw > 0 & lw < 999, lw, NA_real_)
  x[, `:=`(usual = usual, lastwk = lastwk)]
  x[, finalhours := fifelse(!is.na(lastwk), lastwk, usual)]
  denom = fifelse(!is.na(usual), usual, lastwk)
  x[, wage := fifelse(hourly & HOURWAGE2 > 0 & HOURWAGE2 < 999, as.numeric(HOURWAGE2), weekpay / denom)]
  # 5. Who is exempt by occupation or industry: physicians, lawyers, teachers and education administrators in
  #    schools and colleges, outside sales, agriculture, fishermen, media workers outside metro areas, companions,
  #    computer workers paid at least the hourly line or the salary threshold, motor carrier, rail and sea workers,
  #    and salesmen and mechanics at vehicle dealers.
  o = x$OCC2010; i = x$IND
  comp = o %in% c(110, 1005, 1006, 1007, 1010, 1020, 1030, 1050, 1060, 1105, 1106, 1107, 5800, 7900)
  x[, exempt := (o %in% c(3010, 3040, 3060, 3120)) | (o == 2100) |
        (o >= 2200 & o <= 2550 & i %in% c(7860, 7870)) | (o %in% c(2000, 230) & i %in% c(7860, 7870)) |
        (o == 4950) | (i %in% c(170, 180, 290)) | (i == 280 & o %in% c(6100, 9310)) |
        (o %in% c(2810, 2830, 2840, 2850, 2860) & CBSASZ == 0) |
        (o %in% c(3600, 4610) & i %in% c(8170, 8370, 9290)) |
        (comp & ((!is.na(wage) & wage >= comp_hourly) | (!is.na(weekpay) & weekpay >= thr))) |
        (i == 6170 & o %in% c(7110, 7200, 7210, 7220, 9130)) | (o %in% c(9200, 9230, 9240, 9260)) |
        (i == 6090 & o %in% c(9300, 9310, 9570)) |
        (o %in% c(4750, 4760, 7110, 7150, 7160, 7200, 7210, 7220, 7240, 7260) & i %in% c(4670, 4680))]

  # 6. Each occupation's duties class (0 to 4) in the Labor Department's table, which uses the 2002 and 2010
  #    occupation codes. An occupation not in the table takes the class of the nearest lower code.
  eap = as.data.table(haven::read_dta(settings$paths$raw$eap_codes))
  eap = eap[, lapply(.SD, as.numeric)][order(occ18)]
  stopifnot(!anyNA(eap), all(eap$eap_code %in% 0:4))
  idx = findInterval(x$OCC2010, eap$occ18)
  x[, eap_code := fifelse(idx > 0, eap$eap_code[pmax(idx, 1)], NA_real_)]
  x[, eap_exact := OCC2010 %in% eap$occ18]
  wk = x[asked & ELIGORG == 1 & emp]
  cat(sprintf('  duties classes: %.1f%% of asked workers (EARNWT) have an occupation in DOL\'s table, %.1f%% take the nearest lower code\'s class\n',
              100 * wk[, sum(EARNWT * eap_exact) / sum(EARNWT)], 100 * wk[, sum(EARNWT * !eap_exact) / sum(EARNWT)]))

  # 7. The chance of passing the duties test, for salaried workers paid above the threshold (Tedeschi's gamma
  #    step). Within each class, a gamma distribution is fitted to weekly pay by its weighted mean and variance, and
  #    a worker's chance starts as the share of that distribution below their pay. The chances are then rescaled to
  #    the class's range (class 1 from 0.9 to 1.0, class 2 from 0.5 to 0.9, class 3 from 0.1 to 0.5, class 4 from 0
  #    to 0.1). Highly compensated workers, with weekly pay at least the yearly HCE line / 52, pass with a fixed
  #    chance by class. Class 0 never passes.
  x[, eap_prob := NA_real_]
  x[, sal_above := asked & ELIGORG == 1 & !hourly & !is.na(weekpay) & weekpay > thr]
  for (cl in 1:4) {
    fit = x[sal_above & eap_code == cl]
    if (!nrow(fit)) next
    m = sum(fit$EARNWT * fit$weekpay) / sum(fit$EARNWT)
    v = sum(fit$EARNWT * (fit$weekpay - m)^2) / (sum(fit$EARNWT) - 1)
    x[sal_above & eap_code == cl & !inelig & !exempt, eap_prob := pgamma(weekpay, shape = m^2 / v, scale = v / m)]
  }
  ranges = list(`1` = c(0.9, 0.1), `2` = c(0.5, 0.4), `3` = c(0.1, 0.4), `4` = c(0, 0.1))   # lowest chance, width
  for (cl in 1:4) {
    r = range(x[eap_code == cl & !is.na(eap_prob), eap_prob])
    if (!is.finite(r[1])) next
    lo = ranges[[as.character(cl)]][1]; span = ranges[[as.character(cl)]][2]
    x[eap_code == cl & !is.na(eap_prob), eap_prob := lo + (eap_prob - r[1]) * span / (r[2] - r[1])]
  }
  hce_p = c(`1` = 1, `2` = 0.95, `3` = 0.592, `4` = 0.15)
  x[!is.na(eap_prob) & weekpay >= hce / 52 & eap_code %in% 1:4, eap_prob := hce_p[as.character(eap_code)]]
  x[eap_code == 0 & sal_above & !inelig & !exempt, eap_prob := 0]
  # One seeded draw per person, in the order of SERIAL and PERNUM, decides who passes.
  setorder(x, SERIAL, PERNUM)
  set.seed(as.integer(OT$seed) + yr)
  u = runif(nrow(x))
  x[, passes_duties := !is.na(eap_prob) & u < eap_prob]

  # 8. Overtime. A covered, non-exempt worker is "always" in overtime when usual hours are over 40, "some" when
  #    usual hours are 40 or fewer but last week's were over 40, and otherwise has none. Weekly overtime pay is the
  #    whole pay for the hours over 40, base and premium together (the premium alone is a third of it). The year's
  #    overtime is weekly overtime times weeks worked, at most the person's wages. It is missing for people not
  #    asked, and for asked workers whose pay is missing.
  ok = emp & x$ELIGORG == 1 & !x$inelig & !x$exempt
  nonexempt = ok & (x$hourly | (!is.na(x$weekpay) & x$weekpay < thr) | (x$sal_above & !x$passes_duties) |
                      (!x$hourly & !is.na(x$weekpay) & x$weekpay == thr))
  x[, ot_cat := fcase(nonexempt & !is.na(usual) & usual > 40, 'always',
                      nonexempt & !is.na(usual) & usual <= 40 & !is.na(lastwk) & lastwk > 40, 'some',
                      nonexempt, 'eligible, no overtime',
                      emp & x$ELIGORG == 1, 'not covered or exempt',
                      default = 'not a wage worker now')]
  x[, ot_week := fifelse(ot_cat %in% c('always', 'some') & !is.na(finalhours) & finalhours > 40 & !is.na(wage),
                         1.5 * wage * (finalhours - 40), 0)]
  x[, pay_missing := asked & ELIGORG == 1 & emp & !has_pay]
  x[, ot_year_raw := ot_week * WKSWORK1]
  x[, wages_ := pmax(fifelse(INCWAGE >= 99999998, 0, as.numeric(INCWAGE)), 0)]
  x[, ot := fifelse(asked & !pay_missing, pmin(ot_year_raw, wages_), NA_real_)]
  x[]
}
