#-------------------------------------------------------------------------------
# overtime_cps.R: each CPS ASEC person's overtime pay in the tax year, by Ernie
# Tedeschi's rules (Reports/2024/Overtime/src/overtime_PUBLIC.do, The Budget
# Lab, September 2024), with each tax year's salary thresholds. Sourced by
# build_cps.R; cps_overtime(yr, ...) returns one row per person.
#
# The data. The earnings questions (weekly pay, hourly wage, paid by the hour)
# are asked only of the households in their 4th and 8th month in the survey,
# about a quarter of the March sample, and are answered for the current job in
# March of the year after the tax year. They are read from the ASEC extract that
# carries them (settings paths.raw.cps_earnings, the "cps_asec_common_v2" pull of
# 27 October), which has the same records as the common extract build_cps.R
# reads (checked: the same SERIAL and PERNUM). Weeks worked and wages are the
# tax year's (WKSWORK1, INCWAGE).
#
# Who is asked. A person is "asked" when the person's household is in the
# earnings group (EARNWT > 0) and the group of their month in sample still has
# its pay in this year's file. In the March 2024 ASEC (tax year 2023) the
# month-4 group lost its pay (WEEKPAY_GAP.md): a month group whose workers have
# weekly pay on fewer than half of them counts as not asked, unless settings
# paths.raw.cps_march_basic gives the March basic file, which holds the same
# people's pay (linked by MARBASECIDP; scratch explore/2026-10-28/overtime,
# RESULTS.md part 1), in which case the pay is taken from there. The basic file
# also gives hours last week at the main job. A person not asked has overtime missing (NA), not zero; build_cps.R
# makes a unit's overtime missing when either adult's is. A worker in an asked
# group with no weekly pay and no hourly wage (a few per year) is missing too.
#
# Tedeschi's rules, person by person (his variable names in brackets):
#   employed            EMPSTAT 10 or 12 [emp]
#   not covered [inelig] employed but not a private, state or local government
#                       employee (CLASSWKR 21-23, 27, 28): federal workers,
#                       the armed forces, the self-employed, unpaid family
#                       workers; and clergy (OCC2010 2040, 2050, 2060). Put back:
#                       the postal service (IND 6370); federal workers in
#                       electric power (IND 570) in AL, GA, KY, MS, NC, TN, VA
#                       (the TVA); federal workers in libraries (IND 6770) in DC
#                       (the Library of Congress)
#   exempt by occupation [exempt] physicians (OCC2010 3010, 3040, 3060, 3120),
#                       lawyers (2100), teachers (2200-2550) and education
#                       administrators and counselors (230, 2000) in schools and
#                       colleges (IND 7860, 7870), outside sales (4950),
#                       agriculture (IND 170, 180, 290), fishermen (IND 280 and
#                       OCC2010 6100, 9310), media workers outside metro areas
#                       (2810-2860, CBSASZ 0), companions (3600, 4610 in IND
#                       8170, 8370, 9290), computer workers paid at least $27.63
#                       an hour or the year's salary threshold a week (110, 1005-
#                       1107, 5800, 7900), motor carrier workers (IND 6170 and
#                       7110, 7200, 7210, 7220, 9130), rail workers (9200, 9230,
#                       9240, 9260), seamen (IND 6090 and 9300, 9310, 9570), and
#                       salesmen and mechanics at vehicle dealers (IND 4670, 4680
#                       and 4750, 4760, 7110, 7150, 7160, 7200-7260)
#   paid by the hour    PAIDHOUR 2 [hourly]
#   white-collar duties [eap_prob] a salaried worker paid at least the year's
#                       salary threshold a week, covered and not exempt, passes
#                       the duties test with a chance set from the occupation's
#                       class in DOL's 2016 table (eap_codes.dta, classes 0-4)
#                       and the person's weekly pay: within each class a gamma
#                       distribution fitted to the weekly pay of salaried
#                       workers above the threshold (EARNWT-weighted), its
#                       cumulative chance at the person's pay, rescaled to the
#                       class's range (1: 0.9-1, 2: 0.5-0.9, 3: 0.1-0.5, 4:
#                       0-0.1; class 0 never passes). Highly compensated (weekly
#                       pay at least the year's HCE line / 52): 1, 0.95, 0.592,
#                       0.15 by class. One draw per worker, seeded (settings
#                       cps.overtime.seed + the year), in SERIAL, PERNUM order.
#                       Passing makes the worker exempt.
#   overtime categories [otalways, otsome] covered, not exempt, and hourly, or
#                       salaried below the threshold or not passing the duties
#                       test; usual hours at the main job over 40 (always), or
#                       usual 40 or less and hours last week over 40 (some).
#                       Usual hours that vary (UHRSWORK1 997) fall in neither,
#                       as in Tedeschi's code.
#   hours [finalhours]  hours last week at the main job (AHRSWORK1, from the
#                       March basic file: the ASEC has only hours at all jobs,
#                       AHRSWORKT, used where no basic file is given), else
#                       usual hours at the main job
#   wage [wage]         hourly workers: the hourly wage (HOURWAGE2, without
#                       overtime, tips and commissions), else weekly pay over
#                       usual hours; salaried: weekly pay over usual hours at
#                       the main job (hours last week when usual hours vary)
#   weekly overtime     1.5 x wage x (hours - 40) in an overtime category with
#                       hours over 40 [ot_weekly_earnings]: the whole pay for
#                       the hours over 40, base and premium, as Tedeschi counts
#                       it (the premium alone is a third of it)
#   ot                  overtime pay in the year: weekly overtime x weeks worked
#                       in the tax year (WKSWORK1), at most the person's wages
#                       (INCWAGE). Tax-Simulator's name (ot1, ot2, ot on the unit)
#
# The occupation codes. Tedeschi's code lists, and DOL's table of classes, are
# in the Census 2002/2010 occupation codes; the ASEC's own OCC changes to the 2018
# codes from March 2020. Every year is read on IPUMS's OCC2010 (each year's
# occupation put on the 2010 codes), so one set of lists serves every year. A
# 2010 code missing from DOL's table takes the class of the nearest lower code in
# it (Tedeschi's carry-forward). Industry codes (IND, the 2012 and 2017 Census
# codes) are the same for every industry the rules name; build_cps.R prints the
# count of each named code per year.
#-------------------------------------------------------------------------------

OT_VARS = c('SERIAL', 'PERNUM', 'MISH', 'EARNWT', 'ELIGORG', 'PAIDHOUR', 'EARNWEEK2', 'HOURWAGE2', 'UHRSWORK1',
            'AHRSWORKT', 'OCC2010', 'IND', 'CLASSWKR', 'EMPSTAT', 'STATEFIP', 'CBSASZ', 'WKSWORK1', 'INCWAGE', 'GQ')
OT_IND_NAMED = c(170, 180, 280, 290, 570, 4670, 4680, 6090, 6170, 6370, 6770, 7860, 7870, 8170, 8370, 9290)

read_ot_sample = function(dir, sample, vars) {
  dd = file.path(dir, sample)
  ddi = ipumsr::read_ipums_ddi(list.files(dd, '\\.xml$', full.names = TRUE))
  x = as.data.table(ipumsr::read_ipums_micro(ddi, vars = tidyselect::any_of(vars), verbose = FALSE))
  x[, lapply(.SD, function(v) { attributes(v) <- NULL; v })]
}

# The March basic file's answers for the people of one ASEC year (NULL when settings name no basic file or it lacks
# the year): weekly pay, hourly wage and hours last week at the main job. Linked by MARBASECIDP (IPUMS's person link
# between the ASEC and the March basic file; CPSIDP where a sample lacks it, as the 2026 basic sample does). Returns
# SERIAL, PERNUM and the basic file's b_week, b_hour, b_lw1; a duplicated key stops.
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

cps_overtime = function(yr, x_keys) {
  OT = settings$cps$overtime
  thr = as.numeric(OT$salary_threshold[[as.character(yr)]])
  hce = as.numeric(OT$hce_annual[[as.character(yr)]])
  if (!length(thr) || !length(hce)) stop(sprintf('settings cps.overtime has no salary threshold or HCE line for %d', yr))
  comp_hourly = as.numeric(OT$computer_hourly)
  x = read_ot_sample(settings$paths$raw$cps_earnings, sprintf('cps%d_03s', yr + 1), OT_VARS)
  miss = setdiff(OT_VARS, names(x))
  if (length(miss)) stop(sprintf('the earnings extract for tax year %d lacks %s', yr, paste(miss, collapse = ', ')))
  stopifnot(!anyNA(x))
  # the same records as the common extract build_cps.R reads
  setkey(x, SERIAL, PERNUM)
  k = x_keys[order(SERIAL, PERNUM), .(SERIAL, PERNUM)]
  if (nrow(k) != nrow(x) || !all(k$SERIAL == x$SERIAL) || !all(k$PERNUM == x$PERNUM))
    stop(sprintf('tax year %d: the earnings extract and the common extract have different records', yr))
  x = x[GQ != 2]

  # who is asked: the earnings group, in a month group whose pay is in this year's file (or comes from the basic file)
  x[, grp := EARNWT > 0]
  stopifnot(all(x[grp == TRUE, MISH] %in% c(4, 8)))   # ELIGORG is also 1 on the ASEC oversample, never asked (EARNWT 0)
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

  emp = x$EMPSTAT %in% c(10, 12)
  covered_cow = x$CLASSWKR %in% c(21, 22, 23, 27, 28)
  x[, inelig := emp & (!covered_cow | OCC2010 %in% c(2040, 2050, 2060))]
  x[emp & (IND == 6370 | (CLASSWKR == 25 & IND == 570 & STATEFIP %in% c(21, 47, 28, 1, 13, 37, 51)) |
             (CLASSWKR == 25 & IND == 6770 & STATEFIP == 11)), inelig := FALSE]
  x[, hourly := PAIDHOUR == 2]
  x[, weekpay := fifelse(EARNWEEK2 > 0, as.numeric(EARNWEEK2), NA_real_)]
  usual = fifelse(x$UHRSWORK1 > 0 & x$UHRSWORK1 < 997, as.numeric(x$UHRSWORK1), NA_real_)
  # hours last week: at the main job from the basic file (Tedeschi's hourslw1) where linked, else at all jobs
  lw = fifelse(!is.na(x$b_lw1), x$b_lw1, as.numeric(x$AHRSWORKT))
  lastwk = fifelse(lw > 0 & lw < 999, lw, NA_real_)
  x[, `:=`(usual = usual, lastwk = lastwk)]
  x[, finalhours := fifelse(!is.na(lastwk), lastwk, usual)]
  denom = fifelse(!is.na(usual), usual, lastwk)
  x[, wage := fifelse(hourly & HOURWAGE2 > 0 & HOURWAGE2 < 999, as.numeric(HOURWAGE2), weekpay / denom)]
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

  # DOL's duties classes by occupation (2002/2010 codes), the nearest lower code where the table lacks one
  eap = as.data.table(haven::read_dta(settings$paths$raw$eap_codes))
  eap = eap[, lapply(.SD, as.numeric)][order(occ18)]
  stopifnot(!anyNA(eap), all(eap$eap_code %in% 0:4))
  idx = findInterval(x$OCC2010, eap$occ18)
  x[, eap_code := fifelse(idx > 0, eap$eap_code[pmax(idx, 1)], NA_real_)]
  x[, eap_exact := OCC2010 %in% eap$occ18]
  wk = x[asked & ELIGORG == 1 & emp]
  cat(sprintf('  duties classes: %.1f%% of asked workers (EARNWT) have an occupation in DOL\'s table, %.1f%% take the nearest lower code\'s class\n',
              100 * wk[, sum(EARNWT * eap_exact) / sum(EARNWT)], 100 * wk[, sum(EARNWT * !eap_exact) / sum(EARNWT)]))

  # the duties-test chance (Tedeschi's gamma step), for salaried workers above the threshold
  x[, eap_prob := NA_real_]
  x[, sal_above := asked & ELIGORG == 1 & !hourly & !is.na(weekpay) & weekpay > thr]
  for (cl in 1:4) {
    fit = x[sal_above & eap_code == cl]
    if (!nrow(fit)) next
    m = sum(fit$EARNWT * fit$weekpay) / sum(fit$EARNWT)
    v = sum(fit$EARNWT * (fit$weekpay - m)^2) / (sum(fit$EARNWT) - 1)
    x[sal_above & eap_code == cl & !inelig & !exempt, eap_prob := pgamma(weekpay, shape = m^2 / v, scale = v / m)]
  }
  ranges = list(`1` = c(0.9, 0.1), `2` = c(0.5, 0.4), `3` = c(0.1, 0.4), `4` = c(0, 0.1))
  for (cl in 1:4) {
    r = range(x[eap_code == cl & !is.na(eap_prob), eap_prob])
    if (!is.finite(r[1])) next
    lo = ranges[[as.character(cl)]][1]; span = ranges[[as.character(cl)]][2]
    x[eap_code == cl & !is.na(eap_prob), eap_prob := lo + (eap_prob - r[1]) * span / (r[2] - r[1])]
  }
  hce_p = c(`1` = 1, `2` = 0.95, `3` = 0.592, `4` = 0.15)
  x[!is.na(eap_prob) & weekpay >= hce / 52 & eap_code %in% 1:4, eap_prob := hce_p[as.character(eap_code)]]
  x[eap_code == 0 & sal_above & !inelig & !exempt, eap_prob := 0]
  setorder(x, SERIAL, PERNUM)
  set.seed(as.integer(OT$seed) + yr)
  u = runif(nrow(x))
  x[, passes_duties := !is.na(eap_prob) & u < eap_prob]

  # overtime categories, hours and pay
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
  # pay missing on an asked worker who could have overtime: the person's overtime is missing
  x[, pay_missing := asked & ELIGORG == 1 & emp & !has_pay]
  x[, ot_year_raw := ot_week * WKSWORK1]
  x[, wages_ := pmax(fifelse(INCWAGE >= 99999998, 0, as.numeric(INCWAGE)), 0)]
  x[, ot := fifelse(asked & !pay_missing, pmin(ot_year_raw, wages_), NA_real_)]
  x[]
}
