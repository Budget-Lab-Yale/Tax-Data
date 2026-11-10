#-------------------------------------------------------------------------------
# The SCF waves in tax units: one row per unit and implicate, with each unit's
# income and balance sheet in nominal dollars.
#
# roster_scf.py runs first. It builds the tax units and writes them, each adult's
# share of the dollars of the group outside the primary economic unit (PEU), and
# the inheritances received in the income year. This script reads those files,
# the raw interview file and the summary file. A unit that holds the whole PEU
# takes all of the PEU's income and balance sheet. When the respondent and the
# spouse or partner are separate units, each takes a share by Kevin Moore's
# rules, as production's builder does. A unit outside the PEU takes its members'
# shares of the group's dollars. Writes data/scf_units_<wave>.csv.gz.
# FEATURES.md, "The SCF", describes the variables. Run from the repository root.
#-------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(data.table); library(haven)
  library(jsonlite)
})
settings = fromJSON(Sys.getenv('REBUILD_SETTINGS', 'research/rebuild/pipeline/settings.json'))
OUT = file.path(Sys.getenv('REBUILD_OUT', settings$paths$out), 'data')
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
source('src/imputations/wealth_schema.R')       # only the names of the balance sheet's items

#-------------------------------------------------------------------------------
# The SCF items read
#-------------------------------------------------------------------------------
minus_one_to_zero = function(x) if_else(x == -1, 0, x)   # the SCF's -1 means "nothing"
# The business section: owns a private business (X3103); owns a business it does not actively manage (X3401); the
# legal form of actively managed businesses 1 and 2 (X3119, X3219: 1 partnership, 3 S corporation, 11 and 12 limited
# partnership, LLP or LLC); and, among businesses not actively managed, limited partnerships (X3407), other
# partnerships (X3411), LLCs (X3451) and S corporations (X3415).
BUSINESS_ITEMS = c('x3103', 'x3401', 'x3119', 'x3219', 'x3407', 'x3411', 'x3451', 'x3415')
# The items for the deduction for qualified business income (QBI). For each actively managed business b (1, 2): its
# industry group, its workers, the household members who work in it, the household's share of it and the value of
# that share. The legal forms are grouped: cf is a sole proprietorship or not a formal business; ps is a
# partnership, limited partnership, LLP, LLC or S corporation.
QBI_ITEMS = unlist(lapply(c('31', '32'), function(b) paste0('x', b, c('07', '11', '13', '14', '15', '16', '17', '28', '29'))))
QBI_GROUPS = list(cf = c(2, 40), ps = c(1, 3, 11, 12))
K1_FORMS = c(1, 3, 11, 12)      # the legal forms that file a Schedule K-1
INCOME = c(wages = 'X5702', txbl_int = 'X5708', dividends = 'X5710', capital_gains = 'X5712',
           business_income = 'X5704', rent_royalty_estate = 'X5714', ss_pensions_scf = 'X5722',
           exempt_int = 'X5706')
# The items for the own incomes of the respondent and the spouse or partner (Kevin Moore's code, as in production's
# builder)
PERSON_ITEMS = c('X4112', 'X4113', 'X4712', 'X4713', 'X4131', 'X4132', 'X4731', 'X4732',
                 'X5315', 'X5317', 'X5318', 'X5319', 'X5323', 'X5325', 'X5326', 'X5327',
                 'X5331', 'X5333', 'X5334', 'X5335', 'X5415', 'X5417', 'X5418', 'X5419',
                 'X6558', 'X6566', 'X6574', 'X6464', 'X6465', 'X6469', 'X6470', 'X6474', 'X6475', 'X6479', 'X6480',
                 'X6965', 'X6966', 'X6971', 'X6972', 'X6977', 'X6978', 'X6983', 'X6984',
                 'X5606', 'X5614', 'X5622', 'X5630', 'X5724', 'X5725')
AUTO_LOANS = c(x2218 = 'x2219', x2318 = 'x2319', x2418 = 'x2419', x7169 = 'x7170')   # each loan's balance item = its rate item
# The elective deferral items. Each person has two blocks of items, one per plan. In each block, item +40 asks
# whether the person contributes; items +41 to +43 give the percent of pay, the amount and the amount's period; and
# items +44 to +46 ask the same a second way.
DEFERRAL_BLOCKS = list(r = c(11000, 11100), s = c(11300, 11400))
DEFERRAL_ITEMS = unlist(lapply(unlist(DEFERRAL_BLOCKS), function(b) sprintf('X%d', b + 40:46)))
# Social Security for the respondent and for the spouse or partner: whether the household receives any (X5301), the
# kind (X5304, X5309), the amount (X5306, X5311) and its period (X5307, X5312: 4 a month, 5 a quarter, 6 a year,
# 12 every two months)
SS_ITEMS = c('X5301', 'X5304', 'X5306', 'X5307', 'X5309', 'X5311', 'X5312')
ss_factor = function(f) case_when(f == 4 ~ 12, f == 5 ~ 4, f == 6 ~ 1, f == 12 ~ 6, TRUE ~ 0)
# Real estate: the net income of properties 1 and 2 and of the remaining vacation homes and other properties, and
# the questions whether they have income; rent from part of the home and from farm or ranch land (amount and
# period); and whether the household owns other real estate
RENTAL_ITEMS = c('X1730', 'X1830', 'X2010', 'X2020', 'X1729', 'X1829', 'X2009', 'X2019',
                 'X1224', 'X1225', 'X505', 'X506', 'X1700')
# The balance sheet items that a split couple divides half and half
HOUSING = c('primary_home', 'other_home', 're_fund', 'primary_mortgage', 'other_mortgage',
            'kg_primary_home', 'kg_other_re')

# The factor that turns an amount per period into a yearly amount, for the SCF's period codes. Codes not listed
# give 0, as in Kevin Moore's code.
aconv = function(f) {
  case_when(f == 2 ~ 52.18, f == 3 ~ 26.09, f == 4 ~ 12, f == 5 ~ 4, f %in% c(6, 8, 14, 22) ~ 1,
            f == 11 ~ 2, f == 12 ~ 6, f == 31 ~ 24, TRUE ~ 0)
}

#-------------------------------------------------------------------------------
# Dividing the PEU's dollars between the respondent and the spouse or partner
#-------------------------------------------------------------------------------

# A person's regular pensions, made yearly, from the four pension slots of the
# raw file. who is 1 for the respondent and 2 for the spouse or partner. As in
# Kevin Moore's code.
pension_income = function(h, who) {
  pmax(0, h$X5318 * aconv(h$X5319) * (h$X5315 == who) * (h$X5317 >= 1)) +
    pmax(0, h$X5326 * aconv(h$X5327) * (h$X5323 == who) * (h$X5325 >= 1)) +
    pmax(0, h$X5334 * aconv(h$X5335) * (h$X5331 == who) * (h$X5333 >= 1)) +
    pmax(0, h$X5418 * aconv(h$X5419) * (h$X5415 == who) * (h$X5417 >= 1))
}

# A person's account withdrawals, made yearly, as in Kevin Moore's code. own is
# the person's own item (X6558 for the respondent, X6566 for the spouse or
# partner). The withdrawals tied to the person's pension slots and accounts are
# added, and two household items (X6574, and X5724 where X5725 is 11) are split
# half and half.
withdrawals = function(h, who, own) {
  own + 0.5 * h$X6574 +
    pmax(0, h$X6464 * aconv(h$X6465) * (h$X5315 == who)) + pmax(0, h$X6469 * aconv(h$X6470) * (h$X5323 == who)) +
    pmax(0, h$X6474 * aconv(h$X6475) * (h$X5331 == who)) + pmax(0, h$X6479 * aconv(h$X6480) * (h$X5415 == who)) +
    pmax(0, h$X6965 * aconv(h$X6966) * (h$X5606 == who)) + pmax(0, h$X6971 * aconv(h$X6972) * (h$X5614 == who)) +
    pmax(0, h$X6977 * aconv(h$X6978) * (h$X5622 == who)) + pmax(0, h$X6983 * aconv(h$X6984) * (h$X5630 == who)) +
    0.5 * h$X5724 * (h$X5725 == 11)
}

# The own labor, business and total income of the respondent (r_) and of the
# spouse or partner (s_), as in Kevin Moore's code. Labor income is the main
# job's wages (X4112, X4712). Business income is the main job's business income
# (X4131, X4731). Total income adds regular pensions and account withdrawals.
person_incomes = function(h) {
  tibble(Y1 = h$Y1,
         r_lab = pmax(0, h$X4112) * aconv(h$X4113), s_lab = pmax(0, h$X4712) * aconv(h$X4713),
         r_bus = pmax(0, h$X4131) * aconv(h$X4132), s_bus = pmax(0, h$X4731) * aconv(h$X4732)) %>%
    mutate(r_tot = r_lab + r_bus + pension_income(h, 1) + withdrawals(h, 1, h$X6558),
           s_tot = s_lab + s_bus + pension_income(h, 2) + withdrawals(h, 2, h$X6566))
}

# The yearly amount a person defers to one plan, from one way the question is
# asked: the amount per period (items amt and frq), or else the percent of pay
# (item pct) times the person's yearly main-job pay. item(k) reads the plan
# block's item k.
deferral_amount = function(item, pct, amt, frq, pay) {
  a = pmax(item(amt), 0) * aconv(item(frq))
  p = pmax(item(pct), 0) / 10000
  p = if_else(p <= 1, p, 0)                       # a share above all of pay is a code, not a share
  if_else(a > 0, a, p * pay)
}

# The respondent's share of a split PEU's dollars, from the two people's
# incomes r and s. The spouse's or partner's share is 1 minus it. When both
# incomes are 0, each takes half.
r_share = function(r, s) if_else(r + s == 0, 0.5, r / pmax(r + s, 1e-9))

#-------------------------------------------------------------------------------
# Each wave
#-------------------------------------------------------------------------------
for (wave in settings$paths$raw$scf_waves) {
  # 1. Read the wave's summary file and the raw items used. The wave's folder is settings paths.raw.scf_wave_dirs
  #    for a wave that is not in the shared folder (the 2025 wave), and paths.raw.scf otherwise. Income items' -1
  #    ("nothing") is read as 0.
  own = settings$paths$raw$scf_wave_dirs[[as.character(wave)]]
  dir = sub('{wave}', wave, if (is.null(own)) settings$paths$raw$scf else own, fixed = TRUE)
  raw_path = file.path(dir, sprintf('p%02di6.dta', wave %% 100))
  scfp = fread(file.path(dir, sprintf('SCFP%d.csv', wave))) %>% as_tibble()
  hh = read_dta(raw_path, col_select = all_of(c('y1', 'yy1', 'x42001', 'x3902', tolower(INCOME), BUSINESS_ITEMS, QBI_ITEMS,
                                                 tolower(PERSON_ITEMS), names(AUTO_LOANS), unname(AUTO_LOANS),
                                                 tolower(DEFERRAL_ITEMS), 'x14', 'x19', 'x102', tolower(SS_ITEMS),
                                                 tolower(RENTAL_ITEMS)))) %>%
    mutate(across(everything(), as.numeric))
  names(hh) = toupper(names(hh))
  stopifnot(!anyDuplicated(hh$Y1), !anyNA(hh), nrow(hh) == nrow(scfp))
  hh = hh %>% mutate(across(all_of(unname(INCOME)), minus_one_to_zero))
  stopifnot(all(hh$X5702 >= 0), all(hh$X5706 >= 0), all(hh$X5708 >= 0), all(hh$X5710 >= 0), all(hh$X5722 >= 0))

  # 2. Interest on auto loans: each loan's balance times its interest rate (a rate code below 0 is read as 0).
  hh$auto_int_exp = 0
  for (b in names(AUTO_LOANS)) {
    bal = hh[[toupper(b)]]; rate = hh[[toupper(AUTO_LOANS[[b]])]]
    stopifnot(all(bal >= 0))
    hh$auto_int_exp = hh$auto_int_exp + bal * pmax(rate, 0) / 10000
  }
  cat(sprintf('wave %d: auto loan interest $%.2f billion, households with any %.2f million (rate codes below 0 read as 0: %d)\n',
              wave, sum(hh$X42001 / 5 * hh$auto_int_exp) / 1e9, sum(hh$X42001[hh$auto_int_exp > 0] / 5) / 1e6,
              sum(sapply(unname(AUTO_LOANS), function(r) sum(hh[[toupper(r)]] < 0)))))

  # 3. Gross Social Security per person, from the benefits section, made yearly from its period. SSI (kind 7) is
  #    left out.
  for (who in list(c('r', 'X5304', 'X5306', 'X5307'), c('s', 'X5309', 'X5311', 'X5312'))) {
    amt = hh[[who[3]]]; per = hh[[who[4]]]
    got = hh$X5301 == 1 & amt > 0
    cat(sprintf('wave %d: Social Security, %s: recipients %.2f m, of whom SSI (code 7, left out) %.2f m; period code not usable (counted as 0): %d records\n',
                wave, if (who[1] == 'r') 'respondent' else 'spouse or partner', sum(hh$X42001[got] / 5) / 1e6,
                sum(hh$X42001[got & hh[[who[2]]] == 7] / 5) / 1e6, sum(got & ss_factor(per) == 0)))
    hh[[paste0(who[1], '_ss')]] = if_else(got & hh[[who[2]]] != 7, amt * ss_factor(per), 0)
  }
  # 4. Rental income: the net income of the properties, plus gross rent from part of the home or from farm land. Also
  #    the household's real estate flags, which are used only in reports.
  hh = hh %>% mutate(rental_hh = minus_one_to_zero(X1730) + minus_one_to_zero(X1830) + minus_one_to_zero(X2010) +
                       minus_one_to_zero(X2020) + pmax(X1224, 0) * aconv(X1225) + pmax(X505, 0) * aconv(X506))
  re_flags = hh %>% select(Y1) %>% inner_join(scfp %>% select(Y1, ORESRE, NNRESRE), by = 'Y1') %>%
    inner_join(hh %>% select(Y1, all_of(RENTAL_ITEMS)), by = 'Y1') %>%
    transmute(hh = Y1,
              inc_re = X1729 == 1 | X1730 != 0 | X1829 == 1 | X1830 != 0 | X2009 == 1 | X2010 != 0 | X2019 == 1 |
                X2020 != 0,
              inc_re_narrow = X1729 == 1 | X1829 == 1 | X2010 != 0 | X2020 != 0,
              oth_re = X1700 == 1 | ORESRE > 0 | NNRESRE != 0,
              home_part = X1224 > 0 | X505 > 0)
  stopifnot(nrow(re_flags) == nrow(hh), !anyNA(re_flags), !anyNA(hh$rental_hh), !anyNA(hh$r_ss), !anyNA(hh$s_ss))
  cat(sprintf(paste0('wave %d: households owning real estate with income %.2f m (by the income question on properties 1 ',
                     'and 2 and the remaining amounts alone: %.2f m), owning other real estate %.2f m, renting part of ',
                     'the home or farm %.2f m\n'), wave, sum(hh$X42001[re_flags$inc_re] / 5) / 1e6,
              sum(hh$X42001[re_flags$inc_re_narrow] / 5) / 1e6, sum(hh$X42001[re_flags$oth_re] / 5) / 1e6,
              sum(hh$X42001[re_flags$home_part] / 5) / 1e6))

  # 5. The asset price factor: savings bonds in the summary file (SAVBND) over the same item in the raw file
  #    (X3902). It is the same for every household. The summary file's dollars are divided by it below, to put them
  #    in the raw file's dollars of the interview year.
  fx = scfp %>% select(Y1, SAVBND) %>% inner_join(hh, by = 'Y1') %>% filter(X3902 > 0)
  asset_factor = median(fx$SAVBND / fx$X3902)
  stopifnot(max(abs(fx$SAVBND / fx$X3902 / asset_factor - 1)) < 1e-3)

  # 6. The PEU's dollars: the income items, the 20 balance sheet items and the 4 unrealized gains, in the interview
  #    year's dollars.
  peu = scfp %>% transmute(
    Y1, cash = LIQ + CDS, equities = STOCKS + STMUTF + COMUTF, bonds = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    dc = IRAKH + THRIFT, db = FUTPEN + CURRPEN, life_ins = CASHLI, annuities = ANNUIT, trusts = TRUSTS,
    other_fin = OTHFIN + OMUTF, pass_throughs = BUS, primary_home = HOUSES, other_home = ORESRE, re_fund = NNRESRE,
    other_nonfin = VEHIC + OTHNFIN, primary_mortgage = MRTHEL, other_mortgage = RESDBT, credit_lines = OTHLOC,
    credit_cards = CCBAL, installment_debt = INSTALL, other_debt = ODEBT, kg_primary_home = KGHOUSE,
    kg_other_re = KGORE, kg_pass_throughs = KGBUS, kg_other = KGSTMF) %>%
    mutate(across(-Y1, ~ .x / asset_factor))
  # The retirement accounts' stock share: RETEQ over all four kinds of retirement account (RETQLIQ). It is a ratio,
  # so the asset factor does not apply.
  ret = scfp %>% transmute(Y1, ret_stock_share = if_else(RETQLIQ > 0, RETEQ / pmax(RETQLIQ, 1e-9), 0))
  stopifnot(!anyNA(ret$ret_stock_share), all(ret$ret_stock_share >= 0), all(ret$ret_stock_share <= 1 + 1e-9))
  cat(sprintf('wave %d: retirement accounts\' stock share: weighted RETEQ / RETQLIQ %.3f; share of rows with accounts %.3f\n',
              wave, sum(scfp$WGT * scfp$RETEQ) / sum(scfp$WGT * scfp$RETQLIQ), mean(scfp$RETQLIQ > 0)))
  stopifnot(setequal(setdiff(names(peu), 'Y1'), c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)))
  # The balance sheet under Tax-Simulator's names (value.<item>), and the unrealized gains as w_kg_<item>
  names(peu)[-1] = if_else(names(peu)[-1] %in% wealth_kg_vars, paste0('w_', names(peu)[-1]), paste0('value.', names(peu)[-1]))
  WCOLS = setdiff(names(peu), 'Y1')
  peu = peu %>% inner_join(hh %>% select(Y1, all_of(unname(INCOME)), auto_int_exp, r_ss, s_ss, rental_hh), by = 'Y1') %>%
    inner_join(ret, by = 'Y1') %>%
    inner_join(person_incomes(hh), by = 'Y1')
  stopifnot(nrow(peu) == nrow(hh), !anyNA(peu))

  # 7. Elective deferrals per person, from up to two plans on the main job. Each plan's amount is the amount per
  #    period, or else the percent of pay. The total is held to the year's legal limit (with the higher catch-up
  #    limit from the catch-up age) and then to the person's own wages.
  year = wave - 1L
  lim = settings$deferrals$limits[[as.character(year)]]
  stopifnot(length(lim) == 2)
  pay = list(r = pmax(0, hh$X4112) * aconv(hh$X4113), s = pmax(0, hh$X4712) * aconv(hh$X4713))
  age = list(r = hh$X14, s = hh$X19)
  elective = list()
  for (who in c('r', 's')) {
    tot = 0
    for (b in DEFERRAL_BLOCKS[[who]]) {
      x = function(k) hh[[sprintf('X%d', b + k)]]
      p1 = deferral_amount(x, 41, 42, 43, pay[[who]]); p2 = deferral_amount(x, 44, 45, 46, pay[[who]])
      tot = tot + if_else(x(40) == 1, if_else(p1 > 0, p1, p2), 0)   # 1 yes, 5 no, 3 other (not counted)
    }
    elective[[who]] = tot
  }
  limit = lapply(age, function(a) if_else(a >= settings$deferrals$catch_up_age, lim[2], lim[1]))
  # Each person's wages, so that each person's deferrals are at most their own wages. The PEU's wages (X5702) are
  # split by main-job pay, as the split units' wages are (r_share; half each when both are zero). Without a spouse
  # or partner (X102 = 0) they all go to the respondent.
  has_sp = hh$X102 > 0
  r_wage = hh$X5702 * if_else(has_sp, r_share(pay$r, pay$s), 1)
  dper = tibble(Y1 = hh$Y1, weight = hh$X42001 / 5, r_age = hh$X14, s_age = hh$X19, r_pay = pay$r, s_pay = pay$s,
                has_sp = has_sp, r_wage = r_wage, s_wage = hh$X5702 - r_wage,
                r_reported = elective$r, s_reported = elective$s,
                r_limited = pmin(elective$r, limit$r), s_limited = pmin(elective$s, limit$s)) %>%
    mutate(r_elective = pmin(r_limited, r_wage), s_elective = pmin(s_limited, s_wage), X5702 = hh$X5702)
  stopifnot(!anyNA(dper), all(dper$s_wage >= 0))
  cat(sprintf(paste0('wave %d: elective deferrals, respondents and spouses or partners: $%.1f billion reported, $%.1f billion ',
                     'after the %d limit ($%s, $%s from %d), $%.1f billion after each person\'s own wages; people above ',
                     'the limit %.2f m, above their own wages %.2f m\n'),
              wave, sum(dper$weight * (dper$r_reported + dper$s_reported)) / 1e9,
              sum(dper$weight * (dper$r_limited + dper$s_limited)) / 1e9, year, format(lim[1], big.mark = ','),
              format(lim[2], big.mark = ','), settings$deferrals$catch_up_age,
              sum(dper$weight * (dper$r_elective + dper$s_elective)) / 1e9,
              sum(dper$weight * ((dper$r_reported > dper$r_limited) + (dper$s_reported > dper$s_limited))) / 1e6,
              sum(dper$weight * ((dper$r_limited > dper$r_elective) + (dper$s_limited > dper$s_elective))) / 1e6))
  fwrite(dper, file.path(OUT, sprintf('scf_deferral_persons_%d.csv.gz', wave)))
  peu = peu %>% inner_join(dper %>% select(Y1, r_elective, s_elective), by = 'Y1')
  stopifnot(nrow(peu) == nrow(hh))
  # 8. The inheritances received in the income year, per household (roster_scf.inheritances).
  inh = fread(file.path(OUT, sprintf('scf_inheritance_%d.csv.gz', wave))) %>% as_tibble() %>% select(Y1 = hh, inheritance)
  stopifnot(nrow(inh) == nrow(hh), !anyDuplicated(inh$Y1), !anyNA(inh$inheritance), all(inh$inheritance >= 0))
  peu = peu %>% inner_join(inh, by = 'Y1')
  stopifnot(nrow(peu) == nrow(hh))

  # 9. Read the units roster_scf.py built, and the group's dollars by unit. Each household has either one unit
  #    holding the whole PEU, or one unit with the respondent's share ('r') and one with the spouse's or partner's
  #    ('sp').
  mu = fread(file.path(OUT, sprintf('scf_units_module_%d.csv.gz', wave))) %>% as_tibble()
  nd = fread(file.path(OUT, sprintf('scf_npeu_dollars_%d.csv.gz', wave))) %>% as_tibble()
  stopifnot(!anyDuplicated(mu$uid), all(mu$peu_dollars %in% c('all', 'r', 'sp', 'none')))
  k = mu %>% filter(source == 'peu') %>% count(hh, peu_dollars) %>%
    pivot_wider(names_from = peu_dollars, values_from = n, values_fill = 0)
  for (v in c('all', 'r', 'sp')) if (!(v %in% names(k))) k[[v]] = 0L
  stopifnot(all((k$all == 1 & k$r == 0 & k$sp == 0) | (k$all == 0 & k$r == 1 & k$sp == 1)), nrow(k) == nrow(hh))

  # 10. The PEU's units: each takes a share of each item. A unit holding the whole PEU takes all of it. On a split
  #     PEU, wages are divided by the two people's labor income, Schedules C and F by their business income, the
  #     home, other real estate, their mortgages and their gains half each, and everything else by their total
  #     income.
  p = mu %>% filter(source == 'peu') %>% select(uid, hh, peu_dollars, weight, head_pn, filing_status) %>%
    inner_join(peu, by = c('hh' = 'Y1')) %>%
    mutate(sh_lab = case_when(peu_dollars == 'all' ~ 1, peu_dollars == 'r' ~ r_share(r_lab, s_lab),
                              TRUE ~ 1 - r_share(r_lab, s_lab)),
           sh_bus = case_when(peu_dollars == 'all' ~ 1, peu_dollars == 'r' ~ r_share(r_bus, s_bus),
                              TRUE ~ 1 - r_share(r_bus, s_bus)),
           sh_earn = case_when(peu_dollars == 'all' ~ 1, peu_dollars == 'r' ~ r_share(r_tot, s_tot),
                               TRUE ~ 1 - r_share(r_tot, s_tot)),
           sh_house = if_else(peu_dollars == 'all', 1, 0.5))
  for (v in names(INCOME)) {
    p[[v]] = p[[INCOME[[v]]]] * (if (v == 'wages') p$sh_lab else if (v == 'business_income') p$sh_bus else p$sh_earn)
  }
  for (v in WCOLS) p[[v]] = p[[v]] * (if (sub('^(w_|value[.])', '', v) %in% HOUSING) p$sh_house else p$sh_earn)
  p$auto_int_exp = p$auto_int_exp * p$sh_earn     # divided as installment debt (value.installment_debt) is
  # Rental income is divided as X5714 is. Social Security goes by person, as elective deferrals do below.
  p$rental_income = p$rental_hh * p$sh_earn
  p$gross_ss = case_when(p$peu_dollars == 'all' ~ p$r_ss + p$s_ss, p$peu_dollars == 'r' ~ p$r_ss, TRUE ~ p$s_ss)
  # Elective deferrals: elective1 is the unit's head's, and elective2 the spouse's (0 without a spouse). A joint
  # unit holding the whole PEU gives each person their own, by who heads the unit. Any other unit holding the whole
  # PEU gives both people's to the head, because a partner the unit claims has no column of their own. A split unit
  # takes that person's own. Each person's deferrals are at most their own wages, and the check below confirms
  # that a unit's are at most its wages.
  joint_all = p$peu_dollars == 'all' & p$filing_status == 'joint'
  stopifnot(all(p$head_pn[joint_all] %in% c(1, 2)))
  p$elective1 = case_when(joint_all & p$head_pn == 2 ~ p$s_elective, joint_all ~ p$r_elective,
                          p$peu_dollars == 'all' ~ p$r_elective + p$s_elective,
                          p$peu_dollars == 'r' ~ p$r_elective, TRUE ~ p$s_elective)
  p$elective2 = case_when(joint_all & p$head_pn == 2 ~ p$r_elective, joint_all ~ p$s_elective, TRUE ~ 0)
  p$elective_deferrals = p$elective1 + p$elective2
  p$inheritance = p$inheritance * if_else(p$peu_dollars == 'all', 1, 0.5)   # a split couple takes half each
  stopifnot(all(p$elective_deferrals <= p$wages * (1 + 1e-9) + 1e-6))
  p$w_dc_stock_share = if_else(p$value.dc > 0, pmin(p$ret_stock_share, 1), 0)
  # taxunit_code: 0 the whole PEU, 1 the respondent's part, 2 the spouse's or partner's part, 8 other, 9 outside the PEU
  p = p %>% mutate(taxunit_code = case_when(peu_dollars == 'all' ~ 0L, peu_dollars == 'r' ~ 1L, TRUE ~ 2L)) %>%
    select(uid, all_of(names(INCOME)), gross_ss, rental_income, all_of(WCOLS), auto_int_exp, elective_deferrals,
           elective1, elective2, w_dc_stock_share, inheritance, taxunit_code)

  # 11. Units outside the PEU take their members' shares of the group's dollars. Items the SCF asks only of the PEU
  #     are missing on these units, not zero. Other units hold nothing. Then net worth, and the rest of Schedule E
  #     (X5714 less the rental part, held at zero from below).
  npeu = mu %>% filter(source == 'npeu') %>% select(uid) %>% left_join(nd, by = 'uid')
  stopifnot(!anyNA(npeu$wages), 'gross_ss' %in% names(npeu), !anyNA(npeu$gross_ss))
  for (v in setdiff(c(names(INCOME), WCOLS, 'auto_int_exp'), names(npeu))) npeu[[v]] = 0
  for (v in c('elective_deferrals', 'elective1', 'elective2', 'w_dc_stock_share', 'inheritance')) npeu[[v]] = NA_real_   # not asked: missing
  # The SCF asks its property section of the PEU only. Units outside the PEU have no rental part: the group's real
  # estate income is all in its rent_royalty_estate, and from there in sche_rest.
  npeu$rental_income = 0
  npeu = npeu %>% mutate(taxunit_code = 9L) %>%
    select(uid, all_of(names(INCOME)), gross_ss, rental_income, all_of(WCOLS), auto_int_exp, elective_deferrals,
           elective1, elective2, w_dc_stock_share, inheritance, taxunit_code)
  other = mu %>% filter(source == 'other') %>% select(uid)
  for (v in c(names(INCOME), 'gross_ss', 'rental_income', WCOLS, 'auto_int_exp')) other[[v]] = 0
  for (v in c('elective_deferrals', 'elective1', 'elective2', 'w_dc_stock_share', 'inheritance')) other[[v]] = rep(NA_real_, nrow(other))   # not asked
  other$taxunit_code = rep(8L, nrow(other))
  dollars = bind_rows(p, npeu, other)
  stopifnot(nrow(dollars) == nrow(mu), !anyDuplicated(dollars$uid))
  dollars$net_worth = rowSums(as.matrix(dollars[, paste0('value.', wealth_asset_vars)])) -
    rowSums(as.matrix(dollars[, paste0('value.', wealth_debt_vars)]))
  dollars$sche_rest = pmax(dollars$rent_royalty_estate - dollars$rental_income, 0)

  # 12. Business ownership goes to the units that hold the household's business equity, or else to the respondent's
  #     unit.
  biz = hh %>% transmute(hh = Y1, owns = X3103 == 1 | X3401 == 1,
                         k1 = X3119 %in% K1_FORMS | X3219 %in% K1_FORMS | X3407 == 1 | X3411 == 1 |
                           X3451 == 1 | X3415 == 1)
  stopifnot(all(biz$owns | !biz$k1))
  ub = mu %>% filter(source == 'peu') %>% select(uid, hh, peu_dollars) %>%
    inner_join(dollars %>% select(uid, value.pass_throughs), by = 'uid') %>%
    left_join(biz, by = 'hh') %>% group_by(hh) %>%
    mutate(holder = if (any(value.pass_throughs > 0)) value.pass_throughs > 0 else peu_dollars %in% c('all', 'r')) %>%
    ungroup()
  stopifnot(!anyNA(ub$owns), all((ub %>% group_by(hh) %>% summarise(n = sum(holder)))$n >= 1))
  # 13. The real estate flags (used only in reports) go to the units that carry the household's X5714 share, or
  #     else to the respondent's unit.
  ur = mu %>% filter(source == 'peu') %>% select(uid, hh, peu_dollars) %>%
    inner_join(dollars %>% select(uid, rent_royalty_estate), by = 'uid') %>%
    left_join(re_flags, by = 'hh') %>% group_by(hh) %>%
    mutate(holder = if (any(rent_royalty_estate != 0)) rent_royalty_estate != 0 else peu_dollars %in% c('all', 'r')) %>%
    ungroup()
  stopifnot(!anyNA(ur$inc_re), all((ur %>% group_by(hh) %>% summarise(n = sum(holder)))$n >= 1))

  # 14. The QBI items. For each household and group of legal forms: the industry group of the largest business, and
  #     the workers counted, in proportion to the household's share of each business. They go to the units that hold
  #     the business equity, in proportion to each unit's equity.
  qb = tibble(hh = hh$Y1)
  for (k in names(QBI_GROUPS)) {
    ind = rep(0, nrow(hh)); best = rep(-Inf, nrow(hh)); wk = rep(0, nrow(hh))
    for (b in c('31', '32')) {
      x = function(i) hh[[paste0('X', b, i)]]
      is_k = x('19') %in% QBI_GROUPS[[k]]
      fam = (x('13') == 1) + (x('14') == 1) + (x('15') == 1) + (x('16') == 1) + (x('17') == 1)
      # An S corporation pays a working owner W-2 wages, and all its workers count. Other legal forms count only
      # the workers outside the household. Partners and sole proprietors cannot pay themselves W-2 wages.
      counted = if_else(x('19') == 3, pmax(x('11'), 0), pmax(x('11') - fam, 0)) * x('28') / 10000
      wk = wk + if_else(is_k, counted, 0)
      if (k == 'ps') cat(sprintf('wave %d: business %s, S corporations: %d rows, workers X%s11 below 1: %d, one worker: %d, fewer workers than household members working in it: %d\n', wave, b,
                                 sum(x('19') == 3), b, sum(x('19') == 3 & x('11') < 1), sum(x('19') == 3 & x('11') == 1), sum(x('19') == 3 & x('11') < fam)))
      code = if_else(x('07') == 99, 8, x('07'))
      stopifnot(all(code[is_k] %in% 1:8), all(x('28')[is_k] > 0 & x('28')[is_k] <= 10000))
      take = is_k & x('29') > best          # strictly larger, so business 1 wins a tie
      ind = if_else(take, code, ind); best = if_else(take, x('29'), best)
    }
    qb[[paste0('biz_industry_', k)]] = ind
    qb[[paste0('biz_workers_', k)]] = wk
  }
  stopifnot(!anyNA(qb))
  ub = ub %>% group_by(hh) %>% mutate(eq_share = if (any(holder & value.pass_throughs > 0))
    if_else(holder, pmax(value.pass_throughs, 0) / sum(pmax(value.pass_throughs, 0)[holder]), 0) else as.numeric(holder) / sum(holder)) %>%
    ungroup()
  uq = ub %>% select(uid, hh, holder, eq_share) %>% left_join(qb, by = 'hh')
  for (k in names(QBI_GROUPS)) {
    uq[[paste0('biz_industry_', k)]] = if_else(uq$holder, uq[[paste0('biz_industry_', k)]], 0)
    uq[[paste0('biz_workers_', k)]] = uq[[paste0('biz_workers_', k)]] * uq$eq_share
  }
  QBI_COLS = c(paste0('biz_industry_', names(QBI_GROUPS)), paste0('biz_workers_', names(QBI_GROUPS)))
  stopifnot(!anyNA(uq[, QBI_COLS]), all(abs((ub %>% group_by(hh) %>% summarise(s = sum(eq_share)))$s - 1) < 1e-9))
  for (k in names(QBI_GROUPS)) {
    w5 = hh$X42001 / 5
    cat(sprintf('wave %d: QBI %s: households with a business of the group %.2f m; workers counted, owners\' share %.1f m; industry groups (households, m): %s\n',
                wave, k, sum(w5[qb[[paste0('biz_industry_', k)]] > 0]) / 1e6, sum(w5 * qb[[paste0('biz_workers_', k)]]) / 1e6,
                paste(sprintf('%d: %.2f', 1:8, sapply(1:8, function(g) sum(w5[qb[[paste0('biz_industry_', k)]] == g]) / 1e6)), collapse = ', ')))
  }

  # 15. Assemble one row per unit. Units outside the PEU and other units own no business or real estate, because
  #     the business and property sections are asked of the PEU only.
  units = mu %>% inner_join(dollars, by = 'uid') %>%
    left_join(ub %>% transmute(uid, owns_business = as.integer(owns & holder), k1_owner = as.integer(k1 & holder)),
              by = 'uid') %>%
    left_join(uq %>% select(uid, all_of(QBI_COLS)), by = 'uid') %>%
    left_join(ur %>% transmute(uid, owns_income_re = as.integer(inc_re & holder),
                               owns_other_re = as.integer(oth_re & holder), rents_home_part = as.integer(home_part & holder)),
              by = 'uid') %>%
    mutate(across(c(owns_business, k1_owner, owns_income_re, owns_other_re, rents_home_part), ~ replace_na(.x, 0L)),
           across(all_of(QBI_COLS), ~ replace_na(.x, 0))) %>%
    transmute(record_id = if_else(separate, uid, uid * 10), household_id = hh %/% 10,
              implicate = as.integer(hh %% 10), year = wave - 1L, weight, married = as.integer(filing_status == 'joint'),
              n_dep = as.integer(pmin(n_dep, 3)), age1 = as.integer(head_age),
              age2 = as.integer(if_else(filing_status == 'joint', spouse_age, -1L)),
              male1 = if_else(head_sex == 0, NA_integer_, as.integer(head_sex == 1)),
              male2 = if_else(filing_status == 'joint', if_else(spouse_sex == 0, NA_integer_, as.integer(spouse_sex == 1)), -1L),
              taxunit_code, across(all_of(names(INCOME))), gross_ss, rental_income, sche_rest, net_worth,
              asset_factor = asset_factor, across(all_of(WCOLS)), auto_int_exp, elective_deferrals, elective1, elective2,
              w_dc_stock_share, inheritance, across(all_of(QBI_COLS)),
              owns_business, k1_owner, owns_income_re, owns_other_re, rents_home_part, filing_status, n_child_away,
              group_level = as.integer(group_level), npeu_people,
              any_age_assumed = as.integer(any_age_assumed), any_link_assumed = as.integer(any_link_assumed),
              any_income_assumed = as.integer(any_income_assumed),
              any_dep_by_support = as.integer(any_dep_by_support),
              any_claimed_in_group = as.integer(any_claimed_in_group))
  # Sex may be missing: it is observed only for the respondent and the spouse or partner. Elective deferrals, the
  # stock share and inheritances may be missing: they are asked only of the PEU.
  stopifnot(nrow(units) == nrow(mu), !anyDuplicated(units$record_id),
            !anyNA(select(units, -male1, -male2, -elective_deferrals, -elective1, -elective2, -w_dc_stock_share, -inheritance)),
            all(is.na(units$w_dc_stock_share) == (units$taxunit_code > 2)),
            all(is.na(units$inheritance) == (units$taxunit_code > 2)),
            all(is.na(units$elective_deferrals) == (units$taxunit_code > 2)),
            all(is.na(units$elective2) == (units$taxunit_code > 2)),
            all(units$elective2[units$married == 0 & units$taxunit_code <= 2] == 0),
            all(units$age2[units$married == 1] >= 0),
            all(units$taxunit_code[is.na(units$male1) | is.na(units$male2)] > 2))

  # 16. Check that the PEU's dollars are divided between units, never created or lost.
  for (v in c(names(INCOME), WCOLS, 'auto_int_exp')) {
    a = sum(hh$X42001 / 5 * peu[[if (v %in% names(INCOME)) INCOME[[v]] else v]])
    z = sum((units$weight * units[[v]])[units$taxunit_code <= 2])
    stopifnot(abs(z - a) <= 1e-6 * max(1, abs(a)))
  }
  for (v in c('gross_ss', 'rental_income', 'inheritance')) {
    a = sum(hh$X42001 / 5 * (if (v == 'gross_ss') hh$r_ss + hh$s_ss else if (v == 'inheritance') peu$inheritance else hh$rental_hh))
    z = sum((units$weight * units[[v]])[units$taxunit_code <= 2])
    stopifnot(abs(z - a) <= 1e-6 * max(1, abs(a)))
  }
  # 17. Print checks on Social Security, the two parts of Schedule E, the real estate flags and inheritances, and
  #     write the units.
  W = sum(units$weight)
  summ = function(x) c(net_b = sum(units$weight * x) / 1e9, income_b = sum((units$weight * x)[x > 0]) / 1e9,
                       losses_b = -sum((units$weight * x)[x < 0]) / 1e9,
                       any_m = sum(units$weight[x != 0]) / 1e6, any_pct = 100 * sum(units$weight[x != 0]) / W,
                       income_pct = 100 * sum(units$weight[x > 0]) / W,
                       loss_m = sum(units$weight[x < 0]) / 1e6, loss_pct = 100 * sum(units$weight[x < 0]) / W)
  cat(sprintf('wave %d: new columns, all %.2f m units ($ billion, millions of units, %% of units)\n', wave, W / 1e6))
  print(round(t(sapply(c('gross_ss', 'rental_income', 'sche_rest', 'rent_royalty_estate'),
                       function(v) summ(units[[v]]))), 3))
  cat(sprintf('wave %d: Social Security by kind of unit ($ billion): %s\n', wave,
              paste(sprintf('%d: %.1f', sort(unique(units$taxunit_code)),
                            sapply(sort(unique(units$taxunit_code)), function(k)
                              sum((units$weight * units$gross_ss)[units$taxunit_code == k]) / 1e9)), collapse = ', ')))
  bind = units$rental_income > units$rent_royalty_estate
  cat(sprintf(paste0('wave %d: sche_rest held at 0 (rental above X5714): %d rows, %.3f m units, $%.2f billion dropped ',
                     '(of which X5714 itself a loss: %.3f m units, $%.2f billion; rental above a nonnegative X5714: ',
                     '%.3f m units, $%.2f billion)\n'), wave, sum(bind), sum(units$weight[bind]) / 1e6,
              sum((units$weight * (units$rental_income - units$rent_royalty_estate))[bind]) / 1e9,
              sum(units$weight[bind & units$rent_royalty_estate < 0]) / 1e6,
              sum((units$weight * (units$rental_income - units$rent_royalty_estate))[bind & units$rent_royalty_estate < 0]) / 1e9,
              sum(units$weight[bind & units$rent_royalty_estate >= 0]) / 1e6,
              sum((units$weight * (units$rental_income - units$rent_royalty_estate))[bind & units$rent_royalty_estate >= 0]) / 1e9))
  gap = max(abs(units$rental_income + units$sche_rest - units$rent_royalty_estate)[!bind])
  cat(sprintf('wave %d: rental_income + sche_rest - rent_royalty_estate where the floor does not bind: largest %.2e\n',
              wave, gap))
  stopifnot(gap < 1e-6)
  fl = list(owns_income_re = units$owns_income_re == 1, owns_other_re = units$owns_other_re == 1,
            rents_home_part = units$rents_home_part == 1)
  for (f in names(fl)) {
    cat(sprintf('wave %d: %s: %.2f m units (%d rows); with zero rental_income %.2f m\n', wave, f,
                sum(units$weight[fl[[f]]]) / 1e6, sum(fl[[f]]), sum(units$weight[fl[[f]] & units$rental_income == 0]) / 1e6))
  }
  cat(sprintf('wave %d: k1_owner = 1: %.2f m units; with zero sche_rest %.2f m\n', wave,
              sum(units$weight[units$k1_owner == 1]) / 1e6,
              sum(units$weight[units$k1_owner == 1 & units$sche_rest == 0]) / 1e6))
  pe = units$taxunit_code <= 2
  cat(sprintf(paste0('wave %d: inheritances received this year: $%.1f billion on the PEU units (income-year %d dollars), units with any ',
                     '%.3f m; not asked (missing) on %.2f m units outside the PEU or other (%d rows)\n'), wave,
              sum((units$weight * units$inheritance)[pe]) / 1e9, wave - 1L, sum(units$weight[pe & units$inheritance > 0]) / 1e6,
              sum(units$weight[!pe]) / 1e6, sum(!pe)))
  fwrite(units, file.path(OUT, sprintf('scf_units_%d.csv.gz', wave)))
  cat(sprintf('wave %d: %d unit rows, %.2f m units (outside the PEU %.2f m); PEU totals kept; asset factor %.6f\n',
              wave, nrow(units), sum(units$weight) / 1e6, sum(units$weight[units$taxunit_code == 9]) / 1e6,
              asset_factor))
  print(units %>% group_by(taxunit_code) %>% summarise(units_m = sum(weight) / 1e6, rows = n()))
}
