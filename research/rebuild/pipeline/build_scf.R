#-------------------------------------------------------------------------------
# build_scf.R: the SCF waves 2013, 2016, 2019 and 2022 in tax units, with each
# unit's income and balance sheet.
#
# Who is in which unit comes from the shared tax-unit rules (tax_units.py,
# through roster_scf.py, which the scf stage runs first; TAX_UNITS.md,
# section 4): {out}/data/scf_units_module_{wave}.csv.gz, one row per unit, and
# scf_npeu_dollars_{wave}.csv.gz, each outside-PEU unit's share of its group's
# income and balance sheet. Until 24 October the units came from production's
# builder (src/imputations/stage1_scf_tax_units.R, a port of Kevin Moore's
# code); that builder still serves src/ and is no longer called here. Its
# division of the primary economic unit's (PEU's) income and balance sheet
# between two units is kept, ported below.
#
# Dollars, by kind of unit:
#   - A unit holding the whole PEU (peu_dollars 'all': the respondent alone, a
#     joint couple, or a respondent who claims the partner) takes all of it.
#   - When the respondent and the spouse or partner are separate units ('r'
#     and 'sp': separate returns, or a partner with a unit of their own), each
#     takes a share, as Moore's code does: wages by the two people's labor
#     income, Schedules C and F by their business income from the main job,
#     the home, other real estate, their mortgages and their unrealized gains
#     half each, and everything else by their labor, business, pension and
#     account-withdrawal income (a half each when both are 0).
#   - Units outside the PEU (taxunit_code 9, group_level = 1) take their
#     members' shares of the group's answers (roster_scf.py, setting
#     tax_units.npeu_split); capital gains and tax-exempt interest are not asked
#     of the group (0), and its public assistance and other income has no
#     column here.
#   - Other units (a non-relative in the PEU, a partner outside it;
#     taxunit_code 8) and dependents hold nothing (the PEU's balance sheet
#     cannot be separated).
# taxunit_code: 0 the whole PEU, 1 the respondent's part, 2 the spouse's or
# partner's part, 8 other, 9 outside the PEU.
#
# Income, nominal dollars of the tax year, from raw interview items (the
# summary file's incomes are inflated to 2022 dollars):
#   wages X5702, taxable interest X5708, dividends X5710, capital gains X5712,
#   business income X5704 (Schedules C and F), net rent, trusts and royalties
#   X5714 (Schedule E), Social Security and pensions X5722, tax-exempt
#   interest X5706. A value of -1 means "nothing" and is set to 0.
# Balance sheet: the summary file's items (2022 dollars) in production's 20
# components and 4 unrealized gains (src/imputations/wealth_schema.R, names
# only), over the wave's asset price factor (SAVBND / X3902), which puts them
# in the interview year's dollars; net worth is assets less debts.
# Business ownership from the raw business section (BUSINESS_ITEMS below):
# owns_business (owns any private business) and k1_owner (a partnership,
# S corporation or LLC interest), household answers, each given to the units
# holding the household's business equity, or to the respondent's unit when
# that equity is zero.
# Auto loan interest (auto_int_exp): the household's vehicle loans, up to four (the SCF's own vehicles 1-4: the amount
# still owed, X2218, X2318, X2418, X7169, times the current annual rate in hundredths of a percent, X2219, X2319,
# X2419, X7170, over 10,000; Tedeschi's car interest code, Reports/2024/Car-Interest). The rate's code -1 (no
# interest charged) and any other negative code read as 0. The items and their codes are the same in the 2013, 2016,
# 2019 and 2022 waves (checked 26 October). Interview-year dollars, a year's interest at the balance and rate of the
# interview. A household debt: divided among the PEU's units as build_scf.R divides installment debt (all of it to a
# unit holding the whole PEU, else by the earnings shares), and none to units outside the PEU (the group's vehicle
# loans are not asked; its debts are one item, X6439) or to other units.
# Elective deferrals (elective_deferrals, since 28 October; settings deferrals): each of the respondent's and the
# spouse's or partner's contributions to up to two plans on the current main job (X11040-X11046, X11140-X11146 for the
# respondent; X11340-X11346, X11440-X11446 for the spouse or partner; the same items in every wave): a plan's yearly
# amount is its amount per period times periods per year (aconv) where given, else its percent of pay (hundredths of a
# percent; above 100% read as a code, not a share) times the person's yearly pay on the main job; traditional and Roth
# together, as the SCF asks them. The interview year's contributions. Each person's total is held to the legal limit
# of the tax year (the wave's income year; the catch-up limit from age 50 at the interview), then the unit's to its
# wages. A unit holding the whole PEU takes both people's, the respondent's part the respondent's, the spouse's or
# partner's part theirs; units outside the PEU and other units are not asked: missing (NA), not zero. Each person's
# amounts before and after the limit are also written to scf_deferral_persons_{wave}.csv.gz (for the checks against
# SOI's W-2 tables).
# Ages and sexes of the unit's head and spouse (age1, male1; age2, male2 on
# joint units, else -1; male missing (NA, not observed) where the SCF does not
# report sex: members other than the respondent and the spouse or partner), for the primary
# filer's age range (ages.py). n_dep: the unit's dependents, children living
# elsewhere included, at most 3. The module's flags are carried as any_*.
#
# Paths and waves from settings.json; the settings file is REBUILD_SETTINGS if
# set (run.sbatch sets it), else the pipeline's own.
# Writes {out}/data/scf_units_{wave}.csv.gz, one row per tax unit and
# implicate. Run from the repository root through Slurm (run.sbatch, stage scf).
#-------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(data.table); library(haven)
  library(jsonlite)
})
settings = fromJSON(Sys.getenv('REBUILD_SETTINGS', 'research/rebuild/pipeline/settings.json'))
OUT = file.path(Sys.getenv('REBUILD_OUT', settings$paths$out), 'data')
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
source('src/imputations/wealth_schema.R')       # the component names only

minus_one_to_zero = function(x) if_else(x == -1, 0, x)
# Business section: owns a private business (X3103), owns a business not actively managed (X3401); legal
# form of actively managed businesses 1 and 2 (X3119, X3219: 1 partnership, 3 S corporation, 11 and 12
# limited partnership, LLP or LLC); not actively managed limited partnerships (X3407), other partnerships
# (X3411), LLCs (X3451) and S corporations (X3415).
BUSINESS_ITEMS = c('x3103', 'x3401', 'x3119', 'x3219', 'x3407', 'x3411', 'x3451', 'x3415')
K1_FORMS = c(1, 3, 11, 12)
INCOME = c(wages = 'X5702', interest = 'X5708', dividends = 'X5710', capital_gains = 'X5712',
           business_income = 'X5704', rent_royalty_estate = 'X5714', retirement_gross = 'X5722',
           tax_exempt_interest = 'X5706')
# Items for the respondent's and the spouse's or partner's own incomes (Moore's code, as production's builder)
PERSON_ITEMS = c('X4112', 'X4113', 'X4712', 'X4713', 'X4131', 'X4132', 'X4731', 'X4732',
                 'X5315', 'X5317', 'X5318', 'X5319', 'X5323', 'X5325', 'X5326', 'X5327',
                 'X5331', 'X5333', 'X5334', 'X5335', 'X5415', 'X5417', 'X5418', 'X5419',
                 'X6558', 'X6566', 'X6574', 'X6464', 'X6465', 'X6469', 'X6470', 'X6474', 'X6475', 'X6479', 'X6480',
                 'X6965', 'X6966', 'X6971', 'X6972', 'X6977', 'X6978', 'X6983', 'X6984',
                 'X5606', 'X5614', 'X5622', 'X5630', 'X5724', 'X5725')
AUTO_LOANS = c(x2218 = 'x2219', x2318 = 'x2319', x2418 = 'x2419', x7169 = 'x7170')   # balance = rate
# Elective deferral items: per person, two plan blocks; in each, contributes? (+40), percent and amount and frequency
# (+41-43), and the same asked a second way (+44-46)
DEFERRAL_BLOCKS = list(r = c(11000, 11100), s = c(11300, 11400))
DEFERRAL_ITEMS = unlist(lapply(unlist(DEFERRAL_BLOCKS), function(b) sprintf('X%d', b + 40:46)))
HOUSING = c('primary_home', 'other_home', 're_fund', 'primary_mortgage', 'other_mortgage',
            'kg_primary_home', 'kg_other_re')

# Moore's annualizing factor for a period code (codes not listed give 0, his quirk, kept)
aconv = function(f) {
  case_when(f == 2 ~ 52.18, f == 3 ~ 26.09, f == 4 ~ 12, f == 5 ~ 4, f %in% c(6, 8, 14, 22) ~ 1,
            f == 11 ~ 2, f == 12 ~ 6, f == 31 ~ 24, TRUE ~ 0)
}

# The respondent's and the spouse's or partner's labor, business and total own income (production builder,
# Moore's lines 638-656): labor X4112/X4712, business from the main job X4131/X4731, regular pensions in four
# slots, account withdrawals.
person_incomes = function(h) {
  pen = function(who) {
    pmax(0, h$X5318 * aconv(h$X5319) * (h$X5315 == who) * (h$X5317 >= 1)) +
      pmax(0, h$X5326 * aconv(h$X5327) * (h$X5323 == who) * (h$X5325 >= 1)) +
      pmax(0, h$X5334 * aconv(h$X5335) * (h$X5331 == who) * (h$X5333 >= 1)) +
      pmax(0, h$X5418 * aconv(h$X5419) * (h$X5415 == who) * (h$X5417 >= 1))
  }
  wd = function(who, own) {
    own + 0.5 * h$X6574 +
      pmax(0, h$X6464 * aconv(h$X6465) * (h$X5315 == who)) + pmax(0, h$X6469 * aconv(h$X6470) * (h$X5323 == who)) +
      pmax(0, h$X6474 * aconv(h$X6475) * (h$X5331 == who)) + pmax(0, h$X6479 * aconv(h$X6480) * (h$X5415 == who)) +
      pmax(0, h$X6965 * aconv(h$X6966) * (h$X5606 == who)) + pmax(0, h$X6971 * aconv(h$X6972) * (h$X5614 == who)) +
      pmax(0, h$X6977 * aconv(h$X6978) * (h$X5622 == who)) + pmax(0, h$X6983 * aconv(h$X6984) * (h$X5630 == who)) +
      0.5 * h$X5724 * (h$X5725 == 11)
  }
  tibble(Y1 = h$Y1,
         r_lab = pmax(0, h$X4112) * aconv(h$X4113), s_lab = pmax(0, h$X4712) * aconv(h$X4713),
         r_bus = pmax(0, h$X4131) * aconv(h$X4132), s_bus = pmax(0, h$X4731) * aconv(h$X4732)) %>%
    mutate(r_tot = r_lab + r_bus + pen(1) + wd(1, h$X6558), s_tot = s_lab + s_bus + pen(2) + wd(2, h$X6566))
}

# The respondent's share of a split PEU (the spouse's or partner's is 1 minus it); a half when both are 0
r_share = function(r, s) if_else(r + s == 0, 0.5, r / pmax(r + s, 1e-9))

for (wave in settings$paths$raw$scf_waves) {
  dir = sub('{wave}', wave, settings$paths$raw$scf, fixed = TRUE)
  raw_path = file.path(dir, sprintf('p%02di6.dta', wave %% 100))
  scfp = fread(file.path(dir, sprintf('SCFP%d.csv', wave))) %>% as_tibble()
  hh = read_dta(raw_path, col_select = all_of(c('y1', 'yy1', 'x42001', 'x3902', tolower(INCOME), BUSINESS_ITEMS,
                                                 tolower(PERSON_ITEMS), names(AUTO_LOANS), unname(AUTO_LOANS),
                                                 tolower(DEFERRAL_ITEMS), 'x14', 'x19'))) %>%
    mutate(across(everything(), as.numeric))
  names(hh) = toupper(names(hh))
  stopifnot(!anyDuplicated(hh$Y1), !anyNA(hh), nrow(hh) == nrow(scfp))
  hh = hh %>% mutate(across(all_of(unname(INCOME)), minus_one_to_zero))
  stopifnot(all(hh$X5702 >= 0), all(hh$X5706 >= 0), all(hh$X5708 >= 0), all(hh$X5710 >= 0), all(hh$X5722 >= 0))
  hh$auto_int_exp = 0
  for (b in names(AUTO_LOANS)) {
    bal = hh[[toupper(b)]]; rate = hh[[toupper(AUTO_LOANS[[b]])]]
    stopifnot(all(bal >= 0))
    hh$auto_int_exp = hh$auto_int_exp + bal * pmax(rate, 0) / 10000
  }
  cat(sprintf('wave %d: auto loan interest $%.2f billion, households with any %.2f million (rate codes below 0 read as 0: %d)\n',
              wave, sum(hh$X42001 / 5 * hh$auto_int_exp) / 1e9, sum(hh$X42001[hh$auto_int_exp > 0] / 5) / 1e6,
              sum(sapply(unname(AUTO_LOANS), function(r) sum(hh[[toupper(r)]] < 0)))))

  # the asset price factor: SAVBND / X3902, the same on every household
  fx = scfp %>% select(Y1, SAVBND) %>% inner_join(hh, by = 'Y1') %>% filter(X3902 > 0)
  asset_factor = median(fx$SAVBND / fx$X3902)
  stopifnot(max(abs(fx$SAVBND / fx$X3902 / asset_factor - 1)) < 1e-3)

  # the household's (PEU's) dollars: income items, the 20 components and 4 gains in the interview year's dollars
  peu = scfp %>% transmute(
    Y1, cash = LIQ + CDS, equities = STOCKS + STMUTF + COMUTF, bonds = BOND + SAVBND + TFBMUTF + GBMUTF + OBMUTF,
    dc = IRAKH + THRIFT, db = FUTPEN + CURRPEN, life_ins = CASHLI, annuities = ANNUIT, trusts = TRUSTS,
    other_fin = OTHFIN + OMUTF, pass_throughs = BUS, primary_home = HOUSES, other_home = ORESRE, re_fund = NNRESRE,
    other_nonfin = VEHIC + OTHNFIN, primary_mortgage = MRTHEL, other_mortgage = RESDBT, credit_lines = OTHLOC,
    credit_cards = CCBAL, installment_debt = INSTALL, other_debt = ODEBT, kg_primary_home = KGHOUSE,
    kg_other_re = KGORE, kg_pass_throughs = KGBUS, kg_other = KGSTMF) %>%
    mutate(across(-Y1, ~ .x / asset_factor))
  stopifnot(setequal(setdiff(names(peu), 'Y1'), c(wealth_asset_vars, wealth_debt_vars, wealth_kg_vars)))
  names(peu)[-1] = paste0('w_', names(peu)[-1])
  WCOLS = setdiff(names(peu), 'Y1')
  peu = peu %>% inner_join(hh %>% select(Y1, all_of(unname(INCOME)), auto_int_exp), by = 'Y1') %>%
    inner_join(person_incomes(hh), by = 'Y1')
  stopifnot(nrow(peu) == nrow(hh), !anyNA(peu))

  # elective deferrals per person (see the top), then held to the year's legal limit
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
      path = function(pct, amt, frq) {
        a = pmax(x(amt), 0) * aconv(x(frq))
        p = pmax(x(pct), 0) / 10000
        p = if_else(p <= 1, p, 0)                       # a share above all of pay is a code, not a share
        if_else(a > 0, a, p * pay[[who]])
      }
      p1 = path(41, 42, 43); p2 = path(44, 45, 46)
      tot = tot + if_else(x(40) == 1, if_else(p1 > 0, p1, p2), 0)   # 1 yes, 5 no, 3 other (not counted)
    }
    elective[[who]] = tot
  }
  limit = lapply(age, function(a) if_else(a >= settings$deferrals$catch_up_age, lim[2], lim[1]))
  dper = tibble(Y1 = hh$Y1, weight = hh$X42001 / 5, r_age = hh$X14, s_age = hh$X19, r_pay = pay$r, s_pay = pay$s,
                r_reported = elective$r, s_reported = elective$s,
                r_elective = pmin(elective$r, limit$r), s_elective = pmin(elective$s, limit$s), X5702 = hh$X5702)
  stopifnot(!anyNA(dper))
  cat(sprintf(paste0('wave %d: elective deferrals, respondents and spouses or partners: $%.1f billion reported, $%.1f billion ',
                     'after the %d limit ($%s, $%s from %d); people above the limit %.2f m\n'),
              wave, sum(dper$weight * (dper$r_reported + dper$s_reported)) / 1e9,
              sum(dper$weight * (dper$r_elective + dper$s_elective)) / 1e9, year, format(lim[1], big.mark = ','),
              format(lim[2], big.mark = ','), settings$deferrals$catch_up_age,
              sum(dper$weight * ((dper$r_reported > dper$r_elective) + (dper$s_reported > dper$s_elective))) / 1e6))
  fwrite(dper, file.path(OUT, sprintf('scf_deferral_persons_%d.csv.gz', wave)))
  peu = peu %>% inner_join(dper %>% select(Y1, r_elective, s_elective), by = 'Y1')
  stopifnot(nrow(peu) == nrow(hh))

  # the module's units
  mu = fread(file.path(OUT, sprintf('scf_units_module_%d.csv.gz', wave))) %>% as_tibble()
  nd = fread(file.path(OUT, sprintf('scf_npeu_dollars_%d.csv.gz', wave))) %>% as_tibble()
  stopifnot(!anyDuplicated(mu$uid), all(mu$peu_dollars %in% c('all', 'r', 'sp', 'none')))
  # each household has one unit holding the whole PEU, or one 'r' and one 'sp' unit
  k = mu %>% filter(source == 'peu') %>% count(hh, peu_dollars) %>%
    pivot_wider(names_from = peu_dollars, values_from = n, values_fill = 0)
  for (v in c('all', 'r', 'sp')) if (!(v %in% names(k))) k[[v]] = 0L
  stopifnot(all((k$all == 1 & k$r == 0 & k$sp == 0) | (k$all == 0 & k$r == 1 & k$sp == 1)), nrow(k) == nrow(hh))

  # PEU units: shares by kind of item
  p = mu %>% filter(source == 'peu') %>% select(uid, hh, peu_dollars) %>%
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
  for (v in WCOLS) p[[v]] = p[[v]] * (if (sub('^w_', '', v) %in% HOUSING) p$sh_house else p$sh_earn)
  p$auto_int_exp = p$auto_int_exp * p$sh_earn     # as installment debt (w_installment_debt)
  # elective deferrals: both people's to a unit holding the whole PEU, each person's own to a split unit; then held to
  # the unit's wages (the plan's money comes out of them)
  p$elective_deferrals = case_when(p$peu_dollars == 'all' ~ p$r_elective + p$s_elective,
                                    p$peu_dollars == 'r' ~ p$r_elective, TRUE ~ p$s_elective)
  over_wages = p$elective_deferrals > p$wages
  cat(sprintf('wave %d: PEU units with elective deferrals above their wages (held to their wages): %d rows\n',
              wave, sum(over_wages)))
  p$elective_deferrals = pmin(p$elective_deferrals, p$wages)
  p = p %>% mutate(taxunit_code = case_when(peu_dollars == 'all' ~ 0L, peu_dollars == 'r' ~ 1L, TRUE ~ 2L)) %>%
    select(uid, all_of(names(INCOME)), all_of(WCOLS), auto_int_exp, elective_deferrals, taxunit_code)

  # units outside the PEU: their members' shares of the group; other units: nothing
  npeu = mu %>% filter(source == 'npeu') %>% select(uid) %>% left_join(nd, by = 'uid')
  stopifnot(!anyNA(npeu$wages))
  for (v in setdiff(c(names(INCOME), WCOLS, 'auto_int_exp'), names(npeu))) npeu[[v]] = 0
  npeu$elective_deferrals = NA_real_                 # not asked outside the PEU: missing, not zero
  npeu = npeu %>% mutate(taxunit_code = 9L) %>%
    select(uid, all_of(names(INCOME)), all_of(WCOLS), auto_int_exp, elective_deferrals, taxunit_code)
  other = mu %>% filter(source == 'other') %>% select(uid)
  for (v in c(names(INCOME), WCOLS, 'auto_int_exp')) other[[v]] = 0
  other$elective_deferrals = rep(NA_real_, nrow(other))   # not asked: missing, not zero
  other$taxunit_code = rep(8L, nrow(other))
  dollars = bind_rows(p, npeu, other)
  stopifnot(nrow(dollars) == nrow(mu), !anyDuplicated(dollars$uid))
  dollars$net_worth = rowSums(as.matrix(dollars[, paste0('w_', wealth_asset_vars)])) -
    rowSums(as.matrix(dollars[, paste0('w_', wealth_debt_vars)]))

  # Business ownership: to the units holding the household's business equity, else the respondent's unit
  biz = hh %>% transmute(hh = Y1, owns = X3103 == 1 | X3401 == 1,
                         k1 = X3119 %in% K1_FORMS | X3219 %in% K1_FORMS | X3407 == 1 | X3411 == 1 |
                           X3451 == 1 | X3415 == 1)
  stopifnot(all(biz$owns | !biz$k1))
  ub = mu %>% filter(source == 'peu') %>% select(uid, hh, peu_dollars) %>%
    inner_join(dollars %>% select(uid, w_pass_throughs), by = 'uid') %>%
    left_join(biz, by = 'hh') %>% group_by(hh) %>%
    mutate(holder = if (any(w_pass_throughs > 0)) w_pass_throughs > 0 else peu_dollars %in% c('all', 'r')) %>%
    ungroup()
  stopifnot(!anyNA(ub$owns), all((ub %>% group_by(hh) %>% summarise(n = sum(holder)))$n >= 1))

  units = mu %>% inner_join(dollars, by = 'uid') %>%
    left_join(ub %>% transmute(uid, owns_business = as.integer(owns & holder), k1_owner = as.integer(k1 & holder)),
              by = 'uid') %>%
    mutate(across(c(owns_business, k1_owner), ~ replace_na(.x, 0L))) %>%     # outside the PEU, other units: none
    transmute(record_id = if_else(separate, uid, uid * 10), household_id = hh %/% 10,
              implicate = as.integer(hh %% 10), year = wave - 1L, weight, married = as.integer(filing_status == 'joint'),
              n_dep = as.integer(pmin(n_dep, 3)), age1 = as.integer(head_age),
              age2 = as.integer(if_else(filing_status == 'joint', spouse_age, -1L)),
              male1 = if_else(head_sex == 0, NA_integer_, as.integer(head_sex == 1)),
              male2 = if_else(filing_status == 'joint', if_else(spouse_sex == 0, NA_integer_, as.integer(spouse_sex == 1)), -1L),
              taxunit_code, across(all_of(names(INCOME))), net_worth, asset_factor = asset_factor,
              across(all_of(WCOLS)), auto_int_exp, elective_deferrals, owns_business, k1_owner, filing_status, n_child_away,
              group_level = as.integer(group_level), npeu_people,
              any_age_assumed = as.integer(any_age_assumed), any_link_assumed = as.integer(any_link_assumed),
              any_income_assumed = as.integer(any_income_assumed),
              any_dep_by_support = as.integer(any_dep_by_support),
              any_claimed_in_group = as.integer(any_claimed_in_group))
  # sex is allowed missing (not observed outside the respondent and the spouse or partner), and elective deferrals
  # (not asked outside the PEU)
  stopifnot(nrow(units) == nrow(mu), !anyDuplicated(units$record_id), !anyNA(select(units, -male1, -male2, -elective_deferrals)),
            all(is.na(units$elective_deferrals) == (units$taxunit_code > 2)),
            all(units$age2[units$married == 1] >= 0),
            all(units$taxunit_code[is.na(units$male1) | is.na(units$male2)] > 2))

  # the PEU's dollars are divided between units, never created or lost
  for (v in c(names(INCOME), WCOLS, 'auto_int_exp')) {
    a = sum(hh$X42001 / 5 * peu[[if (v %in% names(INCOME)) INCOME[[v]] else v]])
    z = sum((units$weight * units[[v]])[units$taxunit_code <= 2])
    stopifnot(abs(z - a) <= 1e-6 * max(1, abs(a)))
  }
  fwrite(units, file.path(OUT, sprintf('scf_units_%d.csv.gz', wave)))
  cat(sprintf('wave %d: %d unit rows, %.2f m units (outside the PEU %.2f m); PEU totals kept; asset factor %.6f\n',
              wave, nrow(units), sum(units$weight) / 1e6, sum(units$weight[units$taxunit_code == 9]) / 1e6,
              asset_factor))
  print(units %>% group_by(taxunit_code) %>% summarise(units_m = sum(weight) / 1e6, rows = n()))
}
