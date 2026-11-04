"""The training table: the PUF's filers, four SCF waves, six CEX years and five CPS years, stacked, each amount
cut into bands, plus the dollar maps.

Rows, in this order:
  PUF filers      the PUF (the PUF year, settings "years") without its four aggregate
                  records; weight S006/100; filer = 1 and filer_p = 1. The PUF holds
                  filers only: non-filers come from the CPS.
  SCF waves       2016, 2019, 2022, then 2013 (tax years 2015, 2018, 2021, 2012),
                  tax units from build_scf.R, all five implicates
  CEX years       the interview survey's tax units from build_cex.R (settings "cex"),
                  one row per tax unit and quarterly interview, spending annualized;
                  dependent filers are folded into the unit that claims them
  CPS years       the CPS ASEC's tax units from build_cps.R (settings "cps"), every
                  tax unit, filers and non-filers, dependent filers folded in; filer_p is
                  the unit's calibrated filing chance (training draws filer from it in
                  every pass), filer one draw from it (kept for the checks and reports).
                  The CPS rows carry every PUF amount in PUF terms, each from its row
                  of the translation table (zero for what the CPS does not ask), so
                  every PUF amount is present on the CPS.

Variables. married, n_dep (capped at 3), filer, and, on the PUF and the CPS (-1 on the SCF and CEX), mars (filing
status: 1 single, 2 joint with surviving spouses, 3 separate, 4 head of household), n_dep_ctc (child credit children,
PUF N24) and n_dep_eitc (EITC children, PUF EIC), each 0-3 (3 = 3 or more); filer_p (-1 on the SCF and CEX); the amounts:
  wages E00200, taxable interest E00300, ordinary dividends E00600;
  capital gains before the $3,000 loss limit (since 29-30 October; John, 29 October) and its three pieces: short
    term P22250 and long term P23250, the full Schedule D gains and losses, and distributions E01100; the total is
    their sum. The limit is not drawn: AGI applies it (common.allowed_capital_gain), as the calculator does. The
    third piece of before, kg_other (line 13 less short and long term, almost entirely the part of a loss the
    limit holds back) is gone; what is left of line 13 once the limit is applied (E01000 less the allowed
    Schedule D net; zero on nearly every return) stays in other_agi, so AGI is the PUF's exactly;
  business income E00900 + E02000 + E02100 and its eighteen parts (PARTS), and
    two subtotals of it shared with the SCF: Schedules C and F (sch_cf_income;
    SCF X5704) and Schedule E (sch_e_income; SCF X5714, which is where the SCF
    puts partnership and S corporation income);
  age_range: the primary filer's age range (ages.py). On the PUF the
    demographic file's range, or, where it has none, -1 and step 0's
    probabilities (age_p0-age_p8), from which training draws; on the SCF, CEX
    and CPS, the range of the primary filer's age, couples by the
    primary-filer rule;
  the other AGI items (ITEMS), and other_agi = AGI minus everything above and minus taxable Social Security (E02500),
    which is not drawn: the file computes it by the section 86 worksheet (dollars.retirement_items; 3 November);
  pensions in two parts: taxable pensions E01700 (in AGI) and the non-taxable rest E01500 - E01700, never below
    zero (nontxbl_pens_dist, not in AGI); gross pensions are their sum, computed, never drawn (3 November; until
    then gross Social Security plus total pensions, E02400 + E01500, was a slot);
  gross Social Security E02400 and tax-exempt interest (not in AGI);
  mortgage interest paid E19200 and real estate tax E18500 (itemizers only;
    PUF only, not in AGI);
  since stage 3 (27 October), production's other PUF items (STAGE3; PUF only, absent on the SCF, CEX and SIPP, not
    measured by the CPS): ordinary dividends as qualified (div_pref, E00650) plus other (div_ord), the adjustments
    as their eleven parts (ADJ_PARTS), both totals computed from the parts; unrecaptured section 1250 gain, 28%
    collectibles gain, partnership income subject to self-employment tax (part_se, production's formula), the
    itemized deduction items, the two AMT items, the credits and the two other taxes (NEW_ITEMS). On the CPS the
    dividends go to div_pref (div_ord zero; the translation table); the adjustments, which the CPS never asked
    (zero until stage 3), are absent on the CPS like the other new items (generation gives non-filers none);
  k1_owner: any partnership or S corporation part nonzero (on the SCF: a
    partnership, S corporation or LLC interest, from build_scf.R);
  agi_group, the AGI group label (qgroups.py; agi_group_label): on the PUF the record's band-AGI group, one of 12,
    computed from its bands after the maps are built (AGI from the representative amount of each band, on the
    weighted rank lines of band-AGI among the PUF year's returns); -1 elsewhere;
  SCF only: the 20 balance-sheet components (net worth is computed from them),
    with the home mortgage carried as its share of the home's value
    (w_primary_ltv); the 4 unrealized gains; owns_business.
  CEX only: the 8 consumption categories (CEX_CATS). The CEX shares married,
    n_dep, the age range, wages, gross_ss (gross Social Security and
    railroad retirement; on the PUF E02400) and sch_cf_income
    (self-employment income); every other slot is absent on it. Pensions are
    left out: the PUF's total pensions (E01500) include rollovers and lump
    sums, the CEX's do not, so a combined slot put PUF retirees in higher
    bands than their CEX counterparts.
  exact ages of the primary filer and the spouse (EXACT_AGES), on the SCF,
    the CEX and the CPS, absent on the PUF: 0 none, else the age plus 1, ages
    80 and over as 80 (exact_age_value); the primary filer by the rule the age
    range uses, so the exact age always lies in the age range;
  CPS only (CPS_AMOUNTS, CPS_CATEGORIES): the spouse's share of the couple's
    wages and of its self-employment income, each person's weeks worked and
    usual weekly hours, childcare, each adult's overtime pay (since 28 October;
    missing on the units not asked); exact ages of up to three dependents, and
    the two adults' sex (cps_person_slots).
    What the CPS shares with the PUF is in the translation table.
On the PUF the parts add to the totals and the items to AGI, in whole dollars
(checked).

Bands. Each amount is cut into 84 bands: 0 = exactly zero, 1-10 losses, 11-83
positives (BAND_SHARES), cut on the PUF filers' weighted distribution. The amounts a survey shares
with the PUF come from the translation table (settings "translation",
translate.py): for each survey year, the tax data for that year (tax_data),
the row's definition fixes and function 1, then the PUF's cut points grown to
that year per return. A computed total (capital gains, business income and its two subtotals) that a survey has no
row for, but has rows for all its parts, is computed from the translated parts, and the partnership flag k1_owner
likewise. The report is data/translation_report.csv; each year's tax data is data/tax_data_<year>.parquet. The SCF's
balance sheet is cut within each wave, the CEX's categories and the CPS's own amounts within each year.
SCF ids of tax years 2018 and 2021 are offset by 1e8 x (year - 2015), the 2013
wave's by 2.013e12, so every id is unique (the held-back
slice is drawn by household id).

The dollar maps: for each amount and band, every record's value and weight; a
band with no record gets a fill value.
  PUF side    the PUF year's records; the target year's map grows each value by
              the item's line in the target year's growth.yaml (growth.py:
              SOI's dollars per return with the item where SOI Table 1.4 has it,
              else a declared series exogenous to tax policy; growth_factors.csv);
  SCF         the balance sheet, one map per wave from that wave's own records
              (generation draws each record's balance sheet from one wave), for
              the target year only: each item grown per tax unit from its wave to
              the target year's wave (the wave of income year target - 1), by the
              ratio of the two waves' weighted means per unit, each in its own
              dollars (scf_growth; scf_growth_factors.csv). The mortgage's share
              of the home's value is a ratio and is not grown;
  CEX         the generation year's records; the target year's map grows them by
              NIPA PCE per return (growth.yaml);
  CPS         the generation year's records; the target year's map grows childcare
              and overtime by SOI's wages per return, and leaves the shares, weeks
              and hours (growth.yaml).

Writes common.TABLE, common.EDGES, common.MAPS/map_puf_{PUF year, target
year}.parquet, map_scf_wave<wave>_<target year>.parquet, map_cex_ and map_cps_{generation year, target year}.parquet,
growth_factors.csv and scf_growth_factors.csv; data/translation_report.csv; data/scf_units_<target year>.parquet (the tilt's wealth
targets), data/cex_units_<target year>_all.parquet (the consumption
targets) and data/cps_units_<target year>_all.parquet (report_cps.py).
"""
import copy
import json
import os

import numpy as np
import pandas as pd

import ages
import common
import growth as GR
import targets as TG
import translate as TR
from common import PUF_YEAR, TARGET_YEAR

RAW = common.RAW
# ---- constants -------------------------------------------------------------------
WAVE_2013_ID_OFFSET = 2.013e12
# band layout: 0 zero; 1-10 losses (shares of loss weight from the least negative); 11-83 positives
N_NEG, NEG0, POS0 = 10, 1, 11
NEG_SHARES = np.array([0.225, 0.45, 0.675, 0.90, 0.93, 0.96, 0.99, 0.995, 0.999])
POS_SHARES = np.round(np.concatenate([0.02 * np.arange(1, 46), 0.90 + 0.01 * np.arange(1, 10),
                                      0.99 + 0.001 * np.arange(1, 10), 0.999 + 0.0001 * np.arange(1, 10)]), 6)
N_BANDS = POS0 + len(POS_SHARES) + 1
assert N_BANDS == 84

KG_PIECES = ['kg_st', 'kg_lt', 'kg_dist']   # before the loss limit (29-30 October; kg_other, the limit itself, dropped)
# the AGI items: PUF fields, sign in AGI (0: not in AGI), whether the SCF has them, the SCF banding rule
ADJ_FIELDS = ['E03150', 'E03210', 'E03220', 'E03230', 'E03240', 'E03260', 'E03270', 'E03290',
              'E03300', 'E03400', 'E03500']
ITEMS = {  # item: (sign in AGI, on the SCF, SCF rule). The order fixes the model's slot order.
    'business_income': (1, False, None), 'sch_cf_income': (0, True, 'rank'), 'sch_e_income': (0, False, None),
    # (28 October) Schedule E's two parts the SCF shares in place of the whole: rents and royalties (sche_rent, the
    # SCF's property income) and the rest (sche_rest: partnerships, S corporations, estates and trusts, farm rental)
    'sche_rent': (0, True, 'percentile'), 'sche_rest': (0, True, 'percentile'),
    'other_gains': (1, False, None), 'txbl_ira_dist': (1, False, None),
    # (3 November, John) pensions in two parts, both drawn: taxable pensions, and the non-taxable rest of total
    # pensions (E01500 - E01700, never below zero; not in AGI). Gross pensions (gross_pens_dist) are their sum and
    # taxable Social Security comes from the section 86 worksheet, both computed (dollars.retirement_items); the slots
    # retirement_gross (E02400 + E01500) and ss_taxable (E02500) are gone
    'txbl_pens_dist': (1, False, None), 'nontxbl_pens_dist': (0, False, None), 'ui': (1, False, None),
    'alimony': (1, False, None), 'state_ref': (1, False, None), 'adjustments': (-1, False, None),
    'exempt_int': (0, True, 'unchanged'),
    'first_mort_int': (0, False, None), 'salt_prop': (0, False, None),
    'gross_ss': (0, True, 'unchanged')}   # gross Social Security, not in AGI; shared with the CEX, the CPS, the SIPP and
# (since 28 October, in place of Social Security plus pensions) the SCF
PARTS = {  # part: (PUF field, sign in business income); loss fields are stored positive
    'sole_prop': ('E00900', 1), 'part_passive': ('E25940', 1), 'part_active': ('E25980', 1),
    'part_passive_loss': ('E25920', -1), 'part_active_loss': ('E25960', -1), 'part_179': ('E26110', -1),
    'scorp_passive': ('E26170', 1), 'scorp_active': ('E26190', 1), 'scorp_passive_loss': ('E26160', -1),
    'scorp_active_loss': ('E26180', -1), 'scorp_179': ('E26100', -1), 'rent': ('E25850', 1),
    'rent_loss': ('E25860', -1), 'estate': ('E26390', 1), 'estate_loss': ('E26400', -1),
    'farm_rent': ('E27200', 1), 'sche_other': (None, 1), 'farm': ('E02100', 1)}
# Stage 3 (27 October): the PUF items production carries (config/variable_guide/baseline.csv, src/process_puf.R)
# that the model did not draw, with production's names. Every one is a PUF-only amount outside AGI, drawn like the
# other PUF-only items (absent on the SCF, CEX and SIPP; not measured by the CPS, so zero on non-filers). Ordinary
# dividends and the statutory adjustments become totals computed from their parts (COMPUTED), so the totals the
# file had stay exactly the sums of their parts.
DIV_PARTS = {'div_pref': 'E00650', 'div_ord': None}     # qualified dividends; ordinary less qualified (E00600 - E00650)
ADJ_PARTS = dict(zip(['trad_contr_ira', 'sl_int_ded', 'ed_exp', 'tuition_ded', 'dpad', 'se_tax_ded', 'se_health',
                      'hsa_contr', 'keogh_contr', 'early_penalty', 'alimony_exp'], ADJ_FIELDS))
NEW_ITEMS = {  # item: PUF field (None: derived, as production derives it, in puf_rows)
    'kg_1250': 'E24515', 'kg_collect': 'E24518', 'part_se': None,
    'med_exp': 'E17500', 'salt_inc_sales': 'E18400', 'char_cash': 'E19800', 'char_noncash': 'E20100',
    'casualty_exp': 'E20500', 'job_exp': 'E20550', 'tax_prep_exp': 'E20600', 'other_misc_exp': 'E20400',
    'other_item_exp': None,
    'amt_nols': 'P60100', 'amt_ftc': 'E62900',
    'ftc': 'E07300', 'aoc_exp': 'P87482', 'llc_exp': 'E87530', 'old_cred': 'E07200', 'savers_cred': 'E07240',
    'res_energy_cred': 'E07260', 'gbc': 'E07400', 'prior_yr_cred': 'E07600', 'other_nonref': 'P08000',
    'net_ptc': 'E11560', 'excess_ptc': 'E85785', 'recapture_tax': 'E09700', 'ira_penalty': 'E09900'}
STAGE3 = list(DIV_PARTS) + list(ADJ_PARTS) + list(NEW_ITEMS)   # the new slots, in slot order
# How each is grown to another year is its line in the year's growth.yaml (growth.py): Table 1.4 grows the dividend
# parts, the adjustments, the two capital gain items (with Schedule D net gain) and part_se (with partnership and S
# corporation income); the items with no Table 1.4 line (itemized deductions, AMT items, credits and other taxes) are
# grown, since 29 October, by a declared series exogenous to tax policy (until then they stayed in PUF-year dollars).
SCHE_PARTS = [p for p, (f, _) in PARTS.items() if f and p not in ('sole_prop', 'farm')]
CF_PARTS = ['sole_prop', 'farm']                                  # Schedules C and F: the SCF's X5704
E_PARTS = [p for p in PARTS if p not in CF_PARTS]             # Schedule E (before 28 October the SCF's X5714)
RENT_PARTS = ['rent', 'rent_loss']                        # rents and royalties: the SCF's rental income
REST_PARTS = [p for p in E_PARTS if p not in RENT_PARTS]      # the rest of Schedule E: X5714 less rental
K1_PARTS = [p for p in PARTS if p.startswith(('part_', 'scorp_'))]
# The balance sheet (production's wealth_schema.R): net worth is computed from the 20 components. The home
# mortgage is carried as its share of the home's value (w_primary_ltv), so it is drawn after the home.
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin', 'pass_throughs',
          'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
KG_WEALTH = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
NET_WORTH_PARTS = {**{'value.' + c: 1 for c in ASSETS}, **{'value.' + c: -1 for c in DEBTS}}
SCF_AMOUNTS = ['value.' + c for c in ASSETS] + ['w_primary_ltv'] + ['value.' + c for c in DEBTS if c != 'primary_mortgage'] \
    + ['w_' + c for c in KG_WEALTH] + ['auto_int_exp', 'elective_deferrals', 'spouse_deferral_share']
# auto_int_exp (since 26 October): the unit's auto loan interest, a year's interest on its vehicle loans at the
# interview's balance and rate (build_scf.R). Not part of net worth.
# elective_deferrals (since 28 October): the unit's elective deferrals to 401(k)-type plans, traditional plus Roth, as
# the SCF reports them for the respondent and the spouse or partner, each person's held to the legal limit and to
# their own wages (build_scf.R, settings deferrals). spouse_deferral_share (decided 27 October): the spouse's share of
# them, as the CPS's spouse_wage_share is of wages (scf_person_slots); 0 unless married with some deferrals, so
# generation draws it after them and sets it to 0 there (generate.deferral_structure). Both missing (absent, band
# -1) on the units the SCF does not ask: units outside the PEU and other units. Not part of net worth.
# w_dc_stock_share (since 29 October; John, NEXT_STEPS.md item 1, Q6): the share of the household's retirement accounts
# held in stocks, RETEQ / RETQLIQ of the summary file (build_scf.R), carried as a share of value.dc (meta ratios: generation
# draws it after value.dc and sets it to 0 without value.dc; dollars.py makes w_dc_stocks = share x value.dc); missing (absent) on
# the units outside the PEU and other units, as the elective deferrals. non_micro_imputations.unrealized_gains uses it
# in place of production's age rule.
SCF_AMOUNTS = SCF_AMOUNTS + ['w_dc_stock_share']
SCF_NOT_ASKED = ['elective_deferrals', 'spouse_deferral_share', 'w_dc_stock_share']   # missing where the SCF does not ask
SCF_RATIOS = ['w_primary_ltv', 'spouse_deferral_share', 'w_dc_stock_share']   # SCF shares: not grown between waves
# The QBI items (since 29 October; John, NEXT_STEPS.md item 1, Q5; build_scf.R): for each kind of business (Schedule C,
# partnership, S corporation) the industry group of the household's largest business of the kind (a category, 0 none,
# 1-7 the SCF's public groups, 8 no industry code) and the owner's share of its outside workers (an amount, a count of
# workers, never grown between waves). Generation draws each kind's workers after its industry group, the workers zero
# with no industry, and the industry group never none on a record with income of that kind (meta scf_structure).
QBI_KINDS = {'schc': ['sole_prop'],
             'part': ['part_passive', 'part_active', 'part_passive_loss', 'part_active_loss', 'part_179'],
             'scorp': ['scorp_passive', 'scorp_active', 'scorp_passive_loss', 'scorp_active_loss', 'scorp_179']}
SCF_WORKERS = [f'biz_workers_{k}' for k in QBI_KINDS]
SCF_AMOUNTS = SCF_AMOUNTS + SCF_WORKERS
SCF_RATIOS = SCF_RATIOS + SCF_WORKERS                           # (counts, not dollars: not grown either)
SCF_CATEGORIES = {f'biz_industry_{k}': 9 for k in QBI_KINDS}
PUF_AMOUNTS = ['wages', 'txbl_int', 'dividends', 'capital_gains'] + KG_PIECES + list(ITEMS) + list(PARTS) + STAGE3 + \
    ['other_agi']
CEX_CATS = ['c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables', 'c_food_off_premises',
            'c_gasoline', 'c_housing_utilities', 'c_other_services_health']
CEX_AMOUNTS = list(CEX_CATS)
# CPS only (settings "cps"): the spouse's share of the couple's wages and of its self-employment income, each
# person's weeks worked and usual weekly hours, and childcare, as amounts (cut into bands within each year); exact
# ages and sex as categories (CPS_CATEGORIES: the number of values). Every person item is the primary filer's or the
# spouse's, the primary chosen by the rule the age range uses (ages.py).
# Since 28 October, each adult's overtime pay in dollars (CPS_OVERTIME; build_cps.R's ot1, ot2, by
# Tedeschi's rules, overtime_cps.R): at most the person's wages (zero without wages), the spouse's zero unless
# married, and missing (absent) on every unit whose adults were not asked the earnings questions, about three
# quarters of the CPS's units; at generation drawn after the other CPS slots and held to each person's wages less tips.
CPS_OVERTIME = ['ot_primary', 'ot_spouse']
CPS_AMOUNTS = ['spouse_wage_share', 'spouse_se_share', 'weeks_primary', 'weeks_spouse', 'hours_primary',
               'hours_spouse', 'care_exp'] + CPS_OVERTIME
MAX_AGE = 85                                   # the CPS's top code (80 is 80-84, 85 is 85 and over)
AGE_SLOTS = ['age1', 'age2', 'dep_age1', 'dep_age2', 'dep_age3']   # value: 0 none, else the age + 1
# The primary filer's and the spouse's exact ages are shared by the SCF, the CEX and the CPS (translation table), absent
# on the PUF. One coding on every source: exact ages to 79, and 80 for 80 and over (EXACT_AGE_TOP). It is the finest
# all three allow: the CPS codes 80-84 as 80 and 85 and over as 85; the CEX codes everyone from 83 (84 in 2019 and
# 2024) as one value, 87 or 88; the SCF reports ages to 95.
EXACT_AGES = TR.EXACT_AGES
EXACT_AGE_TOP = 80
# Since 28 October (John, NEXT_STEPS.md item 1, Q4): each person's Social Security number status, CPS only
# (build_cps.R's ssn_status1-2 and dep_ssn_status1-3, rule "B2"): 0 none, 1 citizen, 2 noncitizen with a sign of
# lawful status, 3 noncitizen without one (taken to lack a number). The dependents' follow their ages' order. Drawn
# with the other CPS slots, after the ages (model.py's order); dollars.py turns them into ssn1, ssn2, dep_ssn1-3.
SSN_STATUS_SLOTS = ['ssn_status_primary', 'ssn_status_spouse', 'ssn_status_dep1', 'ssn_status_dep2', 'ssn_status_dep3']
DEPENDENT_SSN_SLOTS = ['ssn_status_dep1', 'ssn_status_dep2', 'ssn_status_dep3']   # the k-th: present when n_dep >= k
CPS_CATEGORIES = {**{a: MAX_AGE + 2 for a in AGE_SLOTS if a not in EXACT_AGES},
                  'sex_primary': 3, 'sex_spouse': 3,   # CPS only; sex: 0 none, 1 man, 2 woman
                  **{v: 4 for v in SSN_STATUS_SLOTS}}
PERSON_CATEGORIES = {**{a: EXACT_AGE_TOP + 2 for a in EXACT_AGES}, **CPS_CATEGORIES}   # every person category
SPOUSE_SLOTS = ['age2', 'sex_spouse', 'spouse_wage_share', 'spouse_se_share', 'weeks_spouse', 'hours_spouse',
                'ot_spouse', 'ssn_status_spouse']
DEPENDENT_AGE_SLOTS = ['dep_age1', 'dep_age2', 'dep_age3']   # the k-th: present exactly when n_dep >= k
SHARES = ['spouse_wage_share', 'spouse_se_share']   # only for married couples with some of the item; else absent
# SIPP only (settings "sipp"): each adult's tips in dollars (zero without wages or without tips), amounts cut into
# bands within each year; whether the adult's tips are mostly from leisure and hospitality, a yes/no category (0
# without tips). The spouse's items are zero unless married. The SIPP also reports the spouse's share of the couple's
# wages, which it shares with the CPS (the translation table's row with shared_with "cps").
SIPP_AMOUNTS = ['tips_primary', 'tips_spouse']
SIPP_CATEGORIES = {'tips_lh_primary': 2, 'tips_lh_spouse': 2}
SIPP_SPOUSE_SLOTS = ['tips_spouse', 'tips_lh_spouse']
SIPP_LH_OF = {'tips_lh_primary': 'tips_primary', 'tips_lh_spouse': 'tips_spouse'}
AMOUNTS = PUF_AMOUNTS + SCF_AMOUNTS + CEX_AMOUNTS + CPS_AMOUNTS + SIPP_AMOUNTS
CX = common.SETTINGS['cex']
CP = common.SETTINGS['cps']
CEX_YEAR_ID_OFFSET = 1e10                      # added to build_cex.R's ids (year x 1e10 + interview x 100 + unit)
CPS_ID_OFFSET = 3e12                           # CPS ids: 3e12 + (year - 2000) x 1e9 + build_cps.R's id
SIPP_ID_OFFSET = 4e12                          # SIPP ids: 4e12 + (year - 2000) x 1e10 + roster_sipp.py's id
SP = common.SETTINGS['sipp']
CATEGORIES = {'k1_owner': 'shared', 'owns_business': 'scf'}   # yes/no items beyond married, n_dep, filer
# Categories on the PUF and the CPS (-1 on the SCF and CEX): {slot: number of values}. mars 1-4 (0 unused); n_dep_ctc,
# n_dep_eitc 0-3.
TAX_CATEGORIES = {'filing_status': 5, 'n_dep_ctc': 4, 'n_dep_eitc': 4}
# PUF-only categories: {slot: number of values}. None since the second retrain of 29 October: the primary filer's
# blindness was one (29 October), and is now a rule in the non-microdata step (non_micro_imputations.blindness), with
# the PUF's blind rates by age range read by build_non_micro_tables.puf_blind_rates.
PUF_CATEGORIES = {}


# ---- banding -----------------------------------------------------------------------
def band_variable(x, w, fixed=None):
    """Bands of x (-1 where x is missing) and the edge record: cut points, and per
    band the records, weight, min, max and weighted median. fixed: an edge record
    whose cut points are applied instead of cutting x's own distribution. A value
    equal to a cut goes to the lower-dollar band."""
    band = np.full(len(x), -1, dtype=np.int16)
    ok = ~np.isnan(x)
    band[ok & (x == 0)] = 0
    rec = {'n_bands': N_BANDS, 'zero_band': 0}
    neg, pos = ok & (x < 0), ok & (x > 0)
    if fixed is not None:
        fneg, fpos = fixed['negative'], fixed.get('positive')
        assert fneg['bands_used'] > 0 or not neg.any(), 'losses with no loss cut points'
        assert fpos is not None or not pos.any(), 'positives with no positive cut points'
        if neg.any():
            band[neg] = NEG0 + np.searchsorted(np.array(fneg['cuts']), x[neg], side='left')
        if pos.any():
            band[pos] = POS0 + np.searchsorted(np.array(fpos['cuts']), x[pos], side='left')
        rec['negative'] = dict(fneg)
        if fpos is not None:
            rec['positive'] = dict(fpos)
        rec['cuts_from_other_source'] = True
    else:
        n_neg = int(neg.sum())
        k = N_NEG if n_neg >= 10 * N_NEG else min(N_NEG, max(1, n_neg // 10)) if n_neg else 0
        if k:   # cuts are quantiles of the loss size, counted from the least negative
            probs = NEG_SHARES if k == N_NEG else np.arange(1, k) / k
            mcuts = common.wquantile(-x[neg], w[neg], probs)
            band[neg] = NEG0 + np.searchsorted(-mcuts[::-1], x[neg], side='left')
            rec['negative'] = {'bands_used': k, 'probs_from_least_negative': probs.tolist(),
                               'cuts': (-mcuts[::-1]).tolist(),
                               'note': 'cuts in dollars, most negative first; band 1 = below cuts[0]'}
        else:
            rec['negative'] = {'bands_used': 0, 'probs': [], 'cuts': []}
        if pos.any():
            cuts = common.wquantile(x[pos], w[pos], POS_SHARES)
            band[pos] = POS0 + np.searchsorted(cuts, x[pos], side='left')
            rec['positive'] = {'probs': POS_SHARES.tolist(), 'cuts': cuts.tolist()}
    bands = []
    for b in range(N_BANDS):
        m = band == b
        if not m.any():
            bands.append({'band': b, 'records': 0, 'weight': 0.0, 'min': None, 'max': None, 'median': None})
        else:
            bands.append({'band': b, 'records': int(m.sum()), 'weight': float(w[m].sum()), 'min': float(x[m].min()),
                          'max': float(x[m].max()), 'median': float(common.wquantile(x[m], w[m], [0.5])[0])})
    rec['bands'] = bands
    return band, rec


def grown_edge(edge, positive, negative):
    """An edge record's cut points, the positive ones times one factor, the negative ones times another."""
    rec = copy.deepcopy({k: edge[k] for k in ['negative', 'positive'] if k in edge})
    for k, f in [('positive', positive), ('negative', negative)]:
        if k in rec and f != 1.0:
            rec[k]['cuts'] = (np.array(rec[k]['cuts']) * f).tolist()
    rec['n_bands'] = N_BANDS
    return rec


# ---- the rows --------------------------------------------------------------------------
PUF_FIELDS = ['RECID', 'S006', 'MARS', 'N24', 'EIC', 'XOCAH', 'XOCAWH', 'XOPAR', 'XOODEP', 'E00100', 'E00200', 'E00300', 'E00400',
              'E00600', 'E00700', 'E00800', 'E00900', 'E01000', 'E01100', 'E01200', 'E01400', 'E01500', 'E01700',
              'E02000', 'E02100', 'E02300', 'E02400', 'E02500', 'E18500', 'E19200', 'P22250', 'P23250'] + \
    ADJ_FIELDS + [f for f, _ in PARTS.values() if f and f not in ('E00900', 'E02100')] + \
    ['E00650', 'E30400', 'E30500', 'FDED', 'P04470', 'E21040', 'E19700', 'E20800'] + [f for f in NEW_ITEMS.values() if f]


def read_puf():
    """The PUF, read once for the table and for ages.py, without its four aggregate records."""
    fields = list(dict.fromkeys(PUF_FIELDS + ages.PUF_FIELDS))
    raw = pd.read_csv(RAW['puf'], usecols=fields)
    return raw[raw.RECID < 999996].reset_index(drop=True)


def puf_rows(puf):
    raw = puf[PUF_FIELDS]
    fields = PUF_FIELDS
    assert not raw.isna().any().any() and not raw.RECID.duplicated().any()
    I = {c: raw[c].round().astype(np.int64).to_numpy() for c in fields if c not in ('RECID', 'S006')}
    for c in I:
        assert np.array_equal(I[c], raw[c].to_numpy()), f'{c}: not whole dollars'
    n_dep = I['XOCAH'] + I['XOCAWH'] + I['XOPAR'] + I['XOODEP']
    assert np.isin(I['MARS'], [1, 2, 3, 4]).all(), 'MARS outside 1-4 (surviving spouses are coded joint on this PUF)'
    assert (I['N24'] >= 0).all() and (I['EIC'] >= 0).all() and (I['EIC'] <= 3).all()
    out = {'source': 'puf', 'year': PUF_YEAR, 'record_id': raw.RECID.astype(float), 'household_id': raw.RECID.astype(float),
           'weight': raw.S006 / 100, 'married': (I['MARS'] == 2).astype(np.int64), 'filing_status': I['MARS'],
           'n_dep_ctc': np.minimum(I['N24'], 3).astype(np.int64), 'n_dep_eitc': np.minimum(I['EIC'], 3).astype(np.int64),
           'dep_filer': (puf['DSI'] == 1).astype(np.int64).to_numpy(),
           'n_dep': np.minimum(n_dep, 3).astype(np.int64), 'filer': 1, 'filer_p': 1.0,
           'wages': I['E00200'], 'txbl_int': I['E00300'], 'dividends': I['E00600']}
    # The PUF's line 13 (E01000) falls below the allowed Schedule D net (P22250 + P23250, a loss held to the limit) on
    # 6.2 million 2015 returns, $52 billion: 3.4 million show the full allowed loss on line 13 while their pieces do
    # not (disclosure protection perturbs each field on its own), the rest small gaps. That shortfall is put on the
    # short-term piece, so line 13 is the allowed net again and the returns keep their loss (DECISIONS_FOR_JOHN.md,
    # 30 October); what is left the other way (line 13 above the allowed net; $0.1 billion) stays in other_agi.
    left = I['E01000'] - common.allowed_capital_gain(I['P22250'] + I['P23250'], I['MARS'])
    kg = {'kg_st': I['P22250'] + np.minimum(left, 0).astype(np.int64), 'kg_lt': I['P23250'], 'kg_dist': I['E01100']}
    out.update(kg, capital_gains=kg['kg_st'] + kg['kg_lt'] + kg['kg_dist'])   # before the loss limit
    # line 13 in AGI: the allowed Schedule D net plus distributions; the rest of E01000 goes to other_agi below
    line13_allowed = common.allowed_capital_gain(kg['kg_st'] + kg['kg_lt'], I['MARS']) + I['E01100']
    assert ((line13_allowed - I['E01100'] == I['E01000']) | (left > 0)).all(), 'line 13 not restored'
    items = {'business_income': I['E00900'] + I['E02000'] + I['E02100'], 'other_gains': I['E01200'],
             'txbl_ira_dist': I['E01400'], 'txbl_pens_dist': I['E01700'],
             'nontxbl_pens_dist': np.maximum(I['E01500'] - I['E01700'], 0),
             'ui': I['E02300'], 'alimony': I['E00800'], 'state_ref': I['E00700'],
             'adjustments': sum(I[c] for c in ADJ_FIELDS),
             'gross_ss': I['E02400'],
             'exempt_int': I['E00400'], 'first_mort_int': I['E19200'], 'salt_prop': I['E18500']}
    parts = {p: I[f] for p, (f, _) in PARTS.items() if f}
    parts['sche_other'] = I['E02000'] - sum(PARTS[p][1] * parts[p] for p in SCHE_PARTS)
    out.update(parts)
    assert np.array_equal(sum(PARTS[p][1] * parts[p] for p in PARTS), items['business_income'])
    items['sch_cf_income'] = sum(PARTS[p][1] * parts[p] for p in CF_PARTS)
    items['sch_e_income'] = sum(PARTS[p][1] * parts[p] for p in E_PARTS)
    items['sche_rent'] = sum(PARTS[p][1] * parts[p] for p in RENT_PARTS)
    items['sche_rest'] = sum(PARTS[p][1] * parts[p] for p in REST_PARTS)
    assert np.array_equal(items['sche_rent'] + items['sche_rest'], items['sch_e_income'])
    assert np.array_equal(items['sch_cf_income'], I['E00900'] + I['E02100'])
    assert np.array_equal(items['sch_e_income'], I['E02000'])
    out.update(items)
    out['k1_owner'] = np.column_stack([parts[p] != 0 for p in K1_PARTS]).any(1).astype(np.int64)
    out['owns_business'] = -1                                     # SCF only
    # stage 3: the dividend parts and the adjustments' parts add up to the totals exactly (whole dollars)
    out['div_pref'], out['div_ord'] = I['E00650'], I['E00600'] - I['E00650']
    assert (out['div_pref'] >= 0).all() and (out['div_ord'] >= 0).all(), 'qualified dividends above ordinary dividends'
    assert np.array_equal(out['div_pref'] + out['div_ord'], out['dividends'])
    out.update({p: I[fld] for p, fld in ADJ_PARTS.items()})
    assert np.array_equal(sum(out[p] for p in ADJ_PARTS), items['adjustments'])
    out.update({v: I[fld] for v, fld in NEW_ITEMS.items() if fld})
    # part_se, production's partnership income subject to self-employment tax: on a return with any partnership
    # income or loss, Schedule SE's earnings of both spouses (E30400 + E30500, after the 92.35% factor) grossed up,
    # less Schedules C and F; zero elsewhere (src/process_puf.R). Not whole dollars (the division by 0.9235).
    k1_part = (I['E25940'] + I['E25980'] - I['E25920'] - I['E25960']) != 0
    out['part_se'] = np.where(k1_part, (I['E30400'] + I['E30500']) / 0.9235 - I['E00900'] - I['E02100'], 0.0)
    out['other_item_exp'] = I['FDED'] * 0.0                       # set below, once the age range is known
    # (3 November) the PUF's own taxable Social Security (E02500) comes off too: it is not a slot, and the file computes
    # it by the worksheet, so other_agi is AGI without taxable benefits less the items (puf_ss_check compares the two)
    out['other_agi'] = I['E00100'] - out['wages'] - out['txbl_int'] - out['dividends'] - line13_allowed - \
        sum(ITEMS[v][0] * items[v] for v in ITEMS) - I['E02500']
    # gross pensions = taxable + the rest, exactly; the rest below zero (taxable above total pensions) is held at zero
    over = I['E01700'] > I['E01500']
    common.say(f'PUF: taxable pensions above total pensions (the rest held at zero) on {int(over.sum()):,} records, '
               f'{(raw.S006.to_numpy() / 100)[over].sum() / 1e6:,.3f} million returns')
    assert np.array_equal(items['txbl_pens_dist'] + items['nontxbl_pens_dist'], np.maximum(I['E01500'], I['E01700']))
    df = pd.DataFrame(out)
    for v in PUF_AMOUNTS:
        df[v] = df[v].astype(float)
    for v in SCF_AMOUNTS:
        df[v] = np.nan
    rng, probs = ages.puf_age_ranges(puf)
    assert np.array_equal(rng.RECID.to_numpy(), raw.RECID.to_numpy())
    df['age_range'] = rng.age_range.to_numpy()
    for k in range(ages.N_RANGES):
        df[f'age_p{k}'] = probs[:, k]
    # other_item_exp, production's other itemized deductions (src/process_puf.R): on itemizers (FDED 1), total
    # itemized deductions before the overall limitation (P04470 + E21040) less deductible medical expenses, the two
    # taxes, interest paid, charity, casualty losses and net limited miscellaneous deductions; zero otherwise. The
    # medical floor is 7.5% of AGI for the 65+ range and 10% otherwise (production's rule); a record with no range
    # counts as 65+ when step 0 gives 65+ more than half (production imputes the range there). The PUF rounds each
    # item on its own, so the residual is negative on many itemizers (mostly by under $500).
    old = (df['age_range'].to_numpy() == 5) | (df['age_p5'].to_numpy() > 0.5)
    agi_ = I['E00100'].astype(float)
    medical = np.maximum(0, I['E17500'] - np.maximum(0, agi_ * np.where(old, 0.075, 0.1)))
    df['other_item_exp'] = np.where(I['FDED'] == 1, I['P04470'] + I['E21040'] - medical - I['E18400'] - I['E18500']
                                    - I['E19200'] - I['E19700'] - I['E20500'] - I['E20800'], 0.0)
    return df


COMPUTED = {'capital_gains': {p: 1 for p in KG_PIECES},
            'dividends': {p: 1 for p in DIV_PARTS}, 'adjustments': {p: 1 for p in ADJ_PARTS},
            'business_income': {p: s for p, (_, s) in PARTS.items()},
            'sch_cf_income': {p: PARTS[p][1] for p in CF_PARTS},
            'sch_e_income': {p: PARTS[p][1] for p in E_PARTS},
            'sche_rent': {p: PARTS[p][1] for p in RENT_PARTS}, 'sche_rest': {p: PARTS[p][1] for p in REST_PARTS}}


_GROWTH = {}


def growth(v, y):
    """(factor for positive values, for negative values) of PUF amount v, PUF year to y, by v's line in the target
    year's growth.yaml (growth.factors). Before 29 October this function held the rule itself: SOI Table 1.4's
    dollars per return with the item, capital losses not grown, and 26 items with no Table 1.4 line not grown at
    all; the Table 1.4 lines are now written in growth.yaml as they were."""
    if (v, y) not in _GROWTH:
        _GROWTH[(v, y)] = GR.factors(v, y)
    return _GROWTH[(v, y)]


def grown(df, y, base=PUF_YEAR, columns=None):
    """The PUF amounts of df (default all; columns: only these), in year-base dollars, grown to year y per return;
    the computed totals whose parts are all grown, from the grown parts. A factor must be positive wherever it is
    used (an SOI net amount can be negative in some year)."""
    df = df.copy()
    if y == base:
        return df
    columns = [v for v in (PUF_AMOUNTS if columns is None else columns) if v not in COMPUTED]
    for v in columns:
        x = df[v].to_numpy(float)
        out = x.copy()
        for k, m in enumerate([x > 0, x < 0]):
            if m.any():
                f = growth(v, y)[k] / growth(v, base)[k]
                assert np.isfinite(f) and f > 0, f'{v}: no growth factor {base} to {y} ({f}; an SOI line not found?)'
                out[m] = x[m] * f
        df[v] = out
    for v, parts in COMPUTED.items():
        if set(parts) <= set(columns):
            df[v] = sum(s * df[p] for p, s in parts.items())
    return df


def nearest_cps_year(y):
    """The CPS year nearest y (the earlier on a tie)."""
    return min(CP['years'], key=lambda c: (abs(c - y), c))


def survey_puf_amounts(d, survey, year):
    """The PUF amounts a survey's rows give on its records d (one survey year, with their raw_<item> columns), in PUF
    terms before function 1: each row's items and definition fixes; a computed total with no row of its own but rows
    for all its parts, from the parts; k1_owner likewise from the partnership and S corporation parts. An item the
    survey does not ask that year (TR.not_asked: the CPS's capital gains before tax year 2018) is zero here: these
    amounts serve the tax data's non-filers and the CPS's taxable Social Security count, which read it as none, as
    before 29 October (the training rows have it missing instead, translate_surveys). A row kept out of training
    (in_training false: the CPS's capital gains since the second retrain of 29 October) still gives its amounts here,
    so the tax data does not move."""
    out = {r['variable']: (np.zeros(len(d)) if TR.not_asked(r, year) else TR.survey_amount(d, r, year))
           for r in TR.rows(survey, amounts_only=True)}
    for v, parts in COMPUTED.items():
        if v not in out and set(parts) <= set(out):
            out[v] = sum(s * out[p] for p, s in parts.items())
    if set(K1_PARTS) <= set(out):
        out['k1_owner'] = np.column_stack([out[p] != 0 for p in K1_PARTS]).any(1).astype(np.int64)
    return pd.DataFrame(out, index=d.index)


TAX_DATA_CATEGORIES = ['filer', 'married', 'n_dep', 'k1_owner'] + list(TAX_CATEGORIES)


def tax_data(t, y):
    """The tax data for year y:
      filers      the PUF filers other than dependent filers, each weight times the year-y over PUF-year returns of
                  SOI Table 1.6 in its filing status and age range (a record with no range: averaged over step 0's
                  probabilities), dollars grown per return;
      non-filers  every CPS unit of year y, or of the nearest CPS year (nearest_cps_year), weighted by its chance of
                  not filing (weight x (1 - filer_p)), filer = 0, its amounts in PUF terms from the translation
                  table's rows before function 1 (survey_puf_amounts), zero for the amounts in settings
                  translation.cps_zero_in_tax_data (interest). From another year, the weights are scaled by
                  SOI's total returns (year y over the CPS year) and the dollars grown per return as the PUF's are
                  (a placeholder until the CPS covers every year).
    t needs the CPS rows' raw_<item> columns and age ranges (the table stage's frame, after primary_age_range).
    Columns: weight, the categories, age_p0-5 (the age range as probabilities), the amounts shared by the surveys of
    year y."""
    puf = t[(t.source == 'puf') & (t.dep_filer == 0)]
    assert (puf.filer == 1).all(), 'the PUF holds filers only'
    P = np.column_stack([(puf.age_range == k).to_numpy(float) + puf[f'age_p{k}'].to_numpy(float)
                         for k in range(common.ADULT_RANGES)])
    assert np.allclose(P.sum(1), 1), 'a PUF filer other than a dependent filer with a dependent-filer age range'
    status = ['single', 'joint', 'separate', 'head_of_household']
    t_y, t_0 = TG.table_1_6(y), TG.table_1_6(PUF_YEAR)
    F = np.array([[t_y[(s, k)] / t_0[(s, k)] for k in range(common.ADULT_RANGES)] for s in status])
    f = (P * F[puf.filing_status.to_numpy().astype(np.int64) - 1]).sum(1)
    # the amounts shared by the surveys of year y (an item no survey of year y shares need not have SOI growth to y:
    # other gains has a net loss in 2012)
    surveys = sorted(set(t.loc[(t.year == y) & (t.source != 'puf'), 'source']))
    assert surveys, f'no survey of year {y}'
    shared = sorted({r['variable'] for src in surveys for r in TR.rows(src, amounts_only=True)})
    # and the totals a row is ranked against (rank_against: the CPS's dividends against ordinary dividends; since 28
    # October also a sum, the CPS's pensions against taxable pensions plus taxable IRA distributions: tax_data's column
    # TR.against_name, the parts joined by '+')
    sums = {TR.against_name(r): TR.against(r) for src in surveys for r in TR.rows(src, amounts_only=True)
            if len(TR.against(r)) > 1}
    shared = sorted(set(shared) | {a for src in surveys for r in TR.rows(src, amounts_only=True) for a in TR.against(r)})
    needed = sorted({p for v in shared for p in COMPUTED.get(v, {v: 1})})
    filers = grown(puf, y, columns=needed).assign(weight=puf.weight.to_numpy() * f)
    for k in range(common.ADULT_RANGES):
        filers[f'age_p{k}'] = P[:, k]
    # the non-filers: the CPS units of the nearest CPS year, weighted by their chance of not filing
    cy = nearest_cps_year(y)
    d = t[(t.source == 'cps') & (t.year == cy)]
    assert len(d) and d['filer_p'].between(0, 1).all()
    am = survey_puf_amounts(d, 'cps', cy)
    # an amount the CPS has no row for is zero on its units here only when settings translation.cps_zero_in_tax_data
    # says so (interest, John 23 October); any other gap stops
    zero = [v for v in common.SETTINGS['translation'].get('cps_zero_in_tax_data', []) if v in shared and v not in am]
    for v in zero:
        am[v] = 0.0
    missing = sorted(set(shared + ['k1_owner']) - set(am))
    assert not missing, f'the CPS rows of the translation table do not give these PUF amounts: {missing}'
    scale = TG.all_returns(y) / TG.all_returns(cy) if cy != y else 1.0
    nf = am.assign(weight=d['weight'].to_numpy(float) * (1 - d['filer_p'].to_numpy(float)) * scale, filer=0,
                   **{c: d[c].to_numpy() for c in ['married', 'n_dep'] + list(TAX_CATEGORIES)})
    nf = grown(nf, y, base=cy, columns=needed)
    rng = d['age_range'].to_numpy()
    assert ((rng >= 0) & (rng < common.ADULT_RANGES)).all()
    for k in range(common.ADULT_RANGES):
        nf[f'age_p{k}'] = (rng == k).astype(float)
    cols = ['weight'] + TAX_DATA_CATEGORIES + [f'age_p{k}' for k in range(common.ADULT_RANGES)] + shared
    out = pd.concat([filers[cols], nf[cols]], ignore_index=True)
    for name, parts in sums.items():
        out[name] = sum(out[p] for p in parts)
    assert not out.isna().any().any(), 'missing values in the tax data'
    common.say(f'tax data {y}: {filers.weight.sum() / 1e6:.2f}M filers (PUF {PUF_YEAR}: {puf.weight.sum() / 1e6:.2f}M), '
               f'{nf.weight.sum() / 1e6:.2f}M non-filers (CPS {cy}, expected non-filers'
               + (f', weights x {scale:.4f}' if cy != y else '') + ')')
    return out


def cex_rows():
    """The CEX tax units (build_cex.R), every year in settings "cex"."""
    rows = []
    for y in CX['years']:
        s = pd.read_csv(f'{common.DATA}/cex_units_{y}.csv.gz')
        assert (s['year'] == y).all() and (s['weight'] > 0).all() and not s['record_id'].duplicated().any()
        common.say(f'CEX {y}: {len(s):,} rows, {s.weight.sum() / 1e6:.1f} million units')
        rows.append(s)
    s = pd.concat(rows, ignore_index=True)
    need = sorted({c for r in TR.rows('cex', amounts_only=True) for c in r['items']}) + CEX_CATS
    n_missing = s[need].isna().sum()
    assert not n_missing.any(), f'missing values in the CEX units:\n{n_missing[n_missing > 0]}'
    assert (s['dep_filer'] == 0).all(), 'build_cex.R folds dependent filers into the unit that claims them'
    # record ids unique across years; the household id is not offset, so a consumer unit interviewed in two
    # survey years falls on one side of the held-back split
    off = (s['year'] - 2000) * CEX_YEAR_ID_OFFSET
    df = pd.DataFrame({'source': 'cex', 'year': s['year'], 'record_id': s['record_id'] + off,
                       'household_id': s['household_id'].astype(float), 'weight': s['weight'].astype(float),
                       'married': s['married'].astype(np.int64), 'n_dep': s['n_dep'].clip(upper=3).astype(np.int64),
                       'filer': -1, 'filer_p': -1.0, 'k1_owner': -1, 'owns_business': -1,
                       **{c: -1 for c in TAX_CATEGORIES},
                       **survey_items(s, 'cex'),
                       'svy_age1': s['age1'], 'svy_age2': s['age2'], 'male1': s['male1'], 'male2': s['male2'],
                       'age_key': s['household_id'].astype(float)})
    for c in CEX_CATS:
        df[c] = s[c].astype(float)
    for v in AMOUNTS:
        if v not in df:
            df[v] = np.nan
    return df


def shares(survey, variable):
    """Whether the translation table has the row."""
    return any(r['variable'] == variable for r in TR.rows(survey))


def cps_rows():
    """The CPS ASEC tax units (build_cps.R), every year in settings "cps": every tax unit, filers and non-filers,
    dependent filers folded into the unit that claims them. filer_p is the unit's calibrated filing chance (build_cps.R's
    p_file), filer the one draw from it build_cps.R makes; mars, n_dep_ctc and n_dep_eitc as build_cps.R writes them. The PUF
    amounts come from the translation table (translate_surveys); k1_owner is set there, from the parts. The person items stay as person 1 and person 2 here
    (p1_..., p2_...); cps_person_slots puts them in primary-filer order once the primary rule is set."""
    rows = []
    for y in CP['years']:
        s = pd.read_csv(f'{common.DATA}/cps_units_{y}.csv.gz')
        assert (s['year'] == y).all() and (s['weight'] > 0).all() and not s['record_id'].duplicated().any()
        common.say(f'CPS {y}: {len(s):,} rows, {s.weight.sum() / 1e6:.1f} million units')
        rows.append(s)
    s = pd.concat(rows, ignore_index=True)
    person = ['wages1', 'wages2', 'se1', 'se2', 'weeks_worked1', 'weeks_worked2', 'usual_hours1', 'usual_hours2']
    need = sorted({c for r in TR.rows('cps', amounts_only=True) for c in r['items']}) + person + \
        ['married', 'n_dep', 'filer', 'p_file', 'filing_status', 'n_dep_ctc', 'n_dep_eitc', 'age1', 'age2', 'sex1', 'sex2', 'dep_age1', 'dep_age2', 'dep_age3', 'care_exp',
         'wages', 'sch_cf_income', 'ssn_status1', 'ssn_status2', 'dep_ssn_status1', 'dep_ssn_status2', 'dep_ssn_status3']
    # an item the CPS does not ask in some years (a row's first_year: capital gains before tax year 2018) is missing
    # exactly in those years (build_cps.R, since 29 October)
    late = {c: r['first_year'] for r in TR.rows('cps', amounts_only=True) if r.get('first_year') is not None
            for c in r['items']}
    for c, fy in late.items():
        early = (s['year'] < fy).to_numpy()
        assert s.loc[early, c].isna().all() and not s.loc[~early, c].isna().any(), \
            f'CPS {c}: missing exactly in the years before {fy} (not asked) expected'
        common.say(f'CPS {c}: not asked before tax year {fy}, missing on {int(early.sum()):,} rows of years '
                   f'{sorted(s.loc[early, "year"].unique().tolist())} (not shown in training)')
    n_missing = s[[c for c in need if c not in late]].isna().sum()
    assert not n_missing.any(), f'missing values in the CPS units:\n{n_missing[n_missing > 0]}'
    # overtime (since 28 October): missing exactly on the units not asked the earnings questions, both adults together
    ot_na = s['ot1'].isna()
    assert (ot_na == s['ot2'].isna()).all(), 'overtime missing on one adult of a unit only'
    for y in CP['years']:
        sel = (s['year'] == y).to_numpy()
        common.say(f'CPS {y}: overtime asked on {int((~ot_na[sel]).sum()):,} of {int(sel.sum()):,} units '
                   f'({100 * s.loc[sel & ~ot_na.to_numpy(), "weight"].sum() / s.loc[sel, "weight"].sum():.1f}% of the weight); '
                   f'missing (absent) on the rest')
    assert (s['dep_filer'] == 0).all(), 'build_cps.R folds dependent filers into the unit that claims them'
    # the split of the couple's earnings adds up exactly; a spouse exactly when married
    assert np.array_equal(s['wages1'] + s['wages2'], s['wages'])
    assert np.array_equal(s['se1'] + s['se2'], s['sch_cf_income'])
    couple = (s['married'] == 1).to_numpy()
    assert ((s['age2'] >= 0).to_numpy() == couple).all() and ((s['sex2'] > 0).to_numpy() == couple).all()
    assert (s.loc[~couple, ['wages2', 'se2', 'weeks_worked2', 'usual_hours2']] == 0).all().all()
    for c in ['age1', 'age2', 'dep_age1', 'dep_age2', 'dep_age3']:
        assert s[c].between(-1, MAX_AGE).all(), c
    # Social Security number status: 1-3 for every person present, 0 where there is none
    assert s['ssn_status1'].isin([1, 2, 3]).all() and s['ssn_status2'].isin([0, 1, 2, 3]).all()
    assert ((s['ssn_status2'] > 0).to_numpy() == couple).all()
    for k in (1, 2, 3):
        assert s[f'dep_ssn_status{k}'].isin([0, 1, 2, 3]).all()
        assert ((s[f'dep_ssn_status{k}'] > 0) == (s[f'dep_age{k}'] >= 0)).all(), f'dep_ssn_status{k}'
    assert s['p_file'].between(0, 1).all() and s['filing_status'].isin([1, 2, 3, 4]).all()
    assert ((s['filing_status'] == 2) == couple).all() and (s['n_dep_eitc'] <= s['n_dep']).all() and (s['n_dep_ctc'] <= s['n_dep']).all()
    assert s['record_id'].max() < 1e9 and s['household_id'].max() < 1e9
    rid = CPS_ID_OFFSET + (s['year'] - 2000) * 1e9 + s['record_id'].astype(float)
    hh = CPS_ID_OFFSET + (s['year'] - 2000) * 1e9 + s['household_id'].astype(float)
    df = pd.DataFrame({'source': 'cps', 'year': s['year'], 'record_id': rid, 'household_id': hh,
                       'weight': s['weight'].astype(float), 'married': s['married'].astype(np.int64),
                       'n_dep': s['n_dep'].clip(upper=3).astype(np.int64),
                       'filer': s['filer'].astype(np.int64) if shares('cps', 'filer') else -1,
                       'filer_p': s['p_file'].astype(float),
                       **{c: s[c].astype(np.int64) if shares('cps', c) else -1 for c in TAX_CATEGORIES},
                       'k1_owner': -1, 'owns_business': -1,
                       **survey_items(s, 'cps'),
                       'svy_age1': s['age1'].astype(float), 'svy_age2': s['age2'].astype(float),
                       'male1': (s['sex1'] == 1).astype(float), 'male2': (s['sex2'] == 1).astype(float),
                       'age_key': hh,
                       **{f'p{k}_{c}': s[f'{c}{k}'].astype(float) for k in (1, 2)
                          for c in ['age', 'sex', 'wages', 'se', 'weeks_worked', 'usual_hours']},
                       **{f'p{k}_ot': s[f'ot{k}'].astype(float) for k in (1, 2)},
                       **{f'p{k}_ssn_status': s[f'ssn_status{k}'].astype(np.int64) for k in (1, 2)},
                       **{f'svy_dep_age{k}': s[f'dep_age{k}'].astype(np.int64) for k in (1, 2, 3)},
                       **{f'dep_ssn_status{k}': s[f'dep_ssn_status{k}'].astype(np.int64) for k in (1, 2, 3)},
                       'cps_childcare': s['care_exp'].astype(float)})
    for v in AMOUNTS:
        if v not in df:
            df[v] = np.nan
    return df


PERSON_COLUMNS = [f'p{k}_{c}' for k in (1, 2) for c in ['age', 'sex', 'wages', 'se', 'weeks_worked', 'usual_hours']] + \
    ['p1_ot', 'p2_ot', 'svy_dep_age1', 'svy_dep_age2', 'svy_dep_age3', 'cps_childcare', 'p1_ssn_status', 'p2_ssn_status',
     'dep_ssn_status1', 'dep_ssn_status2', 'dep_ssn_status3']


def cps_person_slots(t, primary_is_2):
    """The CPS-only slots, primary filer first (primary_is_2: the rows where person 2 is the primary filer, by the
    rule the age range uses). Ages as categories, 0 none, else the age plus 1 (age 0 is a real infant); sex 0 none,
    1 a man, 2 a woman. The spouse's share of the couple's wages and of its self-employment income: only on married
    couples with some of the item (elsewhere absent: the model is never asked it there). Where the two spouses' self-employment incomes have opposite signs the share is cut to 0-1
    (counted below), so the file's two parts still add up to the couple's total. Also writes the target year's CPS
    units, primary filer first, for report_cps.py: data/cps_units_<year>_all.parquet, one for each year of the CPS's
    mix in the target year's file (survey_mix.py)."""
    c = (t.source == 'cps').to_numpy()
    sw = primary_is_2[c]
    d = t[c]
    pick = lambda col, k: np.where(sw, d[f'p{3 - k}_{col}'], d[f'p{k}_{col}'])   # k = 1 primary, 2 spouse
    age = {k: pick('age', k) for k in (1, 2)}
    w = d['weight'].to_numpy(float)
    married_ = (d['married'] == 1).to_numpy()
    out = {'age1': exact_age_value(age[1]), 'age2': exact_age_value(age[2]),
           'sex_primary': pick('sex', 1), 'sex_spouse': pick('sex', 2),
           'weeks_primary': pick('weeks_worked', 1), 'weeks_spouse': pick('weeks_worked', 2),
           'hours_primary': pick('usual_hours', 1), 'hours_spouse': pick('usual_hours', 2),
           'care_exp': d['cps_childcare'].to_numpy(float),
           # each adult's overtime in dollars, missing where the unit was not asked (build_cps.R)
           'ot_primary': pick('ot', 1), 'ot_spouse': pick('ot', 2),
           # each person's Social Security number status (0 none, 1 citizen, 2 noncitizen with a sign, 3 without)
           'ssn_status_primary': pick('ssn_status', 1), 'ssn_status_spouse': pick('ssn_status', 2)}
    for k in (1, 2, 3):
        out[f'dep_age{k}'] = np.where(d[f'svy_dep_age{k}'] >= 0, d[f'svy_dep_age{k}'] + 1, 0)
        out[f'ssn_status_dep{k}'] = d[f'dep_ssn_status{k}'].to_numpy()
    for share, item in [('spouse_wage_share', 'wages'), ('spouse_se_share', 'se')]:
        total = pick(item, 1) + pick(item, 2)
        x = np.where(total != 0, pick(item, 2) / np.where(total != 0, total, 1.0), 0.0)
        cut = (x < 0) | (x > 1)
        common.say(f'CPS {share}: the spouses\' amounts have opposite signs in {100 * w[cut].sum() / w[total != 0].sum():.2f}% '
                   f'of the units with any (weighted), {int(cut.sum())} rows; cut to 0-1')
        # recorded only for married couples with some of the item; elsewhere absent (not in the universe of the
        # question), and generation sets it to zero by rule (generate.structural)
        out[share] = np.where(married_ & (total != 0), np.clip(x, 0.0, 1.0), np.nan)
        out[f'{item}_primary_reported'], out[f'{item}_spouse_reported'] = pick(item, 1), pick(item, 2)
    t = pd.concat([t, pd.DataFrame({v: np.nan for v in out if v not in t}, index=t.index)], axis=1)
    for v, x in out.items():
        t.loc[c, v] = x
    # what generation enforces (generate.structural) holds in the data
    d = t[c]
    married = (d.married == 1).to_numpy()
    assert ((d.age2 > 0).to_numpy() == married).all() and ((d.sex_spouse > 0).to_numpy() == married).all()
    assert (d.sex_primary > 0).all() and (d.age1 > 0).all()
    assert (ages.adult_range(d.age1 - 1) == d.age_range).all(), 'the primary filer\'s age is not in its range'
    assert d.age1.between(1, EXACT_AGE_TOP + 1).all() and d.age2.between(0, EXACT_AGE_TOP + 1).all()
    for k in (1, 2, 3):
        assert ((d[f'dep_age{k}'] > 0) == (d.n_dep >= k)).all(), f'dep_age{k} and n_dep disagree'
        assert ((d[f'ssn_status_dep{k}'] > 0) == (d.n_dep >= k)).all(), f'ssn_status_dep{k} and n_dep disagree'
    assert d.ssn_status_primary.isin([1, 2, 3]).all() and ((d.ssn_status_spouse > 0).to_numpy() == married).all()
    for y in sorted(d.year.unique()):
        dy = d[d.year == y]
        common.say(f'CPS {y} Social Security number status (millions, weighted; citizen / noncitizen with a sign / '
                   'without): ' + '; '.join(
                       f'{lab} ' + ' / '.join(f'{dy.weight[dy[v] == k].sum() / 1e6:.2f}' for k in (1, 2, 3))
                       for lab, v in [('primary filers', 'ssn_status_primary'), ('spouses', 'ssn_status_spouse')]
                       + [(f'dependent {k}', f'ssn_status_dep{k}') for k in (1, 2, 3)]))
    for v in SPOUSE_SLOTS:
        if v in CPS_OVERTIME:   # zero unless married where asked; missing where not asked
            assert ((d.loc[~married, v] == 0) | d.loc[~married, v].isna()).all(), v
            continue
        assert (d.loc[~married, v] == 0).all() or (v in SHARES and d.loc[~married, v].isna().all()), v
    # overtime: at most the person's own wages as the CPS reports them (build_cps.R holds it there), so zero without
    # wages; missing on both adults together
    for v, item in [('ot_primary', 'wages_primary_reported'), ('ot_spouse', 'wages_spouse_reported')]:
        x, cap = d[v].to_numpy(float), d[item].to_numpy(float)
        ok = ~np.isnan(x)
        assert (x[ok] >= 0).all() and (x[ok] <= np.maximum(cap[ok], 0) + 1e-6).all(), f'{v} above the person\'s wages'
    assert (d.ot_primary.isna() == d.ot_spouse.isna()).all()
    a = d.ot_primary.notna().to_numpy()
    common.say(f'CPS overtime: asked on {int(a.sum()):,} rows; among them, primary filers with overtime '
               f'{100 * w[a & (d.ot_primary > 0).to_numpy()].sum() / w[a].sum():.1f}% (weighted), spouses '
               f'{100 * w[a & (d.ot_spouse > 0).to_numpy()].sum() / w[a].sum():.1f}%')
    for v in SHARES:
        common.say(f'CPS {v}: recorded on {int(d[v].notna().sum()):,} rows (married couples with some of the item)')
    keep = ['year', 'record_id', 'household_id', 'weight', 'married', 'n_dep', 'filer', 'filer_p', 'age_range'] + \
        list(TAX_CATEGORIES) + \
        list(PERSON_CATEGORIES) + CPS_AMOUNTS + ['wages_primary_reported', 'wages_spouse_reported', 'se_primary_reported',
                                              'se_spouse_reported'] + [c for c in d if c.startswith('raw_')]
    for y in mix_years('cps'):
        d[d.year == y][keep].to_parquet(f'{common.DATA}/cps_units_{y}_all.parquet', index=False)
    return t.drop(columns=[c for c in t if c.endswith(('_primary_reported', '_spouse_reported'))])


SIPP_PERSON = ['wages', 'tips', 'tips_lh']
SIPP_PERSON_COLUMNS = [f'q{k}_{c}' for k in (1, 2) for c in SIPP_PERSON]


def sipp_rows():
    """The SIPP's tax units (roster_sipp.py), every year in settings "sipp": every tax unit, dependent filers folded
    into the unit that claims them. Wages (the translation table's raw_wages) are the primary filer's plus the
    spouse's. The person items stay as person 1 and person 2 here (q1_..., q2_...); sipp_person_slots puts them in
    primary-filer order once the primary rule is set."""
    rows = []
    for y in SP['years']:
        s = pd.read_csv(f'{common.DATA}/sipp_units_{y}.csv.gz')
        assert (s['year'] == y).all() and not s['record_id'].duplicated().any()
        zero = int((s['weight'] <= 0).sum())
        common.say(f'SIPP {y}: {len(s):,} rows, {s.weight.sum() / 1e6:.1f} million units; rows with weight 0 left out '
                   f'(the SIPP gives them no weight in the reference year\'s last month): {zero}')
        rows.append(s[s['weight'] > 0])
    s = pd.concat(rows, ignore_index=True)
    need = sorted({c for r in TR.rows('sipp', amounts_only=True) for c in r['items']}) + \
        ['married', 'n_dep', 'age1', 'age2', 'sex1', 'sex2', 'wages1', 'wages2', 'tips1', 'tips2', 'tips_lh1', 'tips_lh2']
    n_missing = s[need].isna().sum()
    assert not n_missing.any(), f'missing values in the SIPP units:\n{n_missing[n_missing > 0]}'
    # elective deferrals (since 28 October): asked in some years only, so missing in exactly the years not asked
    for y in SP['years']:
        e = s.loc[s.year == y, 'elective_deferrals'].to_numpy(float)
        n_na = int(np.isnan(e).sum())
        assert n_na in (0, len(e)), f'SIPP {y}: elective deferrals missing on some units only'
        common.say(f'SIPP {y}: elective deferrals ' + ('not asked (missing on every unit)' if n_na else 'asked'))
    e, wg = s['elective_deferrals'].to_numpy(float), s['wages'].to_numpy(float)
    asked = ~np.isnan(e)
    assert (e[asked] >= 0).all() and (e[asked] <= wg[asked] + 1e-6).all(), 'SIPP deferrals below zero or above wages'
    assert (s['dep_filer'] == 0).all(), 'roster_sipp.py folds dependent filers into the unit that claims them'
    assert np.allclose(s['wages1'] + s['wages2'], s['wages'], rtol=0, atol=1e-6)
    couple = (s['married'] == 1).to_numpy()
    assert ((s['age2'] >= 0).to_numpy() == couple).all() and ((s['sex2'] > 0).to_numpy() == couple).all()
    assert (s.loc[~couple, ['wages2', 'tips2', 'tips_lh2']] == 0).all().all()
    assert ((s['tips1'] <= s['wages1'] + 1e-6) & (s['tips2'] <= s['wages2'] + 1e-6)).all(), 'tips above the person\'s wages'
    assert s['record_id'].max() < 1e10 and s['household_id'].max() < 1e10
    rid = SIPP_ID_OFFSET + (s['year'] - 2000) * 1e10 + s['record_id'].astype(float)
    hh = SIPP_ID_OFFSET + (s['year'] - 2000) * 1e10 + s['household_id'].astype(float)
    df = pd.DataFrame({'source': 'sipp', 'year': s['year'], 'record_id': rid, 'household_id': hh,
                       'weight': s['weight'].astype(float), 'married': s['married'].astype(np.int64),
                       'n_dep': s['n_dep'].clip(upper=3).astype(np.int64), 'filer': -1, 'filer_p': -1.0,
                       **{c: -1 for c in TAX_CATEGORIES}, 'k1_owner': -1, 'owns_business': -1,
                       **survey_items(s, 'sipp'),
                       'svy_age1': s['age1'].astype(float), 'svy_age2': s['age2'].astype(float),
                       'male1': (s['sex1'] == 1).astype(float), 'male2': (s['sex2'] == 1).astype(float),
                       'age_key': hh,
                       **{f'q{k}_{c}': s[f'{c}{k}'].astype(float) for k in (1, 2) for c in SIPP_PERSON}})
    for v in AMOUNTS:
        if v not in df:
            df[v] = np.nan
    return df


def sipp_person_slots(t, primary_is_2):
    """The SIPP's own slots, primary filer first (primary_is_2 as for the CPS): each adult's tips in dollars (zero
    without wages: the SIPP has no tips above a person's wages, checked in sipp_rows), whether the adult's tips are mostly from leisure and hospitality (0 without
    tips), and the spouse's share of the couple's wages (the CPS's slot, which the SIPP shares: recorded only on
    married couples with some wages, else absent, as on the CPS). Also writes the target year's SIPP units, primary
    filer first, for report_sipp.py: data/sipp_units_<year>_all.parquet, one for each year of the SIPP's mix in the
    target year's file (survey_mix.py)."""
    c = (t.source == 'sipp').to_numpy()
    sw = primary_is_2[c]
    d = t[c]
    pick = lambda col, k: np.where(sw, d[f'q{3 - k}_{col}'], d[f'q{k}_{col}'])   # k = 1 primary, 2 spouse
    married_ = (d['married'] == 1).to_numpy()
    out = {}
    for k, who in [(1, 'primary'), (2, 'spouse')]:
        wg, tp, lh = pick('wages', k), pick('tips', k), pick('tips_lh', k)
        out[f'tips_{who}'] = np.where(wg > 0, tp, 0.0)
        out[f'tips_lh_{who}'] = np.where(tp > 0, lh, 0).astype(np.int64)
        out[f'wages_{who}_reported'], out[f'tips_{who}_reported'] = wg, tp
    total = pick('wages', 1) + pick('wages', 2)
    out['spouse_wage_share'] = np.where(married_ & (total > 0), pick('wages', 2) / np.where(total > 0, total, 1.0), np.nan)
    for who in ['primary', 'spouse']:
        assert (out[f'tips_{who}'] == out[f'tips_{who}_reported']).all(), f'{who}: SIPP tips without wages'
    t = pd.concat([t, pd.DataFrame({v: np.nan for v in out if v not in t}, index=t.index)], axis=1)
    for v, x in out.items():
        t.loc[c, v] = x
    d = t[c]
    married = (d.married == 1).to_numpy()
    assert (d.loc[~married, SIPP_SPOUSE_SLOTS] == 0).all().all() and d.loc[~married, 'spouse_wage_share'].isna().all()
    common.say(f'SIPP spouse_wage_share: recorded on {int(d.spouse_wage_share.notna().sum()):,} rows (married couples '
               f'with some wages); tips above wages: none (checked)')
    keep = ['year', 'record_id', 'household_id', 'weight', 'married', 'n_dep', 'age_range', 'spouse_wage_share'] + \
        SIPP_AMOUNTS + list(SIPP_CATEGORIES) + [c for c in out if c.endswith('_reported')] + \
        [c for c in d if c.startswith('raw_')]
    for y in mix_years('sipp'):
        d[d.year == y][keep].to_parquet(f'{common.DATA}/sipp_units_{y}_all.parquet', index=False)
    return t.drop(columns=[c for c in out if c.endswith('_reported')])


def scf_person_slots(t, primary_is_2):
    """The SCF's spouse_deferral_share (decided 27 October), primary filer first (primary_is_2 as for the CPS): the
    spouse's share of the unit's elective deferrals (build_scf.R's elective1, the head's, and elective2, the
    spouse's), as the CPS's spouse_wage_share is of wages. 0 on a unit that is not married or has no deferrals (a rule
    generation applies, generate.deferral_structure); missing where the SCF does not ask (the deferrals are)."""
    c = (t.source == 'scf').to_numpy()
    d = t[c]
    e, e1, e2 = (d[k].to_numpy(float) for k in ('elective_deferrals', 'elective1', 'elective2'))
    spouse = np.where(primary_is_2[c], e1, e2)
    married = (d['married'] == 1).to_numpy()
    asked = ~np.isnan(e)
    share = np.where(married & (e > 0), spouse / np.where(e > 0, e, 1.0), 0.0)
    t.loc[c, 'spouse_deferral_share'] = np.where(asked, share, np.nan)
    s = t.loc[c, 'spouse_deferral_share'].to_numpy(float)
    assert (np.isnan(s) == ~asked).all() and ((s[asked] >= 0) & (s[asked] <= 1 + 1e-9)).all()
    w = d['weight'].to_numpy(float)
    on = asked & married & (e > 0)
    common.say(f'SCF spouse_deferral_share: married units with deferrals {w[on].sum() / 1e6:.2f} m over the waves '
               f'together; of them, all to the primary filer {100 * w[on & (s == 0)].sum() / w[on].sum():.1f}%, all to the '
               f'spouse {100 * w[on & (s >= 1 - 1e-9)].sum() / w[on].sum():.1f}% (weighted, every wave)')
    return t.drop(columns=['elective1', 'elective2'])


def exact_age_value(age):
    """An exact-age slot's value from ages in years (negative or missing: none): 0 none, else the age plus 1, with
    ages 80 and over as 80 (EXACT_AGE_TOP), the one coding the SCF, the CEX and the CPS all allow."""
    a = np.asarray(age, float)
    return np.where(np.isnan(a) | (a < 0), 0, np.minimum(np.nan_to_num(a, nan=-1), EXACT_AGE_TOP) + 1).astype(np.int64)


def survey_items(s, survey):
    """The survey's columns that the translation table's amount rows read, as raw_<column> (translate.py turns them
    into the shared slots), and the deferral columns of their definition fixes (less_traditional_deferrals)."""
    return {f'raw_{c}': s[c].astype(float) for c in sorted({c for r in TR.rows(survey, amounts_only=True)
                                                            for c in TR.columns(r)})}


def loan_to_value(s):
    """The home mortgage as a share of the home's value; zero with no home. A mortgage on a unit with
    no home value (none expected) is moved to other debt, so net worth is unchanged; the count is printed."""
    home, mort = s['value.primary_home'].to_numpy(float), s['value.primary_mortgage'].to_numpy(float)
    stray = (home <= 0) & (mort != 0)
    common.say(f'  home mortgage with no home value: {int(stray.sum())} rows, moved to other debt')
    s.loc[stray, 'value.other_debt'] = s.loc[stray, 'value.other_debt'] + s.loc[stray, 'value.primary_mortgage']
    s.loc[stray, 'value.primary_mortgage'] = 0.0
    s['w_primary_ltv'] = np.where(home > 0, s['value.primary_mortgage'] / np.where(home > 0, home, 1.0), 0.0)
    return s


def scf_rows():
    waves = []
    for wave in [2016, 2019, 2022, 2013]:   # this order fixes the rows' order, and so the training batches
        s = pd.read_csv(f'{common.DATA}/scf_units_{wave}.csv.gz')
        y = wave - 1
        off = WAVE_2013_ID_OFFSET if wave == 2013 else 1e8 * (y - PUF_YEAR)
        s['record_id'] += off
        s['household_id'] += off
        s['age_key'] = s['household_id']
        common.say(f'wave {wave}:')
        # elective deferrals (since 28 October): asked of the PEU's respondent and spouse or partner only, so missing
        # exactly on the units outside the PEU and the other units (taxunit_code 8, 9)
        asked = (s['taxunit_code'] <= 2).to_numpy()
        e = s['elective_deferrals'].to_numpy(float)
        assert not np.isnan(e[asked]).any() and np.isnan(e[~asked]).all(), 'elective deferrals missing on a PEU unit'
        assert (e[asked] >= 0).all() and (e[asked] <= s['wages'].to_numpy(float)[asked] + 1e-6).all()
        e1, e2 = s['elective1'].to_numpy(float), s['elective2'].to_numpy(float)     # the head's and the spouse's
        assert np.allclose(e1[asked] + e2[asked], e[asked]) and np.isnan(e2[~asked]).all()
        assert (e2[asked & (s['married'] == 0).to_numpy()] == 0).all(), 'a spouse\'s deferrals off a couple'
        common.say(f'  elective deferrals not asked (units outside the PEU, other units; missing, not zero): '
                   f'{int((~asked).sum())} rows, {s.weight[~asked].sum() / 1e6:.2f} m units with '
                   f'${(s.weight * s.wages)[~asked].sum() / 1e9:.1f} billion of wages')
        s = loan_to_value(s.assign(source='scf', filer=-1, filer_p=-1.0, **{c: -1 for c in TAX_CATEGORIES}))
        if wave == TARGET_YEAR:     # the tilt's wealth targets and wealth by wage group, in this wave's dollars
            s.to_parquet(f'{common.DATA}/scf_units_{TARGET_YEAR}.parquet', index=False)
        s = s.rename(columns={'age1': 'svy_age1', 'age2': 'svy_age2'})   # the SCF's person 1 and 2, before the primary rule
        raw = survey_items(s, 'scf')
        s = s.drop(columns=[c for c in s if c in PUF_AMOUNTS]).assign(**raw)
        waves.append(s)
    s = pd.concat(waves, ignore_index=True)
    for v in AMOUNTS:
        if v not in s:
            s[v] = np.nan
    return s


# ---- the AGI group label (qgroups.py) --------------------------------------------------------
def agi_group_label(t, edges):
    """The AGI group label on t (column agi_group): on PUF rows the record's band-AGI group (qgroups.puf_groups, which
    reads the PUF year's dollar map, so this runs after build_maps), -1 on every other source; and its meta in
    edges['meta']['labels'] (the groups' names, lines, records and returns) and present_on. Then a check: band-AGI
    recomputed from the rows' bands gives every PUF row's label back."""
    common._EDGES = None              # qgroups and dollars read the band edges just written
    import importlib
    import sys
    import qgroups as Q
    if 'dollars' in sys.modules:      # imported earlier in this run (survey_mix, through target_list), with the
        importlib.reload(sys.modules['dollars'])   # meta of the edges on disk before this run: read them again
    ref = Q.puf_groups(t)
    label = pd.Series(np.int64(-1), index=t.index)
    label[ref['group'].index] = ref['group'].to_numpy().astype(np.int64)
    t = t.assign(agi_group=label.astype(np.int64))
    edges['meta']['labels'] = {'agi_group': {
        'values': len(Q.PUF_LABELS), 'source': 'puf', 'labels': Q.PUF_LABELS, 'lines': Q.PUF_LINES, 'cuts': ref['cuts'],
        'records': ref['records'], 'returns': ref['returns'],
        'what': 'band-AGI group (qgroups.puf_groups): AGI from the representative amounts of the bands'}}
    edges['meta']['present_on']['agi_group'] = ['puf']
    puf = t[t.source == 'puf']
    again = Q.puf_group(Q.band_agi(puf), ref['cuts'])
    assert (again == puf['agi_group'].to_numpy()).all(), 'a PUF row whose label differs from its band-AGI group'
    tab = pd.DataFrame({'group': Q.PUF_LABELS, 'records': np.asarray(ref['records'], int),
                        'returns (m)': np.asarray(ref['returns']) / 1e6, 'q': ref['q']})
    tab['returns per record'] = 1 / tab['q']
    common.say('the AGI group label (band-AGI lines, ' + ', '.join(f'p{100 * p:g} ${c:,.0f}' for p, c in
                                                                     zip(Q.PUF_LINES, ref['cuts'])) + '):\n'
               + tab.to_string(index=False, float_format=lambda x: f'{x:,.5f}'))
    return t, edges


# ---- bands --------------------------------------------------------------------------------
def translate_surveys(t, edges):
    """The shared amounts of every survey year from the translation table (translate.py): the definition fixes,
    function 1 against the tax data for the year, the PUF's cut points grown to the year; then the computed totals
    (and k1_owner) a survey has rows for all the parts of, from the translated parts; then the categories. The
    report: the survey as reported, the link (after the definition fixes), after function 1 (with its move), the tax
    data. Each year's tax data is written to data/tax_data_<year>.parquet."""
    w = t['weight'].to_numpy(float)
    report, notes, tax, shortfall = [], {}, {}, []

    def put(sel, src, y, v, new, fp, fn, rule):
        """The survey year's amount new on its rows sel, banded on the PUF's cut points grown to the year."""
        b, rec = band_variable(new, w[sel], grown_edge(edges[f'puf/{PUF_YEAR}/{v}'], fp, fn))
        rec['cut_rule'] = rule
        t.loc[sel, v], t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = new, b, rec

    for src in ('scf', 'cex', 'cps', 'sipp'):
        for y in sorted(t.loc[t.source == src, 'year'].unique()):
            if y not in tax:
                tax[y] = tax_data(t, y)
                tax[y].to_parquet(f'{common.DATA}/tax_data_{y}.parquet', index=False)
            T = tax[y]
            tw = T['weight'].to_numpy(float)
            sel = ((t.source == src) & (t.year == y)).to_numpy()
            d = t[sel]
            done = {}
            for row in TR.rows(src, amounts_only=True):
                v = row['variable']
                if TR.not_shown(row, int(y)):
                    # (29 October) a year the survey does not ask the item (the CPS's capital gains before tax year
                    # 2018), or (second retrain of 29 October) a row kept out of training (in_training false: the
                    # CPS's capital gains in every year): missing, so not shown in training, as any item a survey does
                    # not ask; no function 1, no report row, no fill-in (TR.fills_in)
                    new = np.full(int(sel.sum()), np.nan)
                    why = f'not asked in {y}' if TR.not_asked(row, int(y)) else 'kept out of training'
                    put(sel, src, y, v, new, *growth(v, y), f'{why}: missing (not shown in training)')
                    done[v] = new
                    notes[f'{src} {y} {v}'] = {why: 'missing, not shown in training'}
                    continue
                reported = TR.reported_amount(d, row, y)       # the items summed, as the survey reports them
                x = TR.survey_amount(d, row, y)                # the link: after the definition fixes
                fp, fn = growth(v, y)
                line = None if row['dollar_line'] is None else row['dollar_line'] * fp
                match = row.get('match', 'count')
                # rank_against (stage 3): a survey total put in one of its parts is matched to the tax data's total
                against = TR.against_name(row)
                new, beyond = TR.amounts(x, w[sel], T[against].to_numpy(float), tw, line, match, row.get('loss_line'))
                done[v] = new
                put(sel, src, y, v, new, fp, fn, f'PUF {PUF_YEAR} cut points x per-return growth to {y} ({fp:.4f} '
                                                 f'positive, {fn:.4f} losses); dollar line {line}, match {match}')
                notes[f'{src} {y} {v}'] = {'dollar line (survey-year dollars)': line, 'match': match,
                                           'units past the tax data\'s last holder (millions)': beyond / 1e6}
                link = TR.describe(x, w[sel])
                ded, not_asked = TR.deferrals_subtracted(d, row, y)
                if ded is not None:            # the link wage: what the deferral fix took off, and from whom
                    link['traditional deferrals subtracted ($ billion)'] = (w[sel] * ded).sum() / 1e9
                    link['units with deferrals subtracted (millions)'] = w[sel][ded > 0].sum() / 1e6
                    link['units not asked about deferrals (millions)'] = w[sel][not_asked].sum() / 1e6
                    link['wages of units not asked ($ billion)'] = (w[sel] * reported)[not_asked].sum() / 1e9
                    notes[f'{src} {y} {v}']['units not asked about deferrals (millions)'] = w[sel][not_asked].sum() / 1e6
                tx = T[against].to_numpy(float)
                stages = {'as reported': TR.describe(reported, w[sel]), 'link': link,
                          'after function 1': {**TR.describe(new, w[sel]), **TR.move(x, new, w[sel])},
                          'tax data': TR.describe(tx, tw)}
                if line:   # the units beyond the row's dollar line (since 28 October)
                    for st, (xx, ww) in {'as reported': (reported, w[sel]), 'link': (x, w[sel]),
                                         'after function 1': (new, w[sel]), 'tax data': (tx, tw)}.items():
                        stages[st][f'above the dollar line, ${line:,.0f} (millions)'] = ww[xx > line].sum() / 1e6
                        stages[st][f'above the dollar line, ${line:,.0f} (%)'] = 100 * ww[xx > line].sum() / ww.sum()
                report += TR.report_rows(row, y, stages)
                if TR.fills_in(row, int(y)):   # function 2's task: the tax data's share less the survey's, by range
                    shortfall += TR.shortfall(row, int(y), new, w[sel], tx, tw, fp, fn)
            # computed totals the survey has no row for but has every part of: from the translated parts
            for v, parts in COMPUTED.items():
                if v in done or not set(parts) <= set(done):
                    continue
                new = sum(sg * done[p] for p, sg in parts.items())
                fp, fn = growth(v, y)
                put(sel, src, y, v, new, fp, fn, f'computed from its translated parts; PUF {PUF_YEAR} cut points x '
                                                 f'per-return growth to {y} ({fp:.4f} positive, {fn:.4f} losses)')
                done[v] = new
            if set(K1_PARTS) <= set(done):    # the partnership flag from its parts, as on the PUF
                t.loc[sel, 'k1_owner'] = np.column_stack([done[p] != 0 for p in K1_PARTS]).any(1).astype(np.int64)
            d = t[sel]
            for row in TR.rows(src):
                v = row['variable']
                if v in TR.EXACT_AGES:      # no exact age in the tax data: against the CPS of the nearest year
                    stages = {'survey': TR.describe_age(d[v].to_numpy(), w[sel])}
                    if src != 'cps':
                        cy = nearest_cps_year(y)
                        c = ((t.source == 'cps') & (t.year == cy)).to_numpy()
                        stages['CPS (nearest year)'] = TR.describe_age(t.loc[c, v].to_numpy(), w[c])
                        notes[f'{src} {y} {v}'] = {'CPS year compared': cy}
                    assert (d[v] >= 0).all(), f'{src} {y} {v}: missing'
                    report += TR.report_rows(row, y, stages)
                elif v in TR.CATEGORY_VARIABLES:
                    tax_side = TR.describe_category(None, tw, T[[f'age_p{k}' for k in range(common.ADULT_RANGES)]].to_numpy()) \
                        if v == 'age_range' else TR.describe_category(T[v].to_numpy(), tw)
                    assert (d[v] >= 0).all(), f'{src} {y} {v}: missing'
                    report += TR.report_rows(row, y, {'survey': TR.describe_category(d[v].to_numpy(), w[sel]),
                                                      'tax data': tax_side})
    rep = pd.DataFrame(report)
    rep.to_csv(f'{common.DATA}/translation_report.csv', index=False)
    TR.say_report(rep)
    sf = pd.DataFrame(shortfall)
    sf.to_csv(f'{common.DATA}/fillin_shortfall.csv', index=False)
    common.say('function 2, the shortfall it is asked to fill (tax data less the survey after function 1, % of units, '
               'by size range in survey-year dollars):\n' + sf.round(2).to_string(index=False))
    common.say('function 1: ' + json.dumps(notes, indent=1, default=float))
    factors = {}
    for y, T in tax.items():   # the amounts of the surveys of year y, and the totals computed from them
        have = {v for v in T.columns if v in PUF_AMOUNTS}
        have |= {v for v, parts in COMPUTED.items() if set(parts) <= have}
        factors[str(y)] = {v: list(growth(v, y)) for v in sorted(have)}
    return t, factors


# Taxable Social Security: the section 86 worksheet is common.taxable_social_security (the calculator's formula; since
# 3 November the file computes it on every record, dollars.retirement_items). Until then this module had its own copy.
taxable_social_security = common.taxable_social_security


def puf_ss_check(t, puf):
    """(3 November) The worksheet on the PUF's own records against the PUF's taxable Social Security (E02500): returns
    with benefits, and how many the worksheet puts within $1, $100 and $1,000 of the reported amount; the totals.
    Modified AGI is E00100 - E02500 + tax-exempt interest (E00400) + the student loan interest deduction (E03210).
    Written to data/puf_ss_check.csv; the training table carries no taxable Social Security."""
    p = t[t.source == 'puf']
    raw = puf.set_index('RECID').loc[p['record_id'].astype(np.int64).to_numpy()]
    rep = raw['E02500'].to_numpy(float)
    magi = (raw['E00100'] - raw['E02500'] + raw['E00400'] + raw['E03210']).to_numpy(float)
    calc = taxable_social_security(p['gross_ss'].to_numpy(float), magi, p['filing_status'].to_numpy())
    w = p['weight'].to_numpy(float)
    has = p['gross_ss'].to_numpy(float) > 0
    gap = np.abs(calc - rep)
    out = pd.DataFrame([{
        'returns with gross benefits (millions)': w[has].sum() / 1e6,
        'with taxable benefits, reported (millions)': w[rep > 0].sum() / 1e6,
        'with taxable benefits, worksheet (millions)': w[calc > 0].sum() / 1e6,
        'worksheet within $1 of reported (millions, returns with benefits)': w[has & (gap <= 1)].sum() / 1e6,
        'within $100 (millions)': w[has & (gap <= 100)].sum() / 1e6,
        'within $1,000 (millions)': w[has & (gap <= 1000)].sum() / 1e6,
        'taxable benefits, reported ($ billion)': (w * rep).sum() / 1e9,
        'taxable benefits, worksheet ($ billion)': (w * calc).sum() / 1e9,
        'reported above 85% of gross (records)': int((rep > 0.85 * p['gross_ss'].to_numpy(float) + 1).sum()),
        'records with taxable benefits and no gross benefits': int(((rep > 0) & ~has).sum())}])
    out.to_csv(f'{common.DATA}/puf_ss_check.csv', index=False)
    common.say('PUF, taxable Social Security by the worksheet against E02500 (weighted, 2015):\n'
               + out.round(3).T.to_string())


def cps_ss_taxable_check(t):
    """For each CPS year: the units, and the expected non-filers (weight x (1 - filer_p)), the taxable-benefits
    formula would give a positive taxable Social Security, from the CPS's amounts in PUF terms (before function 1).
    Since 3 November taxable Social Security is no slot on any source (the file computes it on every record by the
    same worksheet, dollars.retirement_items); this only counts. Written to data/cps_ss_taxable_check.csv."""
    rows = []
    for y in CP['years']:
        d = t[(t.source == 'cps') & (t.year == y)]
        a = survey_puf_amounts(d, 'cps', y)
        # modified AGI: the AGI items without taxable benefits plus tax-exempt interest (the CPS has no student loan
        # interest deduction)
        other = a['wages'] + a.get('txbl_int', 0.0) + a['dividends'] + a['capital_gains'] + \
            sum(ITEMS[v][0] * a.get(v, 0.0) for v in ITEMS if ITEMS[v][0] != 0) + a['other_agi'] + \
            a['exempt_int']
        tx = taxable_social_security(a['gross_ss'].to_numpy(float), other.to_numpy(float), d['filing_status'].to_numpy())
        w, p = d['weight'].to_numpy(float), d['filer_p'].to_numpy(float)
        pos, ss = tx > 0, a['gross_ss'].to_numpy(float) > 0
        nfw = w * (1 - p)
        rows.append({'year': y, 'units with Social Security (millions)': w[ss].sum() / 1e6,
                     'expected non-filers with Social Security (millions)': nfw[ss].sum() / 1e6,
                     'expected non-filers with taxable benefits by the formula (millions)': nfw[pos].sum() / 1e6,
                     'their taxable benefits by the formula ($bn)': (nfw * tx).sum() / 1e9,
                     'expected filers with taxable benefits by the formula (millions)': (w * p)[pos].sum() / 1e6,
                     'all units with taxable benefits by the formula (millions)': w[pos].sum() / 1e6})
    out = pd.DataFrame(rows)
    out.to_csv(f'{common.DATA}/cps_ss_taxable_check.csv', index=False)
    common.say('CPS, taxable Social Security by the formula (no slot on any source since 3 November):\n'
               + out.round(3).T.to_string())


def band_table(t):
    filers = ((t.source == 'puf') & (t.filer == 1)).to_numpy()
    pside = (t.source == 'puf').to_numpy()
    w = t['weight'].to_numpy(float)
    edges = {}
    t = pd.concat([t, pd.DataFrame({f'{v}_band': np.int16(-1) for v in AMOUNTS}, index=t.index)], axis=1)
    for v in PUF_AMOUNTS:                        # cut on the PUF filers, applied to the whole PUF side
        b, rec = band_variable(t.loc[filers, v].to_numpy(float), w[filers])
        edges[f'puf/{PUF_YEAR}/{v}'] = rec
        t.loc[pside, f'{v}_band'] = band_variable(t.loc[pside, v].to_numpy(float), w[pside], grown_edge(rec, 1.0, 1.0))[0]
    t, factors = translate_surveys(t, edges)
    for src, own, rule in [('scf', SCF_AMOUNTS, 'within the wave'), ('cex', CEX_AMOUNTS, 'within the year'),
                           ('cps', CPS_AMOUNTS, 'within the year'), ('sipp', SIPP_AMOUNTS, 'within the year')]:
        for y in sorted(t.loc[t.source == src, 'year'].unique()):
            sel = ((t.source == src) & (t.year == y)).to_numpy()
            for v in own:
                b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel])
                rec['cut_rule'] = rule
                t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = b, rec
    t = shared_between_surveys(t, edges)
    for v in AMOUNTS:
        b, x = t[f'{v}_band'], t[v]
        assert ((b == -1) == x.isna()).all() and ((b == 0) == (x == 0)).all(), v
        t[f'{v}_band'] = b.astype('int16')
    edges['meta'] = table_meta(t, factors)
    return t, edges


def shared_between_surveys(t, edges):
    """The translation table's rows shared between two surveys and not with the PUF (shared_with: the survey that owns
    the slot): taken as reported, each survey year cut on the owner's cut points of the nearest year it has (the
    CPS's spouse_wage_share is cut within each CPS year), so a band means the same share on both. The report: the
    survey and the owner's same or nearest year, each by describe_share, appended to data/translation_report.csv."""
    w = t['weight'].to_numpy(float)
    report = []
    for row in TR.shared_rows():
        src, v, owner = row['survey'], row['variable'], row['shared_with']
        assert v in AMOUNTS and v in t, f'{src} {v}: a shared amount the table does not have'
        owner_years = sorted(t.loc[t.source == owner, 'year'].unique())
        for y in sorted(t.loc[t.source == src, 'year'].unique()):
            oy = min(owner_years, key=lambda c: (abs(c - y), c))
            sel = ((t.source == src) & (t.year == y)).to_numpy()
            o = ((t.source == owner) & (t.year == oy)).to_numpy()
            b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel], grown_edge(edges[f'{owner}/{oy}/{v}'], 1.0, 1.0))
            rec['cut_rule'] = f'taken as reported; the {owner.upper()} {oy} cut points'
            t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = b, rec
            report += TR.report_rows(row, y, {'survey': TR.describe_share(t.loc[sel, v].to_numpy(float), w[sel]),
                                              f'{owner.upper()} (same year)' if oy == y else f'{owner.upper()} (nearest year)':
                                              TR.describe_share(t.loc[o, v].to_numpy(float), w[o])})
    if report:
        rep = pd.DataFrame(report)
        TR.say_report(rep)
        f = f'{common.DATA}/translation_report.csv'
        pd.concat([pd.read_csv(f), rep], ignore_index=True).to_csv(f, index=False)
    return t


def table_meta(t, factor):
    """What later stages need to know about the table: the items and who has them, the computed totals and
    flags, the surveys' own slots, the AGI signs, net worth's parts."""
    item_order = list(ITEMS) + list(PARTS) + STAGE3
    return {'items': {v: ('shared' if v in ITEMS and ITEMS[v][1] else 'puf') for v in item_order},
            'computed_totals': {k: COMPUTED[k] for k in ['dividends', 'adjustments', 'business_income', 'sch_cf_income',
                                                         'sch_e_income', 'sche_rent', 'sche_rest']},
            'categories': CATEGORIES,
            'tax_categories': TAX_CATEGORIES,
            'puf_categories': PUF_CATEGORIES,
            'scf_categories': SCF_CATEGORIES,
            # (29 October) the QBI items: each kind's workers drawn after its industry group (zero with none); the
            # industry group never none where any of the kind's PUF parts is nonzero
            'scf_structure': {'after': {f'biz_workers_{k}': f'biz_industry_{k}' for k in QBI_KINDS},
                              'industry_parts': {f'biz_industry_{k}': parts for k, parts in QBI_KINDS.items()}},
            'computed_flags': {'k1_owner': K1_PARTS},
            'scf_amounts': SCF_AMOUNTS,
            'cex_amounts': CEX_AMOUNTS,
            'cps_amounts': CPS_AMOUNTS,
            'cps_categories': CPS_CATEGORIES,
            'person_categories': PERSON_CATEGORIES,
            'exact_ages': EXACT_AGES,
            'cps_structure': {'spouse': SPOUSE_SLOTS, 'dependent_ages': DEPENDENT_AGE_SLOTS,
                              'primary_age': 'age1', 'ages': AGE_SLOTS,
                              # (28 October) each adult's overtime: drawn after the other CPS slots; the primary
                              # filer's zero without wages, the spouse's without a spouse's share of wages above zero
                              'overtime': CPS_OVERTIME,
                              # (28 October) the dependents' Social Security number status: the k-th present
                              # exactly when n_dep >= k, as their ages
                              'dependent_ssn': DEPENDENT_SSN_SLOTS, 'primary_ssn': ['ssn_status_primary'],
                              'zero_without': {'ot_primary': 'wages', 'ot_spouse': 'spouse_wage_share'}},
            'sipp_amounts': SIPP_AMOUNTS,
            'sipp_categories': SIPP_CATEGORIES,
            'sipp_structure': {'spouse': SIPP_SPOUSE_SLOTS, 'lh_of': SIPP_LH_OF,
                               'zero_without': {'tips_primary': 'wages', 'tips_spouse': 'spouse_wage_share'}},
            'present_on': present_on(t),
            'net_worth_parts': NET_WORTH_PARTS,
            'ratios': {'w_primary_ltv': ['value.primary_mortgage', 'value.primary_home'],
                       'spouse_wage_share': ['wages2', 'wages'], 'spouse_se_share': ['se2', 'sch_cf_income'],
                       'spouse_deferral_share': ['elective2', 'elective_deferrals'],
                       'w_dc_stock_share': ['w_dc_stocks', 'value.dc']},
            'remainders': {'wages1': ['wages', 'wages2'], 'se1': ['sch_cf_income', 'se2'],
                           'elective1': ['elective_deferrals', 'elective2']},
            # after the remainders: a person's amount in dollars held to at most a remainder (a person's tips, at
            # most the person's wages: tips1 is the smaller of the drawn tips_primary and wages1), then the sums of
            # the person items (tips = tips1 + tips2) and the yes/no items that hold only with the item (tips_lh1:
            # the primary filer's leisure and hospitality flag, 0 without tips)
            # (28 October; decided by John 27 October) each adult's overtime pay, Tax-Simulator's ot1 and ot2: the
            # drawn ot_primary held to the person's wages less the person's tips (ot1 at most wages1 - tips1, never
            # below zero). A third entry is subtracted from the cap; it must be made by an earlier entry, so the
            # tips come first and the overtime gives way. ot = ot1 + ot2.
            'person_caps': {'tips_primary': ['tips1', 'wages1'], 'tips_spouse': ['tips2', 'wages2'],
                            'ot_primary': ['ot1', 'wages1', 'tips1'], 'ot_spouse': ['ot2', 'wages2', 'tips2']},
            'sums': {'tips': ['tips1', 'tips2'], 'ot': ['ot1', 'ot2']},
            'flags_with': {'tips_lh1': ['tips_lh_primary', 'tips1'], 'tips_lh2': ['tips_lh_spouse', 'tips2']},
            'kg_pieces': KG_PIECES,
            'agi_signs': {v: ITEMS[v][0] for v in ITEMS if ITEMS[v][0] != 0},
            'growth_factors': factor}


# ---- dollar maps ----------------------------------------------------------------------------
def fill_value(edge, band):
    """The map value for a band with no record: a lump band's cut, else the band's own edge."""
    pos0 = edge['n_bands'] - (len(edge['positive']['cuts']) + 1)
    n_neg = pos0 - 1
    k = edge['negative']['bands_used']
    if 1 <= band <= n_neg and k == 0:
        return 0.0, 'no negatives in data: zero'
    if 1 <= band <= n_neg and band > k:        # an item with few losses uses fewer than n_neg loss bands
        c = edge['negative']['cuts']
        return (float(c[-1]) if c else -1.0), "loss band beyond this item's loss bands: its least negative cut"
    n_neg = k if 1 <= band <= n_neg else n_neg
    if band >= pos0:
        cuts, i = edge['positive']['cuts'], band - pos0
        lo, hi = (cuts[i - 1] if i >= 1 else 0.0), (cuts[i] if i < len(cuts) else None)
    else:
        cuts = edge['negative']['cuts']
        lo, hi = (cuts[band - 2] if band >= 2 else None), (cuts[band - 1] if band < n_neg else 0.0)
    if lo is not None and lo == hi:
        return float(lo), 'empty band: lump value at its cut'
    own, other = (hi, lo) if band >= pos0 else (lo, hi)
    return float(own if own is not None else other), 'no records in this subset: band edge'


def build_map(df, edges, source, variables, year=PUF_YEAR):
    rows = []
    for v in variables:
        x, b, w = df[v].to_numpy(float), df[f'{v}_band'].to_numpy(), df['weight'].to_numpy(float)
        ok = b >= 0
        m = pd.DataFrame({'variable': v, 'band': b[ok], 'value': x[ok], 'weight': w[ok], 'filled': False,
                          'rule': 'real records'})
        edge = edges[f'{source}/{year}/{v}']
        fills = [dict(zip(['value', 'rule'], fill_value(edge, k)), variable=v, band=k, weight=1.0, filled=True)
                 for k in sorted(set(range(edge['n_bands'])) - set(m['band']))]
        rows += [m, pd.DataFrame(fills, columns=m.columns)]
    return pd.concat(rows, ignore_index=True).sort_values(['variable', 'band', 'value'], kind='stable') \
        .reset_index(drop=True)


def cap_scf_map(m, base, wave):
    """The wave's dollar map with each item in settings dollars.scf_caps held to that percentile among the wave's
    holders (weighted, the wave's own dollars, before growth). Only the dollars a drawn band turns into change; the
    training table and its bands do not. Prints the cap and how many map values it lowers."""
    for v, q in common.SETTINGS['dollars'].get('scf_caps', {}).items():
        x, w = base[v].to_numpy(float), base['weight'].to_numpy(float)
        assert np.isfinite(x).all(), f'SCF wave {wave}: {v} missing or not finite'
        h = x > 0
        assert h.any(), f'SCF wave {wave}: no holders of {v}'
        order = np.argsort(x[h], kind='stable')
        xs, cw = x[h][order], np.cumsum(w[h][order]) / w[h].sum()
        cap = float(xs[min(np.searchsorted(cw, q), len(xs) - 1)])
        r = (m['variable'] == v).to_numpy() & (m['value'].to_numpy(float) > cap)
        common.say(f'SCF wave {wave}: {v} held to ${cap:,.0f} (the {q:.1%} point among holders); '
                   f'{int(r.sum())} map values lowered, the largest ${m.loc[r, "value"].max() if r.any() else 0:,.0f}')
        m.loc[r, 'value'] = cap
    return m


def scf_growth(scf, y_from, y_to):
    """{balance-sheet item: its growth per tax unit from the SCF wave of income year y_from to that of y_to}: the ratio
    of the two waves' weighted means per tax unit, each in its own dollars (all units and all five implicates, zeros
    included; scf: SCF rows with year, weight and the items in dollars). It carries price change and the change in
    holdings per unit together. The mortgage's share of the home's value (w_primary_ltv) is a ratio and is not grown:
    the mortgage in dollars moves with the home. An amount the SCF does not ask of some units (SCF_NOT_ASKED: elective
    deferrals, not asked outside the PEU): the mean over the units asked."""
    out = {}
    for v in SCF_AMOUNTS:
        if v in SCF_RATIOS:
            # The mortgage share changes after the final wave so that the home and mortgage each follow their own Z.1 series.
            last = max(int(y) for y in scf['year'].unique()) + 1
            a, b = max(last, y_from + 1), max(last, y_to + 1)
            out[v] = (GR.scf_extension(a, b, 'value.primary_mortgage') /
                      GR.scf_extension(a, b, 'value.primary_home')) if v == 'w_primary_ltv' else 1.0
            continue
        mean = {y: scf_mean(scf, v, y) for y in (y_from, y_to)}
        # a mean at or below zero (possible for the unrealized gains) makes the ratio meaningless: no factor
        out[v] = mean[y_to] / mean[y_from] if mean[y_from] > 0 and mean[y_to] > 0 else np.nan
    return out


def _wave_mean(scf, v, y):
    d = scf[scf.year == y]
    assert len(d), f'no SCF wave of income year {y}'
    if v in SCF_NOT_ASKED:     # the units asked only: the others are missing by design, not zero
        d = d[d[v].notna()]
    assert not d[[v, 'weight']].isna().any().any(), f'SCF {y}: {v} or the weight missing on some unit'
    return float(np.average(d[v].to_numpy(float), weights=d['weight'].to_numpy(float)))


def scf_mean(scf, v, y):
    """The SCF's weighted mean per tax unit of item v at income year y (30 October, targets for every year): a wave's
    own mean where the table has a wave of income year y; between two waves, the geometric interpolation of their means
    (y = a + l (b - a): mean_a^(1 - l) mean_b^l; the arithmetic one if either mean is at or below zero); after the last
    wave, the last wave's mean grown by household net worth per person (the Federal Reserve's Financial Accounts, Z.1,
    over the Social Security area population; stock years y + 1 over the last wave's, the wave of income year y being
    interviewed in year y + 1; past 2025 Z.1 is held at its ratio to nominal GDP; growth.scf_extension). Before the
    first wave: an error."""
    have = sorted(int(x) for x in scf['year'].unique())
    if y in have:
        return _wave_mean(scf, v, y)
    lo, hi = max([w for w in have if w < y], default=None), min([w for w in have if w > y], default=None)
    assert lo is not None, f'SCF: income year {y} is before the first wave ({have[0]})'
    if hi is None:
        return _wave_mean(scf, v, lo) * GR.scf_extension(lo + 1, y + 1, v)
    a, b, lam = _wave_mean(scf, v, lo), _wave_mean(scf, v, hi), (y - lo) / (hi - lo)
    return a ** (1 - lam) * b ** lam if a > 0 and b > 0 else a + lam * (b - a)


def scf_units_at(scf, y):
    """The SCF's number of tax units (sum of weights, as the wave's rows carry them) at income year y, by the rule of
    scf_mean: a wave's own; between waves geometric interpolation; after the last wave grown with the Social Security
    area population (stock years y + 1 over the last wave's)."""
    have = sorted(int(x) for x in scf['year'].unique())
    n = lambda w: float(scf.loc[scf.year == w, 'weight'].sum())
    if y in have:
        return n(y)
    lo, hi = max([w for w in have if w < y], default=None), min([w for w in have if w > y], default=None)
    assert lo is not None, f'SCF: income year {y} is before the first wave ({have[0]})'
    if hi is None:
        return n(lo) * GR.population(y + 1) / GR.population(lo + 1)
    lam = (y - lo) / (hi - lo)
    return n(lo) ** (1 - lam) * n(hi) ** lam


def puf_target_map(m_puf, y=TARGET_YEAR):
    """The PUF year's map with every value grown to year y by its item's growth line (positive and negative values
    each by their own factor); and the factors."""
    g = {v: growth(v, y) for v in PUF_AMOUNTS}
    bad = {k: f for k, f in g.items() if not all(np.isfinite(x) and x > 0 for x in f)}
    assert not bad, f'growth factors missing or not positive (growth.yaml): {bad}'
    m_target = m_puf.copy()
    for v, (fp, fn) in g.items():
        r = (m_target['variable'] == v).to_numpy()
        x = m_target.loc[r, 'value'].to_numpy(float).copy()
        x[x > 0] *= fp
        x[x < 0] *= fn
        m_target.loc[r, 'value'] = x
    return m_target, g


def build_maps(t, edges):
    os.makedirs(common.MAPS, exist_ok=True)
    GR.check(TARGET_YEAR)   # every dollar item has a growth line (stops otherwise)
    pside = t[t.source == 'puf'].reset_index(drop=True)
    m_puf = build_map(pside, edges, 'puf', PUF_AMOUNTS)
    m_puf.to_parquet(f'{common.MAPS}/map_puf_{PUF_YEAR}.parquet', index=False)
    m_target, g = puf_target_map(m_puf)   # to the target year, by each item's growth line
    m_target.to_parquet(f'{common.MAPS}/map_puf_{TARGET_YEAR}.parquet', index=False)
    # the balance sheet: one map per wave, from the wave's own records on its own band edges, each item grown per tax
    # unit to the target year's wave (scf_growth). Generation draws each record's balance sheet from one wave
    # (generate.py), and dollars.py turns its bands into dollars with that wave's map.
    scf = t[t.source == 'scf']
    rows = []
    for y in sorted(scf['year'].unique()):
        base = scf[scf.year == y].reset_index(drop=True)
        f = scf_growth(scf, y, TARGET_YEAR - 1)
        rows += [{'wave': int(y) + 1, 'income year': int(y), 'variable': v, 'factor to the target year': f[v]}
                 for v in SCF_AMOUNTS]
        no_factor = [v for v, x in f.items() if not np.isfinite(x)]
        if no_factor:   # the wave cannot be put in target-year dollars; generation stops if the year's mix uses it
            common.say(f'SCF wave {int(y) + 1}: NO MAP for {TARGET_YEAR}; a mean per unit at or below zero in '
                       f'{no_factor}')
            continue
        m = build_map(base, edges, 'scf', SCF_AMOUNTS, year=y)
        m = cap_scf_map(m, base, int(y) + 1)
        m['value'] = m['value'].to_numpy(float) * m['variable'].map(f).to_numpy(float)
        assert not m['value'].isna().any(), f'SCF {y}: a map value with no growth factor'
        m.to_parquet(f'{common.MAPS}/map_scf_wave{int(y) + 1}_{TARGET_YEAR}.parquet', index=False)
    pd.DataFrame(rows).to_csv(f'{common.MAPS}/scf_growth_factors.csv', index=False)
    common.say('SCF balance sheet, growth per tax unit to the target year\'s wave, by wave:\n'
               + pd.DataFrame(rows).pivot(index='variable', columns='wave', values='factor to the target year')
               .to_string(float_format=lambda x: f'{x:.4f}'))
    # the CEX's categories, the CPS-only amounts and the SIPP's tips: the generation year's real records, and the same
    # grown to the target year by their growth lines (the CEX's by NIPA PCE per return, the category's PCE over SOI's
    # return count; the CPS's childcare and overtime and the SIPP's tips by SOI's growth in wages per return with wages,
    # a placeholder: no series measures them per unit; the CPS's shares, weeks and hours not grown; no growth when the
    # generation year is the target year)
    for src, sec, amounts in [('cex', CX, CEX_AMOUNTS), ('cps', CP, CPS_AMOUNTS), ('sipp', SP, SIPP_AMOUNTS)]:
        yg = sec['generation_year']
        m = build_map(t[(t.source == src) & (t.year == yg)].reset_index(drop=True), edges, src, amounts, year=yg)
        m.to_parquet(f'{common.MAPS}/map_{src}_{yg}.parquet', index=False)
        for v in amounts:
            f = GR.between(v, yg, TARGET_YEAR)
            g[v] = (f, f)
            r = (m['variable'] == v).to_numpy()
            m.loc[r, 'value'] = m.loc[r, 'value'] * f
        m.to_parquet(f'{common.MAPS}/map_{src}_{TARGET_YEAR}.parquet', index=False)
    pd.DataFrame([{'variable': v, 'factor, positive values': fp, 'factor, negative values': fn}
                  for v, (fp, fn) in g.items()]).to_csv(f'{common.MAPS}/growth_factors.csv', index=False)
    build_year_maps(t, edges)


def build_year_maps(t, edges):
    """One map per survey year of the CEX, the CPS and the SIPP in the target year's file (TARGETS.md section 7,
    survey_mix.py): that year's own records, every value grown from that year to the target year, by the same rules
    the single-year maps above use (each item's growth line, growth.yaml). The CEX's categories: NIPA PCE per return,
    the category's PCE over SOI's return count, target year over the survey year. The CPS's childcare and the SIPP's tips: SOI's wages per return with
    wages, target year over the survey year (a placeholder: no series measures them per unit); the CPS's shares, weeks
    and hours are not grown. For the target year itself the factor is 1. Files map_<survey>_from<year>_to<target>.parquet;
    the factors are in survey_year_growth_factors.csv."""
    rows = []
    for src, amounts in [('cex', CEX_AMOUNTS), ('cps', CPS_AMOUNTS), ('sipp', SIPP_AMOUNTS)]:
        for y in mix_years(src):
            m = build_map(t[(t.source == src) & (t.year == y)].reset_index(drop=True), edges, src, amounts, year=y)
            for v in amounts:
                f = GR.between(v, y, TARGET_YEAR)   # the item's growth line (growth.yaml)
                r = (m['variable'] == v).to_numpy()
                m.loc[r, 'value'] = m.loc[r, 'value'].to_numpy(float) * f
                rows.append({'survey': src, 'year': y, 'variable': v, 'factor to the target year': f})
            m.to_parquet(f'{common.MAPS}/map_{src}_from{y}_to{TARGET_YEAR}.parquet', index=False)
    f = pd.DataFrame(rows)
    f.to_csv(f'{common.MAPS}/survey_year_growth_factors.csv', index=False)
    common.say('CEX, CPS and SIPP maps by year, growth factor to the target year (wages per return for the CPS and SIPP '
               'dollars):\n' + f.pivot_table(index=['survey', 'variable'], columns='year', values='factor to the target year')
               .to_string(float_format=lambda x: f'{x:.4f}'))


def present_on(t):
    """Per slot, the sources that have it: an amount where its band is set, a category where it is not -1."""
    out = {}
    for v in AMOUNTS:
        out[v] = sorted(s for s in t.source.unique() if (t.loc[t.source == s, f'{v}_band'] >= 0).any())
    for v in ['married', 'n_dep', 'age_range', 'filer'] + list(TAX_CATEGORIES) + list(CATEGORIES) + list(PERSON_CATEGORIES) \
            + list(SIPP_CATEGORIES) + list(PUF_CATEGORIES) + list(SCF_CATEGORIES):
        out[v] = sorted(s for s in t.source.unique() if (t.loc[t.source == s, v] >= 0).any())
    return out


def mix_years(source):
    """The tax years of the survey in the target year's file (the year file's prior, survey_mix.py), the target year
    first when it is among them."""
    import survey_mix
    return sorted(survey_mix.years(source), key=lambda y: (y != TARGET_YEAR, y))


def cex_target_units(pi):
    """The CEX's tax units, all of them (a consumer unit's spending divided among its tax units by head count,
    build_cex.R, which folds dependent filers into the unit that claims them), with the primary filer's age range by
    the rule the training rows use, for each year of the CEX's mix in the target year's file. For the tilt's consumption
    targets and the report: data/cex_units_<year>_all.parquet."""
    for y in mix_years('cex'):
        s = pd.read_csv(f'{common.DATA}/cex_units_{y}.csv.gz')
        couple = (s['married'] == 1).to_numpy()
        assert not s.loc[couple, ['age2', 'male2']].isna().any().any() and not s[['age1', 'male1']].isna().any().any()
        age, _ = ages.primary_age(s.age1.to_numpy(float), s.age2.fillna(0).to_numpy(float), s.male1.to_numpy(float),
                                  s.male2.fillna(1).to_numpy(float), couple, s['household_id'].astype(float).to_numpy(),
                                  pi)
        assert (s['dep_filer'] == 0).all()
        s['age_range'] = ages.adult_range(age)
        s.to_parquet(f'{common.DATA}/cex_units_{y}_all.parquet', index=False)
        w = s['weight']
        common.say(f'CEX {y}, all tax units: {w.sum() / 1e6:.1f} million; '
                   'spending ($bn): ' + ', '.join(f'{c} {(s[c] * w).sum() / 1e9:,.0f}' for c in CEX_CATS))


def primary_age_range(t, puf):
    """age_range on the SCF, CEX and CPS rows: the range of the primary filer's age, couples by the rule in
    ages.py, with the woman-first chances set on the SCF's mixed-sex couples (all waves, weighted)."""
    sv = (t.source == 'scf').to_numpy()
    cx = (t.source == 'cex').to_numpy()
    cp = (t.source == 'cps').to_numpy()
    si = (t.source == 'sipp').to_numpy()
    assert (t.loc[t.source == 'puf', 'filer'] == 1).all(), 'the PUF holds filers only'
    rows = sv | cx | cp | si
    couple = (t.married == 1).to_numpy() & rows
    # sex not observed (missing): only on the SCF's units outside the PEU, whose members' sex the SCF does not report.
    # A couple there is ordered by neither sex rule: its primary filer is the unit's head (person 1), as for one adult.
    unknown_sex = (t.male1.isna() | (couple & t.male2.isna())).to_numpy() & rows
    assert not t.loc[rows, 'svy_age1'].isna().any() and (t.loc[unknown_sex, 'source'] == 'scf').all()
    assert not t.loc[couple, 'svy_age2'].isna().any() and (t.loc[couple, 'svy_age2'] >= 0).all()
    ordered = couple & ~unknown_sex      # couples the sex rules order
    mixed = ordered & (t.male1 != t.male2).to_numpy()
    m = t[mixed & sv]
    man = np.where(m.male1 == 1, m.svy_age1, m.svy_age2)
    woman = np.where(m.male1 == 1, m.svy_age2, m.svy_age1)
    target = ages.puf_female_primary_share(puf)
    pi, share = ages.woman_first_chance(man, woman, m.weight, target)
    a1 = t.svy_age1.fillna(0).to_numpy(float)
    a2 = t.svy_age2.fillna(0).to_numpy(float)
    # sex enters only for the couples the rules order (primary_age reads it nowhere else); the PUF rows' and the
    # unknown-sex rows' missing values are not read
    m1 = np.where(ordered, t.male1.to_numpy(float), 0.0)
    m2 = np.where(ordered, t.male2.to_numpy(float), 0.0)
    assert not np.isnan(m1[ordered]).any() and not np.isnan(m2[ordered]).any()
    age, woman_first = ages.primary_age(a1, a2, m1, m2, ordered, t.age_key.fillna(0).to_numpy(), pi)
    cex_target_units(pi)
    t.loc[rows, 'age_range'] = ages.adult_range(age[rows])
    # which person is the primary filer: in a mixed-sex couple the woman if she goes first, else the man; in a
    # same-sex couple the older (as ages.primary_age); in a couple of unknown sex, person 1
    primary_is_2 = (mixed & (woman_first == (m1 == 1))) | (ordered & ~mixed & (a2 > a1))
    t = cps_person_slots(t, primary_is_2)
    t = sipp_person_slots(t, primary_is_2)
    t = scf_person_slots(t, primary_is_2)
    # the exact ages on the SCF and CEX rows (the CPS's are set above), where the translation table shares them: the
    # primary filer's age by the same rule as its range, the spouse's on couples only
    spouse = np.where(couple, np.where(primary_is_2, a1, a2), -1.0)
    assert np.array_equal(np.where(primary_is_2, a2, a1)[rows], age[rows])
    for src, sel in [('scf', sv), ('cex', cx), ('sipp', si)]:
        for v, x in [('age1', age), ('age2', spouse)]:
            if shares(src, v):
                t.loc[sel, v] = exact_age_value(x[sel])
        d = t[sel]
        if shares(src, 'age1'):
            assert (ages.adult_range(d.age1 - 1) == d.age_range).all(), f'{src}: an exact age outside its range'
            assert ((d.age2 > 0) == (d.married == 1)).all(), f'{src}: a spouse age off a couple'
    w = t.weight.to_numpy(float)
    report = pd.DataFrame({'range': ['under 26', '26-34', '35-44', '45-54', '55-64', '65+'],
                           'woman-first chance by the man\'s range (%)': 100 * pi,
                           'PUF female primaries (%)': 100 * target,
                           'SCF female primaries after the rule (%)': 100 * share})
    common.say('primary filer of survey couples:\n' + report.round(1).to_string(index=False))
    for lab, sel in [('SCF', sv), ('CEX', cx), ('CPS', cp), ('SIPP', si)]:
        c = couple & sel
        common.say(f'  {lab}: woman first in {100 * w[c & woman_first].sum() / w[c].sum():.1f}% of couples '
                   f'(weighted); same-sex couples {100 * w[c & ordered & ~mixed].sum() / w[c].sum():.2f}%; '
                   f'sex not observed {100 * w[c & unknown_sex].sum() / w[c].sum():.2f}%')
    assert t.age_range.notna().all()
    return t.drop(columns=['svy_age1', 'svy_age2', 'male1', 'male2', 'age_key', 'taxunit_code'] + PERSON_COLUMNS +
                  SIPP_PERSON_COLUMNS, errors='ignore'), pi


def age_range_table(t):
    """Weighted shares by age range: PUF filers (step 0's probabilities where no range), SCF waves, CEX and CPS years."""
    labels = common.AGE_LABELS
    groups = [('PUF filers', (t.source == 'puf') & (t.filer == 1))] + \
        [(f'SCF {y}', (t.source == 'scf') & (t.year == y)) for y in sorted(t.loc[t.source == 'scf', 'year'].unique())] + \
        [(f'CEX {y}', (t.source == 'cex') & (t.year == y)) for y in sorted(t.loc[t.source == 'cex', 'year'].unique())] + \
        [(f'CPS {y}', (t.source == 'cps') & (t.year == y)) for y in sorted(t.loc[t.source == 'cps', 'year'].unique())] + \
        [(f'SIPP {y}', (t.source == 'sipp') & (t.year == y)) for y in sorted(t.loc[t.source == 'sipp', 'year'].unique())]
    cols = {}
    for lab, sel in groups:
        d = t[sel]
        P = np.column_stack([(d.age_range == k).to_numpy(float) + d[f'age_p{k}'].to_numpy() for k in range(ages.N_RANGES)])
        cols[lab] = 100 * (d.weight.to_numpy()[:, None] * P).sum(0) / d.weight.sum()
    out = pd.DataFrame(cols, index=labels)
    out.to_csv(f'{common.DATA}/age_range_shares.csv')
    common.say('age range, % of weight:\n' + out.round(1).to_string())


def table_summary(t):
    """Rows by source; the CPS's units, expected filers and expected non-filers by year (millions, weighted); the tax
    data's non-filers by year. Written to data/table_summary.csv."""
    w = t.weight.to_numpy(float)
    common.say('rows by source: ' + json.dumps(t.groupby('source').size().to_dict()))
    rows = []
    for y in CP['years']:
        c = ((t.source == 'cps') & (t.year == y)).to_numpy()
        p = t.loc[c, 'filer_p'].to_numpy(float)
        rows.append({'year': y, 'rows': int(c.sum()), 'units (millions)': w[c].sum() / 1e6,
                     'expected filers (millions)': (w[c] * p).sum() / 1e6,
                     'expected non-filers (millions)': (w[c] * (1 - p)).sum() / 1e6,
                     'drawn filers (millions)': (w[c] * t.loc[c, 'filer'].to_numpy()).sum() / 1e6})
    out = pd.DataFrame(rows)
    out.to_csv(f'{common.DATA}/table_summary.csv', index=False)
    common.say('CPS units by year:\n' + out.round(3).to_string(index=False))


def main():
    os.makedirs(common.DATA, exist_ok=True)
    puf = read_puf()
    t = pd.concat([puf_rows(puf), scf_rows(), cex_rows(), cps_rows(), sipp_rows()], ignore_index=True)
    assert not t.duplicated(['source', 'record_id']).any()
    t, pi = primary_age_range(t, puf)
    t, edges = band_table(t)
    cps_ss_taxable_check(t)
    puf_ss_check(t, puf)
    # the report's CPS and SIPP units also carry their wages after function 1 (the CPS's: the tax data's wages at the
    # same percentile among earners since 28 October), so the report's wage groups mean the same on both sides
    for src in ('cps', 'sipp'):
        for y in mix_years(src):
            f = f'{common.DATA}/{src}_units_{y}_all.parquet'
            u = pd.read_parquet(f)
            tw = t.loc[(t.source == src) & (t.year == y), ['record_id', 'wages']]
            u = u.merge(tw.rename(columns={'wages': 'wages_translated'}), on='record_id', how='left', validate='one_to_one')
            assert u['wages_translated'].notna().all()
            u.to_parquet(f, index=False)
    keep = ['source', 'year', 'record_id', 'household_id', 'implicate', 'weight', 'married', 'n_dep', 'filer', 'filer_p'] + \
        list(TAX_CATEGORIES) + list(CATEGORIES) + list(PERSON_CATEGORIES) + list(SIPP_CATEGORIES) + list(PUF_CATEGORIES) + \
        list(SCF_CATEGORIES) + AMOUNTS + ['net_worth', 'value.primary_mortgage'] + [f'{v}_band' for v in AMOUNTS] + \
        ['age_range'] + [f'age_p{k}' for k in range(ages.N_RANGES)] + ['agi_group']
    t['implicate'] = t['implicate'].fillna(0).astype(np.int64)
    for k in range(ages.N_RANGES):
        t[f'age_p{k}'] = t[f'age_p{k}'].fillna(0.0)
    for c in SIPP_CATEGORIES:     # SIPP only
        assert not t.loc[t.source == 'sipp', c].isna().any() and t.loc[t.source != 'sipp', c].isna().all(), c
        t[c] = t[c].fillna(-1)
    for c in PUF_CATEGORIES:      # PUF only
        assert not t.loc[t.source == 'puf', c].isna().any() and t.loc[t.source != 'puf', c].isna().all(), c
        t[c] = t[c].fillna(-1)
    for c in SCF_CATEGORIES:      # SCF only
        assert not t.loc[t.source == 'scf', c].isna().any() and t.loc[t.source != 'scf', c].isna().all(), c
        t[c] = t[c].fillna(-1)
    for c in PERSON_CATEGORIES:   # absent (-1) on every source that does not report it
        has = [src for src in ('scf', 'cex', 'sipp') if c in EXACT_AGES and shares(src, c)] + ['cps']
        assert not t.loc[t.source.isin(has), c].isna().any(), c
        assert t.loc[~t.source.isin(has), c].isna().all(), c
        t[c] = t[c].fillna(-1)
    assert not t['filer_p'].isna().any() and ((t.filer_p == -1) == ~t.source.isin(['puf', 'cps'])).all()
    assert (t.loc[t.source == 'puf', 'filer_p'] == 1).all() and t.loc[t.source == 'cps', 'filer_p'].between(0, 1).all()
    for c in ['year', 'married', 'n_dep', 'filer', 'age_range'] + list(TAX_CATEGORIES) + list(CATEGORIES) + \
            list(PERSON_CATEGORIES) + list(SIPP_CATEGORIES) + list(PUF_CATEGORIES) + list(SCF_CATEGORIES):
        assert not t[c].isna().any(), c
        t[c] = t[c].astype(np.int64)
    age_range_table(t)
    json.dump(edges, open(common.EDGES, 'w'), indent=1)   # dollars.py, which the label uses, reads the edges' meta
    build_maps(t, edges)
    t, edges = agi_group_label(t, edges)                   # from the bands and the PUF year's map
    t[keep].to_parquet(common.TABLE, index=False)
    json.dump(edges, open(common.EDGES, 'w'), indent=1)
    common.say(f'{len(t):,} rows:', t.groupby(['source', 'year', 'filer']).size().to_dict())
    table_summary(t)


def maps_only():
    """The target year's dollar maps alone (30 October, targets for every year), from the table and band edges already
    built (common.TABLE, common.EDGES): the networks trained on that table stay valid, and each target year gets its
    maps by the same build_maps the full build runs. `python build_table.py maps`."""
    t = pd.read_parquet(common.TABLE)
    edges = json.load(open(common.EDGES))
    build_maps(t, edges)
    common.say(f'maps for {TARGET_YEAR} written to {common.MAPS}')


if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == 'maps':
        maps_only()
    else:
        main()
