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
status: 1 single, 2 joint with surviving spouses, 3 separate, 4 head of household), n_ctc (child credit children,
PUF N24) and n_eitc (EITC children, PUF EIC), each 0-3 (3 = 3 or more); filer_p (-1 on the SCF and CEX); the amounts:
  wages E00200, taxable interest E00300, ordinary dividends E00600;
  capital gains as line 13 (E01000 + E01100) and its four pieces: short term
    P22250, long term P23250, distributions E01100, other E01000 - P22250 - P23250;
  business income E00900 + E02000 + E02100 and its eighteen parts (PARTS), and
    two subtotals of it shared with the SCF: Schedules C and F (sch_cf_income;
    SCF X5704) and Schedule E (sch_e_income; SCF X5714, which is where the SCF
    puts partnership and S corporation income);
  age_range: the primary filer's age range (ages.py). On the PUF the
    demographic file's range, or, where it has none, -1 and step 0's
    probabilities (age_p0-age_p8), from which training draws; on the SCF, CEX
    and CPS, the range of the primary filer's age, couples by the
    primary-filer rule;
  the other AGI items (ITEMS), and other_agi = AGI minus everything above;
  gross Social Security plus pensions, and tax-exempt interest (not in AGI);
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
    n_dep, the age range, wages, ss_gross (gross Social Security and
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
    usual weekly hours, childcare; exact ages of up to three dependents, and
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
              SOI's growth in dollars per return with the item
              (growth_factors.csv; targets.py has the SOI lookups);
  SCF         the balance sheet, one map per wave from that wave's own records
              (generation draws each record's balance sheet from one wave), for
              the target year only: each item grown per tax unit from its wave to
              the target year's wave (the wave of income year target - 1), by the
              ratio of the two waves' weighted means per unit, each in its own
              dollars (scf_growth; scf_growth_factors.csv). The mortgage's share
              of the home's value is a ratio and is not grown;
  CEX         the generation year's records; the target year's map grows them by
              NIPA PCE per return;
  CPS         the generation year's records; the target year's map grows childcare
              by SOI's wages per return, and leaves the shares, weeks and hours.

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
import targets as TG
import translate as TR
from common import PUF_YEAR, TARGET_YEAR

RAW = common.RAW
# ---- constants -------------------------------------------------------------------
assert PUF_YEAR == 2015, 'ages.py carries 2015 constants'
WAVE_2013_ID_OFFSET = 2.013e12
# band layout: 0 zero; 1-10 losses (shares of loss weight from the least negative); 11-83 positives
N_NEG, NEG0, POS0 = 10, 1, 11
NEG_SHARES = np.array([0.225, 0.45, 0.675, 0.90, 0.93, 0.96, 0.99, 0.995, 0.999])
POS_SHARES = np.round(np.concatenate([0.02 * np.arange(1, 46), 0.90 + 0.01 * np.arange(1, 10),
                                      0.99 + 0.001 * np.arange(1, 10), 0.999 + 0.0001 * np.arange(1, 10)]), 6)
N_BANDS = POS0 + len(POS_SHARES) + 1
assert N_BANDS == 84

KG_PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']
# the AGI items: PUF fields, sign in AGI (0: not in AGI), whether the SCF has them, the SCF banding rule
ADJ_FIELDS = ['E03150', 'E03210', 'E03220', 'E03230', 'E03240', 'E03260', 'E03270', 'E03290',
              'E03300', 'E03400', 'E03500']
ITEMS = {  # item: (sign in AGI, on the SCF, SCF rule). The order fixes the model's slot order.
    'business_income': (1, False, None), 'sch_cf_income': (0, True, 'rank'), 'sch_e_income': (0, True, 'rank'),
    'other_gains': (1, False, None), 'ira_taxable': (1, False, None),
    'pensions_taxable': (1, False, None), 'ss_taxable': (1, False, None), 'unemployment': (1, False, None),
    'alimony': (1, False, None), 'state_refunds': (1, False, None), 'adjustments': (-1, False, None),
    'retirement_gross': (0, True, 'unchanged'), 'tax_exempt_interest': (0, True, 'unchanged'),
    'mortgage_interest': (0, False, None), 'real_estate_tax': (0, False, None),
    'ss_gross': (0, False, None)}   # gross Social Security, not in AGI; shared with the CEX (not the SCF)
NO_GROWTH = ['mortgage_interest', 'real_estate_tax']   # no SOI 1.4 row: kept in PUF-year dollars in the target map
# (stage 3's items without a Table 1.4 line are added below, STAGE3_NO_GROWTH)
PARTS = {  # part: (PUF field, sign in business income); loss fields are stored positive
    'sch_c': ('E00900', 1), 'part_pass_inc': ('E25940', 1), 'part_nonpass_inc': ('E25980', 1),
    'part_pass_loss': ('E25920', -1), 'part_nonpass_loss': ('E25960', -1), 'part_179': ('E26110', -1),
    'scorp_pass_inc': ('E26170', 1), 'scorp_nonpass_inc': ('E26190', 1), 'scorp_pass_loss': ('E26160', -1),
    'scorp_nonpass_loss': ('E26180', -1), 'scorp_179': ('E26100', -1), 'rent_inc': ('E25850', 1),
    'rent_loss': ('E25860', -1), 'estate_inc': ('E26390', 1), 'estate_loss': ('E26400', -1),
    'farm_rent': ('E27200', 1), 'sche_other': (None, 1), 'farm': ('E02100', 1)}
# Stage 3 (27 October): the PUF items production carries (config/variable_guide/baseline.csv, src/process_puf.R)
# that the model did not draw, with production's names. Every one is a PUF-only amount outside AGI, drawn like the
# other PUF-only items (absent on the SCF, CEX and SIPP; not measured by the CPS, so zero on non-filers). Ordinary
# dividends and the statutory adjustments become totals computed from their parts (COMPUTED), so the totals the
# file had stay exactly the sums of their parts.
DIV_PARTS = {'div_pref': 'E00650', 'div_ord': None}     # qualified dividends; ordinary less qualified (E00600 - E00650)
ADJ_PARTS = dict(zip(['trad_contr_ira', 'sl_int_ded', 'ed_exp', 'tuition_ded', 'dpad', 'se_tax_ded', 'se_health',
                      'hsa_ded', 'keogh_contr', 'early_penalty', 'alimony_exp'], ADJ_FIELDS))
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
# No SOI Table 1.4 line: kept in PUF-year dollars in the target year's map, as mortgage interest and real estate tax
# (itemized deductions, AMT items, credits and other taxes). Table 1.4 grows the dividend parts, the adjustments,
# the two capital gain items (with Schedule D net gain) and part_se (with partnership and S corporation income).
STAGE3_NO_GROWTH = [v for v in NEW_ITEMS if v not in ('kg_1250', 'kg_collect', 'part_se')]
SCHE_PARTS = [p for p, (f, _) in PARTS.items() if f and p not in ('sch_c', 'farm')]
CF_PARTS = ['sch_c', 'farm']                                  # Schedules C and F: the SCF's X5704
E_PARTS = [p for p in PARTS if p not in CF_PARTS]             # Schedule E: the SCF's X5714
K1_PARTS = [p for p in PARTS if p.startswith(('part_', 'scorp_'))]
# The balance sheet (production's wealth_schema.R): net worth is computed from the 20 components. The home
# mortgage is carried as its share of the home's value (w_primary_ltv), so it is drawn after the home.
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin', 'pass_throughs',
          'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
KG_WEALTH = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
NET_WORTH_PARTS = {**{'w_' + c: 1 for c in ASSETS}, **{'w_' + c: -1 for c in DEBTS}}
SCF_AMOUNTS = ['w_' + c for c in ASSETS] + ['w_primary_ltv'] + ['w_' + c for c in DEBTS if c != 'primary_mortgage'] \
    + ['w_' + c for c in KG_WEALTH] + ['auto_int_exp']
# auto_int_exp (since 26 October): the unit's auto loan interest, a year's interest on its vehicle loans at the
# interview's balance and rate (build_scf.R). Not part of net worth.
NO_GROWTH = NO_GROWTH + STAGE3_NO_GROWTH
PUF_AMOUNTS = ['wages', 'interest', 'dividends', 'capital_gains'] + KG_PIECES + list(ITEMS) + list(PARTS) + STAGE3 + \
    ['other_agi']
CEX_CATS = ['c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables', 'c_food_off_premises',
            'c_gasoline', 'c_housing_utilities', 'c_other_services_health']
CEX_AMOUNTS = list(CEX_CATS)
# CPS only (settings "cps"): the spouse's share of the couple's wages and of its self-employment income, each
# person's weeks worked and usual weekly hours, and childcare, as amounts (cut into bands within each year); exact
# ages and sex as categories (CPS_CATEGORIES: the number of values). Every person item is the primary filer's or the
# spouse's, the primary chosen by the rule the age range uses (ages.py).
CPS_AMOUNTS = ['spouse_wage_share', 'spouse_se_share', 'weeks_primary', 'weeks_spouse', 'hours_primary',
               'hours_spouse', 'childcare']
MAX_AGE = 85                                   # the CPS's top code (80 is 80-84, 85 is 85 and over)
AGE_SLOTS = ['age_primary', 'age_spouse', 'age_dep1', 'age_dep2', 'age_dep3']   # value: 0 none, else the age + 1
# The primary filer's and the spouse's exact ages are shared by the SCF, the CEX and the CPS (translation table), absent
# on the PUF. One coding on every source: exact ages to 79, and 80 for 80 and over (EXACT_AGE_TOP). It is the finest
# all three allow: the CPS codes 80-84 as 80 and 85 and over as 85; the CEX codes everyone from 83 (84 in 2019 and
# 2024) as one value, 87 or 88; the SCF reports ages to 95.
EXACT_AGES = TR.EXACT_AGES
EXACT_AGE_TOP = 80
CPS_CATEGORIES = {**{a: MAX_AGE + 2 for a in AGE_SLOTS if a not in EXACT_AGES},
                  'sex_primary': 3, 'sex_spouse': 3}   # CPS only; sex: 0 none, 1 man, 2 woman
PERSON_CATEGORIES = {**{a: EXACT_AGE_TOP + 2 for a in EXACT_AGES}, **CPS_CATEGORIES}   # every person category
SPOUSE_SLOTS = ['age_spouse', 'sex_spouse', 'spouse_wage_share', 'spouse_se_share', 'weeks_spouse', 'hours_spouse']
DEPENDENT_AGE_SLOTS = ['age_dep1', 'age_dep2', 'age_dep3']   # the k-th: present exactly when n_dep >= k
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
# Categories on the PUF and the CPS (-1 on the SCF and CEX): {slot: number of values}. mars 1-4 (0 unused); n_ctc,
# n_eitc 0-3.
TAX_CATEGORIES = {'mars': 5, 'n_ctc': 4, 'n_eitc': 4}


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
           'weight': raw.S006 / 100, 'married': (I['MARS'] == 2).astype(np.int64), 'mars': I['MARS'],
           'n_ctc': np.minimum(I['N24'], 3).astype(np.int64), 'n_eitc': np.minimum(I['EIC'], 3).astype(np.int64),
           'dep_filer': (puf['DSI'] == 1).astype(np.int64).to_numpy(),
           'n_dep': np.minimum(n_dep, 3).astype(np.int64), 'filer': 1, 'filer_p': 1.0,
           'wages': I['E00200'], 'interest': I['E00300'], 'dividends': I['E00600']}
    kg = {'kg_st': I['P22250'], 'kg_lt': I['P23250'], 'kg_dist': I['E01100'],
          'kg_other': I['E01000'] - I['P22250'] - I['P23250']}
    out.update(kg, capital_gains=I['E01000'] + I['E01100'])
    items = {'business_income': I['E00900'] + I['E02000'] + I['E02100'], 'other_gains': I['E01200'],
             'ira_taxable': I['E01400'], 'pensions_taxable': I['E01700'], 'ss_taxable': I['E02500'],
             'unemployment': I['E02300'], 'alimony': I['E00800'], 'state_refunds': I['E00700'],
             'adjustments': sum(I[c] for c in ADJ_FIELDS), 'retirement_gross': I['E02400'] + I['E01500'],
             'ss_gross': I['E02400'],
             'tax_exempt_interest': I['E00400'], 'mortgage_interest': I['E19200'], 'real_estate_tax': I['E18500']}
    parts = {p: I[f] for p, (f, _) in PARTS.items() if f}
    parts['sche_other'] = I['E02000'] - sum(PARTS[p][1] * parts[p] for p in SCHE_PARTS)
    out.update(parts)
    assert np.array_equal(sum(PARTS[p][1] * parts[p] for p in PARTS), items['business_income'])
    items['sch_cf_income'] = sum(PARTS[p][1] * parts[p] for p in CF_PARTS)
    items['sch_e_income'] = sum(PARTS[p][1] * parts[p] for p in E_PARTS)
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
    out['other_agi'] = I['E00100'] - out['wages'] - out['interest'] - out['dividends'] - out['capital_gains'] - \
        sum(ITEMS[v][0] * items[v] for v in ITEMS)
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
            'sch_e_income': {p: PARTS[p][1] for p in E_PARTS}}


def computed_totals(df):
    """The computed totals from their parts."""
    for v, parts in COMPUTED.items():
        df[v] = sum(s * df[p] for p, s in parts.items())
    return df


_GROWTH = {}


def growth(v, y):
    """(factor for positive values, for negative values) of PUF amount v: SOI's growth in dollars per return with the
    item, PUF year to y (targets.py; past the last SOI year, carried forward). Losses on capital assets do not grow
    (the $3,000 cap is nominal); mortgage interest and real estate tax have no Table 1.4 row and do not grow."""
    if (v, y) not in _GROWTH:
        G = lambda item, **k: TG.per_return_growth(item, year=y, **k)
        if v in ('capital_gains', 'kg_st', 'kg_lt', 'kg_other', 'kg_1250', 'kg_collect'):
            f = (G('sch_d_net_gain'), 1.0)
        elif v == 'kg_dist':
            f = (G('kg_dist'), 1.0)
        elif v in NO_GROWTH:
            f = (1.0, 1.0)
        elif v in TG.PART_GROWTH:
            pos_rows, neg_rows = TG.PART_GROWTH[v]
            fp = G('business_income') if pos_rows == 'business_income' else G(None, rows=pos_rows, absolute=True)
            f = (fp, G(None, rows=neg_rows, absolute=True) if neg_rows else fp)
        else:
            assert v in ['wages', 'interest', 'dividends', 'other_agi'] + list(ITEMS) + list(DIV_PARTS) + \
                list(ADJ_PARTS), v
            f = (G(v), G(v))
        _GROWTH[(v, y)] = f
    return _GROWTH[(v, y)]


def growth_factors(y):
    """growth() of every PUF amount."""
    return {v: growth(v, y) for v in PUF_AMOUNTS}


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
    for all its parts, from the parts; k1_owner likewise from the partnership and S corporation parts."""
    out = {r['variable']: TR.survey_amount(d, r, year) for r in TR.rows(survey, amounts_only=True)}
    for v, parts in COMPUTED.items():
        if v not in out and set(parts) <= set(out):
            out[v] = sum(s * out[p] for p, s in parts.items())
    if set(K1_PARTS) <= set(out):
        out['k1_owner'] = np.column_stack([out[p] != 0 for p in K1_PARTS]).any(1).astype(np.int64)
    return pd.DataFrame(out, index=d.index)


TAX_DATA_CATEGORIES = ['filer', 'married', 'n_dep', 'k1_owner'] + list(TAX_CATEGORIES)


def tax_data(t, y, pi=None):
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
    pi is not used (kept for the signature). Columns: weight, the categories, age_p0-5 (the age range as
    probabilities), the amounts shared by the surveys of year y."""
    puf = t[(t.source == 'puf') & (t.dep_filer == 0)]
    assert (puf.filer == 1).all(), 'the PUF holds filers only'
    P = np.column_stack([(puf.age_range == k).to_numpy(float) + puf[f'age_p{k}'].to_numpy(float)
                         for k in range(common.ADULT_RANGES)])
    assert np.allclose(P.sum(1), 1), 'a PUF filer other than a dependent filer with a dependent-filer age range'
    status = ['single', 'joint', 'separate', 'head_of_household']
    t_y, t_0 = TG.table_1_6(y), TG.table_1_6(PUF_YEAR)
    F = np.array([[t_y[(s, k)] / t_0[(s, k)] for k in range(common.ADULT_RANGES)] for s in status])
    f = (P * F[puf.mars.to_numpy().astype(np.int64) - 1]).sum(1)
    # the amounts shared by the surveys of year y (an item no survey of year y shares need not have SOI growth to y:
    # other gains has a net loss in 2012)
    surveys = sorted(set(t.loc[(t.year == y) & (t.source != 'puf'), 'source']))
    assert surveys, f'no survey of year {y}'
    shared = sorted({r['variable'] for src in surveys for r in TR.rows(src, amounts_only=True)})
    # and the totals a row is ranked against (rank_against: the CPS's dividends against ordinary dividends)
    shared = sorted(set(shared) | {r['rank_against'] for src in surveys for r in TR.rows(src, amounts_only=True)
                                   if r.get('rank_against')})
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
                       'age1': s['age1'], 'age2': s['age2'], 'male1': s['male1'], 'male2': s['male2'],
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
    p_file), filer the one draw from it build_cps.R makes; mars, n_ctc and n_eitc as build_cps.R writes them. The PUF
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
        ['married', 'n_dep', 'filer', 'p_file', 'mars', 'n_ctc', 'n_eitc', 'age1', 'age2', 'sex1', 'sex2', 'dep_age1', 'dep_age2', 'dep_age3', 'childcare',
         'wages', 'sch_cf_income']
    n_missing = s[need].isna().sum()
    assert not n_missing.any(), f'missing values in the CPS units:\n{n_missing[n_missing > 0]}'
    assert (s['dep_filer'] == 0).all(), 'build_cps.R folds dependent filers into the unit that claims them'
    # the split of the couple's earnings adds up exactly; a spouse exactly when married
    assert np.array_equal(s['wages1'] + s['wages2'], s['wages'])
    assert np.array_equal(s['se1'] + s['se2'], s['sch_cf_income'])
    couple = (s['married'] == 1).to_numpy()
    assert ((s['age2'] >= 0).to_numpy() == couple).all() and ((s['sex2'] > 0).to_numpy() == couple).all()
    assert (s.loc[~couple, ['wages2', 'se2', 'weeks_worked2', 'usual_hours2']] == 0).all().all()
    for c in ['age1', 'age2', 'dep_age1', 'dep_age2', 'dep_age3']:
        assert s[c].between(-1, MAX_AGE).all(), c
    assert s['p_file'].between(0, 1).all() and s['mars'].isin([1, 2, 3, 4]).all()
    assert ((s['mars'] == 2) == couple).all() and (s['n_eitc'] <= s['n_dep']).all() and (s['n_ctc'] <= s['n_dep']).all()
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
                       'age1': s['age1'].astype(float), 'age2': s['age2'].astype(float),
                       'male1': (s['sex1'] == 1).astype(float), 'male2': (s['sex2'] == 1).astype(float),
                       'age_key': hh,
                       **{f'p{k}_{c}': s[f'{c}{k}'].astype(float) for k in (1, 2)
                          for c in ['age', 'sex', 'wages', 'se', 'weeks_worked', 'usual_hours']},
                       **{f'dep_age{k}': s[f'dep_age{k}'].astype(np.int64) for k in (1, 2, 3)},
                       'cps_childcare': s['childcare'].astype(float)})
    for v in AMOUNTS:
        if v not in df:
            df[v] = np.nan
    return df


PERSON_COLUMNS = [f'p{k}_{c}' for k in (1, 2) for c in ['age', 'sex', 'wages', 'se', 'weeks_worked', 'usual_hours']] + \
    ['dep_age1', 'dep_age2', 'dep_age3', 'cps_childcare']


def cps_person_slots(t, primary_is_2):
    """The CPS-only slots, primary filer first (primary_is_2: the rows where person 2 is the primary filer, by the
    rule the age range uses). Ages as categories, 0 none, else the age plus 1 (age 0 is a real infant); sex 0 none,
    1 a man, 2 a woman. The spouse's share of the couple's wages and of its self-employment income: only on married
    couples with some of the item (elsewhere absent: the model is never asked it there). Where the two spouses' self-employment incomes have opposite signs the share is cut to 0-1
    (counted below), so the file's two parts still add up to the couple's total. Also writes the target year's CPS
    units, primary filer first, for report_cps.py: data/cps_units_<target year>_all.parquet."""
    c = (t.source == 'cps').to_numpy()
    sw = primary_is_2[c]
    d = t[c]
    pick = lambda col, k: np.where(sw, d[f'p{3 - k}_{col}'], d[f'p{k}_{col}'])   # k = 1 primary, 2 spouse
    age = {k: pick('age', k) for k in (1, 2)}
    w = d['weight'].to_numpy(float)
    married_ = (d['married'] == 1).to_numpy()
    out = {'age_primary': exact_age_value(age[1]), 'age_spouse': exact_age_value(age[2]),
           'sex_primary': pick('sex', 1), 'sex_spouse': pick('sex', 2),
           'weeks_primary': pick('weeks_worked', 1), 'weeks_spouse': pick('weeks_worked', 2),
           'hours_primary': pick('usual_hours', 1), 'hours_spouse': pick('usual_hours', 2),
           'childcare': d['cps_childcare'].to_numpy(float)}
    for k in (1, 2, 3):
        out[f'age_dep{k}'] = np.where(d[f'dep_age{k}'] >= 0, d[f'dep_age{k}'] + 1, 0)
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
    assert ((d.age_spouse > 0).to_numpy() == married).all() and ((d.sex_spouse > 0).to_numpy() == married).all()
    assert (d.sex_primary > 0).all() and (d.age_primary > 0).all()
    assert (ages.adult_range(d.age_primary - 1) == d.age_range).all(), 'the primary filer\'s age is not in its range'
    assert d.age_primary.between(1, EXACT_AGE_TOP + 1).all() and d.age_spouse.between(0, EXACT_AGE_TOP + 1).all()
    for k in (1, 2, 3):
        assert ((d[f'age_dep{k}'] > 0) == (d.n_dep >= k)).all(), f'age_dep{k} and n_dep disagree'
    for v in SPOUSE_SLOTS:
        assert (d.loc[~married, v] == 0).all() or (v in SHARES and d.loc[~married, v].isna().all()), v
    for v in SHARES:
        common.say(f'CPS {v}: recorded on {int(d[v].notna().sum()):,} rows (married couples with some of the item)')
    keep = ['year', 'record_id', 'household_id', 'weight', 'married', 'n_dep', 'filer', 'filer_p', 'age_range'] + \
        list(TAX_CATEGORIES) + \
        list(PERSON_CATEGORIES) + CPS_AMOUNTS + ['wages_primary_reported', 'wages_spouse_reported', 'se_primary_reported',
                                              'se_spouse_reported'] + [c for c in d if c.startswith('raw_')]
    d[d.year == TARGET_YEAR][keep].to_parquet(f'{common.DATA}/cps_units_{TARGET_YEAR}_all.parquet', index=False)
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
                       'age1': s['age1'].astype(float), 'age2': s['age2'].astype(float),
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
    filer first, for report_sipp.py: data/sipp_units_<target year>_all.parquet."""
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
    w = d.weight.to_numpy(float)
    common.say(f'SIPP spouse_wage_share: recorded on {int(d.spouse_wage_share.notna().sum()):,} rows (married couples '
               f'with some wages); tips above wages: none (checked)')
    keep = ['year', 'record_id', 'household_id', 'weight', 'married', 'n_dep', 'age_range', 'spouse_wage_share'] + \
        SIPP_AMOUNTS + list(SIPP_CATEGORIES) + [c for c in out if c.endswith('_reported')] + \
        [c for c in d if c.startswith('raw_')]
    d[d.year == TARGET_YEAR][keep].to_parquet(f'{common.DATA}/sipp_units_{TARGET_YEAR}_all.parquet', index=False)
    return t.drop(columns=[c for c in out if c.endswith('_reported')])


def exact_age_value(age):
    """An exact-age slot's value from ages in years (negative or missing: none): 0 none, else the age plus 1, with
    ages 80 and over as 80 (EXACT_AGE_TOP), the one coding the SCF, the CEX and the CPS all allow."""
    a = np.asarray(age, float)
    return np.where(np.isnan(a) | (a < 0), 0, np.minimum(np.nan_to_num(a, nan=-1), EXACT_AGE_TOP) + 1).astype(np.int64)


def survey_items(s, survey):
    """The survey's columns that the translation table's amount rows read, as raw_<column> (translate.py turns them
    into the shared slots)."""
    return {f'raw_{c}': s[c].astype(float) for c in sorted({c for r in TR.rows(survey, amounts_only=True)
                                                            for c in r['items']})}


def loan_to_value(s):
    """The home mortgage as a share of the home's value; zero with no home. A mortgage on a unit with
    no home value (none expected) is moved to other debt, so net worth is unchanged; the count is printed."""
    home, mort = s['w_primary_home'].to_numpy(float), s['w_primary_mortgage'].to_numpy(float)
    stray = (home <= 0) & (mort != 0)
    common.say(f'  home mortgage with no home value: {int(stray.sum())} rows, moved to other debt')
    s.loc[stray, 'w_other_debt'] = s.loc[stray, 'w_other_debt'] + s.loc[stray, 'w_primary_mortgage']
    s.loc[stray, 'w_primary_mortgage'] = 0.0
    s['w_primary_ltv'] = np.where(home > 0, s['w_primary_mortgage'] / np.where(home > 0, home, 1.0), 0.0)
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
        s = loan_to_value(s.assign(source='scf', filer=-1, filer_p=-1.0, **{c: -1 for c in TAX_CATEGORIES}))
        if wave == TARGET_YEAR:     # the tilt's wealth targets and wealth by wage group, in this wave's dollars
            s.to_parquet(f'{common.DATA}/scf_units_{TARGET_YEAR}.parquet', index=False)
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
    import qgroups as Q
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
def translate_surveys(t, edges, pi):
    """The shared amounts of every survey year from the translation table (translate.py): the definition fixes,
    function 1 against the tax data for the year, the PUF's cut points grown to the year; then the computed totals
    (and k1_owner) a survey has rows for all the parts of, from the translated parts; then the categories. The
    report: the survey as reported, after function 1, the tax data. Each year's tax data is written to
    data/tax_data_<year>.parquet."""
    w = t['weight'].to_numpy(float)
    report, notes, tax = [], {}, {}
    for src in ('scf', 'cex', 'cps', 'sipp'):
        for y in sorted(t.loc[t.source == src, 'year'].unique()):
            if y not in tax:
                tax[y] = tax_data(t, y, pi)
                tax[y].to_parquet(f'{common.DATA}/tax_data_{y}.parquet', index=False)
            T = tax[y]
            tw = T['weight'].to_numpy(float)
            sel = ((t.source == src) & (t.year == y)).to_numpy()
            d = t[sel]
            done = {}
            for row in TR.rows(src, amounts_only=True):
                v = row['variable']
                reported = TR.survey_amount(d, {**row, 'first_year': None, 'part': None, 'loss_cap': None}, y)
                x = TR.survey_amount(d, row, y)
                fp, fn = growth(v, y)
                line = None if row['dollar_line'] is None else row['dollar_line'] * fp
                # rank_against (stage 3): a survey total put in one of its parts is matched to the tax data's total
                against = row.get('rank_against', v)
                new, beyond = TR.amounts(x, w[sel], T[against].to_numpy(float), tw, line)
                done[v] = new
                b, rec = band_variable(new, w[sel], grown_edge(edges[f'puf/{PUF_YEAR}/{v}'], fp, fn))
                rec['cut_rule'] = (f'PUF {PUF_YEAR} cut points x per-return growth to {y} ({fp:.4f} positive, '
                                   f'{fn:.4f} losses); dollar line {line}')
                t.loc[sel, v], t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = new, b, rec
                notes[f'{src} {y} {v}'] = {'dollar line (survey-year dollars)': line,
                                           'units past the tax data\'s last holder (millions)': beyond / 1e6}
                report += TR.report_rows(row, y, {'as reported': TR.describe(reported, w[sel]),
                                                  'after function 1': TR.describe(new, w[sel]),
                                                  'tax data': TR.describe(T[against].to_numpy(float), tw)})
            # computed totals the survey has no row for but has every part of: from the translated parts
            for v, parts in COMPUTED.items():
                if v in done or not set(parts) <= set(done):
                    continue
                new = sum(sg * done[p] for p, sg in parts.items())
                fp, fn = growth(v, y)
                b, rec = band_variable(new, w[sel], grown_edge(edges[f'puf/{PUF_YEAR}/{v}'], fp, fn))
                rec['cut_rule'] = (f'computed from its translated parts; PUF {PUF_YEAR} cut points x per-return growth '
                                   f'to {y} ({fp:.4f} positive, {fn:.4f} losses)')
                t.loc[sel, v], t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = new, b, rec
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
    common.say('function 1: ' + json.dumps(notes, indent=1, default=float))
    factors = {}
    for y, T in tax.items():   # the amounts of the surveys of year y, and the totals computed from them
        have = {v for v in T.columns if v in PUF_AMOUNTS}
        have |= {v for v, parts in COMPUTED.items() if set(parts) <= have}
        factors[str(y)] = {v: list(growth(v, y)) for v in sorted(have)}
    return t, factors


# Taxable Social Security (Form 1040 worksheet): base and adjusted amounts by filing status (1 single, 2 joint, 3
# separate, taken as living apart, 4 head of household); not indexed
SS_BASE = {1: 25_000, 2: 32_000, 3: 25_000, 4: 25_000}
SS_ADJUSTED = {1: 34_000, 2: 44_000, 3: 34_000, 4: 34_000}


def taxable_social_security(ss, other_income, mars):
    """The taxable part of Social Security benefits ss by the worksheet: provisional income is the other income in
    AGI plus tax-exempt interest plus half the benefits."""
    base = np.vectorize(SS_BASE.get)(mars).astype(float)
    adj = np.vectorize(SS_ADJUSTED.get)(mars).astype(float)
    pi = other_income + 0.5 * ss
    tier1 = np.minimum(0.5 * ss, 0.5 * np.maximum(pi - base, 0))
    tier2 = np.minimum(0.85 * ss, 0.85 * np.maximum(pi - adj, 0) + np.minimum(0.5 * ss, 0.5 * (adj - base)))
    return np.where(pi <= base, 0.0, np.where(pi <= adj, tier1, tier2))


def cps_ss_taxable_check(t):
    """For each CPS year: the units, and the expected non-filers (weight x (1 - filer_p)), the taxable-benefits
    formula would give a positive taxable Social Security, from the CPS's amounts in PUF terms (before function 1).
    The translation table keeps taxable Social Security at zero on the CPS (agreed 23 October); this only counts.
    Written to data/cps_ss_taxable_check.csv."""
    rows = []
    for y in CP['years']:
        d = t[(t.source == 'cps') & (t.year == y)]
        a = survey_puf_amounts(d, 'cps', y)
        other = a['wages'] + a.get('interest', 0.0) + a['dividends'] + a['capital_gains'] + \
            sum(ITEMS[v][0] * a.get(v, 0.0) for v in ITEMS if ITEMS[v][0] != 0 and v != 'ss_taxable') + a['other_agi'] + \
            a['tax_exempt_interest']
        tx = taxable_social_security(a['ss_gross'].to_numpy(float), other.to_numpy(float), d['mars'].to_numpy())
        w, p = d['weight'].to_numpy(float), d['filer_p'].to_numpy(float)
        pos, ss = tx > 0, a['ss_gross'].to_numpy(float) > 0
        nfw = w * (1 - p)
        rows.append({'year': y, 'units with Social Security (millions)': w[ss].sum() / 1e6,
                     'expected non-filers with Social Security (millions)': nfw[ss].sum() / 1e6,
                     'expected non-filers with taxable benefits by the formula (millions)': nfw[pos].sum() / 1e6,
                     'their taxable benefits by the formula ($bn)': (nfw * tx).sum() / 1e9,
                     'expected filers with taxable benefits by the formula (millions)': (w * p)[pos].sum() / 1e6,
                     'all units with taxable benefits by the formula (millions)': w[pos].sum() / 1e6})
    out = pd.DataFrame(rows)
    out.to_csv(f'{common.DATA}/cps_ss_taxable_check.csv', index=False)
    common.say('CPS, taxable Social Security by the formula (kept at zero on the CPS):\n' + out.round(3).T.to_string())


def band_table(t, pi):
    filers = ((t.source == 'puf') & (t.filer == 1)).to_numpy()
    pside = (t.source == 'puf').to_numpy()
    w = t['weight'].to_numpy(float)
    edges = {}
    t = pd.concat([t, pd.DataFrame({f'{v}_band': np.int16(-1) for v in AMOUNTS}, index=t.index)], axis=1)
    for v in PUF_AMOUNTS:                        # cut on the PUF filers, applied to the whole PUF side
        b, rec = band_variable(t.loc[filers, v].to_numpy(float), w[filers])
        edges[f'puf/{PUF_YEAR}/{v}'] = rec
        t.loc[pside, f'{v}_band'] = band_variable(t.loc[pside, v].to_numpy(float), w[pside], grown_edge(rec, 1.0, 1.0))[0]
    t, factors = translate_surveys(t, edges, pi)
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
            'computed_totals': {'dividends': COMPUTED['dividends'], 'adjustments': COMPUTED['adjustments'],
                                'business_income': {p: s for p, (_, s) in PARTS.items()},
                                    'sch_cf_income': {p: PARTS[p][1] for p in CF_PARTS},
                                    'sch_e_income': {p: PARTS[p][1] for p in E_PARTS}},
            'categories': CATEGORIES,
            'tax_categories': TAX_CATEGORIES,
            'computed_flags': {'k1_owner': K1_PARTS},
            'scf_amounts': SCF_AMOUNTS,
            'cex_amounts': CEX_AMOUNTS,
            'cps_amounts': CPS_AMOUNTS,
            'cps_categories': CPS_CATEGORIES,
            'person_categories': PERSON_CATEGORIES,
            'exact_ages': EXACT_AGES,
            'cps_structure': {'spouse': SPOUSE_SLOTS, 'dependent_ages': DEPENDENT_AGE_SLOTS,
                              'primary_age': 'age_primary', 'ages': AGE_SLOTS},
            'sipp_amounts': SIPP_AMOUNTS,
            'sipp_categories': SIPP_CATEGORIES,
            'sipp_structure': {'spouse': SIPP_SPOUSE_SLOTS, 'lh_of': SIPP_LH_OF,
                               'zero_without': {'tips_primary': 'wages', 'tips_spouse': 'spouse_wage_share'}},
            'present_on': present_on(t),
            'net_worth_parts': NET_WORTH_PARTS,
            'ratios': {'w_primary_ltv': ['w_primary_mortgage', 'w_primary_home'],
                       'spouse_wage_share': ['wages2', 'wages'], 'spouse_se_share': ['se2', 'sch_cf_income']},
            'remainders': {'wages1': ['wages', 'wages2'], 'se1': ['sch_cf_income', 'se2']},
            # after the remainders: a person's amount in dollars held to at most a remainder (a person's tips, at
            # most the person's wages: tips1 is the smaller of the drawn tips_primary and wages1), then the sums of
            # the person items (tips = tips1 + tips2) and the yes/no items that hold only with the item (tips_lh1:
            # the primary filer's leisure and hospitality flag, 0 without tips)
            'person_caps': {'tips_primary': ['tips1', 'wages1'], 'tips_spouse': ['tips2', 'wages2']},
            'sums': {'tips': ['tips1', 'tips2']},
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


def scf_growth(scf, y_from, y_to):
    """{balance-sheet item: its growth per tax unit from the SCF wave of income year y_from to that of y_to}: the ratio
    of the two waves' weighted means per tax unit, each in its own dollars (all units and all five implicates, zeros
    included; scf: SCF rows with year, weight and the items in dollars). It carries price change and the change in
    holdings per unit together. The mortgage's share of the home's value (w_primary_ltv) is a ratio and is not grown:
    the mortgage in dollars moves with the home."""
    out = {}
    for v in SCF_AMOUNTS:
        if v == 'w_primary_ltv':
            out[v] = 1.0
            continue
        mean = {}
        for y in (y_from, y_to):
            d = scf[scf.year == y]
            assert len(d), f'no SCF wave of income year {y}'
            assert not d[[v, 'weight']].isna().any().any(), f'SCF {y}: {v} or the weight missing on some unit'
            mean[y] = float(np.average(d[v].to_numpy(float), weights=d['weight'].to_numpy(float)))
        # a mean at or below zero (possible for the unrealized gains) makes the ratio meaningless: no factor
        out[v] = mean[y_to] / mean[y_from] if mean[y_from] > 0 and mean[y_to] > 0 else np.nan
    return out


def build_maps(t, edges):
    os.makedirs(common.MAPS, exist_ok=True)
    pside = t[t.source == 'puf'].reset_index(drop=True)
    m_puf = build_map(pside, edges, 'puf', PUF_AMOUNTS)
    m_puf.to_parquet(f'{common.MAPS}/map_puf_{PUF_YEAR}.parquet', index=False)
    g = dict(growth_factors(TARGET_YEAR))   # to the target year, per return with the item
    bad = {k: f for k, f in g.items() if not all(np.isfinite(x) and x > 0 for x in f)}
    assert not bad, f'SOI growth factors missing (an SOI line not found?): {bad}'
    m_target = m_puf.copy()
    for v, (fp, fn) in g.items():
        r = (m_target['variable'] == v).to_numpy()
        x = m_target.loc[r, 'value'].to_numpy(float).copy()
        x[x > 0] *= fp
        x[x < 0] *= fn
        m_target.loc[r, 'value'] = x
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
        m['value'] = m['value'].to_numpy(float) * m['variable'].map(f).to_numpy(float)
        assert not m['value'].isna().any(), f'SCF {y}: a map value with no growth factor'
        m.to_parquet(f'{common.MAPS}/map_scf_wave{int(y) + 1}_{TARGET_YEAR}.parquet', index=False)
    pd.DataFrame(rows).to_csv(f'{common.MAPS}/scf_growth_factors.csv', index=False)
    common.say('SCF balance sheet, growth per tax unit to the target year\'s wave, by wave:\n'
               + pd.DataFrame(rows).pivot(index='variable', columns='wave', values='factor to the target year')
               .to_string(float_format=lambda x: f'{x:.4f}'))
    # the CEX categories: the generation year's real records, and the same grown to the target year by NIPA PCE per
    # return (the category's PCE over SOI's return count, target year over the CEX year)
    yc = CX['generation_year']
    mc = build_map(t[(t.source == 'cex') & (t.year == yc)].reset_index(drop=True), edges, 'cex', CEX_AMOUNTS, year=yc)
    mc.to_parquet(f'{common.MAPS}/map_cex_{yc}.parquet', index=False)
    pce = pd.read_csv(RAW['pce']).set_index('year')
    m_cex_target = mc.copy()
    for v in CEX_AMOUNTS:
        f = (pce.loc[TARGET_YEAR, v] / TG.all_returns(TARGET_YEAR)) / (pce.loc[yc, v] / TG.all_returns(yc))
        g[v] = (f, f)
        r = (m_cex_target['variable'] == v).to_numpy()
        m_cex_target.loc[r, 'value'] = m_cex_target.loc[r, 'value'] * f
    m_cex_target.to_parquet(f'{common.MAPS}/map_cex_{TARGET_YEAR}.parquet', index=False)
    # the CPS-only amounts: the generation year's real records; for the target year, childcare grown by SOI's growth
    # in wages per return with wages (a placeholder: no series measures childcare spending per unit), the shares,
    # weeks and hours unchanged
    yp = CP['generation_year']
    mp = build_map(t[(t.source == 'cps') & (t.year == yp)].reset_index(drop=True), edges, 'cps', CPS_AMOUNTS, year=yp)
    mp.to_parquet(f'{common.MAPS}/map_cps_{yp}.parquet', index=False)
    m_cps_target = mp.copy()
    for v in CPS_AMOUNTS:
        f = growth('wages', TARGET_YEAR)[0] / growth('wages', yp)[0] if v in CP['dollar_amounts'] else 1.0
        g[v] = (f, f)
        r = (m_cps_target['variable'] == v).to_numpy()
        m_cps_target.loc[r, 'value'] = m_cps_target.loc[r, 'value'] * f
    m_cps_target.to_parquet(f'{common.MAPS}/map_cps_{TARGET_YEAR}.parquet', index=False)
    # the SIPP's tips in dollars: the generation year's real records; for the target year, grown by SOI's growth in
    # wages per return with wages, as the CPS's childcare (no growth when the generation year is the target year)
    ys = SP['generation_year']
    ms = build_map(t[(t.source == 'sipp') & (t.year == ys)].reset_index(drop=True), edges, 'sipp', SIPP_AMOUNTS, year=ys)
    ms.to_parquet(f'{common.MAPS}/map_sipp_{ys}.parquet', index=False)
    m_sipp_target = ms.copy()
    f = growth('wages', TARGET_YEAR)[0] / growth('wages', ys)[0]
    m_sipp_target['value'] = m_sipp_target['value'] * f
    for v in SIPP_AMOUNTS:
        g[v] = (f, f)
    m_sipp_target.to_parquet(f'{common.MAPS}/map_sipp_{TARGET_YEAR}.parquet', index=False)
    pd.DataFrame([{'variable': v, 'factor, positive values': fp, 'factor, negative values': fn}
                  for v, (fp, fn) in g.items()]).to_csv(f'{common.MAPS}/growth_factors.csv', index=False)


def present_on(t):
    """Per slot, the sources that have it: an amount where its band is set, a category where it is not -1."""
    out = {}
    for v in AMOUNTS:
        out[v] = sorted(s for s in t.source.unique() if (t.loc[t.source == s, f'{v}_band'] >= 0).any())
    for v in ['married', 'n_dep', 'age_range', 'filer'] + list(TAX_CATEGORIES) + list(CATEGORIES) + list(PERSON_CATEGORIES) \
            + list(SIPP_CATEGORIES):
        out[v] = sorted(s for s in t.source.unique() if (t.loc[t.source == s, v] >= 0).any())
    return out


def cex_target_units(pi):
    """The target year's CEX tax units, all of them (a consumer unit's spending divided among its tax units by head
    count, build_cex.R, which folds dependent filers into the unit that claims them), with the primary filer's age
    range by the rule the training rows use. For the tilt's consumption targets and the report:
    data/cex_units_<target year>_all.parquet."""
    y = TARGET_YEAR
    s = pd.read_csv(f'{common.DATA}/cex_units_{y}.csv.gz')
    couple = (s['married'] == 1).to_numpy()
    assert not s.loc[couple, ['age2', 'male2']].isna().any().any() and not s[['age1', 'male1']].isna().any().any()
    age, _ = ages.primary_age(s.age1.to_numpy(float), s.age2.fillna(0).to_numpy(float), s.male1.to_numpy(float),
                              s.male2.fillna(1).to_numpy(float), couple, s['household_id'].astype(float).to_numpy(), pi)
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
    assert not t.loc[rows, 'age1'].isna().any() and (t.loc[unknown_sex, 'source'] == 'scf').all()
    assert not t.loc[couple, 'age2'].isna().any() and (t.loc[couple, 'age2'] >= 0).all()
    ordered = couple & ~unknown_sex      # couples the sex rules order
    mixed = ordered & (t.male1 != t.male2).to_numpy()
    m = t[mixed & sv]
    man = np.where(m.male1 == 1, m.age1, m.age2)
    woman = np.where(m.male1 == 1, m.age2, m.age1)
    target = ages.puf_female_primary_share(puf)
    pi, share = ages.woman_first_chance(man, woman, m.weight, target)
    a1 = t.age1.fillna(0).to_numpy(float)
    a2 = t.age2.fillna(0).to_numpy(float)
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
    # the exact ages on the SCF and CEX rows (the CPS's are set above), where the translation table shares them: the
    # primary filer's age by the same rule as its range, the spouse's on couples only
    spouse = np.where(couple, np.where(primary_is_2, a1, a2), -1.0)
    assert np.array_equal(np.where(primary_is_2, a2, a1)[rows], age[rows])
    for src, sel in [('scf', sv), ('cex', cx), ('sipp', si)]:
        for v, x in [('age_primary', age), ('age_spouse', spouse)]:
            if shares(src, v):
                t.loc[sel, v] = exact_age_value(x[sel])
        d = t[sel]
        if shares(src, 'age_primary'):
            assert (ages.adult_range(d.age_primary - 1) == d.age_range).all(), f'{src}: an exact age outside its range'
            assert ((d.age_spouse > 0) == (d.married == 1)).all(), f'{src}: a spouse age off a couple'
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
    return t.drop(columns=['age1', 'age2', 'male1', 'male2', 'age_key', 'taxunit_code'] + PERSON_COLUMNS +
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
    t, edges = band_table(t, pi)
    cps_ss_taxable_check(t)
    # the report's CPS and SIPP units also carry their wages after function 1 (the CPS's: the tax data's wages at the
    # same count from the top), so the report's wage groups mean the same on both sides
    for src in ('cps', 'sipp'):
        f = f'{common.DATA}/{src}_units_{TARGET_YEAR}_all.parquet'
        u = pd.read_parquet(f)
        tw = t.loc[(t.source == src) & (t.year == TARGET_YEAR), ['record_id', 'wages']]
        u = u.merge(tw.rename(columns={'wages': 'wages_translated'}), on='record_id', how='left', validate='one_to_one')
        assert u['wages_translated'].notna().all()
        u.to_parquet(f, index=False)
    keep = ['source', 'year', 'record_id', 'household_id', 'implicate', 'weight', 'married', 'n_dep', 'filer', 'filer_p'] + \
        list(TAX_CATEGORIES) + list(CATEGORIES) + list(PERSON_CATEGORIES) + list(SIPP_CATEGORIES) + AMOUNTS + \
        ['net_worth', 'w_primary_mortgage'] + [f'{v}_band' for v in AMOUNTS] + \
        ['age_range'] + [f'age_p{k}' for k in range(ages.N_RANGES)] + ['agi_group']
    t['implicate'] = t['implicate'].fillna(0).astype(np.int64)
    for k in range(ages.N_RANGES):
        t[f'age_p{k}'] = t[f'age_p{k}'].fillna(0.0)
    for c in SIPP_CATEGORIES:     # SIPP only
        assert not t.loc[t.source == 'sipp', c].isna().any() and t.loc[t.source != 'sipp', c].isna().all(), c
        t[c] = t[c].fillna(-1)
    for c in PERSON_CATEGORIES:   # absent (-1) on every source that does not report it
        has = [src for src in ('scf', 'cex', 'sipp') if c in EXACT_AGES and shares(src, c)] + ['cps']
        assert not t.loc[t.source.isin(has), c].isna().any(), c
        assert t.loc[~t.source.isin(has), c].isna().all(), c
        t[c] = t[c].fillna(-1)
    assert not t['filer_p'].isna().any() and ((t.filer_p == -1) == ~t.source.isin(['puf', 'cps'])).all()
    assert (t.loc[t.source == 'puf', 'filer_p'] == 1).all() and t.loc[t.source == 'cps', 'filer_p'].between(0, 1).all()
    for c in ['year', 'married', 'n_dep', 'filer', 'age_range'] + list(TAX_CATEGORIES) + list(CATEGORIES) + \
            list(PERSON_CATEGORIES) + list(SIPP_CATEGORIES):
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


if __name__ == '__main__':
    main()
