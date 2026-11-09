"""The training table: the PUF's filers and the survey tax units (SCF, CEX, CPS, SIPP), stacked, every amount cut
into bands.

Reads the PUF and each survey's units from the data folder. The amounts a survey shares with the PUF are translated
through the translation table (translate.py) against each year's tax data. Writes the table, the band edges with
their meta, the dollar maps (scale/maps.py), and the tax data, translation report and survey units that later stages
read. FEATURES.md, "The training table" and "Translation", has the method.
"""
import copy
import json
import os

import numpy as np
import pandas as pd

from shared import ages
from shared import common
from prep import earn_split as ES
from shared import growth as GR
from prep import soi as TG
from shared import translate as TR
from shared.common import PUF_YEAR, TARGET_YEAR

# ---- constants -------------------------------------------------------------------
WAVE_2013_ID_OFFSET = 2.013e12
# band layout: 0 zero; 1-10 losses (shares of loss weight from the least negative); 11-83 positives
N_NEG, NEG0, POS0 = 10, 1, 11
NEG_SHARES = np.array([0.225, 0.45, 0.675, 0.90, 0.93, 0.96, 0.99, 0.995, 0.999])
POS_SHARES = np.round(np.concatenate([0.02 * np.arange(1, 46), 0.90 + 0.01 * np.arange(1, 10),
                                      0.99 + 0.001 * np.arange(1, 10), 0.999 + 0.0001 * np.arange(1, 10)]), 6)
N_BANDS = POS0 + len(POS_SHARES) + 1
assert N_BANDS == 84

KG_PIECES = ['kg_st', 'kg_lt', 'kg_dist']   # capital gain pieces, before the $3,000 loss limit
ADJ_FIELDS = ['E03150', 'E03210', 'E03220', 'E03230', 'E03240', 'E03260', 'E03270', 'E03290',
              'E03300', 'E03400', 'E03500']
ITEMS = {  # item: (sign in AGI, 0 if not in AGI; on the SCF; SCF rule). The order fixes the model's slot order.
    'business_income': (1, False, None), 'sch_cf_income': (0, True, 'rank'), 'sch_e_income': (0, False, None),
    # Schedule E in the two parts the SCF reports: rents and royalties, and the rest (partnerships, S corporations,
    # estates and trusts, farm rental)
    'sche_rent': (0, True, 'percentile'), 'sche_rest': (0, True, 'percentile'),
    'other_gains': (1, False, None), 'txbl_ira_dist': (1, False, None),
    # pensions in two parts: taxable, and the non-taxable rest of total pensions (E01500 - E01700, at least zero).
    # Gross pensions and taxable Social Security are computed later (dollars.retirement_items).
    'txbl_pens_dist': (1, False, None), 'nontxbl_pens_dist': (0, False, None), 'ui': (1, False, None),
    'alimony': (1, False, None), 'state_ref': (1, False, None), 'adjustments': (-1, False, None),
    'exempt_int': (0, True, 'unchanged'),
    'first_mort_int': (0, False, None), 'salt_prop': (0, False, None),
    'gross_ss': (0, True, 'unchanged')}   # gross Social Security, not in AGI
PARTS = {  # part: (PUF field, sign in business income); loss fields are stored positive
    'sole_prop': ('E00900', 1), 'part_passive': ('E25940', 1), 'part_active': ('E25980', 1),
    'part_passive_loss': ('E25920', -1), 'part_active_loss': ('E25960', -1), 'part_179': ('E26110', -1),
    'scorp_passive': ('E26170', 1), 'scorp_active': ('E26190', 1), 'scorp_passive_loss': ('E26160', -1),
    'scorp_active_loss': ('E26180', -1), 'scorp_179': ('E26100', -1), 'rent': ('E25850', 1),
    'rent_loss': ('E25860', -1), 'estate': ('E26390', 1), 'estate_loss': ('E26400', -1),
    'farm_rent': ('E27200', 1), 'sche_other': (None, 1), 'farm': ('E02100', 1)}
# The other PUF items the production pipeline carries, with its names (config/variable_guide/baseline.csv). PUF only.
# Ordinary dividends and the adjustments are computed as the sums of their parts (COMPUTED).
DIV_PARTS = {'div_pref': 'E00650', 'div_ord': None}     # qualified dividends; ordinary less qualified (E00600 - E00650)
ADJ_PARTS = dict(zip(['trad_contr_ira', 'sl_int_ded', 'ed_exp', 'tuition_ded', 'dpad', 'se_tax_ded', 'se_health',
                      'hsa_contr', 'keogh_contr', 'early_penalty', 'alimony_exp'], ADJ_FIELDS))
NEW_ITEMS = {  # item: PUF field (None: derived in puf_rows)
    'kg_1250': 'E24515', 'kg_collect': 'E24518', 'part_se': None,
    'med_exp': 'E17500', 'salt_inc_sales': 'E18400', 'char_cash': 'E19800', 'char_noncash': 'E20100',
    'casualty_exp': 'E20500', 'job_exp': 'E20550', 'tax_prep_exp': 'E20600', 'other_misc_exp': 'E20400',
    'other_item_exp': None,
    'amt_nols': 'P60100', 'amt_ftc': 'E62900',
    'ftc': 'E07300', 'aoc_exp': 'P87482', 'llc_exp': 'E87530', 'old_cred': 'E07200', 'savers_cred': 'E07240',
    'res_energy_cred': 'E07260', 'gbc': 'E07400', 'prior_yr_cred': 'E07600', 'other_nonref': 'P08000',
    'net_ptc': 'E11560', 'excess_ptc': 'E85785', 'recapture_tax': 'E09700', 'ira_penalty': 'E09900'}
STAGE3 = list(DIV_PARTS) + list(ADJ_PARTS) + list(NEW_ITEMS)   # in slot order
SCHE_PARTS = [p for p, (f, _) in PARTS.items() if f and p not in ('sole_prop', 'farm')]
CF_PARTS = ['sole_prop', 'farm']                                  # Schedules C and F: the SCF's X5704
E_PARTS = [p for p in PARTS if p not in CF_PARTS]             # Schedule E
RENT_PARTS = ['rent', 'rent_loss']                        # rents and royalties: the SCF's rental income
REST_PARTS = [p for p in E_PARTS if p not in RENT_PARTS]      # the rest of Schedule E: X5714 less rental
K1_PARTS = [p for p in PARTS if p.startswith(('part_', 'scorp_'))]
# The balance sheet (production's wealth_schema.R). Net worth is computed from the 20 components. The home mortgage
# is carried as its share of the home's value (w_primary_ltv).
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin', 'pass_throughs',
          'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
KG_WEALTH = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
NET_WORTH_PARTS = {**{'value.' + c: 1 for c in ASSETS}, **{'value.' + c: -1 for c in DEBTS}}
# SCF slots outside net worth (build_scf.R): auto_int_exp, a year's interest on vehicle loans; elective_deferrals to
# 401(k)-type plans and spouse_deferral_share, the spouse's share of them; w_dc_stock_share, the share of retirement
# accounts held in stocks (a share of value.dc); inheritance, inheritances and trusts received this year; and for
# each business group (cf: Schedules C and F; ps: partnerships and S corporations) the industry of the household's
# largest business (0 none, 1-7 the SCF's public groups, 8 no code) and the owner's share of its workers.
# Each business group's PUF parts:
QBI_GROUPS = {'cf': ['sole_prop', 'farm'],
              'ps': ['part_passive', 'part_active', 'part_passive_loss', 'part_active_loss', 'part_179',
                     'scorp_passive', 'scorp_active', 'scorp_passive_loss', 'scorp_active_loss', 'scorp_179']}
SCF_WORKERS = [f'biz_workers_{k}' for k in QBI_GROUPS]
SCF_AMOUNTS = ['value.' + c for c in ASSETS] + ['w_primary_ltv'] + ['value.' + c for c in DEBTS if c != 'primary_mortgage'] \
    + ['w_' + c for c in KG_WEALTH] + ['auto_int_exp', 'elective_deferrals', 'spouse_deferral_share', 'w_dc_stock_share'] \
    + SCF_WORKERS + ['inheritance']
# missing on the units outside the primary economic unit, whom the SCF does not ask
SCF_NOT_ASKED = ['elective_deferrals', 'spouse_deferral_share', 'w_dc_stock_share', 'inheritance']
# SCF shares and counts: not grown between waves
SCF_RATIOS = ['w_primary_ltv', 'spouse_deferral_share', 'w_dc_stock_share'] + SCF_WORKERS
SCF_CATEGORIES = {f'biz_industry_{k}': 9 for k in QBI_GROUPS}
PUF_AMOUNTS = ['wages', 'txbl_int', 'dividends', 'capital_gains'] + KG_PIECES + list(ITEMS) + list(PARTS) + STAGE3 + \
    ['other_agi']
CEX_CATS = ['c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables', 'c_food_off_premises',
            'c_gasoline', 'c_housing_utilities', 'c_other_services_health']
CEX_AMOUNTS = CEX_CATS
# CPS-only amounts, cut into bands within each year: the spouse's share of the couple's wages, each adult's weeks
# worked, usual weekly hours and overtime pay (overtime_cps.R), and childcare. Overtime is missing on the units not
# asked the earnings questions, about three quarters of them.
CPS_OVERTIME = ['ot_primary', 'ot_spouse']
CPS_AMOUNTS = ['spouse_wage_share', 'weeks_primary', 'weeks_spouse', 'hours_primary',
               'hours_spouse', 'care_exp'] + CPS_OVERTIME
# The spouse's share of the couple's Schedule SE earnings, a PUF slot shared with the CPS (earn_split.py), cut on
# fixed cut points so that a band means the same share on both.
PUF_SHARES = ['spouse_se_share']
SHARE_CUTS = np.arange(1, 73) / 73.0            # 72 cuts, 73 positive bands; a share of exactly 1 is in the top band
# A share is present only on a married couple with some of its base. A base a source lacks counts as zero.
SHARE_GATES = {'spouse_wage_share': ['wages'], 'spouse_se_share': ['sch_cf_income', 'part_se']}
# earn_split: the PUF's EARNSPLIT, the primary's share of the couple's earnings in three ranges (0 none), on the PUF
# and the CPS. PUF joint returns with no demographics carry probabilities instead (es_p0-3).
SHARED_CATEGORIES = {'earn_split': ES.N_CODES}
ES_PROBS = [f'es_p{k}' for k in range(ES.N_CODES)]
MAX_AGE = 85                                   # the CPS's top code (80 is 80-84, 85 is 85 and over)
AGE_SLOTS = ['age1', 'age2', 'dep_age1', 'dep_age2', 'dep_age3']   # value: 0 none, else the age + 1
# Exact ages of the primary filer and spouse, shared by the SCF, CEX and CPS: to 79, then 80 for 80 and over, the
# finest coding all three allow (the CPS top-codes at 80 and 85, the CEX from 83 or 84).
EXACT_AGES = TR.EXACT_AGES
EXACT_AGE_TOP = 80
# Social Security number status, CPS only: 0 none, 1 citizen, 2 noncitizen with a sign of lawful status, 3
# noncitizen without one (taken to lack a number)
SSN_STATUS_SLOTS = ['ssn_status_primary', 'ssn_status_spouse', 'ssn_status_dep1', 'ssn_status_dep2', 'ssn_status_dep3']
DEPENDENT_SSN_SLOTS = ['ssn_status_dep1', 'ssn_status_dep2', 'ssn_status_dep3']   # the k-th: present when n_dep >= k
# The household's state, CPS only: 0 none, else 1 + its position in STATE_FIPS (the 50 states and DC; the CPS has
# neither Puerto Rico nor the IRS's other areas)
STATE_FIPS = [1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
              34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56]
CPS_CATEGORIES = {**{a: MAX_AGE + 2 for a in AGE_SLOTS if a not in EXACT_AGES},
                  'sex_primary': 3, 'sex_spouse': 3,   # CPS only; sex: 0 none, 1 man, 2 woman
                  **{v: 4 for v in SSN_STATUS_SLOTS},
                  'state': len(STATE_FIPS) + 1}
PERSON_CATEGORIES = {**{a: EXACT_AGE_TOP + 2 for a in EXACT_AGES}, **CPS_CATEGORIES}   # every person category
SPOUSE_SLOTS = ['age2', 'sex_spouse', 'spouse_wage_share', 'spouse_se_share', 'weeks_spouse', 'hours_spouse',
                'ot_spouse', 'ssn_status_spouse']
DEPENDENT_AGE_SLOTS = ['dep_age1', 'dep_age2', 'dep_age3']   # the k-th: present exactly when n_dep >= k
SHARES = ['spouse_wage_share', 'spouse_se_share']   # only for married couples with some of the item; else absent
# SIPP only: each adult's tips, cut within each year, and whether they come mostly from leisure and hospitality (0
# without tips)
SIPP_AMOUNTS = ['tips_primary', 'tips_spouse']
SIPP_CATEGORIES = {'tips_lh_primary': 2, 'tips_lh_spouse': 2}
SIPP_SPOUSE_SLOTS = ['tips_spouse', 'tips_lh_spouse']
SIPP_LH_OF = {'tips_lh_primary': 'tips_primary', 'tips_lh_spouse': 'tips_spouse'}
AMOUNTS = PUF_AMOUNTS + SCF_AMOUNTS + CEX_AMOUNTS + CPS_AMOUNTS + SIPP_AMOUNTS + PUF_SHARES
CX = common.SETTINGS['cex']
CP = common.SETTINGS['cps']
SP = common.SETTINGS['sipp']
CEX_YEAR_ID_OFFSET = 1e10                      # added to build_cex.R's ids (year x 1e10 + interview x 100 + unit)
CPS_ID_OFFSET = 3e12                           # CPS ids: 3e12 + (year - 2000) x 1e9 + build_cps.R's id
SIPP_ID_OFFSET = 4e12                          # SIPP ids: 4e12 + (year - 2000) x 1e10 + roster_sipp.py's id
CATEGORIES = {'k1_owner': 'shared', 'owns_business': 'scf'}   # yes/no items beyond married, n_dep, filer
# Categories on the PUF and the CPS, -1 elsewhere: {slot: number of values}. Filing status is 1-4 (0 unused).
TAX_CATEGORIES = {'filing_status': 5, 'n_dep_ctc': 4, 'n_dep_eitc': 4}
PUF_CATEGORIES = {}                            # PUF-only categories: none (model.py and the edges' meta read the key)
# the source-only categories, with their source
OWN_CATEGORIES = {**{c: 'sipp' for c in SIPP_CATEGORIES}, **{c: 'scf' for c in SCF_CATEGORIES}}


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
    ['E00650', 'E09400', 'E30400', 'E30500', 'FDED', 'P04470', 'E21040', 'E19700', 'E20800'] + [f for f in NEW_ITEMS.values() if f]


def read_puf():
    """The PUF, read once for the table and for ages.py, without its four aggregate records."""
    fields = list(dict.fromkeys(PUF_FIELDS + ages.PUF_FIELDS + ES.PUF_FIELDS))
    raw = pd.read_csv(common.RAW['puf'], usecols=fields)
    return raw[raw.RECID < 999996].reset_index(drop=True)


def puf_items(f, itemizer, medical_floor):
    """The PUF amounts the table draws, from the raw fields f (a dict of arrays of whole dollars): wages and interest, the
    dividend parts, the capital gain pieces, the parts of business income, the items, the adjustments' parts, the other
    items, Schedule SE earnings outside Schedules C and F, other itemized deductions and the remainder of AGI. The
    totals (COMPUTED) are the signed sums of these. itemizer says who has other itemized deductions, medical_floor the
    share of AGI (one number or one per record) below which medical expenses are not deducted."""
    out = {'wages': f['E00200'], 'txbl_int': f['E00300'], 'div_pref': f['E00650'], 'div_ord': f['E00600'] - f['E00650']}
    # Disclosure protection perturbs each PUF field on its own, so line 13 (E01000) can fall below the allowed
    # Schedule D net (P22250 + P23250, a loss held to the limit). The shortfall is put on the short-term piece, so
    # line 13 is the allowed net again; a gap the other way stays in other_agi.
    left = f['E01000'] - common.allowed_capital_gain(f['P22250'] + f['P23250'], f['MARS'])
    out.update(kg_st=f['P22250'] + np.minimum(left, 0), kg_lt=f['P23250'], kg_dist=f['E01100'])
    out.update({p: f[fld] for p, (fld, _) in PARTS.items() if fld})
    out['sche_other'] = f['E02000'] - sum(PARTS[p][1] * out[p] for p in SCHE_PARTS)
    out.update(other_gains=f['E01200'], txbl_ira_dist=f['E01400'], txbl_pens_dist=f['E01700'],
               nontxbl_pens_dist=np.maximum(f['E01500'] - f['E01700'], 0), ui=f['E02300'], alimony=f['E00800'],
               state_ref=f['E00700'], gross_ss=f['E02400'], exempt_int=f['E00400'], first_mort_int=f['E19200'],
               salt_prop=f['E18500'])
    out.update({p: f[fld] for p, fld in ADJ_PARTS.items()})
    out.update({v: f[fld] for v, fld in NEW_ITEMS.items() if fld})
    # part_se: Schedule SE earnings other than Schedules C and F, read from the reported tax (earn_split.py). Not
    # whole dollars, because of the division by 0.9235.
    k1_part = (f['E25940'] + f['E25980'] - f['E25920'] - f['E25960']) != 0
    out['part_se'] = ES.part_se_from_schedule_se(f['E09400'], f['E30400'], f['E00900'] + f['E02100'], k1_part)
    # other_item_exp, as the production pipeline derives it: on itemizers, total itemized deductions before the
    # overall limitation (P04470 + E21040) less deductible medical expenses, the two taxes, interest paid, charity,
    # casualty losses and net limited miscellaneous deductions. The PUF rounds each item on its own, so the remainder
    # is floored at zero.
    medical = np.maximum(0, f['E17500'] - np.maximum(0, f['E00100'] * medical_floor))
    other_item = np.where(itemizer, f['P04470'] + f['E21040'] - medical - f['E18400'] - f['E18500']
                          - f['E19200'] - f['E19700'] - f['E20500'] - f['E20800'], 0.0)
    out['other_item_exp'] = np.maximum(other_item, 0.0)
    # the remainder of AGI: AGI less wages, interest, dividends, line 13 (the allowed Schedule D net plus distributions)
    # and the items in AGI. Taxable Social Security (E02500) comes off too: it is not a slot, because the file computes
    # it by the worksheet
    totals = {'business_income': f['E00900'] + f['E02000'] + f['E02100'], 'adjustments': sum(f[c] for c in ADJ_FIELDS)}
    items = {v: totals.get(v, out.get(v)) for v in ITEMS if ITEMS[v][0] != 0}   # in ITEMS' order (the aggregate records' means are not whole dollars, so the order matters)
    allowed = common.allowed_capital_gain(out['kg_st'] + out['kg_lt'], f['MARS'])
    out['other_agi'] = f['E00100'] - out['wages'] - out['txbl_int'] - f['E00600'] - allowed - f['E01100'] - \
        sum(ITEMS[v][0] * items[v] for v in items) - f['E02500']
    return out


def puf_rows(puf):
    raw = puf[PUF_FIELDS]
    assert not raw.isna().any().any() and not raw.RECID.duplicated().any()
    I = {c: raw[c].round().astype(np.int64).to_numpy() for c in PUF_FIELDS if c not in ('RECID', 'S006')}
    for c in I:
        assert np.array_equal(I[c], raw[c].to_numpy()), f'{c}: not whole dollars'
    n_dep = I['XOCAH'] + I['XOCAWH'] + I['XOPAR'] + I['XOODEP']
    assert np.isin(I['MARS'], [1, 2, 3, 4]).all(), 'MARS outside 1-4 (surviving spouses are coded joint on this PUF)'
    assert (I['N24'] >= 0).all() and (I['EIC'] >= 0).all() and (I['EIC'] <= 3).all()
    out = {'source': 'puf', 'year': PUF_YEAR, 'record_id': raw.RECID.astype(float), 'household_id': raw.RECID.astype(float),
           'weight': raw.S006 / 100, 'married': (I['MARS'] == 2).astype(np.int64), 'filing_status': I['MARS'],
           'n_dep_ctc': np.minimum(I['N24'], 3).astype(np.int64), 'n_dep_eitc': np.minimum(I['EIC'], 3).astype(np.int64),
           'dep_filer': (puf['DSI'] == 1).astype(np.int64).to_numpy(),
           'n_dep': np.minimum(n_dep, 3).astype(np.int64), 'filer': 1, 'filer_p': 1.0, 'owns_business': -1}
    # The medical floor is 7.5% of AGI at 65 and over, else 10%; a record without an age range counts as 65+ when its
    # age-range probabilities give that more than half.
    rng, probs = ages.puf_age_ranges(puf)
    assert np.array_equal(rng.RECID.to_numpy(), raw.RECID.to_numpy())
    old = (rng.age_range.to_numpy() == 5) | (probs[:, 5] > 0.5)
    f = {c: x.astype(float) for c, x in I.items()}
    out.update(puf_items(f, I['FDED'] == 1, np.where(old, 0.075, 0.1)))
    for v, parts in COMPUTED.items():
        out[v] = sum(sg * out[p] for p, sg in parts.items())
    out['k1_owner'] = np.column_stack([out[p] != 0 for p in K1_PARTS]).any(1).astype(np.int64)
    # the parts add up to the PUF's own totals exactly (whole dollars)
    left = f['E01000'] - common.allowed_capital_gain(f['P22250'] + f['P23250'], f['MARS'])
    assert ((common.allowed_capital_gain(out['kg_st'] + out['kg_lt'], f['MARS']) == f['E01000']) | (left > 0)).all(), 'line 13 not restored'
    assert np.array_equal(out['business_income'], f['E00900'] + f['E02000'] + f['E02100'])
    assert np.array_equal(out['sch_cf_income'], f['E00900'] + f['E02100']) and np.array_equal(out['sch_e_income'], f['E02000'])
    assert np.array_equal(out['sche_rent'] + out['sche_rest'], out['sch_e_income'])
    assert (out['div_pref'] >= 0).all() and (out['div_ord'] >= 0).all(), 'qualified dividends above ordinary dividends'
    assert np.array_equal(out['dividends'], f['E00600']) and np.array_equal(out['adjustments'], sum(f[c] for c in ADJ_FIELDS))
    # gross pensions = taxable + the rest, exactly; the rest below zero (taxable above total pensions) is held at zero
    over = I['E01700'] > I['E01500']
    common.say(f'PUF: taxable pensions above total pensions (the rest held at zero) on {int(over.sum()):,} records, '
               f'{(raw.S006.to_numpy() / 100)[over].sum() / 1e6:,.3f} million returns')
    assert np.array_equal(out['txbl_pens_dist'] + out['nontxbl_pens_dist'], np.maximum(f['E01500'], f['E01700']))
    df = pd.DataFrame(out)
    df['age_range'] = rng.age_range.to_numpy()
    for k in range(ages.N_RANGES):
        df[f'age_p{k}'] = probs[:, k]
    # the spouse's share of the couple's Schedule SE earnings, E30500 / E30400, on a joint return with some of the
    # share's base; missing where such a return has E30400 zero (a loss, under the $400 floor, or wages at the cap)
    joint = I['MARS'] == 2
    se_all, se_spouse = I['E30400'].astype(float), I['E30500'].astype(float)
    assert (se_spouse <= se_all).all() and (se_spouse[~joint] == 0).all() and (se_spouse >= 0).all(), \
        'E30500 (the spouse\'s Schedule SE earnings) above E30400 (the couple\'s), or on a return that is not joint'
    base = np.column_stack([df[b].to_numpy(float) != 0 for b in SHARE_GATES['spouse_se_share']]).any(1)
    asked = joint & base
    df['spouse_se_share'] = np.where(asked & (se_all > 0), se_spouse / np.where(se_all > 0, se_all, 1.0), np.nan)
    wt = raw.S006.to_numpy() / 100
    known = asked & (se_all > 0)
    common.say(f'PUF spouse_se_share: asked of {wt[asked].sum() / 1e6:.2f} million joint returns ({int(asked.sum()):,} '
               f'records); known (Schedule SE earnings) on {wt[known].sum() / 1e6:.2f} million, missing on '
               f'{wt[asked & ~known].sum() / 1e6:.2f} million (a loss or under the floor); share 0 '
               f'{100 * wt[known & (se_spouse == 0)].sum() / wt[known].sum():.1f}%, 1 '
               f'{100 * wt[known & (se_spouse == se_all)].sum() / wt[known].sum():.1f}% (weighted); Schedule SE earnings '
               f'on joint returns with no base, not split: {wt[joint & ~base & (se_all > 0)].sum() / 1e6:.2f} million returns')
    code, eprobs = ES.puf_codes(puf)
    df['earn_split'] = code
    for k in range(ES.N_CODES):
        df[f'es_p{k}'] = eprobs[:, k]
    return df


COMPUTED = {'capital_gains': {p: 1 for p in KG_PIECES},
            'dividends': {p: 1 for p in DIV_PARTS}, 'adjustments': {p: 1 for p in ADJ_PARTS},
            'business_income': {p: s for p, (_, s) in PARTS.items()},
            'sch_cf_income': {p: PARTS[p][1] for p in CF_PARTS},
            'sch_e_income': {p: PARTS[p][1] for p in E_PARTS},
            'sche_rent': {p: PARTS[p][1] for p in RENT_PARTS}, 'sche_rest': {p: PARTS[p][1] for p in REST_PARTS}}


growth = GR.factors   # (factor for positive values, for negative values) of a PUF amount, PUF year to y, by the target year's growth.yaml


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
    """The PUF amounts a survey's translation rows give on its records d (one survey year), in PUF terms before
    function 1, with computed totals and k1_owner from their parts. An item the survey does not ask that year is zero
    here (missing in training), and rows kept out of training still count, so the tax data does not depend on them."""
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
    """The tax data for year y: the PUF filers other than dependent filers, reweighted to SOI Table 1.6's returns of
    year y by filing status and age range, dollars grown per return; plus the CPS units of year y (or of the nearest
    CPS year, scaled to SOI's returns and grown) weighted by their chance of not filing, with amounts in PUF terms.
    Columns: weight, the categories, age_p0-5, and the amounts the surveys of year y share."""
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
    rows = [r for src in surveys for r in TR.rows(src, amounts_only=True)]
    # and the totals a row is ranked against, which can be a sum (the CPS's pensions against taxable pensions plus
    # taxable IRA distributions)
    sums = {TR.against_name(r): TR.against(r) for r in rows if len(TR.against(r)) > 1}
    shared = sorted({r['variable'] for r in rows} | {a for r in rows for a in TR.against(r)})
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
    # says so (interest); any other gap stops
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


def unit_frame(src, s, record_id, household_id, male1, male2, **columns):
    """The table's columns of a survey's units s: the common ones (a category the survey lacks is -1, the person
    items as the survey's person 1 and 2 until the primary rule), the survey's raw columns, and columns, the
    survey's own or its values of the common ones."""
    return pd.DataFrame({'source': src, 'year': s['year'], 'record_id': record_id, 'household_id': household_id,
                         'weight': s['weight'].astype(float), 'married': s['married'].astype(np.int64),
                         'n_dep': s['n_dep'].clip(upper=3).astype(np.int64), 'filer': -1, 'filer_p': -1.0,
                         **{c: -1 for c in TAX_CATEGORIES}, 'k1_owner': -1, 'owns_business': -1,
                         **survey_items(s, src),
                         'svy_age1': s['age1'].astype(float), 'svy_age2': s['age2'].astype(float),
                         'male1': male1.astype(float), 'male2': male2.astype(float), 'age_key': household_id, **columns})


def read_units(src, years):
    """The survey's units of every year, stacked, after the checks every survey's file passes."""
    rows = []
    for y in years:
        s = pd.read_csv(f'{common.DATA}/{src}_units_{y}.csv.gz')
        assert (s['year'] == y).all() and not s['record_id'].duplicated().any()
        rows.append(s)
    return pd.concat(rows, ignore_index=True)


def cex_rows():
    """The CEX tax units (build_cex.R), every year in settings "cex"."""
    s = read_units('cex', CX['years'])
    assert (s['weight'] > 0).all()
    for y in CX['years']:
        d = s[s['year'] == y]
        common.say(f'CEX {y}: {len(d):,} rows, {d.weight.sum() / 1e6:.1f} million units')
    need = sorted({c for r in TR.rows('cex', amounts_only=True) for c in r['items']}) + CEX_CATS
    n_missing = s[need].isna().sum()
    assert not n_missing.any(), f'missing values in the CEX units:\n{n_missing[n_missing > 0]}'
    assert (s['dep_filer'] == 0).all(), 'build_cex.R folds dependent filers into the unit that claims them'
    # record ids unique across years; the household id is not offset, so a consumer unit interviewed in two
    # survey years falls on one side of the held-back split
    off = (s['year'] - 2000) * CEX_YEAR_ID_OFFSET
    return unit_frame('cex', s, s['record_id'] + off, s['household_id'].astype(float), s['male1'], s['male2'],
                      **{c: s[c].astype(float) for c in CEX_CATS})


def shares(survey, variable):
    """Whether the translation table has a row for the survey's variable."""
    return any(r['variable'] == variable for r in TR.rows(survey))


def cps_rows():
    """The CPS tax units (build_cps.R), every year in settings "cps", filers and non-filers. filer_p is the unit's
    calibrated filing chance. The person items stay as person 1 and person 2 (p1_..., p2_...) until cps_person_slots
    puts them in primary-filer order."""
    s = read_units('cps', CP['years'])
    assert (s['weight'] > 0).all()
    for y in CP['years']:
        d = s[s['year'] == y]
        common.say(f'CPS {y}: {len(d):,} rows, {d.weight.sum() / 1e6:.1f} million units')
    person = ['wages1', 'wages2', 'se1', 'se2', 'weeks_worked1', 'weeks_worked2', 'usual_hours1', 'usual_hours2']
    need = sorted({c for r in TR.rows('cps', amounts_only=True) for c in r['items']}) + person + \
        ['married', 'n_dep', 'filer', 'p_file', 'filing_status', 'n_dep_ctc', 'n_dep_eitc', 'age1', 'age2', 'sex1', 'sex2', 'dep_age1', 'dep_age2', 'dep_age3', 'care_exp',
         'wages', 'sch_cf_income', 'ssn_status1', 'ssn_status2', 'dep_ssn_status1', 'dep_ssn_status2', 'dep_ssn_status3']
    # an item the CPS does not ask in some years (a row's first_year: capital gains before tax year 2018) is missing
    # exactly in those years (build_cps.R)
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
    # overtime: missing exactly on the units not asked the earnings questions, both adults together
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
    return unit_frame('cps', s, rid, hh, s['sex1'] == 1, s['sex2'] == 1,
                      filer=s['filer'].astype(np.int64) if shares('cps', 'filer') else -1,
                      filer_p=s['p_file'].astype(float),
                      **{c: s[c].astype(np.int64) if shares('cps', c) else -1 for c in TAX_CATEGORIES},
                      **{f'p{k}_{c}': s[f'{c}{k}'].astype(float) for k in (1, 2)
                         for c in ['age', 'sex', 'wages', 'se', 'weeks_worked', 'usual_hours']},
                      **{f'p{k}_ot': s[f'ot{k}'].astype(float) for k in (1, 2)},
                      **{f'p{k}_ssn_status': s[f'ssn_status{k}'].astype(np.int64) for k in (1, 2)},
                      **{f'svy_dep_age{k}': s[f'dep_age{k}'].astype(np.int64) for k in (1, 2, 3)},
                      **{f'dep_ssn_status{k}': s[f'dep_ssn_status{k}'].astype(np.int64) for k in (1, 2, 3)},
                      cps_childcare=s['care_exp'].astype(float), svy_statefip=s['statefip'].astype(np.int64))


PERSON_COLUMNS = [f'p{k}_{c}' for k in (1, 2) for c in ['age', 'sex', 'wages', 'se', 'weeks_worked', 'usual_hours']] + \
    ['p1_ot', 'p2_ot', 'svy_dep_age1', 'svy_dep_age2', 'svy_dep_age3', 'cps_childcare', 'p1_ssn_status', 'p2_ssn_status',
     'dep_ssn_status1', 'dep_ssn_status2', 'dep_ssn_status3', 'svy_statefip']


def cps_person_slots(t, primary_is_2):
    """The CPS-only slots, primary filer first (primary_is_2 marks the rows where person 2 is the primary filer).
    Ages are 0 for none, else the age plus 1, so that an infant is not none. Also writes the CPS units of each year in
    the target year's survey mix, for report_cps.py."""
    c = (t.source == 'cps').to_numpy()
    sw = primary_is_2[c]
    d = t[c]
    pick = lambda col, k: np.where(sw, d[f'p{3 - k}_{col}'], d[f'p{k}_{col}'])   # k = 1 primary, 2 spouse
    age = {k: pick('age', k) for k in (1, 2)}
    w = d['weight'].to_numpy(float)
    married_ = (d['married'] == 1).to_numpy()
    statefip = d['svy_statefip'].to_numpy()
    assert np.isin(statefip, STATE_FIPS).all(), f'CPS: a state code outside the 50 states and DC: {sorted(set(statefip) - set(STATE_FIPS))}'
    out = {'age1': exact_age_value(age[1]), 'age2': exact_age_value(age[2]),
           'state': 1 + np.searchsorted(STATE_FIPS, statefip),
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
        # recorded only for married couples with some of the item; generation sets it to zero elsewhere
        out[share] = np.where(married_ & (total != 0), np.clip(x, 0.0, 1.0), np.nan)
        out[f'{item}_primary_reported'], out[f'{item}_spouse_reported'] = pick(item, 1), pick(item, 2)
    # the self-employment share on the PUF's definition: the spouse's share of the couple's Schedule SE earnings
    # where the couple has any, else the share of self-employment income above. The earnings split code from the
    # couple's wages plus Schedule SE earnings, 0 unless married.
    e = {k: ES.se_earnings(pick('se', k)) for k in (1, 2)}
    has_e = married_ & (e[1] + e[2] > 0)
    out['spouse_se_share'] = np.where(has_e, e[2] / np.where(has_e, e[1] + e[2], 1.0), out['spouse_se_share'])
    wg = {k: np.maximum(pick('wages', k), 0.0) for k in (1, 2)}
    out['earn_split'] = np.where(married_, ES.code_of(wg[1] + e[1], wg[1] + wg[2] + e[1] + e[2]), 0)
    sel = married_ & np.isfinite(out['spouse_se_share'])
    common.say(f'CPS spouse_se_share on the Schedule SE definition: couples with Schedule SE earnings '
               f'{w[has_e].sum() / 1e6:.2f} million of {w[sel].sum() / 1e6:.2f} million asked (weighted, all CPS years); '
               f'earn_split among married units (%): ' + ', '.join(
                   f'{k} {100 * w[married_ & (out["earn_split"] == k)].sum() / w[married_].sum():.1f}' for k in range(4)))
    t = put_rows(t, c, out)
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
    # overtime: at most the person's own wages as the CPS reports them; missing on both adults together
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
        list(PERSON_CATEGORIES) + CPS_AMOUNTS + PUF_SHARES + list(SHARED_CATEGORIES) + ['wages_primary_reported', 'wages_spouse_reported', 'se_primary_reported',
                                              'se_spouse_reported'] + [c for c in d if c.startswith('raw_')]
    for y in mix_years('cps'):
        d[d.year == y][keep].to_parquet(f'{common.DATA}/cps_units_{y}_all.parquet', index=False)
    return t.drop(columns=[c for c in t if c.endswith(('_primary_reported', '_spouse_reported'))])


SIPP_PERSON = ['wages', 'tips', 'tips_lh']
SIPP_PERSON_COLUMNS = [f'q{k}_{c}' for k in (1, 2) for c in SIPP_PERSON]


def sipp_rows():
    """The SIPP's tax units (roster_sipp.py), every year in settings "sipp". The person items stay as person 1 and
    person 2 (q1_..., q2_...) until sipp_person_slots puts them in primary-filer order."""
    s = read_units('sipp', SP['years'])
    for y in SP['years']:
        d = s[s['year'] == y]
        zero = int((d['weight'] <= 0).sum())
        common.say(f'SIPP {y}: {len(d):,} rows, {d.weight.sum() / 1e6:.1f} million units; rows with weight 0 left out '
                   f'(the SIPP gives them no weight in the reference year\'s last month): {zero}')
    s = s[s['weight'] > 0].reset_index(drop=True)
    need = sorted({c for r in TR.rows('sipp', amounts_only=True) for c in r['items']}) + \
        ['married', 'n_dep', 'age1', 'age2', 'sex1', 'sex2', 'wages1', 'wages2', 'tips1', 'tips2', 'tips_lh1', 'tips_lh2']
    n_missing = s[need].isna().sum()
    assert not n_missing.any(), f'missing values in the SIPP units:\n{n_missing[n_missing > 0]}'
    # elective deferrals: asked in some years only, so missing in exactly the years not asked
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
    return unit_frame('sipp', s, rid, hh, s['sex1'] == 1, s['sex2'] == 1,
                      **{f'q{k}_{c}': s[f'{c}{k}'].astype(float) for k in (1, 2) for c in SIPP_PERSON})


def sipp_person_slots(t, primary_is_2):
    """The SIPP's own slots and the spouse's share of wages (shared with the CPS), primary filer first. Also writes
    the SIPP units of each year in the target year's survey mix, for report_sipp.py."""
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
    t = put_rows(t, c, out)
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


def put_rows(t, c, out):
    """The columns of out (arrays on the rows c of t) written into t; a new column is missing on the other rows."""
    t = pd.concat([t, pd.DataFrame({v: np.nan for v in out if v not in t}, index=t.index)], axis=1)
    for v, x in out.items():
        t.loc[c, v] = x
    return t


def scf_person_slots(t, primary_is_2):
    """The SCF's spouse_deferral_share: the spouse's share of the unit's elective deferrals, primary filer first. 0
    on a unit that is not married or has no deferrals; missing where the SCF does not ask."""
    c = (t.source == 'scf').to_numpy()
    d = t[c]
    e, e1, e2 = (d[k].to_numpy(float) for k in ('elective_deferrals', 'elective1', 'elective2'))
    spouse = np.where(primary_is_2[c], e1, e2)
    married = (d['married'] == 1).to_numpy()
    asked = ~np.isnan(e)
    share = np.where(married & (e > 0), spouse / np.where(e > 0, e, 1.0), 0.0)
    t = put_rows(t, c, {'spouse_deferral_share': np.where(asked, share, np.nan)})
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
    ages 80 and over as 80."""
    a = np.asarray(age, float)
    return np.where(np.isnan(a) | (a < 0), 0, np.minimum(np.nan_to_num(a, nan=-1), EXACT_AGE_TOP) + 1).astype(np.int64)


def survey_items(s, survey):
    """The survey's columns that the translation table's amount rows read, renamed raw_<column>."""
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
    # the order fixes the rows' order, and so the training batches
    for wave in [2016, 2019, 2022, 2013, 2025]:
        s = pd.read_csv(f'{common.DATA}/scf_units_{wave}.csv.gz')
        y = wave - 1
        off = WAVE_2013_ID_OFFSET if wave == 2013 else 1e8 * (y - PUF_YEAR)      # ids unique across waves
        s['record_id'] += off
        s['household_id'] += off
        s['age_key'] = s['household_id']
        common.say(f'wave {wave}:')
        # elective deferrals: asked only of the respondent and spouse or partner in the primary economic unit (PEU),
        # so missing exactly on the units outside it (taxunit_code 8, 9)
        asked = (s['taxunit_code'] <= 2).to_numpy()
        e = s['elective_deferrals'].to_numpy(float)
        assert not np.isnan(e[asked]).any() and np.isnan(e[~asked]).all(), 'elective deferrals missing on a PEU unit'
        assert (e[asked] >= 0).all() and (e[asked] <= s['wages'].to_numpy(float)[asked] + 1e-6).all()
        e1, e2 = s['elective1'].to_numpy(float), s['elective2'].to_numpy(float)     # the head's and the spouse's
        assert np.allclose(e1[asked] + e2[asked], e[asked]) and np.isnan(e2[~asked]).all()
        assert (e2[asked & (s['married'] == 0).to_numpy()] == 0).all(), 'a spouse\'s deferrals off a couple'
        # inheritances received this year: asked of the PEU only, like the deferrals
        h = s['inheritance'].to_numpy(float)
        assert not np.isnan(h[asked]).any() and np.isnan(h[~asked]).all() and (h[asked] >= 0).all(), \
            'inheritance missing on a PEU unit, or present outside it, or negative'
        common.say(f'  inheritances received this year: ${(s.weight * s.inheritance)[asked].sum() / 1e9:.1f} billion '
                   f'(income-year dollars) on {s.weight[asked & (h > 0)].sum() / 1e6:.3f} m units ({int((asked & (h > 0)).sum())} '
                   f'rows); not asked (missing, not zero) on the {int((~asked).sum())} rows outside the PEU')
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
    return pd.concat(waves, ignore_index=True)


# ---- the AGI group label (qgroups.py) --------------------------------------------------------
def agi_group_label(t, edges):
    """The AGI group label (column agi_group): on PUF rows the record's band-AGI group, -1 elsewhere, with its meta in
    the edges. Runs after the dollar maps are built, because band-AGI reads the PUF year's map."""
    from pretrain import qgroups as Q       # reads the band edges just written
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
    """The amounts every survey year shares with the PUF, through the translation table: definition fixes, function
    1 against the year's tax data, then the PUF's cut points grown to the year. Writes each year's tax data, the
    translation report and the fill-in shortfall."""
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
                    # not asked that year, or a row kept out of training: missing, so not shown in training
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
                # rank_against: a survey total put in one of its parts is matched to the tax data's total
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
                if line:   # the units beyond the row's dollar line
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


def puf_ss_check(t, puf):
    """A check: taxable Social Security by the section 86 worksheet against the PUF's reported amount (E02500), written
    to data/puf_ss_check.csv. Modified AGI is E00100 - E02500 plus tax-exempt interest and the student loan interest
    deduction."""
    p = t[t.source == 'puf']
    raw = puf.set_index('RECID').loc[p['record_id'].astype(np.int64).to_numpy()]
    rep = raw['E02500'].to_numpy(float)
    magi = (raw['E00100'] - raw['E02500'] + raw['E00400'] + raw['E03210']).to_numpy(float)
    calc = common.taxable_social_security(p['gross_ss'].to_numpy(float), magi, p['filing_status'].to_numpy())
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
    """A check: for each CPS year, the units and expected non-filers to whom the worksheet gives taxable Social
    Security, from the CPS's amounts in PUF terms. Written to data/cps_ss_taxable_check.csv."""
    rows = []
    for y in CP['years']:
        d = t[(t.source == 'cps') & (t.year == y)]
        a = survey_puf_amounts(d, 'cps', y)
        # modified AGI: the AGI items without taxable benefits plus tax-exempt interest (the CPS has no student loan
        # interest deduction)
        other = a['wages'] + a.get('txbl_int', 0.0) + a['dividends'] + a['capital_gains'] + \
            sum(ITEMS[v][0] * a.get(v, 0.0) for v in ITEMS if ITEMS[v][0] != 0) + a['other_agi'] + \
            a['exempt_int']
        tx = common.taxable_social_security(a['gross_ss'].to_numpy(float), other.to_numpy(float), d['filing_status'].to_numpy())
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
    common.say('CPS, taxable Social Security by the formula (no slot on any source):\n'
               + out.round(3).T.to_string())


def band_table(t):
    pside = (t.source == 'puf').to_numpy()
    w = t['weight'].to_numpy(float)
    edges = {}
    t = pd.concat([t, pd.DataFrame({f'{v}_band': np.int16(-1) for v in AMOUNTS}, index=t.index)], axis=1)
    for v in PUF_AMOUNTS:                        # cut on the PUF's records (filers, all of them)
        b, rec = band_variable(t.loc[pside, v].to_numpy(float), w[pside])
        edges[f'puf/{PUF_YEAR}/{v}'] = rec
        t.loc[pside, f'{v}_band'] = b
    t, factors = translate_surveys(t, edges)
    for src, own, rule in [('scf', SCF_AMOUNTS, 'within the wave'), ('cex', CEX_AMOUNTS, 'within the year'),
                           ('cps', CPS_AMOUNTS, 'within the year'), ('sipp', SIPP_AMOUNTS, 'within the year')]:
        for y in sorted(t.loc[t.source == src, 'year'].unique()):
            sel = ((t.source == src) & (t.year == y)).to_numpy()
            for v in own:
                b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel])
                rec['cut_rule'] = rule
                t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = b, rec
    for v in PUF_SHARES:   # one set of fixed cut points on the PUF and on every CPS year
        x = t[v].to_numpy(float)
        held = pside & ~np.isnan(x) & (x > 0)
        probs = [float(w[held & (x <= c)].sum() / w[held].sum()) for c in SHARE_CUTS]
        fixed = {'negative': {'bands_used': 0, 'probs': [], 'cuts': []},
                 'positive': {'probs': probs, 'cuts': SHARE_CUTS.tolist()}}
        for src in ['puf', 'cps']:
            for y in sorted(t.loc[t.source == src, 'year'].unique()):
                sel = ((t.source == src) & (t.year == y)).to_numpy()
                b, rec = band_variable(x[sel], w[sel], fixed)
                rec['cut_rule'] = 'fixed share cut points (build_table.SHARE_CUTS), the same on the PUF and the CPS'
                t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = b, rec
        assert (t.loc[~t.source.isin(['puf', 'cps']), v].isna()).all(), f'{v} on a source other than the PUF and the CPS'
    t = shared_between_surveys(t, edges)
    for v in AMOUNTS:
        b, x = t[f'{v}_band'], t[v]
        assert ((b == -1) == x.isna()).all() and ((b == 0) == (x == 0)).all(), v
        t[f'{v}_band'] = b.astype('int16')
    edges['meta'] = table_meta(t, factors)
    return t, edges


def shared_between_surveys(t, edges):
    """The translation rows shared between two surveys and not with the PUF: taken as reported and cut on the owning
    survey's cut points of the nearest year, so a band means the same on both. Appends to the translation report."""
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
            # the QBI items: each group's workers drawn after its industry group (zero with none); the
            # industry group never none where any of the group's PUF parts is nonzero
            'scf_structure': {'after': {f'biz_workers_{k}': f'biz_industry_{k}' for k in QBI_GROUPS},
                              'industry_parts': {f'biz_industry_{k}': parts for k, parts in QBI_GROUPS.items()}},
            'computed_flags': {'k1_owner': K1_PARTS},
            'scf_amounts': SCF_AMOUNTS,
            'cex_amounts': CEX_AMOUNTS,
            'cps_amounts': CPS_AMOUNTS,
            # the PUF slots shared with the CPS that are shares, drawn at generation after
            # the PUF items and their totals; the shares' gates (asked only of a married couple with some of a base);
            # the categories shared by the PUF and the CPS (the earnings split code) and the code's ranges
            'puf_shares': PUF_SHARES,
            'share_gates': SHARE_GATES,
            'shared_categories': SHARED_CATEGORIES,
            'earn_split': {'ranges': {str(k): list(r) for k, r in ES.RANGES.items()}, 'se_factor': ES.SE_FACTOR,
                           'se_floor': ES.SE_FLOOR, 'se_wage_base': ES.SE_WAGE_BASE},
            'cps_categories': CPS_CATEGORIES,
            'person_categories': PERSON_CATEGORIES,
            'exact_ages': EXACT_AGES,
            'cps_structure': {'spouse': SPOUSE_SLOTS, 'dependent_ages': DEPENDENT_AGE_SLOTS,
                              'primary_age': 'age1', 'ages': AGE_SLOTS,
                              # each adult's overtime: drawn after the other CPS slots; the primary
                              # filer's zero without wages, the spouse's without a spouse's share of wages above zero
                              'overtime': CPS_OVERTIME,
                              # the dependents' Social Security number status: the k-th present
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
            # person amounts held to a cap after the remainders: tips1 at most wages1, ot1 at most wages1 - tips1
            # (a third entry is subtracted from the cap and must come from an earlier entry); then the sums of the
            # person items, and yes/no items that hold only with their item
            'person_caps': {'tips_primary': ['tips1', 'wages1'], 'tips_spouse': ['tips2', 'wages2'],
                            'ot_primary': ['ot1', 'wages1', 'tips1'], 'ot_spouse': ['ot2', 'wages2', 'tips2']},
            'sums': {'tips': ['tips1', 'tips2'], 'ot': ['ot1', 'ot2']},
            'flags_with': {'tips_lh1': ['tips_lh_primary', 'tips1'], 'tips_lh2': ['tips_lh_spouse', 'tips2']},
            'kg_pieces': KG_PIECES,
            'agi_signs': {v: ITEMS[v][0] for v in ITEMS if ITEMS[v][0] != 0},
            'growth_factors': factor}


def present_on(t):
    """Per slot, the sources that have it: an amount where its band is set, a category where it is not -1."""
    rows = {s: (t.source == s).to_numpy() for s in t.source.unique()}
    out = {}
    for v in AMOUNTS:
        b = t[f'{v}_band'].to_numpy()
        out[v] = sorted(s for s, m in rows.items() if (b[m] >= 0).any())
    for v in ['married', 'n_dep', 'age_range', 'filer'] + list(TAX_CATEGORIES) + list(CATEGORIES) + list(PERSON_CATEGORIES) \
            + list(OWN_CATEGORIES) + list(SHARED_CATEGORIES):
        x = t[v].to_numpy()
        out[v] = sorted(s for s, m in rows.items() if (x[m] >= 0).any())
    return out


ALL_SURVEY_YEARS = False   # the side-file build (all_years): every survey year below, in place of the year's mix
SIDE_FILE_YEARS = [2015, 2017, 2019, 2021, 2023]


def mix_years(source):
    """The tax years of the survey in the target year's file (the year file's prior, survey_mix.py), the target year
    first when it is among them; with ALL_SURVEY_YEARS, SIDE_FILE_YEARS in that order."""
    if ALL_SURVEY_YEARS:
        return sorted(SIDE_FILE_YEARS, key=lambda y: (y != TARGET_YEAR, y))
    from shared import survey_mix
    return sorted(survey_mix.years(source), key=lambda y: (y != TARGET_YEAR, y))


def cex_target_units(pi):
    """The CEX's tax units with the primary filer's age range, for each year in the target year's survey mix: the
    reweighting's consumption targets and the report read them."""
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
    """Weighted shares by age range: PUF filers (probabilities where no range), and each survey year."""
    labels = common.AGE_LABELS
    groups = [('PUF filers', (t.source == 'puf') & (t.filer == 1))]
    for src in ('scf', 'cex', 'cps', 'sipp'):
        groups += [(f'{src.upper()} {y}', (t.source == src) & (t.year == y))
                   for y in sorted(t.loc[t.source == src, 'year'].unique())]
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
    # same percentile among earners), so the report's wage groups mean the same on both sides
    for src in ('cps', 'sipp'):
        for y in mix_years(src):
            f = f'{common.DATA}/{src}_units_{y}_all.parquet'
            u = pd.read_parquet(f)
            tw = t.loc[(t.source == src) & (t.year == y), ['record_id', 'wages']]
            u = u.merge(tw.rename(columns={'wages': 'wages_translated'}), on='record_id', how='left', validate='one_to_one')
            assert u['wages_translated'].notna().all()
            u.to_parquet(f, index=False)
    keep = ['source', 'year', 'record_id', 'household_id', 'implicate', 'weight', 'married', 'n_dep', 'filer', 'filer_p'] + \
        list(TAX_CATEGORIES) + list(CATEGORIES) + list(PERSON_CATEGORIES) + list(OWN_CATEGORIES) + \
        AMOUNTS + ['net_worth', 'value.primary_mortgage'] + [f'{v}_band' for v in AMOUNTS] + \
        ['age_range'] + [f'age_p{k}' for k in range(ages.N_RANGES)] + ['agi_group'] + list(SHARED_CATEGORIES) + ES_PROBS
    t['implicate'] = t['implicate'].fillna(0).astype(np.int64)
    for c in ES_PROBS + [f'age_p{k}' for k in range(ages.N_RANGES)]:
        t[c] = t[c].fillna(0.0)
    # a category is -1 on every source that does not report it, and on PUF records that carry probabilities instead
    has = {**{c: ['puf', 'cps'] for c in SHARED_CATEGORIES}, **{c: [s] for c, s in OWN_CATEGORIES.items()},
           **{c: [s for s in ('scf', 'cex', 'sipp') if c in EXACT_AGES and shares(s, c)] + ['cps']
              for c in PERSON_CATEGORIES}}
    for c, sources in has.items():
        on = t.source.isin(sources)
        assert not t.loc[on, c].isna().any() and t.loc[~on, c].isna().all(), c
        t[c] = t[c].fillna(-1)
    assert (t.loc[t.source == 'cps', list(SHARED_CATEGORIES)] >= 0).all().all()
    assert not t['filer_p'].isna().any() and ((t.filer_p == -1) == ~t.source.isin(['puf', 'cps'])).all()
    assert (t.loc[t.source == 'puf', 'filer_p'] == 1).all() and t.loc[t.source == 'cps', 'filer_p'].between(0, 1).all()
    for c in ['year', 'married', 'n_dep', 'filer', 'age_range'] + list(TAX_CATEGORIES) + list(CATEGORIES) + \
            list(PERSON_CATEGORIES) + list(OWN_CATEGORIES) + list(SHARED_CATEGORIES):
        assert not t[c].isna().any(), c
        t[c] = t[c].astype(np.int64)
    age_range_table(t)
    json.dump(edges, open(common.EDGES, 'w'), indent=1)   # dollars.py, which the label uses, reads the edges' meta
    from scale import maps as MP
    MP.build_maps(t, edges)
    t, edges = agi_group_label(t, edges)                   # from the bands and the PUF year's map
    t[keep].to_parquet(common.TABLE, index=False)
    json.dump(edges, open(common.EDGES, 'w'), indent=1)
    common.say(f'{len(t):,} rows:', t.groupby(['source', 'year', 'filer']).size().to_dict())
    table_summary(t)


if __name__ == '__main__':
    import sys
    from prep import build_table   # the module object the other stages import, so the option reaches every caller
    assert sys.argv[1:] in ([], ['all_years']), 'usage: python -m prep.build_table [all_years]'
    build_table.ALL_SURVEY_YEARS = sys.argv[1:] == ['all_years']
    build_table.main()
