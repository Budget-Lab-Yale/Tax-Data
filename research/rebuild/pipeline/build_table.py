"""The training table: the PUF, crude non-filers, four SCF waves and six CEX years, stacked,
each amount cut into bands, plus the dollar maps.

Rows, in this order:
  PUF filers      the 2015 PUF without its four aggregate records; weight S006/100
  non-filers      the 2017 CPS non-filer pool on the PUF side (filer = 0),
                  dollars put in 2015 terms with the CPI-U
  SCF waves       2016, 2019, 2022, then 2013 (tax years 2015, 2018, 2021, 2012),
                  tax units from build_scf.R, all five implicates
  CEX years       the interview survey's tax units from build_cex.R (settings "cex"),
                  one row per tax unit and quarterly interview, spending annualized;
                  with one_tax_unit_only, only consumer units that are one tax unit;
                  dependent filers are folded into the unit that claims them

Variables. married, n_dep (capped at 3), filer, and the amounts:
  wages E00200, taxable interest E00300, ordinary dividends E00600;
  capital gains as line 13 (E01000 + E01100) and its four pieces: short term
    P22250, long term P23250, distributions E01100, other E01000 - P22250 - P23250;
  business income E00900 + E02000 + E02100 and its eighteen parts (PARTS), and
    two subtotals of it shared with the SCF: Schedules C and F (sch_cf_income;
    SCF X5704) and Schedule E (sch_e_income; SCF X5714, which is where the SCF
    puts partnership and S corporation income);
  age_range: the primary filer's age range (ages.py). On the PUF the
    demographic file's range, or, where it has none, -1 and step 0's
    probabilities (age_p0-age_p8), from which training draws; on the SCF and
    the non-filers, the range of the primary filer's age, couples by the
    primary-filer rule;
  the other AGI items (ITEMS), and other_agi = AGI minus everything above;
  gross Social Security plus pensions, and tax-exempt interest (not in AGI);
  mortgage interest paid E19200 and real estate tax E18500 (itemizers only;
    PUF only, not in AGI);
  k1_owner: any partnership or S corporation part nonzero (on the SCF: a
    partnership, S corporation or LLC interest, from build_scf.R);
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
On the PUF the parts add to the totals and the items to AGI, in whole dollars
(checked).

Bands. Each amount is cut into 84 bands: 0 = exactly zero, 1-10 losses, 11-83
positives (BAND_SHARES), cut on the PUF filers' weighted distribution; the
non-filers are placed on the filers' cut points. On the SCF:
  - wages, interest, dividends, capital gains, retirement income and
    tax-exempt interest are placed on the PUF's 2015 cut points, the positive
    ones grown to the wave's year by SOI's growth in the item's total;
  - SCF capital losses below -$3,000 are read as -$3,000 first (the limit on
    the return);
  - the two business subtotals are cut on the wave's own weighted ranks;
  - the balance-sheet items are cut within each wave.
On the CEX, wages and ss_gross as on the SCF (PUF cut points grown by
SOI; years past the last SOI year by its last year's growth), self-employment
income on the year's own weighted ranks, the categories within each year.
SCF ids of tax years 2018 and 2021 are offset by 1e8 x (year - 2015), the 2013
wave's by 2.013e12, non-filers' by 9e8, so every id is unique (the held-back
slice is drawn by household id).

The dollar maps: for each PUF-side amount and band, every record's value and
weight (and the SCF balance-sheet items from the 2016 wave); a band with no
record gets a fill value. The 2019 map grows each value by SOI's 2019-over-2015
growth in dollars per return with the item (growth_factors.csv).

Writes common.TABLE, common.EDGES, common.MAPS/{map_puf_2015, map_puf_2019,
map_scf_2015, map_cex_<generation year>, map_cex_2019}.parquet and
growth_factors.csv, data/scf_units_2019.parquet (for the tilt's wealth
targets, with net worth also in 2016 dollars, the balance-sheet maps') and
data/cex_units_<target year>_all.parquet (for the consumption targets).
"""
import copy
import json
import os
import re

import numpy as np
import pandas as pd

import ages
import common

RAW = common.RAW
# ---- constants -------------------------------------------------------------------
CPI_2015, CPI_2017 = 237.017, 245.120          # CPI-U, to put the 2017 pool in 2015 dollars
NONFILER_ID_OFFSET = 900_000_000
WAVE_2013_ID_OFFSET = 2.013e12
NW_2016_WAVE = 1.2493                          # the 2016 wave's asset price factor (2016 dollars to 2022 dollars)
LOSS_CAP = common.SETTINGS['translation']['loss_cap']['capital_gains']
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
NO_GROWTH = ['mortgage_interest', 'real_estate_tax']   # no SOI 1.4 row: kept in 2015 dollars in the 2019 map
PARTS = {  # part: (PUF field, sign in business income); loss fields are stored positive
    'sch_c': ('E00900', 1), 'part_pass_inc': ('E25940', 1), 'part_nonpass_inc': ('E25980', 1),
    'part_pass_loss': ('E25920', -1), 'part_nonpass_loss': ('E25960', -1), 'part_179': ('E26110', -1),
    'scorp_pass_inc': ('E26170', 1), 'scorp_nonpass_inc': ('E26190', 1), 'scorp_pass_loss': ('E26160', -1),
    'scorp_nonpass_loss': ('E26180', -1), 'scorp_179': ('E26100', -1), 'rent_inc': ('E25850', 1),
    'rent_loss': ('E25860', -1), 'estate_inc': ('E26390', 1), 'estate_loss': ('E26400', -1),
    'farm_rent': ('E27200', 1), 'sche_other': (None, 1), 'farm': ('E02100', 1)}
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
    + ['w_' + c for c in KG_WEALTH]
PUF_AMOUNTS = ['wages', 'interest', 'dividends', 'capital_gains'] + KG_PIECES + list(ITEMS) + list(PARTS) + ['other_agi']
CEX_CATS = ['c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables', 'c_food_off_premises',
            'c_gasoline', 'c_housing_utilities', 'c_other_services_health']
CEX_AMOUNTS = list(CEX_CATS)
CEX_FIXED = ['wages', 'ss_gross']          # on the PUF's cut points, grown by SOI
AMOUNTS = PUF_AMOUNTS + SCF_AMOUNTS + CEX_AMOUNTS
CX = common.SETTINGS['cex']
CEX_YEAR_ID_OFFSET = 1e10                      # CEX ids (interview number x 10 + tax unit) are below 1e10
CATEGORIES = {'k1_owner': 'shared', 'owns_business': 'scf'}   # yes/no items beyond married, n_dep, filer
SCF_FIXED = ['wages', 'interest', 'dividends', 'capital_gains', 'retirement_gross', 'tax_exempt_interest']


# ---- banding -----------------------------------------------------------------------
def wquantile(x, w, probs):
    """Smallest x whose cumulative weight share reaches each probability."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)]


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
            mcuts = wquantile(-x[neg], w[neg], probs)
            band[neg] = NEG0 + np.searchsorted(-mcuts[::-1], x[neg], side='left')
            rec['negative'] = {'bands_used': k, 'probs_from_least_negative': probs.tolist(),
                               'cuts': (-mcuts[::-1]).tolist(),
                               'note': 'cuts in dollars, most negative first; band 1 = below cuts[0]'}
        else:
            rec['negative'] = {'bands_used': 0, 'probs': [], 'cuts': []}
        if pos.any():
            cuts = wquantile(x[pos], w[pos], POS_SHARES)
            band[pos] = POS0 + np.searchsorted(cuts, x[pos], side='left')
            rec['positive'] = {'probs': POS_SHARES.tolist(), 'cuts': cuts.tolist()}
    bands = []
    for b in range(N_BANDS):
        m = band == b
        if not m.any():
            bands.append({'band': b, 'records': 0, 'weight': 0.0, 'min': None, 'max': None, 'median': None})
        else:
            bands.append({'band': b, 'records': int(m.sum()), 'weight': float(w[m].sum()), 'min': float(x[m].min()),
                          'max': float(x[m].max()), 'median': float(wquantile(x[m], w[m], [0.5])[0])})
    rec['bands'] = bands
    return band, rec


def grown_edge(edge, factor):
    """An edge record's cut points, the positive ones multiplied by factor."""
    rec = copy.deepcopy({k: edge[k] for k in ['negative', 'positive'] if k in edge})
    if 'positive' in rec and factor != 1.0:
        rec['positive']['cuts'] = (np.array(rec['positive']['cuts']) * factor).tolist()
    rec['n_bands'] = N_BANDS
    return rec


# ---- SOI totals (Table 1.4, all returns) ----------------------------------------------
NET = lambda name: [(name + ' > net income', 1), (name + r' > net loss.*', -1)]
SOI_ITEMS = {  # item: list of (SOI item pattern, sign)
    'wages': [('(salaries and wages|total wages > total)', 1)],   # SOI's name from tax year 2022
    'interest': [('taxable interest', 1)],
    'dividends': [('ordinary dividends', 1)],
    'sch_d_net_gain': [('sales of capital assets reported on form 1040, schedule d > taxable net gain', 1)],
    'kg_dist': [('capital gain distributions reported on form 1040', 1)],
    'sch_cf_income': NET('business or profession') + NET('farm'),
    'sch_e_income': NET('partnership and s corporation') + NET('rent') + NET('royalty') + NET('estate and trust')
    + NET('farm rental'),
    'business_income': NET('business or profession') + NET('partnership and s corporation') + NET('farm')
    + NET('rent') + NET('royalty') + NET('estate and trust') + NET('farm rental'),
    'other_gains': [('sales of property other than capital assets > net gain', 1),
                    ('sales of property other than capital assets > net loss', -1)],
    'ira_taxable': [(r'(taxable ira distributions|taxable individual retirement arrangement \(ira\) distributions'
                     r'|individual retirement arrangement.*taxable.*)', 1)],
    'pensions_taxable': [('pensions and annuities > taxable', 1)],
    'ss_taxable': [('social security benefits > taxable', 1)], 'unemployment': [('unemployment compensation', 1)],
    'alimony': [('alimony received', 1)], 'state_refunds': [('state income tax refunds', 1)],
    'adjustments': [('statutory adjustments > total', 1)],
    'retirement_gross': [('social security benefits > total', 1), ('pensions and annuities > total', 1)],
    'ss_gross': [('social security benefits > total', 1)],
    'tax_exempt_interest': [('tax exempt interest', 1)], 'other_agi': NET('other income')}
NI, NL = ' > net income', r' > net loss.*'
PART_GROWTH = {  # part: (SOI rows for positive values, for negative values); see build_maps
    'sch_c': (['business or profession' + NI], ['business or profession' + NL]),
    'farm': (['farm' + NI], ['farm' + NL]),
    'rent_inc': (['total rental and royalty' + NI], None), 'rent_loss': (['total rental and royalty' + NL], None),
    'estate_inc': (['estate and trust' + NI], None), 'estate_loss': (['estate and trust' + NL], None),
    'farm_rent': (['farm rental' + NI], ['farm rental' + NL]),
    **{p: (['partnership and s corporation' + NI], None)
       for p in ['part_pass_inc', 'part_nonpass_inc', 'scorp_pass_inc', 'scorp_nonpass_inc', 'part_179', 'scorp_179']},
    **{p: (['partnership and s corporation' + NL], None)
       for p in ['part_pass_loss', 'part_nonpass_loss', 'scorp_pass_loss', 'scorp_nonpass_loss']},
    'sche_other': ('business_income', None)}


def soi_table(years):
    t = pd.read_csv(os.path.join(RAW['soi_mirror'], 'bysize_income_sources.csv'), dtype=str)
    t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')
          & t['tax_year'].isin([str(y) for y in years])]
    return {(int(r.tax_year), r.item.lower().strip()): float(r.value) for r in t.itertuples()}


def soi_get(val, y, pattern, stat):
    hit = [n for (yy, n) in val if yy == y and re.fullmatch(pattern + ' > ' + stat, n)]
    if len(hit) != 1:
        assert not hit, (y, pattern, hit)
        raise KeyError(f'{y}: no SOI item {pattern} > {stat}')
    return val[(y, hit[0])] * (1000.0 if stat == 'amount' else 1.0)


def soi_item(val, y, item, base=None, rows=None, absolute=False):
    """Amount and returns of an item in year y, summed over its SOI rows; with
    base, only the rows published in both years (so a ratio compares like with like)."""
    amt = cnt = 0.0
    for p, s in ([(r, 1) for r in rows] if rows is not None else SOI_ITEMS[item]):
        try:
            a, c = soi_get(val, y, p, 'amount'), soi_get(val, y, p, 'number of returns')
            if base is not None:
                soi_get(val, base, p, 'amount')
        except KeyError:
            continue
        amt, cnt = amt + (abs(a) if absolute else s * a), cnt + c
    return amt, cnt


def per_return_growth(val, item, rows=None, absolute=False):
    """2019-over-2015 growth in dollars per return with the item."""
    (a15, c15), (a19, c19) = (soi_item(val, 2015, item, 2019, rows, absolute),
                              soi_item(val, 2019, item, 2015, rows, absolute))
    return (a19 / c19) / (a15 / c15)


# ---- the rows --------------------------------------------------------------------------
def puf_rows():
    fields = ['RECID', 'S006', 'MARS', 'XOCAH', 'XOCAWH', 'XOPAR', 'XOODEP', 'E00100', 'E00200', 'E00300', 'E00400',
              'E00600', 'E00700', 'E00800', 'E00900', 'E01000', 'E01100', 'E01200', 'E01400', 'E01500', 'E01700',
              'E02000', 'E02100', 'E02300', 'E02400', 'E02500', 'E18500', 'E19200', 'P22250', 'P23250'] + \
        ADJ_FIELDS + [f for f, _ in PARTS.values() if f and f not in ('E00900', 'E02100')]
    raw = pd.read_csv(RAW['puf'], usecols=fields)
    raw = raw[raw.RECID < 999996].reset_index(drop=True)          # the four aggregate records
    assert not raw.isna().any().any() and not raw.RECID.duplicated().any()
    I = {c: raw[c].round().astype(np.int64).to_numpy() for c in fields if c not in ('RECID', 'S006')}
    for c in I:
        assert np.array_equal(I[c], raw[c].to_numpy()), f'{c}: not whole dollars'
    n_dep = I['XOCAH'] + I['XOCAWH'] + I['XOPAR'] + I['XOODEP']
    out = {'source': 'puf', 'year': 2015, 'record_id': raw.RECID.astype(float), 'household_id': raw.RECID.astype(float),
           'weight': raw.S006 / 100, 'married': (I['MARS'] == 2).astype(np.int64),
           'n_dep': np.minimum(n_dep, 3).astype(np.int64), 'filer': 1,
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
    out['other_agi'] = I['E00100'] - out['wages'] - out['interest'] - out['dividends'] - out['capital_gains'] - \
        sum(ITEMS[v][0] * items[v] for v in ITEMS)
    df = pd.DataFrame(out)
    for v in PUF_AMOUNTS:
        df[v] = df[v].astype(float)
    for v in SCF_AMOUNTS:
        df[v] = np.nan
    rng, probs = ages.puf_age_ranges()
    assert np.array_equal(rng.RECID.to_numpy(), raw.RECID.to_numpy())
    df['age_range'] = rng.age_range.to_numpy()
    for k in range(ages.N_RANGES):
        df[f'age_p{k}'] = probs[:, k]
    return df


def nonfiler_rows():
    p = pd.read_csv(RAW['nonfiler_pool'].format(year=2017))
    assert (p['tax_year'] == 2017).all() and (p['filer'] == 0).all()
    d = CPI_2015 / CPI_2017
    rid = (NONFILER_ID_OFFSET + p['id'].astype(np.int64)).astype(float)
    df = pd.DataFrame({'source': 'puf', 'year': 2015, 'record_id': rid, 'household_id': rid,
                       'weight': p['weight'].astype(float), 'married': (p['filing_status'] == 2).astype(np.int64),
                       'n_dep': p['n_dep'].clip(upper=3).astype(np.int64), 'filer': 0,
                       'wages': p['wages'] * d, 'interest': p['txbl_int'].clip(lower=0) * d,
                       'dividends': p['div_pref'] * d, 'capital_gains': 0.0, 'k1_owner': 0, 'owns_business': -1,
                       'age1': p['age1'], 'age2': p['age2'], 'male1': p['male1'], 'male2': p['male2'],
                       'age_key': rid})
    for v in KG_PIECES:
        df[v] = 0.0
    items = {'business_income': (p['sole_prop'] + p['rent'] + p['rent_loss']) * d,
             'sch_cf_income': p['sole_prop'] * d, 'sch_e_income': (p['rent'] + p['rent_loss']) * d,
             'pensions_taxable': p['txbl_pens_dist'] * d, 'unemployment': p['ui'] * d,
             'retirement_gross': (p['gross_ss'] + p['gross_pens_dist']) * d, 'ss_gross': p['gross_ss'] * d}
    for v in ITEMS:
        df[v] = items.get(v, 0.0)
    assert (p['rent'] >= 0).all() and (p['rent_loss'] <= 0).all()
    parts = {'sch_c': p['sole_prop'] * d, 'rent_inc': p['rent'] * d, 'rent_loss': -p['rent_loss'] * d}
    for q in PARTS:
        df[q] = parts.get(q, 0.0)
    # the rest of AGI: the pool's whole AGI list in 2015 dollars, less the items (computed as a
    # difference, as the table of record was)
    whole = (p['sole_prop'] + p['txbl_pens_dist'] + p['rent'] + p['rent_loss'] + p['ui'] + p['other_inc']) * d
    df['other_agi'] = whole - sum(ITEMS[v][0] * df[v] for v in ITEMS)
    assert np.abs(df['other_agi'] - p['other_inc'] * d).max() < 0.01
    for v in SCF_AMOUNTS:
        df[v] = np.nan
    return df


def cex_rows():
    """The CEX tax units (build_cex.R); with one_tax_unit_only, those whose consumer unit is one tax unit."""
    rows = []
    for y in CX['years']:
        s = pd.read_csv(f'{common.DATA}/cex_units_{y}.csv.gz')
        assert (s['year'] == y).all() and (s['weight'] > 0).all() and not s['record_id'].duplicated().any()
        n0, w0 = len(s), s['weight'].sum()
        if CX['one_tax_unit_only']:
            s = s[s['one_tu'] == 1]
        common.say(f'CEX {y}: {len(s):,} of {n0:,} rows kept, {100 * s.weight.sum() / w0:.1f}% of the weight '
                   f'({s.weight.sum() / 1e6:.1f} million units)')
        rows.append(s)
    s = pd.concat(rows, ignore_index=True)
    need = ['social_security', 'pensions', 'wages', 'se_income'] + CEX_CATS
    n_missing = s[need].isna().sum()
    assert not n_missing.any(), f'missing values in the CEX units:\n{n_missing[n_missing > 0]}'
    assert (s['dep_filer'] == 0).all(), 'build_cex.R folds dependent filers into the unit that claims them'
    # record ids unique across years; the household id is not offset, so a consumer unit interviewed in two
    # survey years falls on one side of the held-back split
    off = (s['year'] - 2000) * CEX_YEAR_ID_OFFSET
    df = pd.DataFrame({'source': 'cex', 'year': s['year'], 'record_id': s['record_id'] + off,
                       'household_id': s['household_id'].astype(float), 'weight': s['weight'].astype(float),
                       'married': s['married'].astype(np.int64), 'n_dep': s['n_dep'].clip(upper=3).astype(np.int64),
                       'filer': -1, 'k1_owner': -1, 'owns_business': -1,
                       'wages': s['wages'].astype(float), 'ss_gross': s['social_security'].astype(float),
                       'sch_cf_income': s['se_income'].astype(float),
                       'age1': s['age1'], 'age2': s['age2'], 'male1': s['male1'], 'male2': s['male2'],
                       'age_key': s['household_id'].astype(float)})
    for c in CEX_CATS:
        df[c] = s[c].astype(float)
    for v in AMOUNTS:
        if v not in df:
            df[v] = np.nan
    return df


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
    for wave in [2016, 2019, 2022, 2013]:
        s = pd.read_csv(f'{common.DATA}/scf_units_{wave}.csv.gz')
        y = wave - 1
        off = WAVE_2013_ID_OFFSET if wave == 2013 else 1e8 * (y - 2015)
        s['record_id'] += off
        s['household_id'] += off
        s['age_key'] = s['household_id']
        s['sch_cf_income'], s['sch_e_income'] = s['business_income'], s['rent_royalty_estate']
        s['business_income'] = s['business_income'] + s['rent_royalty_estate']   # Schedules C, E and F together
        s['capital_gains'] = np.maximum(s['capital_gains'], np.where(s['capital_gains'] < 0, LOSS_CAP, -np.inf))
        common.say(f'wave {wave}:')
        s = loan_to_value(s.assign(source='scf', filer=-1))
        if wave == 2019:
            f = s['asset_factor'] / NW_2016_WAVE         # 2019 dollars to 2016 dollars, the balance-sheet maps' dollars
            dollars = list(NET_WORTH_PARTS) + ['w_' + c for c in KG_WEALTH]
            s.assign(net_worth_2016_dollars=s['net_worth'] * f, **{c + '_2016_dollars': s[c] * f for c in dollars}) \
                .to_parquet(f'{common.DATA}/scf_units_2019.parquet', index=False)
        s['business_income'] = np.nan     # PUF only: on the SCF it is its two subtotals
        waves.append(s)
    s = pd.concat(waves, ignore_index=True)
    for v in AMOUNTS:
        if v not in s:
            s[v] = np.nan
    return s


# ---- bands --------------------------------------------------------------------------------
def scale_top_wages(x, w, puf_x, puf_w, line):
    """SCF wages above line moved onto the PUF's distribution by count from the top: an SCF unit with N
    units (weighted) earning more than it gets the PUF wage with N returns earning more, times one constant
    that makes the map continuous at line (the map is y(x) * line / y(line)). Below line, unchanged."""
    o = np.argsort(-puf_x)
    p_val, p_above = puf_x[o], np.cumsum(puf_w[o]) - puf_w[o] / 2       # PUF returns above each value
    so = np.argsort(-x)
    s_above = np.empty(len(x))
    s_above[so] = np.cumsum(w[so]) - w[so] / 2
    y = np.interp(s_above, p_above, p_val)
    y_line = np.interp((w * (x > line)).sum(), p_above, p_val)
    out = x.copy()
    top = x > line
    out[top] = y[top] * line / y_line
    return out


def band_table(t):
    filers = ((t.source == 'puf') & (t.filer == 1)).to_numpy()
    pside = (t.source == 'puf').to_numpy()
    w = t['weight'].to_numpy(float)
    years = sorted(t.loc[t.source == 'scf', 'year'].unique())
    cex_years = sorted(t.loc[t.source == 'cex', 'year'].unique())
    soi_years = sorted({int(y) for (y, _) in soi_table(range(2010, 2040))})
    last = soi_years[-1]
    val = soi_table(sorted((set(years) | set(cex_years) | {2015, 2019, last - 1, last}) & set(soi_years)))
    total = lambda item, y: soi_item(val, y, item)[0]
    factor = {}
    for y in sorted(set(years) | set(cex_years)):
        if y > last:
            continue
        for v in ['wages', 'interest', 'dividends']:
            factor[(v, y)] = total(v, y) / total(v, 2015)
        line13 = lambda yy: total('sch_d_net_gain', yy) + total('kg_dist', yy) - \
            soi_get(val, yy, 'sales of capital assets reported on form 1040, schedule d > taxable net loss', 'amount')
        factor[('capital_gains', y)] = line13(y) / line13(2015)
        for v in ['retirement_gross', 'tax_exempt_interest', 'ss_gross']:
            factor[(v, y)] = soi_item(val, y, v, base=2015)[0] / soi_item(val, 2015, v, base=y)[0]
    for y in cex_years:                          # past the last SOI year: that year's growth carried forward
        if y > last:
            for v in CEX_FIXED:
                g = soi_item(val, last, v, base=last - 1)[0] / soi_item(val, last - 1, v, base=last)[0]
                factor[(v, y)] = factor[(v, last)] * g ** (y - last)
                common.say(f'{v} {y}: no SOI year; factor {factor[(v, y)]:.4f} = {last} x {last - 1}-{last} growth')
    bad = {k: f for k, f in factor.items() if not (np.isfinite(f) and f > 0)}
    assert not bad, f'SOI growth factors missing (an SOI line not found?): {bad}'
    common.say('SOI growth factors from 2015: ' + ', '.join(f'{v} {y} {f:.3f}' for (v, y), f in sorted(factor.items())))
    edges = {}
    for v in AMOUNTS:
        t[f'{v}_band'] = np.int16(-1)
    for v in PUF_AMOUNTS:                        # cut on the PUF filers, applied to the whole PUF side
        b, rec = band_variable(t.loc[filers, v].to_numpy(float), w[filers])
        edges[f'puf/2015/{v}'] = rec
        t.loc[pside, f'{v}_band'] = band_variable(t.loc[pside, v].to_numpy(float), w[pside], grown_edge(rec, 1.0))[0]
    t['wages_as_reported'] = t['wages']
    W = common.SETTINGS['translation'].get('wage_top_scaling')
    for y in years:
        sel = ((t.source == 'scf') & (t.year == y)).to_numpy()
        if W:
            line = W['line_2015_dollars'] * factor[('wages', y)]
            x = t.loc[sel, 'wages'].to_numpy(float)
            new = scale_top_wages(x, w[sel], t.loc[filers, 'wages'].to_numpy(float) * factor[('wages', y)],
                                  w[filers], line)
            t.loc[sel, 'wages'] = new
            counts = {f'${c / 1e3:,.0f}k+': (round(w[sel][x >= c * factor[('wages', y)]].sum() / 1e6, 3),
                                              round(w[sel][new >= c * factor[('wages', y)]].sum() / 1e6, 3),
                                              round(w[filers][t.loc[filers, 'wages'].to_numpy(float) >= c].sum() / 1e6, 3))
                      for c in [200e3, 500e3, 1e6]}
            common.say(f'SCF {y} wages above ${line:,.0f} moved onto the PUF scale; units at each 2015 line grown '
                       f'(millions: as reported, after, PUF 2015): {counts}')
        for v in SCF_FIXED:
            b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel], grown_edge(edges[f'puf/2015/{v}'], factor[(v, y)]))
            rec['cut_rule'] = f'PUF 2015 positive cuts x SOI factor {factor[(v, y)]:.4f}'
            t.loc[sel, f'{v}_band'], edges[f'scf/{y}/{v}'] = b, rec
        for v, rule in [(v, "rank: the wave's own weighted ranks at the PUF's band shares")
                        for v in ('sch_cf_income', 'sch_e_income')] + [(v, 'within the wave') for v in SCF_AMOUNTS]:
            b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel])
            rec['cut_rule'] = rule
            t.loc[sel, f'{v}_band'], edges[f'scf/{y}/{v}'] = b, rec
    for y in cex_years:
        sel = ((t.source == 'cex') & (t.year == y)).to_numpy()
        for v in CEX_FIXED:
            b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel], grown_edge(edges[f'puf/2015/{v}'], factor[(v, y)]))
            rec['cut_rule'] = f'PUF 2015 positive cuts x SOI factor {factor[(v, y)]:.4f}'
            t.loc[sel, f'{v}_band'], edges[f'cex/{y}/{v}'] = b, rec
        for v, rule in [('sch_cf_income', "rank: the year's own weighted ranks at the PUF's band shares")] + \
                [(v, 'within the year') for v in CEX_AMOUNTS]:
            b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel])
            rec['cut_rule'] = rule
            t.loc[sel, f'{v}_band'], edges[f'cex/{y}/{v}'] = b, rec
    for v in AMOUNTS:
        b, x = t[f'{v}_band'], t[v]
        assert ((b == -1) == x.isna()).all() and ((b == 0) == (x == 0)).all(), v
        t[f'{v}_band'] = b.astype('int16')
    item_order = list(ITEMS) + list(PARTS)
    edges['meta'] = {'items': {v: ('shared' if v in ITEMS and ITEMS[v][1] else 'puf') for v in item_order},
                     'computed_totals': {'business_income': {p: s for p, (_, s) in PARTS.items()},
                                         'sch_cf_income': {p: PARTS[p][1] for p in CF_PARTS},
                                         'sch_e_income': {p: PARTS[p][1] for p in E_PARTS}},
                     'categories': CATEGORIES,
                     'computed_flags': {'k1_owner': K1_PARTS},
                     'scf_amounts': SCF_AMOUNTS,
                     'cex_amounts': CEX_AMOUNTS,
                     'present_on': present_on(t),
                     'net_worth_parts': NET_WORTH_PARTS,
                     'ratios': {'w_primary_ltv': ['w_primary_mortgage', 'w_primary_home']},
                     'scf_growth_factors': {f'{v} {y}': f for (v, y), f in factor.items()}}
    return t, edges


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


def build_map(df, edges, source, variables, year=2015):
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


def build_maps(t, edges):
    os.makedirs(common.MAPS, exist_ok=True)
    pside = t[t.source == 'puf'].reset_index(drop=True)
    m15 = build_map(pside, edges, 'puf', PUF_AMOUNTS)
    m15.to_parquet(f'{common.MAPS}/map_puf_2015.parquet', index=False)
    build_map(t[(t.source == 'scf') & (t.year == 2015)].reset_index(drop=True), edges, 'scf', SCF_AMOUNTS) \
        .to_parquet(f'{common.MAPS}/map_scf_2015.parquet', index=False)
    # growth to 2019, per return with the item: (factor for positive values, for negative values)
    val = soi_table([2015, 2019])
    sch_d = per_return_growth(val, 'sch_d_net_gain')
    g = {'wages': per_return_growth(val, 'wages'), 'interest': per_return_growth(val, 'interest'),
         'dividends': per_return_growth(val, 'dividends')}
    g = {v: (f, f) for v, f in g.items()}
    g.update({'capital_gains': (sch_d, 1.0), 'kg_st': (sch_d, 1.0), 'kg_lt': (sch_d, 1.0), 'kg_other': (sch_d, 1.0),
              'kg_dist': (per_return_growth(val, 'kg_dist'), 1.0)})   # losses: the $3,000 cap is nominal
    for v in [v for v in ITEMS if v not in NO_GROWTH] + ['other_agi']:
        f = per_return_growth(val, v)
        g[v] = (f, f)
    g.update({v: (1.0, 1.0) for v in NO_GROWTH})
    for p, (pos_rows, neg_rows) in PART_GROWTH.items():
        fp = per_return_growth(val, 'business_income') if pos_rows == 'business_income' else \
            per_return_growth(val, None, rows=pos_rows, absolute=True)
        fn = per_return_growth(val, None, rows=neg_rows, absolute=True) if neg_rows else fp
        g[p] = (fp, fn)
    m19 = m15.copy()
    for v, (fp, fn) in g.items():
        r = (m19['variable'] == v).to_numpy()
        x = m19.loc[r, 'value'].to_numpy(float).copy()
        x[x > 0] *= fp
        x[x < 0] *= fn
        m19.loc[r, 'value'] = x
    m19.to_parquet(f'{common.MAPS}/map_puf_2019.parquet', index=False)
    # the CEX categories: the generation year's real records, and the same grown to 2019 by NIPA PCE per return
    # (the category's PCE over SOI's return count, 2019 over the CEX year)
    yc = CX['generation_year']
    mc = build_map(t[(t.source == 'cex') & (t.year == yc)].reset_index(drop=True), edges, 'cex', CEX_AMOUNTS, year=yc)
    mc.to_parquet(f'{common.MAPS}/map_cex_{yc}.parquet', index=False)
    pce = pd.read_csv(RAW['pce']).set_index('year')
    val_c = soi_table([2019, yc])
    returns = {y: val_c[(y, 'number of returns')] for y in (2019, yc)}
    m19c = mc.copy()
    for v in CEX_AMOUNTS:
        f = (pce.loc[2019, v] / returns[2019]) / (pce.loc[yc, v] / returns[yc])
        g[v] = (f, f)
        r = (m19c['variable'] == v).to_numpy()
        m19c.loc[r, 'value'] = m19c.loc[r, 'value'] * f
    m19c.to_parquet(f'{common.MAPS}/map_cex_2019.parquet', index=False)
    pd.DataFrame([{'variable': v, 'factor, positive values': fp, 'factor, negative values': fn}
                  for v, (fp, fn) in g.items()]).to_csv(f'{common.MAPS}/growth_factors.csv', index=False)


def present_on(t):
    """Per slot, the sources that have it: an amount where its band is set, a category where it is not -1."""
    out = {}
    for v in AMOUNTS:
        out[v] = sorted(s for s in t.source.unique() if (t.loc[t.source == s, f'{v}_band'] >= 0).any())
    for v in ['married', 'n_dep', 'age_range', 'filer'] + list(CATEGORIES):
        out[v] = sorted(s for s in t.source.unique() if (t.loc[t.source == s, v] >= 0).any())
    return out


def cex_target_units(pi):
    """The target year's CEX tax units, all of them (a consumer unit's spending divided among its tax units by head
    count, build_cex.R, which folds dependent filers into the unit that claims them), with the primary filer's age
    range by the rule the training rows use. For the tilt's consumption targets and the report:
    data/cex_units_<target year>_all.parquet."""
    y = CX['target_year']
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


def primary_age_range(t):
    """age_range on the SCF and non-filer rows: the range of the primary filer's age, couples by the rule in
    ages.py, with the woman-first chances set on the SCF's mixed-sex couples (all waves, weighted)."""
    sv = (t.source == 'scf').to_numpy()
    cx = (t.source == 'cex').to_numpy()
    nf = ((t.source == 'puf') & (t.filer == 0)).to_numpy()
    rows = sv | nf | cx
    couple = (t.married == 1).to_numpy() & rows
    assert not t.loc[rows, ['age1', 'male1']].isna().any().any()
    assert not t.loc[couple, ['age2', 'male2']].isna().any().any() and (t.loc[couple, 'age2'] >= 0).all()
    mixed = couple & (t.male1 != t.male2).to_numpy()
    m = t[mixed & sv]
    man = np.where(m.male1 == 1, m.age1, m.age2)
    woman = np.where(m.male1 == 1, m.age2, m.age1)
    target = ages.puf_female_primary_share()
    pi, share = ages.woman_first_chance(man, woman, m.weight, target)
    a1 = t.age1.fillna(0).to_numpy(float)
    a2 = t.age2.fillna(0).to_numpy(float)
    m1 = t.male1.fillna(1).to_numpy(float)
    m2 = t.male2.fillna(1).to_numpy(float)
    age, woman_first = ages.primary_age(a1, a2, m1, m2, couple, t.age_key.fillna(0).to_numpy(), pi)
    cex_target_units(pi)
    t.loc[rows, 'age_range'] = ages.adult_range(age[rows])
    w = t.weight.to_numpy(float)
    report = pd.DataFrame({'range': ['under 26', '26-34', '35-44', '45-54', '55-64', '65+'],
                           'woman-first chance by the man\'s range (%)': 100 * pi,
                           'PUF female primaries (%)': 100 * target,
                           'SCF female primaries after the rule (%)': 100 * share})
    common.say('primary filer of survey couples:\n' + report.round(1).to_string(index=False))
    for lab, sel in [('SCF', sv), ('non-filers', nf), ('CEX', cx)]:
        c = couple & sel
        common.say(f'  {lab}: woman first in {100 * w[c & woman_first].sum() / w[c].sum():.1f}% of couples '
                   f'(weighted); same-sex couples {100 * w[c & ~mixed].sum() / w[c].sum():.2f}%')
    assert t.age_range.notna().all()
    return t.drop(columns=['age1', 'age2', 'male1', 'male2', 'age_key', 'taxunit_code'], errors='ignore')


def age_range_table(t):
    """Weighted shares by age range: PUF filers (step 0's probabilities where no range), non-filers, SCF waves."""
    labels = ['under 26', '26-34', '35-44', '45-54', '55-64', '65+', 'dep <18', 'dep 18-25', 'dep 26+']
    groups = [('PUF filers', (t.source == 'puf') & (t.filer == 1)), ('non-filers', (t.source == 'puf') & (t.filer == 0))] + \
        [(f'SCF {y}', (t.source == 'scf') & (t.year == y)) for y in sorted(t.loc[t.source == 'scf', 'year'].unique())] + \
        [(f'CEX {y}', (t.source == 'cex') & (t.year == y)) for y in sorted(t.loc[t.source == 'cex', 'year'].unique())]
    cols = {}
    for lab, sel in groups:
        d = t[sel]
        P = np.column_stack([(d.age_range == k).to_numpy(float) + d[f'age_p{k}'].to_numpy() for k in range(ages.N_RANGES)])
        cols[lab] = 100 * (d.weight.to_numpy()[:, None] * P).sum(0) / d.weight.sum()
    out = pd.DataFrame(cols, index=labels)
    out.to_csv(f'{common.DATA}/age_range_shares.csv')
    common.say('age range, % of weight:\n' + out.round(1).to_string())


def main():
    os.makedirs(common.DATA, exist_ok=True)
    t = pd.concat([puf_rows(), nonfiler_rows(), scf_rows(), cex_rows()], ignore_index=True)
    assert not t.duplicated(['source', 'record_id']).any()
    t = primary_age_range(t)
    t, edges = band_table(t)
    keep = ['source', 'year', 'record_id', 'household_id', 'implicate', 'weight', 'married', 'n_dep', 'filer'] + \
        list(CATEGORIES) + AMOUNTS + ['net_worth', 'w_primary_mortgage', 'wages_as_reported'] + [f'{v}_band' for v in AMOUNTS] + \
        ['age_range'] + [f'age_p{k}' for k in range(ages.N_RANGES)]
    t['implicate'] = t['implicate'].fillna(0).astype(np.int64)
    for k in range(ages.N_RANGES):
        t[f'age_p{k}'] = t[f'age_p{k}'].fillna(0.0)
    for c in ['year', 'married', 'n_dep', 'filer', 'age_range'] + list(CATEGORIES):
        assert not t[c].isna().any(), c
        t[c] = t[c].astype(np.int64)
    age_range_table(t)
    t[keep].to_parquet(common.TABLE, index=False)
    json.dump(edges, open(common.EDGES, 'w'), indent=1)
    build_maps(t, edges)
    common.say(f'{len(t):,} rows:', t.groupby(['source', 'year', 'filer']).size().to_dict())


if __name__ == '__main__':
    main()
