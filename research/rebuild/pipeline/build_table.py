"""The training table: the PUF, crude non-filers and four SCF waves, stacked,
each amount cut into bands, plus the dollar maps.

Rows, in this order:
  PUF filers      the 2015 PUF without its four aggregate records; weight S006/100
  non-filers      the 2017 CPS non-filer pool on the PUF side (filer = 0),
                  dollars put in 2015 terms with the CPI-U
  SCF waves       2016, 2019, 2022, then 2013 (tax years 2015, 2018, 2021, 2012),
                  tax units from build_scf.R, all five implicates

Variables. married, n_dep (capped at 3), filer, and the amounts:
  wages E00200, taxable interest E00300, ordinary dividends E00600;
  capital gains as line 13 (E01000 + E01100) and its four pieces: short term
    P22250, long term P23250, distributions E01100, other E01000 - P22250 - P23250;
  business income E00900 + E02000 + E02100 and its eighteen parts (PARTS);
  the other AGI items (ITEMS), and other_agi = AGI minus everything above;
  gross Social Security plus pensions, and tax-exempt interest (not in AGI);
  net worth (SCF only).
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
  - business income is cut on the wave's own weighted ranks;
  - net worth is cut within each wave.
SCF ids of tax years 2018 and 2021 are offset by 1e8 x (year - 2015), the 2013
wave's by 2.013e12, non-filers' by 9e8, so every id is unique (the held-back
slice is drawn by household id).

The dollar maps: for each PUF-side amount and band, every record's value and
weight (and SCF net worth from the 2016 wave); a band with no record gets a
fill value. The 2019 map grows each value by SOI's 2019-over-2015 growth in
dollars per return with the item (growth_factors.csv).

Writes common.TABLE, common.EDGES, common.MAPS/{map_puf_2015, map_puf_2019,
map_scf_2015}.parquet and growth_factors.csv, and data/scf_units_2019.parquet
(for the tilt's wealth targets).
"""
import copy
import json
import os
import re

import numpy as np
import pandas as pd

import common

RAW = common.RAW
# ---- constants -------------------------------------------------------------------
CPI_2015, CPI_2017 = 237.017, 245.120          # CPI-U, to put the 2017 pool in 2015 dollars
NONFILER_ID_OFFSET = 900_000_000
WAVE_2013_ID_OFFSET = 2.013e12
NW_2016_WAVE_TO_2015 = 1.2493                  # the 2016 wave's 2022-dollar factor, the wealth targets' "2015 dollars"
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
    'business_income': (1, True, 'rank'), 'other_gains': (1, False, None), 'ira_taxable': (1, False, None),
    'pensions_taxable': (1, False, None), 'ss_taxable': (1, False, None), 'unemployment': (1, False, None),
    'alimony': (1, False, None), 'state_refunds': (1, False, None), 'adjustments': (-1, False, None),
    'retirement_gross': (0, True, 'unchanged'), 'tax_exempt_interest': (0, True, 'unchanged')}
PARTS = {  # part: (PUF field, sign in business income); loss fields are stored positive
    'sch_c': ('E00900', 1), 'part_pass_inc': ('E25940', 1), 'part_nonpass_inc': ('E25980', 1),
    'part_pass_loss': ('E25920', -1), 'part_nonpass_loss': ('E25960', -1), 'part_179': ('E26110', -1),
    'scorp_pass_inc': ('E26170', 1), 'scorp_nonpass_inc': ('E26190', 1), 'scorp_pass_loss': ('E26160', -1),
    'scorp_nonpass_loss': ('E26180', -1), 'scorp_179': ('E26100', -1), 'rent_inc': ('E25850', 1),
    'rent_loss': ('E25860', -1), 'estate_inc': ('E26390', 1), 'estate_loss': ('E26400', -1),
    'farm_rent': ('E27200', 1), 'sche_other': (None, 1), 'farm': ('E02100', 1)}
SCHE_PARTS = [p for p, (f, _) in PARTS.items() if f and p not in ('sch_c', 'farm')]
AMOUNTS = ['wages', 'interest', 'dividends', 'capital_gains'] + KG_PIECES + list(ITEMS) + list(PARTS) + \
          ['other_agi', 'net_worth']
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
    'wages': [('salaries and wages', 1)], 'interest': [('taxable interest', 1)],
    'dividends': [('ordinary dividends', 1)],
    'sch_d_net_gain': [('sales of capital assets reported on form 1040, schedule d > taxable net gain', 1)],
    'kg_dist': [('capital gain distributions reported on form 1040', 1)],
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
              'E02000', 'E02100', 'E02300', 'E02400', 'E02500', 'P22250', 'P23250'] + ADJ_FIELDS + \
             [f for f, _ in PARTS.values() if f and f not in ('E00900', 'E02100')]
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
             'tax_exempt_interest': I['E00400']}
    out.update(items)
    parts = {p: I[f] for p, (f, _) in PARTS.items() if f}
    parts['sche_other'] = I['E02000'] - sum(PARTS[p][1] * parts[p] for p in SCHE_PARTS)
    out.update(parts)
    assert np.array_equal(sum(PARTS[p][1] * parts[p] for p in PARTS), items['business_income'])
    out['other_agi'] = I['E00100'] - out['wages'] - out['interest'] - out['dividends'] - out['capital_gains'] - \
        sum(ITEMS[v][0] * items[v] for v in ITEMS)
    df = pd.DataFrame(out)
    for v in AMOUNTS[:-1]:
        df[v] = df[v].astype(float)
    df['net_worth'] = np.nan
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
                       'dividends': p['div_pref'] * d, 'capital_gains': 0.0})
    for v in KG_PIECES:
        df[v] = 0.0
    items = {'business_income': (p['sole_prop'] + p['rent'] + p['rent_loss']) * d,
             'pensions_taxable': p['txbl_pens_dist'] * d, 'unemployment': p['ui'] * d,
             'retirement_gross': (p['gross_ss'] + p['gross_pens_dist']) * d}
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
    df['net_worth'] = np.nan
    return df


def scf_rows():
    waves = []
    for wave in [2016, 2019, 2022, 2013]:
        s = pd.read_csv(f'{common.DATA}/scf_units_{wave}.csv.gz')
        y = wave - 1
        off = WAVE_2013_ID_OFFSET if wave == 2013 else 1e8 * (y - 2015)
        s['record_id'] += off
        s['household_id'] += off
        s['business_income'] = s['business_income'] + s['rent_royalty_estate']   # Schedules C, E and F together
        s['capital_gains'] = np.maximum(s['capital_gains'], np.where(s['capital_gains'] < 0, LOSS_CAP, -np.inf))
        s = s.assign(source='scf', filer=-1)
        waves.append(s)
        if wave == 2019:
            s.assign(net_worth_2015_dollars=s['net_worth'] * s['asset_factor'] / NW_2016_WAVE_TO_2015) \
                .to_parquet(f'{common.DATA}/scf_units_2019.parquet', index=False)
    s = pd.concat(waves, ignore_index=True)
    for v in AMOUNTS:
        if v not in s:
            s[v] = np.nan
    return s


# ---- bands --------------------------------------------------------------------------------
def band_table(t):
    filers = ((t.source == 'puf') & (t.filer == 1)).to_numpy()
    pside = (t.source == 'puf').to_numpy()
    w = t['weight'].to_numpy(float)
    years = sorted(t.loc[t.source == 'scf', 'year'].unique())
    val = soi_table(sorted(set(years) | {2015, 2019}))
    total = lambda item, y: soi_item(val, y, item)[0]
    factor = {}
    for y in years:
        for v in ['wages', 'interest', 'dividends']:
            factor[(v, y)] = total(v, y) / total(v, 2015)
        line13 = lambda yy: total('sch_d_net_gain', yy) + total('kg_dist', yy) - \
            soi_get(val, yy, 'sales of capital assets reported on form 1040, schedule d > taxable net loss', 'amount')
        factor[('capital_gains', y)] = line13(y) / line13(2015)
        for v in ['retirement_gross', 'tax_exempt_interest']:
            factor[(v, y)] = soi_item(val, y, v, base=2015)[0] / soi_item(val, 2015, v, base=y)[0]
    edges = {}
    for v in AMOUNTS:
        t[f'{v}_band'] = np.int16(-1)
    for v in AMOUNTS[:-1]:                       # cut on the PUF filers, applied to the whole PUF side
        b, rec = band_variable(t.loc[filers, v].to_numpy(float), w[filers])
        edges[f'puf/2015/{v}'] = rec
        t.loc[pside, f'{v}_band'] = band_variable(t.loc[pside, v].to_numpy(float), w[pside], grown_edge(rec, 1.0))[0]
    for y in years:
        sel = ((t.source == 'scf') & (t.year == y)).to_numpy()
        for v in SCF_FIXED:
            b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel], grown_edge(edges[f'puf/2015/{v}'], factor[(v, y)]))
            rec['cut_rule'] = f'PUF 2015 positive cuts x SOI factor {factor[(v, y)]:.4f}'
            t.loc[sel, f'{v}_band'], edges[f'scf/{y}/{v}'] = b, rec
        for v, rule in [('business_income', "rank: the wave's own weighted ranks at the PUF's band shares"),
                        ('net_worth', None)]:
            b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel])
            if rule:
                rec['cut_rule'] = rule
            t.loc[sel, f'{v}_band'], edges[f'scf/{y}/{v}'] = b, rec
    for v in AMOUNTS:
        b, x = t[f'{v}_band'], t[v]
        assert ((b == -1) == x.isna()).all() and ((b == 0) == (x == 0)).all(), v
        t[f'{v}_band'] = b.astype('int16')
    item_order = list(ITEMS) + list(PARTS)
    edges['meta'] = {'items': {v: ('shared' if v in ITEMS and ITEMS[v][1] else 'puf') for v in item_order},
                     'computed_totals': {'business_income': {p: s for p, (_, s) in PARTS.items()}},
                     'scf_growth_factors': {f'{v} {y}': f for (v, y), f in factor.items()}}
    return t, edges


# ---- dollar maps ----------------------------------------------------------------------------
def fill_value(edge, band):
    """The map value for a band with no record: a lump band's cut, else the band's own edge."""
    pos0 = edge['n_bands'] - (len(edge['positive']['cuts']) + 1)
    n_neg = pos0 - 1
    if 1 <= band <= n_neg and edge['negative']['bands_used'] == 0:
        return 0.0, 'no negatives in data: zero'
    assert edge['negative']['bands_used'] in (0, n_neg), 'unexpected negative band count'
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
    puf_vars = AMOUNTS[:-1]
    m15 = build_map(pside, edges, 'puf', puf_vars)
    m15.to_parquet(f'{common.MAPS}/map_puf_2015.parquet', index=False)
    build_map(t[(t.source == 'scf') & (t.year == 2015)].reset_index(drop=True), edges, 'scf', ['net_worth']) \
        .to_parquet(f'{common.MAPS}/map_scf_2015.parquet', index=False)
    # growth to 2019, per return with the item: (factor for positive values, for negative values)
    val = soi_table([2015, 2019])
    sch_d = per_return_growth(val, 'sch_d_net_gain')
    g = {'wages': per_return_growth(val, 'wages'), 'interest': per_return_growth(val, 'interest'),
         'dividends': per_return_growth(val, 'dividends')}
    g = {v: (f, f) for v, f in g.items()}
    g.update({'capital_gains': (sch_d, 1.0), 'kg_st': (sch_d, 1.0), 'kg_lt': (sch_d, 1.0), 'kg_other': (sch_d, 1.0),
              'kg_dist': (per_return_growth(val, 'kg_dist'), 1.0)})   # losses: the $3,000 cap is nominal
    for v in list(ITEMS) + ['other_agi']:
        f = per_return_growth(val, v)
        g[v] = (f, f)
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
    pd.DataFrame([{'variable': v, 'factor, positive values': fp, 'factor, negative values': fn}
                  for v, (fp, fn) in g.items()]).to_csv(f'{common.MAPS}/growth_factors.csv', index=False)


def main():
    os.makedirs(common.DATA, exist_ok=True)
    t = pd.concat([puf_rows(), nonfiler_rows(), scf_rows()], ignore_index=True)
    assert not t.duplicated(['source', 'record_id']).any()
    t, edges = band_table(t)
    keep = ['source', 'year', 'record_id', 'household_id', 'implicate', 'weight', 'married', 'n_dep', 'filer'] + \
        AMOUNTS + [f'{v}_band' for v in AMOUNTS]
    t['implicate'] = t['implicate'].fillna(0).astype(np.int64)
    for c in ['year', 'married', 'n_dep', 'filer']:
        t[c] = t[c].astype(np.int64)
    t[keep].to_parquet(common.TABLE, index=False)
    json.dump(edges, open(common.EDGES, 'w'), indent=1)
    build_maps(t, edges)
    common.say(f'{len(t):,} rows:', t.groupby(['source', 'year', 'filer']).size().to_dict())


if __name__ == '__main__':
    main()
