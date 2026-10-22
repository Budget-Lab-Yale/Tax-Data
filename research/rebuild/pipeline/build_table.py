"""The training table: the PUF, crude non-filers, four SCF waves and six CEX years, stacked,
each amount cut into bands, plus the dollar maps.

Rows, in this order:
  PUF filers      the PUF (the PUF year, settings "years") without its four aggregate
                  records; weight S006/100
  non-filers      the PUF year's non-filer tax units (filer = 0; nonfiler_rows: the CPS
                  pool's nearest year, scaled to the PUF year as the tax data is)
  SCF waves       2016, 2019, 2022, then 2013 (tax years 2015, 2018, 2021, 2012),
                  tax units from build_scf.R, all five implicates
  CEX years       the interview survey's tax units from build_cex.R (settings "cex"),
                  one row per tax unit and quarterly interview, spending annualized;
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
non-filers are placed on the filers' cut points. The amounts a survey shares
with the PUF come from the translation table (settings "translation",
translate.py): for each survey year, the tax data for that year (tax_data),
the row's definition fixes and function 1, then the PUF's cut points grown to
that year per return. The report is data/translation_report.csv. The SCF's
balance sheet is cut within each wave, the CEX's categories within each year.
SCF ids of tax years 2018 and 2021 are offset by 1e8 x (year - 2015), the 2013
wave's by 2.013e12, non-filers' by 9e8, so every id is unique (the held-back
slice is drawn by household id).

The dollar maps: for each amount and band, every record's value and weight; a
band with no record gets a fill value.
  PUF side    the PUF year's records; the target year's map grows each value by
              SOI's growth in dollars per return with the item
              (growth_factors.csv; targets.py has the SOI lookups);
  SCF         the balance sheet from the wave whose tax year is the PUF year (the
              2016 wave), in that wave's dollars; the target year's map carries
              them to the target year's wave dollars by the two waves' asset
              price factors;
  CEX         the generation year's records; the target year's map grows them by
              NIPA PCE per return.

Writes common.TABLE, common.EDGES, common.MAPS/map_{puf,scf}_{PUF year, target
year}.parquet, map_cex_{generation year, target year}.parquet and
growth_factors.csv; data/translation_report.csv; data/scf_units_<target year>.parquet (the tilt's wealth
targets) and data/cex_units_<target year>_all.parquet (the consumption
targets).
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
POOL_YEARS = range(2017, 2024)                 # the years the non-filer pool covers
NONFILER_ID_OFFSET = 900_000_000
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
AMOUNTS = PUF_AMOUNTS + SCF_AMOUNTS + CEX_AMOUNTS
CX = common.SETTINGS['cex']
CEX_YEAR_ID_OFFSET = 1e10                      # added to build_cex.R's ids (year x 1e10 + interview x 100 + unit)
CATEGORIES = {'k1_owner': 'shared', 'owns_business': 'scf'}   # yes/no items beyond married, n_dep, filer


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
PUF_FIELDS = ['RECID', 'S006', 'MARS', 'XOCAH', 'XOCAWH', 'XOPAR', 'XOODEP', 'E00100', 'E00200', 'E00300', 'E00400',
              'E00600', 'E00700', 'E00800', 'E00900', 'E01000', 'E01100', 'E01200', 'E01400', 'E01500', 'E01700',
              'E02000', 'E02100', 'E02300', 'E02400', 'E02500', 'E18500', 'E19200', 'P22250', 'P23250'] + \
    ADJ_FIELDS + [f for f, _ in PARTS.values() if f and f not in ('E00900', 'E02100')]


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
    out = {'source': 'puf', 'year': PUF_YEAR, 'record_id': raw.RECID.astype(float), 'household_id': raw.RECID.astype(float),
           'weight': raw.S006 / 100, 'married': (I['MARS'] == 2).astype(np.int64), 'mars': I['MARS'],
           'dep_filer': (puf['DSI'] == 1).astype(np.int64).to_numpy(),
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
    rng, probs = ages.puf_age_ranges(puf)
    assert np.array_equal(rng.RECID.to_numpy(), raw.RECID.to_numpy())
    df['age_range'] = rng.age_range.to_numpy()
    for k in range(ages.N_RANGES):
        df[f'age_p{k}'] = probs[:, k]
    return df


def nonfiler_rows(year):
    """The non-filer tax units of year (filer = 0) from the CPS pool, in that year's dollars. A year the pool does not
    cover takes the nearest pool year, its weights scaled by Table 1.4's returns (year over pool year) and its dollars
    grown per return as the PUF's are: a placeholder until the pool is extended (ROADMAP item 8)."""
    py = min(max(year, POOL_YEARS[0]), POOL_YEARS[-1])
    p = pd.read_csv(RAW['nonfiler_pool'].format(year=py))
    assert (p['tax_year'] == py).all() and (p['filer'] == 0).all() and (p['dep_status'] == 0).all()
    rid = (NONFILER_ID_OFFSET + p['id'].astype(np.int64)).astype(float)
    neg = int((p['txbl_int'] < 0).sum())
    scale = TG.all_returns(year) / TG.all_returns(py)
    common.say(f'non-filers {year}: pool {py}, weights x {scale:.4f}; {neg} records with negative taxable interest '
               'read as zero (no loss on the return)')
    df = pd.DataFrame({'source': 'puf', 'year': year, 'record_id': rid, 'household_id': rid,
                       'weight': p['weight'].astype(float) * scale, 'married': (p['filing_status'] == 2).astype(np.int64),
                       'n_dep': p['n_dep'].clip(upper=3).astype(np.int64), 'filer': 0, 'mars': p['filing_status'],
                       'dep_filer': 0, 'wages': p['wages'], 'interest': p['txbl_int'].clip(lower=0),
                       'dividends': p['div_pref'], 'k1_owner': 0, 'owns_business': -1,
                       'age1': p['age1'], 'age2': p['age2'], 'male1': p['male1'], 'male2': p['male2'],
                       'age_key': rid})
    for v in KG_PIECES:
        df[v] = p['kg_lt'].astype(float) if v == 'kg_lt' else 0.0    # the pool's capital gains are long-term
    common.say(f'  {int((p["kg_lt"] != 0).sum())} records with capital gains, '
               f'${(p["kg_lt"] * p["weight"]).sum() / 1e9:.1f}bn (weighted, pool-year dollars)')
    items = {'pensions_taxable': p['txbl_pens_dist'], 'unemployment': p['ui'],
             'retirement_gross': p['gross_ss'] + p['gross_pens_dist'], 'ss_gross': p['gross_ss']}
    for v in ITEMS:
        df[v] = items.get(v, 0.0)
    assert (p['rent'] >= 0).all() and (p['rent_loss'] <= 0).all()
    parts = {'sch_c': p['sole_prop'], 'rent_inc': p['rent'], 'rent_loss': -p['rent_loss']}
    for q in PARTS:
        df[q] = parts.get(q, 0.0)
    df = computed_totals(df)
    # the rest of AGI: the pool's whole AGI list less the items (computed as a difference, as the table of record was)
    whole = p['sole_prop'] + p['txbl_pens_dist'] + p['rent'] + p['rent_loss'] + p['ui'] + p['other_inc']
    df['other_agi'] = whole - sum(ITEMS[v][0] * df[v] for v in ITEMS)
    assert np.abs(df['other_agi'] - p['other_inc']).max() < 0.01
    df = grown(df, year, py)
    for v in SCF_AMOUNTS:
        df[v] = np.nan
    return df


COMPUTED = {'capital_gains': {p: 1 for p in KG_PIECES},
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
        if v in ('capital_gains', 'kg_st', 'kg_lt', 'kg_other'):
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
            assert v in ['wages', 'interest', 'dividends', 'other_agi'] + list(ITEMS), v
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


def tax_data(t, y, pi):
    """The tax data for year y: the PUF filers other than dependent filers, each weight times the year-y over PUF-year
    returns of SOI Table 1.6 in its filing status and age range (a record with no range: averaged over step 0's
    probabilities), dollars grown per return; and year y's non-filer tax units. Columns: weight, the categories,
    age_p0-5 (the age range as probabilities), the PUF amounts."""
    puf = t[(t.source == 'puf') & (t.filer == 1) & (t.dep_filer == 0)]
    P = np.column_stack([(puf.age_range == k).to_numpy(float) + puf[f'age_p{k}'].to_numpy(float)
                         for k in range(common.ADULT_RANGES)])
    assert np.allclose(P.sum(1), 1), 'a PUF filer other than a dependent filer with a dependent-filer age range'
    status = ['single', 'joint', 'separate', 'head_of_household']
    t_y, t_0 = TG.table_1_6(y), TG.table_1_6(PUF_YEAR)
    F = np.array([[t_y[(s, k)] / t_0[(s, k)] for k in range(common.ADULT_RANGES)] for s in status])
    f = (P * F[puf.mars.to_numpy().astype(np.int64) - 1]).sum(1)
    shared = sorted({r['variable'] for r in TR.rows(amounts_only=True)})
    needed = sorted({p for v in shared for p in COMPUTED.get(v, {v: 1})})
    filers = grown(puf, y, columns=needed).assign(weight=puf.weight.to_numpy() * f)
    for k in range(common.ADULT_RANGES):
        filers[f'age_p{k}'] = P[:, k]
    nf = nonfiler_rows(y)
    couple = (nf.married == 1).to_numpy()
    age, _ = ages.primary_age(nf.age1.to_numpy(float), nf.age2.fillna(0).to_numpy(float), nf.male1.to_numpy(float),
                              nf.male2.fillna(1).to_numpy(float), couple, nf.age_key.to_numpy(float), pi)
    rng = ages.adult_range(age)
    for k in range(common.ADULT_RANGES):
        nf[f'age_p{k}'] = (rng == k).astype(float)
    cols = ['weight', 'filer', 'married', 'n_dep', 'k1_owner'] + [f'age_p{k}' for k in range(common.ADULT_RANGES)] \
        + shared
    out = pd.concat([filers[cols], nf[cols]], ignore_index=True)
    common.say(f'tax data {y}: {filers.weight.sum() / 1e6:.2f}M filers (PUF {PUF_YEAR}: {puf.weight.sum() / 1e6:.2f}M), '
               f'{nf.weight.sum() / 1e6:.2f}M non-filers')
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
                       'filer': -1, 'k1_owner': -1, 'owns_business': -1,
                       **survey_items(s, 'cex'),
                       'age1': s['age1'], 'age2': s['age2'], 'male1': s['male1'], 'male2': s['male2'],
                       'age_key': s['household_id'].astype(float)})
    for c in CEX_CATS:
        df[c] = s[c].astype(float)
    for v in AMOUNTS:
        if v not in df:
            df[v] = np.nan
    return df


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
        s = loan_to_value(s.assign(source='scf', filer=-1))
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


# ---- bands --------------------------------------------------------------------------------
def translate_surveys(t, edges, pi):
    """The shared amounts of every survey year from the translation table (translate.py): the definition fixes,
    function 1 against the tax data for the year, the PUF's cut points grown to the year. The report: the survey as
    reported, after function 1, the tax data."""
    w = t['weight'].to_numpy(float)
    report, notes, tax = [], {}, {}
    for src in ('scf', 'cex'):
        for y in sorted(t.loc[t.source == src, 'year'].unique()):
            if y not in tax:
                tax[y] = tax_data(t, y, pi)
            T = tax[y]
            tw = T['weight'].to_numpy(float)
            sel = ((t.source == src) & (t.year == y)).to_numpy()
            d = t[sel]
            for row in TR.rows(src):
                v = row['variable']
                if v in TR.CATEGORY_VARIABLES:
                    tax_side = TR.describe_category(None, tw, T[[f'age_p{k}' for k in range(common.ADULT_RANGES)]].to_numpy()) \
                        if v == 'age_range' else TR.describe_category(T[v].to_numpy(), tw)
                    assert (d[v] >= 0).all(), f'{src} {y} {v}: missing'
                    report += TR.report_rows(row, y, {'survey': TR.describe_category(d[v].to_numpy(), w[sel]),
                                                      'tax data': tax_side})
                    continue
                reported = sum(d[f'raw_{c}'].to_numpy(float) for c in row['items'])
                assert not np.isnan(reported).any(), f'{src} {y} {v}: missing values'
                x = TR.definition_fixes(reported, row)
                fp, fn = growth(v, y)
                line = None if row['dollar_line'] is None else row['dollar_line'] * fp
                new, beyond = TR.amounts(x, w[sel], T[v].to_numpy(float), tw, line)
                b, rec = band_variable(new, w[sel], grown_edge(edges[f'puf/{PUF_YEAR}/{v}'], fp, fn))
                rec['cut_rule'] = (f'PUF {PUF_YEAR} cut points x per-return growth to {y} ({fp:.4f} positive, '
                                   f'{fn:.4f} losses); dollar line {line}')
                t.loc[sel, v], t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = new, b, rec
                notes[f'{src} {y} {v}'] = {'dollar line (survey-year dollars)': line,
                                           'units past the tax data\'s last holder (millions)': beyond / 1e6}
                report += TR.report_rows(row, y, {'as reported': TR.describe(reported, w[sel]),
                                                  'after function 1': TR.describe(new, w[sel]),
                                                  'tax data': TR.describe(T[v].to_numpy(float), tw)})
    rep = pd.DataFrame(report)
    rep.to_csv(f'{common.DATA}/translation_report.csv', index=False)
    TR.say_report(rep)
    common.say('function 1: ' + json.dumps(notes, indent=1, default=float))
    shared = sorted({r['variable'] for r in TR.rows(amounts_only=True)})
    return t, {str(y): {v: list(growth(v, y)) for v in shared} for y in tax}


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
    for src, own, rule in [('scf', SCF_AMOUNTS, 'within the wave'), ('cex', CEX_AMOUNTS, 'within the year')]:
        for y in sorted(t.loc[t.source == src, 'year'].unique()):
            sel = ((t.source == src) & (t.year == y)).to_numpy()
            for v in own:
                b, rec = band_variable(t.loc[sel, v].to_numpy(float), w[sel])
                rec['cut_rule'] = rule
                t.loc[sel, f'{v}_band'], edges[f'{src}/{y}/{v}'] = b, rec
    for v in AMOUNTS:
        b, x = t[f'{v}_band'], t[v]
        assert ((b == -1) == x.isna()).all() and ((b == 0) == (x == 0)).all(), v
        t[f'{v}_band'] = b.astype('int16')
    edges['meta'] = table_meta(t, factors)
    return t, edges


def table_meta(t, factor):
    """What later stages need to know about the table: the items and who has them, the computed totals and
    flags, the surveys' own slots, the AGI signs, net worth's parts."""
    item_order = list(ITEMS) + list(PARTS)
    return {'items': {v: ('shared' if v in ITEMS and ITEMS[v][1] else 'puf') for v in item_order},
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
    # the balance sheet: the wave whose tax year is the PUF year, in its own dollars; for the target year, carried
    # to the target year's wave dollars by the two waves' asset price factors (2022 dollars per wave dollar). The
    # mortgage's share of the home's value is a ratio, not dollars.
    base = t[(t.source == 'scf') & (t.year == PUF_YEAR)].reset_index(drop=True)
    m_scf = build_map(base, edges, 'scf', SCF_AMOUNTS)
    m_scf.to_parquet(f'{common.MAPS}/map_scf_{PUF_YEAR}.parquet', index=False)
    af = {y: t.loc[(t.source == 'scf') & (t.year == y), 'asset_factor'] for y in (PUF_YEAR, TARGET_YEAR - 1)}
    assert all(a.nunique() == 1 for a in af.values())
    f_scf = af[PUF_YEAR].iloc[0] / af[TARGET_YEAR - 1].iloc[0]
    m_scf_target = m_scf.copy()
    dollars = (m_scf_target['variable'] != 'w_primary_ltv').to_numpy()
    m_scf_target.loc[dollars, 'value'] = m_scf_target.loc[dollars, 'value'] * f_scf
    m_scf_target.to_parquet(f'{common.MAPS}/map_scf_{TARGET_YEAR}.parquet', index=False)
    for v in SCF_AMOUNTS:
        g[v] = (1.0, 1.0) if v == 'w_primary_ltv' else (f_scf, f_scf)
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
    target = ages.puf_female_primary_share(puf)
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
    return t.drop(columns=['age1', 'age2', 'male1', 'male2', 'age_key', 'taxunit_code'], errors='ignore'), pi


def age_range_table(t):
    """Weighted shares by age range: PUF filers (step 0's probabilities where no range), non-filers, SCF waves."""
    labels = common.AGE_LABELS
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
    puf = read_puf()
    t = pd.concat([puf_rows(puf), nonfiler_rows(PUF_YEAR), scf_rows(), cex_rows()], ignore_index=True)
    assert not t.duplicated(['source', 'record_id']).any()
    t, pi = primary_age_range(t, puf)
    t, edges = band_table(t, pi)
    keep = ['source', 'year', 'record_id', 'household_id', 'implicate', 'weight', 'married', 'n_dep', 'filer'] + \
        list(CATEGORIES) + AMOUNTS + ['net_worth', 'w_primary_mortgage'] + [f'{v}_band' for v in AMOUNTS] + \
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
