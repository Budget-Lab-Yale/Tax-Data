"""
33_tables.py  (minimal example, step 33: the whole chain with more variables.
Status: 33_STATUS.md; settings: 33_settings_scf.json.)

What it does
  Builds table E33 from table E27 (27_tables.py), new files only:
  - PUF filers: eleven more items from the raw 2015 PUF, in whole dollars.
    Nine are parts of AGI (business_income = Schedules C, E and F, other_gains,
    ira_taxable, pensions_taxable, ss_taxable, unemployment, alimony,
    state_refunds, and adjustments, which is subtracted); two are not
    (retirement_gross, tax_exempt_interest). other_agi becomes what is left:
    E00100 - wages - interest - dividends - the four capital gains pieces -
    the nine parts (adjustments with a minus sign). AGI adds up to the dollar
    on every PUF record (asserted, integers).
  - Crude non-filers (step 23's CPS rows): business_income = sole_prop +
    rent + rent_loss, pensions_taxable = txbl_pens_dist, unemployment = ui, retirement_gross = gross_ss + gross_pens_dist, all in
    2015 dollars with step 23's CPI factor; every other new item zero (the
    CPS pool has no field for it; step 23 took taxable Social Security as
    zero). other_agi becomes other_inc, so their AGI is unchanged (asserted).
  - SCF waves: three items from 33_build_scf.R (business_income = X5704 +
    X5714, retirement_gross X5722, tax_exempt_interest X5706), joined by
    record id; the other eight are PUF-owned (absent).
  Bands: the new items and the new other_agi cut on the PUF filers (as
  27_tables.py cuts its new variables) and applied to the whole PUF side; the
  SCF's four items by the settings sheet's dollar rule ("unchanged": the
  PUF's cut points grown by the IRS growth in the item's total from 2015 to
  the wave's tax year, as E27 does for interest and dividends; "rank": the
  wave's own weighted ranks).
  The model learns which slots exist from the band-edge file's
  meta.step33_items ({item: 'puf' or 'shared'}), read by 02_model.py.

Writes
  data/stacked_E33.parquet, data/band_edges_E33.json
  map/V33/map_puf_2015.parquet, map_puf_2019.parquet, map_scf_2015.parquet,
          growth_factors_33.csv
  step33/tables_checks.md  (AGI identity, items against SOI 2015, NA counts,
                            SCF holders as reported against the PUF side)
Usage (Slurm): sbatch research/rebuild/minimal/33_cpu.sbatch tables
"""
import ast
import copy
import json
import os
import re

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
D = f'{R}/data'
OUT = f'{R}/step33'
MAPS = f'{R}/map/V33'
MAPS27 = f'{R}/map/V27'
PUF_RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
SOI = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/bysize_income_sources.csv'
POOL = ('/nfs/roberts/project/pi_nrs36/shared/model_data/ASEC-Nonfilers/v1/2026091201/'
        'historical/nonfiler_pool_2017.csv.gz')
CPI_2015, CPI_2017 = 237.017, 245.120
NF_OFFSET = 900_000_000
SETTINGS = json.load(open(f'{HERE}/33_settings_scf.json'))
ITEMS = SETTINGS['step33_items']
# sign of each item in AGI (0: not part of AGI)
AGI_SIGN = {'business_income': 1, 'other_gains': 1, 'ira_taxable': 1,
            'pensions_taxable': 1, 'ss_taxable': 1, 'unemployment': 1, 'alimony': 1, 'state_refunds': 1,
            'adjustments': -1, 'retirement_gross': 0, 'tax_exempt_interest': 0}
NEW = list(AGI_SIGN)
assert set(NEW) == set(ITEMS), set(NEW) ^ set(ITEMS)
SHARED_NEW = [v for v in NEW if not ITEMS[v]['absent_on_scf']]
ADJ_FIELDS = ['E03150', 'E03210', 'E03220', 'E03230', 'E03240', 'E03260', 'E03270', 'E03290',
              'E03300', 'E03400', 'E03500']
RAW_FIELDS = ['RECID', 'E00100', 'E00200', 'E00300', 'E00400', 'E00600', 'E00700', 'E00800', 'E00900',
              'E01000', 'E01100', 'E01200', 'E01400', 'E01500', 'E01700', 'E02000', 'E02100', 'E02300',
              'E02400', 'E02500', 'E26270'] + ADJ_FIELDS
PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']

_f = f'{HERE}/01_band.py'
_tree = ast.parse(open(_f).read())
_stop = next(i for i, n in enumerate(_tree.body) if 'read_csv' in ast.unparse(n))
_keep = [n for n in _tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
         or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
             and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=_keep, type_ignores=[]), _f, 'exec'), B)
band_variable, wquantile, N_BANDS = B['band_variable'], B['wquantile'], B['N_BANDS']
LOG = []


def say(*a):
    s = ' '.join(str(x) for x in a)
    print(s, flush=True)
    LOG.append(s)


# ---------------------------------------------------------------- SOI

def soi_table(years):
    t = pd.read_csv(SOI, dtype=str)
    t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')
          & t['tax_year'].isin([str(y) for y in years])]
    t = t.assign(item=t['item'].str.lower().str.strip())
    return {(int(r.tax_year), r.item): float(r.value) for r in t.itertuples()}


def soi_get(val, y, patterns, stat):
    """Value of the one item matching a pattern in year y; amounts in dollars."""
    names = {k[1] for k in val if k[0] == y}
    for p in patterns:
        hit = [n for n in names if re.fullmatch(p + ' > ' + stat, n)]
        if len(hit) == 1:
            v = val[(y, hit[0])]
            return v * 1000.0 if stat == 'amount' else v
        assert len(hit) <= 1, (y, p, hit)
    raise KeyError(f'{y}: no SOI item for {patterns} > {stat}')


NET = lambda name: [(name + ' > net income', 1), (name + r' > net loss.*', -1)]
SOI_SPEC = {  # item: list of (pattern, sign); counts add the matched rows
    'business_income': NET('business or profession') + NET('partnership and s corporation') + NET('farm')
    + NET('rent') + NET('royalty') + NET('estate and trust') + NET('farm rental'),
    'other_gains': [('sales of property other than capital assets > net gain', 1),
                    ('sales of property other than capital assets > net loss', -1)],
    'ira_taxable': [(r'(taxable ira distributions|taxable individual retirement arrangement \(ira\) distributions'
                     r'|individual retirement arrangement.*taxable.*)', 1)],
    'pensions_taxable': [('pensions and annuities > taxable', 1)],
    'ss_taxable': [('social security benefits > taxable', 1)],
    'unemployment': [('unemployment compensation', 1)],
    'alimony': [('alimony received', 1)],
    'state_refunds': [('state income tax refunds', 1)],
    'adjustments': [('statutory adjustments > total', 1)],
    'retirement_gross': [('social security benefits > total', 1), ('pensions and annuities > total', 1)],
    'tax_exempt_interest': [('tax exempt interest', 1)],
    'other_agi': NET('other income'),
}


def soi_item(val, y, item, base=None):
    """Amount and count of an item in year y. base: a second year; then only the
    component rows present in both years are summed (logged), so a ratio of
    the two compares like with like."""
    amt = cnt = 0.0
    for p, s in SOI_SPEC[item]:
        try:
            a, c = soi_get(val, y, [p], 'amount'), soi_get(val, y, [p], 'number of returns')
            if base is not None:
                soi_get(val, base, [p], 'amount')
        except KeyError as ex:
            say(f'SOI: {item}: component {p!r} missing ({ex}); left out of the {y} figure')
            continue
        amt, cnt = amt + s * a, cnt + c
    return amt, cnt


# ---------------------------------------------------------------- banding helpers

def fixed_edge(puf_edge, factor):
    rec = copy.deepcopy({k: puf_edge[k] for k in ['negative', 'positive'] if k in puf_edge})
    if 'positive' in rec and factor != 1.0:
        rec['positive']['cuts'] = (np.array(rec['positive']['cuts']) * factor).tolist()
    rec['n_bands'] = N_BANDS
    return rec


def grow_values(variable, x, factor):
    x = np.array(x, dtype=float)
    x[x != 0] *= factor
    return x


def wstats(x, w):
    x, w = np.asarray(x, float), np.asarray(w, float)
    nz = x != 0
    out = {'share nonzero (%)': 100 * w[nz].sum() / w.sum(), 'total ($ billion)': (w * x).sum() / 1e9}
    if nz.any():
        q = wquantile(x[nz], w[nz], [0.5, 0.9, 0.99])
        out.update({'P50 of nonzero': q[0], 'P90 of nonzero': q[1], 'P99 of nonzero': q[2]})
    return out


def main():
    os.makedirs(OUT, exist_ok=True)
    os.makedirs(MAPS, exist_ok=True)
    t = pd.read_parquet(f'{D}/stacked_E27.parquet')
    edges = json.load(open(f'{D}/band_edges_E27.json'))
    say('E27 rows by source and year:', t.groupby(['source', 'year']).size().to_dict())
    years_scf = sorted(int(y) for y in t.loc[t.source == 'scf', 'year'].unique())
    val = soi_table(sorted(set(years_scf) | {2015, 2019}))

    # ---- PUF filers
    filers = ((t.source == 'puf') & (t.filer == 1)).to_numpy()
    nfr = ((t.source == 'puf') & (t.filer == 0)).to_numpy()
    scf = (t.source == 'scf').to_numpy()
    raw = pd.read_csv(PUF_RAW, usecols=RAW_FIELDS)
    raw = raw[raw.RECID < 999996]
    na = {c: int(raw[c].isna().sum()) for c in raw.columns}
    say('raw PUF fields, missing values per field:', na)
    assert sum(na.values()) == 0, 'missing values in the raw PUF fields'
    I = {c: raw[c].round().astype(np.int64).to_numpy() for c in RAW_FIELDS if c != 'RECID'}
    for c in I:
        assert np.array_equal(I[c], raw[c].to_numpy()), f'{c}: not whole dollars'
    neg_adj = {c: int((I[c] < 0).sum()) for c in ADJ_FIELDS}
    say('adjustment fields below zero (records):', neg_adj)
    item = {'business_income': I['E00900'] + I['E02000'] + I['E02100'],
            'other_gains': I['E01200'], 'ira_taxable': I['E01400'], 'pensions_taxable': I['E01700'],
            'ss_taxable': I['E02500'], 'unemployment': I['E02300'], 'alimony': I['E00800'],
            'state_refunds': I['E00700'], 'adjustments': sum(I[c] for c in ADJ_FIELDS),
            'retirement_gross': I['E02400'] + I['E01500'], 'tax_exempt_interest': I['E00400']}
    pf = pd.DataFrame({'record_id': raw['RECID'].astype(float).to_numpy(), **item})
    # align to E27's filer rows
    rid = t.loc[filers, 'record_id'].to_numpy()
    pf = pf.set_index('record_id').loc[rid]
    assert len(pf) == filers.sum()
    old_other = t.loc[filers, 'other_agi'].to_numpy()
    assert np.array_equal(np.round(old_other), old_other), 'E27 other_agi not whole dollars'
    old_other = old_other.astype(np.int64)
    folded = sum(AGI_SIGN[v] * pf[v].to_numpy() for v in NEW)
    new_other = old_other - folded
    e00100 = raw.set_index(raw.RECID.astype(float)).loc[rid, 'E00100'].round().astype(np.int64).to_numpy()
    parts = sum(t.loc[filers, v].round().astype(np.int64).to_numpy() for v in ['wages', 'interest', 'dividends'] + PIECES)
    agi = parts + folded + new_other
    bad = int((agi != e00100).sum())
    say(f'AGI identity on {len(agi):,} PUF filer records, in whole dollars: {bad} records off')
    assert bad == 0
    for v in NEW:
        t[v] = np.nan
        t.loc[filers, v] = pf[v].to_numpy().astype(float)
    t.loc[filers, 'other_agi'] = new_other.astype(float)
    t['other_agi_e27'] = np.where(filers | nfr, t['other_agi'], np.nan)
    t.loc[filers, 'other_agi_e27'] = old_other.astype(float)

    # ---- non-filers
    p = pd.read_csv(POOL)
    p['record_id'] = (NF_OFFSET + p['id'].astype(np.int64)).astype(float)
    p = p.set_index('record_id').loc[t.loc[nfr, 'record_id'].to_numpy()]
    dfl = CPI_2015 / CPI_2017
    nfcols = ['sole_prop', 'txbl_pens_dist', 'rent', 'rent_loss', 'ui', 'gross_ss', 'gross_pens_dist', 'other_inc']
    say('non-filer pool fields, missing values:', p[nfcols].isna().sum().to_dict())
    assert not p[nfcols].isna().any().any()
    nf_items = {'business_income': (p['sole_prop'] + p['rent'] + p['rent_loss']) * dfl,
                'pensions_taxable': p['txbl_pens_dist'] * dfl, 'unemployment': p['ui'] * dfl,
                'retirement_gross': (p['gross_ss'] + p['gross_pens_dist']) * dfl}
    for v in NEW:
        t.loc[nfr, v] = nf_items[v].to_numpy() if v in nf_items else 0.0
    nf_old = t.loc[nfr, 'other_agi'].to_numpy()
    nf_new = nf_old - sum(AGI_SIGN[v] * t.loc[nfr, v].to_numpy() for v in NEW)
    gap = np.abs(nf_new - p['other_inc'].to_numpy() * dfl).max()
    say(f'non-filers: remainder against other_inc in 2015 dollars, largest gap ${gap:.6f}')
    assert gap < 0.01
    t.loc[nfr, 'other_agi'] = nf_new

    # ---- SCF items
    for v in SHARED_NEW:
        t[v + '_reported'] = np.nan
    for y in years_scf:
        wave = y + 1
        s = pd.read_csv(f'{D}/scf{wave}_units33.csv.gz')
        sel = scf & (t.year == y).to_numpy()
        ids = t.loc[sel, 'record_id'].to_numpy()
        s = s.set_index('record_id')
        # table Y1 offset the ids of tax years 2018 and 2021 by 1e8 x (year - 2015)
        # (01_band.py --years); 27_tables.py offset the 2013 wave's by 2.013e12
        base_index = s.index
        for off in [0.0, 1e8 * (y - 2015), 2.013e12]:
            s.index = base_index + off
            if np.isin(ids, s.index).all():
                say(f'wave {wave}: record ids offset by {off:,.0f}')
                break
        assert np.isin(ids, s.index).all(), f'wave {wave}: E27 rows missing from scf{wave}_units33'
        s = s.loc[ids]
        dw = np.abs(s['wages'].to_numpy() - t.loc[sel, 'wages'].to_numpy()).max()
        dk = np.abs(s['capital_gains'].to_numpy() - t.loc[sel, 'capital_gains_reported'].to_numpy()).max()
        say(f'wave {wave}: {sel.sum()} rows; wages against E27 largest gap ${dw:.4f}; '
            f'capital gains against E27 as reported ${dk:.4f}')
        assert dw < 0.01 and dk < 0.01
        # business income on the SCF: X5704 (business, practice, partnership, farm) plus X5714
        # (net rent, trusts, royalties), the concept of Schedules C, E and F together
        s = s.assign(business_income=s['business_income'] + s['rent_royalty_estate'])
        for v in SHARED_NEW:
            t.loc[sel, v] = s[v].to_numpy()
            t.loc[sel, v + '_reported'] = s[v].to_numpy()

    # ---- bands
    pside = (t.source == 'puf').to_numpy()
    w_all = t['weight'].to_numpy(float)
    for v in NEW + ['other_agi']:
        b, rec = band_variable(t.loc[filers, v].to_numpy(float), w_all[filers])
        edges[f'puf/2015/{v}'] = rec
        b2, _ = band_variable(t.loc[pside, v].to_numpy(float), w_all[pside], fixed_edge(rec, 1.0))
        t[f'{v}_band'] = np.int16(-1) if v != 'other_agi' else t['other_agi_band']
        t.loc[pside, f'{v}_band'] = b2
        assert np.array_equal(b2[t.loc[pside, 'filer'].to_numpy() == 1], b), v
    factors_scf = {}
    for v in SHARED_NEW:
        rule = ITEMS[v]['dollar_rule']
        for y in years_scf:
            a15, _ = soi_item(val, 2015, v, base=y)
            sel = scf & (t.year == y).to_numpy()
            x = t.loc[sel, v].to_numpy(float)
            if rule == 'unchanged' or isinstance(rule, (int, float)):
                f = soi_item(val, y, v, base=2015)[0] / a15
                xx = x * (float(rule) if isinstance(rule, (int, float)) else 1.0)
                b, rec = band_variable(xx, w_all[sel], fixed_edge(edges[f'puf/2015/{v}'], f))
                rec['cut_rule'] = f'PUF 2015 positive cuts x SOI factor {f:.4f}' + \
                    (f'; SCF dollars x {rule} first' if isinstance(rule, (int, float)) else '')
                factors_scf[f'{v} {y}'] = round(f, 4)
            elif rule == 'rank':
                b, rec = band_variable(x, w_all[sel])
                rec['cut_rule'] = 'rank: the wave\'s own weighted ranks at the PUF\'s band shares'
            else:
                raise ValueError(f'{v}: unknown dollar rule {rule!r}')
            t.loc[sel, f'{v}_band'] = b
            edges[f'scf/{y}/{v}'] = rec
    say('SOI growth factors for the SCF cut points (total amount, 2015 = 1):', factors_scf)

    # ---- checks
    amounts = ['wages', 'interest', 'dividends', 'capital_gains'] + PIECES + NEW + ['other_agi', 'net_worth']
    for v in amounts:
        b, x = t[f'{v}_band'], t[v]
        assert ((b == -1) == x.isna()).all(), v
        assert ((b == 0) == (x == 0)).all(), v
        assert b.between(-1, N_BANDS - 1).all(), v
    for v in NEW:
        assert (t.loc[pside, f'{v}_band'] >= 0).all(), v
        own = ITEMS[v]['absent_on_scf']
        assert ((t.loc[scf, f'{v}_band'] == -1).all() if own else (t.loc[scf, f'{v}_band'] >= 0).all()), v
    absent_ok = PIECES + ['other_agi', 'net_worth', 'capital_gains_reported', 'other_agi_e27'] + \
        [v for v in NEW if ITEMS[v]['absent_on_scf']] + [v + '_reported' for v in SHARED_NEW]
    na = t.drop(columns=absent_ok).isna().sum()
    say('missing values outside the absent slots:', int(na.sum()), na[na > 0].to_dict())
    assert na.sum() == 0
    for v in NEW:
        t[f'{v}_band'] = t[f'{v}_band'].astype('int16')
    t['other_agi_band'] = t['other_agi_band'].astype('int16')
    t.to_parquet(f'{D}/stacked_E33.parquet', index=False)
    edges['meta'] = dict(edges.get('meta', {}), E33='step 33 (33_tables.py): E27 plus eleven PUF items; '
                         'other_agi is the remainder after them',
                         step33_items={v: ('puf' if ITEMS[v]['absent_on_scf'] else 'shared') for v in NEW})
    json.dump(edges, open(f'{D}/band_edges_E33.json', 'w'), indent=1)
    say('written', f'{D}/stacked_E33.parquet', len(t), 'rows')

    # ---- maps
    import importlib.util
    spec = importlib.util.spec_from_file_location('dollars', f'{HERE}/04_dollars.py')
    dollars = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(dollars)
    keep = dollars.AMOUNTS
    old = ['wages', 'interest', 'dividends', 'capital_gains'] + PIECES
    try:
        dollars.AMOUNTS = old + ['other_agi'] + NEW
        mp = dollars.build_map(t[pside].reset_index(drop=True), edges, 'puf')
    finally:
        dollars.AMOUNTS = keep
    m27 = pd.read_parquet(f'{MAPS27}/map_puf_2015.parquet')
    a = mp[mp.variable.isin(old)].reset_index(drop=True)
    b = m27[m27.variable.isin(old)].reset_index(drop=True)
    assert len(a) == len(b) and all(np.array_equal(a[c].to_numpy(), b[c].to_numpy())
                                     for c in ['variable', 'band', 'value', 'weight']), \
        'unchanged variables: the 2015 map differs from V27'
    say('2015 map rows of wages, interest, dividends, capital gains and the pieces equal V27: checked')
    mp.to_parquet(f'{MAPS}/map_puf_2015.parquet', index=False)
    pd.read_parquet(f'{MAPS27}/map_scf_2015.parquet').to_parquet(f'{MAPS}/map_scf_2015.parquet', index=False)
    g27 = pd.read_parquet(f'{MAPS27}/map_puf_2019.parquet')
    rows = [g27[g27.variable.isin(old)]]
    fac = []
    for v in NEW + ['other_agi']:
        (a15, c15), (a19, c19) = soi_item(val, 2015, v, base=2019), soi_item(val, 2019, v, base=2015)
        f = (a19 / c19) / (a15 / c15)
        fac.append({'variable': v, 'amount_2015': a15, 'returns_2015': c15, 'amount_2019': a19,
                    'returns_2019': c19, 'factor (per return with the item)': f,
                    'note': 'other_agi (the remainder) grows with the IRS "other income" item' if v == 'other_agi' else ''})
        g = mp[mp.variable == v].copy()
        g['value'] = grow_values(v, g['value'].to_numpy(), f)
        rows.append(g)
    g19 = pd.concat(rows, ignore_index=True).sort_values(['variable', 'band', 'value'], kind='stable')
    g19.to_parquet(f'{MAPS}/map_puf_2019.parquet', index=False)
    fac = pd.DataFrame(fac)
    fac.to_csv(f'{MAPS}/growth_factors_33.csv', index=False)
    say(fac.to_string())

    # ---- checks against SOI 2015 and SCF as reported
    w = w_all[filers]
    soi_rows = []
    for v in NEW:
        a15, c15 = soi_item(val, 2015, v)
        x = t.loc[filers, v].to_numpy(float)
        soi_rows.append({'item': v, 'PUF returns with it (millions)': w[x != 0].sum() / 1e6,
                         'IRS returns, sum of the item rows (millions)': c15 / 1e6,
                         'PUF amount ($ billion)': (w * x).sum() / 1e9, 'IRS amount ($ billion)': a15 / 1e9})
    soi_check = pd.DataFrame(soi_rows)
    say(soi_check.to_string())
    rows = []
    for v in SHARED_NEW:
        ps = t[pside]
        r = {'item': v, 'side': 'PUF side (filers + crude non-filers), 2015'}
        r.update(wstats(ps[v], ps['weight']))
        rows.append(r)
        for y in years_scf:
            s = t[scf & (t.year == y).to_numpy()]
            r = {'item': v, 'side': f'SCF tax year {y}, as reported (tax units)'}
            r.update(wstats(s[v], s['weight']))
            rows.append(r)
    holders = pd.DataFrame(rows)
    say(holders.to_string())
    nf_tab = pd.DataFrame([{'item': v, 'non-filers with it (millions)': w_all[nfr][t.loc[nfr, v].to_numpy() != 0].sum() / 1e6,
                            'amount ($ billion)': (w_all[nfr] * t.loc[nfr, v].to_numpy()).sum() / 1e9} for v in NEW])
    md = ['# Step 33 tables: checks', '', '## Log', '', '```', *LOG, '```', '',
          '## New items on PUF filers against SOI 2015 (Table 1.4)', '',
          'IRS returns are the sum of the rows that make up the item (e.g. returns with a Schedule C profit plus '
          'returns with a Schedule C loss plus the partnership and farm rows), so a return with two of them counts twice.', '',
          soi_check.to_markdown(index=False, floatfmt='.2f'), '',
          '## Crude non-filers: the new items', '', nf_tab.to_markdown(index=False, floatfmt='.2f'), '',
          '## Shared items: PUF side against the SCF as reported', '',
          holders.to_markdown(index=False, floatfmt=',.1f'), '']
    open(f'{OUT}/tables_checks.md', 'w').write('\n'.join(md) + '\n')
    open(f'{OUT}/TABLES_DONE', 'w').close()


if __name__ == '__main__':
    main()
