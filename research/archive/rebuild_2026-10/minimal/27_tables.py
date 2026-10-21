"""
27_tables.py  (minimal example, step 27: the tables; 27_MAPPING_SPEC.md and
John's sign-off of 17 October 2026. Results: 27_RESULTS.md.)

What it does
  Builds table E27 and its dollar maps, new files only:
  - The 2015 PUF with capital gains in four pieces (John's sign-off):
      kg_st    = P22250 (net short-term gain or loss, Schedule D)
      kg_lt    = P23250 (net long-term gain or loss, Schedule D)
      kg_dist  = E01100 (capital gain distributions reported without a Schedule D)
      kg_other = E01000 - P22250 - P23250 (the residual: the loss limit,
                 carryovers not in the two lines, and the unexplained conflict)
    capital_gains is the Form 1040 line-13 total E01000 + E01100 (computed
    from the pieces at generation, never drawn); Form 4797 (E01200) stays in
    other income; other_agi = E00100 - wages - interest - dividends - the
    four pieces. AGI adds up to the cent on every PUF record (asserted, in
    whole dollars as integers).
  - The crude 2017 CPS non-filers of step 23 (table E4N's rows), pieces zero.
  - The SCF waves 2016, 2019, 2022 (table Y1's rows, tax years 2015, 2018,
    2021) and 2013 (tax year 2012, data/scf2013_units.csv.gz from
    27_scf2013.sbatch) if it was built; otherwise three waves (written in
    the checks).
  Bands: wages, interest, dividends and net worth exactly as table Y1 (the
  PUF's 2015 cut points with the positive cuts grown by the SOI growth in
  each item; net worth cut within each wave); the 2012 wave the same way.
  capital_gains (the new line-13 total), the four pieces and other_agi (new
  definition) are cut anew on the PUF filers, as 01_band.py cuts; the SCF
  waves' capital gains on the PUF cuts grown by the SOI line-13 growth.

Writes
  data/stacked_E27.parquet, data/band_edges_E27.json
  map/V27/map_puf_2015.parquet, map_puf_2019.parquet, map_scf_2015.parquet
  step27/tables_checks.md   the checks (AGI identity, pieces against SOI 2015,
                            the E01000 conflict, NA counts, waves)
  step27/irs_counts.csv     SOI Table 1.4 returns with interest, dividends,
                            capital gains (and amounts), tax years 2012-2021
  step27/kg_mix.json        the capital gains basis mix, closed form (see 27_RESULTS.md)
Usage (Slurm): sbatch research/rebuild/minimal/27_cpu.sbatch tables
"""
import ast
import copy
import json
import os

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
D = f'{R}/data'
OUT = f'{R}/step27'
MAPS = f'{R}/map/V27'
PUF_RAW = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv'
SOI = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/bysize_income_sources.csv'
TAG = 'E27'
PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']
SHARED = ['wages', 'interest', 'dividends', 'capital_gains']
AMOUNTS = SHARED + PIECES + ['other_agi', 'net_worth']
NF_OFFSET = 900_000_000

# 01_band.py's constants and functions only (as 10_years_tables.py loads them)
_f = f'{HERE}/01_band.py'
_tree = ast.parse(open(_f).read())
_stop = next(i for i, n in enumerate(_tree.body) if 'read_csv' in ast.unparse(n))
_keep = [n for n in _tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
         or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
             and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=_keep, type_ignores=[]), _f, 'exec'), B)
band_variable, wquantile, N_BANDS, POS0, NEG0 = (B[k] for k in
                                                 ['band_variable', 'wquantile', 'N_BANDS', 'POS0', 'NEG0'])
LOG = []


def say(*a):
    s = ' '.join(str(x) for x in a)
    print(s, flush=True)
    LOG.append(s)


# ---------------------------------------------------------------- SOI

SOI_ITEMS = {'number of returns': ('returns', 'count'),
             'salaries and wages > number of returns': ('wages', 'count'),
             'salaries and wages > amount': ('wages', 'amount'),
             'taxable interest > number of returns': ('interest', 'count'),
             'taxable interest > amount': ('interest', 'amount'),
             'ordinary dividends > number of returns': ('dividends', 'count'),
             'ordinary dividends > amount': ('dividends', 'amount'),
             'capital gain distributions reported on form 1040 > number of returns': ('kg_dist', 'count'),
             'capital gain distributions reported on form 1040 > amount': ('kg_dist', 'amount'),
             'sales of capital assets reported on form 1040, schedule d > taxable net gain > number of returns': ('sch_d_gain', 'count'),
             'sales of capital assets reported on form 1040, schedule d > taxable net gain > amount': ('sch_d_gain', 'amount'),
             'sales of capital assets reported on form 1040, schedule d > taxable net loss > number of returns': ('sch_d_loss', 'count'),
             'sales of capital assets reported on form 1040, schedule d > taxable net loss > amount': ('sch_d_loss', 'amount')}


def soi_counts(years):
    """All-returns totals of Table 1.4 (counts in returns, amounts in dollars)."""
    t = pd.read_csv(SOI, dtype=str)
    t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')
          & t['tax_year'].isin([str(y) for y in years]) & t['item'].isin(SOI_ITEMS)]
    rows = []
    for r in t.itertuples():
        item, stat = SOI_ITEMS[r.item]
        v = float(r.value) * (1000.0 if stat == 'amount' else 1.0)   # amounts are in thousands
        rows.append({'year': int(r.tax_year), 'item': item, 'statistic': stat, 'value': v})
    out = pd.DataFrame(rows).drop_duplicates(['year', 'item', 'statistic'])
    assert len(out) == len(years) * len(SOI_ITEMS), f'SOI rows missing: {len(out)}'
    p = out.pivot_table(index=['year', 'statistic'], columns='item', values='value').reset_index()
    # capital gains on the return (line 13): Schedule D gain or loss, or distributions
    p['capital_gains'] = np.where(p['statistic'] == 'count',
                                  p['sch_d_gain'] + p['sch_d_loss'] + p['kg_dist'],
                                  p['sch_d_gain'] - p['sch_d_loss'] + p['kg_dist'])
    p['sch_d_net'] = np.where(p['statistic'] == 'count', p['sch_d_gain'] + p['sch_d_loss'],
                              p['sch_d_gain'] - p['sch_d_loss'])
    return p


# ---------------------------------------------------------------- PUF pieces

def puf_pieces(puf):
    raw = pd.read_csv(PUF_RAW, usecols=['RECID', 'E00100', 'E00200', 'E00300', 'E00600', 'E01000',
                                        'E01100', 'E01200', 'P22250', 'P23250', 'MARS'])
    raw = raw[raw.RECID < 999996]
    na = {c: int(raw[c].isna().sum()) for c in raw.columns}
    say('raw PUF fields read, missing values per field:', na)
    assert sum(na.values()) == 0, 'missing values in the raw PUF fields'
    j = puf[['record_id', 'weight', 'wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].merge(
        raw.assign(record_id=raw.RECID.astype(float)), on='record_id', how='left', validate='one_to_one')
    assert len(j) == len(puf) and j.RECID.notna().all()
    for a, b in [('wages', 'E00200'), ('interest', 'E00300'), ('dividends', 'E00600'), ('capital_gains', 'E01000')]:
        assert np.allclose(j[a], j[b]), f'{a} on table Y1 is not {b}'
    I = {c: j[c].round().astype(np.int64).to_numpy() for c in
         ['E00100', 'E00200', 'E00300', 'E00600', 'E01000', 'E01100', 'E01200', 'P22250', 'P23250']}
    for c in I:
        assert np.array_equal(I[c], j[c].to_numpy()), f'{c}: not whole dollars'
    kg = {'kg_st': I['P22250'], 'kg_lt': I['P23250'], 'kg_dist': I['E01100'],
          'kg_other': I['E01000'] - I['P22250'] - I['P23250']}
    line13 = kg['kg_st'] + kg['kg_lt'] + kg['kg_dist'] + kg['kg_other']
    assert np.array_equal(line13, I['E01000'] + I['E01100'])
    other = I['E00100'] - I['E00200'] - I['E00300'] - I['E00600'] - line13
    agi = I['E00200'] + I['E00300'] + I['E00600'] + sum(kg.values()) + other
    bad = int((agi != I['E00100']).sum())
    say(f'AGI identity on {len(agi):,} PUF records, in whole dollars: {bad} records off')
    assert bad == 0
    out = puf.copy()
    for k, v in kg.items():
        out[k] = v.astype(float)
    out['capital_gains'] = line13.astype(float)
    out['other_agi'] = other.astype(float)
    # the E01000 conflict (28_WAGES_CG.md): E01000 against Schedule D lines 7 + 15 after the limit
    w = j['weight'].to_numpy(float)
    sd = I['P22250'] + I['P23250']
    lim = np.where(j['MARS'].to_numpy() == 3, -1500, -3000)
    after = np.maximum(sd, lim)
    e = I['E01000']
    WM = lambda msk: round(float(w[msk].sum() / 1e6), 3)
    exact = e == after
    near = np.abs(e - after) <= 1
    conf = ~near
    diag = {
        'returns, millions': WM(np.ones(len(e), bool)),
        'E01000 equals Schedule D lines 7 + 15 after the loss limit (MFS: $1,500), millions': WM(exact),
        'within $1, millions': WM(near & ~exact),
        'in conflict (more than $1 apart), millions': WM(conf),
        'conflict: Schedule D lines zero, E01000 nonzero, millions': WM(conf & (sd == 0)),
        'conflict: E01000 zero, Schedule D lines nonzero, millions': WM(conf & (e == 0) & (sd != 0)),
        'conflict: both nonzero, same sign, millions': WM(conf & (e != 0) & (sd != 0) & (np.sign(e) == np.sign(sd))),
        'conflict: both nonzero, opposite signs, millions': WM(conf & (e != 0) & (sd != 0) & (np.sign(e) != np.sign(sd))),
        'conflict: with E01100 nonzero, millions': WM(conf & (I['E01100'] != 0)),
        'conflict: E01000 at the loss limit, millions': WM(conf & (e == lim)),
        'kg_other nonzero, millions': WM(kg['kg_other'] != 0),
        'kg_other from the loss limit alone (Schedule D below the limit, E01000 at it), millions':
            WM((sd < lim) & (e == lim)),
        'kg_other, weighted total ($ billion)': round(float((w * kg['kg_other']).sum() / 1e9), 2),
        'kg_other on loss-limit returns ($ billion)': round(float((w * kg['kg_other'])[(sd < lim) & (e == lim)].sum() / 1e9), 2),
        'kg_other on conflict returns ($ billion)': round(float((w * kg['kg_other'])[conf].sum() / 1e9), 2),
        'conflicts: weighted E01000 ($ billion)': round(float((w * e)[conf].sum() / 1e9), 2),
        'conflicts: weighted Schedule D after the limit ($ billion)': round(float((w * after)[conf].sum() / 1e9), 2),
    }
    say('E01000 against the Schedule D lines:')
    for k, v in diag.items():
        say(f'  {k}: {v}')
    return out, diag


# ---------------------------------------------------------------- banding

def fixed_edge(puf_edge, factor):
    rec = copy.deepcopy({k: puf_edge[k] for k in ['negative', 'positive'] if k in puf_edge})
    if 'positive' in rec and factor != 1.0:
        rec['positive']['cuts'] = (np.array(rec['positive']['cuts']) * factor).tolist()
    rec['n_bands'] = N_BANDS
    return rec


def band_with(x, w, edge):
    b, rec = band_variable(x, w, fixed=edge)
    return b, rec


def main():
    os.makedirs(OUT, exist_ok=True)
    os.makedirs(MAPS, exist_ok=True)
    y1 = pd.read_parquet(f'{D}/stacked_Y1.parquet')
    e_y1 = json.load(open(f'{D}/band_edges_Y1.json'))
    e4n = pd.read_parquet(f'{D}/stacked_E4N.parquet')
    say('Y1 rows by source and year:', y1.groupby(['source', 'year']).size().to_dict())
    # ---- waves
    wave13 = f'{D}/scf2013_units.csv.gz'
    have13 = os.path.exists(wave13)
    years_scf = ([2012] if have13 else []) + [2015, 2018, 2021]
    say('2013 SCF wave built:', have13, '; SCF tax years in E27:', years_scf)
    soi = soi_counts([2012, 2015, 2018, 2019, 2021])
    soi.to_csv(f'{OUT}/irs_counts.csv', index=False)
    amt = soi[soi.statistic == 'amount'].set_index('year')
    factor = {(v, y): float(amt.loc[y, v] / amt.loc[2015, v]) for v in SHARED for y in years_scf}
    gy = pd.read_csv(f'{D}/growth_factors_years.csv')
    for r in gy.itertuples():
        if r.variable in ('wages', 'interest', 'dividends'):
            assert abs(factor[(r.variable, r.year)] - r.soi_factor) < 1e-9, (r.variable, r.year)
    say('SOI growth factors used for the SCF cut points:', {f'{k[0]} {k[1]}': round(v, 4) for k, v in factor.items()})

    # ---- PUF filers with pieces
    puf = y1[y1.source == 'puf'].reset_index(drop=True)
    puf, conflict = puf_pieces(puf)
    puf['filer'] = 1
    # ---- non-filers: E4N's pool rows
    nf = e4n[e4n.record_id >= NF_OFFSET].reset_index(drop=True)
    assert (nf.source == 'puf').all() and (nf.filer == 0).all() and (nf.capital_gains == 0).all()
    for p in PIECES:
        nf[p] = 0.0
    # ---- SCF rows
    scf = y1[y1.source == 'scf'].reset_index(drop=True)
    if have13:
        s13 = pd.read_csv(wave13)
        say('2013 wave: rows', len(s13), 'households', s13.household_id.nunique(),
            'weighted units (millions)', round(s13.weight.sum() / 1e6, 2))
        na13 = s13.drop(columns=['other_agi']).isna().sum()
        say('2013 wave missing values (other_agi excluded, PUF-owned):', na13[na13 > 0].to_dict())
        assert not na13.any(), 'NA in the 2013 wave'
        assert (s13.year == 2012).all()
        clash = set(s13.household_id) & set(scf.household_id) or set(s13.record_id) & set(scf.record_id)
        if clash:
            say(f'2013 ids overlap other waves ({len(clash)}): offset by 2,013,000,000,000')
            s13['record_id'] += 2.013e12
            s13['household_id'] += 2.013e12
        s13['split'] = 'train'
        scf = pd.concat([scf, s13[[c for c in s13.columns if c in scf.columns]]], ignore_index=True)
    for p in PIECES:
        scf[p] = np.nan
    scf['other_agi'] = np.nan
    scf['filer'] = -1
    scf['capital_gains_reported'] = scf['capital_gains']
    puf['capital_gains_reported'] = np.nan
    nf['capital_gains_reported'] = np.nan
    base_cols = ['source', 'year', 'record_id', 'household_id', 'implicate', 'split', 'weight', 'married',
                 'n_dep', 'filer'] + AMOUNTS + ['capital_gains_reported']
    t = pd.concat([puf[base_cols], nf[base_cols], scf[base_cols]], ignore_index=True)
    assert not t.duplicated(['source', 'record_id']).any()

    # ---- bands
    edges = {}
    filers = (t.source == 'puf') & (t.filer == 1)
    pside = t.source == 'puf'
    w_all = t['weight'].to_numpy(float)
    for v in AMOUNTS:
        t[f'{v}_band'] = np.int16(-1)
    for v in ['wages', 'interest', 'dividends']:
        pe = e_y1[f'puf/2015/{v}']
        b, rec = band_with(t.loc[pside, v].to_numpy(float), w_all[pside], fixed_edge(pe, 1.0))
        t.loc[pside, f'{v}_band'] = b
        edges[f'puf/2015/{v}'] = dict(pe)
        for y in years_scf:
            sel = (t.source == 'scf') & (t.year == y)
            ed = fixed_edge(pe, factor[(v, y)])
            b, rec = band_with(t.loc[sel, v].to_numpy(float), w_all[sel], ed)
            t.loc[sel, f'{v}_band'] = b
            rec['cut_rule'] = f'PUF 2015 positive cuts x SOI factor {factor[(v, y)]:.4f}'
            edges[f'scf/{y}/{v}'] = rec
    # new cuts on the PUF filers: line-13 total, pieces, other income
    for v in ['capital_gains'] + PIECES + ['other_agi']:
        b, rec = band_variable(t.loc[filers, v].to_numpy(float), w_all[filers])
        edges[f'puf/2015/{v}'] = rec
        b2, _ = band_with(t.loc[pside, v].to_numpy(float), w_all[pside], fixed_edge(rec, 1.0))
        t.loc[pside, f'{v}_band'] = b2
        assert np.array_equal(b2[t.loc[pside, 'filer'].to_numpy() == 1], b), v
    for y in years_scf:
        sel = (t.source == 'scf') & (t.year == y)
        ed = fixed_edge(edges['puf/2015/capital_gains'], factor[('capital_gains', y)])
        b, rec = band_with(t.loc[sel, 'capital_gains'].to_numpy(float), w_all[sel], ed)
        t.loc[sel, 'capital_gains_band'] = b
        rec['cut_rule'] = f'PUF 2015 line-13 positive cuts x SOI factor {factor[("capital_gains", y)]:.4f}'
        edges[f'scf/{y}/capital_gains'] = rec
    # net worth: Y1's own cuts per wave; the 2012 wave cut within itself
    for y in years_scf:
        sel = (t.source == 'scf') & (t.year == y)
        if f'scf/{y}/net_worth' in e_y1:
            b, rec = band_with(t.loc[sel, 'net_worth'].to_numpy(float), w_all[sel],
                               fixed_edge(e_y1[f'scf/{y}/net_worth'], 1.0))
            rec = dict(e_y1[f'scf/{y}/net_worth'], bands=rec['bands'])
        else:
            b, rec = band_variable(t.loc[sel, 'net_worth'].to_numpy(float), w_all[sel])
        t.loc[sel, 'net_worth_band'] = b
        edges[f'scf/{y}/net_worth'] = rec
    # unchanged bands equal Y1's
    y1i = y1.set_index(['source', 'record_id'])
    old = t[t.record_id.isin(y1.record_id) & ~((t.source == 'scf') & (t.year == 2012))].set_index(['source', 'record_id'])
    for v in ['wages', 'interest', 'dividends', 'net_worth']:
        a = old[f'{v}_band']
        assert (a.to_numpy() == y1i.loc[a.index, f'{v}_band'].to_numpy()).all(), f'{v}: bands differ from Y1'
    say('wages, interest, dividends and net worth bands equal table Y1 on its rows: checked')
    sel = (t.source == 'scf') & (t.year != 2012)
    a = t[sel].set_index(['source', 'record_id'])['capital_gains_band']
    same = (a.to_numpy() == y1i.loc[a.index, 'capital_gains_band'].to_numpy()).mean()
    say(f'SCF capital gains bands equal to Y1 (cut points changed with the line-13 PUF total): {100 * same:.1f}% of rows')
    # checks
    for v in AMOUNTS:
        b, x = t[f'{v}_band'], t[v]
        assert ((b == -1) == x.isna()).all(), v
        assert ((b == 0) == (x == 0)).all(), v
        assert b.between(-1, N_BANDS - 1).all(), v
    for v in PIECES + ['other_agi']:
        assert (t.loc[t.source == 'scf', f'{v}_band'] == -1).all() and (t.loc[pside, f'{v}_band'] >= 0).all()
    assert (t.loc[pside, 'net_worth_band'] == -1).all() and (t.loc[t.source == 'scf', 'net_worth_band'] >= 0).all()
    na = t.drop(columns=PIECES + ['other_agi', 'net_worth', 'capital_gains_reported']).isna().sum()
    say('missing values outside the absent slots:', int(na.sum()))
    assert na.sum() == 0
    for c in [f'{v}_band' for v in AMOUNTS]:
        t[c] = t[c].astype('int16')
    t['married'] = t['married'].astype('int64')
    t['n_dep'] = t['n_dep'].astype('int64')
    t['filer'] = t['filer'].astype('int64')
    t['year'] = t['year'].astype('int64')
    t.to_parquet(f'{D}/stacked_{TAG}.parquet', index=False)
    edges['meta'] = {'E27': 'step 27 (27_tables.py): Y1 + crude non-filers + 2013 wave if built; capital gains '
                            'as line 13 (E01000 + E01100) and four pieces kg_st, kg_lt, kg_dist, kg_other'}
    json.dump(edges, open(f'{D}/band_edges_{TAG}.json', 'w'), indent=1)
    say('written', f'{D}/stacked_{TAG}.parquet', len(t), 'rows;', t.groupby(['source', 'year']).size().to_dict())

    # ---- maps (04_dollars), from the PUF side (filers and non-filers) and the 2015 SCF wave
    import importlib.util
    spec = importlib.util.spec_from_file_location('dollars', f'{HERE}/04_dollars.py')
    dollars = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(dollars)
    keep = dollars.AMOUNTS
    try:
        dollars.AMOUNTS = ['wages', 'interest', 'dividends', 'capital_gains'] + PIECES + ['other_agi']
        mp = dollars.build_map(t[pside].reset_index(drop=True), edges, 'puf')
        dollars.AMOUNTS = ['net_worth']
        ms = dollars.build_map(t[(t.source == 'scf') & (t.year == 2015)].reset_index(drop=True), edges, 'scf')
    finally:
        dollars.AMOUNTS = keep
    mp.to_parquet(f'{MAPS}/map_puf_2015.parquet', index=False)
    ms.to_parquet(f'{MAPS}/map_scf_2015.parquet', index=False)
    factors = pd.read_csv(f'{R}/map/growth_factors.csv')
    g = dollars.grow_map(mp, factors)          # the five as before (capital_gains: positives only)
    cnt = soi[soi.statistic == 'count'].set_index('year')
    per = lambda item, y: amt.loc[y, item] / cnt.loc[y, item]
    f_sd = float(factors.set_index('variable').loc['capital_gains', 'factor'])
    f_dist = float(per('kg_dist', 2019) / per('kg_dist', 2015))
    for v, f in [('kg_st', f_sd), ('kg_lt', f_sd), ('kg_other', f_sd), ('kg_dist', f_dist)]:
        rows = (g['variable'] == v) & (g['value'] > 0)
        g.loc[rows, 'value'] = g.loc[rows, 'value'] * f
    g.to_parquet(f'{MAPS}/map_puf_2019.parquet', index=False)
    say(f'maps written to {MAPS}; 2019 factors: Schedule D pieces {f_sd:.4f} (positives only), '
        f'distributions {f_dist:.4f}')

    # ---- checks against SOI 2015
    p = t[filers]
    w = p['weight'].to_numpy(float)
    s15 = soi.set_index(['year', 'statistic'])
    rows = []
    for name, x, item in [('kg_dist (E01100)', p['kg_dist'], 'kg_dist'),
                          ('kg_st + kg_lt + kg_other (E01000)', p['kg_st'] + p['kg_lt'] + p['kg_other'], 'sch_d_net'),
                          ('line 13 (all four pieces)', p['capital_gains'], 'capital_gains'),
                          ('interest', p['interest'], 'interest'), ('dividends', p['dividends'], 'dividends'),
                          ('wages', p['wages'], 'wages')]:
        x = x.to_numpy(float)
        rows.append({'item': name, 'PUF returns with it (millions)': w[x != 0].sum() / 1e6,
                     'IRS returns with it (millions)': s15.loc[(2015, 'count'), item] / 1e6,
                     'PUF amount ($ billion)': (w * x).sum() / 1e9,
                     'IRS amount ($ billion)': s15.loc[(2015, 'amount'), item] / 1e9})
    for v in ['kg_st', 'kg_lt', 'kg_other']:
        x = p[v].to_numpy(float)
        rows.append({'item': f'{v} alone (no Table 1.4 line)', 'PUF returns with it (millions)': w[x != 0].sum() / 1e6,
                     'IRS returns with it (millions)': np.nan, 'PUF amount ($ billion)': (w * x).sum() / 1e9,
                     'IRS amount ($ billion)': np.nan})
    soi_check = pd.DataFrame(rows)
    say(soi_check.to_string())

    # ---- the capital gains basis mix in closed form, 2016 wave (see 27_RESULTS.md)
    s = t[(t.source == 'scf') & (t.year == 2015)]
    ws, xs = s['weight'].to_numpy(float), s['capital_gains_reported'].to_numpy(float)
    at = np.isclose(xs, -3000)
    beyond = xs < -3000
    pf = t[filers]
    wp = pf['weight'].to_numpy(float)
    before = (pf['kg_st'] + pf['kg_lt'] + pf['kg_dist']).to_numpy(float)
    mix = {'SCF 2016 wave: units reporting exactly -$3,000 (millions)': ws[at].sum() / 1e6,
           'SCF 2016 wave: units reporting a loss beyond -$3,000 (millions)': ws[beyond].sum() / 1e6,
           'share of SCF units at or beyond -$3,000 who report beyond it (before-limit basis), %':
               100 * ws[beyond].sum() / ws[at | beyond].sum(),
           'PUF filers: Schedule D lines plus distributions below -$3,000 (millions)': wp[before < -3000].sum() / 1e6,
           'PUF filers: line 13 at -$3,000 (millions)': wp[np.isclose(pf['capital_gains'].to_numpy(float), -3000)].sum() / 1e6}
    json.dump(mix, open(f'{OUT}/kg_mix.json', 'w'), indent=1)
    say('capital gains basis mix:', json.dumps(mix, indent=1))

    # ---- waves: holders as reported, against IRS counts
    wave_rows = []
    for y in years_scf:
        g_ = t[(t.source == 'scf') & (t.year == y)]
        wg = g_['weight'].to_numpy(float)
        for v in ['interest', 'dividends', 'capital_gains']:
            wave_rows.append({'tax year': y, 'item': v, 'SCF units (millions)': wg.sum() / 1e6,
                              'SCF units reporting it (millions)': wg[g_[v].to_numpy() != 0].sum() / 1e6,
                              'IRS returns with it (millions)': s15.loc[(y, 'count'), v] / 1e6,
                              'IRS returns (millions)': s15.loc[(y, 'count'), 'returns'] / 1e6})
    waves = pd.DataFrame(wave_rows)
    say(waves.to_string())
    md = ['# Step 27 tables: checks', '', '## Log', '', '```', *LOG, '```', '',
          '## Pieces against SOI 2015 (PUF filers)', '', soi_check.to_markdown(index=False, floatfmt='.2f'), '',
          '## SCF waves as reported against IRS counts', '', waves.to_markdown(index=False, floatfmt='.2f'), '']
    open(f'{OUT}/tables_checks.md', 'w').write('\n'.join(md) + '\n')
    json.dump({'conflict': conflict, 'waves': years_scf, 'have_2013': have13},
              open(f'{OUT}/tables_meta.json', 'w'), indent=1)
    open(f'{OUT}/TABLES_DONE', 'w').close()


if __name__ == '__main__':
    main()
