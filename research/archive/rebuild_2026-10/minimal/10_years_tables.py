"""
10_years_tables.py  (minimal example, set B of 10_TRANSLATION_EXPERIMENTS.md;
2026-10-15)

What it does: builds four versions of the three-year table (PUF 2015 plus the
SCF waves for tax years 2015, 2018 and 2021) that differ only in how the SCF's
four shared variables (wages, interest, dividends, capital gains) are cut into
bands. Everything else is taken from stacked_years.parquet unchanged: record
ids, split labels, weights, dollar amounts, the PUF's bands, other_agi (PUF
only, on its own 2015 cut points) and net worth (SCF only, cut within each
wave). The banding function is 01_band.py's own band_variable (loaded from
that file, not copied).

  Y0  the table as built: each SCF wave cut on its own weighted quantiles
      (within source and year).
  Y1  the PUF 2015 cut points, grown to year t by one factor per variable:
      the ratio of the SOI all-returns total for tax year t to the 2015 total
      (Table 1.4: salaries and wages; taxable interest; ordinary dividends;
      net capital gain = capital gain distributions on Form 1040 plus Schedule
      D taxable net gain minus Schedule D taxable net loss). Only the positive
      cut points are grown. The negative ones (capital losses) are left as
      they are, because the $3,000 limit on deductible losses is the same
      dollar figure in every year.
  Y2  the PUF 2015 cut points unchanged in every year.
  Y3  the PUF 2015 cut points moved by the SCF's own growth at each
      percentile: cut c in year t is the SCF wave t's weighted quantile at
      the share F of the SCF 2015 wave lying at or below c. Positive cuts use
      the distribution among positive amounts, negative cuts the distribution
      among negative amounts. A cut below every 2015 amount stays below
      every year-t amount; a cut at or above every 2015 amount stays at or
      above every year-t amount. For 2015 itself this returns cut points that
      put every SCF 2015 record in the same band as the PUF cut points do
      (checked against table E4).

In Y1 to Y3 the SCF 2015 wave is cut on the PUF 2015 cut points unchanged, so
its shared-variable bands must equal table E4's (stacked_E4.parquet): checked.

Usage: python research/rebuild/minimal/10_years_tables.py Y0 Y1 Y2 Y3
Through Slurm: sbatch research/rebuild/minimal/10_years_tables.sbatch Y0 Y1 Y2 Y3

Writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/):
  data/stacked_<T>.parquet, data/band_edges_<T>.json, data/band_report_<T>.csv
  data/growth_factors_years.csv   the SOI factors used by Y1, and the SCF's own
                                  wave-to-wave growth in each total beside them
  markers/TABLE_<T>_DONE          only after the checks pass
"""
import ast
import copy
import json
import os
import shutil
import sys
import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal/01_band.py'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
D = f'{R}/data'
M = f'{R}/markers'
SOI = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/bysize_income_sources.csv'

# 01_band.py's constants and functions only (as 01_variants.py loads them).
tree = ast.parse(open(HERE).read())
_stop = next(i for i, n in enumerate(tree.body) if 'read_csv' in ast.unparse(n))
keep = [n for n in tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
        or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
            and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=keep, type_ignores=[]), HERE, 'exec'), B)
band_variable, wquantile, N_BANDS, POS0, NEG0, POS_PROBS = (
    B[k] for k in ['band_variable', 'wquantile', 'N_BANDS', 'POS0', 'NEG0', 'POS_PROBS'])
assert N_BANDS == 84

SHARED = ['wages', 'interest', 'dividends', 'capital_gains']
AMOUNTS = SHARED + ['other_agi', 'net_worth']
YEARS = [2015, 2018, 2021]
src = pd.read_parquet(f'{D}/stacked_years.parquet')
edges_y = json.load(open(f'{D}/band_edges_years.json'))
assert sorted(src.loc[src.source == 'scf', 'year'].unique()) == YEARS
assert (src.loc[src.source == 'puf', 'year'] == 2015).all()


# ---------------------------------------------------------------- growth factors

def soi_totals():
    """All-returns totals (thousands of dollars) per tax year for the four items."""
    t = pd.read_csv(SOI, dtype=str)
    t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')
          & t['tax_year'].isin([str(y) for y in YEARS])]
    item = {'salaries and wages > amount': 'wages',
            'taxable interest > amount': 'interest',
            'ordinary dividends > amount': 'dividends',
            'capital gain distributions reported on form 1040 > amount': 'kg_dist',
            'sales of capital assets reported on form 1040, schedule d > taxable net gain > amount': 'kg_gain',
            'sales of capital assets reported on form 1040, schedule d > taxable net loss > amount': 'kg_loss'}
    t = t[t['item'].isin(item)]
    v = {(int(r.tax_year), item[r.item]): float(r.value) for r in t.itertuples()}
    assert len(v) == len(YEARS) * len(item), f'SOI rows missing: {len(v)}'
    out = {}
    for y in YEARS:
        out[(y, 'wages')] = v[(y, 'wages')]
        out[(y, 'interest')] = v[(y, 'interest')]
        out[(y, 'dividends')] = v[(y, 'dividends')]
        out[(y, 'capital_gains')] = v[(y, 'kg_dist')] + v[(y, 'kg_gain')] - v[(y, 'kg_loss')]
    return out


def growth_table():
    soi = soi_totals()
    summ = pd.read_csv(f'{D}/summary_years.csv')
    summ = summ[(summ.source == 'scf') & summ.variable.isin(SHARED)]
    scf = {(int(r.year), r.variable): r.weighted_total_billions for r in summ.itertuples()}
    rows = []
    for v in SHARED:
        for y in YEARS:
            rows.append({'variable': v, 'year': y,
                         'soi_total_billions': soi[(y, v)] / 1e6,
                         'soi_factor': soi[(y, v)] / soi[(2015, v)],
                         'scf_total_billions': scf[(y, v)],
                         'scf_factor': scf[(y, v)] / scf[(2015, v)]})
    g = pd.DataFrame(rows)
    # check against the totals 01_targets.py wrote for 2015 (whole dollars there)
    t = pd.read_csv(f'{R}/targets/soi_totals_2015_2019.csv')
    t = t[(t.year == 2015) & (t.statistic == 'amount')].set_index('variable')['value']
    assert abs(soi[(2015, 'wages')] * 1e3 - t['wages']) < 1e3
    assert abs(soi[(2015, 'capital_gains')] * 1e3 - t['net_capital_gain']) < 1e4
    g.to_csv(f'{D}/growth_factors_years.csv', index=False)
    print(g.to_string())
    return {(r.variable, r.year): r.soi_factor for r in g.itertuples()}


# ---------------------------------------------------------------- cut points

def percentile_cuts(cuts, ref_x, ref_w, x, w):
    """Y3: each cut c moved to wave t's weighted quantile at the 2015 wave's
    share at or below c, within one sign (positives or negatives)."""
    cuts = np.asarray(cuts, dtype=float)
    o = np.argsort(ref_x, kind='stable')
    rs, F = ref_x[o], np.cumsum(ref_w[o]) / ref_w.sum()
    out = np.empty_like(cuts)
    lo_t, hi_t = x.min(), x.max()
    for i, c in enumerate(cuts):
        if c < rs[0]:
            out[i] = min(c, np.nextafter(lo_t, -np.inf))     # below every amount, in both waves
        elif c >= rs[-1]:
            out[i] = max(c, hi_t)                            # at or above every amount, in both waves
        else:
            p = F[np.searchsorted(rs, c, side='right') - 1]
            out[i] = wquantile(x, w, [p])[0]
    assert (np.diff(out) >= 0).all(), 'percentile cuts out of order'
    return out


def fixed_record(T, v, yr, puf_edge, factors, ref):
    """The PUF 2015 edge record with the cut points of rule T for year yr."""
    rec = copy.deepcopy({k: puf_edge[k] for k in ['negative', 'positive']})
    if T == 'Y2' or yr == 2015 and T in ('Y1', 'Y2'):
        return rec, 'PUF 2015 cuts'
    if T == 'Y1':
        rec['positive']['cuts'] = (np.array(rec['positive']['cuts']) * factors[(v, yr)]).tolist()
        return rec, f'PUF 2015 positive cuts x SOI factor {factors[(v, yr)]:.4f}; negative cuts unchanged'
    assert T == 'Y3'
    g = src[(src.source == 'scf') & (src.year == yr)]
    x, w = g[v].to_numpy(float), g['weight'].to_numpy(float)
    rx, rw = ref[v]
    for sign, key in [(1, 'positive'), (-1, 'negative')]:
        cuts = rec[key]['cuts'] if key in rec else []
        if not len(cuts):
            continue
        m, rm = sign * x > 0, sign * rx > 0
        assert m.any() and rm.any(), f'{v} {yr}: no {key} amounts to move the cuts with'
        rec[key]['cuts'] = percentile_cuts(cuts, rx[rm], rw[rm], x[m], w[m]).tolist()
    return rec, 'PUF 2015 cuts moved by SCF percentile growth from the 2015 wave'


# ---------------------------------------------------------------- build and check

def band_report(T, df, edges):
    rep = []
    for (s, yr), g in df.groupby(['source', 'year']):
        w = g['weight'].to_numpy(float)
        for v in AMOUNTS:
            x = g[v].to_numpy(float)
            if np.isnan(x).all():
                continue
            b = g[f'{v}_band'].to_numpy()
            cnt = np.bincount(b[b >= 0], minlength=N_BANDS)
            rec = edges[f'{s}/{yr}/{v}']
            k = rec['negative']['bands_used']
            pos = b >= POS0
            r = {'table': T, 'source': s, 'year': yr, 'variable': v,
                 'cuts_from_puf': bool(rec.get('cuts_from_other_source', False)),
                 'cut_rule': rec.get('cut_rule', 'within source and year'),
                 'nonempty_bands': int((cnt > 0).sum()),
                 'positive_bands_empty': int((cnt[POS0:] == 0).sum()),
                 'negative_bands_empty_of_used': int((cnt[NEG0:NEG0 + k] == 0).sum()),
                 'holder_share_weighted': float(w[x != 0].sum() / w.sum())}
            # where the year's positives sit on the cut points: share of positive
            # weight above the cut at the owner's 50th, 90th, 99th percentile
            for name, j in [('p50', 24), ('p90', 44), ('p99', 53)]:
                r[f'positive_weight_share_above_cut_{name}'] = float(w[pos & (b > POS0 + j)].sum() / w[pos].sum()) \
                    if pos.any() else np.nan
                r[f'cut_{name}_dollars'] = rec['positive']['cuts'][j] if 'positive' in rec else np.nan
            for bb in range(POS0 + 54, N_BANDS):      # the top 19 positive bands
                r[f'records_band_{bb}'] = int(cnt[bb])
            rep.append(r)
    return pd.DataFrame(rep)


def check(T, df, e4):
    assert len(df) == len(src)
    for c in src.columns:
        if c.endswith('_band') and c[:-5] in SHARED:
            continue
        assert df[c].equals(src[c]), f'{T}: {c} differs from stacked_years.parquet'
    for v in AMOUNTS:
        b = df[f'{v}_band']
        assert ((b == -1) == df[v].isna()).all(), f'{T}: {v}: band -1 where an amount exists, or the reverse'
        assert b.between(-1, N_BANDS - 1).all(), T
        assert ((b == 0) == (df[v] == 0)).all(), f'{T}: {v}: band 0 does not match zero amount'
        for (s, yr), g in df[df[v].notna()].groupby(['source', 'year']):
            key = np.where(g[f'{v}_band'] == 0, NEG0 + 9.5, g[f'{v}_band'])
            mx = g.groupby(key)[v].agg(['min', 'max']).sort_index()
            assert (mx['max'].to_numpy()[:-1] <= mx['min'].to_numpy()[1:]).all(), f'{T}: {v} {s} {yr} bands overlap'
    if T != 'Y0':
        # the 2015 SCF wave and the PUF get exactly table E4's shared bands
        for s in ['puf', 'scf']:
            a = df[(df.source == s) & (df.year == 2015)].set_index('record_id').sort_index()
            b = e4[e4.source == s].set_index('record_id').sort_index()
            assert a.index.equals(b.index), f'{T}: {s} 2015 record ids differ from E4'
            for v in SHARED:
                assert (a[f'{v}_band'].to_numpy() == b[f'{v}_band'].to_numpy()).all(), \
                    f'{T}: {s} 2015 {v} bands differ from E4'
    print(f'{T}: checks passed', flush=True)


def write(T, df, edges, meta):
    df.to_parquet(f'{D}/stacked_{T}.parquet', index=False)
    json.dump({'meta': meta, **edges}, open(f'{D}/band_edges_{T}.json', 'w'), indent=1)
    rep = band_report(T, df, edges)
    rep.to_csv(f'{D}/band_report_{T}.csv', index=False)
    os.makedirs(M, exist_ok=True)
    open(f'{M}/TABLE_{T}_DONE', 'w').close()
    pd.set_option('display.width', 250)
    print(rep[rep.variable.isin(SHARED)].iloc[:, :16].to_string())
    print(f'{T}: written', flush=True)


def build(T, factors, e4):
    df = src.copy()
    edges = copy.deepcopy({k: v for k, v in edges_y.items() if k != 'meta'})
    if T != 'Y0':
        ref = {}
        g15 = src[(src.source == 'scf') & (src.year == 2015)]
        for v in SHARED:
            ref[v] = (g15[v].to_numpy(float), g15['weight'].to_numpy(float))
        for yr in YEARS:
            g = src[(src.source == 'scf') & (src.year == yr)]
            w = g['weight'].to_numpy(float)
            for v in SHARED:
                fixed, rule = fixed_record(T, v, yr, edges_y[f'puf/2015/{v}'], factors, ref)
                band, rec = band_variable(g[v].to_numpy(float), w, fixed)
                rec['cut_rule'] = rule
                df.loc[g.index, f'{v}_band'] = band
                edges[f'scf/{yr}/{v}'] = rec
        for v in SHARED:
            df[f'{v}_band'] = df[f'{v}_band'].astype('int16')
    check(T, df, e4)
    meta = dict(edges_y['meta'])
    meta['table'] = {
        'Y0': 'Y0: the three-year table as built; every source and year on its own cut points',
        'Y1': 'Y1: SCF shared variables on the PUF 2015 cut points, positive cuts grown by SOI total growth per variable',
        'Y2': 'Y2: SCF shared variables on the PUF 2015 cut points unchanged in every year',
        'Y3': 'Y3: SCF shared variables on the PUF 2015 cut points moved by SCF percentile growth from the 2015 wave'}[T]
    if T == 'Y1':
        meta['growth_factors'] = {f'{v}/{y}': factors[(v, y)] for v in SHARED for y in YEARS}
    write(T, df, edges, meta)


if __name__ == '__main__':
    tags = sys.argv[1:] or ['Y0', 'Y1', 'Y2', 'Y3']
    factors = growth_table()
    e4 = pd.read_parquet(f'{D}/stacked_E4.parquet', columns=['source', 'record_id'] + [f'{v}_band' for v in SHARED])
    for T in tags:
        assert T in ('Y0', 'Y1', 'Y2', 'Y3'), T
        print(f'==== {T} ====', flush=True)
        build(T, factors, e4)
