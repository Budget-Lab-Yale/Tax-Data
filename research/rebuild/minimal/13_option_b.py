"""
13_option_b.py  (minimal example; John's "option B", 2026-10-15)

What it does. Builds two variants of the single-year table (PUF 2015 + SCF
2016 wave, tax year 2015) from stacked.parquet, which it does not change.
For the four shared variables (wages, interest, dividends, capital gains)
each source is banded on its OWN full weighted distribution, zeros and
losses included: the 84 bands are slices of the whole distribution, not of
the positives. This is the rule "rank over the whole distribution including
zeros" of 12_CROSSWALK_THEORY.md, section 2. other_agi and net_worth keep
their stacked.parquet bands and edges (their own source's cuts, as before).

The full-distribution layout (84 bands, band 0 = the lowest slice). The 83
cut points are cumulative shares of the source's weight:
  4 bands in the bottom 1%:  0.01%, then to 0.1%, 0.5% and 1%
  15 bands of 1% to the 16th percentile
  37 bands of 2% to the 90th percentile
  then the old top pattern: 9 of 1% to the 99th, 9 of 0.1% to the 99.9th,
  10 of 0.01% to the top.
So the top is cut as finely as in step 1's layout, and the bottom (capital
gains losses) is cut finely at the extreme as step 1's loss bands were.

Zeros are ties. Each zero record gets a position inside the zero mass by a
placement rule, and is then banded by position like any other record:
  B_rand  uniform random position within the zero mass (fixed seed).
  B_wage  position by the record's wages (higher wages, higher position),
          ties in wages broken at random; for wages itself, by interest
          plus dividends, ties at random.
The same rule is applied in both sources. Nonzero records keep the dollar
rule of 01_band.py: equal dollar amounts are ties that go to the same band,
so a lump wider than a band empties the bands it covers. The cut rule is
01_band.py's: a cut is the smallest position whose cumulative weight share
reaches the probability, and a record at a cut goes to the lower band.

The coarse_hi / coarse_neg columns of stacked.parquet are dropped: they are
written in step 1's band numbers and mean nothing in this layout (they are
read only with MINIMAL_COARSEN=1, which these runs do not set).

Usage: python research/rebuild/minimal/13_option_b.py B_rand B_wage
Run through Slurm: sbatch research/rebuild/minimal/13_option_b.sbatch B_rand B_wage

Writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/):
  data/stacked_<T>.parquet, data/band_edges_<T>.json, data/band_report_<T>.csv
  data/crosswalk_<T>.json   the SCF interest amounts of the results file:
                            band under this table and under E4, with the PUF
                            dollar range of each band
  markers/TABLE_<T>_DONE    only after the checks pass
"""
import ast
import json
import os
import sys
import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal/01_band.py'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
D = f'{R}/data'
M = f'{R}/markers'
SEED = 20261015

# 01_band.py's constants and functions only, as 01_variants.py loads them.
tree = ast.parse(open(HERE).read())
_stop = next(i for i, n in enumerate(tree.body) if 'read_csv' in ast.unparse(n))
keep = [n for n in tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
        or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
            and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=keep, type_ignores=[]), HERE, 'exec'), B)
wquantile, N_BANDS, POS0 = B['wquantile'], B['N_BANDS'], B['POS0']
assert N_BANDS == 84

FULL_PROBS = np.round(np.concatenate([
    [0.0001, 0.001, 0.005, 0.01],
    0.01 + 0.01 * np.arange(1, 16),          # to 0.16
    0.16 + 0.02 * np.arange(1, 38),          # to 0.90
    0.90 + 0.01 * np.arange(1, 10),
    0.99 + 0.001 * np.arange(1, 10),
    0.999 + 0.0001 * np.arange(1, 10)]), 6)
assert len(FULL_PROBS) == N_BANDS - 1 and np.all(np.diff(FULL_PROBS) > 0) and FULL_PROBS[-1] < 1
assert abs(FULL_PROBS[18] - 0.16) < 1e-12 and abs(FULL_PROBS[55] - 0.90) < 1e-12
WIDTHS = np.diff(np.r_[0.0, FULL_PROBS, 1.0])

SHARED = ['wages', 'interest', 'dividends', 'capital_gains']
OWN = ['other_agi', 'net_worth']
AMOUNTS = SHARED + OWN
BASE = ['source', 'year', 'record_id', 'household_id', 'implicate', 'split', 'weight', 'married', 'n_dep']
PLACEMENT = {'B_rand': 'random', 'B_wage': 'wages'}
PLACEMENT_TEXT = {
    'random': 'zero records placed at a uniform random position within the zero mass',
    'wages': 'zero records placed within the zero mass in order of wages (for wages itself: interest plus '
             'dividends), higher amount higher position, ties at random'}
CROSSWALK_SCF_INTEREST = [40, 140, 580, 2_500, 16_000, 156_000]
E4_EXPECTED = {40: 27, 140: 40, 580: 50, 2_500: 56, 16_000: 64, 156_000: 73}   # from the conversation

src = pd.read_parquet(f'{D}/stacked.parquet')
V2_EDGES = json.load(open(f'{D}/band_edges.json'))
E4_EDGES = json.load(open(f'{D}/band_edges_E4.json'))
for v in SHARED:
    assert src[v].notna().all(), f'{v}: NA amounts in stacked.parquet'


def zero_order(g, v, rule, rng):
    """Position rank (0 = lowest) of each zero record of v among the zeros of
    this source. Returns an array over g's rows, NaN on nonzero rows."""
    x = g[v].to_numpy(float)
    z = np.flatnonzero(x == 0)
    u = rng.random(len(z))
    if rule == 'random':
        o = np.argsort(u, kind='stable')
    else:
        prior = (g['interest'] + g['dividends']).to_numpy(float) if v == 'wages' else g['wages'].to_numpy(float)
        assert not np.isnan(prior[z]).any()
        o = np.lexsort((u, prior[z]))          # by prior, ties by u
    rank = np.full(len(x), np.nan)
    rank[z[o]] = np.arange(len(z))
    return rank


def full_band(x, w, zrank):
    """Band every record on the full weighted distribution. Order: dollars,
    then (for zeros only) the placement rank. Nonzero equal amounts are one
    group (a tie), each zero is its own group. Returns the band array, the
    edge record and the per-record cumulative check data."""
    tie = np.where(x == 0, zrank + 1, 0.0)
    assert not np.isnan(tie).any()
    o = np.lexsort((tie, x))
    xs, ts, ws = x[o], tie[o], w[o]
    new = np.r_[True, (xs[1:] != xs[:-1]) | (ts[1:] != ts[:-1])]
    gi = np.cumsum(new) - 1                          # group index in sorted order
    W = ws.sum()
    F = np.cumsum(ws) / W
    idx = np.minimum(np.searchsorted(F, FULL_PROBS - 1e-12, side='left'), len(xs) - 1)
    cut_g = gi[idx]
    bs = np.searchsorted(cut_g, gi, side='left').astype(np.int16)
    band = np.empty(len(x), dtype=np.int16)
    band[o] = bs
    # group end shares, to check the cut rule
    g_end = F[np.r_[np.flatnonzero(new)[1:] - 1, len(F) - 1]]   # cumulative share at each group's end
    g_w = np.bincount(gi, weights=ws) / W
    top = []                                          # cumulative share at the top of each band 0..82
    for j in range(N_BANDS - 1):
        top.append(float(g_end[cut_g[j]]))
    top = np.array(top)
    dev = top - FULL_PROBS
    cut_wt = g_w[cut_g]
    assert (dev >= -1e-9).all(), 'a band top falls below its probability'
    assert (dev <= cut_wt + 1e-9).all(), 'a band top overshoots by more than the cut group'
    zero_lo = float(F[xs < 0][-1]) if (xs < 0).any() else 0.0     # share below the zero mass
    zero_hi = float(F[xs <= 0][-1])                                 # share at or below zero
    bands = []
    for b in range(N_BANDS):
        mb = band == b
        zb = mb & (x == 0)
        if not mb.any():
            bands.append({'band': b, 'records': 0, 'weight': 0.0, 'share': 0.0, 'zero_records': 0,
                          'zero_weight_share_of_band': None, 'min': None, 'max': None, 'median': None})
            continue
        bands.append({'band': b, 'records': int(mb.sum()), 'weight': float(w[mb].sum()),
                      'share': float(w[mb].sum() / W), 'zero_records': int(zb.sum()),
                      'zero_weight_share_of_band': float(w[zb].sum() / w[mb].sum()),
                      'min': float(x[mb].min()), 'max': float(x[mb].max()),
                      'median': float(wquantile(x[mb], w[mb], [0.5])[0])})
    zb_list = [r['band'] for r in bands if r['zero_records'] > 0]
    only_zero = [r['band'] for r in bands if r['records'] > 0 and r['zero_records'] == r['records']]
    straddle = [r['band'] for r in bands if 0 < r['zero_records'] < r['records']]
    rec = {'n_bands': N_BANDS, 'zero_band': None, 'layout': 'full_distribution',
           'full': {'probs': FULL_PROBS.tolist(),
                    'cut_dollars': [float(xs[i]) for i in idx],
                    'cut_in_zero_mass': [bool(xs[i] == 0) for i in idx],
                    'top_share': top.tolist(),
                    'note': 'band j holds positions from cut j-1 (exclusive) to cut j (inclusive); band 0 '
                            'is open below, band 83 above; positions are dollars, then the zero placement'},
           'negative': {'bands_used': 0, 'cuts': [],
                        'note': 'no separate loss bands: losses are the lowest slices of the full distribution'},
           'zero_mass': {'from_share': zero_lo, 'to_share': zero_hi, 'share': zero_hi - zero_lo,
                         'first_band': min(zb_list), 'last_band': max(zb_list),
                         'first_band_above': max(zb_list) + 1, 'bands_only_zero': only_zero,
                         'bands_straddling': straddle},
           'bands': bands}
    return band, rec, dev, cut_wt


def build(T):
    rule = PLACEMENT[T]
    df = src[BASE + AMOUNTS + [f'{v}_band' for v in OWN]].copy()
    edges, rep = {}, []
    for v in SHARED:
        df[f'{v}_band'] = np.int16(-1)
    for si, ((s, yr), g) in enumerate(sorted(df.groupby(['source', 'year']), key=lambda t: t[0][0] != 'puf')):
        w = g['weight'].to_numpy(float)
        for vi, v in enumerate(SHARED):
            rng = np.random.default_rng([SEED, si, vi])   # same stream for both tables: B_rand and B_wage
            x = g[v].to_numpy(float)                      # share the uniform draws, only the order rule differs
            zr = zero_order(g, v, rule, rng)
            band, rec, dev, cut_wt = full_band(x, w, zr)
            rec['placement'] = PLACEMENT_TEXT[rule]
            df.loc[g.index, f'{v}_band'] = band
            edges[f'{s}/{yr}/{v}'] = rec
            sh = np.array([b['share'] for b in rec['bands']])
            lump = cut_wt > 1e-4           # bands whose top cut sits on a group holding over 0.01% of weight
            lump_b = set(np.flatnonzero(lump)) | set(np.flatnonzero(lump) + 1)
            clean = np.array([b not in lump_b for b in range(N_BANDS)])
            cnt = np.array([b['records'] for b in rec['bands']])
            zm = rec['zero_mass']
            rep.append({'table': T, 'source': s, 'year': yr, 'variable': v,
                        'zero_share': zm['share'], 'zero_from_share': zm['from_share'], 'zero_to_share': zm['to_share'],
                        'zero_first_band': zm['first_band'], 'zero_last_band': zm['last_band'],
                        'bands_only_zero': len(zm['bands_only_zero']), 'bands_straddling': str(zm['bands_straddling']),
                        'nonempty_bands': int((cnt > 0).sum()), 'empty_bands': int((cnt == 0).sum()),
                        'empty_band_list': str(list(np.flatnonzero(cnt == 0))),
                        'max_band_top_overshoot': float(dev.max()),
                        'max_abs_share_minus_width_clean_bands': float(np.abs(sh - WIDTHS)[clean].max()),
                        'bands_next_to_a_lump': len(lump_b),
                        'min_records_nonempty_band': int(cnt[cnt > 0].min()),
                        'records_top_19_bands': int(cnt[N_BANDS - 19:].sum())})
    for v in SHARED:
        df[f'{v}_band'] = df[f'{v}_band'].astype('int16')
    for k in [f'{s}/2015/{v}' for s in ['puf', 'scf'] for v in OWN]:
        edges[k] = V2_EDGES[k]
    check(T, df, edges)
    xw = crosswalk(T, edges)
    meta = {'table': f'{T}: option B, shared variables banded on each source\'s own full distribution '
                     f'including zeros; {PLACEMENT_TEXT[rule]}',
            'layout': '84 bands; -1 = amount NA. wages, interest, dividends, capital_gains: layout '
                      '"full_distribution" (no zero band, no loss bands; band 0 = lowest slice of the whole '
                      'weighted distribution, cut at full.probs). other_agi, net_worth: step 1 layout (0 = zero, '
                      '1-10 negatives, 11-83 positives), copied from band_edges.json.',
            'full_layout_probs': FULL_PROBS.tolist(),
            'full_layout_text': '4 bands in the bottom 1% (0.01%, to 0.1%, to 0.5%, to 1%), 15 of 1% to the 16th '
                                'percentile, 37 of 2% to the 90th, 9 of 1% to the 99th, 9 of 0.1% to the 99.9th, '
                                '10 of 0.01% to the top',
            'placement': PLACEMENT_TEXT[rule], 'seed': SEED, 'dollars': '2015 dollars',
            'dropped_columns': 'coarse_hi / coarse_neg (step 1 band numbers)'}
    df.to_parquet(f'{D}/stacked_{T}.parquet', index=False)
    json.dump({'meta': meta, **edges}, open(f'{D}/band_edges_{T}.json', 'w'), indent=1)
    pd.DataFrame(rep).to_csv(f'{D}/band_report_{T}.csv', index=False)
    json.dump(xw, open(f'{D}/crosswalk_{T}.json', 'w'), indent=1)
    os.makedirs(M, exist_ok=True)
    open(f'{M}/TABLE_{T}_DONE', 'w').close()
    pd.set_option('display.width', 250); pd.set_option('display.max_columns', 40)
    print(pd.DataFrame(rep).to_string())
    print(json.dumps(xw, indent=1))
    print(f'{T}: written', flush=True)


def check(T, df, edges):
    assert len(df) == len(src)
    for c in BASE + AMOUNTS + [f'{v}_band' for v in OWN]:
        assert df[c].equals(src[c]), f'{T}: {c} differs from stacked.parquet'
    for v in AMOUNTS:
        b = df[f'{v}_band']
        assert ((b == -1) == df[v].isna()).all(), f'{T}: {v} band -1 does not match NA amount'
        assert b.between(-1, N_BANDS - 1).all(), T
    for v in SHARED:
        assert (df[f'{v}_band'] >= 0).all(), f'{T}: {v} has an NA band'
        for s, g in df.groupby('source'):
            mx = g.groupby(f'{v}_band')[v].agg(['min', 'max']).sort_index()
            assert (mx['max'].to_numpy()[:-1] <= mx['min'].to_numpy()[1:]).all(), f'{T}: {v} {s} bands not monotone'
            zb = g.loc[g[v] == 0, f'{v}_band']
            pb = g.loc[g[v] > 0, f'{v}_band']
            nb = g.loc[g[v] < 0, f'{v}_band']
            assert zb.max() <= pb.min(), f'{T}: {v} {s}: a zero above the first positive'
            if len(nb):
                assert nb.max() <= zb.min(), f'{T}: {v} {s}: a loss above a zero'
            e = edges[f'{s}/2015/{v}']
            assert e['zero_mass']['first_band'] == zb.min() and e['zero_mass']['last_band'] == zb.max()
    # the PUF's weighted share per band against the layout (exact up to one tie group, checked in full_band)
    print(f'{T}: checks passed', flush=True)


def crosswalk(T, edges):
    """Band of each SCF interest amount under this table and under E4, and the
    PUF dollar range of that band."""
    out = []
    es, ep = edges['scf/2015/interest'], edges['puf/2015/interest']
    e4p = E4_EDGES['puf/2015/interest']
    for s in CROSSWALK_SCF_INTEREST:
        b = int(np.searchsorted(np.array(es['full']['cut_dollars']), s, side='left'))
        pb = ep['bands'][b]
        b4 = POS0 + int(np.searchsorted(np.array(e4p['positive']['cuts']), s, side='left'))
        p4 = e4p['bands'][b4]
        out.append({'scf_interest': s, 'band': b,
                    'scf_share_below_band_top': es['full']['top_share'][b] if b < N_BANDS - 1 else 1.0,
                    'puf_min': pb['min'], 'puf_max': pb['max'], 'puf_median': pb['median'],
                    'puf_zero_share_of_band': pb['zero_weight_share_of_band'],
                    'e4_band': b4, 'e4_expected': E4_EXPECTED[s], 'e4_puf_min': p4['min'], 'e4_puf_max': p4['max']})
        assert b4 == E4_EXPECTED[s], f'E4 band of ${s} is {b4}, not {E4_EXPECTED[s]}'
    return out


if __name__ == '__main__':
    for T in sys.argv[1:]:
        print(f'==== {T} ====', flush=True)
        build(T)
