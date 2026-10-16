"""
01_band.py  (minimal example, step 1, part 2 of 2)

What it does: reads the stacked table written by 01_build_table.R, maps each
amount to an integer band by weighted quantiles within source and year, adds
a fixed 80/10/10 train/validation/test split (PUF by record, SCF by household
so all implicates and all units of a household share a split), and writes
the summary tables.

Band layout, the same index space for every variable (84 bands):
  0      exactly zero
  1-10   negatives, 10 bands, 1 = most negative (changed 2026-10-14 from 4
         quartile bands: the lowest quartile of other_agi ran from about
         -$5,300 to -$137 million, too wide for a within-band dollar draw).
         Cut by weighted quantile of the loss size among the negatives,
         finer at the extreme like the positive top: 4 bands of 22.5% for
         the least negative 90% of negative weight, 3 of 3% to 99%, then
         0.5%, 0.4% and 0.1%. Under 100 negative records: equal-weight bands,
         one per 10 records (at least 1), numbered up from band 1.
  11-83  positives, 73 bands: 45 of 2% of the positive weight to the 90th
         percentile, 9 of 1% to the 99th, 9 of 0.1% to the 99.9th, 10 of
         0.01% to the top.
Cut points are weighted quantiles (the smallest value whose cumulative weight
share reaches the probability). A record equal to a cut point goes to the
lower band. When a single dollar value carries more weight than a band's
width, two cut points coincide and the band between them is empty: that is
the only way a band ends up empty or merged, and it is deterministic.

What it writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/):
  stacked.parquet    the table with split and *_band columns
  band_edges.json    per source/year/variable: cut points, probabilities,
                     and per band the record count, weight, min, max and
                     weighted median in dollars
  summary.csv        per source and variable: counts, totals, shares at
                     zero and negative, weighted percentiles
  band_report.csv    per source and variable: empty bands, largest lumps
What to look at: summary.csv and band_report.csv (the report quotes both).

With --years (added 2026-10-14) it builds the several-years table instead:
the PUF plus the SCF 2016, 2019 and 2022 waves (scf<wave>_units files from
01_build_table.R --scf-wave and 06_build_scf2019.R), banded within source
and tax year, and writes the same files with the suffix _years. The step-1
files are not touched.
"""
import json
import sys
import numpy as np
import pandas as pd

D = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
AMOUNTS = ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi', 'net_worth']
SEED = 20261014
YEARS = '--years' in sys.argv   # several-years table (see below)
SUF = '_years' if YEARS else ''
N_NEG, NEG0, POS0 = 10, 1, 11
# cumulative share of negative weight, counted from the least negative value
NEG_PROBS = np.array([0.225, 0.45, 0.675, 0.90, 0.93, 0.96, 0.99, 0.995, 0.999])
POS_PROBS = np.round(np.concatenate([
    0.02 * np.arange(1, 46), 0.90 + 0.01 * np.arange(1, 10),
    0.99 + 0.001 * np.arange(1, 10), 0.999 + 0.0001 * np.arange(1, 10)]), 6)
N_POS = len(POS_PROBS) + 1
N_BANDS = POS0 + N_POS
assert N_POS == 73 and N_BANDS == 84 and len(NEG_PROBS) == N_NEG - 1


def wquantile(x, w, probs):
    """Smallest x whose cumulative weight share reaches each probability."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    idx = np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)
    return xs[idx]


def band_variable(x, w, fixed=None):
    """Return the band array (-1 where x is NA) and the edge record.
    fixed: optional edge record of another source in the same year (added
    2026-10-15 for 01_variants.py, table B); its cut points are applied
    unchanged instead of being computed from x."""
    band = np.full(len(x), -1, dtype=np.int16)
    ok = ~np.isnan(x)
    band[ok & (x == 0)] = 0
    rec = {'n_bands': N_BANDS, 'zero_band': 0}
    neg, pos = ok & (x < 0), ok & (x > 0)
    if fixed is not None:
        fneg, fpos = fixed['negative'], fixed.get('positive')
        assert fneg['bands_used'] > 0 or not neg.any(), 'negatives with no negative cuts to apply'
        assert fpos is not None or not pos.any(), 'positives with no positive cuts to apply'
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
        if k:
            # Cut points are quantiles of the loss size (-x), counted from the
            # least negative. Assignment is in dollars, as for positives: a value
            # equal to a cut goes to the lower-dollar (more negative) band. So a
            # lump at the extreme (PUF losses at the -$3,000 limit) stays in its
            # own band 1 and the bands it swallows are empty.
            probs = NEG_PROBS if k == N_NEG else np.arange(1, k) / k
            mcuts = wquantile(-x[neg], w[neg], probs)
            band[neg] = NEG0 + np.searchsorted(-mcuts[::-1], x[neg], side='left')
            rec['negative'] = {'bands_used': k,
                               'probs_from_least_negative': probs.tolist(),
                               'cuts': (-mcuts[::-1]).tolist(),
                               'note': 'cuts in dollars, most negative first; band 1 = below cuts[0]'}
        else:
            rec['negative'] = {'bands_used': 0, 'probs': [], 'cuts': []}
        if pos.any():
            cuts = wquantile(x[pos], w[pos], POS_PROBS)
            band[pos] = POS0 + np.searchsorted(cuts, x[pos], side='left')
            rec['positive'] = {'probs': POS_PROBS.tolist(), 'cuts': cuts.tolist()}
    bands = []
    for b in range(N_BANDS):
        m = band == b
        if not m.any():
            bands.append({'band': b, 'records': 0, 'weight': 0.0, 'min': None, 'max': None, 'median': None})
            continue
        xb, wb = x[m], w[m]
        bands.append({'band': b, 'records': int(m.sum()), 'weight': float(wb.sum()),
                      'min': float(xb.min()), 'max': float(xb.max()),
                      'median': float(wquantile(xb, wb, [0.5])[0])})
    rec['bands'] = bands
    return band, rec


def lump(x, w):
    """Largest single dollar value by weight share, among the given records."""
    if len(x) == 0:
        return np.nan, np.nan
    s = pd.Series(w).groupby(x).sum()
    return float(s.idxmax()), float(s.max() / w.sum())


def household_split(hh_ids, seed):
    """80/10/10 by household: one uniform draw per sorted unique id."""
    ids = np.sort(hh_ids.unique())
    u = pd.Series(np.random.default_rng(seed).random(len(ids)), index=ids)
    uu = hh_ids.map(u).to_numpy()
    return np.where(uu < 0.8, 'train', np.where(uu < 0.9, 'validation', 'test'))


raw = pd.read_csv(f'{D}/raw_stacked.csv.gz')
if not YEARS:
    df = raw
    df['split'] = ''
    for s, seed in [('puf', SEED), ('scf', SEED + 1)]:
        m = df['source'] == s   # PUF: household_id = record_id
        df.loc[m, 'split'] = household_split(df.loc[m, 'household_id'], seed)
else:
    # Several-years table (2026-10-14): the PUF plus SCF waves 2016, 2019 and
    # 2022 (tax years 2015, 2018, 2021). SCF net worth is in each interview
    # year's dollars (asset factor). SCF ids of the 2018 and 2021 waves are
    # offset by 1e8 x (year - 2015) so that (source, record_id) stays unique;
    # the original id is record_id - 1e8 x (year - 2015).
    cols = list(raw.columns)
    waves = [pd.read_csv(f'{D}/scf2016_units.csv.gz'), pd.read_parquet(f'{D}/scf2019_units.parquet'),
             pd.read_csv(f'{D}/scf2022_units.csv.gz')]
    for wv in waves:
        if 'other_agi' not in wv:
            wv['other_agi'] = np.nan
    # The 2016 wave rebuilt in wave mode must be the step-1 rows, with net
    # worth rescaled by one constant (asset factor instead of income factor).
    old = raw[raw.source == 'scf'].set_index('record_id').sort_index()
    new = waves[0].set_index('record_id').sort_index()
    assert old.index.equals(new.index)
    for v in ['weight', 'married', 'n_dep', 'wages', 'interest', 'dividends', 'capital_gains']:
        assert np.allclose(old[v], new[v], rtol=1e-12, atol=1e-6), v
    r = old['net_worth'][old.net_worth != 0] / new['net_worth'][old.net_worth != 0]
    print('2016 wave: step-1 net worth / wave-mode net worth, min and max:', r.min(), r.max())
    df = pd.concat([raw[raw.source == 'puf']] + [wv[cols] for wv in waves], ignore_index=True)
    scf = df.source == 'scf'
    off = np.where(scf, 1e8 * (df['year'] - 2015), 0)
    df['record_id'] += off
    df['household_id'] += off
    assert not df.duplicated(['source', 'record_id']).any()
    # Split: rows already in stacked.parquet keep their labels; new waves are
    # split by household with seed SEED + (year - 2014) (2016 wave: SEED + 1).
    base = pd.read_parquet(f'{D}/stacked.parquet', columns=['source', 'record_id', 'split'])
    df = df.merge(base, on=['source', 'record_id'], how='left')
    old_rows = (df.source == 'puf') | (df.year == 2015)
    assert df.loc[old_rows, 'split'].notna().all() and df.loc[~old_rows, 'split'].isna().all()
    for yr in [2018, 2021]:
        m = (df.source == 'scf') & (df.year == yr)
        df.loc[m, 'split'] = household_split(df.loc[m, 'household_id'], SEED + yr - 2014)
    # check: the formula reproduces the 2016 wave's existing labels
    m = (df.source == 'scf') & (df.year == 2015)
    assert (household_split(df.loc[m, 'household_id'], SEED + 1) == df.loc[m, 'split']).all()
df['year'] = df['year'].astype(int)
assert df['split'].isin(['train', 'validation', 'test']).all()
assert df[df.source == 'scf'].groupby('household_id')['split'].nunique().max() == 1

# ---- bands ----
edges, rep, summ = {}, [], []
for (s, yr), g in df.groupby(['source', 'year']):
    w_all = g['weight'].to_numpy()
    summ.append({'source': s, 'year': yr, 'variable': 'records_and_weight', 'records': len(g),
                 'weighted_count': w_all.sum()})
    for v in AMOUNTS:
        x = g[v].to_numpy(dtype=float)
        band, rec = band_variable(x, w_all)
        df.loc[g.index, f'{v}_band'] = band
        edges[f'{s}/{yr}/{v}'] = rec
        ok = ~np.isnan(x)
        xo, wo = x[ok], w_all[ok]
        row = {'source': s, 'year': yr, 'variable': v, 'records': int(ok.sum()), 'na_records': int((~ok).sum()),
               'weighted_count': wo.sum() if ok.any() else 0.0}
        if ok.any():
            row.update({'weighted_total_billions': (wo * xo).sum() / 1e9,
                        'share_zero_weighted': wo[xo == 0].sum() / wo.sum(),
                        'share_zero_records': float((xo == 0).mean()),
                        'share_negative_weighted': wo[xo < 0].sum() / wo.sum(),
                        'share_positive_weighted': wo[xo > 0].sum() / wo.sum()})
            for p, q in zip(['p50', 'p90', 'p99', 'p999'], wquantile(xo, wo, [0.5, 0.9, 0.99, 0.999])):
                row[p] = q
            if (xo > 0).any():
                for p, q in zip(['pos_p50', 'pos_p90', 'pos_p99', 'pos_p999'],
                                wquantile(xo[xo > 0], wo[xo > 0], [0.5, 0.9, 0.99, 0.999])):
                    row[p] = q
        summ.append(row)
        if not ok.any():
            continue
        cnt = np.array([b['records'] for b in rec['bands']])
        pc = np.array(rec['positive']['cuts']) if 'cuts' in rec.get('positive', {}) else np.array([])
        nc = np.array(rec['negative']['cuts'])
        pos_w = np.array([b['weight'] for b in rec['bands'][POS0:]])
        widths = np.diff(np.concatenate([[0], POS_PROBS, [1]]))
        lv, ls = lump(xo[xo > 0], wo[xo > 0])
        nv, ns = lump(xo[xo < 0], wo[xo < 0])
        rep.append({'source': s, 'year': yr, 'variable': v,
                    'negative_records': int((xo < 0).sum()),
                    'negative_bands_used': rec['negative']['bands_used'],
                    'negative_bands_empty_of_used': int((cnt[NEG0:NEG0 + rec['negative']['bands_used']] == 0).sum()),
                    'negative_duplicate_cuts': int((np.diff(nc) == 0).sum()) if len(nc) > 1 else 0,
                    'positive_records': int((xo > 0).sum()),
                    'positive_bands_empty': int((cnt[POS0:] == 0).sum()) if (xo > 0).any() else N_POS,
                    'positive_duplicate_cuts': int((np.diff(pc) == 0).sum()) if len(pc) > 1 else 0,
                    'positive_min_records_in_nonempty_band': int(cnt[POS0:][cnt[POS0:] > 0].min()) if (xo > 0).any() else 0,
                    'positive_max_weight_over_nominal': float((pos_w / pos_w.sum() / widths).max()) if (xo > 0).any() else np.nan,
                    'largest_positive_lump_value': lv, 'largest_positive_lump_share': ls,
                    'largest_negative_lump_value': nv, 'largest_negative_lump_share': ns,
                    'band1_min': rec['bands'][1]['min'], 'band1_max': rec['bands'][1]['max'],
                    'band1_records': rec['bands'][1]['records']})

for v in AMOUNTS:
    df[f'{v}_band'] = df[f'{v}_band'].astype('int16')   # -1 = amount is NA
    assert df[f'{v}_band'].between(-1, N_BANDS - 1).all()
    assert ((df[f'{v}_band'] == -1) == df[v].isna()).all()

# demographics, for the report
for (s, yr), g in df.groupby(['source', 'year']):
    w = g['weight']
    row = {'source': s, 'year': yr, 'variable': 'demographics', 'married_share_weighted': (w * g.married).sum() / w.sum()}
    for k in range(4):
        row[f'n_dep_{k}_share_weighted'] = w[g.n_dep == k].sum() / w.sum()
    for sp in ['train', 'validation', 'test']:
        row[f'split_{sp}_records'] = int((g.split == sp).sum())
        row[f'split_{sp}_weight_share'] = w[g.split == sp].sum() / w.sum()
    summ.append(row)

cols = ['source', 'year', 'record_id', 'household_id', 'implicate', 'split', 'weight', 'married', 'n_dep'] + \
       AMOUNTS + [f'{v}_band' for v in AMOUNTS]
df[cols].to_parquet(f'{D}/stacked{SUF}.parquet', index=False)
meta = {'layout': '84 bands: 0 = exactly zero; 1-10 negatives (1 = most negative); 11-83 positives; -1 in the table = amount NA',
        'cut_rule': 'cut = smallest value whose cumulative weight share reaches the probability; a value equal to a cut goes to the lower band',
        'dollars': '2015 dollars', 'split_seed': SEED}
if YEARS:
    meta.update({'keys': 'source/tax year/variable; bands computed within source and tax year',
                 'dollars': 'incomes in the tax year own dollars; SCF net worth in the interview year dollars (tax year + 1)',
                 'scf_ids': 'record_id and household_id offset by 1e8 x (year - 2015) for tax years 2018 and 2021',
                 'split_seed': 'PUF and tax year 2015 SCF: labels of stacked.parquet; SCF tax year t: household split with seed 20261014 + (t - 2014)'})
with open(f'{D}/band_edges{SUF}.json', 'w') as f:
    json.dump({'meta': meta, **edges}, f, indent=1)
pd.DataFrame(summ).to_csv(f'{D}/summary{SUF}.csv', index=False)
pd.DataFrame(rep).to_csv(f'{D}/band_report{SUF}.csv', index=False)

pd.set_option('display.width', 250); pd.set_option('display.max_columns', 40)
print(pd.DataFrame(summ).to_string())
print(pd.DataFrame(rep).to_string())
print(f'01_band: wrote {len(df)} rows')
