"""
10_distort.py  (minimal example, translation experiments, Set A; 2026-10-15)

What it does. The SCF 2016 wave stands in for a third survey with a known
distortion (10_TRANSLATION_EXPERIMENTS.md, Set A). Starting from
stacked.parquet (not changed), it distorts the SCF's four shared amounts
(wages, interest, dividends, capital gains) in one of five ways, then bands
the table under three rules, and writes one table per rule.

Distortions (SCF rows only; PUF rows, every other column and the record ids,
split labels and weights are those of stacked.parquet):
  D1  scale: the four amounts times 0.5.
  D2  uneven scale: wages x 0.9, interest x 0.5, dividends x 0.5, capital
      gains x 2.
  D3  definition: wages replaced by wages plus business and farm income
      (SCFP variable BUSSEFARMINC, read from data/scfp2016_rawincome.csv,
      joined by the SCF implicate id Y1 = record_id // 100, checked against
      household_id and implicate). BUSSEFARMINC is a household amount in 2022
      dollars; it is put in 2015 dollars with the wage factor of step 1
      (checks_r.csv, WAGEINC / X5702) and shared among the household's tax
      units in that implicate in proportion to their wages; where those units
      have no wages it goes to the unit with the lowest record id. A sum below
      zero (a business loss larger than wages) is set to zero, because the
      PUF's wages are never negative and so have no negative cut points.
  D4  rounding: each amount rounded to the nearest $1,000, halves away from
      zero (so any amount under $500 in size becomes zero).
  D5  top-coding: each amount capped at the PUF's weighted 99th percentile of
      that variable over all PUF units (zeros included); capital gains losses
      also capped at the PUF's 1st percentile.

Banding rules (84-band layout of 01_band.py; its functions are loaded from
that file, not copied):
  R0  within source: every amount banded on its own source's cut points.
  R1  PUF cut points: the four shared amounts of SCF rows banded on the PUF's
      cut points; other_agi on PUF cuts (PUF only), net_worth on SCF cuts
      (SCF only). As 01_variants.py table E4.
  R2  10_translate.py reads the distorted table and writes, per shared
      variable, a case and a factor (data/translation_<D>.json). Where the
      case is "scale", the SCF's amounts are divided by the factor; then the
      table is banded as R1.

The coarse_hi / coarse_neg columns are copied through unchanged; they are
read by the model only with MINIMAL_COARSEN=1, which these runs do not set.

Usage:  python research/rebuild/minimal/10_distort.py D1 [D2 ...]
        (through Slurm: sbatch research/rebuild/minimal/10_distort.sbatch D1)
Writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/):
  data/stacked_A_<D>_<R>.parquet, data/band_edges_A_<D>_<R>.json
  data/translation_<D>.json, score/detection_<D>.md   (10_translate.py)
  data/distort_<D>_note.json   what the distortion changed (row counts,
                               dollar totals, caps, D3 join checks)
  markers/TABLE_A_<D>_<R>_DONE only after the checks pass
"""
import ast
import importlib.util
import json
import os
import sys
import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
DATA = f'{R}/data'
M = f'{R}/markers'

# Load 01_band.py's constants and functions only (as 01_variants.py does).
_band_file = f'{HERE}/01_band.py'
tree = ast.parse(open(_band_file).read())
_stop = next(i for i, n in enumerate(tree.body) if 'read_csv' in ast.unparse(n))
keep = [n for n in tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
        or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
            and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=keep, type_ignores=[]), _band_file, 'exec'), B)
band_variable, wquantile, N_BANDS, POS0, NEG0 = (B[k] for k in ['band_variable', 'wquantile', 'N_BANDS', 'POS0', 'NEG0'])
assert N_BANDS == 84

_spec = importlib.util.spec_from_file_location('translate10', f'{HERE}/10_translate.py')
TR = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(TR)

SHARED = ['wages', 'interest', 'dividends', 'capital_gains']
AMOUNTS = SHARED + ['other_agi', 'net_worth']
BASE = ['source', 'year', 'record_id', 'household_id', 'implicate', 'split', 'weight', 'married', 'n_dep']
src = pd.read_parquet(f'{DATA}/stacked.parquet')
COARSE_COLS = [c for c in src.columns if c.endswith('_coarse_hi') or c.endswith('_coarse_neg')]
PUF = (src.source == 'puf').to_numpy()
SCF = ~PUF
chk = pd.read_csv(f'{DATA}/checks_r.csv')   # columns group, key, value
WAGE_FACTOR = float(chk.loc[chk['key'] == 'WAGEINC_over_X5702_median_used', 'value'].iloc[0])

DISTORTION_TEXT = {
    'D1': 'SCF wages, interest, dividends and capital gains times 0.5.',
    'D2': 'SCF wages x 0.9, interest x 0.5, dividends x 0.5, capital gains x 2.',
    'D3': 'SCF wages replaced by wages plus business and farm income (BUSSEFARMINC), floored at zero.',
    'D4': 'SCF amounts rounded to the nearest $1,000 (under $500 becomes zero).',
    'D5': "SCF amounts capped at the PUF's weighted 99th percentile (capital gains losses at the 1st percentile).",
}
RULE_TEXT = {
    'R0': 'within-source cut points (the v2 rule)',
    'R1': "PUF cut points for the four shared variables, no translation (as E4)",
    'R2': "10_translate.py's decision applied, then PUF cut points",
}


def round_1000(x):
    return np.sign(x) * np.floor(np.abs(x) / 1000 + 0.5) * 1000


def d3_business(df, note):
    """Household business and farm income, 2015 dollars, shared among the
    household's units in each implicate; returns the amount per row (0 on PUF)."""
    raw = pd.read_csv(f'{DATA}/scfp2016_rawincome.csv', usecols=['YY1', 'Y1', 'BUSSEFARMINC'])
    assert not raw['Y1'].duplicated().any() and not raw.isna().any().any()
    s = df.loc[SCF, ['record_id', 'household_id', 'implicate', 'wages']].copy()
    s['Y1'] = (s['record_id'] // 100).astype('int64')
    m = s.merge(raw, on='Y1', how='left', validate='many_to_one')
    assert m['YY1'].notna().all(), 'SCF rows with no SCFP implicate row'
    assert (m['YY1'].to_numpy() == s['household_id'].to_numpy()).all(), 'Y1 does not belong to household_id'
    imp_ok = float((m['Y1'] - 10 * m['YY1'] == m['implicate']).mean())
    note['d3_check_Y1_minus_10xYY1_equals_implicate_share'] = imp_ok
    bus = m['BUSSEFARMINC'].to_numpy(float) / WAGE_FACTOR
    # share among the units of the same Y1: by wages, else all to the lowest record id
    g = pd.DataFrame({'Y1': m['Y1'].to_numpy(), 'rid': s['record_id'].to_numpy(),
                      'wages': s['wages'].to_numpy(float)})
    tot_w = g.groupby('Y1')['wages'].transform('sum').to_numpy()
    first = (g.groupby('Y1')['rid'].transform('min').to_numpy() == g['rid'].to_numpy())
    share = np.where(tot_w > 0, g['wages'].to_numpy() / np.where(tot_w > 0, tot_w, 1), first.astype(float))
    alloc = bus * share
    # the household amount is kept exactly, per implicate
    hh = pd.DataFrame({'Y1': g['Y1'], 'a': alloc}).groupby('Y1')['a'].sum()
    ref = (raw.set_index('Y1')['BUSSEFARMINC'] / WAGE_FACTOR).loc[hh.index]
    assert np.allclose(hh.to_numpy(), ref.to_numpy(), rtol=1e-9, atol=1e-6)
    note['d3_units_per_implicate_household'] = {str(k): int(v) for k, v in g.groupby('Y1').size().value_counts().items()}
    note['d3_wage_factor_2022_to_2015'] = WAGE_FACTOR
    out = np.zeros(len(df))
    out[SCF] = alloc
    return out


def distort(D):
    df = src[BASE + AMOUNTS + COARSE_COLS].copy()
    note = {'distortion': D, 'text': DISTORTION_TEXT[D]}
    before = {v: df.loc[SCF, v].to_numpy(float).copy() for v in SHARED}
    w = df.loc[SCF, 'weight'].to_numpy()
    if D == 'D1':
        for v in SHARED:
            df.loc[SCF, v] = before[v] * 0.5
    elif D == 'D2':
        for v, f in zip(SHARED, [0.9, 0.5, 0.5, 2.0]):
            df.loc[SCF, v] = before[v] * f
    elif D == 'D3':
        bus = d3_business(df, note)[SCF]
        raw_sum = before['wages'] + bus
        df.loc[SCF, 'wages'] = np.maximum(raw_sum, 0.0)
        note['d3_rows_with_business_income_nonzero'] = int((bus != 0).sum())
        note['d3_rows_floored_at_zero'] = int((raw_sum < 0).sum())
        note['d3_weighted_share_floored'] = float(w[raw_sum < 0].sum() / w.sum())
        note['d3_business_total_bn'] = float((w * bus).sum() / 1e9)
        note['d3_zero_wage_rows_made_positive'] = int(((before['wages'] == 0) & (raw_sum > 0)).sum())
    elif D == 'D4':
        for v in SHARED:
            df.loc[SCF, v] = round_1000(before[v])
    elif D == 'D5':
        caps = {}
        for v in SHARED:
            x, wp = src.loc[PUF, v].to_numpy(float), src.loc[PUF, 'weight'].to_numpy()
            hi = float(wquantile(x, wp, [0.99])[0])
            lo = float(wquantile(x, wp, [0.01])[0]) if v == 'capital_gains' else -np.inf
            caps[v] = {'cap_p99': hi, 'floor_p1': lo if np.isfinite(lo) else None}
            df.loc[SCF, v] = np.clip(before[v], lo, hi)
        note['d5_caps'] = caps
    else:
        raise ValueError(D)
    note['scf_rows'] = int(SCF.sum())
    for v in SHARED:
        after = df.loc[SCF, v].to_numpy(float)
        note[f'{v}_scf_rows_changed'] = int((after != before[v]).sum())
        note[f'{v}_scf_total_bn_before'] = float((w * before[v]).sum() / 1e9)
        note[f'{v}_scf_total_bn_after'] = float((w * after).sum() / 1e9)
        note[f'{v}_scf_holder_share_before'] = float(w[before[v] != 0].sum() / w.sum())
        note[f'{v}_scf_holder_share_after'] = float(w[after != 0].sum() / w.sum())
    assert df.loc[PUF].equals(src.loc[PUF, df.columns]), 'PUF rows changed'
    return df, note


def band_all(df, fixed_from_puf=()):
    """Band each amount within source; for `fixed_from_puf`, the SCF uses the PUF's cuts."""
    edges = {}
    for v in AMOUNTS:
        df[f'{v}_band'] = np.int16(-1)
    for (s, yr), g in sorted(df.groupby(['source', 'year']), key=lambda t: t[0][0] != 'puf'):
        wg = g['weight'].to_numpy()
        for v in AMOUNTS:
            x = g[v].to_numpy(dtype=float)
            fixed = edges[f'puf/{yr}/{v}'] if (s == 'scf' and v in fixed_from_puf) else None
            band, rec = band_variable(x, wg, fixed)
            df.loc[g.index, f'{v}_band'] = band
            edges[f'{s}/{yr}/{v}'] = rec
    for v in AMOUNTS:
        df[f'{v}_band'] = df[f'{v}_band'].astype('int16')
    return edges


def check(T, df):
    assert len(df) == len(src)
    for c in BASE + COARSE_COLS + ['other_agi', 'net_worth']:
        assert df[c].equals(src[c]), f'{T}: {c} differs from stacked.parquet'
    # PUF rows: amounts and bands exactly v2's (the PUF is never changed and is always banded on its own cuts)
    for v in AMOUNTS:
        assert (df.loc[PUF, v].fillna(-9e99) == src.loc[PUF, v].fillna(-9e99)).all(), (T, v)
        assert (df.loc[PUF, f'{v}_band'] == src.loc[PUF, f'{v}_band']).all(), f'{T}: PUF {v} bands differ from v2'
    for v in ['other_agi', 'net_worth']:
        assert (df[f'{v}_band'] == src[f'{v}_band']).all(), f'{T}: {v} bands differ from v2'
    for v in AMOUNTS:
        b = df[f'{v}_band']
        assert ((b == -1) == df[v].isna()).all(), f'{T}: {v} band -1 does not match NA amount'
        assert b.between(-1, N_BANDS - 1).all(), T
        assert ((b == 0) == (df[v] == 0)).all(), f'{T}: {v} band 0 does not match zero amount'
        for s, g in df[df[v].notna()].groupby('source'):
            key = np.where(g[f'{v}_band'] == 0, NEG0 + 9.5, g[f'{v}_band'])
            mx = g.groupby(key)[v].agg(['min', 'max']).sort_index()
            assert (mx['max'].to_numpy()[:-1] <= mx['min'].to_numpy()[1:]).all(), f'{T}: {v} {s} bands overlap'
    assert (df.loc[SCF, 'other_agi_band'] == -1).all() and (df.loc[PUF, 'net_worth_band'] == -1).all(), T
    print(f'{T}: checks passed', flush=True)


def write(T, df, edges, meta):
    cols = BASE + AMOUNTS + [f'{v}_band' for v in AMOUNTS] + COARSE_COLS
    assert list(src.columns) == cols or set(src.columns) == set(cols), (src.columns, cols)
    df[list(src.columns)].to_parquet(f'{DATA}/stacked_{T}.parquet', index=False)
    json.dump({'meta': meta, **edges}, open(f'{DATA}/band_edges_{T}.json', 'w'), indent=1)
    os.makedirs(M, exist_ok=True)
    open(f'{M}/TABLE_{T}_DONE', 'w').close()
    # what the SCF rows' shared bands look like: records in the top bands and bands used
    for v in SHARED:
        bb = df.loc[SCF, f'{v}_band'].to_numpy()
        print(f'{T} {v}: SCF bands used {len(np.unique(bb))}, SCF records in the top 19 positive bands '
              f'{int((bb >= POS0 + 54).sum())}', flush=True)
    print(f'{T}: written', flush=True)


def sanity_e4():
    """The R1 banding on the undistorted table must reproduce stacked_E4."""
    e4 = pd.read_parquet(f'{DATA}/stacked_E4.parquet')
    df = src[BASE + AMOUNTS + COARSE_COLS].copy()
    band_all(df, fixed_from_puf=SHARED)
    for v in AMOUNTS:
        assert (df[f'{v}_band'].to_numpy() == e4[f'{v}_band'].to_numpy()).all(), f'R1 on v2 dollars != E4 for {v}'
    print('sanity: R1 on the undistorted table reproduces stacked_E4 bands exactly', flush=True)


def build(D):
    df0, note = distort(D)
    json.dump(note, open(f'{DATA}/distort_{D}_note.json', 'w'), indent=1)
    print(json.dumps(note, indent=1), flush=True)
    base_meta = {'set': 'A', 'distortion': D, 'distortion_text': DISTORTION_TEXT[D],
                 'layout': '84 bands; -1 = amount NA', 'note': note}
    # R0
    T = f'A_{D}_R0'
    df = df0.copy()
    e = band_all(df)
    check(T, df)
    if D == 'D1':   # a uniform scaling keeps every rank, so the SCF bands are v2's
        for v in SHARED:
            assert (df[f'{v}_band'].to_numpy() == src[f'{v}_band'].to_numpy()).all(), f'D1 R0: {v} bands differ from v2'
        print('D1 R0: every band equals v2 (stacked.parquet), as it must', flush=True)
    write(T, df, e, {**base_meta, 'rule': 'R0', 'rule_text': RULE_TEXT['R0']})
    # R1
    T = f'A_{D}_R1'
    df = df0.copy()
    e = band_all(df, fixed_from_puf=SHARED)
    check(T, df)
    write(T, df, e, {**base_meta, 'rule': 'R1', 'rule_text': RULE_TEXT['R1']})
    # R2: translate, then PUF cuts
    res = TR.write(D, df0, DISTORTION_TEXT[D])
    T = f'A_{D}_R2'
    df = df0.copy()
    for v in SHARED:
        if res[v]['case'] == 'scale':
            df.loc[SCF, v] = df0.loc[SCF, v].to_numpy(float) / res[v]['factor']
    e = band_all(df, fixed_from_puf=SHARED)
    check(T, df)
    write(T, df, e, {**base_meta, 'rule': 'R2', 'rule_text': RULE_TEXT['R2'], 'translation': res})


if __name__ == '__main__':
    pd.set_option('display.width', 250)
    if 'D1' in sys.argv[1:]:
        sanity_e4()
    for D in sys.argv[1:]:
        print(f'==== {D} ====', flush=True)
        build(D)
