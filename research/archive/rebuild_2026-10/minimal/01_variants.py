"""
01_variants.py  (minimal example, step 1; the data-side tables of the
bridge-variable grid, 09_BRIDGE_EXPERIMENTS.md, E1 to E6; 2026-10-15)

What it does: builds variants of the single-year table (PUF 2015 + SCF 2016
wave, tax year 2015) from stacked.parquet, which it does not change. Record
ids, split labels, weights and dollar amounts come from stacked.parquet; only
the amounts a variant deliberately changes, and the band columns, differ. The
band columns are recomputed with 01_band.py's own functions (loaded from that
file, not copied), 84-band layout. The coarse_hi / coarse_neg columns of
stacked.parquet are copied through unchanged (E6: set to -1 on the SCF rows
it turns into holders, since those rows no longer report zero).

The dollar lines ("E2 lines") are the 2015 ones in coarsen_lines.json:
interest $110, dividends $290, capital gains $2,650, on the absolute amount.

  E1  production's threshold. PUF interest, dividends, capital gains set to 0
      where 0 < |amount| < line. SCF unchanged. Banded within source.
  E2  split (formerly table A). <var> = amount if |amount| >= line else 0, both
      sources; <var>_small = amount if 0 < |amount| < line else 0 on PUF rows,
      NA on SCF rows. All banded within source. PUF: <var> + <var>_small =
      the original amount, checked exactly.
  E3  E2 with the line raised, from the detection-table rule: the lowest
      dollar bracket (01_detection_table.py brackets) above the E2 line where
      the SCF/PUF share in the bracket is 0.9 or more and the within-bracket
      means agree within 10%; the line is that bracket's lower edge. If no
      bracket qualifies, five times the E2 line. Override with the
      environment variable E3_LINES (JSON, e.g. '{"interest": 500, ...}').
  E4  common dollar scale (formerly table B). v2 columns; wages, interest,
      dividends and capital gains banded on the PUF's cut points for both
      sources. other_agi on PUF cuts, net_worth on SCF cuts.
  E5  E2's split, with wages and the three shared parts banded on the PUF's
      cut points for both sources; the small parts banded within source (PUF
      only).
  E6  D23 translation. For each of the three variables, on the SCF: a weighted
      logistic model of holding is fit on the PUF (wage band, married, number
      of dependents, holding of the other two variables), SCF non-holders are
      scored, and the highest-scoring are turned into holders until the SCF's
      weighted holder share equals the PUF's. Each gets an amount drawn
      (weighted, fixed seed) from PUF holders in the same cell (wage group,
      married, holds either other item) with |amount| under the E2 line.
      Holding of the other two items is the SCF's as reported (before any
      flip). Then everything is banded within source. Writes data/E6_NOTE.md.

Usage: python research/rebuild/minimal/01_variants.py E1 E2 E4 ...  (any of E1-E6)
Run through Slurm: sbatch research/rebuild/minimal/01_variants.sbatch E1 E2 E4

Writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/):
  data/stacked_<T>.parquet, data/band_edges_<T>.json, data/band_report_<T>.csv
  data/variants_summary.csv   one row per table and variable: the dollar line,
                              holder shares by source before and after
  markers/TABLE_<T>_DONE      only after the checks pass
What to look at: variants_summary.csv; band_report_<T>.csv (empty bands,
records in the top bands).
"""
import ast
import fcntl
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

# Load 01_band.py's constants and functions only (its top-level script part,
# from `raw = pd.read_csv(...)` on, is not run).
tree = ast.parse(open(HERE).read())
_stop = next(i for i, n in enumerate(tree.body) if 'read_csv' in ast.unparse(n))   # the script part starts here
keep = [n for n in tree.body[:_stop] if isinstance(n, (ast.Import, ast.ImportFrom, ast.FunctionDef))
        or (isinstance(n, (ast.Assign, ast.Assert)) and 'sys.argv' not in ast.unparse(n)
            and 'SUF' not in ast.unparse(n))]
B = {}
exec(compile(ast.Module(body=keep, type_ignores=[]), HERE, 'exec'), B)
band_variable, wquantile, N_BANDS, POS0, NEG0 = (B[k] for k in ['band_variable', 'wquantile', 'N_BANDS', 'POS0', 'NEG0'])
assert N_BANDS == 84

VARS = ['interest', 'dividends', 'capital_gains']
SHARED = ['wages'] + VARS
AMOUNTS = ['wages'] + VARS + ['other_agi', 'net_worth']
BASE = ['source', 'year', 'record_id', 'household_id', 'implicate', 'split', 'weight', 'married', 'n_dep']
src = pd.read_parquet(f'{D}/stacked.parquet')
COARSE_COLS = [c for c in src.columns if c.endswith('_coarse_hi') or c.endswith('_coarse_neg')]
lines = json.load(open(f'{D}/coarsen_lines.json'))['single']
LINE = {v: lines[f'scf/2015/{v}']['f'] for v in VARS}
PUF = (src.source == 'puf').to_numpy()
print('E2 dollar lines:', LINE, '; coarse columns copied through:', COARSE_COLS)


def holder_share(df, v, s):
    g = df[df.source == s]
    x, w = g[v].to_numpy(dtype=float), g['weight'].to_numpy()
    ok = ~np.isnan(x)
    return float(w[ok & (x != 0)].sum() / w[ok].sum()) if ok.any() else np.nan


def band_all(df, amounts, fixed_from_puf=()):
    """Band each amount within source and year; for `fixed_from_puf`, the SCF uses the PUF's cuts."""
    edges, rep = {}, []
    for v in amounts:
        df[f'{v}_band'] = np.int16(-1)
    for (s, yr), g in sorted(df.groupby(['source', 'year']), key=lambda t: t[0][0] != 'puf'):
        w = g['weight'].to_numpy()
        for v in amounts:
            x = g[v].to_numpy(dtype=float)
            fixed = edges[f'puf/{yr}/{v}'] if (s == 'scf' and v in fixed_from_puf) else None
            band, rec = band_variable(x, w, fixed)
            df.loc[g.index, f'{v}_band'] = band
            edges[f'{s}/{yr}/{v}'] = rec
            if np.isnan(x).all():
                continue
            cnt = np.array([b['records'] for b in rec['bands']])
            k = rec['negative']['bands_used']
            r = {'source': s, 'year': yr, 'variable': v, 'cuts_from_puf': fixed is not None,
                 'nonempty_bands': int((cnt > 0).sum()),
                 'positive_bands_empty': int((cnt[POS0:] == 0).sum()),
                 'negative_bands_empty_of_used': int((cnt[NEG0:NEG0 + k] == 0).sum()),
                 'records_in_bands_below_puf_p99': int(cnt[POS0:POS0 + 54].sum())}
            for b in range(POS0 + 54, N_BANDS):   # the top 19 positive bands: 99th percentile up
                r[f'records_band_{b}'] = int(cnt[b])
            rep.append(r)
    for v in amounts:
        df[f'{v}_band'] = df[f'{v}_band'].astype('int16')
    return edges, rep


def split(df, line):
    """Shared part at or above the line (both sources), PUF-owned part below it."""
    for v in VARS:
        amt = src[v].to_numpy(dtype=float)
        small = (amt != 0) & (np.abs(amt) < line[v])
        df[f'{v}_small'] = np.where(PUF, np.where(small, amt, 0.0), np.nan)
        df[v] = np.where(small, 0.0, amt)
    return df


def check(T, df, amounts, changed, split_vars=()):
    """Stops on any failure; returns nothing. `changed`: amounts the variant may alter."""
    assert len(df) == len(src)
    for c in BASE:
        assert df[c].equals(src[c]), f'{T}: {c} differs from stacked.parquet'
    for c in COARSE_COLS:
        if T != 'E6':
            assert df[c].equals(src[c]), f'{T}: {c} differs'
    for v in AMOUNTS:
        if v not in changed:
            assert df[v].equals(src[v]), f'{T}: {v} changed but should not'
    for v in split_vars:
        p = PUF
        tot = df.loc[p, v].to_numpy() + df.loc[p, f'{v}_small'].to_numpy()
        assert (tot == src.loc[p, v].to_numpy()).all(), f'{T}: {v} PUF parts do not sum to the original'
        assert df.loc[~p, f'{v}_small'].isna().all() and df.loc[p, f'{v}_small'].notna().all(), T
        assert (df.loc[p, f'{v}_small'].abs() < line_of[T][v]).all(), T
        assert ((df.loc[:, v] == 0) | (df.loc[:, v].abs() >= line_of[T][v])).all(), T
    for v in amounts:
        b = df[f'{v}_band']
        assert ((b == -1) == df[v].isna()).all(), f'{T}: {v} band -1 does not match NA amount'
        assert b.between(-1, N_BANDS - 1).all(), T
        assert ((b == 0) == (df[v] == 0)).all(), f'{T}: {v} band 0 does not match zero amount'
        # bands in dollar order within source
        for s, g in df[df[v].notna()].groupby('source'):
            key = np.where(g[f'{v}_band'] == 0, NEG0 + 9.5, g[f'{v}_band'])   # zero sits between negatives and positives
            mx = g.groupby(key)[v].agg(['min', 'max']).sort_index()
            assert (mx['max'].to_numpy()[:-1] <= mx['min'].to_numpy()[1:]).all(), f'{T}: {v} {s} bands overlap'
    # owned slots absent on the other source
    for v in amounts:
        if v.endswith('_small') or v == 'other_agi':
            assert (df.loc[~PUF, f'{v}_band'] == -1).all(), (T, v)
        if v == 'net_worth':
            assert (df.loc[PUF, f'{v}_band'] == -1).all(), (T, v)
    print(f'{T}: checks passed')


def write(T, df, amounts, edges, rep, meta, summ_rows):
    cols = BASE + amounts + [f'{v}_band' for v in amounts] + COARSE_COLS
    df[cols].to_parquet(f'{D}/stacked_{T}.parquet', index=False)
    json.dump({'meta': meta, **edges}, open(f'{D}/band_edges_{T}.json', 'w'), indent=1)
    pd.DataFrame(rep).to_csv(f'{D}/band_report_{T}.csv', index=False)
    # one row per table and variable in variants_summary.csv (replaces this table's old rows)
    path = f'{D}/variants_summary.csv'
    with open(f'{D}/.variants_summary.lock', 'w') as lk:
        fcntl.flock(lk, fcntl.LOCK_EX)
        old = pd.read_csv(path) if os.path.exists(path) else pd.DataFrame()
        if len(old):
            old = old[old['table'] != T]
        pd.concat([old, pd.DataFrame(summ_rows)], ignore_index=True).to_csv(path, index=False)
    os.makedirs(M, exist_ok=True)
    open(f'{M}/TABLE_{T}_DONE', 'w').close()
    print(pd.DataFrame(summ_rows).to_string())
    print(pd.DataFrame(rep).iloc[:, :9].to_string())
    print(f'{T}: written')


def summary(T, df, line, cuts, notes=None):
    rows = []
    for v in SHARED:
        r = {'table': T, 'variable': v, 'dollar_line': line.get(v, np.nan) if line else np.nan,
             'puf_holder_share_before': holder_share(src, v, 'puf'),
             'puf_holder_share_after': holder_share(df, v, 'puf'),
             'scf_holder_share_before': holder_share(src, v, 'scf'),
             'scf_holder_share_after': holder_share(df, v, 'scf'),
             'puf_small_part_holder_share': holder_share(df, f'{v}_small', 'puf') if f'{v}_small' in df else np.nan,
             'shared_bands': cuts, 'note': (notes or {}).get(v, '')}
        rows.append(r)
    return rows


line_of = {}


# ---- the detection rule for E3 ------------------------------------------------
LOWER = [100, 500, 2_000, 10_000, 50_000, 200_000]   # as in 01_detection_table.py


def detection_lines():
    out, notes = {}, {}
    for v in VARS:
        st = {}
        for s in ['puf', 'scf']:
            g = src[src.source == s]
            a, w = g[v].abs().to_numpy(), g['weight'].to_numpy()
            b = np.where(a == 0, 0, 1 + np.searchsorted(LOWER, a, side='right'))
            st[s] = (np.array([w[b == k].sum() / w.sum() for k in range(8)]),
                     np.array([np.average(a[b == k], weights=w[b == k]) if (b == k).any() else np.nan for k in range(8)]))
        ratio, mratio = st['scf'][0] / st['puf'][0], st['scf'][1] / st['puf'][1]
        print(f'E3 detection, {v}: bracket share SCF/PUF', np.round(ratio, 3), 'mean SCF/PUF', np.round(mratio, 3))
        pick = None
        for k in range(2, 8):                    # brackets with a lower edge LOWER[k-2]
            edge = LOWER[k - 2]
            if edge > LINE[v] and ratio[k] >= 0.9 and abs(mratio[k] - 1) <= 0.10:
                pick = edge
                break
        if pick is None:
            out[v], notes[v] = 5 * LINE[v], 'no bracket met the rule: five times the E2 line'
        else:
            out[v] = float(pick)
            notes[v] = f'detection rule: lowest bracket above the E2 line with share ratio >= 0.9 and means within 10%: lower edge {pick:,}'
    if os.environ.get('E3_LINES'):
        ov = json.loads(os.environ['E3_LINES'])
        for v, f in ov.items():
            out[v], notes[v] = float(f), f'set by hand from 01_DETECTION_TABLE.md (E3_LINES): {f:,}'
    return out, notes


# ---- builders -----------------------------------------------------------------
def build_E1():
    T = 'E1'
    df = src[BASE + AMOUNTS + COARSE_COLS].copy()
    for v in VARS:
        amt = df[v].to_numpy(dtype=float)
        df[v] = np.where(PUF & (amt != 0) & (np.abs(amt) < LINE[v]), 0.0, amt)
    e, r = band_all(df, AMOUNTS)
    check(T, df, AMOUNTS, changed=VARS)
    assert (df.loc[~PUF, VARS].equals(src.loc[~PUF, VARS]))
    write(T, df, AMOUNTS, e, r, {'table': 'E1: PUF amounts under the 2015 dollar lines set to zero; SCF unchanged',
                                 'lines': LINE, 'layout': '84 bands; -1 = amount NA'},
          summary(T, df, LINE, 'within source'))


def build_split(T, line, fixed, meta_text, notes=None):
    line_of[T] = line
    df = split(src[BASE + AMOUNTS + COARSE_COLS].copy(), line)
    amounts = ['wages', 'interest', 'interest_small', 'dividends', 'dividends_small',
               'capital_gains', 'capital_gains_small', 'other_agi', 'net_worth']
    e, r = band_all(df, amounts, fixed_from_puf=fixed)
    check(T, df, amounts, changed=VARS, split_vars=VARS)
    scf_lost = {v: float((src.weight * np.where(~PUF & (src[v] != 0) & (src[v].abs() < line[v]),
                                                 src[v].abs(), 0)).sum() / 1e9) for v in VARS}
    nt = {v: (notes or {}).get(v, '') + f'; SCF dollars set to zero below the line: ${scf_lost[v]:.2f}bn' for v in VARS}
    write(T, df, amounts, e, r, {'table': meta_text, 'lines': line, 'layout': '84 bands; -1 = amount NA'},
          summary(T, df, line, 'PUF cut points, both sources' if fixed else 'within source', nt))


def build_E4():
    T = 'E4'
    df = src[BASE + AMOUNTS + COARSE_COLS].copy()
    e, r = band_all(df, AMOUNTS, fixed_from_puf=SHARED)
    check(T, df, AMOUNTS, changed=[])
    write(T, df, AMOUNTS, e, r, {'table': 'E4: shared variables on the PUF 2015 cut points in both sources',
                                 'layout': '84 bands; -1 = amount NA'},
          summary(T, df, {}, 'PUF cut points, both sources'))


def build_E6():
    from sklearn.linear_model import LogisticRegression
    T = 'E6'
    df = src[BASE + AMOUNTS + COARSE_COLS].copy()
    rng = np.random.default_rng(SEED)
    hold = {v: (src[v].to_numpy() != 0) for v in VARS}      # as reported, before any flip
    wb = src['wages_band'].to_numpy()
    # wage groups for the donor cells: zero (or negative) wages, then weighted quintiles of positive wages within source
    wgrp = np.zeros(len(src), dtype=int)
    for s in ['puf', 'scf']:
        m = (src.source == s).to_numpy() & (src.wages.to_numpy() > 0)
        q = wquantile(src.wages.to_numpy()[m], src.weight.to_numpy()[m], [.2, .4, .6, .8])
        wgrp[m] = 1 + np.searchsorted(q, src.wages.to_numpy()[m], side='left')
    levels = np.unique(wb)
    Xw = (wb[:, None] == levels[None, :]).astype(float)
    Xd = (src.n_dep.to_numpy()[:, None] == np.arange(1, 4)[None, :]).astype(float)
    note, info = {}, []
    for v in VARS:
        others = [u for u in VARS if u != v]
        X = np.column_stack([Xw, src.married.to_numpy(), Xd] + [hold[u].astype(float) for u in others])
        w = src.weight.to_numpy()
        lr = LogisticRegression(C=1.0, max_iter=5000)
        lr.fit(X[PUF], hold[v][PUF], sample_weight=w[PUF] / w[PUF].mean())
        p_puf = lr.predict_proba(X[PUF])[:, 1]
        target = holder_share(src, v, 'puf')
        scf = ~PUF
        W = w[scf].sum()
        cur = w[scf & hold[v]].sum() / W
        need = (target - cur) * W
        cand = np.flatnonzero(scf & ~hold[v])
        score = lr.predict_proba(X[cand])[:, 1]
        tieb = rng.random(len(cand))
        order = cand[np.lexsort((tieb, -score))]
        cw = np.cumsum(w[order])
        n = int(np.searchsorted(cw, need))           # cw[n-1] < need <= cw[n]
        if n < len(cw) and abs(cw[n] - need) < abs((cw[n - 1] if n else 0) - need):
            n += 1
        flip = order[:n]
        # amounts: weighted draws from PUF holders under the line, same cell
        anyo = (hold[others[0]] | hold[others[1]]).astype(int)
        cell = wgrp * 4 + src.married.to_numpy() * 2 + anyo
        amt = src[v].to_numpy(dtype=float)
        donor = PUF & (amt != 0) & (np.abs(amt) < LINE[v])
        drawn = np.empty(len(flip))
        fallback = 0
        for c in np.unique(cell[flip]):
            tgt = flip[cell[flip] == c]
            pool = np.flatnonzero(donor & (cell == c))
            if len(pool) < 30:                         # thin cell: wage group only, then all donors
                fallback += len(tgt)
                pool = np.flatnonzero(donor & (wgrp == wgrp[tgt[0]]))
                if len(pool) < 30:
                    pool = np.flatnonzero(donor)
            pw = w[pool] / w[pool].sum()
            drawn[np.isin(flip, tgt)] = amt[rng.choice(pool, size=len(tgt), p=pw)]
        assert (drawn != 0).all() and (np.abs(drawn) < LINE[v]).all()
        col = df[v].to_numpy(dtype=float).copy()
        col[flip] = drawn
        df[v] = col
        for c in [f'{v}_coarse_hi'] + (['capital_gains_coarse_neg'] if v == 'capital_gains' else []):
            if c in df:
                cc = df[c].to_numpy().copy()
                cc[flip] = -1
                df[c] = cc
        after = holder_share(df, v, 'scf')
        dollars = float((w[flip] * drawn).sum() / 1e9)
        info.append({'variable': v, 'puf_share': target, 'scf_share_before': cur, 'scf_share_after': after,
                     'scf_rows_flipped': len(flip), 'scf_weight_flipped_m': float(w[flip].sum() / 1e6),
                     'score_cutoff': float(lr.predict_proba(X[flip[-1:]])[:, 1][0]) if len(flip) else np.nan,
                     'puf_mean_score_holders': float(np.average(p_puf[hold[v][PUF]], weights=w[PUF][hold[v][PUF]])),
                     'mean_drawn_amount': float(np.average(drawn, weights=w[flip])),
                     'dollars_added_bn': dollars, 'rows_in_thin_cells': fallback})
        note[v] = (f'{len(flip)} SCF rows ({w[flip].sum() / 1e6:.1f}m weighted) turned into holders; '
                   f'drawn amounts mean ${info[-1]["mean_drawn_amount"]:.0f}, ${dollars:.2f}bn added')
    e, r = band_all(df, AMOUNTS)
    check(T, df, AMOUNTS, changed=VARS)
    assert df.loc[PUF, VARS].equals(src.loc[PUF, VARS])
    for v in VARS:   # only SCF zeros were changed, and only to amounts under the line
        ch = (~PUF) & (df[v].to_numpy() != src[v].to_numpy())
        assert (src[v].to_numpy()[ch] == 0).all()
    info = pd.DataFrame(info)
    print(info.to_string())
    with open(f'{D}/E6_NOTE.md', 'w') as f:
        f.write('# E6, the D23 translation: what was done\n\n'
                'Written by research/rebuild/minimal/01_variants.py (E6), 2026-10-15.\n\n'
                'For each of interest, dividends and capital gains, separately: a weighted logistic model of '
                '"has a nonzero amount" was fit on the PUF, on the wage band (one indicator per band, bands within '
                'source), married, the number of dependents (indicators for 1, 2, 3+), and whether the unit has the '
                'other two items. SCF rows at zero were scored with it, and the highest-scoring were turned into '
                'holders, one implicate row at a time, until the SCF weighted holder share equalled the PUF\'s (ties '
                f'broken by a seeded random draw, seed {SEED}). Holding of the other two items is the SCF\'s as '
                'reported, before any of these changes. Each converted row got an amount drawn, weighted by the PUF '
                'weight, from PUF holders with an absolute amount under the E2 dollar line '
                f'(interest ${LINE["interest"]:,.0f}, dividends ${LINE["dividends"]:,.0f}, capital gains '
                f'${LINE["capital_gains"]:,.0f}; capital gains donors include small losses) in the same cell: '
                'wage group (zero wages, then quintiles of positive wages within source), married, and whether the '
                'unit has either other item; cells with fewer than 30 donors fall back to the wage group alone. SCF '
                'amounts already nonzero, all PUF rows, and every other column are unchanged, except that the '
                'coarse_hi / coarse_neg columns are set to -1 on converted rows (they no longer report zero). All '
                'amounts were then banded within source (84 bands).\n\n'
                'The five implicates of an SCF household are treated as separate rows, so a household can be '
                'converted in some implicates and not others.\n\n'
                + '```\n' + info.to_string(index=False) + '\n```\n')
    write(T, df, AMOUNTS, e, r, {'table': 'E6: D23 translation; SCF non-holders most likely to hold, by a PUF '
                                          'logistic model, turned into holders with small PUF amounts',
                                 'lines_for_drawn_amounts': LINE, 'layout': '84 bands; -1 = amount NA',
                                 'flips': info.to_dict(orient='records')},
          summary(T, df, LINE, 'within source', note))


for T in sys.argv[1:]:
    print(f'==== {T} ====', flush=True)
    if T == 'E1':
        build_E1()
    elif T == 'E2':
        build_split('E2', LINE, (), 'E2: split at the 2015 dollar lines, within-source bands')
    elif T == 'E3':
        L3, n3 = detection_lines()
        print('E3 lines:', L3)
        build_split('E3', L3, (), 'E3: split at the raised dollar lines (detection rule), within-source bands', n3)
    elif T == 'E4':
        build_E4()
    elif T == 'E5':
        build_split('E5', LINE, ('wages', 'interest', 'dividends', 'capital_gains'),
                    'E5: split at the 2015 dollar lines; wages and shared parts on PUF cut points, small parts within source')
    elif T == 'E6':
        build_E6()
    else:
        raise ValueError(T)
