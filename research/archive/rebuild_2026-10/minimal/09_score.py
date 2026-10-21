"""
09_score.py  (minimal example, step 9: the scoreboard for the bridge-variable grid)

Usage (through Slurm, 09_score.sbatch):
  python 09_score.py <T>            score run T (E0 = the v2 model, E1..E10)
  python 09_score.py E11            tilt only: the E0 file tilted to the six
                                    wealth targets plus mean net worth by wage
                                    group from the 2019 SCF tax units
  python 09_score.py E12 <T> <label>  the best run T, its file under <label>
                                    ('puf' or 'scf') tilted to the 136 IRS
                                    targets plus the six wealth targets; read
                                    from R/score/<T>/score.json, nothing rerun

The scoreboard (09_BRIDGE_EXPERIMENTS.md, "The scoreboard"), one per run:

  1. Wealth on PUF-style records against the actual SCF (2016 wave, 2015
     dollars, survey weights): total, mean, median, top 1% and top 0.1%
     shares, 99th percentile; mean and median by wage group (06's groups).
     Two columns: net worth drawn under the PUF label and under the SCF
     label. Equal weights summing to SOI's 2019 return count, as 06 does.
  2. Holder shares (amount not zero; for a split table, shared or small part
     not zero) for interest, dividends, capital gains on the PUF-label file,
     against the weighted 2015 PUF and the IRS 2015 counts (Table 1.4: returns
     with the item over all returns; capital gains = Schedule D gain plus
     Schedule D loss returns, as soi_cells.py counts them). Also the SCF-style
     file's holder shares against the actual SCF, raw and as the run's table
     defines the shared part.
  3. Label swap on shared variables, copied from the run's 02_eval.json.
  4. The meter. Net worth in 2015 dollars, drawn with the run's map; equal
     starting weights summing to SOI's 2019 return count. (a) Tilt to the six
     soft 2019 SCF wealth targets of 07_baseline.py (sigma 2%, ranks iterated
     as in 06's tilt_with_wealth), plus one exact target: the total return
     count. Without it the six could be met by shrinking every weight by the
     same factor, which the move (measured on normalized weights) does not
     see. (b) The 136 IRS targets of run B1 alone, and with the six wealth
     targets, incomes in 2019 dollars from the run's 2015 PUF map grown by
     step 4's factors (as 05/06 do); the move beyond the 136 alone is the
     difference. Both for the PUF-label and the SCF-label file.
  5. Held-out loss per variable from 02_eval.json, each flagged when its band
     layout differs from v2's (the band_edges files compared cut by cut).

Dollars. The rank-dollar map for incomes is built from the run's own table
and band edges (PUF rows), never from the v2 ones; net worth always uses the
SCF map R/map/map_scf_2015.parquet. The draw is 04_dollars.draw_one with the
seed scheme of 04_dollars.draw_dollars, so for the v2 layout the draws equal
06's exactly. A split table's dollar amount is shared part plus small part.

Layouts with no zero band (option B, 13_option_b.py, 2026-10-15). When the
run's band edges give a shared variable the layout 'full_distribution', a
band can hold zeros only, zeros and small amounts, or amounts only, so a
band number does not say whether a record holds the item. For those
variables a holder is a record whose mapped dollars are nonzero: the PUF-
style files are drawn with the run's PUF map as for everything else, the
SCF-style file with a map built the same way from the table's SCF rows, and
the actual SCF uses its dollars. A band of the map that holds only zeros
maps to $0; a band straddling zero and small amounts draws $0 with the zero
records' weight share of the band (map_check in score.json). Runs whose
edges have no such layout are scored exactly as before.

Writes R/score/<T>/score.json, score.md and SCORE_<T>_DONE in R/markers/, and
rewrites the summary table between the TABLE markers in 09_RESULTS.md.
"""
import fcntl
import importlib.util
import json
import os
import sys
import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS_MD = os.path.join(HERE, '09_RESULTS.md')
# Environment overrides (added 2026-10-15 for the translation experiments,
# 10_TRANSLATION_EXPERIMENTS.md; unset = the behaviour above):
#   SCORE_RESULTS_MD       another results file (path, or a name in this
#                          folder). A run scored with it keeps its row there;
#                          each results file's table lists only its own runs.
#   SCORE_DESCRIBE         the "What it changes" text of this run's row.
#   SCORE_REFERENCE_RUNS   comma-separated runs copied to the top of the table
#                          (for example E0,E4), from their own score.json.
RESULTS_MD = os.path.join(HERE, os.environ['SCORE_RESULTS_MD']) if os.environ.get('SCORE_RESULTS_MD') else RESULTS_MD
REFERENCE_RUNS = [r for r in os.environ.get('SCORE_REFERENCE_RUNS', '').split(',') if r]
V2_EDGES = f'{ROOT}/data/band_edges.json'
SHARED = ['married', 'n_dep', 'wages', 'interest', 'dividends', 'capital_gains']
INV = ['interest', 'dividends', 'capital_gains']
SIGMA = 0.02
MAX_PASSES = 50
# 2019 SCF incomes are 2018 dollars (raw X5702 etc.); in 2022 dollars they are
# times 1.1802 (checks_scf2019.csv, WAGEINC over X5702), and 2015 dollars are
# 2022 dollars over 1.2493, the factor 06 and step 1 use. So 2015 dollars =
# 2018 dollars x 1.1802 / 1.2493.
WAGE_2018_TO_2015 = 1.1802474966 / 1.2493

DESCRIBE = {  # what each run changes, in five words or so
    'E0': 'control: the v2 model',
    'E1': 'zero PUF amounts under limit',
    'E2': 'split at limit; small part PUF-only',
    'E3': 'E2 with the limits raised',
    'E4': 'shared variables on PUF dollar scale',
    'E5': 'E2 split on PUF dollar scale',
    'E6': 'flip SCF non-holders (translation)',
    'E7': 'shared variables without source label',
    'E8': 'E7 plus coarsened SCF zeros',
    'E9': 'reporting curve in the loss',
    'E10': 'SCF investment zeros not scored',
    'E11': 'tilt only, wealth by wage group',
    'E12': 'best run plus the full tilt',
}
ORDER = list(DESCRIBE)


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


wt = load('wt', '06_wealth_tables.py')
dollars, tiltmod = wt.dollars, wt.tiltmod
import soi_cells  # noqa: E402  (on the path: this folder)


# ---------------------------------------------------------------- the run

def resolve(T):
    """Paths for run T. E0 is the v2 model and its files (task convention)."""
    if T == 'E0':
        model = f'{ROOT}/model/v2'
        return {'run': T, 'model_dir': model, 'table': 'v2', 'flags': {},
                'stacked': f'{ROOT}/data/stacked.parquet', 'edges': V2_EDGES,
                'gen_puf': f'{ROOT}/gen/v2/gen_puf_with_net_worth.parquet',
                'gen_owner': f'{ROOT}/gen/gen_puf_with_net_worth_ownerlabel.parquet',
                'gen_scf': f'{ROOT}/gen/v2/gen_scf.parquet',
                'eval': f'{model}/02_eval.json'}
    model, gen = f'{ROOT}/model/{T}', f'{ROOT}/gen/{T}'
    meta = json.load(open(f'{model}/train_meta.json'))
    tag = meta.get('table', 'v2')
    suffix = '' if tag in ('v2', '', None) else f'_{tag}'
    return {'run': T, 'model_dir': model, 'table': tag, 'flags': meta.get('flags', {}),
            'stacked': f'{ROOT}/data/stacked{suffix}.parquet',
            'edges': f'{ROOT}/data/band_edges{suffix}.json',
            'gen_puf': f'{gen}/gen_puf_with_net_worth.parquet',
            'gen_owner': f'{gen}/gen_puf_with_net_worth_ownerlabel.parquet',
            'gen_scf': f'{gen}/gen_scf.parquet', 'eval': f'{model}/02_eval.json'}


def amount_vars(df):
    return [c[:-5] for c in df.columns if c.endswith('_band')]


def small_of(v, cols):
    """The small part of v in a split table (as dollars or as a band column)."""
    return f'{v}_small' if (f'{v}_small' in cols or f'{v}_small_band' in cols) else None


# ---------------------------------------------------------------- dollars

FULL = set()   # shared variables with a full-distribution layout in this run (set in run_maps)


def is_full(edge):
    return edge.get('layout') == 'full_distribution'


def full_fill_row(edge, band):
    """Map row for an empty band of a full-distribution layout. Such a band
    can only be empty because a tie (one dollar amount) is wider than the
    band, so its two cuts coincide and the value is that amount; for a band
    empty only in a subset of records, its upper cut (lower cut for band 83)."""
    cuts = edge['full']['cut_dollars']
    lo = cuts[band - 1] if band >= 1 else None
    hi = cuts[band] if band < len(cuts) else None
    if lo is not None and lo == hi:
        return float(lo), 'empty band: lump value at its cut'
    return float(hi if hi is not None else lo), 'no records in this subset: band edge'


def build_map(df, edges, source, variables):
    """Copied from 04_dollars.build_map with the variable list as an argument
    (04's is fixed to its six amounts; split tables add small parts). Empty
    bands of a full-distribution layout get full_fill_row."""
    rows = []
    for v in variables:
        x, b, w = df[v].to_numpy(float), df[f'{v}_band'].to_numpy(), df['weight'].to_numpy(float)
        present = b >= 0
        assert np.array_equal(present, ~np.isnan(x)), f'{v}: band -1 must match missing dollars'
        if not present.any():
            continue
        m = pd.DataFrame({'variable': v, 'band': b[present], 'value': x[present],
                          'weight': w[present], 'filled': False, 'rule': 'real records'})
        edge = edges[f'{source}/2015/{v}']
        missing = sorted(set(range(edge['n_bands'])) - set(m['band']))
        fill = full_fill_row if is_full(edge) else dollars.fill_row
        fills = [dict(zip(['value', 'rule'], fill(edge, k)), variable=v, band=k,
                      weight=1.0, filled=True) for k in missing]
        rows += [m, pd.DataFrame(fills, columns=m.columns)]
    out = pd.concat(rows, ignore_index=True)
    return out.sort_values(['variable', 'band', 'value'], kind='stable').reset_index(drop=True)


def map_check(maps, edges, source):
    """For full-distribution variables: bands holding only zeros must map to
    $0, and a straddling band's chance of drawing $0 is its zero weight share."""
    out = {}
    for v in sorted(FULL):
        g = maps[v]
        zm = edges[f'{source}/2015/{v}']['zero_mass']
        only, strad = {}, {}
        for b, gb in g.groupby('band'):
            w = gb['weight'].to_numpy(float)
            z = gb['value'].to_numpy(float) == 0
            if z.all():
                only[int(b)] = True
            elif z.any():
                strad[int(b)] = {'zero weight share': float(w[z].sum() / w.sum()),
                                 'largest amount': float(gb['value'].max()),
                                 'smallest amount': float(gb['value'].min())}
        assert sorted(only) == sorted(zm['bands_only_zero']), f'{v}: zero-only bands of the map differ from the edges'
        assert sorted(strad) == sorted(zm['bands_straddling']), f'{v}: straddling bands differ from the edges'
        out[v] = {'bands mapping to $0 only': sorted(only), 'straddling bands': strad}
    return out


def draw(df, maps, seed=dollars.SEED):
    """04_dollars.draw_dollars for any variable list: the stream for one of
    04's six amounts is [seed, its position in 04's list], so v2-layout draws
    equal 04's and 06's; a small part gets [seed, 10 + its position]."""
    out = df.copy()
    extra = [v for v in maps if v not in dollars.AMOUNTS]
    for v, g in maps.items():
        i = dollars.AMOUNTS.index(v) if v in dollars.AMOUNTS else 10 + extra.index(v)
        out[v], _ = dollars.draw_one(df[f'{v}_band'].to_numpy(), g, np.random.default_rng([seed, i]))
    return out


def run_maps(run):
    """PUF income maps from the run's table and edges; SCF net worth map."""
    table = pd.read_parquet(run['stacked'])
    edges = json.load(open(run['edges']))
    puf = table[table['source'] == 'puf'].reset_index(drop=True)
    vars_ = [v for v in amount_vars(puf) if v != 'net_worth' and puf[v].notna().any()]
    FULL.clear()
    FULL.update(v for v in vars_ if is_full(edges[f'puf/2015/{v}']))
    maps = dollars.split_by_variable(build_map(puf, edges, 'puf', vars_))
    maps['net_worth'] = dollars.split_by_variable(
        pd.read_parquet(f'{ROOT}/map/map_scf_2015.parquet'))['net_worth']
    return table, edges, maps


def with_dollars(path, maps):
    """A PUF-labelled generated file in 2015 and 2019 dollars, parts summed."""
    raw = pd.read_parquet(path)
    raw = raw[raw['source'] == 'puf'].reset_index(drop=True)
    assert (raw['net_worth_band'] >= 0).all(), 'a PUF-style record lacks a net worth band'
    g = draw(raw, {v: m for v, m in maps.items() if f'{v}_band' in raw})
    factors = dict(pd.read_csv(f'{ROOT}/map/growth_factors.csv')[['variable', 'factor']].values)
    g15, g19 = g.copy(), g.copy()
    for v in soi_cells.PARTS:
        parts = [v] + ([small_of(v, g.columns)] if small_of(v, g.columns) else [])
        g15[v] = sum(g[p].to_numpy(float) for p in parts)
        g19[v] = sum(dollars.grow_values(v, g[p], factors[v]) for p in parts)
    need = soi_cells.PARTS + ['married', 'n_dep', 'net_worth']
    assert not g15[need].isna().any().any(), 'missing dollars on a PUF-style record'
    g19['record_id'] = np.arange(len(g19))
    return raw, g15, g19


# ---------------------------------------------------------------- 1. wealth

def wealth_stats(df, w):
    x = df['net_worth'].to_numpy(float)
    u = wt.unconditional(x, w)
    c = wt.conditional(df, x, w)
    c = c[c['grouping'] == 'wages']
    return {'all': u, 'by_wage': {r['group']: {k: r.get(k) for k in
                                               ['share of units (%)', 'mean ($)', 'median ($)']}
                                  for r in c.to_dict('records')}}


def ratios(gen, act):
    a, g = act['all'], gen['all']
    out = {'total ratio': g['total net worth ($ trillion)'] / a['total net worth ($ trillion)'],
           'mean ratio': g['mean ($)'] / a['mean ($)'],
           'median ratio': g['median ($)'] / a['median ($)'],
           'p99 ratio': g['99th percentile ($)'] / a['99th percentile ($)'],
           'top 1% share (points)': g['top 1% share (%)'] - a['top 1% share (%)'],
           'top 0.1% share (points)': g['top 0.1% share (%)'] - a['top 0.1% share (%)']}
    for grp, s in gen['by_wage'].items():
        out[f'mean ratio, wages {grp}'] = s['mean ($)'] / act['by_wage'][grp]['mean ($)']
        out[f'median ratio, wages {grp}'] = s['median ($)'] / act['by_wage'][grp]['median ($)']
    return out


# ---------------------------------------------------------------- 2. holder shares

def holder_flags(df, v, band=True, dollars_of=None):
    """Nonzero amount; for a split table, shared or small part nonzero.
    For a variable with a full-distribution layout (FULL) the band does not
    say it: then the mapped dollars in dollars_of (same rows) are used."""
    parts = [v] + ([small_of(v, df.columns)] if small_of(v, df.columns) else [])
    if band and v in FULL:
        assert dollars_of is not None and len(dollars_of) == len(df), f'{v}: dollars needed for holders'
        return np.logical_or.reduce([np.nan_to_num(dollars_of[p].to_numpy(float)) != 0 for p in parts])
    if band:
        return np.logical_or.reduce([df[f'{p}_band'].to_numpy() > 0 for p in parts])
    return np.logical_or.reduce([np.nan_to_num(df[p].to_numpy(float)) != 0 for p in parts])


def irs_shares_2015():
    t = pd.read_csv(f'{ROOT}/targets/soi_2015_validation.csv')
    t = t[(t['year'] == 2015) & (t['table'] == 1.4) & (t['filing_status'] == 'all')
          & (t['agi_bracket_label'] == 'all') & (t['statistic'] == 'count')]
    c = dict(zip(t['variable'], t['value']))
    n = c['returns']
    return {'interest': c['taxable_interest'] / n, 'dividends': c['ordinary_dividends'] / n,
            'capital_gains': (c['sch_d_net_gain'] + c['sch_d_net_loss']) / n}


def holder_shares(run, table, gpuf_raw, w_gen=None, gpuf_dollars=None, edges=None):
    v2 = pd.read_parquet(f'{ROOT}/data/stacked.parquet')
    puf, scf = v2[v2['source'] == 'puf'], v2[v2['source'] == 'scf']
    tscf = table[table['source'] == 'scf']
    gscf = pd.read_parquet(run['gen_scf'])
    gscf = gscf[gscf['source'] == 'scf'].reset_index(drop=True)
    gscf_held = {v: gscf[f'{v}_band'].to_numpy() > 0 for v in INV}
    tscf_held = {v: tscf[f'{v}_band'].to_numpy() > 0 for v in INV}
    if FULL & set(INV):   # SCF-style dollars from the table's SCF rows (see the top note)
        fv = sorted(FULL & set(INV))
        smaps = dollars.split_by_variable(build_map(tscf.reset_index(drop=True), edges, 'scf', fv))
        gd = draw(gscf, smaps)
        for v in fv:
            gscf_held[v] = gd[v].to_numpy(float) != 0
            tscf_held[v] = tscf[v].to_numpy(float) != 0
    irs = irs_shares_2015()
    w = np.ones(len(gpuf_raw)) if w_gen is None else w_gen
    out = {}
    for v in INV:
        wp, ws, wts = puf['weight'].to_numpy(float), scf['weight'].to_numpy(float), tscf['weight'].to_numpy(float)
        out[v] = {'PUF-style generated': w[holder_flags(gpuf_raw, v, dollars_of=gpuf_dollars)].sum() / w.sum(),
                  'PUF 2015 (weighted)': wp[holder_flags(puf, v, band=False)].sum() / wp.sum(),
                  'IRS 2015': irs[v],
                  'SCF-style generated (shared part)': gscf_held[v].mean(),
                  'actual SCF, raw': ws[holder_flags(scf, v, band=False)].sum() / ws.sum(),
                  "actual SCF, run's table (shared part)": wts[tscf_held[v]].sum() / wts.sum()}
    return out


# ---------------------------------------------------------------- 3 and 5. from 02_eval.json

def label_swap(ev):
    out = {}
    for s in ('puf', 'scf'):
        ch = ev.get('label_swap', {}).get(s, {}).get('change', {})
        for v in SHARED:
            if v in ch:
                out.setdefault(v, {})[f'{s.upper()} records, one hidden'] = ch[v].get('one')
                out[v][f'{s.upper()} records, half hidden'] = ch[v].get('half')
    return out


def bands_differ(run_edges, v2_edges):
    """Per variable: 'same', 'differ' or 'new variable' (cuts compared in both sources)."""
    out = {}
    for k, e in run_edges.items():
        if k == 'meta' or not isinstance(e, dict) or 'positive' not in e:
            continue
        v = k.split('/')[-1]
        if k not in v2_edges:
            out[v] = 'new variable'
            continue
        o = v2_edges[k]
        same = (e['n_bands'] == o['n_bands'] and e['positive']['cuts'] == o['positive']['cuts']
                and e['negative']['cuts'] == o['negative']['cuts'])
        if out.get(v) in (None, 'same'):
            out[v] = 'same' if same else 'differ'
    return out


def held_out(ev, flags):
    rows, total, comparable = [], 0.0, True
    for s, d in ev.get('cross_entropy', {}).items():
        for v, r in d.items():
            f = flags.get(v, 'same' if v in ('married', 'n_dep') else 'differ')  # not banded
            rows.append({'source': s, 'variable': v, **r, 'bands vs v2': f})
            total += r.get('half', np.nan)
            comparable &= f == 'same'
    v2_vars = {('puf', v) for v in SHARED + ['other_agi']} | {('scf', v) for v in SHARED + ['net_worth']}
    comparable &= {(r['source'], r['variable']) for r in rows} == v2_vars
    return rows, total, comparable


# ---------------------------------------------------------------- 4. the meter

def tilt_scaled(w, F, T, S, rel_tol, max_iter=200):
    """05_tilt.tilt with each target's scale S given (05 uses S = |T|; equal
    when S = |T|), so a target can be zero: (x - m) * in_group, target 0, is a
    group mean. rel_tol is then a share of S. Copied from 05_tilt.py."""
    W = w.sum()
    p = w / W
    X = F * (W / S)
    t = T / S
    s2 = np.asarray(rel_tol, float) ** 2
    lam = np.zeros(len(T))

    def dual(lam):
        e = p * np.exp(X @ lam)
        return e.sum() - lam @ t + 0.5 * (s2 * lam ** 2).sum(), e

    D, e = dual(lam)
    for it in range(max_iter):
        g = X.T @ e - t + s2 * lam
        if np.abs(g).max() < 1e-11:
            break
        H = X.T @ (X * e[:, None]) + np.diag(s2)
        step = np.linalg.solve(H + 1e-12 * np.eye(len(T)), -g)
        a = 1.0
        while True:
            D_new, e_new = dual(lam + a * step)
            if np.isfinite(D_new) and D_new <= D + 1e-4 * a * (g @ step):
                break
            a /= 2
            if a < 1e-12:
                return W * e, False, it
        lam, D, e = lam + a * step, D_new, e_new
    return W * e, bool(np.abs(X.T @ e - t + s2 * lam).max() < 1e-7), it


def wealth_tilt(x, w0, F, T, targets, threshold, extra=None, damp=False):
    """06_wealth_tables.tilt_with_wealth (copied), with optional extra soft
    targets extra = (F_extra, T_extra, S_extra) that do not depend on ranks.
    F, T exact; the six wealth targets soft at SIGMA; ranks iterated.
    damp: rank the next pass on the geometric mean of this pass's and the
    last pass's weights (E11: undamped, the passes with wage-group targets
    drifted and never settled). Undamped is 06's rule exactly."""
    Tw = np.array([targets[k] for k in wt.WEALTH_KEYS])
    Fe, Te, Se = extra if extra is not None else (np.zeros((len(x), 0)), np.zeros(0), np.zeros(0))
    rel_tol = np.r_[np.zeros(len(T)), np.full(len(Tw) + len(Te), SIGMA)]
    S = np.r_[np.abs(T), np.abs(Tw), Se]
    above = (x > threshold).astype(float)
    w_rank, prev, log = w0, None, []
    for p in range(1, MAX_PASSES + 1):
        m = wt.rank_groups(x, w_rank)
        Fw = np.column_stack([x] + [x * m[g] for g in wt.GROUP_BOUNDS] + [above])
        w1, ok, iters = tilt_scaled(w0, np.column_stack([F, Fw, Fe]), np.r_[T, Tw, Te], S, rel_tol)
        held = wt.wealth_held(x, w1, threshold)
        change = np.nan if prev is None else max(abs(held[k] / prev[k] - 1) for k in wt.WEALTH_KEYS)
        log.append({'pass': p, 'converged': ok, 'newton_steps': iters, 'max_rel_change': change,
                    **{f'{k} miss %': 100 * (held[k] / targets[k] - 1) for k in wt.WEALTH_KEYS}})
        print(log[-1], flush=True)
        if prev is not None and change < 1e-4:
            break
        prev, w_rank = held, (np.sqrt(w_rank * w1) if damp else w1)
    return w1, log, held, ok


def settled_tilt(x, w0, F, T, targets, threshold):
    """wealth_tilt by 06's rule; if its ranks do not settle in MAX_PASSES,
    again with damping (and the passes column says so)."""
    w, log, held, ok = wealth_tilt(x, w0, F, T, targets, threshold)
    if len(log) < MAX_PASSES:
        return w, log, held, ok, str(len(log))
    print('ranks did not settle; damped rerun', flush=True)
    w, log, held, ok = wealth_tilt(x, w0, F, T, targets, threshold, damp=True)
    return w, log, held, ok, f"{len(log)} (damped{'' if len(log) < MAX_PASSES else ', not settled'})"


def short_move(w0, w1):
    """05_tilt.move, with the KL sum taken over records whose new weight is
    above zero (0 log 0 = 0): a weight that underflows to exactly zero made
    05's sum NaN."""
    with np.errstate(divide='ignore', invalid='ignore'):
        m = tiltmod.move(w0, w1)
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    pos = p1 > 0
    m['kl_nats'] = float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos])))
    return {k: m[k] for k in ['kl_nats', 'ess_ratio', 'factor_min', 'factor_max',
                              'records_factor_over_10', 'records_factor_under_0.1']}


_Q = {}


def threshold_form(x, returns, targets, threshold):
    """The six wealth targets without ranks: units and net worth above the
    2019 SCF's 99th, 90th and 50th percentiles (2015 dollars; units as the
    SCF's share of units times the return count, net worth as the SCF's
    amount), total net worth, units above $1 million. At a solution that
    matches the SCF these say what the rank groups say; they need no rank
    iteration, which often did not settle when the 136 IRS targets were not
    there to hold the income distribution."""
    if not _Q:
        u19, x19, w19, _ = wt.scf2019()
        _Q['q'] = dollars.wquantile(x19, w19, [0.99, 0.90, 0.50])
        _Q['t'] = [(w19[x19 > q].sum() / w19.sum(), (x19 * w19)[x19 > q].sum()) for q in _Q['q']]
    F, T = [], []
    for q, (share, amount) in zip(_Q['q'], _Q['t']):
        a = (x > q).astype(float)
        F += [a, x * a]
        T += [returns * share, amount]
    F += [x, (x > threshold).astype(float)]
    T += [targets['total'], targets['units above $1m']]
    return np.column_stack(F), np.array(T)


def soft_solve(w0, F_exact, T_exact, F_soft, T_soft):
    return tilt_scaled(w0, np.column_stack([F_exact, F_soft]), np.r_[T_exact, T_soft],
                       np.r_[np.abs(T_exact), np.abs(T_soft)],
                       np.r_[np.zeros(len(T_exact)), np.full(len(T_soft), SIGMA)])


def meter(g15, g19, returns, targets, threshold, do_136=True):
    x = g15['net_worth'].to_numpy(float)
    n = len(x)
    w0 = np.full(n, returns / n)
    out, weights = {}, {'start': w0}
    one = np.ones((n, 1))
    Fq, Tq = threshold_form(x, returns, targets, threshold)
    wq, okq, _ = soft_solve(w0, one, np.array([returns]), Fq, Tq)
    out['six, threshold form'] = {**short_move(w0, wq), 'converged': okq,
                                  'miss %': list(100 * (Fq.T @ wq / Tq - 1)),
                                  'rank-form statistics, miss %': {
                                      k: 100 * (v / targets[k] - 1)
                                      for k, v in wt.wealth_held(x, wq, threshold).items()}}
    weights['six, threshold'] = wq
    # The rank form of the six alone (06's iteration without the 136) is not
    # run: on E2, E4 and E7 it did not settle in 50 passes, damped or not.
    if do_136:
        brackets, cells = tiltmod.target_set('brackets')
        F = tiltmod.contributions(g19, brackets)
        T = cells['value'].to_numpy(float)
        tiltmod.check_reachable(F, cells)
        w1, ok1, _ = tiltmod.tilt(w0, F, T)
        w2, log2, held2, ok2, pass2 = settled_tilt(x, w0, F, T, targets, threshold)
        m1, m2 = short_move(w0, w1), short_move(w0, w2)
        out['136 IRS alone'] = {**m1, 'converged': ok1}
        out['136 IRS + six wealth'] = {**m2, 'passes': pass2, 'converged': ok2,
                                       'max abs gap 136 (%)': float(np.abs(100 * (F.T @ w2 / T - 1)).max()),
                                       'miss %': {k: 100 * (held2[k] / targets[k] - 1) for k in held2}}
        out['move beyond the 136 (nats)'] = m2['kl_nats'] - m1['kl_nats']
        weights['136'], weights['136+six'] = w1, w2
        wq2, okq2, _ = soft_solve(w0, F, T, Fq, Tq)
        mq2 = short_move(w0, wq2)
        out['136 IRS + six, threshold form'] = {**mq2, 'converged': okq2,
                                                'max abs gap 136 (%)': float(np.abs(100 * (F.T @ wq2 / T - 1)).max())}
        out['move beyond the 136, threshold form (nats)'] = mq2['kl_nats'] - m1['kl_nats']
        weights['136+six, threshold'] = wq2
    return out, weights


# ---------------------------------------------------------------- E11 targets

def scf2019_wage_means():
    """Mean net worth (2015 dollars) by 06's wage groups, 2019 SCF tax units,
    wages put in 2015 dollars (WAGE_2018_TO_2015)."""
    u, x, w, _ = wt.scf2019()
    df = pd.DataFrame({'wages': u['wages'].to_numpy(float) * WAGE_2018_TO_2015,
                       'dividends': u['dividends'].to_numpy(float) * WAGE_2018_TO_2015,
                       'married': u['married'].to_numpy()})
    g = wt.groups(df)['wages']
    return {lab: float((x * w)[g == lab].sum() / w[g == lab].sum()) for lab in wt.ORDER['wages']}, \
        {lab: float(w[g == lab].sum() / w.sum()) for lab in wt.ORDER['wages']}


# ---------------------------------------------------------------- output

def fmt(v, kind='num'):
    if v is None or (isinstance(v, float) and not np.isfinite(v)):
        return '—'
    if kind == 'ratio':
        return f'{v:.2f}'
    if kind == 'pct':
        return f'{v:.1f}%'
    if kind == 'pts':
        return f'{v:+.1f}'
    return wt.sig3(v)


def md_score(S):
    L = [f"# Scoreboard, run {S['run']}", '',
         f"Table: `{S['table']}`; flags: `{json.dumps(S['flags'])}`. Files: `{S['files']}`.", '']
    L += ['## 1. Wealth, PUF-style records against the actual SCF (2016 wave, 2015 dollars)', '',
          '| statistic | actual SCF | PUF label | SCF label | SCF-style file |', '|---|---:|---:|---:|---:|']
    W = S['wealth']
    for k in W['actual SCF']['all']:
        if k == 'units (millions)':
            continue
        p = '(%)' in k
        L.append(f'| {k} | ' + ' | '.join(
            (fmt(W[c]['all'][k], 'pct') if p else fmt(W[c]['all'][k])) if c in W else '—'
            for c in ['actual SCF', 'PUF label', 'SCF label', 'SCF-style file']) + ' |')
    L += ['', 'Ratios to the actual SCF (shares in points):', '',
          '| measure | PUF label | SCF label |', '|---|---:|---:|']
    for k in S['ratios']['PUF label']:
        kind = 'pts' if 'points' in k else 'ratio'
        L.append(f'| {k} | ' + ' | '.join(fmt(S['ratios'][c].get(k), kind) for c in ['PUF label', 'SCF label']) + ' |')
    L += ['', '### Mean and median net worth by wage group ($, 2015)', '',
          '| wages | share of units, SCF | share, PUF-style | mean, SCF | mean, PUF label | mean, SCF label | median, SCF | median, PUF label | median, SCF label |',
          '|---|---:|---:|---:|---:|---:|---:|---:|---:|']
    for g in wt.ORDER['wages']:
        a, p, s = (W[c]['by_wage'].get(g, {}) for c in ['actual SCF', 'PUF label', 'SCF label'])
        L.append(f"| {g} | {fmt(a.get('share of units (%)'), 'pct')} | {fmt(p.get('share of units (%)'), 'pct')} | "
                 f"{fmt(a.get('mean ($)'))} | {fmt(p.get('mean ($)'))} | {fmt(s.get('mean ($)'))} | "
                 f"{fmt(a.get('median ($)'))} | {fmt(p.get('median ($)'))} | {fmt(s.get('median ($)'))} |")
    L += ['', '## 2. Holder shares (share of units with the item, %)', '']
    cols = list(next(iter(S['holders'].values())))
    L += ['| item | ' + ' | '.join(cols) + ' |', '|---|' + '---:|' * len(cols)]
    for v, r in S['holders'].items():
        L.append(f'| {v} | ' + ' | '.join(fmt(100 * r[c], 'pct') for c in cols) + ' |')
    L += ['', '## 3. Label swap (change in cross-entropy under the other label, nats; from 02_eval.json)', '']
    if S['label_swap']:
        cols = list(next(iter(S['label_swap'].values())))
        L += ['| variable | ' + ' | '.join(cols) + ' |', '|---|' + '---:|' * len(cols)]
        for v, r in S['label_swap'].items():
            L.append(f'| {v} | ' + ' | '.join(f'{r[c]:.4f}' if r.get(c) is not None else '—' for c in cols) + ' |')
    else:
        L.append('No label swap in 02_eval.json.')
    L += ['', '## 4. The meter (tilt from equal weights at SOI 2019 returns; net worth 2015 dollars)', '',
          '| file | tilt | move (nats) | ESS ratio | passes | converged | smallest factor | largest factor | records under 0.1x |',
          '|---|---|---:|---:|---:|---|---:|---:|---:|']
    for lab, M in S['meter'].items():
        for name, r in M.items():
            if not isinstance(r, dict):
                continue
            L.append(f"| {lab} | {name} | {r['kl_nats']:.4f} | {r['ess_ratio']:.3f} | {r.get('passes', '—')} | "
                     f"{r['converged']} | {r['factor_min']:.2g} | {r['factor_max']:.3g} | {r['records_factor_under_0.1']} |")
        for name, r in M.items():
            if not isinstance(r, dict):
                L.append(f"| {lab} | {name} | {r:.4f} | | | | | | |")
    L += ['', 'Wealth after each tilt (PUF label):', '',
          '| statistic | actual SCF | before | six (threshold form) | 136 + six (threshold form) | six (rank form) | 136 + six (rank form) |',
          '|---|---:|---:|---:|---:|---:|---:|']
    T = S.get('after_tilt', {}).get('PUF label', {})
    for k in ['total net worth ($ trillion)', 'mean ($)', 'median ($)', 'top 1% share (%)',
              'top 0.1% share (%)', '99th percentile ($)']:
        p = '(%)' in k
        vals = [W['actual SCF']['all'][k], W['PUF label']['all'][k]] + \
               [T.get(n, {}).get('all', {}).get(k) for n in ['six, threshold', '136+six, threshold', 'six', '136+six']]
        L.append(f'| {k} | ' + ' | '.join(fmt(v, 'pct') if p else fmt(v) for v in vals) + ' |')
    L += ['', '## 5. Held-out cross-entropy per variable (nats, from 02_eval.json)', '',
          '| source | variable | one hidden | half hidden | all hidden | marginal | bands vs v2 |',
          '|---|---|---:|---:|---:|---:|---|']
    for r in S['held_out']:
        L.append(f"| {r['source']} | {r['variable']} | " + ' | '.join(
            f"{r[k]:.4f}" if isinstance(r.get(k), (int, float)) else '—' for k in ['one', 'half', 'all', 'marginal'])
            + f" | {r['bands vs v2']} |")
    L += ['', f"Sum of half-hidden cross-entropy over all variables and both sources: "
          f"{S['loss_total']:.4f} ({'comparable with v2' if S['loss_comparable'] else 'bands differ from v2'})."]
    return '\n'.join(L) + '\n'


def summary_row(S):
    r = S['ratios']
    h = S['holders']
    hs = '/'.join(f"{100 * h[v]['PUF-style generated']:.1f}" for v in INV)
    ls = S['label_swap'].get('interest', {}).get('PUF records, one hidden')
    mp = S['meter'].get('PUF label', {})
    mt = mp.get('six, threshold form', mp.get('six wealth targets', {}))
    loss = (f"{S['loss_total']:.3f}" if S['loss_comparable'] else 'bands differ') \
        if S.get('loss_total') is not None else S.get('loss_note', '—')
    w = S['wealth']
    return {'run': S['run'], 'what': os.environ.get('SCORE_DESCRIBE') or DESCRIBE.get(S['run'], ''),
            'results_md': os.path.basename(RESULTS_MD),
            'total': f"{r['PUF label']['total ratio']:.2f} / {r['SCF label']['total ratio']:.2f}"
            if 'SCF label' in r else f"{r['PUF label']['total ratio']:.2f}",
            'top1': f"{w['PUF label']['all']['top 1% share (%)']:.1f} / {w['SCF label']['all']['top 1% share (%)']:.1f}"
            if 'SCF label' in w else f"{w['PUF label']['all']['top 1% share (%)']:.1f}",
            'median': f"{r['PUF label']['median ratio']:.2f} / {r['SCF label']['median ratio']:.2f}"
            if 'SCF label' in r else f"{r['PUF label']['median ratio']:.2f}",
            'holders': hs, 'swap': '—' if ls is None else f'{ls:.3f}',
            'move': f"{mt['kl_nats']:.3f} / {mt['ess_ratio']:.2f}" if mt else '—',
            'loss': loss}


def write_summary_table():
    """Rebuild the table between the markers in 09_RESULTS.md from every
    R/score/*/score.json. Locked: two scoring jobs may finish together."""
    rows = []
    for T in os.listdir(f'{ROOT}/score'):
        p = f'{ROOT}/score/{T}/score.json'
        if os.path.exists(p):
            row = json.load(open(p))['row']
            # rows without the key were scored before 2026-10-15 for 09_RESULTS.md
            if row.get('results_md', '09_RESULTS.md') == os.path.basename(RESULTS_MD) or row['run'] in REFERENCE_RUNS:
                rows.append(row)
    rows.sort(key=lambda r: (REFERENCE_RUNS.index(r['run']) if r['run'] in REFERENCE_RUNS else 99,
                             ORDER.index(r['run']) if r['run'] in ORDER else 99, r['run']))
    act = json.load(open(f'{ROOT}/score/E0/score.json'))['wealth']['actual SCF']['all'] \
        if os.path.exists(f'{ROOT}/score/E0/score.json') else None
    hp = json.load(open(f'{ROOT}/score/E0/score.json'))['holders'] \
        if os.path.exists(f'{ROOT}/score/E0/score.json') else None
    top = f"{act['top 1% share (%)']:.1f}" if act else '?'
    pufh = '/'.join(f"{100 * hp[v]['PUF 2015 (weighted)']:.1f}" for v in INV) if hp else '?'
    head = ['| Run | What it changes | Net worth total, ratio to SCF (PUF / SCF label) '
            f'| Top 1% share, % (actual {top}) | Median ratio (PUF / SCF label) '
            f'| Holders % int/div/kg (PUF {pufh}) | Interest label swap (nats) '
            '| Wealth tilt: move (nats) / ESS ratio | Held-out loss |',
            '|---|---|---:|---:|---:|---:|---:|---:|---:|']
    body = [f"| {r['run']} | {r['what']} | {r['total']} | {r['top1']} | {r['median']} | {r['holders']} "
            f"| {r['swap']} | {r['move']} | {r['loss']} |" for r in rows]
    with open(RESULTS_MD + '.lock', 'w') as lk:
        fcntl.flock(lk, fcntl.LOCK_EX)
        text = open(RESULTS_MD).read()
        a, b = '<!-- TABLE START -->', '<!-- TABLE END -->'
        i, j = text.index(a) + len(a), text.index(b)
        text = text[:i] + '\n' + '\n'.join(head + body) + '\n' + text[j:]
        open(RESULTS_MD, 'w').write(text)
        fcntl.flock(lk, fcntl.LOCK_UN)


def finish(S):
    out = f"{ROOT}/score/{S['run']}"
    os.makedirs(out, exist_ok=True)
    S['row'] = summary_row(S)
    json.dump(S, open(f'{out}/score.json', 'w'), indent=1, default=float)
    open(f'{out}/score.md', 'w').write(md_score(S))
    print(md_score(S), flush=True)
    write_summary_table()
    os.makedirs(f'{ROOT}/markers', exist_ok=True)
    open(f"{ROOT}/markers/SCORE_{S['run']}_DONE", 'w').close()


# ---------------------------------------------------------------- main

def score_run(T):
    run = resolve(T)
    print(json.dumps(run, indent=1), flush=True)
    table, edges, maps = run_maps(run)
    scf, w_scf = wt.actual_scf()
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    targets, threshold = wt.wealth_targets()
    S = {'run': T, 'table': run['table'], 'flags': run['flags'], 'files': run,
         'wealth': {'actual SCF': wealth_stats(scf, w_scf)}, 'ratios': {}, 'meter': {},
         'after_tilt': {}}
    raws, g15s = {}, {}
    if FULL:
        S['map_check'] = map_check(maps, edges, 'puf')
        print('map check:', json.dumps(S['map_check'], indent=1), flush=True)
    for lab, key in [('PUF label', 'gen_puf'), ('SCF label', 'gen_owner')]:
        if not os.path.exists(run[key]):
            print(f'{lab}: no file {run[key]}', flush=True)
            continue
        raw, g15, g19 = with_dollars(run[key], maps)
        raws[lab], g15s[lab] = raw, g15
        w0 = np.full(len(g15), returns / len(g15))
        S['wealth'][lab] = wealth_stats(g15, w0)
        S['ratios'][lab] = ratios(S['wealth'][lab], S['wealth']['actual SCF'])
        S['meter'][lab], weights = meter(g15, g19, returns, targets, threshold)
        S['after_tilt'][lab] = {k: wealth_stats(g15, w) for k, w in weights.items() if k != 'start'}
        wh = weights['136+six, threshold']
        S['after_tilt'][lab]['holders, 136+six, threshold'] = {
            v: float(wh[holder_flags(raw, v, dollars_of=g15)].sum() / wh.sum()) for v in INV}
    # SCF-style file: net worth only (SCF map), equal weights at the actual SCF's count
    gscf = pd.read_parquet(run['gen_scf'])
    gscf = draw(gscf[gscf['source'] == 'scf'].reset_index(drop=True), {'net_worth': maps['net_worth']})
    S['wealth']['SCF-style file'] = {'all': wt.unconditional(gscf['net_worth'].to_numpy(float),
                                                             np.full(len(gscf), w_scf.sum() / len(gscf)))}
    S['holders'] = holder_shares(run, table, raws['PUF label'], gpuf_dollars=g15s['PUF label'], edges=edges)
    ev = json.load(open(run['eval'])) if os.path.exists(run['eval']) else {}
    S['label_swap'] = label_swap(ev)
    flags = bands_differ(edges, json.load(open(V2_EDGES)))
    S['band_flags'] = flags
    S['held_out'], S['loss_total'], S['loss_comparable'] = held_out(ev, flags)
    finish(S)


def score_e11():
    """E0's PUF-label file tilted to the six wealth targets plus mean net worth
    by wage group (2019 SCF tax units), soft at sigma 2%; with and without the
    136 IRS targets."""
    run = resolve('E0')
    table, edges, maps = run_maps(run)
    scf, w_scf = wt.actual_scf()
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    targets, threshold = wt.wealth_targets()
    means, shares19 = scf2019_wage_means()
    raw, g15, g19 = with_dollars(run['gen_puf'], maps)
    x = g15['net_worth'].to_numpy(float)
    n = len(x)
    w0 = np.full(n, returns / n)
    grp = wt.groups(g15)['wages']
    labs = wt.ORDER['wages']
    Fe = np.column_stack([(x - means[g]) * (grp == g) for g in labs])
    Se = np.array([means[g] * w0[grp == g].sum() for g in labs])
    Te = np.zeros(len(labs))
    one = np.ones((n, 1))
    res, W = {}, {'before': w0}
    w6, l6, h6, ok6 = wealth_tilt(x, w0, one, np.array([returns]), targets, threshold)
    wg, lg, hg, okg = wealth_tilt(x, w0, one, np.array([returns]), targets, threshold, (Fe, Te, Se),
                                  damp=True)
    # Rank-free check: total net worth and the wage-group means only.
    wm, okm, _ = tilt_scaled(w0, np.column_stack([one, x, Fe]),
                             np.r_[returns, targets['total'], Te],
                             np.r_[returns, targets['total'], Se], np.r_[0, SIGMA, np.full(len(Te), SIGMA)])
    res['total + wage-group means only (no rank targets)'] = {**short_move(w0, wm), 'converged': okm}
    W['total + wage means'] = wm
    # The six targets in a rank-free form: net worth and units above the 2019
    # SCF's 50th, 90th and 99th percentiles (2015 dollars), plus the total and
    # units above $1 million. At a solution that matches the SCF these say the
    # same as the rank groups; they need no rank iteration, which with the
    # wage-group means did not settle (damped or not).
    u19, x19, w19, _ = wt.scf2019()
    Fq, Tq = [], []
    for q in dollars.wquantile(x19, w19, [0.99, 0.90, 0.50]):
        a = (x > q).astype(float)
        Fq += [a, x * a]
        Tq += [returns * w19[x19 > q].sum() / w19.sum(), (x19 * w19)[x19 > q].sum()]
    Fq += [x, (x > threshold).astype(float)]
    Tq = np.array(Tq + [targets['total'], targets['units above $1m']])
    Fq = np.column_stack(Fq)
    for name, extra_F, extra_T, extra_S in [('six, threshold form', np.zeros((n, 0)), np.zeros(0), np.zeros(0)),
                                            ('six, threshold form + wage-group means', Fe, Te, Se)]:
        wq, okq, _ = tilt_scaled(w0, np.column_stack([one, Fq, extra_F]), np.r_[returns, Tq, extra_T],
                                 np.r_[returns, np.abs(Tq), extra_S],
                                 np.r_[0, np.full(len(Tq) + len(extra_T), SIGMA)])
        res[name] = {**short_move(w0, wq), 'converged': okq}
        W[name] = wq
    brk, cls = tiltmod.target_set('brackets')
    F136, T136 = tiltmod.contributions(g19, brk), cls['value'].to_numpy(float)
    wq2, okq2, _ = tilt_scaled(w0, np.column_stack([F136, Fq, Fe]), np.r_[T136, Tq, Te],
                               np.r_[np.abs(T136), np.abs(Tq), Se],
                               np.r_[np.zeros(len(T136)), np.full(len(Tq) + len(Te), SIGMA)])
    res['136 IRS + six, threshold form + wage-group means'] = {**short_move(w0, wq2), 'converged': okq2}
    W['136 + six, threshold form + wage means'] = wq2
    res['six wealth targets'] = {**short_move(w0, w6), 'passes': len(l6), 'converged': ok6}
    res['six + wage-group means'] = {**short_move(w0, wg), 'passes': len(lg), 'converged': okg,
                                     'ranks settled': len(lg) < MAX_PASSES,
                                     'miss %': {k: 100 * (hg[k] / targets[k] - 1) for k in hg}}
    W['six'], W['six + wage means'] = w6, wg
    brackets, cells = tiltmod.target_set('brackets')
    F = tiltmod.contributions(g19, brackets)
    T = cells['value'].to_numpy(float)
    w1, ok1, _ = tiltmod.tilt(w0, F, T)
    w2, l2, h2, ok2 = wealth_tilt(x, w0, F, T, targets, threshold, (Fe, Te, Se), damp=True)
    res['136 IRS alone'] = {**short_move(w0, w1), 'converged': ok1}
    res['136 IRS + six + wage-group means'] = {**short_move(w0, w2), 'passes': len(l2), 'converged': ok2,
                                               'ranks settled': len(l2) < MAX_PASSES,
                                               'max abs gap 136 (%)': float(np.abs(100 * (F.T @ w2 / T - 1)).max())}
    res['move beyond the 136 (nats)'] = res['136 IRS + six + wage-group means']['kl_nats'] - res['136 IRS alone']['kl_nats']
    W['136 + six + wage means'] = w2
    S = {'run': 'E11', 'table': 'v2', 'flags': {'tilt': 'six wealth targets + mean net worth by wage group'},
         'files': run, 'wealth': {'actual SCF': wealth_stats(scf, w_scf)}, 'ratios': {}, 'meter': {},
         'wage_targets_2019': {'mean ($, 2015)': means, 'share of units 2019 SCF': shares19}}
    for k, w in W.items():
        S['wealth'][k] = wealth_stats(g15, w)
    # The row reads the file after the tilt to the six (threshold form) plus
    # the wage-group means: the rank form did not settle.
    key = 'six, threshold form + wage-group means'
    S['wealth']['PUF label'] = S['wealth'][key]
    S['ratios']['PUF label'] = ratios(S['wealth'][key], S['wealth']['actual SCF'])
    S['meter']['PUF label'] = {'six wealth targets': res[key]}
    wg = W[key]
    S['meter_all'] = res
    S['holders'] = {v: {'PUF-style generated': float(wg[holder_flags(raw, v)].sum() / wg.sum()),
                        'before tilt': float((holder_flags(raw, v)).mean())} for v in INV}
    S['label_swap'], S['loss_total'], S['loss_note'] = {}, None, 'as E0'
    out = f'{ROOT}/score/E11'
    os.makedirs(out, exist_ok=True)
    S['row'] = summary_row(S)
    json.dump(S, open(f'{out}/score.json', 'w'), indent=1, default=float)
    L = ['# Scoreboard, E11: tilt only', '',
         'The E0 PUF-label file, equal weights at SOI 2019 returns, tilted to the six 2019 SCF wealth '
         'targets plus mean net worth by wage group from the 2019 SCF tax units (wages and net worth '
         'in 2015 dollars), soft at 2%, total returns exact. Wage-group shares are not targeted.', '',
         '| tilt | move (nats) | ESS ratio | passes | converged | records under 0.1x |', '|---|---:|---:|---:|---|---:|']
    for k, r in res.items():
        if isinstance(r, dict):
            L.append(f"| {k} | {r['kl_nats']:.4f} | {r['ess_ratio']:.3f} | {r.get('passes', '—')} | {r['converged']} | {r['records_factor_under_0.1']} |")
    L.append(f"| move beyond the 136 | {res['move beyond the 136 (nats)']:.4f} | | | | |")
    cols = ['actual SCF', 'before', 'six', 'six, threshold form', 'total + wage means',
            'six, threshold form + wage-group means', '136 + six, threshold form + wage means',
            'six + wage means']
    L += ['', '## Net worth, all units', '', '| statistic | ' + ' | '.join(cols) + ' |', '|---|' + '---:|' * len(cols)]
    for k in S['wealth']['actual SCF']['all']:
        p = '(%)' in k
        L.append(f'| {k} | ' + ' | '.join(fmt(S['wealth'][c]['all'][k], 'pct' if p else 'num') for c in cols) + ' |')
    for stat in ['mean ($)', 'median ($)', 'share of units (%)']:
        L += ['', f'## {stat} by wage group (2015 dollars)', '',
              '| wages | 2019 SCF target | ' + ' | '.join(cols) + ' |', '|---|---:|' + '---:|' * len(cols)]
        for g in labs:
            tgt = means[g] if stat == 'mean ($)' else (100 * shares19[g] if stat.startswith('share') else None)
            L.append(f'| {g} | {fmt(tgt, "pct" if stat.startswith("share") else "num")} | ' + ' | '.join(
                fmt(S['wealth'][c]['by_wage'][g][stat], 'pct' if stat.startswith('share') else 'num') for c in cols) + ' |')
    L += ['', 'Ranks settled: ' + ', '.join(f"{k}: {r['ranks settled']}" for k, r in res.items()
                                           if isinstance(r, dict) and 'ranks settled' in r)]
    L += ['', '## Holder shares after the tilt to six (threshold form) + wage-group means (%)', '',
          '| item | before | after |', '|---|---:|---:|']
    for v, r in S['holders'].items():
        L.append(f"| {v} | {100 * r['before tilt']:.1f}% | {100 * r['PUF-style generated']:.1f}% |")
    open(f'{out}/score.md', 'w').write('\n'.join(L) + '\n')
    print('\n'.join(L), flush=True)
    write_summary_table()
    open(f'{ROOT}/markers/SCORE_E11_DONE', 'w').close()


def score_e12(T, label):
    """Run T's file under `label`, tilted to the 136 IRS targets plus the six
    wealth targets: the numbers already in R/score/<T>/score.json."""
    src = json.load(open(f'{ROOT}/score/{T}/score.json'))
    lab = {'puf': 'PUF label', 'scf': 'SCF label'}[label]
    after = src['after_tilt'][lab]['136+six, threshold']
    act = src['wealth']['actual SCF']
    M = src['meter'][lab]['136 IRS + six, threshold form']
    S = {'run': 'E12', 'table': src['table'], 'flags': {'from': T, 'label': lab},
         'files': src['files'], 'wealth': {'actual SCF': act, 'PUF label': after},
         'ratios': {'PUF label': ratios(after, act)},
         'meter': {'PUF label': {'six wealth targets': M}},
         'holders': {v: {'PUF-style generated': s} for v, s in src['after_tilt'][lab]['holders, 136+six, threshold'].items()},
         'label_swap': src['label_swap'], 'loss_total': src['loss_total'],
         'loss_comparable': src['loss_comparable'], 'source_meter': src['meter'][lab]}
    out = f'{ROOT}/score/E12'
    os.makedirs(out, exist_ok=True)
    S['row'] = summary_row(S)
    S['row']['what'] = f'{T} ({lab}) plus the full tilt'
    json.dump(S, open(f'{out}/score.json', 'w'), indent=1, default=float)
    open(f'{out}/score.md', 'w').write(
        f'# E12: {T}, {lab}, tilted to the 136 IRS targets plus the six wealth targets (threshold form)\n\n'
        f'Move {M["kl_nats"]:.4f} nats (136 alone {src["meter"][lab]["136 IRS alone"]["kl_nats"]:.4f}); '
        f'ESS ratio {M["ess_ratio"]:.3f}. Full numbers in R/score/{T}/score.json.\n')
    write_summary_table()
    open(f'{ROOT}/markers/SCORE_E12_DONE', 'w').close()


if __name__ == '__main__':
    pd.set_option('display.width', 250)
    arg = sys.argv[1]
    if arg == 'E11':
        score_e11()
    elif arg == 'E12':
        score_e12(sys.argv[2], sys.argv[3])
    else:
        score_run(arg)
