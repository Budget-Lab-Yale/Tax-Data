"""
06_wealth_tables.py  (minimal example, step 6: wealth tables)

What it does. Compares net worth in the actual SCF with net worth on
generated records, before and after the tilt. Four columns:

  actual SCF          every SCF row in the step 1 table (all splits), survey
                      weights, net worth in 2015 dollars.
  pretrain SCF        gen/gen_scf.parquet: SCF-labelled generated records,
                      dollars drawn from the 2015 SCF map (step 4). Equal
                      weights summing to the actual SCF's weighted count.
  pretrain PUF-style  gen/gen_puf_with_net_worth.parquet: PUF-labelled
                      generated records that also carry a net worth band.
                      Incomes drawn from the 2015 PUF map, net worth from the
                      2015 SCF map (step 4's puf_maps). Equal weights summing
                      to SOI's 2019 return count, the same start as run B1.
  posttrain PUF-style the same records, reweighted by step 5's tilt to the
                      136 targets of run B1 (returns, item counts and item
                      amounts by AGI bracket, plus joint returns), with the
                      incomes in 2019 dollars from the 2019 map, as B1 did.
                      Net worth is not a target; it stays in 2015 dollars.
  posttrain, wealth   the same tilt plus six soft wealth targets (added
  targeted            2026-10-14 at John's request) from the 2019 SCF wave's
                      tax units (06_build_scf2019.R): total net worth; net
                      worth held by the top 1%, next 9%, next 40% and bottom
                      50% of units ranked by net worth; units with net worth
                      above $1 million (2019 dollars). In 2015 dollars, the
                      records' basis. Soft, each with a standard deviation of
                      2% of the target, through 05_tilt's solver (rel_tol).
                      Group targets depend on ranks under the new weights,
                      so ranks are fixed, the tilt solved, ranks recomputed,
                      until every wealth statistic changes by less than one
                      part in 10,000.

The 2015 and 2019 draws use the same seed and the 2019 map is the 2015 map
with each value scaled, so each record's 2019 dollars are its 2015 dollars
grown. The tables group records by their 2015 dollars.

Tables (weighted), one column per file above:
  1. Unconditional: total, mean, median, share negative, top 10/1/0.1%
     wealth shares, 90th/99th/99.9th percentiles.
  2. Mean and median net worth by wage group, by joint or not, and by
     dividend group; and each group's share of units.
  3. Share of units with net worth above $1 million, by wage group.

Reuses 04_dollars.py (draw_dollars, puf_maps, split_by_variable, wquantile)
and 05_tilt.py (generated_2019, target_set, contributions, tilt, move); does
not change them and writes nothing into their folders.

Writes, in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/wealth/:
  tables.md          the tables, formatted to 3 significant figures, with
                     the records behind each group
  tables.csv         the same numbers unrounded
  tilt_summary.csv   the tilt's fit and move for this file
  weights.parquet    record_id, weight before and after the tilt
  weights_wealth.parquet   the same for the wealth-targeted tilt
  wealth_tilt_passes.csv   one row per pass of the rank iteration
  wealth_targets.csv       each wealth target (2019 SCF) and the statistic
                           before the tilt, after it, and after the
                           wealth-targeted tilt
  moves.csv                the move (nats) and effective sample size ratio,
                           with and without the wealth targets
"""
import importlib.util
import os
import sys
import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{ROOT}/wealth'
GEN_PUF_NW = f'{ROOT}/gen/gen_puf_with_net_worth.parquet'
GEN_SCF = f'{ROOT}/gen/gen_scf.parquet'
HERE = os.path.dirname(os.path.abspath(__file__))
COLS = ['actual SCF', 'pretrain SCF', 'pretrain PUF-style', 'posttrain PUF-style',
        'posttrain, wealth targeted']

# Wealth targets: the 2019 SCF wave, couple and single tax units built by
# 06_build_scf2019.R (split-off units dropped, as in step 1). Wealth in that
# wave is measured in 2019, matching a tax-year-2019 file. Its net worth is
# used in 2015 dollars (net_worth_2015_dollars: the SCFP's 2022-dollar value
# over 1.2493, the factor step 1 used for the 2016 wave), the price basis of
# the records' net worth. Above $1 million means $1 million in 2019 dollars,
# which is ASSET_FACTOR_2019 / 1.2493 million in 2015 dollars.
SCF2019 = f'{ROOT}/data/scf2019_units.parquet'
SCF2019_CHECKS = f'{ROOT}/data/checks_scf2019.csv'
FACTOR_2016_WAVE = 1.2493
GROUP_BOUNDS = {'top 1%': (0.0, 0.01), 'next 9%': (0.01, 0.10),
                'next 40%': (0.10, 0.50), 'bottom 50%': (0.50, 1.0)}
WEALTH_KEYS = ['total'] + list(GROUP_BOUNDS) + ['units above $1m']
SIGMA = 0.02            # soft-target standard deviation, share of each target
MAX_PASSES = 50


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


dollars = load('dollars', '04_dollars.py')
tiltmod = load('tiltmod', '05_tilt.py')


# ---------------------------------------------------------------- statistics

def top_share(x, w, p):
    """Share of total net worth held by the top p of units (by weight),
    counting the boundary unit's weight in part."""
    o = np.argsort(-x, kind='stable')
    xs, ws = x[o], w[o]
    cap = p * w.sum()
    cum = np.cumsum(ws)
    take = np.clip(cap - (cum - ws), 0, ws)      # weight taken from each unit
    return (xs * take).sum() / (x * w).sum()


def unconditional(x, w):
    q = dollars.wquantile(x, w, [0.5, 0.9, 0.99, 0.999])
    return {'units (millions)': w.sum() / 1e6,
            'total net worth ($ trillion)': (x * w).sum() / 1e12,
            'mean ($)': (x * w).sum() / w.sum(),
            'median ($)': q[0],
            'share negative (%)': 100 * w[x < 0].sum() / w.sum(),
            'top 10% share (%)': 100 * top_share(x, w, 0.10),
            'top 1% share (%)': 100 * top_share(x, w, 0.01),
            'top 0.1% share (%)': 100 * top_share(x, w, 0.001),
            '90th percentile ($)': q[1],
            '99th percentile ($)': q[2],
            '99.9th percentile ($)': q[3]}


def groups(df):
    """The three groupings, from 2015 dollars."""
    wage = pd.cut(df['wages'], [-np.inf, 0, 25e3, 50e3, 100e3, 200e3, np.inf],
                  labels=['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+'])
    div = pd.cut(df['dividends'], [-np.inf, 0, 1e3, 10e3, np.inf],
                 labels=['0', '1-1k', '1k-10k', '10k+'])
    joint = df['married'].map({1: 'joint', 0: 'not joint'})
    assert not joint.isna().any(), 'married must be 0 or 1'
    return {'wages': wage.astype(str).to_numpy(), 'filing status': joint.to_numpy(),
            'dividends': div.astype(str).to_numpy()}


ORDER = {'wages': ['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+'],
         'filing status': ['joint', 'not joint'],
         'dividends': ['0', '1-1k', '1k-10k', '10k+']}


def conditional(df, x, w):
    rows = []
    for gname, g in groups(df).items():
        for lab in ORDER[gname]:
            m = g == lab
            if not m.any():
                rows.append({'grouping': gname, 'group': lab, 'records': 0})
                continue
            rows.append({'grouping': gname, 'group': lab, 'records': int(m.sum()),
                         'share of units (%)': 100 * w[m].sum() / w.sum(),
                         'mean ($)': (x[m] * w[m]).sum() / w[m].sum(),
                         'median ($)': dollars.wquantile(x[m], w[m], [0.5])[0],
                         'share above $1m (%)': 100 * w[m & (x > 1e6)].sum() / w[m].sum()})
    return pd.DataFrame(rows)


# ---------------------------------------------------------------- the four files

def actual_scf():
    df = pd.read_parquet(f'{ROOT}/data/stacked.parquet')
    df = df[df['source'] == 'scf'].reset_index(drop=True)
    need = ['net_worth', 'wages', 'dividends', 'married', 'weight']
    assert not df[need].isna().any().any(), 'missing values in actual SCF columns used'
    return df, df['weight'].to_numpy(float)


def pretrain_scf(scf_total_weight):
    gen = pd.read_parquet(GEN_SCF)
    assert (gen['source'] == 'scf').all()
    gen, hits = dollars.draw_dollars(gen, dollars.split_by_variable(
        pd.read_parquet(f'{ROOT}/map/map_scf_2015.parquet')))
    print(f'pretrain SCF: {len(gen)} records; fill-rule draws {hits}')
    assert not gen[['net_worth', 'wages', 'dividends', 'married']].isna().any().any()
    return gen, np.full(len(gen), scf_total_weight / len(gen))


def pretrain_and_posttrain_puf():
    # 2019 dollars and record ids exactly as run B1 builds them, on this file.
    os.environ['GEN_FILE'] = GEN_PUF_NW
    g19 = tiltmod.generated_2019()
    assert (g19['net_worth_band'] >= 0).all(), 'a PUF-style record lacks a net worth band'
    # 2015 dollars, same seed: each record's 2015 draw is its 2019 draw ungrown.
    raw = pd.read_parquet(GEN_PUF_NW)
    raw = raw[raw['source'] == 'puf'].reset_index(drop=True)
    g15, hits = dollars.draw_dollars(raw, dollars.puf_maps(2015))
    print(f'pretrain PUF-style: {len(g15)} records; fill-rule draws {hits}')
    g15['record_id'] = g19['record_id'].to_numpy()
    assert np.array_equal(g15['net_worth'].to_numpy(), g19['net_worth'].to_numpy())
    factors = pd.read_csv(f'{ROOT}/map/growth_factors.csv')
    for v, f in zip(factors['variable'], factors['factor']):
        assert np.allclose(dollars.grow_values(v, g15[v], f), g19[v], rtol=1e-12), v

    # The tilt, B1's way: equal start summing to the 2019 return count, 136 targets.
    brackets, cells = tiltmod.target_set('brackets')
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    w0 = np.full(len(g19), returns / len(g19))
    F = tiltmod.contributions(g19, brackets)
    T = cells['value'].to_numpy(float)
    tiltmod.check_reachable(F, cells)
    w1, converged, iters = tiltmod.tilt(w0, F, T)
    gap = 100 * (F.T @ w1 / T - 1)
    summary = {'file': GEN_PUF_NW, 'records': len(g19), 'targets': len(T),
               'converged': converged, 'newton_steps': iters,
               'max_abs_gap_after_pct': np.abs(gap).max(), **tiltmod.move(w0, w1)}
    print(pd.Series(summary).to_string(), flush=True)
    assert converged, 'tilt did not converge'
    pd.DataFrame([summary]).to_csv(f'{OUT}/tilt_summary.csv', index=False)
    pd.DataFrame({'record_id': g19['record_id'], 'weight_before': w0,
                  'weight_after': w1}).to_parquet(f'{OUT}/weights.parquet', index=False)
    return g15, w0, w1, F, T, cells


# The filing rule (added 2026-10-15 for the v4 baseline, 15_BASELINE_V4.md):
# which 2019 SCF tax units would file a return. Gross income is the sum of
# the income components the units file has: wages, interest, dividends and
# capital gains (gains only; a loss does not lower gross income). A unit
# files when gross income is at or above the 2019 filing threshold for its
# filing status: $24,400 married filing jointly (married = 1), $12,200
# otherwise. The units file has no age and no self-employment income, so the
# age-65 addition and the $400 self-employment test are not applied, and no
# head-of-household threshold is used. The incomes are the wave's 2018
# amounts in 2018 dollars, compared with the 2019 thresholds as given.
FILING_THRESHOLD_2019 = {'joint': 24_400.0, 'single': 12_200.0}
FILING_THRESHOLD_2018 = {'joint': 24_000.0, 'single': 12_000.0}   # for the record only


def gross_income(u):
    parts = u[['wages', 'interest', 'dividends']].to_numpy(float)
    assert not np.isnan(parts).any() and not u['capital_gains'].isna().any()
    return parts.sum(axis=1) + np.clip(u['capital_gains'].to_numpy(float), 0, None)


def would_file(u, thresholds=FILING_THRESHOLD_2019):
    """True for the units that would file under the rule above."""
    married = u['married'].to_numpy()
    assert np.isin(married, [0, 1]).all()
    line = np.where(married == 1, thresholds['joint'], thresholds['single'])
    return gross_income(u) >= line


def scf2019(units='all'):
    """The 2019 SCF units, their net worth in 2015 dollars, their weights,
    and the $1 million (2019 dollars) threshold in 2015 dollars.
    units: 'all' (every couple and single unit, as before) or 'filers' (the
    units that would file, would_file)."""
    assert units in ('all', 'filers'), units
    u = pd.read_parquet(SCF2019)
    if units == 'filers':
        u = u[would_file(u)].reset_index(drop=True)
    c = pd.read_csv(SCF2019_CHECKS)
    asset_factor = c.loc[c['key'] == 'asset_factor_used_for_net_worth', 'value'].item()
    threshold = 1e6 * asset_factor / FACTOR_2016_WAVE
    return u, u['net_worth_2015_dollars'].to_numpy(float), u['weight'].to_numpy(float), threshold


def rank_groups(x, w):
    """Each record's net worth group under weights w: units ranked from the
    top, a record placed by the midpoint of its weight in the ranking."""
    o = np.argsort(-x, kind='stable')
    cum = np.cumsum(w[o]) - w[o] / 2
    pos = np.empty(len(x))
    pos[o] = cum / w.sum()
    return {g: (pos >= lo) & (pos < hi) for g, (lo, hi) in GROUP_BOUNDS.items()}


def wealth_held(x, w, threshold):
    """The wealth target statistics under w: total net worth, net worth held
    by each group (groups ranked under w), units above the threshold."""
    m = rank_groups(x, w)
    return {'total': (x * w).sum(), **{g: (x * w)[m[g]].sum() for g in GROUP_BOUNDS},
            'units above $1m': w[x > threshold].sum()}


def wealth_targets(units='all'):
    u, x, w, threshold = scf2019(units)
    return wealth_held(x, w, threshold), threshold


def tilt_with_wealth(x, w0, F, T, targets, threshold):
    """Exact targets F, T plus the soft wealth targets. 05_tilt.tilt's rel_tol
    makes a target soft: it minimizes the move plus, per soft target,
    (miss / target)^2 / (2 * SIGMA^2), keeping the exponential weight form.
    The four group targets depend on ranks under the new weights, so ranks
    are fixed, the tilt solved from w0, ranks recomputed, and so on until
    every wealth statistic changes by less than one part in 10,000 between
    passes. Returns the weights, one row per pass, the final statistics and
    whether the last solve converged."""
    Tw = np.array([targets[k] for k in WEALTH_KEYS])
    rel_tol = np.r_[np.zeros(len(T)), np.full(len(WEALTH_KEYS), SIGMA)]
    above = (x > threshold).astype(float)
    w_rank, prev, log = w0, None, []
    for p in range(1, MAX_PASSES + 1):
        m = rank_groups(x, w_rank)
        Fw = np.column_stack([x] + [x * m[g] for g in GROUP_BOUNDS] + [above])
        w1, converged, iters = tiltmod.tilt(w0, np.column_stack([F, Fw]), np.r_[T, Tw],
                                            rel_tol=rel_tol)
        held = wealth_held(x, w1, threshold)
        moved = int(sum((rank_groups(x, w1)[g] != m[g]).sum() for g in GROUP_BOUNDS))
        change = np.nan if prev is None else max(abs(held[k] / prev[k] - 1) for k in WEALTH_KEYS)
        log.append({'pass': p, 'solver_converged': converged, 'newton_steps': iters,
                    'records_changing_group': moved, 'max_rel_change': change,
                    **{f'{k} miss %': 100 * (held[k] / targets[k] - 1) for k in WEALTH_KEYS}})
        print(log[-1], flush=True)
        if prev is not None and change < 1e-4:
            break
        prev, w_rank = held, w1
    return w1, pd.DataFrame(log), held, converged


def miss_table(x, targets, threshold, weights):
    """Each wealth target and the statistic under each set of weights."""
    rows = []
    for k in WEALTH_KEYS:
        r = {'target': k, 'scf2019': targets[k]}
        for name, w in weights.items():
            v = wealth_held(x, w, threshold)[k]
            r[name] = v
            r[f'{name} miss %'] = 100 * (v / targets[k] - 1)
        rows.append(r)
    return pd.DataFrame(rows)


# ---------------------------------------------------------------- formatting

def sig3(v, pct=False):
    """Three significant figures; trailing zeros kept (11.0%, not 11%)."""
    if v is None or (isinstance(v, float) and np.isnan(v)):
        return '—'
    if v == 0:
        return '0%' if pct else '0'
    r = float(f'{v:.3g}')
    s = f'{r:,.0f}' if abs(r) >= 100 else f'{v:#.3g}'.rstrip('.')
    return s + '%' if pct else s


def md_table(index_name, rows, data, pct, names=None):
    """rows: keys into data[column]; pct: format as percentages."""
    names = names or rows
    lines = [f'| {index_name} | ' + ' | '.join(COLS) + ' |',
             '|---|' + '---:|' * len(COLS)]
    for key, name in zip(rows, names):
        p = pct if isinstance(pct, bool) else pct(key)
        lines.append(f'| {name} | ' + ' | '.join(sig3(data[c].get(key), p) for c in COLS) + ' |')
    return '\n'.join(lines)


def main():
    os.makedirs(OUT, exist_ok=True)
    scf, w_scf = actual_scf()
    gscf, w_gscf = pretrain_scf(w_scf.sum())
    gpuf, w0, w1, F, T, cells = pretrain_and_posttrain_puf()
    x = gpuf['net_worth'].to_numpy(float)
    targets, threshold = wealth_targets()
    w2, passes, held, solver_ok = tilt_with_wealth(x, w0, F, T, targets, threshold)
    passes.to_csv(f'{OUT}/wealth_tilt_passes.csv', index=False)
    pd.DataFrame({'record_id': gpuf['record_id'], 'weight_before': w0,
                  'weight_after': w2}).to_parquet(f'{OUT}/weights_wealth.parquet', index=False)
    gap = 100 * (F.T @ w2 / T - 1)
    moves = pd.DataFrame([{'run': 'posttrain PUF-style (136 targets)', **tiltmod.move(w0, w1)},
                          {'run': 'posttrain, wealth targeted (136 exact + 6 soft)',
                           **tiltmod.move(w0, w2), 'passes': len(passes),
                           'solver_converged': solver_ok,
                           'max_abs_gap_136_targets_pct': np.abs(gap).max()}])
    moves.to_csv(f'{OUT}/moves.csv', index=False)
    miss = miss_table(x, targets, threshold, {'pretrain': w0, 'posttrain': w1, 'wealth targeted': w2})
    miss.to_csv(f'{OUT}/wealth_targets.csv', index=False)
    print(f'threshold for $1m (2019 dollars) in 2015 dollars: {threshold:,.0f}')
    print(moves.to_string(), '\n', miss.to_string(), flush=True)
    files = {'actual SCF': (scf, w_scf), 'pretrain SCF': (gscf, w_gscf),
             'pretrain PUF-style': (gpuf, w0), 'posttrain PUF-style': (gpuf, w1),
             'posttrain, wealth targeted': (gpuf, w2)}

    t1, t2 = {}, {}
    for c, (df, w) in files.items():
        x = df['net_worth'].to_numpy(float)
        t1[c] = unconditional(x, w)
        t2[c] = conditional(df, x, w)
    long = pd.concat([pd.DataFrame([t1[c]]).melt(var_name='statistic').assign(column=c, table=1)
                      for c in COLS] +
                     [t2[c].melt(id_vars=['grouping', 'group', 'records'], var_name='statistic')
                      .assign(column=c, table=2) for c in COLS], ignore_index=True)
    long.to_csv(f'{OUT}/tables.csv', index=False)

    labs1 = list(t1['actual SCF'])
    out = ['## Table 1. Net worth, all units', '',
           md_table('', labs1, t1, pct=lambda k: '(%)' in k,
                    names=[k.replace(' (%)', '') for k in labs1]), '']
    for stat, title in [('mean ($)', 'Table 2a. Mean net worth by group ($)'),
                        ('median ($)', 'Table 2b. Median net worth by group ($)'),
                        ('share of units (%)', 'Table 2c. Share of units in each group'),
                        ('share above $1m (%)', 'Table 3. Share of units with net worth above $1 million'),
                        ('records', 'Records in each group (unweighted)')]:
        out += [f'## {title}', '']
        for gname in (['wages'] if stat == 'share above $1m (%)' else ORDER):
            data = {c: {r.group: r.value for r in t2[c][t2[c]['grouping'] == gname]
                        .rename(columns={stat: 'value'}).itertuples()} for c in COLS}
            if stat == 'records':
                data = {c: {k: f'{int(v):,}' for k, v in d.items()} for c, d in data.items()}
                lines = [f'| {gname} | ' + ' | '.join(COLS) + ' |', '|---|' + '---:|' * len(COLS)]
                lines += [f'| {g} | ' + ' | '.join(data[c][g] for c in COLS) + ' |' for g in ORDER[gname]]
                tab = '\n'.join(lines)
            else:
                tab = md_table(gname, ORDER[gname], data, pct=stat.endswith('(%)'))
            out += [tab, '']
    text = '\n'.join(out)
    open(f'{OUT}/tables.md', 'w').write(text)
    print(text)


if __name__ == '__main__':
    sys.exit(main())
