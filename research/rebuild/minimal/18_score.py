"""
18_score.py  (minimal example, experiment 18: tilt-then-distill, the scoring)

What it does
  Scores the files of the fine-tuned models (18_distill.py) against the same
  things as the reweighted v4b file, with the same code (05_tilt, 06_wealth_
  tables, 07_baseline), and sets them side by side:

    v4b plain        200,000 records from V4, equal weights (no post-training)
    v4b stack, pre   the v4b stack with its importance weights (no post-training)
    v4b reweighted   the v4b stack with the tilted weights (the current
                     post-training)
    v4b redraw       200,000 equal-weight records drawn from the reweighted
                     stack (tilt/v4b_allunits/redraw_v4b.parquet)
    <v> plain        200,000 records from the fine-tuned model <v>, equal weights
    <v> stack        the fine-tuned model's oversampled stack, importance weights
    <v> stack + final reweight   that stack tilted to the same targets as v4b
                     (136 exact, eight wealth soft at 2%); its move is what the
                     fine-tune left for a reweight to do

  Every file is scaled to SOI's 2019 return count. Dollars as in
  07_baseline.py: incomes from the V4 2019 map (2015 map for the wealth
  groupings), net worth from the 2015 SCF map.

  For each file: the 136 IRS targets (estimate, standard error, z), the
  eight wealth targets in threshold form, net worth statistics, net worth by
  wage group, holder shares, and the untargeted relationships of 05_tilt.
  Standard errors use the linearised formula for a weighted total with the
  weights scaled to a fixed sum: R^2 sum p_i^2 (f_i - fbar)^2, p = w / sum w.

Usage (through Slurm, 18_score.sbatch): python 18_score.py A [A20 B ...]
Writes checks/D18/score_long.csv, checks/D18/score_tables.md, and for each
variant tilt/D18/<v>/summary.csv and weights_final.parquet.
"""
import importlib.util
import os
import sys

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
os.environ.setdefault('MINIMAL_MAP_DIR', f'{R}/map/V4')

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


base = load('base18', '07_baseline.py')
wt, dollars, tiltmod = base.wt, base.dollars, base.tiltmod
OUT = f'{R}/checks/D18'
V4B_STACK = f'{R}/gen/gen_puf_stack_v4b.parquet'
V4B_PLAIN = f'{R}/gen/V4b/gen_puf_with_net_worth_ownerlabel_absent.parquet'
V4B_TILT = f'{R}/tilt/v4b_allunits'

BRACKETS, CELLS = tiltmod.target_set('brackets')
T136 = CELLS['value'].to_numpy(float)
RETURNS = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
TARGETS, THRESHOLD = wt.wealth_targets('all')
WEALTH_NAMES = ['units above SCF P99', 'net worth above SCF P99', 'units above SCF P90',
                'net worth above SCF P90', 'units above SCF P50', 'net worth above SCF P50',
                'total net worth', 'units above $1 million']


def dollars_of(path):
    """2019 and 2015 dollars for a generated file, as 07_baseline.py."""
    os.environ['GEN_FILE'] = path
    g19 = tiltmod.generated_2019()
    raw = pd.read_parquet(path)
    raw = raw[raw['source'] == 'puf'].reset_index(drop=True)
    g15, _ = dollars.draw_dollars(raw, dollars.puf_maps(2015))
    assert len(g15) == len(g19) and (g19['net_worth_band'] >= 0).all()
    return g19, g15


def score(name, g19, g15, w):
    """Long rows: (file, group, key, value)."""
    w = np.asarray(w, float)
    w = w * RETURNS / w.sum()
    p = w / w.sum()
    rows = []

    def add(group, key, value):
        rows.append({'file': name, 'group': group, 'key': key, 'value': float(value)})

    F = tiltmod.contributions(g19, BRACKETS)
    est = F.T @ w
    fbar = p @ F
    se = RETURNS * np.sqrt((p[:, None] ** 2 * (F - fbar) ** 2).sum(0))
    z = np.divide(est - T136, se, out=np.full(len(T136), np.nan), where=se > 0)
    for (b, c), e, s, zz in zip(CELLS[['bracket', 'cell']].itertuples(index=False), est, se, z):
        add('irs estimate', f'{b} | {c}', e)
        add('irs se', f'{b} | {c}', s)
        add('irs z', f'{b} | {c}', zz)
    ok = np.isfinite(z)
    miss = 100 * (est / T136 - 1)
    add('irs summary', 'targets with a standard error', ok.sum())
    add('irs summary', 'within 2 standard errors', (np.abs(z[ok]) <= 2).sum())
    add('irs summary', 'largest |z|', np.nanmax(np.abs(z)))
    add('irs summary', 'mean |z|', np.nanmean(np.abs(z)))
    add('irs summary', 'median |% miss|', np.median(np.abs(miss)))
    add('irs summary', 'joint returns % miss', miss[-1])
    x15 = g15['net_worth'].to_numpy(float)
    Fq, Tq = base.threshold_form(x15, RETURNS, TARGETS, THRESHOLD, 'all')
    for k, f, t in zip(WEALTH_NAMES, Fq.T, Tq):
        add('wealth target miss %', k, 100 * (f @ w / t - 1))
    for k, v in wt.unconditional(x15, w).items():
        add('net worth', k, v)
    c = wt.conditional(g15, x15, w)
    for r in c[c.grouping == 'wages'].itertuples():
        for stat in ['mean ($)', 'median ($)', 'share above $1m (%)', 'share of units (%)']:
            add(f'by wages: {stat}', r.group, c.loc[r.Index, stat])
    for v in ['interest', 'dividends', 'capital_gains']:
        add('holders (%)', v, 100 * w[g19[v].to_numpy(float) != 0].sum() / w.sum())
    add('holders (%)', 'joint', 100 * w[g19['married'].to_numpy() == 1].sum() / w.sum())
    rel = tiltmod.relationships(g19, w, BRACKETS)
    rel = rel[rel['relationship'] != 'any dividends, by AGI bracket']
    for r in rel.itertuples():
        add('untargeted (%)', f'{r.relationship} | {r.group}', 100 * r.share)
    return rows


def final_reweight(v, g19, g15, imp):
    """The fine-tuned stack tilted to the v4b targets: what a small final
    reweight has to do."""
    os.makedirs(f'{R}/tilt/D18/{v}', exist_ok=True)
    w0 = imp * RETURNS / imp.sum()
    F = tiltmod.contributions(g19, BRACKETS)
    tiltmod.check_reachable(F, CELLS)
    x = g15['net_worth'].to_numpy(float)
    Fq, Tq = base.threshold_form(x, RETURNS, TARGETS, THRESHOLD, 'all')
    w1, ok1, _ = tiltmod.tilt(w0, F, T136)
    w2, ok2, _ = tiltmod.tilt(w0, np.column_stack([F, Fq]), np.r_[T136, Tq],
                              rel_tol=np.r_[np.zeros(len(T136)), np.full(len(Tq), wt.SIGMA)])
    rows = []
    for name, ww, ok in [('136 targets', w1, ok1), ('136 exact + 8 soft wealth', w2, ok2)]:
        p0, p1 = w0 / w0.sum(), ww / ww.sum()
        pos = p1 > 0
        mv = tiltmod.move(w0, ww)
        mv['kl_nats'] = float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos])))   # 0 log 0 = 0
        rows.append({'variant': v, 'run': name, 'converged': ok,
                     'max_abs_gap_136_pct': float(np.abs(100 * (F.T @ ww / T136 - 1)).max()), **mv})
    pd.DataFrame(rows).to_csv(f'{R}/tilt/D18/{v}/summary.csv', index=False)
    pd.DataFrame({'record_id': g19['record_id'], 'weight_before': w0, 'weight_after_136': w1,
                  'weight_after': w2}).to_parquet(f'{R}/tilt/D18/{v}/weights_final.parquet', index=False)
    return w2, rows


def main():
    variants = sys.argv[1:]
    os.makedirs(OUT, exist_ok=True)
    rows, moves = [], []
    # v4b: plain, stack before and after the tilt, redraw
    g19, g15 = dollars_of(V4B_PLAIN)
    rows += score('v4b plain', g19, g15, np.ones(len(g19)))
    g19, g15 = dollars_of(V4B_STACK)
    w = pd.read_parquet(f'{V4B_TILT}/weights_v4b.parquet')
    assert (w['record_id'].to_numpy() == g19['record_id'].to_numpy()).all()
    rows += score('v4b stack, pre', g19, g15, w['weight_before'])
    rows += score('v4b reweighted', g19, g15, w['weight_after'])
    rd = pd.read_parquet(f'{V4B_TILT}/redraw_v4b.parquet')
    idx = rd['stack_row'].to_numpy()
    rows += score('v4b redraw', g19.iloc[idx].reset_index(drop=True),
                  g15.iloc[idx].reset_index(drop=True), np.ones(len(idx)))
    for v in variants:
        gd = f'{R}/gen/D18/{v}'
        g19, g15 = dollars_of(f'{gd}/gen_puf_plain_d18{v}.parquet')
        rows += score(f'{v} plain', g19, g15, np.ones(len(g19)))
        g19, g15 = dollars_of(f'{gd}/gen_puf_stack_d18{v}.parquet')
        imp = g19['weight'].to_numpy(float)
        rows += score(f'{v} stack', g19, g15, imp)
        w2, mv = final_reweight(v, g19, g15, imp)
        moves += mv
        rows += score(f'{v} stack + final reweight', g19, g15, w2)
        print(v, 'scored', flush=True)
    long = pd.DataFrame(rows)
    long.to_csv(f'{OUT}/score_long.csv', index=False)
    pd.DataFrame(moves).to_csv(f'{OUT}/final_reweight_moves.csv', index=False)
    # markdown: one wide table per group (the IRS cells summarised)
    files = list(dict.fromkeys(long['file']))
    out = []
    for grp in dict.fromkeys(long['group']):
        if grp in ('irs estimate', 'irs se'):
            continue
        t = long[long.group == grp].pivot(index='key', columns='file', values='value')[files]
        t = t.reindex(list(dict.fromkeys(long.loc[long.group == grp, 'key'])))
        out += [f'## {grp}', '', t.to_markdown(floatfmt='.4g'), '']
    out += ['## final reweight moves', '', pd.DataFrame(moves).to_markdown(index=False, floatfmt='.4g')]
    open(f'{OUT}/score_tables.md', 'w').write('\n'.join(out) + '\n')
    print('\n'.join(out))


if __name__ == '__main__':
    main()
