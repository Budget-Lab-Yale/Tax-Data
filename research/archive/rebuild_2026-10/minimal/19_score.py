"""
19_score.py  (minimal example, step 19: score the adjusted networks against reweighted v4b)

Usage (through Slurm, 19_cpu.sbatch), from the repo root:
  python research/rebuild/minimal/19_score.py <tag> [<tag> ...]

For each tag of 19_direct.py (gen/G19/<tag>/), the fresh equal-weight draws
of the adjusted network are scored against the same things as the
reweighted v4b file (tilt/v4b_allunits/), side by side:
  - the 136 IRS targets: estimates, standard errors of an equal-weight file,
    count within 2 standard errors, the largest misses (first 200,000 records,
    the size of v4b's redraw, and all 400,000);
  - the eight wealth targets (threshold form, all 2019 SCF units);
  - net worth against the 2019 SCF (total, mean, median, top shares);
  - net worth by wage group; holder shares; the untargeted relationships;
  - the move: KL(adjusted || V4) from the draws (from 19_direct's meta.json)
    next to the reweighting's 0.0243 nats;
  - the order check: draws in a random block-1 order against the targets;
  - where the change goes: on the first-round V4 sample, the network's
    per-record log change against the tilt's per-record log factor.
Columns: "SCF" the 2019 SCF, all units; "V4 (fixed order)" V4's own draws
in the fixed order (gen/G19/pretrain_fixed.parquet); "v4b stack, before"
and "v4b reweighted" the stack with its starting and tilted weights;
"direct <tag>" the adjusted network's fixed-order draws.

Writes checks/G19/scores.md and CSVs next to it.
"""
import importlib.util
import json
import os
import sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('d19', os.path.join(HERE, '19_direct.py'))
d19 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(d19)
ROOT, wt, base, tiltmod, dollars = d19.ROOT, d19.wt, d19.base, d19.tiltmod, d19.dollars
OUT = d19.OUT_CHECK
V4B = f'{ROOT}/tilt/v4b_allunits'


def md(df, digits=4):
    def f(x):
        if isinstance(x, (float, np.floating)):
            return '—' if np.isnan(x) else f'{x:,.0f}' if abs(x) >= 1e4 else f'{x:.{digits}g}'
        return str(x)
    return '\n'.join(['| ' + ' | '.join(map(str, df.columns)) + ' |', '|' + '---|' * len(df.columns)] +
                     ['| ' + ' | '.join(f(x) for x in r) + ' |' for r in df.itertuples(index=False)])


def fit136(tg, g19, n=None):
    """Equal-weight file: estimate, standard error, z for the 136 cells."""
    if n is not None:
        g19 = g19.iloc[:n].reset_index(drop=True)
    F = tiltmod.contributions(g19, tg.brackets)
    T = tg.cells['value'].to_numpy(float)
    k = len(F)
    est = tg.R * F.mean(0)
    se = tg.R * F.std(0) / np.sqrt(k)
    z = np.divide(est - T, se, out=np.full(len(T), np.nan), where=se > 0)
    return tg.cells.assign(estimate=est, std_error=se, z=z, miss_pct=100 * (est / T - 1))


def fit_summary(f):
    big = f.loc[f['z'].abs().nlargest(3).index]
    return {'within 2 SE': f'{int((f["z"].abs() <= 2).sum())} of {int(f["z"].notna().sum())}',
            'largest |z|': float(f['z'].abs().max()),
            'RMS miss (%)': float(np.sqrt((f['miss_pct'] ** 2).mean())),
            'largest |miss| (%)': float(f['miss_pct'].abs().max()),
            'cell with largest |miss|': f.loc[f['miss_pct'].abs().idxmax(), 'bracket'] + ' / ' +
            f.loc[f['miss_pct'].abs().idxmax(), 'cell'],
            'three largest |z|': '; '.join(f'{r.bracket} / {r.cell} ({r.miss_pct:+.1f}%, z {r.z:.2f})'
                                           for r in big.itertuples())}


def wealth8(tg, x, w):
    Fq, Tq = base.threshold_form(x, tg.R, tg.wealth, tg.threshold, 'all')
    est = Fq.T @ w
    return pd.Series(100 * (est / Tq - 1), index=tg.names[tg.n_exact:])


def files(tags):
    """Every file scored: (2019-dollar table, 2015-dollar table, weights)."""
    tg = d19.Targets()
    out = {}
    # v4b stack, before and after the tilt, dollars exactly as 07_baseline draws them
    stack = pd.read_parquet(f'{ROOT}/gen/gen_puf_stack_v4b.parquet')
    s19, _ = dollars.draw_dollars(stack, tg.maps19)
    s15, _ = dollars.draw_dollars(stack, tg.maps15)
    w = pd.read_parquet(f'{V4B}/weights_v4b.parquet')
    out['v4b stack, before'] = (s19, s15, w['weight_before'].to_numpy(float))
    out['v4b reweighted'] = (s19, s15, w['weight_after'].to_numpy(float))
    for name, path in [('V4 (fixed order)', f'{d19.OUT_GEN}/pretrain_fixed.parquet')] + \
            [(f'direct {t}', f'{d19.OUT_GEN}/{t}/final_fixed.parquet') for t in tags]:
        df = pd.read_parquet(path)
        g19, g15 = tg.dollars(d19.table_to_values(df))
        out[name] = (g19, g15, np.full(len(df), tg.R / len(df)))
    return tg, out


def main(tags):
    os.makedirs(OUT, exist_ok=True)
    tg, fl = files(tags)
    names = list(fl)
    text = []

    # ---- the move
    rows = [{'file': 'v4b reweighted', 'KL from V4 (nats)': 0.0243, 'how measured':
             'tilt weights against the stack\'s starting weights'}]
    for t in tags:
        meta = json.load(open(f'{d19.OUT_TILT}/{t}/meta.json'))
        rnd = pd.read_csv(f'{d19.OUT_TILT}/{t}/rounds.csv')
        rows.append({'file': f'direct {t}', 'KL from V4 (nats)': meta['kl_from_draws'],
                     'how measured': f'mean log q/p over {meta.get("final_n", 400000):,} fresh draws, '
                                     f'standard error {meta["kl_se"]:.4f}'})
        rows.append({'file': f'exact tilt on direct {t}\'s first sample', 'KL from V4 (nats)':
                     rnd['tilt_kl'].iloc[0], 'how measured': 'tilt weights against p/s, same order and sample'})
    move = pd.DataFrame(rows)
    text += ['## Move', '', md(move), '']
    for t in tags:
        meta = json.load(open(f'{d19.OUT_TILT}/{t}/meta.json'))
        text += [f'KL by step ({t}): ' + ', '.join(f'{k} {v:.4f}' for k, v in meta['kl_by_step'].items()),
                 f'order KL, adjusted ({t}): {meta["order_kl_adjusted"]:.4f}; train minutes '
                 f'{meta["train_minutes"]:.1f}; total minutes {meta["total_minutes"]:.1f}', '']
    pre = json.load(open(f'{d19.OUT_GEN}/pretrain_meta.json'))
    text += [f'order KL, V4: {pre["order_kl_v4"]:.4f} (se {pre["order_kl_v4_se"]:.4f})', '']

    # ---- the 136 targets
    fits, summ = {}, []
    rd = pd.read_csv(f'{V4B}/redraw_v4b.csv').rename(columns={'redrawn': 'estimate', 'gap_pct': 'miss_pct'})
    summ.append({'file': 'v4b reweighted, redraw of 200,000', **fit_summary(rd)})
    eq = {'V4 (fixed order)': fl['V4 (fixed order)'][0], **{f'direct {t}': fl[f'direct {t}'][0] for t in tags}}
    for t in tags:
        rnd = pd.read_parquet(f'{d19.OUT_GEN}/{t}/final_random.parquet')
        eq[f'direct {t}, random block-1 order'] = tg.dollars(d19.table_to_values(rnd))[0]
    pr = pd.read_parquet(f'{d19.OUT_GEN}/pretrain_random.parquet')
    eq['V4 (random block-1 order)'] = tg.dollars(d19.table_to_values(pr))[0]
    for name, g19 in eq.items():
        for n in [200_000, None]:
            if n is None and len(g19) <= 200_000:
                continue
            f = fit136(tg, g19, n)
            label = f'{name}, {len(g19) if n is None else n:,} records'
            fits[label] = f
            summ.append({'file': label, **fit_summary(f)})
    summ = pd.DataFrame(summ)
    summ.to_csv(f'{OUT}/fit136_summary.csv', index=False)
    allfit = pd.concat([f.assign(file=k) for k, f in fits.items()])
    allfit.to_csv(f'{OUT}/fit136.csv', index=False)
    text += ['## The 136 IRS targets, equal-weight files', '', md(summ), '']

    # ---- the eight wealth targets
    w8 = {}
    for name, (g19, g15, w) in fl.items():
        w8[name] = wealth8(tg, g15['net_worth'].to_numpy(float), w)
    for name, g19 in eq.items():
        if name not in fl:
            w8[name] = wealth8(tg, g19['net_worth'].to_numpy(float), np.full(len(g19), tg.R / len(g19)))
    w8 = pd.DataFrame(w8)
    w8.insert(0, 'target', np.round(np.r_[tg.T[tg.n_exact:]], 0))
    w8 = w8.reset_index().rename(columns={'index': 'wealth target (miss, %)'})
    w8.to_csv(f'{OUT}/wealth8.csv', index=False)
    text += ['## The eight wealth targets (miss, %)', '', md(w8), '']

    # ---- wealth against the SCF
    u19, x19, w19, _ = wt.scf2019('all')
    u19 = u19.assign(net_worth=x19, wages=u19['wages'] * base.WAGE_2018_TO_2015,
                     dividends=u19['dividends'] * base.WAGE_2018_TO_2015)
    cols = {'SCF 2019, all units': (u19, x19, w19)}
    for name, (g19, g15, w) in fl.items():
        cols[name] = (g15, g15['net_worth'].to_numpy(float), w)
    unc = pd.DataFrame({c: wt.unconditional(x, w) for c, (df, x, w) in cols.items()})
    unc.to_csv(f'{OUT}/wealth_unconditional.csv')
    text += ['## Net worth, all units (2015 dollars)', '', md(unc.reset_index().rename(columns={'index': ''})), '']
    cond = {c: wt.conditional(df, x, w) for c, (df, x, w) in cols.items()}
    for stat in ['mean ($)', 'median ($)', 'share above $1m (%)', 'share of units (%)']:
        t = pd.DataFrame({c: cond[c][cond[c]['grouping'] == 'wages'].set_index('group')[stat]
                          for c in cols})
        t.to_csv(f'{OUT}/wealth_by_wage_{stat.split()[0]}.csv')
        text += [f'## Net worth by wage group: {stat}', '', md(t.reset_index()), '']
    for grouping in ['filing status', 'dividends']:
        t = pd.DataFrame({c: cond[c][cond[c]['grouping'] == grouping].set_index('group')['mean ($)']
                          for c in cols})
        text += [f'## Mean net worth by {grouping}', '', md(t.reset_index()), '']

    # ---- holder shares
    hold = base.holders(fl['v4b reweighted'][0], {'v4b stack, before': fl['v4b stack, before'][2],
                                                   'v4b reweighted': fl['v4b reweighted'][2]})
    for name in ['V4 (fixed order)'] + [f'direct {t}' for t in tags]:
        g19, _, w = fl[name]
        h = base.holders(g19, {name: w})
        hold[name] = h[name].to_numpy()
    hold.to_csv(f'{OUT}/holders.csv', index=False)
    text += ['## Holder shares (% of returns)', '', md(hold), '']

    # ---- untargeted relationships
    real = tiltmod.puf_2019_from_real()
    rel = tiltmod.relationships(real, real['weight'].to_numpy(float), tg.brackets).rename(
        columns={'share': 'actual 2015 PUF'})
    for name, (g19, g15, w) in fl.items():
        r = tiltmod.relationships(g19, w, tg.brackets)
        assert (r['group'].to_numpy() == rel['group'].to_numpy()).all()
        rel[name] = r['share'].to_numpy()
    rel = rel[rel['relationship'] != 'any dividends, by AGI bracket']
    rel.to_csv(f'{OUT}/untargeted.csv', index=False)
    num = rel.columns[2:]
    rel_pct = rel.copy()
    rel_pct[num] = 100 * rel_pct[num]
    text += ['## Untargeted relationships (% of returns)', '', md(rel_pct, 3), '']
    for name in [f'direct {t}' for t in tags]:
        dv, dr = rel_pct[name] - rel_pct['V4 (fixed order)'], rel_pct['v4b reweighted'] - rel_pct['v4b stack, before']
        text += [f'{name}: largest change from V4 (fixed order) {dv.abs().max():.2f} points '
                 f'({rel_pct.loc[dv.abs().idxmax(), "relationship"]}, {rel_pct.loc[dv.abs().idxmax(), "group"]}); '
                 f'mean absolute change {dv.abs().mean():.2f}. Reweighting: largest {dr.abs().max():.2f}, '
                 f'mean absolute {dr.abs().mean():.2f}.', '']

    # ---- where the change goes: network against tilt on the first-round sample
    for t in tags:
        w0w = np.load(f'{d19.OUT_TILT}/{t}/round1_tilt_weights.npy')
        r1 = d19.torch.load(f'{d19.OUT_TILT}/{t}/round1_sample.pt')
        lq = np.load(f'{d19.OUT_TILT}/{t}/round1_logq_steps.npy')
        lp = r1['log_p'].numpy()
        w0, ws = w0w[:, 0], w0w[:, 1]
        p0 = w0 / w0.sum()
        tilt_log = np.log(np.maximum(ws, 1e-300) / w0)
        net_log = lq.sum(1) - lp
        qn = p0 * np.exp(net_log)
        qn /= qn.sum()
        pt = ws / ws.sum()
        clip = (tilt_log > -20)
        def wcorr(a, b, w):
            a, b = a - w @ a, b - w @ b
            return float((w @ (a * b)) / np.sqrt((w @ a ** 2) * (w @ b ** 2)))
        r_all = wcorr(net_log[clip], tilt_log[clip], p0[clip] / p0[clip].sum())
        g19, g15 = tg.dollars(r1['values'])
        F = np.load(f'{d19.OUT_TILT}/{t}/round1_F.npy')
        wage = pd.cut(g15['wages'], [-np.inf, 0, 25e3, 50e3, 100e3, 200e3, np.inf],
                      labels=wt.ORDER['wages']).astype(str).to_numpy()
        rows = []
        for grp in wt.ORDER['wages']:
            s = wage == grp
            rows.append({'wage group': grp, 'share, V4': 100 * p0[s].sum(), 'share, exact tilt': 100 * pt[s].sum(),
                         'share, network': 100 * qn[s].sum(),
                         'mean net worth, V4': (p0 * g15['net_worth'])[s].sum() / p0[s].sum(),
                         'mean net worth, exact tilt': (pt * g15['net_worth'])[s].sum() / pt[s].sum(),
                         'mean net worth, network': (qn * g15['net_worth'])[s].sum() / qn[s].sum()})
        mt = (F.T @ qn) * tg.R / tg.T - 1
        text += [f'## Where the change goes ({t}): first-round sample of V4, network against the exact tilt', '',
                 f'Weighted correlation of the per-record log change, network against tilt (records whose tilt '
                 f'factor is above e^-20; {int((~clip).sum())} records excluded): {r_all:.3f}. '
                 f'KL of the network on this sample (importance weights): '
                 f'{float(qn @ np.log(np.maximum(qn, 1e-300) / p0)):.4f}; of the tilt: '
                 f'{float(pt[pt > 0] @ np.log(pt[pt > 0] / p0[pt > 0])):.4f}. '
                 f'Network on this sample: largest exact-target miss {100 * np.abs(mt[:tg.n_exact]).max():.2f}%.',
                 '', md(pd.DataFrame(rows)), '']
    open(f'{OUT}/scores.md', 'w').write('\n'.join(text))
    print('\n'.join(text))


if __name__ == '__main__':
    main(sys.argv[1:])
