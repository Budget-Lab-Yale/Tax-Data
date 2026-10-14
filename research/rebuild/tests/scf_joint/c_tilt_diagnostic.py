"""Oracle-target KL calibration of completion draws, preserving each row's mass.

Only four net-worth means (one per income group) are supplied from the
held-out truth. They are calibration inputs, not predictive successes.
No true individual wealth is used to select completions. All remaining
metrics are untargeted diagnostics. This is a support/adjustment test,
not an estimate of generalization to unknown population targets.
"""
import argparse
import json
from pathlib import Path

import numpy as np
from scipy.optimize import brentq
from scipy.special import softmax, xlogy

from c_common import (Dataset, truth_frame, completed_frame, frame_stats,
                      compare_stats, jsonable, INC_LABELS)


def check_calibration():
    import pandas as pd
    rows = [dict(row=g, draw=d, ndraw=3, w=(g+1)/3, nw=float(d), inc_group=g)
            for g in range(4) for d in range(3)]
    C = pd.DataFrame(rows)
    T = pd.DataFrame(dict(inc_group=range(4), w=[1., 2., 3., 4.], nw=[1.]*4))
    same, diag = calibrate(C, T)
    assert np.allclose(same['w'], C['w'])
    assert abs(diag['population_weighted_kl']) < 1e-10
    T['nw'] = [0.5, 0.8, 1.2, 1.5]
    tilted, diag = calibrate(C, T)
    assert diag['population_weighted_kl'] > 0
    assert all(g['feasible'] and abs(g['after'] - g['target']) < 1e-8 for g in diag['groups'])
    T['nw'] = [3.]*4
    _, diag = calibrate(C, T)
    assert all(not g['feasible'] for g in diag['groups'])
    print('PASS: unchanged weights at existing targets; feasible tilts; infeasible targets detected')


def calibrate(C, T):
    C = C.sort_values(['row', 'draw']).reset_index(drop=True).copy()
    k = int(C['ndraw'].iloc[0])
    assert C.groupby('row').size().eq(k).all()
    x = C['nw'].to_numpy().reshape(-1, k)
    row = C['row'].to_numpy().reshape(-1, k)
    assert (row == row[:, :1]).all()
    w = C['w'].to_numpy().reshape(-1, k).sum(axis=1)
    g = C['inc_group'].to_numpy().reshape(-1, k)[:, 0]
    p = np.full_like(x, 1 / k)
    groups = []
    for group, name in enumerate(INC_LABELS):
        sel = g == group
        tt = T[T['inc_group'] == group]
        target = np.average(tt['nw'], weights=tt['w'])
        xx, ww = x[sel], w[sel]
        ww = ww / ww.sum()
        lo = np.average(xx.min(axis=1), weights=ww)
        hi = np.average(xx.max(axis=1), weights=ww)
        before = np.sum(ww[:, None] * xx) / k
        feasible = bool(lo < target < hi)
        lam = None
        if feasible:
            scale = max(np.sum(ww[:, None] * np.abs(xx)) / k, 1.)
            z = xx / scale
            def residual(lam):
                return np.sum(ww[:, None] * softmax(lam * z, axis=1) * z) - target / scale
            lower, upper = -1., 1.
            while residual(lower) > 0:
                lower *= 2
                if abs(lower) > 1e12:
                    raise RuntimeError('could not bracket tilt')
            while residual(upper) < 0:
                upper *= 2
                if upper > 1e12:
                    raise RuntimeError('could not bracket tilt')
            lam = brentq(residual, lower, upper, xtol=1e-12)
            p[sel] = softmax(lam * z, axis=1)
        after = np.sum(ww[:, None] * p[sel] * xx)
        if feasible:
            assert abs(after - target) < 1e-5 * max(abs(target), 1.)
        groups.append(dict(group=name, target=target, before=before, after=after,
                           lower=lo, upper=hi, feasible=feasible, scaled_lambda=lam))
    C['w'] = (w[:, None] * p).ravel()
    assert np.allclose(C.groupby('row', sort=True)['w'].sum().to_numpy(), w)
    diag = dict(groups=groups, draws=k,
                population_weighted_mean_effective_draws=float(np.average(1/(p*p).sum(axis=1), weights=w)),
                population_weighted_kl=float(np.average(xlogy(p, k*p).sum(axis=1), weights=w)),
                max_draw_probability=float(p.max()))
    return C, diag


def main():
    check_calibration()
    ap = argparse.ArgumentParser()
    ap.add_argument('--dataset', required=True)
    ap.add_argument('--completed', required=True)
    ap.add_argument('--out-root', required=True)
    a = ap.parse_args()
    ds = Dataset(a.dataset)
    T = truth_frame(ds, (ds.records['source'] != 'PUF').to_numpy())
    C, covered = completed_frame(ds, T, a.completed)
    assert covered.all(), 'diagnostic expects the full held-out SCF sample'
    age = sorted(T['t_age_range'].unique())
    ST = frame_stats(T, ds.tinfo, age)
    before, _ = compare_stats(ST, frame_stats(C, ds.tinfo, age))
    tilted, diag = calibrate(C, T)
    after, detail = compare_stats(ST, frame_stats(tilted, ds.tinfo, age))
    label = Path(a.completed).stem.removeprefix('completed_')
    out = Path(a.out_root) / a.dataset / ('tilt_' + label)
    out.mkdir(parents=True, exist_ok=True)
    result = dict(dataset=a.dataset, label=label, file=a.completed,
                  kind='oracle_target_calibration', before=before, after=after, diagnostics=diag,
                  n_records=len(T),
                  warning='Four true income-group net-worth means supplied as calibration targets; not predictions.')
    (out / 'summary.json').write_text(json.dumps(jsonable(result), indent=2))
    for name, frame in detail.items():
        frame.to_csv(out / f'detail_{name}.csv', index=False)
    print(label, json.dumps(jsonable(diag)))
    print('holding error:', before['hold_inc_mae_pp'], '->', after['hold_inc_mae_pp'],
          'income correlation error:', before['income_corr_mae'], '->', after['income_corr_mae'],
          'top wealth share:', before['nw_top1_cand'], '->', after['nw_top1_cand'])


if __name__ == '__main__':
    main()
