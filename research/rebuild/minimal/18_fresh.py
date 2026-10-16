"""
18_fresh.py  (minimal example, experiment 18: fresh tilted records for variant C)

Why. Variants A, A20 and B fine-tune on the 380,000 records of the v4b stack,
seen again in every pass. The tilt is a formula, w = exp(lambda . f(record)),
so it can be applied to any number of new records drawn from V4. Variant C
fine-tunes on 3.2 million such records, each seen once, to tell apart two
reasons the fine-tune could fall short of the tilt: too few distinct records,
or the network and objective themselves.

Steps (through Slurm: 18_distill.sbatch for gen, 18_score.sbatch style for weights):
  python 18_fresh.py gen      GPU: 8 files of 400,000 PUF records with net
                              worth from V4 by the v4b sampler (no
                              oversampling), seeds 1801-1808
  python 18_fresh.py weights  CPU: recover lambda from the v4b tilt (least
                              squares of log(weight_after / weight_before)
                              on the 144 target columns), apply it to every
                              fresh record, write w_tilt per file.
                              RESULT (2026-10-15): this fails. The factor is
                              exp(lambda . f), linear in dollars inside the
                              exponent, and on fresh records a few extreme
                              amounts get enormous factors (effective records
                              0.0003% to 46% of each file; lambda_check.csv).
                              So the tilt is not usable as a formula outside
                              the sample it was solved on.
  python 18_fresh.py retilt   CPU: instead, tilt each fresh file on its own to
                              the same targets as v4b (136 exact, 8 wealth soft
                              at 2%, equal starting weights); overwrites
                              fresh_<k>_w.parquet with those weights and
                              writes retilt_summary.csv

Writes gen/D18/fresh/fresh_<k>.parquet, fresh_<k>_w.parquet, lambda_check.csv.
"""
import importlib.util
import os
import sys

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
os.environ.setdefault('MINIMAL_TABLE', 'E4')
os.environ.setdefault('MINIMAL_RUN', 'V4')
os.environ.setdefault('MINIMAL_ALL_RECORDS', '1')
os.environ.setdefault('MINIMAL_MAP_DIR', f'{R}/map/V4')

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = f'{R}/gen/D18/fresh'
FILES, PER_FILE = 8, 400_000


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def gen():
    import torch
    g = load('g18', '03_generate.py')
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    model = g.m.load_model(device, f'{R}/model/V4')
    os.makedirs(OUT, exist_ok=True)
    for k in range(FILES):
        t = g.generate(model, PER_FILE, 'puf', with_net_worth=True, fixed_order=False,
                       seed=1801 + k, borrowed={'label': 'owner', 'foreign': 'absent'})
        t.to_parquet(f'{OUT}/fresh_{k}.parquet', index=False)
        print('file', k, flush=True)


def columns(sc, path):
    """The 136 + 8 target columns of a generated file, as the v4b tilt."""
    g19, g15 = sc.dollars_of(path)
    F = sc.tiltmod.contributions(g19, sc.BRACKETS)
    Fq, _ = sc.base.threshold_form(g15['net_worth'].to_numpy(float), sc.RETURNS, sc.TARGETS,
                                   sc.THRESHOLD, 'all')
    return np.column_stack([F, Fq])


def weights():
    sc = load('sc18', '18_score.py')
    X = columns(sc, f'{R}/gen/gen_puf_stack_v4b.parquet')
    w = pd.read_parquet(f'{R}/tilt/v4b_allunits/weights_v4b.parquet')
    y = np.log(w['weight_after'].to_numpy(float) / w['weight_before'].to_numpy(float))
    scale = X.std(0)
    scale[scale == 0] = 1
    A = np.column_stack([X / scale, np.ones(len(X))])
    coef, *_ = np.linalg.lstsq(A, y, rcond=None)
    resid = y - A @ coef
    rows = [{'check': 'largest |residual| of log tilt factor on the v4b stack', 'value': np.abs(resid).max()}]
    T = np.r_[sc.T136, sc.base.threshold_form(np.zeros(1), sc.RETURNS, sc.TARGETS, sc.THRESHOLD, 'all')[1]]
    pooled_num, pooled_den = 0, 0
    for k in range(FILES):
        path = f'{OUT}/fresh_{k}.parquet'
        Xk = columns(sc, path)
        lw = np.column_stack([Xk / scale, np.ones(len(Xk))]) @ coef
        wk = np.exp(lw - lw.max())
        pd.DataFrame({'w_tilt': wk, 'log_factor': lw}).to_parquet(f'{OUT}/fresh_{k}_w.parquet', index=False)
        pooled_num = pooled_num + Xk.T @ wk
        pooled_den = pooled_den + wk.sum()
        rows.append({'check': f'file {k}: effective records / records', 'value': wk.sum() ** 2 / (wk ** 2).sum() / len(wk)})
        print(k, flush=True)
    est = pooled_num / pooled_den * sc.RETURNS
    miss = 100 * (est / T - 1)
    rows += [{'check': 'pooled fresh files: largest |% miss| of the 136 IRS targets', 'value': np.abs(miss[:136]).max()},
             {'check': 'pooled fresh files: median |% miss| of the 136 IRS targets', 'value': np.median(np.abs(miss[:136]))},
             {'check': 'pooled fresh files: largest |% miss| of the 8 wealth targets', 'value': np.abs(miss[136:]).max()}]
    pd.DataFrame(rows).to_csv(f'{OUT}/lambda_check.csv', index=False)
    print(pd.DataFrame(rows).to_string())


def retilt():
    sc = load('sc18', '18_score.py')
    tiltmod, wt = sc.tiltmod, sc.wt
    T = np.r_[sc.T136, sc.base.threshold_form(np.zeros(1), sc.RETURNS, sc.TARGETS, sc.THRESHOLD, 'all')[1]]
    tol = np.r_[np.zeros(len(sc.T136)), np.full(len(T) - len(sc.T136), wt.SIGMA)]
    rows = []
    for k in range(FILES):
        X = columns(sc, f'{OUT}/fresh_{k}.parquet')
        w0 = np.full(len(X), sc.RETURNS / len(X))
        w, ok, it = tiltmod.tilt(w0, X, T, rel_tol=tol)
        p0, p1 = w0 / w0.sum(), w / w.sum()
        pos = p1 > 0
        rows.append({'file': k, 'converged': ok, 'newton_steps': it,
                     'move (nats)': float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos]))),
                     'effective records / records': w.sum() ** 2 / (w ** 2).sum() / len(w),
                     'largest |% miss| of the 136': float(np.abs(100 * (X[:, :136].T @ w / sc.T136 - 1)).max())})
        pd.DataFrame({'w_tilt': w}).to_parquet(f'{OUT}/fresh_{k}_w.parquet', index=False)
        print(rows[-1], flush=True)
    pd.DataFrame(rows).to_csv(f'{OUT}/retilt_summary.csv', index=False)


if __name__ == '__main__':
    {'gen': gen, 'weights': weights, 'retilt': retilt}[sys.argv[1]]()
