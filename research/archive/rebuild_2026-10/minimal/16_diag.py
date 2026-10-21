"""
16_diag.py  (minimal example, step 16: why the model-side fixes E7-E9 failed)

Diagnostics on models that are already trained. Nothing is retrained and no
existing file is changed. One run per call; the run's flags must be in the
environment exactly as it was trained (16_diag.sbatch sets them from the
run's train_meta.json), because 02_model.py reads them when it is loaded.

  DIAG_RUN=<E0|E4|E7|E8|E9>  python research/rebuild/minimal/16_diag.py

What it computes (written to R/diag16/<run>/diag.json and printed):

  A. Zero shares. The model's probability that interest, dividends or
     capital gains is zero, averaged over records, against the actual
     share: on PUF test records under the PUF label, and on SCF test
     records under the SCF label; (i) with only that item hidden, (ii) with
     only wages, married and n_dep visible. For a run with the reporting
     curve (E9), also the SCF zero share the curve implies:
     p(0) + sum_b p(b)(1 - g(b)).
  B. Net worth given X, split into its two sources of error. Net worth is
     drawn (5 draws per record) under the SCF label, married and n_dep
     masked (as in block 2 of 03_generate.py), for:
       real PUF test records, other_agi visible / other_agi absent
       the run's generated PUF-style records (gen_owner file), other_agi
         visible (reproduces the scoreboard) / absent; and under the PUF
         label with other_agi visible (the scoreboard's PUF-label column)
       real SCF test records (a check: drawn against actual net worth)
     Each: mean and median, overall and by wage group.
  C. The joint distribution of the generated PUF-style X against the real
     PUF (all PUF rows, weighted): zero-wage share, holder shares, married
     share and other_agi by wage group.
  D. (E9 only, from data) How much the SCF's zero band mixes unreported
     holders into the true zeros: mean net worth of SCF units reporting
     zero, and of true non-holders implied by the run's reporting curve.

Run through Slurm: research/rebuild/minimal/16_diag.sbatch.
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


m = load('m02', '02_model.py')
sc = load('s09', '09_score.py')
wt, dollars = sc.wt, sc.dollars
ROOT = m.ROOT
RUN = os.environ['DIAG_RUN']
OUT = f'{ROOT}/diag16/{RUN}'
NC = len(m.CONTEXT)
J = {v: m.PREDICTED.index(v) for v in m.PREDICTED}
INV = ['interest', 'dividends', 'capital_gains']
K = 5
DEVICE = 'cuda' if torch.cuda.is_available() else 'cpu'
WAGE_GROUPS = ['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+']


def values_of(df):
    cols = [df['source'].map(m.SOURCES.index), df['year'].map(m.YEARS.index),
            df['married'], df['n_dep']] + [df[a + '_band'] for a in m.AMOUNTS]
    return torch.tensor(np.column_stack([np.asarray(c, dtype=np.int64) for c in cols]),
                        device=DEVICE)


@torch.no_grad()
def probs(model, vals, hidden, var, chunk=50_000):
    out = []
    for i in range(0, len(vals), chunk):
        lp = m.predict(model, vals[i:i + chunk], hidden[i:i + chunk])
        out.append(lp[:, J[var], :m.N_CHOICES[var]].exp())
    return torch.cat(out)


def wage_group(wages):
    return pd.cut(pd.Series(wages), [-np.inf, 0, 25e3, 50e3, 100e3, 200e3, np.inf],
                  labels=WAGE_GROUPS).astype(str).to_numpy()


def wquant(x, w, p):
    return float(dollars.wquantile(np.asarray(x, float), np.asarray(w, float), [p])[0])


def summary(nw, wages, w):
    nw, w = np.asarray(nw, float), np.asarray(w, float)
    g = wage_group(wages)
    out = {'mean': float((nw * w).sum() / w.sum()), 'median': wquant(nw, w, 0.5),
           'p90': wquant(nw, w, 0.9), 'by_wage': {}}
    for lab in WAGE_GROUPS:
        k = g == lab
        if k.any():
            out['by_wage'][lab] = {'share': float(w[k].sum() / w.sum()),
                                   'mean': float((nw[k] * w[k]).sum() / w[k].sum()),
                                   'median': wquant(nw[k], w[k], 0.5)}
    return out


# ---------------------------------------------------------------- A

def zero_shares(model, df, label):
    """Mean model p(item = 0) against the actual zero share, weighted."""
    vals = values_of(df)
    vals[:, 0] = m.SOURCES.index(label)
    present = vals[:, NC:] >= 0
    w = torch.tensor(df['weight'].to_numpy(float), device=DEVICE)
    curve = None
    if m.REPORT_CURVE:
        curve = json.load(open(os.path.join(m.MODEL_DIR, 'report_curve.json')))
    out = {}
    for v in INV:
        res = {'actual': float((w * (vals[:, NC + J[v]] == 0)).sum() / w.sum())}
        one = torch.zeros_like(present)
        one[:, J[v]] = True
        few = present.clone()
        for keep in ['wages', 'married', 'n_dep']:
            few[:, J[keep]] = False
        for name, hidden in [('one hidden', one), ('wages+demographics visible', few)]:
            p = probs(model, vals, hidden, v)
            res[name] = float((w * p[:, 0]).sum() / w.sum())
            if curve is not None:
                g = torch.tensor(curve[v]['g'], device=DEVICE, dtype=p.dtype)
                res[name + ', implied SCF zero'] = float((w * (p * (1 - g)).sum(1)).sum() / w.sum())
        out[v] = res
    return out


# ---------------------------------------------------------------- B

def draw_nw(model, df, label, other_agi, seed):
    """K net worth bands per record, block-2 view: married, n_dep masked."""
    vals = values_of(df)
    vals[:, 0] = m.SOURCES.index(label)
    vals[:, NC + J['net_worth']] = 0
    if other_agi == 'absent':
        vals[:, NC + J['other_agi']] = -1
    hidden = torch.zeros(len(df), len(m.PREDICTED), dtype=torch.bool, device=DEVICE)
    hidden[:, J['net_worth']] = True
    hidden[:, J['married']] = True
    hidden[:, J['n_dep']] = True
    p = probs(model, vals, hidden, 'net_worth')
    gen = torch.Generator(device=DEVICE).manual_seed(seed)
    return torch.multinomial(p, K, replacement=True, generator=gen).cpu().numpy()


def nw_dollars(bands, nwmap, seed):
    x, _ = dollars.draw_one(bands.reshape(-1), nwmap, np.random.default_rng([seed, 99]))
    return x


def nw_case(model, df, wages, w, label, other_agi, nwmap, seed):
    bands = draw_nw(model, df, label, other_agi, seed)
    x = nw_dollars(bands, nwmap, seed)
    return summary(x, np.repeat(wages, K), np.repeat(w, K) / K)


# ---------------------------------------------------------------- C

def x_joint(df, w):
    w = np.asarray(w, float)
    g = wage_group(df['wages'].to_numpy(float))
    out = {'zero wages': float(w[g == '0'].sum() / w.sum()), 'by_wage': {}}
    for lab in WAGE_GROUPS:
        k = g == lab
        if not k.any():
            continue
        wk = w[k]
        row = {'share': float(wk.sum() / w.sum()),
               'married': float((wk * df['married'].to_numpy()[k]).sum() / wk.sum())}
        for v in INV:
            row[f'{v} holders'] = float((wk * (df[v].to_numpy(float)[k] != 0)).sum() / wk.sum())
        oa = df['other_agi'].to_numpy(float)[k]
        row['other_agi mean'] = float((wk * oa).sum() / wk.sum())
        row['other_agi over 25k'] = float((wk * (oa > 25e3)).sum() / wk.sum())
        out['by_wage'][lab] = row
    return out


# ---------------------------------------------------------------- D

def zero_band_mix(table):
    """Mean net worth of SCF zero-reporters vs implied true non-holders, using
    the run's reporting curve and the actual SCF (all splits, weights)."""
    curve = json.load(open(os.path.join(m.MODEL_DIR, 'report_curve.json')))
    scf = table[table['source'] == 'scf']
    w = scf['weight'].to_numpy(float)
    nw = scf['net_worth'].to_numpy(float)
    out = {}
    for v in INV:
        b = scf[v + '_band'].to_numpy()
        g = np.asarray(curve[v]['g'])
        n = len(g)
        ps = np.bincount(b, weights=w, minlength=n) / w.sum()
        sw = np.bincount(b, weights=w * nw, minlength=n)
        mean_s = np.divide(sw, np.bincount(b, weights=w, minlength=n),
                           out=np.zeros(n), where=ps > 0)
        pos = (np.arange(n) > 0) & (ps > 0)
        p_true = np.zeros(n)
        p_true[pos] = ps[pos] / g[pos]
        p_true0 = 1 - p_true[pos].sum()
        unrep = p_true * (1 - g)
        unrep[~pos] = 0
        mean_true0 = (ps[0] * mean_s[0] - (unrep * mean_s).sum()) / p_true0
        mean_unrep = (unrep * mean_s).sum() / unrep.sum()
        out[v] = {'SCF reported zero share': float(ps[0]),
                  'implied true zero share': float(p_true0),
                  'unreported holders, share of all units': float(unrep.sum()),
                  'mean NW, reported zeros': float(mean_s[0]),
                  'mean NW, unreported holders (implied)': float(mean_unrep),
                  'mean NW, true non-holders (implied)': float(mean_true0),
                  'mean NW, reporting holders': float((ps[pos] * mean_s[pos]).sum() / ps[pos].sum())}
    return out


# ---------------------------------------------------------------- main

def main():
    run = sc.resolve(RUN)
    assert os.path.abspath(run['stacked']) == os.path.abspath(m.DATA), (run['stacked'], m.DATA)
    os.makedirs(OUT, exist_ok=True)
    model = m.load_model(DEVICE)
    table, edges, maps = sc.run_maps(run)
    nwmap = maps['net_worth']
    income_maps = {v: g for v, g in maps.items() if v != 'net_worth'}
    R = {'run': RUN, 'flags': run['flags'], 'model_dir': m.MODEL_DIR, 'draws_per_record': K}

    puf_test = table[(table.source == 'puf') & (table.split == 'test')].reset_index(drop=True)
    scf_test = table[(table.source == 'scf') & (table.split == 'test')].reset_index(drop=True)
    puf_all = table[table.source == 'puf'].reset_index(drop=True)

    # A
    R['A zero shares'] = {'PUF test, PUF label': zero_shares(model, puf_test, 'puf'),
                          'SCF test, SCF label': zero_shares(model, scf_test, 'scf')}
    print(json.dumps(R['A zero shares'], indent=1), flush=True)

    # B
    B = {}
    pw, pwg = puf_test['weight'].to_numpy(float), puf_test['wages'].to_numpy(float)
    for oa in ['visible', 'absent']:
        B[f'real PUF test X, SCF label, other_agi {oa}'] = nw_case(
            model, puf_test, pwg, pw, 'scf', oa, nwmap, 11)
    sw = scf_test['weight'].to_numpy(float)
    B['real SCF test X, SCF label (model)'] = nw_case(
        model, scf_test, scf_test['wages'].to_numpy(float), sw, 'scf', 'absent', nwmap, 12)
    B['real SCF test, actual net worth'] = summary(scf_test['net_worth'].to_numpy(float),
                                                   scf_test['wages'].to_numpy(float), sw)
    scf_all = table[table.source == 'scf']
    B['actual SCF, all rows'] = summary(scf_all['net_worth'].to_numpy(float),
                                        scf_all['wages'].to_numpy(float),
                                        scf_all['weight'].to_numpy(float))
    raw = pd.read_parquet(run['gen_owner'])
    raw = raw[raw['source'] == 'puf'].reset_index(drop=True)
    gx = sc.draw(raw, {v: g for v, g in income_maps.items() if f'{v}_band' in raw})
    gw = np.ones(len(gx))
    gwg = gx['wages'].to_numpy(float)
    for label, oa in [('scf', 'visible'), ('scf', 'absent'), ('puf', 'visible'), ('puf', 'absent')]:
        B[f'generated X, {label.upper()} label, other_agi {oa}'] = nw_case(
            model, raw, gwg, gw, label, oa, nwmap, 13)
    B['generated X, net worth as in the gen file (scoreboard)'] = summary(
        nw_dollars(raw['net_worth_band'].to_numpy(), nwmap, 14), gwg, gw)
    R['B net worth'] = B
    for k, v in B.items():
        print(f"{k:70s} mean {v['mean']:>12,.0f}  median {v['median']:>10,.0f}", flush=True)

    # C
    R['C X joint'] = {'real PUF (all rows, weighted)': x_joint(puf_all, puf_all['weight']),
                      'generated PUF-style X': x_joint(gx, gw)}
    print(json.dumps(R['C X joint'], indent=1), flush=True)

    # D
    if m.REPORT_CURVE:
        R['D zero band'] = zero_band_mix(table)
        print(json.dumps(R['D zero band'], indent=1), flush=True)

    with open(f'{OUT}/diag.json', 'w') as f:
        json.dump(R, f, indent=1)
    print('wrote', f'{OUT}/diag.json')


if __name__ == '__main__':
    main()
