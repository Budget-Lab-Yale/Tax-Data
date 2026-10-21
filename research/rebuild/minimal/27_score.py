"""
27_score.py  (minimal example, step 27: scores of the reporting rule after
training; 27_MAPPING_SPEC.md sections 3, 5 and 9. Results: 27_RESULTS.md.)

For one model trained by 27_train.py (MINIMAL_RUN), on the held-back 20% of
the 2016 wave's households (tax year 2015), for each mapped item:

  - the chance each family gives to exactly what each household reported,
    given only married, dependents and wages and the PUF-labelled belief (the
    belief the settings were fit against):
      family A (who reports, and how many dollars), with the learned settings,
        and with the dollar factor held at 0.8, 0.9, 1.0 and 1.1 (S-curve kept);
      family B (rank-preserving, zeros included): the survey household at the
        p-th percentile of the wave is the PUF-side household at the p-th
        percentile. A PUF slice b covers an interval of percentiles; a
        reported slice r covers an interval of the wave's percentiles (the
        wave's fit households, weighted). B gives r the share of b's interval
        that lies in r's.
    The sum of the log chances over households (each household counts once:
    the five implicates are averaged), and the probability that each family is
    the right one, starting from one half each.
  - each wave's holders after filling in (the five draws of fills.parquet)
    against the units reporting, the PUF side, and the IRS count for that tax
    year.

Writes model/<run>/score.json.
Usage (Slurm, inside 27_run.sbatch): MINIMAL_TABLE=E27 MINIMAL_RUN=step27/R2s1 python 27_score.py
"""
import importlib.util
import json
import math
import os

import numpy as np
import pandas as pd
import torch

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


def main():
    T = load('train27', '27_train.py')
    M = T.load_model_module()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    meta = json.load(open(os.path.join(M.MODEL_DIR, 'train_meta.json')))
    LATENT = meta['latent']
    model = M.load_model(device)
    table = M.read_table()
    edges = json.load(open(M.BAND_EDGES))
    held = set(meta['held_households'])
    fy = meta['fit_year']
    wave = table[(table.source == 'scf') & (table.year == fy)].copy()
    wave['role'] = np.where(wave.household_id.isin(held), 'held', 'fit')
    C, P = len(M.CONTEXT), M.PREDICTED
    KEEP = torch.tensor([v in ('married', 'n_dep', 'wages') for v in P], device=device)
    SCF_OWNED = [P.index(v) for v, o in M.OWNED_BY.items() if o == 'scf']
    PUF_OWNED = [P.index(v) for v, o in M.OWNED_BY.items() if o == 'puf']
    puf = table[table.source == 'puf']
    out = {'run': M.RUN, 'held-back households': int(wave.loc[wave.role == 'held', 'household_id'].nunique()),
           'held-back unit rows': int((wave.role == 'held').sum()), 'items': {}}

    def anchored(vals):
        view = vals.clone()
        view[:, 0] = M.SOURCES.index('puf')
        view[:, 1] = M.YEARS.index(2015)
        for j in SCF_OWNED:
            view[:, C + j] = -1
        for j in PUF_OWNED:
            view[:, C + j] = 0
        hid = ~KEEP[None, :].expand(len(vals), -1) & (view[:, C:] >= 0)
        with torch.no_grad():
            return model(view, hid)

    hw = wave[wave.role == 'held'].reset_index(drop=True)
    enc = M.encode(hw, device)
    with torch.no_grad():
        lp_all = torch.cat([anchored(enc['values'][i:i + 20_000]) for i in range(0, len(hw), 20_000)])
    for k, v in enumerate(LATENT):
        s = meta['settings'][v]
        alpha, beta, f_learned = s['alpha'], s['beta'], s['final dollar factor']
        NB, POS0 = M.N_CHOICES[v], M.BAND_LAYOUT[v]['first_positive']
        x, b = puf[v].to_numpy(float), puf[f'{v}_band'].to_numpy()
        w = puf['weight'].to_numpy(float)
        lnx, pts = np.full(NB, math.log(150.0)), np.full((NB, 32), np.nan)
        for kk in range(1, NB):
            m_ = b == kk
            if m_.any():
                lnx[kk] = np.average(np.log(np.abs(x[m_])), weights=w[m_])
                o = np.argsort(x[m_], kind='stable')
                xs, F = x[m_][o], np.cumsum(w[m_][o]) / w[m_].sum()
                pts[kk] = xs[np.minimum(np.searchsorted(F, (np.arange(32) + 0.5) / 32), len(xs) - 1)]
        g = 1 / (1 + np.exp(-(alpha + beta * (lnx - math.log(150.0)))))
        g[0] = 0.0
        q = lp_all[:, P.index(v), :NB].exp().double().cpu().numpy()
        reported = hw[v].to_numpy(float)
        r_obs = T.band_signed(np.where(reported < -3000, -3000, reported) if v == 'capital_gains' else reported,
                              edges[f'scf/{fy}/{v}'], NB, POS0)

        def transfer(f):
            Tm = np.zeros((NB, NB))
            for bb in range(1, NB):
                if np.isnan(pts[bb]).any():
                    Tm[bb, bb] = 1.0
                    continue
                y = pts[bb] * f
                if v == 'capital_gains':
                    y = np.where(y < -3000, -3000, y)
                r = T.band_signed(y, edges[f'puf/2015/{v}'], NB, POS0)
                np.add.at(Tm[bb], r, 1.0 / len(r))
            return Tm

        def score_A(f):
            p_zero = (q * (1 - g)).sum(1)
            p_amt = ((q * g) @ transfer(f))[np.arange(len(q)), r_obs]
            p = np.where(r_obs == 0, p_zero, p_amt)
            return np.log(np.clip(p, 1e-300, None))

        # family B: percentile intervals in dollar order (negatives, zero, positives)
        order = list(range(1, POS0)) + [0] + list(range(POS0, NB))
        pf = np.bincount(b, weights=w, minlength=NB)
        pf = pf / pf.sum()
        fw = wave[wave.role == 'fit']
        fr = fw[v].to_numpy(float)
        fb = T.band_signed(np.where(fr < -3000, -3000, fr) if v == 'capital_gains' else fr,
                           edges[f'scf/{fy}/{v}'], NB, POS0)
        ps = np.bincount(fb, weights=fw['weight'].to_numpy(float), minlength=NB)
        ps = ps / ps.sum()
        cf = np.cumsum(pf[order]); lo_f = np.r_[0, cf[:-1]]
        cs = np.cumsum(ps[order]); lo_s = np.r_[0, cs[:-1]]
        TB = np.zeros((NB, NB))
        for i, bb in enumerate(order):
            width = cf[i] - lo_f[i]
            if width <= 0:
                continue
            for jdx, rr in enumerate(order):
                ov = min(cf[i], cs[jdx]) - max(lo_f[i], lo_s[jdx])
                if ov > 0:
                    TB[bb, rr] = ov / width
        logB = np.log(np.clip((q @ TB)[np.arange(len(q)), r_obs], 1e-300, None))
        logA = score_A(f_learned)
        pinned = {f'{f:.1f}': float(score_A(f).sum() / 5) for f in (0.8, 0.9, 1.0, 1.1)}
        LA, LB = float(logA.sum() / 5), float(logB.sum() / 5)
        pA = 1 / (1 + math.exp(min(700, LB - LA)))
        out['items'][v] = {
            'family A: alpha, beta, dollar factor': [alpha, beta, f_learned],
            'family A: chance of reporting at $50, $500, $5,000 (%)': s['chance of reporting at $50, $500, $5,000 (%)'],
            'log score, family A, learned settings (nats, households)': LA,
            'log score, family B (nats, households)': LB,
            'probability family A is right': pA, 'probability family B is right': 1 - pA,
            'log score, family A with the dollar factor held at': pinned,
            'mean log score per household, A and B': [LA / out['held-back households'], LB / out['held-back households']],
        }

    # holders after filling in, by wave
    fills = pd.read_parquet(os.path.join(M.MODEL_DIR, 'fills.parquet'))
    irs = pd.read_csv(f'{R}/step27/irs_counts.csv')
    irs = irs[irs.statistic == 'count'].set_index('year')
    rows = []
    for y, g_ in fills.groupby('year'):
        w = g_['weight'].to_numpy(float)
        for v in LATENT:
            filled = np.mean([w[g_[f'{v}_fill{d}'].to_numpy() != 0].sum() for d in range(1, 6)]) / 1e6
            rows.append({'tax year': int(y), 'item': v, 'SCF units (millions)': w.sum() / 1e6,
                         'SCF units reporting it (millions)': w[g_[f'{v}_reported'].to_numpy() != 0].sum() / 1e6,
                         'SCF units holding it after filling in (millions, mean of 5 draws)': filled,
                         'IRS returns with it (millions)': float(irs.loc[int(y), v]) / 1e6,
                         'IRS returns (millions)': float(irs.loc[int(y), 'returns']) / 1e6})
    pw = puf['weight'].to_numpy(float)
    for v in LATENT:
        rows.append({'tax year': 2015, 'item': f'{v}, PUF side (filers and crude non-filers)',
                     'SCF units (millions)': pw.sum() / 1e6,
                     'SCF units reporting it (millions)': pw[puf[v].to_numpy() != 0].sum() / 1e6})
    out['holders by wave'] = rows
    out['fill draws: rows'] = int(len(fills))
    json.dump(out, open(os.path.join(M.MODEL_DIR, 'score.json'), 'w'), indent=1)
    print(json.dumps(out, indent=1))


if __name__ == '__main__':
    main()
