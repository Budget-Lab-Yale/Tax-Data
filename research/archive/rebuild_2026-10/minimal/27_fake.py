"""
27_fake.py  (minimal example, step 27: the fake-survey test of the capital
gains split, decision 18 test 2; 27_MAPPING_SPEC.md section 8, item 2.
Results: 27_RESULTS.md.)

  python 27_fake.py tables   builds table F27 from table E27's PUF filers:
     half the returns (drawn by record, seed 27) stay the PUF; the other half
     become a fake survey (source 'scf', year 2015) that reports
       - married, dependents, wages exactly;
       - interest and dividends through family A with planted settings: a
         holder reports with chance 1 / (1 + exp(-(-0.2 + 1.3 (ln x - ln 150))))
         (16% at $50, 80% at $500, 99% at $5,000), dollars exact (factor 1);
       - ONE capital gains number on a planted mix of the two bases: 40% of
         households (drawn independently of everything) on the before-limit
         basis kg_st + kg_lt + kg_dist, the rest on the after-limit basis (the
         line-13 total, all four pieces); holders hide with the same curve in
         the size of the number;
       - its own slot (the net worth slot) holding the true other income, as
         in step 22.
     The PUF half keeps every slot, pieces included. Truth columns are kept.
  python 27_fake.py check    reads the model trained on F27 by 27_train.py
     (run step27/F27s1, 27_run.sbatch with MINIMAL_TABLE=F27) and writes
     step27/fake_check.md: learned settings against the planted ones, the
     mix estimate against the plant, hidden holders against the truth, and
     the split: the survey households' pieces drawn under the PUF label given
     married, dependents, wages, the filled-in interest, dividends and
     capital gains total, against their true pieces.
"""
import importlib.util
import json
import math
import os
import sys

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
D = f'{R}/data'
PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']
A_PLANT, B_PLANT, MIX_BEFORE = -0.2, 1.3, 0.40


def g_plant(x):
    ax = np.abs(x)
    out = np.zeros(len(x))
    nz = ax > 0
    out[nz] = 1 / (1 + np.exp(-(A_PLANT + B_PLANT * (np.log(ax[nz]) - math.log(150.0)))))
    return out


def tables():
    T = importlib.util.spec_from_file_location('t27', f'{HERE}/27_tables.py')
    t27 = importlib.util.module_from_spec(T)
    T.loader.exec_module(t27)
    e = pd.read_parquet(f'{D}/stacked_E27.parquet')
    edges = json.load(open(f'{D}/band_edges_E27.json'))
    p = e[(e.source == 'puf') & (e.filer == 1)].reset_index(drop=True)
    rng = np.random.default_rng(27)
    srv = rng.random(len(p)) < 0.5
    p['split'] = 'train'
    s = p[srv].copy()
    pu = p[~srv].copy()
    for v in ['interest', 'dividends', 'capital_gains'] + PIECES + ['other_agi']:
        s[f'true_{v}'] = s[v]
    before = rng.random(len(s)) < MIX_BEFORE
    s['true_basis_before'] = before.astype(int)
    cg = np.where(before, s['kg_st'] + s['kg_lt'] + s['kg_dist'], s['capital_gains'])
    s['true_cg_as_asked'] = cg
    for v, x in [('interest', s['interest'].to_numpy(float)), ('dividends', s['dividends'].to_numpy(float)),
                 ('capital_gains', cg)]:
        rep = rng.random(len(s)) < g_plant(x)
        s[v] = np.where(rep, x, 0.0)
        s[f'{v}_reported_flag'] = rep.astype(int)
    s['capital_gains_reported'] = s['capital_gains']
    s['source'] = 'scf'
    s['net_worth'] = s['true_other_agi']
    for v in PIECES + ['other_agi']:
        s[v] = np.nan
    s['filer'] = -1
    s['record_id'] = s['record_id'] + 5e9
    s['household_id'] = s['record_id']
    t = pd.concat([pu, s], ignore_index=True)
    fe = {k: v for k, v in edges.items() if k.startswith('puf/2015/') or k == 'meta'}
    w = t['weight'].to_numpy(float)
    sv = (t.source == 'scf').to_numpy()
    for v in ['wages', 'interest', 'dividends', 'capital_gains']:
        b, rec = t27.band_with(t.loc[sv, v].to_numpy(float), w[sv], t27.fixed_edge(edges[f'puf/2015/{v}'], 1.0))
        t.loc[sv, f'{v}_band'] = b
        fe[f'scf/2015/{v}'] = rec
    b, rec = t27.band_variable(t.loc[sv, 'net_worth'].to_numpy(float), w[sv])
    t['net_worth_band'] = np.int16(-1)
    t.loc[sv, 'net_worth_band'] = b
    fe['scf/2015/net_worth'] = rec
    for v in PIECES + ['other_agi']:
        t.loc[sv, f'{v}_band'] = -1
    for c in [c for c in t.columns if c.endswith('_band')]:
        t[c] = t[c].astype('int16')
    fe['meta'] = {'F27': '27_fake.py: fake survey from E27 PUF filers; planted A (-0.2, 1.3), mix 40% before the limit'}
    t.to_parquet(f'{D}/stacked_F27.parquet', index=False)
    json.dump(fe, open(f'{D}/band_edges_F27.json', 'w'), indent=1)
    note = {'survey rows': int(sv.sum()), 'PUF rows': int((~sv).sum()),
            'planted chance of reporting at $50, $500, $5,000 (%)': [float(100 * g_plant(np.array([x]))[0]) for x in (50, 500, 5000)],
            'planted mix, before-limit share (%)': 100 * MIX_BEFORE,
            'drawn mix, before-limit share (%)': float(100 * before.mean())}
    json.dump(note, open(f'{R}/step27/fake_note.json', 'w'), indent=1)
    print(json.dumps(note, indent=1))


def check():
    import torch
    os.environ.setdefault('MINIMAL_TABLE', 'F27')
    os.environ.setdefault('MINIMAL_RUN', 'step27/F27s1')
    T = importlib.util.spec_from_file_location('train27', f'{HERE}/27_train.py')
    tr = importlib.util.module_from_spec(T)
    T.loader.exec_module(tr)
    M = tr.load_model_module()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    meta = json.load(open(os.path.join(M.MODEL_DIR, 'train_meta.json')))
    note = json.load(open(f'{R}/step27/fake_note.json'))
    fills = pd.read_parquet(os.path.join(M.MODEL_DIR, 'fills.parquet'))
    table = pd.read_parquet(M.DATA)
    s = table[table.source == 'scf'].set_index('record_id')
    lines = ['# Step 27: fake-survey test of the capital gains split', '',
             f'Run {M.RUN}; planted chance of reporting at $50, $500, $5,000: '
             + ', '.join(f'{x:.0f}%' for x in note['planted chance of reporting at $50, $500, $5,000 (%)'])
             + '; dollar factor 1.00; before-limit share 40% (drawn: '
             + f"{note['drawn mix, before-limit share (%)']:.1f}%).", '',
             '## 1. Learned settings', '', '| Item | At $50 | At $500 | At $5,000 | Dollar factor |', '|---|---:|---:|---:|---:|']
    for v in meta['latent']:
        st = meta['settings'][v]
        a = st['chance of reporting at $50, $500, $5,000 (%)']
        lines.append(f'| {v} | {a[0]:.0f}% | {a[1]:.0f}% | {a[2]:.0f}% | {st["final dollar factor"]:.2f} |')
    # 2. the mix, closed form as on the real SCF
    ws, xs = s['weight'].to_numpy(float), s['capital_gains_reported'].to_numpy(float)
    at, beyond = np.isclose(xs, -3000), xs < -3000
    lines += ['', '## 2. The basis mix', '',
              f'Among survey households reporting a loss of $3,000 or more, {100 * ws[beyond].sum() / ws[at | beyond].sum():.1f}% '
              f'report beyond $3,000 (the closed-form estimate of the before-limit share used on the real SCF); '
              f'planted 40%.']
    tb = s['true_basis_before'].to_numpy()
    tl = (s['true_kg_st'] + s['true_kg_lt'] + s['true_kg_dist']).to_numpy(float)
    big = tl < -3000
    lines.append(f'Among households whose before-limit loss exceeds $3,000 and who report anything, the true before-limit '
                 f'share is {100 * ws[big & (xs != 0) & (tb == 1)].sum() / ws[big & (xs != 0)].sum():.1f}%.')
    # 3. hidden holders
    f = fills.set_index('record_id').loc[s.index]
    lines += ['', '## 3. Hidden holders among the survey zeros', '', '| Item | Model (mean of 5 draws) | Truth |', '|---|---:|---:|']
    for v in meta['latent']:
        zero = s[v].to_numpy() == 0
        truth = s[f'true_{v}'].to_numpy() != 0
        model_share = np.mean([(ws * (f[f'{v}_fill{d}'].to_numpy() != 0))[zero].sum() / ws[zero].sum() for d in range(1, 6)])
        lines.append(f'| {v} | {100 * model_share:.1f}% | {100 * ws[zero & truth].sum() / ws[zero].sum():.1f}% |')
    # 4. the split: pieces under the PUF label, given the filled-in record
    model = M.load_model(device)
    C, P = len(M.CONTEXT), M.PREDICTED
    enc = M.encode(s.reset_index(), device)
    vals = enc['values'].clone()
    for v in meta['latent']:                      # the first draw of the fill-ins
        vals[:, C + P.index(v)] = torch.tensor(f[f'{v}_fill1'].to_numpy(), device=device)
    vals[:, 0] = M.SOURCES.index('puf')
    vals[:, C + P.index('net_worth')] = -1
    for v in PIECES + ['other_agi', 'filer']:
        if v in P:
            vals[:, C + P.index(v)] = 0
    gen = torch.Generator(device=device).manual_seed(27)
    filled = torch.zeros(len(vals), len(P), dtype=torch.bool, device=device)
    for v in P:
        if v not in PIECES + ['other_agi', 'filer']:
            filled[:, P.index(v)] = True
    order = [P.index(v) for v in PIECES + ['other_agi', 'filer'] if v in P]
    perm = np.random.default_rng(27).permutation(len(order))
    with torch.no_grad():
        for j in [order[i] for i in perm]:
            out = []
            for i in range(0, len(vals), 20_000):
                hid = (vals[i:i + 20_000, C:] >= 0) & ~filled[i:i + 20_000]
                pr = model(vals[i:i + 20_000], hid)[:, j, :M.N_CHOICES[P[j]]].exp()
                out.append(torch.multinomial(pr, 1, generator=gen).squeeze(1))
            vals[:, C + j] = torch.cat(out)
            filled[:, j] = True
    lines += ['', '## 4. The split into pieces (survey households, pieces drawn under the PUF label)', '',
              '| Piece | Holders, drawn | Holders, truth | Gains, drawn | Gains, truth | Losses, drawn | Losses, truth | Same slice as truth |',
              '|---|---:|---:|---:|---:|---:|---:|---:|']
    for v in PIECES:
        d = vals[:, C + P.index(v)].cpu().numpy()
        tbnd = table.loc[table.source == 'scf', f'{v}_band']   # absent on survey rows; band the truth instead
        tt = s[f'true_{v}'].to_numpy(float)
        pos0 = M.BAND_LAYOUT[v]['first_positive']
        tb_ = tr.band_signed(tt, json.load(open(M.BAND_EDGES))[f'puf/2015/{v}'], M.N_CHOICES[v], pos0)
        W = ws.sum()
        lines.append(f'| {v} | {100 * ws[d != 0].sum() / W:.1f}% | {100 * ws[tt != 0].sum() / W:.1f}% | '
                     f'{100 * ws[d >= pos0].sum() / W:.1f}% | {100 * ws[tt > 0].sum() / W:.1f}% | '
                     f'{100 * ws[(d > 0) & (d < pos0)].sum() / W:.1f}% | {100 * ws[tt < 0].sum() / W:.1f}% | '
                     f'{100 * ws[d == tb_].sum() / W:.1f}% |')
    lines += ['', 'Shares are of all survey households, weighted. "Same slice" is the share whose drawn slice equals '
              'the slice of the true amount (zero counts as a slice).']
    lines += other_income_measure(M, device, table)
    open(f'{R}/step27/fake_check_{os.path.basename(M.RUN)}.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


def other_income_measure(M, device, table):
    """Step 22's measure, label on as in step 26: for every PUF-half return,
    the model's mean of the survey's own slot (true other income) under the
    survey label, given the return's married, dependents, wages, interest,
    dividends and line-13 capital gains (PUF-owned slots absent); against the
    truth, each amount held between the survey's 1st and 99th percentiles.
    For the new method (F27s1) and today's rule (R0F27s1), when trained."""
    import torch
    out = ['', '## 5. Other income of PUF returns, model against the truth (step 22 and 26 measure)', '',
           'Gap of the mean (each amount held between the survey\'s 1st and 99th percentiles) from the truth, %.', '',
           '| Run | All returns | No interest | No interest and no dividends | Wages 0 | Wages 200k+ |',
           '|---|---:|---:|---:|---:|---:|']
    A = table[table.source == 'puf'].reset_index(drop=True)
    srv = table[table.source == 'scf']
    ws = srv['weight'].to_numpy(float)
    lo, hi = wq(srv['net_worth'].to_numpy(float), ws, [0.01, 0.99])
    nb = srv['net_worth_band'].to_numpy()
    clipped = srv['net_worth'].clip(lo, hi).to_numpy()
    NBn = M.N_CHOICES['net_worth']
    m_clip = np.array([np.average(clipped[nb == k], weights=ws[nb == k]) if (nb == k).any() else 0.0 for k in range(NBn)])
    truth = np.clip(A['other_agi'].to_numpy(float), lo, hi)
    w = A['weight'].to_numpy(float)
    C, P = len(M.CONTEXT), M.PREDICTED
    for run in ['F27s1', 'F27Js1', 'R0F27s1']:
        d = f'{R}/model/step27/{run}'
        if not os.path.exists(f'{d}/model.pt'):
            out.append(f'| {run} | not trained | | | | |')
            continue
        model = M.load_model(device, d)
        enc = M.encode(A, device)
        vals = enc['values'].clone()
        vals[:, 0] = M.SOURCES.index('scf')
        for v, o in M.OWNED_BY.items():
            vals[:, C + P.index(v)] = -1 if o == 'puf' else 0
        hid = torch.zeros(len(A), len(P), dtype=torch.bool, device=device)
        hid[:, P.index('net_worth')] = True
        with torch.no_grad():
            p = torch.cat([model(vals[i:i + 50_000], hid[i:i + 50_000])[:, P.index('net_worth'), :NBn].exp()
                           for i in range(0, len(A), 50_000)]).double().cpu().numpy()
        exp_ = p @ m_clip
        gap = lambda m: 100 * (np.average(exp_[m], weights=w[m]) / np.average(truth[m], weights=w[m]) - 1)
        i0 = A['interest'].to_numpy() == 0
        d0 = A['dividends'].to_numpy() == 0
        wg = A['wages'].to_numpy()
        out.append(f'| {run} | {gap(np.ones(len(A), bool)):+.1f} | {gap(i0):+.1f} | {gap(i0 & d0):+.1f} | '
                   f'{gap(wg == 0):+.1f} | {gap(wg >= 2e5):+.1f} |')
    return out


def wq(x, w, probs):
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12), len(xs) - 1)]


if __name__ == '__main__':
    {'tables': tables, 'check': check}[sys.argv[1]]()
