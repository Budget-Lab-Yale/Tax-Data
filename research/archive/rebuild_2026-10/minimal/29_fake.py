"""
29_fake.py  (minimal example, step 29: the fake-survey test of the three rules
for hidden holders when the chance of reporting differs by wage group.
Status: 29_STATUS.md; results: 29_RESULTS.md.)

  python 29_fake.py tables   builds table F29 from table E27's PUF filers, as
     27_fake.py built F27, with two changes:
       - the chance a holder reports interest, dividends or capital gains
         varies by wage group: g = 1 / (1 + exp(-(-0.2 + 1.3 (ln|x| - ln 150
         - s)))), with s = ln(sqrt 3) for no wages and wages under $25,000,
         0 for $25,000 to $75,000, -ln(sqrt 3) for $75,000 and over. So the
         curve's middle (half report) is about $303 for low-wage households,
         $175 in the middle group and $101 for high-wage ones: a factor of 3
         between low and high. Married or not makes no difference. Dollars
         exact (factor 1).
       - capital gains is reported on one basis only, the line-13 total
         (all four pieces), so that the test is about the reporting rule
         alone; the two-basis mix of step 27 is left out.
     Half the filers (by record, seed 29) are the fake survey (source 'scf',
     year 2015; its own slot holds true other income, as in steps 22 and 27);
     the other half stay the PUF. Truth columns are kept.
  python 29_fake.py check   for the run in MINIMAL_RUN (trained on F29),
     writes step29/fake/<run>.json and .md: hidden holders among the survey's
     zeros against the truth, overall and by cell, the size of the hidden
     holdings against the truth, the learned curve middles, and the step-22
     and step-26 measure: the model's mean other income for the PUF-half
     returns, under the survey label, against the truth (all returns and
     returns with no interest).
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
OUT = f'{R}/step29/fake'
PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']
A_PLANT, B_PLANT = -0.2, 1.3
SHIFT = {0: math.log(math.sqrt(3)), 1: math.log(math.sqrt(3)), 2: 0.0,
         3: -math.log(math.sqrt(3)), 4: -math.log(math.sqrt(3))}
LATENT = ['interest', 'dividends', 'capital_gains']


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    m = importlib.util.module_from_spec(s)
    s.loader.exec_module(m)
    return m


def g_plant(x, shift):
    ax = np.abs(x)
    out = np.zeros(len(x))
    nz = ax > 0
    out[nz] = 1 / (1 + np.exp(-(A_PLANT + B_PLANT * (np.log(ax[nz]) - math.log(150.0) - shift[nz]))))
    return out


def tables():
    os.makedirs(OUT, exist_ok=True)
    t27 = load('t27', '27_tables.py')
    t29 = load('train29', '29_train.py')
    e = pd.read_parquet(f'{D}/stacked_E27.parquet')
    edges = json.load(open(f'{D}/band_edges_E27.json'))
    p = e[(e.source == 'puf') & (e.filer == 1)].reset_index(drop=True)
    rng = np.random.default_rng(29)
    srv = rng.random(len(p)) < 0.5
    p['split'] = 'train'
    s = p[srv].copy()
    pu = p[~srv].copy()
    for v in LATENT + PIECES + ['other_agi']:
        s[f'true_{v}'] = s[v]
    wg = t29.wage_group_of_band(edges)[s['wages_band'].to_numpy()]
    s['wage_group'] = wg
    shift = np.array([SHIFT[g] for g in wg])
    for v in LATENT:
        x = s[v].to_numpy(float)
        rep = rng.random(len(s)) < g_plant(x, shift)
        s[v] = np.where(rep, x, 0.0)
        s[f'{v}_reported_flag'] = rep.astype(int)
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
    for v in ['wages'] + LATENT:
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
    # the survey's wages slices must equal the PUF's (same cuts), so the wage groups are the same
    assert (t.loc[sv, 'wages_band'].to_numpy() == s['wages_band'].to_numpy()).all()
    fe['meta'] = {'F29': '29_fake.py: fake survey from E27 PUF filers; planted A (-0.2, 1.3) with the middle '
                         'shifted by wage group (x sqrt3 below $25k wages, x 1/sqrt3 from $75k), line-13 basis'}
    t.to_parquet(f'{D}/stacked_F29.parquet', index=False)
    json.dump(fe, open(f'{D}/band_edges_F29.json', 'w'), indent=1)
    note = {'survey rows': int(sv.sum()), 'PUF rows': int((~sv).sum()),
            'planted middle (dollars) by wage group': {t29.WAGE_GROUPS[g][0]: 150 * math.exp(-A_PLANT / B_PLANT + SHIFT[g])
                                                       for g in range(5)},
            'planted steepness': B_PLANT}
    for v in LATENT:
        note[f'{v}: survey holders reporting (%)'] = float(100 * (w[sv] * s[f'{v}_reported_flag'].to_numpy()
                                                                  * (s[f'true_{v}'].to_numpy() != 0)).sum()
                                                         / (w[sv] * (s[f'true_{v}'].to_numpy() != 0)).sum())
    json.dump(note, open(f'{OUT}/fake_note.json', 'w'), indent=1)
    print(json.dumps(note, indent=1))


def wq(x, w, probs):
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12), len(xs) - 1)]


def other_income(M, model_dir, table, device):
    """Step 22 and 26 measure (27_fake.py other_income_measure, one run)."""
    import torch
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
    model = M.load_model(device, model_dir)
    vals = M.encode(A, device)['values'].clone()
    vals[:, 0] = M.SOURCES.index('scf')
    for v, o in M.OWNED_BY.items():
        vals[:, C + P.index(v)] = -1 if o == 'puf' else 0
    hid = torch.zeros(len(A), len(P), dtype=torch.bool, device=device)
    hid[:, P.index('net_worth')] = True
    with torch.no_grad():
        p = torch.cat([model(vals[i:i + 50_000], hid[i:i + 50_000])[:, P.index('net_worth'), :NBn].exp()
                       for i in range(0, len(A), 50_000)]).double().cpu().numpy()
    exp_ = p @ m_clip
    gap = lambda m: float(100 * (np.average(exp_[m], weights=w[m]) / np.average(truth[m], weights=w[m]) - 1))
    i0 = A['interest'].to_numpy() == 0
    d0 = A['dividends'].to_numpy() == 0
    wg = A['wages'].to_numpy()
    return {'all returns': gap(np.ones(len(A), bool)), 'no interest': gap(i0), 'no interest and no dividends': gap(i0 & d0),
            'wages 0': gap(wg == 0), 'wages 200k+': gap(wg >= 2e5)}


def check():
    import torch
    os.makedirs(OUT, exist_ok=True)
    t29 = load('train29', '29_train.py')
    M = t29.load_model_module()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    run = os.path.basename(M.RUN)
    table = M.read_table()
    edges = json.load(open(M.BAND_EDGES))
    table['cell'], cells = t29.cell_map(table, edges)
    s = table[table.source == 'scf'].set_index('record_id')
    out = {'run': run, 'cells': cells}
    meta_p = os.path.join(M.MODEL_DIR, 'train_meta.json')
    meta = json.load(open(meta_p)) if os.path.exists(meta_p) else {}
    out['settings'] = meta.get('settings')
    out['rule'] = meta.get('rule', 'R0 (zeros as reported)' if 'R0' in run else '27_train.py, learned factor')
    puf = table[table.source == 'puf']
    # PUF slice mean dollars, for sizes
    meanx = {}
    for v in LATENT:
        x, b, w = puf[v].to_numpy(float), puf[f'{v}_band'].to_numpy(), puf['weight'].to_numpy(float)
        meanx[v] = np.array([np.average(x[b == k], weights=w[b == k]) if (b == k).any() else 0.0
                             for k in range(M.N_CHOICES[v])])
    fp = os.path.join(M.MODEL_DIR, 'fills.parquet')
    f = pd.read_parquet(fp).set_index('record_id').loc[s.index] if os.path.exists(fp) else None
    ws = s['weight'].to_numpy(float)
    cell = s['cell'].to_numpy()
    rows = []
    for v in LATENT:
        zero = s[v].to_numpy() == 0
        tt = s[f'true_{v}'].to_numpy(float)
        truth = tt != 0
        for c in [-1] + list(range(len(cells))):
            m = zero & ((cell == c) if c >= 0 else True)
            W = ws[m].sum()
            r = {'item': v, 'cell': 'all' if c < 0 else cells[c]['label'],
                 'survey zeros (weighted, millions)': W / 1e6,
                 'hidden holders among zeros, truth (%)': 100 * ws[m & truth].sum() / W,
                 'hidden dollars per zero, truth': (ws[m] * tt[m]).sum() / W}
            if f is not None:
                fills = [f[f'{v}_fill{d}'].to_numpy() for d in range(1, 6)]
                r['hidden holders among zeros, model (%)'] = float(np.mean([100 * ws[m & (b != 0)].sum() / W for b in fills]))
                r['hidden dollars per zero, model'] = float(np.mean([(ws[m] * meanx[v][b[m]]).sum() / W for b in fills]))
            else:
                r['hidden holders among zeros, model (%)'] = 0.0
                r['hidden dollars per zero, model'] = 0.0
            rows.append(r)
    out['hidden holders'] = rows
    out['other income gap (%)'] = other_income(M, M.MODEL_DIR, table, device)
    json.dump(out, open(f'{OUT}/{run}.json', 'w'), indent=1, default=float)
    lines = [f'# Step 29 fake check: {run}', '', f"Rule: {out['rule']}", '',
             pd.DataFrame(rows).to_markdown(index=False, floatfmt='.2f'), '',
             'Other income gap (%): ' + json.dumps(out['other income gap (%)'])]
    open(f'{OUT}/{run}.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    {'tables': tables, 'check': check}[sys.argv[1]]()
