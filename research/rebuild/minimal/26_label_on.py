"""
26_label_on.py  (minimal example, step 26: the fake-survey test of decision 18
rerun with the source label on; 2026-10-17. Write-up: 26_LABEL_ON.md. The
training is 22_fake_survey.py's arms latent_fixed_label, latent_learn_label
and latent_learn_label_drop; this file only reads saved models and scores.)

What it does
  pufcheck  For every saved fake-survey model of the arms compared in step 26
            (step 22's undistorted survey, today's rule and the two label-free
            latent arms, and step 26's label-on arms; the same for the
            non-filer variant PX of step 24), asks the model about the PUF
            pile and compares its answers with the PUF pile itself:
              - the share married, and dependents given marriage;
              - wages given married and dependents: mean wages and the share
                with no wages in each of the eight cells, and the log score of
                the true wage slice;
              - the share holding interest and dividends given married,
                dependents and wages.
            Label-on and today's models are asked under the PUF label, with
            other income absent as on PUF rows; label-free models with no
            label and other income present but hidden, as they were trained.
            The PUF pile is the models' own training data (all but a 2%
            held-back slice), so this is a check of fit, not of prediction.
            Writes fake26/pufcheck/<variant>_<run>.json.
  fills     (asked for by John, 2026-10-17) For every saved latent model
            (label on and label off), the filled-in true amounts of every
            survey row: its id, its reported interest and dividends (dollars
            and slice), and N_FILL_DRAWS independent draws of the true slice
            and dollars. Each draw is made from the final model (label-on
            models under the survey's label; label-free ones with no label),
            given everything else on the record, times the chance of
            reporting what the row reported, with the run's final settings
            (learned or planted): a zero gets a slice drawn from the belief
            times the chance of reporting zero; a reporter's true slice is
            its dollars over the dollar factor, as in training. Interest
            and dividends are drawn in turn, each given the other at its
            current draw, FILL_SWEEPS rounds per draw, in a random order.
            Dollars for a drawn slice: a PUF-pile amount drawn from that
            slice. Writes fills.parquet in the run's folder for step 26's
            runs, fake26/fills/<variant>_<run>.parquet for the older ones,
            and fake26/fills/summary.json (the survey's share holding each
            item after filling in, against the reported share, the survey's
            true share and the PUF pile's share).
  report    Collects step 22's, step 24's (PX) and step 26's score.json files
            and the pufcheck files into fake26/report.md: the measures of
            22_FAKE_SURVEY.md, label on against label off.

Usage (Slurm, 26_check.sbatch):
  python research/rebuild/minimal/26_label_on.py pufcheck
  python research/rebuild/minimal/26_label_on.py fills
  python research/rebuild/minimal/26_label_on.py report
"""
import glob
import importlib.util
import json
import math
import os
import sys

import numpy as np

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/fake26'
# variant -> (folder with step 22 / step 24 runs, folder with step 26 runs, table tag)
VARIANTS = {'main': (f'{R}/fake22', f'{R}/fake26', ''),
            'PX': (f'{R}/fake24/PX', f'{R}/fake26/PX', '_PX')}
OFF = ['oracle', 'rule', 'latent_fixed', 'latent_learn']
ON = ['latent_fixed_label', 'latent_learn_label', 'latent_learn_label_drop']
LABEL_FREE = ['latent_fixed', 'latent_learn']
NAMES = {'oracle': 'Undistorted survey', 'rule': "Today's rule",
         'latent_fixed': 'Latent, settings fixed, label off',
         'latent_fixed_label': 'Latent, settings fixed, label on',
         'latent_learn': 'Latent, settings learned, label off',
         'latent_learn_label': 'Latent, settings learned, label on',
         'latent_learn_label_drop': 'Latent, settings learned, label on, drop rule kept'}
SEEDS = [20261016, 20261017]
DS = ['D1', 'D2', 'D3', 'D4']


def runs():
    """(variant, arm, D, seed, folder) of every saved model to compare."""
    out = []
    for var, (old, new, _) in VARIANTS.items():
        for arm in OFF + ON:
            base = new if arm in ON else old
            for D in (['D0'] if arm == 'oracle' else DS):
                for s in SEEDS:
                    f = f'{base}/{arm}_{D}_s{s}'
                    if os.path.exists(f'{f}/model.pt'):
                        out.append((var, arm, D, s, f))
    return out


def load22():
    spec = importlib.util.spec_from_file_location('f22', f'{HERE}/22_fake_survey.py')
    F = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(F)
    return F


def wmean(x, w):
    return float(np.sum(x * w) / np.sum(w))


# ---- pufcheck --------------------------------------------------------------------------
def pufcheck():
    import torch
    F = load22()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    os.makedirs(f'{OUT}/pufcheck', exist_ok=True)
    for var, arm, D, seed, folder in runs():
        dest = f'{OUT}/pufcheck/{var}_{os.path.basename(folder)}.json'
        if os.path.exists(dest):
            continue
        tag = f"F22{VARIANTS[var][2]}_{D}_dollar"
        M = F.load_model_module(tag, folder)
        table = M.read_table()
        model = M.load_model(device, folder)
        res = check_one(M, model, table, arm in LABEL_FREE, device)
        res.update(variant=var, arm=arm, D=D, seed=seed, folder=folder)
        json.dump(res, open(dest, 'w'), indent=1)
        print(f'pufcheck {dest}', flush=True)


def check_one(M, model, table, label_free, device):
    import torch
    A = table[table.source == 'puf'].reset_index(drop=True)
    w = A['weight'].to_numpy()
    C = len(M.CONTEXT)
    j = {v: M.PREDICTED.index(v) for v in M.PREDICTED}
    vals = M.encode(A, device)['values'].clone()
    if label_free:            # as trained: no label, other income present but always hidden
        vals[:, 0] = -1
        vals[:, C + j['net_worth']] = 0
    else:                     # the PUF label; other income absent, as on PUF rows
        assert (vals[:, C + j['net_worth']] == -1).all()
    present = vals[:, C:] >= 0
    model.eval()

    def belief(visible, target):
        keep = torch.tensor([v in visible for v in M.PREDICTED], device=device)
        hid = present & ~keep[None, :]
        out = []
        with torch.no_grad():
            for i in range(0, len(A), 50_000):
                out.append(model(vals[i:i + 50_000], hid[i:i + 50_000])[:, j[target], :M.N_CHOICES[target]])
        return torch.cat(out).double().exp().cpu().numpy()

    married, n_dep = A['married'].to_numpy(), A['n_dep'].to_numpy()
    res = {}
    p = belief([], 'married')
    res['married share'] = {'truth': 100 * wmean(married == 1, w), 'model': 100 * wmean(p[:, 1], w)}
    p = belief(['married'], 'n_dep')
    res['dependents by marriage'] = {}
    for m in [0, 1]:
        s = married == m
        res['dependents by marriage'][f'married {m}'] = {
            'truth': [100 * wmean(n_dep[s] == k, w[s]) for k in range(4)],
            'model': [100 * wmean(p[s, k], w[s]) for k in range(4)]}
    # wages given married and dependents
    wb = A['wages_band'].to_numpy()
    x = A['wages'].to_numpy(dtype=float)
    dollars = np.array([wmean(x[wb == b], w[wb == b]) if (wb == b).any() else 0.0
                        for b in range(M.N_CHOICES['wages'])])
    p = belief(['married', 'n_dep'], 'wages')
    expected = p @ dollars
    nll = -np.log(np.clip(p[np.arange(len(A)), wb], 1e-300, None))
    cells = {}
    for m in [0, 1]:
        for k in range(4):
            s = (married == m) & (n_dep == k)
            cells[f'married {m}, dependents {k}'] = {
                'returns (weighted, millions)': float(w[s].sum() / 1e6),
                'mean wages, truth': wmean(x[s], w[s]), 'mean wages, model': wmean(expected[s], w[s]),
                'no wages, truth (%)': 100 * wmean(x[s] == 0, w[s]),
                'no wages, model (%)': 100 * wmean(p[s, 0], w[s]),
                'log score': wmean(nll[s], w[s])}
    res['wages by cell'] = cells
    res['wages, all'] = {'mean wages, truth': wmean(x, w), 'mean wages, model': wmean(expected, w),
                         'no wages, truth (%)': 100 * wmean(x == 0, w),
                         'no wages, model (%)': 100 * wmean(p[:, 0], w), 'log score': wmean(nll, w)}
    # interest and dividends held, given married, dependents and wages
    groups = [('wages 0', 0, 1), ('wages 1-50k', 1, 50e3), ('wages 50-200k', 50e3, 200e3),
              ('wages 200k+', 200e3, np.inf)]
    for v in ['interest', 'dividends']:
        p = belief(['married', 'n_dep', 'wages'], v)
        held = A[v].to_numpy() > 0
        out = {'all': {'truth': 100 * wmean(held, w), 'model': 100 * wmean(1 - p[:, 0], w)}}
        for name, lo, hi in groups:
            s = (x >= lo) & (x < hi)
            out[name] = {'truth': 100 * wmean(held[s], w[s]), 'model': 100 * wmean(1 - p[s, 0], w[s])}
        res[f'{v} held'] = out
    return res


# ---- fills ----------------------------------------------------------------------------
N_FILL_DRAWS, FILL_SWEEPS, FILL_SEED = 5, 3, 20261017
LATENT = ['interest', 'dividends']


def fills():
    import pandas as pd
    import torch
    F = load22()
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    os.makedirs(f'{OUT}/fills', exist_ok=True)
    summary_file = f'{OUT}/fills/summary.json'
    summary = json.load(open(summary_file)) if os.path.exists(summary_file) else {}
    for var, arm, D, seed, folder in runs():
        if arm in ('oracle', 'rule') or not os.path.exists(f'{folder}/score.json'):
            continue
        dest = (f'{folder}/fills.parquet' if arm in ON
                else f'{OUT}/fills/{var}_{os.path.basename(folder)}.parquet')
        key = f'{var}/{os.path.basename(folder)}'
        if os.path.exists(dest) and key in summary:
            continue
        M = F.load_model_module(f"F22{VARIANTS[var][2]}_{D}_dollar", folder)
        table = M.read_table()
        model = M.load_model(device, folder).eval()
        sc = json.load(open(f'{folder}/score.json'))
        aux = json.load(open(f'{os.path.dirname(folder)}/aux_{D}.json'))
        out, summ = fill_one(F, M, model, table, aux, sc, arm in LABEL_FREE, device)
        out.to_parquet(dest, index=False)
        summ.update(variant=var, arm=arm, D=D, seed=seed, file=dest)
        summary[key] = summ
        json.dump(summary, open(summary_file, 'w'), indent=1)
        print(f'fills {dest}', json.dumps(summ), flush=True)


def fill_one(F, M, model, table, aux, sc, label_free, device):
    import pandas as pd
    import torch
    gen = torch.Generator(device=device).manual_seed(FILL_SEED)
    rng = np.random.default_rng(FILL_SEED)
    POS0, C = F.POS0, len(M.CONTEXT)
    S = table[table.source == 'scf'].reset_index(drop=True)
    P = table[table.source == 'puf']
    vals = M.encode(S, device)['values'].clone()
    if label_free:
        vals[:, 0] = -1
    jl = {v: M.PREDICTED.index(v) for v in LATENT}
    n = {v: M.N_CHOICES[v] for v in LATENT}
    band = torch.arange(max(n.values()), device=device)
    l1mg, factor, zero = {}, {}, {}
    for v in LATENT:
        if sc.get('learned_settings') and sc['arm'].startswith('latent_learn'):
            st = sc['learned_settings'][v]
            lnx = torch.nan_to_num(torch.tensor(aux['lnx'][v], dtype=torch.float64, device=device), nan=0.0)
            g = torch.sigmoid(st['alpha'] + st['beta'] * lnx)
            factor[v] = st['factor']
        else:
            g = torch.tensor(aux['g_planted'][v], dtype=torch.float64, device=device)
            factor[v] = aux['planted']['factor']
        g = g.clamp(1e-6, 1 - 1e-6)
        b = band[:n[v]]
        l1mg[v] = torch.where(b == 0, 0.0, torch.where(b >= POS0, torch.log1p(-g), -math.inf)).float()
        x = S[v].to_numpy(dtype=float)
        zero[v] = torch.tensor(x == 0, device=device)
        rep = x > 0
        tb = vals[:, C + jl[v]].cpu().numpy()
        tb[rep] = POS0 + np.searchsorted(np.array(aux['cuts'][v]), x[rep] / factor[v], side='left')
        vals[:, C + jl[v]] = torch.tensor(tb, device=device)
    # PUF-pile dollars in each slice, for the dollars of a drawn slice
    pool = {v: {b: P.loc[P[f'{v}_band'] == b, v].to_numpy(dtype=float) for b in range(POS0, n[v])} for v in LATENT}
    out = pd.DataFrame({'record_id': S['record_id'], 'split': S['split'], 'weight': S['weight']})
    for v in LATENT:
        out[f'{v}_reported'] = S[v].to_numpy(dtype=float)
        out[f'{v}_reported_slice'] = S[f'{v}_band'].to_numpy()
        out[f'{v}_true'] = S[f'{v}_true'].to_numpy(dtype=float)       # the fake survey's truth, for checks
    w = S['weight'].to_numpy()
    shares = {v: [] for v in LATENT}
    with torch.no_grad():
        for d in range(N_FILL_DRAWS):
            cur = vals.clone()
            for _ in range(FILL_SWEEPS):
                for k in torch.randperm(len(LATENT), generator=gen, device=device).tolist():
                    v = LATENT[k]
                    hid = torch.zeros(len(S), len(M.PREDICTED), dtype=torch.bool, device=device)
                    hid[:, jl[v]] = True
                    lps = [model(cur[i:i + 50_000], hid[i:i + 50_000])[:, jl[v], :n[v]]
                           for i in range(0, len(S), 50_000)]
                    logits = torch.cat(lps) + l1mg[v]
                    u = torch.rand(logits.shape, generator=gen, device=device).clamp_min(1e-20)
                    draw = (logits - torch.log(-torch.log(u))).argmax(1)
                    col = cur[:, C + jl[v]].clone()
                    col[zero[v]] = draw[zero[v]]
                    cur[:, C + jl[v]] = col
            for v in LATENT:
                b = cur[:, C + jl[v]].cpu().numpy()
                dollars = np.where(S[v].to_numpy() > 0, S[v].to_numpy(dtype=float) / factor[v], 0.0)
                for i in np.flatnonzero(zero[v].cpu().numpy() & (b > 0)):
                    p = pool[v][int(b[i])]
                    dollars[i] = rng.choice(p) if len(p) else aux['cuts'][v][min(int(b[i]) - POS0, len(aux['cuts'][v]) - 1)]
                out[f'{v}_fill_slice_{d + 1}'] = b
                out[f'{v}_fill_{d + 1}'] = dollars
                shares[v].append(100 * wmean(b > 0, w))
    summ = {}
    for v in LATENT:
        tru = S[f'{v}_true'].to_numpy() > 0
        summ[v] = {'survey holders after filling in, mean of draws (%)': float(np.mean(shares[v])),
                   'draws (%)': shares[v],
                   'survey holders reported (%)': 100 * wmean(S[v].to_numpy() > 0, w),
                   'survey holders, truth (%)': 100 * wmean(tru, w),
                   'PUF pile holders (%)': 100 * wmean(P[v].to_numpy() > 0, P['weight'].to_numpy())}
    return out, summ


# ---- report ----------------------------------------------------------------------------
def sig(z):
    return 1 / (1 + math.exp(-z))


def report():
    sc, pc = {}, {}
    for var, arm, D, seed, folder in runs():
        f = f'{folder}/score.json'
        if os.path.exists(f):
            sc[(var, arm, D, seed)] = json.load(open(f))
        g = f'{OUT}/pufcheck/{var}_{os.path.basename(folder)}.json'
        if os.path.exists(g):
            pc[(var, arm, D, seed)] = json.load(open(g))
    L = ['# Step 26: fake survey with the source label on, results tables', '',
         'Generated by `26_label_on.py report`. Two numbers per cell: seed 20261016, seed 20261017. '
         '"-" means the run is missing.', '']

    def gap(s, group='interest: none'):
        g = s['groups'][group]
        return 100 * (g['model, held 1st-99th pct'] / g['truth, held 1st-99th pct'] - 1)

    def cell(var, arm, D, fn, fmt):
        vals = [fn(sc[(var, arm, D, s)]) if (var, arm, D, s) in sc else None for s in SEEDS]
        return ', '.join('-' if v is None else fmt.format(v) for v in vals)

    for var, Ds in [('main', DS), ('PX', ['D1'])]:
        L += [f'## Variant {var}', '']
        for group in ['interest: none', 'all', 'interest and dividends both none']:
            L += [f'### Mean other income of PUF returns, group "{group}": gap from the truth (%), '
                  'amounts held between the 1st and 99th percentiles', '',
                  '| | ' + ' | '.join(Ds) + ' |', '|---|' + '---:|' * len(Ds)]
            for arm in OFF + ON:
                L.append(f'| {NAMES[arm]} | ' + ' | '.join(
                    cell(var, arm, 'D0' if arm == 'oracle' else D, lambda s: gap(s, group), '{:+.0f}')
                    for D in Ds) + ' |')
            L.append('')
        for v in ['interest', 'dividends']:
            L += [f'### {v.capitalize()}: hidden holders among the survey\'s zeros, model (truth), % ', '',
                  '| | ' + ' | '.join(Ds) + ' |', '|---|' + '---:|' * len(Ds)]
            for arm in ['latent_fixed', 'latent_fixed_label', 'latent_learn', 'latent_learn_label',
                        'latent_learn_label_drop']:
                row = []
                for D in Ds:
                    c = cell(var, arm, D, lambda s: s['fill_diagnostics'][v]['hidden holder share, model (%)'],
                             '{:.1f}')
                    t = [sc[(var, arm, D, s)]['fill_diagnostics'][v]['hidden holder share, truth (%)']
                         for s in SEEDS if (var, arm, D, s) in sc]
                    row.append(c + (f' ({t[0]:.1f})' if t else ''))
                L.append(f'| {NAMES[arm]} | ' + ' | '.join(row) + ' |')
            L += ['', f'Ranking quality of the hidden {v} holders (weighted AUC: 0.5 = coin flip, 1 = perfect):', '',
                  '| | ' + ' | '.join(Ds) + ' |', '|---|' + '---:|' * len(Ds)]
            for arm in ['latent_fixed', 'latent_fixed_label', 'latent_learn', 'latent_learn_label',
                        'latent_learn_label_drop']:
                L.append(f'| {NAMES[arm]} | ' + ' | '.join(
                    cell(var, arm, D, lambda s: s['fill_diagnostics'][v]['ranking quality (AUC)'], '{:.2f}')
                    for D in Ds) + ' |')
            L.append('')
        L += ['### Learned reporting settings (planted: 13% / 78% / 98% at $50 / $500 / $5,000; '
              'factor 1.00, D2 0.80)', '',
              '| Arm | D | Item | At $50 (%) | At $500 (%) | At $5,000 (%) | Dollar factor |',
              '|---|---|---|---:|---:|---:|---:|']
        for arm in ['latent_learn', 'latent_learn_label', 'latent_learn_label_drop']:
            for D in Ds:
                for v in ['interest', 'dividends']:
                    def at(x):
                        return lambda s: 100 * sig(s['learned_settings'][v]['alpha']
                                                   + s['learned_settings'][v]['beta'] * math.log(x))
                    L.append(f'| {NAMES[arm]} | {D} | {v} | ' + ' | '.join(
                        cell(var, arm, D, at(x), '{:.0f}') for x in [50, 500, 5000]) + ' | ' +
                        cell(var, arm, D, lambda s: s['learned_settings'][v]['factor'], '{:.2f}') + ' |')
        L.append('')
        # the PUF-pile check
        L += ['### The PUF pile as the model sees it (pufcheck), against the PUF pile', '',
              'Mean over seeds and distortions. Wage gap: weighted mean, over the eight '
              'married-by-dependents cells, of the absolute gap of mean wages from the truth (% of the truth). '
              'No-wages gap: the same for the share with no wages (points). Log score: of the true wage '
              'slice given married and dependents (nats). Interest and dividends held: share given married, '
              'dependents and wages, model minus truth (points).', '',
              '| | Runs | Married share gap (pts) | Wage gap (%) | No-wages gap (pts) | Wage log score | '
              'Interest held gap (pts) | Dividends held gap (pts) |',
              '|---|---:|---:|---:|---:|---:|---:|---:|']
        truth_wls = None
        for arm in OFF + ON:
            ks = [k for k in pc if k[0] == var and k[1] == arm]
            if not ks:
                continue
            m, wg, zg, ls, ig, dg = [], [], [], [], [], []
            for k in ks:
                r = pc[k]
                m.append(r['married share']['model'] - r['married share']['truth'])
                cells = r['wages by cell'].values()
                W = sum(c['returns (weighted, millions)'] for c in cells)
                wg.append(sum(c['returns (weighted, millions)'] * abs(c['mean wages, model'] /
                                                                     c['mean wages, truth'] - 1) for c in cells) / W * 100)
                zg.append(sum(c['returns (weighted, millions)'] * abs(c['no wages, model (%)'] -
                                                                     c['no wages, truth (%)']) for c in cells) / W)
                ls.append(r['wages, all']['log score'])
                ig.append(r['interest held']['all']['model'] - r['interest held']['all']['truth'])
                dg.append(r['dividends held']['all']['model'] - r['dividends held']['all']['truth'])
            L.append(f'| {NAMES[arm]} | {len(ks)} | {np.mean(m):+.2f} | {np.mean(wg):.2f} | {np.mean(zg):.2f} | '
                     f'{np.mean(ls):.4f} | {np.mean(ig):+.2f} | {np.mean(dg):+.2f} |')
        L.append('')
        # cell detail for D1, one line per arm
        ks = [k for k in pc if k[0] == var and k[2] in ('D0', 'D1')]
        if ks:
            first = pc[ks[0]]
            names = list(first['wages by cell'])
            L += ['Mean wages by cell, D1 (undistorted survey: D0), mean over seeds ($):', '',
                  '| Cell | Returns (m) | Truth | ' + ' | '.join(NAMES[a] for a in OFF + ON
                                                               if any(k[1] == a for k in ks)) + ' |',
                  '|---|---:|---:|' + '---:|' * len({k[1] for k in ks})]
            for c in names:
                fc = first['wages by cell'][c]
                row = [c, f"{fc['returns (weighted, millions)']:.1f}", f"{fc['mean wages, truth']:,.0f}"]
                for a in OFF + ON:
                    kk = [k for k in ks if k[1] == a]
                    if kk:
                        row.append(f"{np.mean([pc[k]['wages by cell'][c]['mean wages, model'] for k in kk]):,.0f}")
                L.append('| ' + ' | '.join(row) + ' |')
            L.append('')
    sf = f'{OUT}/fills/summary.json'
    if os.path.exists(sf):
        fs = json.load(open(sf))
        L += ['## Survey units holding each item after filling in (fills; mean of 5 draws, %)', '',
              '| Variant | D | Arm | Item | Reported | After filling in, seed 1, seed 2 | Survey truth | PUF pile |',
              '|---|---|---|---|---:|---:|---:|---:|']
        for var, Ds in [('main', DS), ('PX', ['D1'])]:
            for D in Ds:
                for arm in ['latent_fixed', 'latent_fixed_label', 'latent_learn', 'latent_learn_label',
                            'latent_learn_label_drop']:
                    ss = [fs.get(f'{var}/{arm}_{D}_s{s}') for s in SEEDS]
                    have = [x for x in ss if x]
                    if not have:
                        continue
                    for v in LATENT:
                        h = have[0][v]
                        after = ', '.join('-' if x is None else
                                          f"{x[v]['survey holders after filling in, mean of draws (%)']:.1f}" for x in ss)
                        L.append(f"| {var} | {D} | {NAMES[arm]} | {v} | {h['survey holders reported (%)']:.1f} | "
                                 f"{after} | {h['survey holders, truth (%)']:.1f} | {h['PUF pile holders (%)']:.1f} |")
        L.append('')
    open(f'{OUT}/report.md', 'w').write('\n'.join(L) + '\n')
    print('\n'.join(L), flush=True)


if __name__ == '__main__':
    {'pufcheck': pufcheck, 'fills': fills, 'report': report}[sys.argv[1]]()
