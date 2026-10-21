"""Business income split into parts: is the shortfall in holders learned in
pretraining, or lost in generation?

John, 19 October. Every step-34 run draws too few holders of the large
business parts (partnership nonpassive income: 1.14-1.24% of units against the
PUF's 1.33%, each run 5 to 15 standard errors below). This asks each trained
model, on the real PUF records, for its chance that a unit holds each part, and
averages it with the PUF weights. Nothing is trained or generated.

Three questions to the model, under the PUF label:
  1. "all else shown": only this part hidden; every other variable visible.
  2. "parts hidden": all eighteen parts and the business income total hidden;
     everything else visible.
  3. "nothing shown": every variable hidden; only source and year visible.
     This is the model's own share of holders, the number generation should
     reproduce if it draws from the model faithfully.
Compared with the PUF's actual share and with the share in each run's
generated stack (shortfall check, before reweighting).

If question 3 gives about the PUF's share while the stack is short, the loss
is in generation; if question 3 is short too, pretraining learned too low a
chance.

Output: $R/step34/holdcheck.md. GPU job:
  sbatch -J hold34 --partition=gpu_h100,gpu_h200,gpu_b200 --gres=gpu:1 --time=1:00:00 \
    --mem=64G --wrap "... python research/rebuild/minimal/34_holdcheck.py"
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
RUNS = ['Ns1', 'Ns2', 'Ns3', 'Bs1', 'Bs2']
SHEET = json.load(open(f'{HERE}/34_settings_scf.json'))['step34_parts']
PARTS = [p for p in SHEET if p != '_what']
OTHERS = ['business_income', 'interest', 'dividends', 'tax_exempt_interest', 'unemployment', 'alimony']


def load_model_module():
    os.environ['MINIMAL_ALL_RECORDS'] = '1'
    spec = importlib.util.spec_from_file_location('model02', f'{HERE}/02_model.py')
    M = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(M)
    return M


def main():
    M = load_model_module()
    device = 'cuda'
    edges = json.load(open(M.BAND_EDGES))
    table = M.read_table()
    puf = table[table.source == 'puf'].reset_index(drop=True)
    w = puf['weight'].to_numpy(float)          # the PUF's own weights; the share is a ratio, so scale is irrelevant
    d = M.encode(puf, device)
    values = d['values']
    C, P = len(M.CONTEXT), M.PREDICTED
    vs = [v for v in PARTS + OTHERS if v in P]
    zero = {v: edges[f'puf/2015/{v}']['zero_band'] for v in vs}
    real = {v: 100 * w[puf[v].to_numpy(float) != 0].sum() / w.sum() for v in vs}

    def hold_share(model, hidden_fn, v):
        """Weighted mean over PUF records of the model's chance that v is nonzero."""
        out = []
        for i in range(0, len(values), 4096):
            vals = values[i:i + 4096]
            hid = hidden_fn(vals)
            with torch.no_grad():
                lp = M.predict(model, vals, hid)[:, P.index(v), :M.N_CHOICES[v]]
            p0 = (lp - torch.logsumexp(lp, 1, keepdim=True)).exp()[:, zero[v]]
            out.append((1 - p0).double().cpu().numpy())
        h = np.concatenate(out)
        return 100 * (w * h).sum() / w.sum()

    parts_idx = [P.index(v) for v in PARTS + ['business_income'] if v in P]

    def only(v):
        j = P.index(v)
        def f(vals):
            hid = torch.zeros(len(vals), len(P), dtype=torch.bool, device=device)
            hid[:, j] = True
            return hid & (vals[:, C:] >= 0)
        return f

    def parts_hidden(vals):
        hid = torch.zeros(len(vals), len(P), dtype=torch.bool, device=device)
        hid[:, parts_idx] = True
        return hid & (vals[:, C:] >= 0)

    def all_hidden(vals):
        return vals[:, C:] >= 0

    # the stack's share of holders (from the bands), per run
    stack = {}
    for r in RUNS:
        g = pd.read_parquet(f'{R}/gen/gen_puf_stack_s34_{r.lower()}.parquet')
        g = g[g.source == 'puf']
        wg = g['weight'].to_numpy(float)
        stack[r] = {v: 100 * wg[g[v + '_band'].to_numpy() != zero[v]].sum() / wg.sum()
                    for v in vs if v + '_band' in g.columns}

    rows = []
    for r in RUNS:
        model = M.load_model(device, f'{R}/model/step34/{r}')
        for v in vs:
            rows.append({'run': r, 'variable': v, 'PUF (%)': real[v],
                         'all else shown (%)': hold_share(model, only(v), v),
                         'parts hidden (%)': hold_share(model, parts_hidden, v),
                         'nothing shown (%)': hold_share(model, all_hidden, v),
                         'generated stack (%)': stack[r].get(v, np.nan)})
        print(r, 'done', flush=True)
        del model
        torch.cuda.empty_cache()
    df = pd.DataFrame(rows)
    num = ['PUF (%)', 'all else shown (%)', 'parts hidden (%)', 'nothing shown (%)', 'generated stack (%)']
    mean = df.groupby('variable', sort=False)[num].mean().reset_index()
    for c in num[1:]:
        mean[c.replace(' (%)', ', ratio to PUF')] = mean[c] / mean['PUF (%)']

    def md(t, fmt):
        return t.to_markdown(index=False, floatfmt=fmt)
    lines = ['# Is the shortfall in holders learned, or lost in generation?', '',
             'Written by `34_holdcheck.py`. Each trained step-34 model, under the PUF label, gives its chance that '
             'a unit holds each variable (nonzero), averaged over the real PUF-side records (filers and crude '
             'non-filers, both splits) with the PUF weights. "All else shown": only this variable hidden. '
             '"Parts hidden": the eighteen parts and the business income total hidden. "Nothing shown": every '
             'variable hidden, source and year visible: the model\'s own share of holders. "Generated stack": '
             'the share in the run\'s 400,000-record stack, before reweighting.', '',
             '## Mean of the five runs (Ns1-3 base network, Bs1-2 bigger network)', '',
             md(mean, ',.3f'), '', '## Every run', '', md(df, ',.3f'), '']
    open(f'{R}/step34/holdcheck.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
