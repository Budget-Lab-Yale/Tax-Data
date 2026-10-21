"""Business income split into parts: does the shortfall in holders depend on
how much of the record the model sees?

John, 19 October. 34_holdcheck.py found the trained models' chance of holding
a rare part, averaged over the real PUF records, is right when every variable
is hidden but 10-20% low when only that part is hidden. Training hides a
random number k of a record's slots, each count equally likely, and scores
every hidden slot, so for a given variable most of its training comes from
records with many slots hidden: given that v is hidden, the chance that k
slots are hidden is proportional to k. Generation draws the PUF variables in
a random order, so v is drawn with k = 1, 2, ..., n slots still hidden
equally often. If the shortfall grows as k falls, the models are least
accurate where training gave them the least practice.

For each model and variable: hide v plus k - 1 other slots chosen at random
(fixed seed), average the model's chance that v is nonzero over the real PUF
records with the PUF weights, separately for the training records and the
held-back ones. Nothing is trained or generated.

Output: $R/step34/hidecount.md. GPU job, as 34_holdcheck.py.
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
RUNS = os.environ.get('HIDE_RUNS', 'Ns1,Ns2,Ns3,Bs1,Bs2').split(',')
OUT = os.environ.get('HIDE_OUT', 'hidecount')     # $R/step34/<OUT>.md
VARS = ['part_nonpass_inc', 'scorp_nonpass_inc', 'rent_inc', 'farm', 'estate_inc', 'sch_c', 'interest']
KS = [1, 2, 4, 8, 16, 32, None]          # None: every slot hidden


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
    w = puf['weight'].to_numpy(float)
    is_test = (puf['split'] == 'test').to_numpy()
    values = M.encode(puf, device)['values']
    C, P = len(M.CONTEXT), M.PREDICTED
    zero = {v: edges[f'puf/2015/{v}']['zero_band'] for v in VARS}
    n_present = int((values[:, C:] >= 0).sum(1).max())

    def hidden_with(v, k, seed):
        """v hidden plus k - 1 other present slots at random; every slot if k is None."""
        present = values[:, C:] >= 0
        if k is None:
            return present
        g = torch.Generator(device=device).manual_seed(seed)
        scores = torch.rand(present.shape, generator=g, device=device)
        scores[~present] = 2.0
        scores[:, P.index(v)] = -1.0               # v sorts first, so it is always hidden
        rank = scores.argsort(1).argsort(1)
        return (rank < k) & present

    def chance(model, v, hid):
        out = []
        for i in range(0, len(values), 4096):
            with torch.no_grad():
                lp = M.predict(model, values[i:i + 4096], hid[i:i + 4096])[:, P.index(v), :M.N_CHOICES[v]]
            p0 = (lp - torch.logsumexp(lp, 1, keepdim=True)).exp()[:, zero[v]]
            out.append((1 - p0).double().cpu().numpy())
        return np.concatenate(out)

    def share(h, sel):
        return 100 * (w[sel] * h[sel]).sum() / w[sel].sum()

    rows = []
    for r in RUNS:
        model = M.load_model(device, f'{R}/model/step34/{r}')
        for v in VARS:
            x = puf[v + '_band'].to_numpy() != zero[v]
            for k in KS:
                h = chance(model, v, hidden_with(v, k, seed=1000 + (k or 0)))
                for lab, sel in [('training records', ~is_test), ('held-back records', is_test)]:
                    rows.append({'run': r, 'variable': v, 'records': lab,
                                 'slots hidden': 'all' if k is None else k,
                                 'PUF (%)': share(x.astype(float), sel), 'model (%)': share(h, sel)})
        print(r, 'done', flush=True)
        del model
        torch.cuda.empty_cache()
    df = pd.DataFrame(rows)
    df['ratio'] = df['model (%)'] / df['PUF (%)']
    order = ['1', '2', '4', '8', '16', '32', 'all']
    df['slots hidden'] = df['slots hidden'].astype(str)
    piv = (df.groupby(['records', 'variable', 'slots hidden'], sort=False)['ratio'].mean()
             .unstack('slots hidden')[order].reset_index())
    lines = ['# Shortfall in holders by number of slots hidden', '',
             'Written by `34_hidecount.py`. Model\'s chance that the variable is nonzero, under the PUF label, '
             'averaged over the real PUF records with the PUF weights, divided by the PUF\'s share of holders. '
             f'Runs: {", ".join(RUNS)}. '
             f'The variable is hidden together with k - 1 other slots chosen at random (a PUF record has up to '
             f'{n_present} slots). 1.00 = right. Mean of the runs.', '',
             piv.to_markdown(index=False, floatfmt='.3f'), '', '## Every run', '',
             df.to_markdown(index=False, floatfmt='.3f'), '']
    open(f'{R}/step34/{OUT}.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines[:6 + len(piv) + 2]))


if __name__ == '__main__':
    main()
