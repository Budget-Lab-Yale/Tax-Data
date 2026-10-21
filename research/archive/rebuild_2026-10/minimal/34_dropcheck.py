"""Business income split into parts: is the shortfall in rare holders caused by
dropout?

John, 19 October. The network is trained with dropout 0.1: in training, each
internal number is set to zero with chance 10% (and the rest scaled up), so
the model's prediction carries random noise; at use, dropout is off. Each
band's free intercept in the output layer settles where the average chance
WITH the noise matches the data. For a rare outcome the chance is close to
exp(logit), and noise in the logit raises the average of exp (it is convex),
so the intercept settles lower; with the noise removed at use, the chance is
then too low. The effect is near zero for common outcomes, and larger where
the logit draws on more visible inputs.

Test: on the real PUF records, the model's share of holders with dropout off
(as used) and on (as trained, mean of four noise draws), under three hidings:
only the variable hidden, every slot hidden, and the training rule's random
hiding (draw_hidden), where the share with dropout on should match the data
if the intercepts have settled. Nothing is trained.

Output: $R/step34/dropcheck.md. GPU job, as 34_holdcheck.py.
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
VARS = ['part_nonpass_inc', 'scorp_nonpass_inc', 'rent_inc', 'farm', 'estate_inc', 'sch_c', 'interest', 'wages']
DRAWS = 4


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
    puf = table[(table.source == 'puf') & (table.split == 'train')].reset_index(drop=True)
    w = puf['weight'].to_numpy(float)
    values = M.encode(puf, device)['values']
    C, P = len(M.CONTEXT), M.PREDICTED
    zero = {v: edges[f'puf/2015/{v}']['zero_band'] for v in VARS}
    present = values[:, C:] >= 0
    g = torch.Generator(device=device).manual_seed(7)
    train_rule = M.draw_hidden(values, g, None)

    def hidings(v):
        only = torch.zeros_like(present)
        only[:, P.index(v)] = True
        return {'only this hidden': only & present, 'all hidden': present, 'training rule': train_rule}

    def share(model, v, hid, dropout_on):
        """Weighted mean chance of nonzero over the records where v is hidden."""
        model.train(dropout_on)
        draws = DRAWS if dropout_on else 1
        h = torch.zeros(len(values), dtype=torch.float64, device=device)
        with torch.no_grad():
            for _ in range(draws):
                for i in range(0, len(values), 4096):
                    lp = M.predict(model, values[i:i + 4096], hid[i:i + 4096])[:, P.index(v), :M.N_CHOICES[v]]
                    h[i:i + 4096] += 1 - lp.log_softmax(1)[:, zero[v]].exp().double()
        h = (h / draws).cpu().numpy()
        sel = hid[:, P.index(v)].cpu().numpy()
        return 100 * (w[sel] * h[sel]).sum() / w[sel].sum(), sel

    rows = []
    for r in RUNS:
        model = M.load_model(device, f'{R}/model/step34/{r}')
        for v in VARS:
            x = (puf[v + '_band'].to_numpy() != zero[v]).astype(float)
            for lab, hid in hidings(v).items():
                off, sel = share(model, v, hid, False)
                on, _ = share(model, v, hid, True)
                actual = 100 * (w[sel] * x[sel]).sum() / w[sel].sum()
                rows.append({'run': r, 'variable': v, 'hiding': lab, 'PUF (%)': actual,
                             'dropout off, ratio': off / actual, 'dropout on, ratio': on / actual})
        print(r, 'done', flush=True)
        del model
        torch.cuda.empty_cache()
    df = pd.DataFrame(rows)
    mean = df.groupby(['hiding', 'variable'], sort=False)[['PUF (%)', 'dropout off, ratio', 'dropout on, ratio']] \
             .mean().reset_index()
    lines = ['# Is the shortfall in rare holders caused by dropout?', '',
             'Written by `34_dropcheck.py`. PUF training records, PUF weights. Ratio = model\'s share of holders / '
             'the PUF\'s, over the records where the variable is hidden. Dropout off = as used in generation; on = '
             f'as in training (mean of {DRAWS} noise draws). Mean of the five runs.', '',
             mean.to_markdown(index=False, floatfmt='.3f'), '', '## Every run', '',
             df.to_markdown(index=False, floatfmt='.3f'), '']
    open(f'{R}/step34/dropcheck.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines[:6 + len(mean) + 2]))


if __name__ == '__main__':
    main()
