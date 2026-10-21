"""Business income split into parts: where generation loses holders.

John, 19 October. Without dropout, the model's chance of holding a rare part,
averaged over the real PUF records, is about right (34_hidecount.py), yet the
generated files still hold too few (34_fixes.py: partnership nonpassive
income 1.21-1.24% against 1.33%). This splits the gap into four steps, for
each model and variable:

  1. real records, generation's questions: on each real PUF record, draw an
     order of the PUF variables exactly as generation does
     (03_generate.generation_order); the variable is predicted with the
     record's real values of the variables before it in the order visible,
     and everything after it hidden, the computed totals (business income,
     capital gains) included, as in generation. Weighted mean chance that it
     is nonzero.
  2. generated records, the same questions: the same on the run's plain
     generated file (200,000 records, equal weights, 03_stack's check file).
  3. the plain file's actual share of holders.
  4. the stack's share (importance weights), as in 34_fixes.py.

Step 1 against the PUF's share: the model on the questions generation asks,
with real contexts. Step 2 against step 3: whether generation draws what the
model says (they should agree; a check of the code). Step 1 against step 2:
the effect of drawing the earlier variables from the model instead of
taking them from real records (errors carried forward). Step 3 against
step 4: the stack's oversampling and weights.

Also by position: step 1 and step 2 for the variable drawn in the first,
middle and last third of the order.

Output: $R/step34/gengap.md. GPU job, as 34_holdcheck.py, with 34_env.sh and
the stack's generation settings (MINIMAL_ORDER=own_first,
MINIMAL_COMPUTED_TOTAL=1, MINIMAL_COMPUTED_EXTRA=1).
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
RUNS = os.environ.get('GAP_RUNS', 'D0,D0s2,D0w,Ns1').split(',')
VARS = ['part_nonpass_inc', 'scorp_nonpass_inc', 'farm', 'rent_inc', 'sch_c', 'interest']
PLAIN = 'gen_puf_with_net_worth_ownerlabel_absent_ownfirst.parquet'


def load(name, f):
    spec = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def main():
    os.environ['MINIMAL_ALL_RECORDS'] = '1'
    G = load('gen03', '03_generate.py')
    M = G.m
    device = 'cuda'
    edges = json.load(open(M.BAND_EDGES))
    zero = {v: edges[f'puf/2015/{v}']['zero_band'] for v in VARS}
    C, P = len(M.CONTEXT), M.PREDICTED
    table = M.read_table()
    puf = table[table.source == 'puf'].reset_index(drop=True)
    real_vals = M.encode(puf, device)['values']
    real_w = puf['weight'].to_numpy(float)

    def questions(vals, w, model, seed):
        """Steps 1/2: mean chance of nonzero under generation's hiding, overall and by third."""
        g = torch.Generator().manual_seed(seed)
        order = G.generation_order(len(vals), 'puf', False, False, g).to(device)   # [n, steps]
        n_steps = order.shape[1]
        # step_of[i, j]: the step at which record i draws slot j (n_steps: not in the order)
        step_of = torch.full((len(vals), len(P)), n_steps, dtype=torch.long, device=device)
        step_of.scatter_(1, order, torch.arange(n_steps, device=device).expand_as(order))
        present = vals[:, C:] >= 0
        out = {}
        for v in VARS:
            j = P.index(v)
            before = step_of < step_of[:, j:j + 1]           # drawn before v (slots not in the order: never)
            hid = present & ~before
            h = []
            with torch.no_grad():
                for i in range(0, len(vals), 4096):
                    lp = M.predict(model, vals[i:i + 4096], hid[i:i + 4096])[:, j, :M.N_CHOICES[v]]
                    h.append(1 - lp.log_softmax(1)[:, zero[v]].exp().double())
            h = torch.cat(h).cpu().numpy()
            third = (3 * step_of[:, j].cpu().numpy()) // n_steps
            out[v] = {'all': 100 * (w * h).sum() / w.sum()}
            for t, lab in enumerate(['first third', 'middle third', 'last third']):
                s = third == t
                out[v][lab] = 100 * (w[s] * h[s]).sum() / w[s].sum()
        return out

    rows = []
    for r in RUNS:
        model = M.load_model(device, f'{R}/model/step34/{r}')
        plain = pd.read_parquet(f'{R}/gen/step34/{r}/{PLAIN}')
        plain = plain[plain.source == 'puf'].reset_index(drop=True)
        # as a PUF record looks when its own variables are drawn: no net worth slot
        gen_vals = M.encode(plain.assign(split='train', weight=1.0, net_worth_band=-1), device)['values']
        gw = np.ones(len(plain))
        stack = pd.read_parquet(f'{R}/gen/gen_puf_stack_s34_{r.lower()}.parquet')
        stack = stack[stack.source == 'puf']
        sw = stack['weight'].to_numpy(float)
        q_real = questions(real_vals, real_w, model, seed=11)
        q_gen = questions(gen_vals, gw, model, seed=12)
        for v in VARS:
            real_share = 100 * real_w[puf[v + '_band'].to_numpy() != zero[v]].sum() / real_w.sum()
            plain_share = 100 * (plain[v + '_band'].to_numpy() != zero[v]).mean()
            stack_share = 100 * sw[stack[v + '_band'].to_numpy() != zero[v]].sum() / sw.sum()
            row = {'run': r, 'variable': v, 'PUF (%)': real_share,
                   '1. real records, generation questions (%)': q_real[v]['all'],
                   '2. generated records, same questions (%)': q_gen[v]['all'],
                   '3. plain generated file (%)': plain_share, '4. stack (%)': stack_share}
            for lab in ['first third', 'middle third', 'last third']:
                row[f'1, drawn in {lab} (%)'] = q_real[v][lab]
                row[f'2, drawn in {lab} (%)'] = q_gen[v][lab]
            rows.append(row)
        print(r, 'done', flush=True)
        del model
        torch.cuda.empty_cache()
    df = pd.DataFrame(rows)
    num = [c for c in df.columns if c.endswith('(%)')]
    ratio = df[['run', 'variable']].copy()
    for c in num[1:]:
        ratio[c.replace(' (%)', '')] = df[c] / df['PUF (%)']
    mean = ratio.groupby('variable', sort=False).mean(numeric_only=True).reset_index()
    lines = ['# Where generation loses holders', '',
             'Written by `34_gengap.py`. Ratios to the PUF\'s share of holders (PUF-side records, PUF weights). '
             'Steps: 1 = the model on real PUF records, asked generation\'s questions (a random draw order as in '
             'generation; the variables before it visible with their real values, the rest and the computed '
             'totals hidden); 2 = the same on the run\'s plain generated file (200,000 records); 3 = the plain '
             'file\'s actual share; 4 = the stack\'s share. Runs: ' + ', '.join(RUNS) + '.', '',
             '## Mean of the runs, ratio to the PUF', '', mean.to_markdown(index=False, floatfmt='.3f'), '',
             '## Every run, ratio to the PUF', '', ratio.to_markdown(index=False, floatfmt='.3f'), '',
             '## Every run, shares (%)', '', df.to_markdown(index=False, floatfmt='.3f'), '']
    open(f'{R}/step34/gengap.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines[:8 + len(mean)]))


if __name__ == '__main__':
    main()
