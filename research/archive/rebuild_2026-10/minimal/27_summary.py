"""
27_summary.py  (minimal example, step 27: compact tables for 27_RESULTS.md,
from the outputs 27_report.py and the runs wrote; reads only, writes
step27/summary.md.)
"""
import json
import os
import re

import numpy as np
import pandas as pd

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
M = f'{R}/model/step27'
MAIN = ['R2J', 'R2JAB', 'R1JAB', 'R2PJ']
OLD = ['R2', 'R1', 'R2AB', 'R1AB', 'R2W', 'R2P']
ITEMS = ['interest', 'dividends', 'capital_gains']


def runs(meth):
    return [f'{meth}s{k}' for k in (1, 2, 3) if os.path.exists(f'{M}/{meth}s{k}/score.json')]


def main():
    out = ['# Step 27: compact tables', '']
    # 1. families and settings
    rows = []
    for meth in MAIN + OLD:
        for v in ITEMS:
            S = [json.load(open(f'{M}/{r}/score.json'))['items'][v] for r in runs(meth)]
            if not S:
                continue
            d = lambda f: [f(s) for s in S]
            a = np.array(d(lambda s: s['family A: chance of reporting at $50, $500, $5,000 (%)']))
            pin = np.array(d(lambda s: [s['log score, family A with the dollar factor held at'][k] for k in ('0.8', '0.9', '1.0', '1.1')]))
            la = np.array(d(lambda s: s['log score, family A, learned settings (nats, households)']))
            lb = np.array(d(lambda s: s['log score, family B (nats, households)']))
            rows.append({'method': meth, 'item': v, 'seeds': len(S),
                         'log score A minus B (nats)': ', '.join(f'{x:+.0f}' for x in la - lb),
                         'P(B)': ', '.join(f'{x:.3f}' for x in d(lambda s: s['probability family B is right'])),
                         'report at $50 (%)': a[:, 0].mean(), 'at $500 (%)': a[:, 1].mean(), 'at $5,000 (%)': a[:, 2].mean(),
                         'dollar factor': ', '.join(f'{x:.2f}' for x in d(lambda s: s['family A: alpha, beta, dollar factor'][2])),
                         'score at factor 0.8, 0.9, 1.0, 1.1 minus learned (nats)':
                             ', '.join(f'{x:+.0f}' for x in (pin - la[:, None]).mean(0))})
    out += ['## Families and settings', '', pd.DataFrame(rows).to_markdown(index=False, floatfmt='.1f'), '']
    # 2. the family drawn in training (AB runs)
    rows = []
    for meth in ['R2JAB', 'R1JAB', 'R2AB', 'R1AB']:
        for r in runs(meth):
            lg = pd.read_csv(f'{M}/{r}/train_log.csv')
            for v in ITEMS:
                fam = lg[f'{v}_family_this_pass']
                rows.append({'run': r, 'item': v, 'passes under A (of 78)': int((fam.iloc[2:] == 'A').sum()),
                             'passes under B': int((fam.iloc[2:] == 'B').sum()),
                             'probability of A after the last pass': float(lg[f'{v}_prob_A'].iloc[-1]),
                             'last pass on which A was drawn': int(lg.loc[fam == 'A', 'epoch'].max()) if (fam == 'A').any() else 0})
    out += ['## The family drawn each pass (runs with both families)', '', pd.DataFrame(rows).to_markdown(index=False, floatfmt='.3g'), '']
    # 3. holders by wave
    rows = []
    for meth in ['R2J', 'R2JAB', 'R1JAB', 'R2PJ', 'R2']:
        H = []
        for r in runs(meth):
            H += [dict(h, seed=r) for h in json.load(open(f'{M}/{r}/score.json'))['holders by wave']
                  if 'PUF side' not in h['item']]
        if not H:
            continue
        h = pd.DataFrame(H).groupby(['tax year', 'item'], sort=False).mean(numeric_only=True).reset_index()
        for x in h.itertuples(index=False):
            rows.append({'method': meth, 'tax year': x[0], 'item': x[1], 'SCF reporting (m)': x[3],
                         'SCF after filling in (m)': x[4], 'IRS returns with it (m)': x[5],
                         'after filling / IRS': x[4] / x[5], 'SCF units (m)': x[2], 'IRS returns (m)': x[6]})
    out += ['## Holders by wave (millions; mean over seeds)', '', pd.DataFrame(rows).to_markdown(index=False, floatfmt='.2f'), '']
    # 4. other runs' fill share, and training minutes
    rows = []
    for meth in MAIN + OLD + ['R0']:
        for r in [f'{meth}s{k}' for k in (1, 2, 3) if os.path.exists(f'{M}/{meth}s{k}/train_meta.json')]:
            m = json.load(open(f'{M}/{r}/train_meta.json'))
            row = {'run': r, 'minutes': m.get('minutes'),
                   'held-back loss, last pass': m.get('heldback_loss_last_epoch', m.get('last_val_loss'))}
            for k, x in (m.get('settings') or {}).items():
                if isinstance(x, dict) and 'hidden holders among SCF zeros (%)' in x and x['hidden holders among SCF zeros (%)'] is not None:
                    row[f'hidden holders among zeros: {k} (%)'] = x['hidden holders among SCF zeros (%)']
            rows.append(row)
    out += ['## Training', '', pd.DataFrame(rows).to_markdown(index=False, floatfmt='.2f'), '']
    # 5. net worth table for the main methods, from report.md
    rep = open(f'{R}/step27/report.md').read()
    sec = rep.split('## 6.')[1].split('### Every seed')[0]
    lines = [l for l in sec.splitlines() if l.startswith('|')]
    head = [c.strip() for c in lines[0].strip('|').split('|')]
    body = [[c.strip() for c in l.strip('|').split('|')] for l in lines[2:]]
    t = pd.DataFrame(body, columns=head).set_index(head[0])
    keep = [c for c in t.columns if c.startswith('SCF') or c.startswith('N1') or c.startswith('N2')
            or re.match(r'^(R0|R2J|R2JAB|R1JAB|R2PJ) (restated )?mean', c)]
    out += ['## Net worth, nominal 2019 dollars (from report.md section 6)', '', t[keep].T.to_markdown(), '']
    sec7 = rep.split('## 7.')[1].split('## 8.')[0]
    out += ['## Generated PUF variables (report.md section 7, main runs only)', '']
    lines7 = [l for l in sec7.splitlines() if l.startswith('|')]
    head7 = [c.strip() for c in lines7[0].strip('|').split('|')]
    body7 = [[c.strip() for c in l.strip('|').split('|')] for l in lines7[2:]]
    t7 = pd.DataFrame(body7, columns=head7).set_index(head7[0])
    k7 = [c for c in t7.columns if c.startswith('PUF') or re.match(r'^(R0|R2J|R2JAB|R1JAB)s\d$', c)]
    out += [t7[k7].to_markdown(), '']
    open(f'{R}/step27/summary.md', 'w').write('\n'.join(out) + '\n')
    print('\n'.join(out))


if __name__ == '__main__':
    main()
