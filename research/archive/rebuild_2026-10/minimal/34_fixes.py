"""Business income split into parts: the fixes for too few rare holders, side by side.

John, 19 October (34_hidecount.py, 34_dropcheck.py). For each generated stack
that exists (the five original runs; the same models generated with dropout
on, suffix "do"; and the new training runs), the share of units holding each
business part and the mean business income per unit, against the PUF.
Before reweighting, 2015 dollars, all PUF-side units. Nothing is trained or
generated.

Output: $R/step34/fixes.md. CPU job:
  sbatch -J fix34 --partition=day --time=1:00:00 --mem=48G --wrap "...; python research/rebuild/minimal/34_fixes.py"
"""
import importlib.util
import os

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
RUNS = ['Ns1', 'Ns2', 'Ns3', 'Bs1', 'Bs2', 'Ns1do', 'Ns2do', 'Ns3do', 'Bs1do', 'Bs2do',
        'Hw1', 'Lg1', 'HL1', 'D0', 'D0s2', 'D0s3', 'Doff', 'D0w', 'D0o']
RUNS = os.environ['FIX_RUNS'].split(',') if os.environ.get('FIX_RUNS') else RUNS
OUT = os.environ.get('FIX_OUT', 'fixes')     # $R/step34/<OUT>.md
WHAT = {'Ns': 'base network, as before', 'Bs': 'bigger network, as before',
        'do': 'same model, dropout on in generation', 'Hw1': 'graded by boxes covered',
        'Lg1': '160 passes', 'HL1': 'graded by boxes covered, 160 passes', 'D0': 'no dropout', 'D0s2': 'no dropout', 'D0s3': 'no dropout', 'D0o': 'no dropout, rare business holders x4 at 1/4 weight',
        'Doff': 'dropout off for the last 10 passes', 'D0w': 'no dropout, graded by boxes covered'}
SHOWN = ['part_nonpass_inc', 'scorp_nonpass_inc', 'rent_inc', 'farm', 'estate_inc', 'sch_c']


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


def what(r):
    if r.endswith('do'):
        return WHAT['do']
    return WHAT.get(r, WHAT.get(r[:2], ''))


def main():
    rep = load('rep34', '34_report.py')
    dollars = load('dollars', '04_dollars.py')
    real, wr = rep.real_puf_side()
    files = [('PUF (real)', '', real, wr)]
    for r in RUNS:
        if os.path.exists(f'{R}/gen/gen_puf_stack_s34_{r.lower()}.parquet'):
            g, w = rep.gen_2015('s34', r, dollars, f'{R}/map/V34')
            files.append((r, what(r), g, w))
    m_real = (wr * real['business_income']).sum() / wr.sum()
    rows = []
    for name, desc, t, w in files:
        row = {'file': name, 'what': desc}
        for v in SHOWN:
            row[f'{v} holders (%)'] = 100 * w[t[v].to_numpy(float) != 0].sum() / w.sum()
        x = t['business_income'].to_numpy(float)
        m = (w * x).sum() / w.sum()
        p = w / w.sum()
        row['business income mean, ratio to PUF'] = m / m_real
        row['standard error of the ratio'] = np.sqrt((p ** 2 * (x - m) ** 2).sum()) / m_real
        rows.append(row)
    df = pd.DataFrame(rows)
    lines = ['# Too few rare holders: the fixes side by side', '',
             'Written by `34_fixes.py`. Each generated stack (400,000 records, importance weights, before '
             'reweighting) against the PUF side (filers and crude non-filers). Holders: share of units with the '
             'part nonzero. All new training runs use the seed of Ns1.', '',
             df.to_markdown(index=False, floatfmt='.3f'), '']
    open(f'{R}/step34/{OUT}.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
