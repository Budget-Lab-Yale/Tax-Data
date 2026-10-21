"""
25_totals.py  (step 25: which holders does the SCF miss? 25_REPORTING_SHAPE.md)

Compares the 2016 SCF wave (tax year 2015; table E4, dollars as the SCF
reports them, in 2015 dollars) with the 2015 PUF and the IRS's published 2015
totals, for taxable interest and ordinary dividends:

  1. Weighted totals and the share of units with any amount: SCF (all units),
     PUF filers, PUF filers plus the crude non-filers of table E4N, IRS 2015
     (SOI Pub 1304 table 1.4, all returns).
  2. Where reporters' amounts sit: weighted percentiles of positive amounts,
     SCF reporters against PUF holders (filers plus non-filers).
  3. Holders by dollar range: units per range in each file, and the SCF's
     count as a share of the PUF side's. If small holders are the ones
     missing, the share rises with the amount; if holders of every size are
     missing at the same rate, it is flat.

Writes scratch checks/step25/totals.md.
"""
import os

import numpy as np
import pandas as pd

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
IRS = {'interest': 'taxable_interest', 'dividends': 'ordinary_dividends'}
RANGES = [(0.01, 100), (100, 500), (500, 2_000), (2_000, 10_000), (10_000, 50_000), (50_000, np.inf)]


def wq(x, w, p):
    o = np.argsort(x)
    c = np.cumsum(w[o]) / w.sum()
    return np.array([x[o][np.searchsorted(c, q)] for q in p])


def main():
    os.makedirs(f'{R}/checks/step25', exist_ok=True)
    t = pd.read_parquet(f'{R}/data/stacked_E4N.parquet')
    scf = t[t.source == 'scf']
    puf = t[(t.source == 'puf') & (t.filer == 1)]
    both = t[t.source == 'puf']
    irs = pd.read_csv(f'{R}/targets/soi_2015_validation.csv')
    irs = irs[(irs.year == 2015) & (irs.table == 1.4) & (irs.filing_status == 'all') & (irs.agi_bracket_label == 'all')]
    get = lambda var, stat: float(irs[(irs.variable == var) & (irs.statistic == stat)]['value'].iloc[0])
    files = {'SCF 2016 wave, all units': scf, 'PUF 2015, filers': puf, 'PUF 2015 + crude non-filers': both}
    out = ['# Step 25: which holders does the SCF miss?', '',
           'Dollars in 2015 dollars. SCF: 2016 wave (tax year 2015), all units, five implicates with '
           'their weights. Non-filers: the crude stopgap of step 23.', '']
    for v in ['interest', 'dividends']:
        rows = []
        for name, f in files.items():
            x, w = f[v].to_numpy(float), f['weight'].to_numpy(float)
            rows.append({'file': name, 'units (millions)': w.sum() / 1e6,
                         'total ($ billion)': (x * w).sum() / 1e9,
                         'units with any (millions)': w[x > 0].sum() / 1e6,
                         'share with any (%)': 100 * w[x > 0].sum() / w.sum()})
        rows.append({'file': 'IRS 2015, all returns', 'units (millions)': get('returns', 'count') / 1e6,
                     'total ($ billion)': get(IRS[v], 'amount') / 1e9,
                     'units with any (millions)': get(IRS[v], 'count') / 1e6,
                     'share with any (%)': 100 * get(IRS[v], 'count') / get('returns', 'count')})
        d = pd.DataFrame(rows)
        out += [f'## {v}: totals', '', d.to_markdown(index=False, floatfmt='.1f'), '']
        # percentiles of positive amounts
        pr = []
        for name, f in [('SCF reporters', scf), ('PUF holders (filers + non-filers)', both), ('PUF holders (filers)', puf)]:
            x, w = f[v].to_numpy(float), f['weight'].to_numpy(float)
            m = x > 0
            q = wq(x[m], w[m], [0.1, 0.25, 0.5, 0.75, 0.9, 0.99])
            pr.append({'who': name, **{f'p{int(100 * p)}': f'${v_:,.0f}' for p, v_ in zip([0.1, 0.25, 0.5, 0.75, 0.9, 0.99], q)}})
        out += [f'## {v}: where positive amounts sit (weighted percentiles)', '', pd.DataFrame(pr).to_markdown(index=False), '']
        # holders by dollar range
        rr = []
        for lo, hi in RANGES:
            row = {'range': f'${lo:,.0f} to ' + ('more' if hi == np.inf else f'${hi:,.0f}')}
            for name, f in [('SCF', scf), ('PUF + non-filers', both)]:
                x, w = f[v].to_numpy(float), f['weight'].to_numpy(float)
                m = (x >= lo) & (x < hi)
                row[f'{name}: units (millions)'] = w[m].sum() / 1e6
                row[f'{name}: dollars ($ billion)'] = (x[m] * w[m]).sum() / 1e9
            row['SCF units as % of PUF side'] = 100 * row['SCF: units (millions)'] / row['PUF + non-filers: units (millions)']
            rr.append(row)
        out += [f'## {v}: holders by dollar range', '', pd.DataFrame(rr).to_markdown(index=False, floatfmt='.2f'), '']
    text = '\n'.join(out)
    open(f'{R}/checks/step25/totals.md', 'w').write(text + '\n')
    print(text)


if __name__ == '__main__':
    main()
