"""
36_compare_shared.py  (minimal example: every shared variable, SCF against PUF; 2026-10-18)

What it does
  The standard comparison of each variable the SCF and the PUF both carry,
  in table E33 (the more-variables table), tax year 2015: the SCF's 2016
  wave against the 2015 PUF, with and without the crude non-filers. For the
  assumed-correct variables (married, dependents, wages) and the mapped ones
  (interest, dividends, capital gains, business income, gross Social
  Security plus pensions, tax-exempt interest), weighted:
    - share zero, positive, negative; total;
    - percentiles of positive amounts (P10 ... P99, P99.9) and of losses;
    - the same by cell (wage group x married): share positive, median positive.
  Plus a check of the PUF's capital gains concept: the smallest value, and
  the share of returns at exactly -$3,000, below it, and between.
  SCF amounts are as reported (raw items split to tax units), before any
  mapping rule; PUF amounts as on the return.

Writes /nfs/roberts/scratch/pi_nrs36/jar335/minimal/checks/compare_shared/compare.md
Run: sbatch research/rebuild/minimal/36_cpu.sbatch
"""
import os

import numpy as np
import pandas as pd

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/checks/compare_shared'
YEAR = 2015
ASSUMED = ['married', 'n_dep', 'wages']
MAPPED = ['interest', 'dividends', 'capital_gains', 'business_income', 'retirement_gross', 'tax_exempt_interest']
WAGE_GROUPS = [('none', 0, 0), ('under $25k', 0, 25e3), ('$25k-75k', 25e3, 75e3),
               ('$75k-200k', 75e3, 200e3), ('over $200k', 200e3, np.inf)]
PCT = [10, 25, 50, 75, 90, 99, 99.9]


def wpct(x, w, q):
    if len(x) == 0:
        return np.nan
    o = np.argsort(x)
    c = np.cumsum(w[o]) / w.sum()
    return float(x[o][min(np.searchsorted(c, q / 100), len(x) - 1)])


def wage_group(wages):
    g = np.zeros(len(wages), dtype=int)
    for k, (_, lo, hi) in enumerate(WAGE_GROUPS[1:], start=1):
        g[(wages > lo) & (wages <= hi)] = k
    return g


def summary(name, x, w):
    tot = w.sum()
    pos, neg = x > 0, x < 0
    r = {'source': name, 'units (millions)': tot / 1e6,
         'zero (%)': 100 * w[x == 0].sum() / tot, 'positive (%)': 100 * w[pos].sum() / tot,
         'negative (%)': 100 * w[neg].sum() / tot, 'total ($ billions)': (w * x).sum() / 1e9}
    for q in PCT:
        r[f'P{q} of positives'] = wpct(x[pos], w[pos], q)
    for q in (10, 50, 90):
        r[f'P{q} of losses'] = wpct(-x[neg], w[neg], q) * -1 if neg.any() else np.nan
    return r


def main():
    os.makedirs(OUT, exist_ok=True)
    t = pd.read_parquet(f'{R}/data/stacked_E33.parquet')
    print('columns:', list(t.columns))
    t = t[t.year == YEAR]
    vars_ = [v for v in ASSUMED + MAPPED if v in t.columns]
    missing = [v for v in ASSUMED + MAPPED if v not in t.columns]
    sources = {'PUF filers': t[(t.source == 'puf') & (t.filer == 1)],
               'PUF side (filers + crude non-filers)': t[t.source == 'puf'],
               'SCF 2016 wave (tax units)': t[t.source == 'scf']}
    lines = ['# Shared variables: SCF 2016 wave against the 2015 PUF, as reported', '',
             'Weighted. Amounts in 2015 dollars as reported, before any mapping rule. '
             f'Variables not in table E33: {missing or "none"}.', '']
    for v in vars_:
        rows = []
        for name, f in sources.items():
            x = f[v].to_numpy(float)
            n_na = int(np.isnan(x).sum())
            assert n_na == 0, f'{name} {v}: {n_na} NA'
            rows.append(summary(name, x, f['weight'].to_numpy(float)))
        lines += [f'## {v}', '', pd.DataFrame(rows).to_markdown(index=False, floatfmt=',.1f'), '']
        if v in ('married', 'n_dep'):
            dist = []
            for name, f in sources.items():
                w = f['weight'].to_numpy(float)
                s = f.groupby(v)['weight'].sum() / w.sum() * 100
                dist.append(s.rename(name))
            lines += ['Distribution (% of units):', '', pd.concat(dist, axis=1).reset_index().to_markdown(index=False, floatfmt='.1f'), '']
            continue
        # by cell
        rows = []
        for name, f in sources.items():
            x, w = f[v].to_numpy(float), f['weight'].to_numpy(float)
            g, m = wage_group(f['wages'].to_numpy(float)), f['married'].to_numpy()
            for mm in (0, 1):
                for k, (lab, _, _) in enumerate(WAGE_GROUPS):
                    c = (g == k) & (m == mm)
                    if v == 'wages' and k == 0:
                        continue
                    pos = c & (x > 0)
                    rows.append({'cell': f"{'married' if mm else 'not married'}, wages {lab}", 'source': name,
                                 'positive (%)': 100 * w[pos].sum() / w[c].sum() if w[c].sum() else np.nan,
                                 'negative (%)': 100 * w[c & (x < 0)].sum() / w[c].sum() if w[c].sum() else np.nan,
                                 'median positive': wpct(x[pos], w[pos], 50),
                                 'P90 positive': wpct(x[pos], w[pos], 90)})
        d = pd.DataFrame(rows).sort_values(['cell', 'source'], kind='stable')
        lines += ['By cell:', '', d.to_markdown(index=False, floatfmt=',.1f'), '']
    # capital gains concept on the PUF
    p = sources['PUF filers']
    w = p['weight'].to_numpy(float)
    lines += ['## The PUF\'s capital gains: is it capped at -$3,000?', '']
    cols = [c for c in ['capital_gains', 'E01000', 'kg_st', 'kg_lt', 'kg_dist', 'kg_other'] if c in p.columns]
    rows = []
    for c in cols:
        x = p[c].to_numpy(float)
        ok = ~np.isnan(x)
        rows.append({'column': c, 'NA rows': int((~ok).sum()), 'minimum': float(np.nanmin(x)),
                     'below -$3,000 (% of returns)': 100 * w[ok & (x < -3000)].sum() / w.sum(),
                     'exactly -$3,000 (%)': 100 * w[ok & (x == -3000)].sum() / w.sum(),
                     '-$3,000 < x < 0 (%)': 100 * w[ok & (x > -3000) & (x < 0)].sum() / w.sum(),
                     'exactly -$1,500 (%)': 100 * w[ok & (x == -1500)].sum() / w.sum()})
    lines += [pd.DataFrame(rows).to_markdown(index=False, floatfmt=',.3f'), '']
    s = sources['SCF 2016 wave (tax units)']
    x, ws = s['capital_gains'].to_numpy(float), s['weight'].to_numpy(float)
    lines += [f'SCF capital gains as reported: below -$3,000 {100 * ws[x < -3000].sum() / ws.sum():.2f}% of units, '
              f'exactly -$3,000 {100 * ws[x == -3000].sum() / ws.sum():.2f}%, minimum {x.min():,.0f}.', '']
    open(f'{OUT}/compare.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
