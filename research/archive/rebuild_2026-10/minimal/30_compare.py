"""
30_compare.py  (minimal example, step 30, part 1; 2026-10-18)

What it does
  Compares the three SCF variables that are mapped (interest, dividends,
  capital gains) with the PUF, cell by cell and in broad size ranges, to
  choose the rule for reporters' dollars (g) per variable. Data: table E27
  (step 27), tax year 2015: the 2016 SCF wave against the 2015 PUF with the
  crude non-filers of step 23.

  Cells: wage group x married, as step 29 (29_train.py), with step 29's two
  merges (not married $75k and over; married no wages or under $25k).
  Ranges: broad size ranges per variable (RANGES below).

  For each variable, cell and range: the weighted share of units in the
  range in each file, the ratio SCF over PUF, the mean amount in the range
  in each file, and the number of SCF households in the range.
  The top of each cell: among holders at or above a line (TOP_LINE), the
  share of units there in each file and weighted percentiles of the
  amounts, SCF over PUF. Where nearly every holder reports, a ratio of
  percentiles near one says reporters give PUF dollars (g unchanged).

  Capital gains: the SCF's one number against two PUF concepts: line 13
  (E01000 + E01100, after the loss limit) and before the limit
  (kg_st + kg_lt + kg_dist).

Writes /nfs/roberts/scratch/pi_nrs36/jar335/minimal/checks/step30/compare.md
Run: sbatch research/rebuild/minimal/30_run.sbatch compare
"""
import os

import numpy as np
import pandas as pd

R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/checks/step30'
YEAR = 2015
WAGE_GROUPS = [('none', 0, 0), ('under $25k', 0, 25e3), ('$25k-75k', 25e3, 75e3),
               ('$75k-200k', 75e3, 200e3), ('over $200k', 200e3, np.inf)]
# step 29's cells: (married, wage groups)
CELLS = [(0, [0]), (0, [1]), (0, [2]), (0, [3, 4]), (1, [0, 1]), (1, [2]), (1, [3]), (1, [4])]
INF = np.inf
RANGES = {
    'interest': [(0, 0), (0, 100), (100, 500), (500, 2_000), (2_000, 10_000), (10_000, 50_000), (50_000, INF)],
    'dividends': [(0, 0), (0, 100), (100, 500), (500, 2_000), (2_000, 10_000), (10_000, 50_000), (50_000, INF)],
    'capital_gains': [(-INF, -3_000), (-3_000, 0), (0, 0), (0, 1_000), (1_000, 10_000), (10_000, 50_000),
                      (50_000, 250_000), (250_000, INF)],
}
TOP_LINE = {'interest': 2_000, 'dividends': 2_000, 'capital_gains': 10_000}
PCTS = [25, 50, 75, 90]


def label(lo, hi):
    if lo == hi == 0:
        return 'zero'
    f = lambda x: f'${x:,.0f}'.replace('$-', '-$')
    if lo == -INF:
        return f'below {f(hi)}'
    if hi == INF:
        return f'{f(lo)} and over'
    return f'{f(lo)} to {f(hi)}'


def in_range(x, lo, hi):
    if lo == hi == 0:
        return x == 0
    if hi == 0:                     # negative ranges: [lo, 0)
        return (x >= lo) & (x < 0)
    if lo == 0:                     # small positive: (0, hi)
        return (x > 0) & (x < hi)
    return (x >= lo) & (x < hi)


def wpct(x, w, q):
    o = np.argsort(x)
    x, w = x[o], w[o]
    c = np.cumsum(w) / w.sum()
    return float(np.interp(q / 100, c, x))


def wage_group(wages):
    g = np.full(len(wages), -1, dtype=np.int64)
    g[wages == 0] = 0
    for k, (_, lo, hi) in enumerate(WAGE_GROUPS[1:], start=1):
        g[(wages > lo) & (wages <= hi) if k > 1 else (wages > 0) & (wages <= hi)] = k
    return g


def main():
    os.makedirs(OUT, exist_ok=True)
    t = pd.read_parquet(f'{R}/data/stacked_E27.parquet',
                        columns=['source', 'year', 'household_id', 'weight', 'married', 'filer', 'wages',
                                 'interest', 'dividends', 'capital_gains', 'kg_st', 'kg_lt', 'kg_dist'])
    t = t[t.year == YEAR].copy()
    puf, scf = t[t.source == 'puf'].copy(), t[t.source == 'scf'].copy()
    out = ['# Step 30: the mapped SCF variables against the PUF, by cell and size range', '',
           f'Tax year {YEAR}: SCF 2016 wave ({scf.weight.sum() / 1e6:.1f} million units, '
           f'{scf.household_id.nunique():,} households, five implicates) against the PUF '
           f'({puf[puf.filer == 1].weight.sum() / 1e6:.1f} million returns) plus crude non-filers '
           f'({puf[puf.filer == 0].weight.sum() / 1e6:.1f} million). Shares are of all units in the cell.', '']

    # capital gains before the loss limit: filers from the Schedule D pieces;
    # non-filers have no capital gains (the CPS does not ask; step 23)
    nf = puf.filer == 0
    assert (puf.loc[nf, 'capital_gains'] == 0).all()
    for c in ['kg_st', 'kg_lt', 'kg_dist']:
        n_na = int(puf.loc[~nf, c].isna().sum())
        assert n_na == 0, f'{c}: {n_na} NA on PUF filers'
    puf['cg_before'] = np.where(nf, 0.0, puf[['kg_st', 'kg_lt', 'kg_dist']].fillna(0).sum(axis=1))
    for c in ['wages', 'interest', 'dividends', 'capital_gains']:
        for name, f in [('PUF side', puf), ('SCF', scf)]:
            n_na = int(f[c].isna().sum())
            assert n_na == 0, f'{name} {c}: {n_na} NA'

    for f in (puf, scf):
        g = wage_group(f.wages.to_numpy(float))
        assert (g >= 0).all()
        f['cell'] = -1
        for k, (m, gs) in enumerate(CELLS):
            f.loc[(f.married.to_numpy() == m) & np.isin(g, gs), 'cell'] = k
        assert (f.cell >= 0).all()
    cell_label = [f"{'married' if m else 'not married'}, wages "
                  + (WAGE_GROUPS[gs[0]][0] if len(gs) == 1 else f'{WAGE_GROUPS[gs[0]][0]} to {WAGE_GROUPS[gs[-1]][0]}')
                  for m, gs in CELLS]

    comparisons = [('interest', 'interest', 'interest'), ('dividends', 'dividends', 'dividends'),
                   ('capital gains, PUF line 13 (after the loss limit)', 'capital_gains', 'capital_gains'),
                   ('capital gains, PUF before the loss limit', 'cg_before', 'capital_gains')]
    for title, pv, sv in comparisons:
        rng = RANGES[sv]
        out += [f'## {title}', '']
        # the summary per cell: holders, mean per unit, top of the cell
        rows = []
        for k in list(range(len(CELLS))) + ['all']:
            p = puf if k == 'all' else puf[puf.cell == k]
            s = scf if k == 'all' else scf[scf.cell == k]
            pw, sw = p.weight.to_numpy(float), s.weight.to_numpy(float)
            px, sx = p[pv].to_numpy(float), s[sv].to_numpy(float)
            top = TOP_LINE[sv]
            pt, st = px >= top, sx >= top
            r = {'cell': 'all cells' if k == 'all' else cell_label[k],
                 'SCF households': s.household_id.nunique(),
                 'holders, PUF (%)': 100 * pw[px != 0].sum() / pw.sum(),
                 'holders, SCF (%)': 100 * sw[sx != 0].sum() / sw.sum(),
                 'mean per unit, PUF ($)': (pw * px).sum() / pw.sum(),
                 'mean per unit, SCF ($)': (sw * sx).sum() / sw.sum(),
                 f'share at ${top:,}+, PUF (%)': 100 * pw[pt].sum() / pw.sum(),
                 f'share at ${top:,}+, SCF (%)': 100 * sw[st].sum() / sw.sum(),
                 f'SCF households at ${top:,}+': s.household_id[st].nunique()}
            for q in PCTS:
                if pt.sum() > 0 and st.sum() > 0:
                    r[f'P{q} at ${top:,}+, SCF/PUF'] = wpct(sx[st], sw[st], q) / wpct(px[pt], pw[pt], q)
                else:
                    r[f'P{q} at ${top:,}+, SCF/PUF'] = np.nan
            rows.append(r)
        d = pd.DataFrame(rows)
        out += ['### By cell: holders, mean, and the top of the cell', '',
                d.to_markdown(index=False, floatfmt='.2f'), '']
        # by cell and range
        rows = []
        for k in list(range(len(CELLS))) + ['all']:
            p = puf if k == 'all' else puf[puf.cell == k]
            s = scf if k == 'all' else scf[scf.cell == k]
            pw, sw = p.weight.to_numpy(float), s.weight.to_numpy(float)
            px, sx = p[pv].to_numpy(float), s[sv].to_numpy(float)
            for lo, hi in rng:
                pm, sm = in_range(px, lo, hi), in_range(sx, lo, hi)
                ps, ss = 100 * pw[pm].sum() / pw.sum(), 100 * sw[sm].sum() / sw.sum()
                rows.append({'cell': 'all cells' if k == 'all' else cell_label[k], 'range': label(lo, hi),
                             'PUF (% of units)': ps, 'SCF (% of units)': ss,
                             'SCF / PUF': ss / ps if ps > 0 else np.nan,
                             'PUF minus SCF (points)': ps - ss,
                             'mean in range, PUF ($)': (pw[pm] * px[pm]).sum() / pw[pm].sum() if pm.any() else np.nan,
                             'mean in range, SCF ($)': (sw[sm] * sx[sm]).sum() / sw[sm].sum() if sm.any() else np.nan,
                             'SCF households': s.household_id[sm].nunique()})
        d = pd.DataFrame(rows)
        out += ['### By cell and size range', '', d.to_markdown(index=False, floatfmt='.2f'), '']
    open(f'{OUT}/compare.md', 'w').write('\n'.join(out))
    print('\n'.join(out))


if __name__ == '__main__':
    main()
