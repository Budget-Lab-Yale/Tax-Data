"""
31_combos.py  (minimal example, step 31: does the filled-in SCF hold the right
COMBINATIONS of items? Status: 31_STATUS.md.)

The cell method targets each item (interest, dividends, capital gains) on its
own. Which combination a household ends up with is left to the model. This
compares, cell by cell, the share of units holding each combination of the
three items, 2016 wave (tax year 2015):
  PUF side          the 2015 PUF plus crude non-filers (what the method aims at);
  SCF reported      the SCF as reported;
  SCF filled in     cell method runs Ks1-3 (five draws each, averaged), and step
                    29's C4 runs for comparison.
Not targeted by the method, so a fair check of the assumption that hiding one
item is unrelated to hiding another.

Writes $R/step31/combos.md. Run: sbatch -J combos31 research/rebuild/minimal/31_cpu.sbatch combos
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/step31'
LATENT = ['interest', 'dividends', 'capital_gains']
SHORT = {'interest': 'I', 'dividends': 'D', 'capital_gains': 'G'}
RUNS = [('cell method', 'step31', ['Ks1', 'Ks2', 'Ks3']), ('step 29 C4', 'step29', ['C4s1', 'C4s2', 'C4s3'])]


def pattern(held):
    """held: dict item -> bool array. Returns a label per unit, e.g. 'I+D', 'none'."""
    n = len(next(iter(held.values())))
    lab = np.array([''] * n, dtype=object)
    for v in LATENT:
        lab = np.where(held[v], lab + np.where(lab == '', '', '+') + SHORT[v], lab)
    return np.where(lab == '', 'none', lab)


def shares(lab, w, cell, ncell):
    """Share (%) of units in each pattern, per cell and over all cells."""
    out = {}
    for c in list(range(ncell)) + ['all']:
        m = np.ones(len(w), bool) if c == 'all' else cell == c
        tot = w[m].sum()
        out[c] = {p: 100 * w[m & (lab == p)].sum() / tot for p in np.unique(lab)}
    return out


def main():
    os.makedirs(OUT, exist_ok=True)
    s = importlib.util.spec_from_file_location('t31', f'{HERE}/31_train.py')
    t31 = importlib.util.module_from_spec(s)
    s.loader.exec_module(t31)
    e = pd.read_parquet(f'{R}/data/stacked_E27.parquet')
    edges = json.load(open(f'{R}/data/band_edges_E27.json'))
    e['cell'], cells = t31.cell_map(e, edges)
    ncell = len(cells)
    labels = {c['cell']: c['label'] for c in cells}
    puf = e[e.source == 'puf']
    for v in LATENT:
        n_na = int(puf[v].isna().sum())
        assert n_na == 0, f'PUF side {v}: {n_na} NA'
    pw = puf['weight'].to_numpy(float)
    table = {'PUF side': shares(pattern({v: puf[v].to_numpy() != 0 for v in LATENT}), pw,
                                puf['cell'].to_numpy(), ncell)}
    # PUF side at the SCF's mix of cells, for the all-cells row
    scf15 = e[(e.source == 'scf') & (e.year == 2015)]
    mix = scf15.groupby('cell')['weight'].sum() / scf15['weight'].sum()
    pats = sorted({p for c in table['PUF side'].values() for p in c})
    table['PUF side']['all, at SCF cell mix'] = {
        p: sum(mix[c] * table['PUF side'][c].get(p, 0.0) for c in range(ncell)) for p in pats}
    first = True
    for name, step, runs in RUNS:
        acc = []
        for run in runs:
            fp = f'{R}/model/{step}/{run}/fills.parquet'
            if not os.path.exists(fp):
                print('missing', fp)
                continue
            f = pd.read_parquet(fp)
            f = f[f.year == 2015].drop(columns=['cell'], errors='ignore').merge(
                scf15[['record_id', 'cell']], on='record_id', how='left', validate='one_to_one')
            assert not f['cell'].isna().any(), f'{run}: rows without a cell'
            w, cell = f['weight'].to_numpy(float), f['cell'].to_numpy().astype(int)
            if first:
                table['SCF reported'] = shares(pattern({v: f[f'{v}_reported_band'].to_numpy() != 0 for v in LATENT}),
                                               w, cell, ncell)
                first = False
            for d in range(1, 6):
                acc.append(shares(pattern({v: f[f'{v}_fill{d}'].to_numpy() != 0 for v in LATENT}), w, cell, ncell))
        if acc:
            table[f'SCF filled in, {name}'] = {c: {p: float(np.mean([a[c].get(p, 0.0) for a in acc])) for p in pats}
                                              for c in list(range(ncell)) + ['all']}
    order = ['none', 'I', 'D', 'G', 'I+D', 'I+G', 'D+G', 'I+D+G']
    lines = ['# Step 31: combinations of interest (I), dividends (D) and capital gains (G), 2016 wave', '',
             'Share of units holding exactly each combination, % of the cell. Filled-in columns average five draws '
             'per run over the runs. The method targets each item on its own; the combinations are not targeted.', '']
    rows = []
    for c in list(range(ncell)) + ['all']:
        for src, tab in table.items():
            if c not in tab:
                continue
            rows.append({'cell': 'all cells' if c == 'all' else labels[c], 'source': src,
                         **{p: tab[c].get(p, 0.0) for p in order}})
    if 'all, at SCF cell mix' in table['PUF side']:
        rows.append({'cell': 'all cells', 'source': 'PUF side at the SCF cell mix',
                     **{p: table['PUF side']['all, at SCF cell mix'].get(p, 0.0) for p in order}})
    d = pd.DataFrame(rows)
    lines += ['## Every combination', '', d.to_markdown(index=False, floatfmt='.1f'), '']
    # the two summaries that matter most: none of the three, and interest with dividends
    k = d[['cell', 'source', 'none']].copy()
    k['holds both I and D'] = d['I+D'] + d['I+D+G']
    k['holds I but not D'] = d['I'] + d['I+G']
    k['holds D but not I'] = d['D'] + d['D+G']
    lines += ['## Summary: none of the three, and how interest and dividends go together', '',
              k.to_markdown(index=False, floatfmt='.1f'), '']
    open(f'{OUT}/combos.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
