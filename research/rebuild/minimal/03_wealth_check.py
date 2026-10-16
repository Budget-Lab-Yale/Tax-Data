"""
03_wealth_check.py  (minimal example: wealth tables and holder shares for the v3 model)

What it does
  After the v3 model (coarsened SCF zeros for interest, dividends and capital
  gains; research/rebuild/minimal/08_STATE_2026-10-14.md), rebuilds three
  tables of 06_WEALTH_TABLES.md with the functions of 06_wealth_tables.py:
  Table 1 (net worth, all units), mean net worth by wage group, and the share
  above $1 million by wage group. Columns:
    actual SCF                   every SCF row, survey weights
    SCF-style v3                 gen/gen_scf.parquet from the v3 model
    PUF-style v3 (owner label)   gen/gen_puf_with_net_worth.parquet from the
                                 v3 model, net worth drawn under the SCF label
    PUF-style v2 (owner label)   the same from the v2 model, for reference
                                 (gen/gen_puf_with_net_worth_ownerlabel.parquet)
  Dollars come from the step 4 maps (net worth from the SCF map); PUF-style
  files get equal weights summing to SOI's 2019 return count, the SCF-style
  file to the actual SCF's weighted count, as in 06_wealth_tables.py.

  Also the holder shares (share with a nonzero amount) for interest,
  dividends and capital gains, in band space: the real PUF and SCF, and the
  generated files. PUF-style records should still match the PUF.

What it writes
  /nfs/roberts/scratch/pi_nrs36/jar335/minimal/wealth/v3fix/tables.md
"""
import importlib.util
import os

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{ROOT}/wealth/v3fix'
FILES = {'PUF-style v3 (owner label)': f'{ROOT}/gen/gen_puf_with_net_worth.parquet',
         'PUF-style v2 (owner label)': f'{ROOT}/gen/gen_puf_with_net_worth_ownerlabel.parquet'}
HOLDER_FILES = {'generated SCF-style v3': f'{ROOT}/gen/gen_scf.parquet',
                'generated SCF-style v2': f'{ROOT}/gen/v2/gen_scf.parquet',
                'generated PUF-style v3 (gen_puf)': f'{ROOT}/gen/gen_puf.parquet',
                'generated PUF-style v2 (gen_puf)': f'{ROOT}/gen/v2/gen_puf.parquet',
                'generated PUF-style v3 with net worth': f'{ROOT}/gen/gen_puf_with_net_worth.parquet'}


def load_module(filename):
    """The step files start with a digit, so a plain `import` cannot load them."""
    spec = importlib.util.spec_from_file_location(filename[:-3], os.path.join(HERE, filename))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


wt = load_module('06_wealth_tables.py')


def puf_style(path, returns):
    raw = pd.read_parquet(path)
    assert (raw['source'] == 'puf').all() and (raw['net_worth_band'] >= 0).all()
    df, hits = wt.dollars.draw_dollars(raw, wt.dollars.puf_maps(2015))
    assert not df[['net_worth', 'wages', 'dividends', 'married']].isna().any().any()
    print(f'{os.path.basename(path)}: fill-rule draws {hits}', flush=True)
    return df, np.full(len(df), returns / len(df))


def holder_shares():
    """Weighted share with a nonzero band, real data and generated files."""
    table = pd.read_parquet(f'{ROOT}/data/stacked.parquet')
    frames = {'actual PUF': table[table['source'] == 'puf'],
              'actual SCF (as reported)': table[table['source'] == 'scf']}
    frames.update({k: pd.read_parquet(p) for k, p in HOLDER_FILES.items()})
    rows = {}
    for name, f in frames.items():
        w = f['weight'].to_numpy(float)
        rows[name] = {v: (w * (f[v + '_band'].to_numpy() != 0)).sum() / w.sum()
                      for v in ['interest', 'dividends', 'capital_gains']}
    out = pd.DataFrame(rows).T
    lines = ['## Holder shares (share of records with a nonzero amount, weighted)', '',
             '| file | interest | dividends | capital gains |', '|---|---:|---:|---:|']
    lines += [f'| {i} | ' + ' | '.join(f'{100 * v:.1f}%' for v in r) + ' |'
              for i, r in out.iterrows()]
    return lines + ['']


def main():
    os.makedirs(OUT, exist_ok=True)
    returns = wt.tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    scf, w_scf = wt.actual_scf()
    gscf, w_gscf = wt.pretrain_scf(w_scf.sum())
    files = {'actual SCF': (scf, w_scf), 'SCF-style v3': (gscf, w_gscf),
             **{k: puf_style(p, returns) for k, p in FILES.items()}}
    wt.COLS = list(files)                       # md_table formats these columns
    t1, t2 = {}, {}
    for c, (df, w) in files.items():
        x = df['net_worth'].to_numpy(float)
        t1[c], t2[c] = wt.unconditional(x, w), wt.conditional(df, x, w)
    labs = list(t1['actual SCF'])
    out = ['## Table 1. Net worth, all units', '',
           wt.md_table('', labs, t1, pct=lambda k: '(%)' in k,
                       names=[k.replace(' (%)', '') for k in labs]), '']
    for stat, title in [('mean ($)', 'Mean net worth by wage group ($)'),
                        ('share above $1m (%)', 'Share of units with net worth above $1 million')]:
        sel = {c: t2[c][t2[c]['grouping'] == 'wages'] for c in files}
        data = {c: dict(zip(sel[c]['group'], sel[c][stat])) for c in files}
        out += [f'## {title}', '', wt.md_table('wages', wt.ORDER['wages'], data,
                                               pct=stat.endswith('(%)')), '']
    out += holder_shares()
    text = '\n'.join(out) + '\n'
    open(f'{OUT}/tables.md', 'w').write(text)
    print(text)


if __name__ == '__main__':
    main()
