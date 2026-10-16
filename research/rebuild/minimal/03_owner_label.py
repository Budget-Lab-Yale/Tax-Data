"""
03_owner_label.py  (minimal example, step 3 extra: how net worth is drawn for PUF-style records)

What it does
  06_WEALTH_TABLES.md found that net worth generated for PUF-labeled records
  is about twice the SCF's at every income level, while SCF-labeled records
  match the SCF. The model learned net worth only from SCF records, and no
  training record has both net worth and other_agi (one is SCF-only, the
  other PUF-only). So for a PUF-style record, "net worth given everything
  else" is a question the model never trained on, in two ways: the label,
  and the other_agi slot next to wealth. Three variants change only the view
  the model has when it draws net worth; incomes, married and n_dep are drawn
  as before, under the PUF label, in block order.

    A  source token set to scf; everything else visible.
    B  source token set to scf, and other_agi left out, so net worth is drawn
       from exactly the slots an SCF record has (married, n_dep, wages,
       interest, dividends, capital_gains).
    C  diagnostic: PUF label kept, other_agi hidden behind the mask token.

  All three use seed 3 and run in the same job, so their incomes are drawn
  identically; only net worth (and the married and n_dep drawn after it)
  can differ. The current file gen_puf_with_net_worth.parquet (no change in
  the view) also used seed 3, but on another GPU, so it is an independent
  sample, not a record-by-record pair.

  Then reproduces Table 1 and the mean-by-wage-group table of
  06_WEALTH_TABLES.md with the functions of 06_wealth_tables.py (dollars from
  the step 4 maps, net worth from the SCF map; PUF-style files with equal
  weights summing to SOI's 2019 return count), and two checks: the net worth
  band distribution of each file, and the zero-wage group's mean net worth
  split by whether other_agi is zero.

What it writes
  gen/gen_puf_with_net_worth_ownerlabel.parquet   variant A (200,000 records)
  gen/gen_meta_ownerlabel.json                    what the files are
  wealth/owner_label/gen_puf_with_net_worth_B.parquet, ..._C.parquet
  wealth/owner_label/tables.md                    the tables for 03_REPORT.md
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd
import torch

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{ROOT}/wealth/owner_label'
VARIANTS = {   # file, how net worth is drawn (see 03_generate.generate)
    'A': (f'{ROOT}/gen/gen_puf_with_net_worth_ownerlabel.parquet',
          {'label': 'owner', 'foreign': None}),
    'B': (f'{OUT}/gen_puf_with_net_worth_B.parquet', {'label': 'owner', 'foreign': 'absent'}),
    'C': (f'{OUT}/gen_puf_with_net_worth_C.parquet', {'label': 'own', 'foreign': 'mask'}),
}
DESCRIBE = {'A': 'A: scf label', 'B': 'B: scf label, no other_agi',
            'C': 'C: puf label, other_agi masked'}


def load_module(filename):
    """The step files start with a digit, so a plain `import` cannot load them."""
    spec = importlib.util.spec_from_file_location(filename[:-3], os.path.join(HERE, filename))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


g = load_module('03_generate.py')
ev = load_module('02_evaluate.py')
wt = load_module('06_wealth_tables.py')
m = g.m


def generate_files():
    torch.manual_seed(m.SEED)
    model = m.load_model('cuda' if torch.cuda.is_available() else 'cpu')
    for name, (path, borrowed) in VARIANTS.items():
        table = g.generate(model, 200_000, 'puf', with_net_worth=True, fixed_order=False,
                           seed=3, borrowed=borrowed)
        table.to_parquet(path, index=False)
    meta = {'model': os.path.join(m.MODEL_DIR, 'model.pt'), 'records': 200_000, 'seed': 3,
            'weight': 1, 'files': {
                os.path.basename(VARIANTS[k][0]): {'variant': k, 'net_worth_view': VARIANTS[k][1],
                                                   'what': DESCRIBE[k]} for k in VARIANTS},
            'what': 'as gen_puf_with_net_worth.parquet (block order, seed 3); only the '
                    'view the model has when it draws net worth changes. Records keep all '
                    'their values, other_agi included.'}
    with open(f'{ROOT}/gen/gen_meta_ownerlabel.json', 'w') as f:
        json.dump(meta, f, indent=2)


def puf_style(path, returns):
    raw = pd.read_parquet(path)
    assert (raw['source'] == 'puf').all() and (raw['net_worth_band'] >= 0).all()
    df, hits = wt.dollars.draw_dollars(raw, wt.dollars.puf_maps(2015))
    assert not df[['net_worth', 'wages', 'dividends', 'married']].isna().any().any()
    print(f'{os.path.basename(path)}: fill-rule draws {hits}', flush=True)
    return df, np.full(len(df), returns / len(df))


def any_shares(path):
    t = pd.read_parquet(path)
    return {'married': (t['married'] == 1).mean(),
            **{f'any {v}': (t[v + '_band'] != 0).mean()
               for v in ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']}}


def band_check(scf, gscf, files):
    """Check 1: net worth band groups, before any dollars are drawn."""
    labels = ['zero or neg.'] + ev.GROUP_LABELS[1:]
    cols = {'actual SCF': (scf['net_worth_band'], scf['weight']),
            'pretrain SCF-style': (gscf['net_worth_band'], None)}
    cols.update({c: (files[c][0]['net_worth_band'], None) for c in files if c not in cols})
    table = {}
    for c, (bands, w) in cols.items():
        w = np.ones(len(bands)) if w is None else w.to_numpy(float)
        table[c] = np.bincount(ev.band_group(bands.to_numpy(), 'net_worth'), weights=w,
                               minlength=len(labels)) / w.sum()
    lines = ['## Check 1. Net worth band groups (share of records, before dollars)', '',
             '| net worth group | ' + ' | '.join(table) + ' |', '|---|' + '---:|' * len(table)]
    lines += [f'| {lab} | ' + ' | '.join(f'{100 * table[c][k]:.1f}%' for c in table) + ' |'
              for k, lab in enumerate(labels)]
    return lines + ['']


def zero_wage_split(files):
    """Check 2: zero-wage records, mean net worth by whether other_agi is zero."""
    lines = ['## Check 2. Zero-wage records: mean net worth ($) by other_agi', '',
             '| file | other_agi zero: share | mean | other_agi nonzero: share | mean |',
             '|---|---:|---:|---:|---:|']
    for c, (df, w) in files.items():
        if 'other_agi' not in df or df['other_agi'].isna().all():
            continue                                  # SCF files have no other_agi
        zero_wage = df['wages'].to_numpy() == 0
        cells = []
        for has in [False, True]:
            sel = zero_wage & ((df['other_agi'].to_numpy() != 0) == has)
            cells += [f'{100 * w[sel].sum() / w[zero_wage].sum():.1f}%',
                      wt.sig3((df['net_worth'].to_numpy()[sel] * w[sel]).sum() / w[sel].sum())]
        lines.append(f'| {c} | ' + ' | '.join(cells) + ' |')
    return lines + ['']


def income_shares():
    """Did the incomes move? Shares of records in each PUF-style file."""
    paths = {'current': wt.GEN_PUF_NW, **{k: VARIANTS[k][0] for k in VARIANTS}}
    shares = pd.DataFrame({k: any_shares(p) for k, p in paths.items()})
    lines = ['## Shares of records by file', '',
             '| | ' + ' | '.join(shares.columns) + ' |', '|---|' + '---:|' * len(shares.columns)]
    lines += [f'| {i} | ' + ' | '.join(f'{100 * v:.2f}%' for v in r) + ' |'
              for i, r in shares.iterrows()]
    return lines + ['']


def main():
    os.makedirs(OUT, exist_ok=True)
    generate_files()
    returns = wt.tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    scf, w_scf = wt.actual_scf()
    gscf, w_gscf = wt.pretrain_scf(w_scf.sum())
    files = {'actual SCF': (scf, w_scf), 'pretrain SCF-style': (gscf, w_gscf),
             'current PUF-style': puf_style(wt.GEN_PUF_NW, returns),
             **{DESCRIBE[k]: puf_style(VARIANTS[k][0], returns) for k in VARIANTS}}
    wt.COLS = list(files)            # md_table formats these columns
    t1, t2 = {}, {}
    for c, (df, w) in files.items():
        x = df['net_worth'].to_numpy(float)
        t1[c], t2[c] = wt.unconditional(x, w), wt.conditional(df, x, w)
    labs = list(t1['actual SCF'])
    out = ['## Table 1. Net worth, all units', '',
           wt.md_table('', labs, t1, pct=lambda k: '(%)' in k,
                       names=[k.replace(' (%)', '') for k in labs]), '',
           '## Mean net worth by wage group ($)', '']
    for gname in ['wages']:
        data = {c: dict(zip(t2[c].loc[t2[c]['grouping'] == gname, 'group'],
                            t2[c].loc[t2[c]['grouping'] == gname, 'mean ($)'])) for c in files}
        out += [wt.md_table(gname, wt.ORDER[gname], data, pct=False), '']

    out += band_check(scf, gscf, files) + zero_wage_split(files)
    out += income_shares()
    text = '\n'.join(out) + '\n'
    open(f'{OUT}/tables.md', 'w').write(text)
    print(text)


if __name__ == '__main__':
    main()
