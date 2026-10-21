"""
32_mix.py  (minimal example, step 32: a 2019 starting file that mixes SCF
waves; 32_STATUS.md; 2026-10-18)

What it does. For one seed k of step 31, builds the starting files the tilt
reads, as 21_priors.py did for step 21: a mixture over net worth's year token
is the per-token stacks stacked together, each token's importance weights
multiplied by its share. Each per-token stack's weights sum to 400,000, so
each mixture's do too.

  gen/gen_puf_stack_s32_ks<k>_m16.parquet   100% 2016 wave (token 2015):
      step 31's stack itself (gen/gen_puf_stack_s31_ks<k>.parquet), rows in
      the same order, plus a column net_worth_year = 2015. Tilted, it must
      reproduce step 31's tilt (the check).
  gen/gen_puf_stack_s32_ks<k>_m80.parquet   80% 2019 wave (token 2018), 20%
      2016 wave: step 31's stack with weights x 0.2 (first, so its rows keep
      step 31's dollar draws), then the token-2018 stack with weights x 0.8.
      800,000 records.
  gen/gen_puf_stack_s32_ks<k>_m19.parquet   100% 2019 wave: the token-2018
      stack (gen/step32/gen_puf_stack_s32_ks<k>_nw2018.parquet).

Checks, written to step32/stack_check_ks<k>.csv:
  - the token-2015 stack drawn through the new path
    (gen/step32/..._nw2015.parquet, if it exists) against step 31's stack:
    share of rows with every band identical, and largest weight difference;
  - the token-2018 stack against step 31's stack: share of rows whose PUF
    variables (every band but net worth) are identical. When the GPU type
    matches, the PUF part should be the same records and only net worth
    differs.
Usage: python 32_mix.py <k>
"""
import json
import os
import sys

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
SHARE_2019 = 0.8


def read(path):
    s = pd.read_parquet(path)
    assert (s['source'] == 'puf').all(), path
    assert abs(s['weight'].sum() - 400_000) < 1e-3, (path, s['weight'].sum())
    return s


def compare(a, b, what, cols):
    assert len(a) == len(b), (what, len(a), len(b))
    same = np.ones(len(a), bool)
    for c in cols:
        same &= a[c].to_numpy() == b[c].to_numpy()
    by_part = {p: float(same[a['stratifier'].to_numpy() == p].mean()) for p in a['stratifier'].unique()}
    return {'comparison': what, 'rows': len(a), 'share of rows identical (%)': 100 * same.mean(),
            **{f'identical, {p} part (%)': 100 * v for p, v in by_part.items()}}


def main():
    k = int(sys.argv[1])
    s31 = read(f'{ROOT}/gen/gen_puf_stack_s31_ks{k}.parquet')
    assert 'net_worth_year' not in s31 and (s31['year'] == 2015).all()
    s31['net_worth_year'] = np.int64(2015)
    y18 = read(f'{ROOT}/gen/step32/gen_puf_stack_s32_ks{k}_nw2018.parquet')
    assert (y18['year'] == 2015).all() and (y18['net_worth_year'] == 2018).all()
    assert set(y18.columns) == set(s31.columns), (list(y18.columns), list(s31.columns))
    y18 = y18[list(s31.columns)]          # step 31's column order (rows untouched)
    for c in s31.columns:
        n = int(s31[c].isna().sum()) + int(y18[c].isna().sum())
        if n and c not in ('p_band', 'q_band'):
            print(f'unexpected missing values in {c}: {n}', flush=True)
            raise AssertionError(c)
    bands = [c for c in s31.columns if c.endswith('_band') and c not in ('p_band', 'q_band')]
    puf_cols = [c for c in bands if c != 'net_worth_band'] + ['married', 'n_dep'] + \
        [c for c in ['filer'] if c in s31]
    checks = []
    p15 = f'{ROOT}/gen/step32/gen_puf_stack_s32_ks{k}_nw2015.parquet'
    if os.path.exists(p15):
        y15 = read(p15)
        row = compare(s31, y15, 'token 2015 through the new path vs step 31 stack, every band', bands + ['married', 'n_dep'])
        row['largest weight difference'] = float(np.abs(s31['weight'].to_numpy() - y15['weight'].to_numpy()).max())
        checks.append(row)
    checks.append(compare(s31, y18, 'token 2018 vs step 31 stack, PUF variables only', puf_cols))
    os.makedirs(f'{ROOT}/step32', exist_ok=True)
    pd.DataFrame(checks).to_csv(f'{ROOT}/step32/stack_check_ks{k}.csv', index=False)
    print(pd.DataFrame(checks).T.to_string(), flush=True)

    files = {'m16': [(s31, 1.0)], 'm80': [(s31, 1 - SHARE_2019), (y18, SHARE_2019)], 'm19': [(y18, 1.0)]}
    for name, parts in files.items():
        out = pd.concat([p.assign(weight=p['weight'] * pi) for p, pi in parts], ignore_index=True)
        out = out.drop(columns=[c for c in ['record_id'] if c in out])
        v = f's32_ks{k}_{name}'
        out.to_parquet(f'{ROOT}/gen/gen_puf_stack_{v}.parquet', index=False)
        meta = {'file': f'gen_puf_stack_{v}.parquet', 'model_run': f'step31/Ks{k}',
                'net_worth_year_shares': {str(int(y)): float(out.loc[out.net_worth_year == y, 'weight'].sum() / 4e5)
                                          for y in sorted(out['net_worth_year'].unique())},
                'records': len(out), 'weight_sum': float(out['weight'].sum()),
                'parts': [(f'gen_puf_stack_s31_ks{k}.parquet' if p is s31
                           else f'step32/gen_puf_stack_s32_ks{k}_nw2018.parquet') + f', weights x {pi:g}'
                          for p, pi in parts],
                'what': 'mixture over net worth year tokens of step 31 style stacks (32_mix.py)'}
        json.dump(meta, open(f'{ROOT}/gen/gen_meta_{v}.json', 'w'), indent=2)
        print(json.dumps(meta, indent=2), flush=True)


if __name__ == '__main__':
    main()
