"""
21_priors.py  (minimal example, step 21: priors for 2019 from several SCF
waves; 21_PRIORS.md; 2026-10-16)

What it does. Builds the starting file for one prior: a mixture of the
model's PUF-style records over year tokens, with a weight on each token.

A prior with weight pi_t on token t is the distribution sum_t pi_t p(record |
t). Each per-token stack (03_stack.py with MINIMAL_STACK_YEAR=t) is a sample
from p(record | t) with importance weights summing to 400,000, so stacking
them with each token's weights multiplied by pi_t is a sample from the
mixture, weights summing to 400,000. Tokens with pi_t = 0 are left out.

Usage: python 21_priors.py <name> <run> <t>:<pi_t>[,<t>:<pi_t>...] [nw]
  nw (second round, plan decision 3 as of 2026-10-16): t is net worth's
  year only; every stack draws the PUF variables at 2015.
  e.g. python 21_priors.py p21_a_near V5A 2018:0.6667,2021:0.3333
Reads   gen/<run>/gen_puf_stack_<run lower>_y<t>.parquet
Writes  gen/gen_puf_stack_<name>.parquet  (record_id = row number, year =
        the record's token) and gen/gen_meta_<name>.json, where
        07_baseline.py (STACK_VERSION=<name>) reads them.
"""
import json
import os
import sys

import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'


def main():
    name, run, spec = sys.argv[1], sys.argv[2], sys.argv[3]
    # 'nw' (second round): the prior is on net worth's year only; the PUF
    # variables are drawn at 2015 in every stack (gen_puf_stack_<run>_nw<t>).
    per_variable = len(sys.argv) > 4 and sys.argv[4] == 'nw'
    pis = {int(t): float(p) for t, p in (x.split(':') for x in spec.split(','))}
    total = sum(pis.values())
    pis = {t: p / total for t, p in pis.items() if p > 0}
    parts, files = [], {}
    for t, pi in sorted(pis.items()):
        if per_variable:
            path = f'{ROOT}/gen/{run}/gen_puf_stack_{run.lower()}_nw{t}.parquet'
            s = pd.read_parquet(path)
            assert (s['year'] == 2015).all() and (s['net_worth_year'] == t).all(), path
        else:
            path = f'{ROOT}/gen/{run}/gen_puf_stack_{run.lower()}_y{t}.parquet'
            s = pd.read_parquet(path)
            assert (s['year'] == t).all(), path
        assert (s['source'] == 'puf').all(), path
        assert abs(s['weight'].sum() - 400_000) < 1e-3, (path, s['weight'].sum())
        s['weight'] = s['weight'] * pi
        parts.append(s)
        files[path] = pi
    out = pd.concat(parts, ignore_index=True)
    out = out.drop(columns=[c for c in ['record_id'] if c in out])
    out.to_parquet(f'{ROOT}/gen/gen_puf_stack_{name}.parquet', index=False)
    meta = {'file': f'gen_puf_stack_{name}.parquet', 'prior': {str(t): p for t, p in pis.items()},
            'model_run': run, 'prior_on': 'net worth only' if per_variable else 'whole record', 'parts': files, 'records': len(out),
            'weight_sum': float(out['weight'].sum()),
            'what': 'mixture over year tokens of per-token v4c-rule stacks (21_priors.py)'}
    with open(f'{ROOT}/gen/gen_meta_{name}.json', 'w') as f:
        json.dump(meta, f, indent=2)
    print(json.dumps(meta, indent=2))


if __name__ == '__main__':
    main()
