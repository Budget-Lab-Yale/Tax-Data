"""
37_real_vs_gen.py  (minimal example, step 37: the real PUF tilted to the same 2019 targets, as a yardstick)

John, 19 October: tilt the actual PUF to the 2019 targets, and the PUF part
of the generated file to the same targets, and compare. How much the real
PUF has to move to reach 2019 is the cost the data themselves carry (two
years of change, and the PUF's own sample); what a generated file needs
beyond that is the model's error.

The same targets for both, filers only (the real PUF has no net worth, so no
wealth targets): the 136 IRS targets (05_tilt.target_set('brackets'), joint
returns included) and the SOI 2019 business targets in six AGI groups
(36_tilt.COARSE; families and readings as 36_tilt), exact, soft at 2% if the
exact tilt does not converge.

  real PUF   the 2015 PUF filers (table E34), PUF weights, dollars grown to
             2019 as in 36_tilt.real_2019 (the five AGI items by step 4's
             factors, the business parts by step 34's per-holder factors)
  generated  each run's stack (GEN_RUNS; 2019 dollars from the 2019 map, as
             07_baseline), filers only, importance weights scaled to the
             2019 return count

Reported: fit and cost of each tilt (effective sample kept, KL move, weight
factors, records moved more than tenfold), the business cells before, and
the untargeted business statistics of 36_tilt.untargeted before and after.

Writes $R/step37/real_vs_gen.md and summary.csv.
Usage: research/rebuild/minimal/37_real_vs_gen.sbatch
"""
import importlib.util
import os
import sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{ROOT}/step37'
RUNS = os.environ.get('GEN_RUNS', 'D0,D0s2,D0s3').split(',')


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


def main():
    os.makedirs(OUT, exist_ok=True)
    os.environ.setdefault('STACK_VERSION', f's34_{RUNS[0].lower()}')
    t36 = load('t36', '36_tilt.py')
    tiltmod, dollars = t36.tiltmod, t36.dollars
    brackets, cells = tiltmod.target_set('brackets')
    T = cells['value'].to_numpy(float)
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    bc = t36.business_targets(t36.COARSE)
    Tb_all = bc['value'].to_numpy(float)

    def run_tilt(name, df, w0):
        F = tiltmod.contributions(df, brackets)
        Fb = t36.business_contributions(df, t36.COARSE)
        use = (Tb_all != 0) & np.array([np.any(c > 0) for c in Fb.T])
        Fb, Tb = Fb[:, use], Tb_all[use]
        out = {'file': name, 'records': len(df), 'business cells used': int(use.sum())}
        w1, ok1, _ = tiltmod.tilt(w0, F, T)
        out.update({'136 only: converged': ok1, '136 only: effective sample kept': tiltmod.move(w0, w1)['ess_ratio'],
                    '136 only: KL (nats)': tiltmod.move(w0, w1)['kl_nats']})
        w2, ok2, it = tiltmod.tilt(w0, np.column_stack([F, Fb]), np.r_[T, Tb])
        form = 'exact'
        if not ok2:
            w2, ok2, it = tiltmod.tilt(w0, np.column_stack([F, Fb]), np.r_[T, Tb],
                                       rel_tol=np.r_[np.zeros(len(T)), np.full(len(Tb), 0.02)])
            form = 'soft, 2%'
        mv = tiltmod.move(w0, w2)
        out.update({'136 + business: form': form, '136 + business: converged': ok2,
                    '136 + business: effective sample kept': mv['ess_ratio'],
                    '136 + business: KL (nats)': mv['kl_nats'],
                    'weight factor, 1st percentile': mv['factor_p01'], 'weight factor, 99th percentile': mv['factor_p99'],
                    'records moved more than tenfold up': mv['records_factor_over_10'],
                    'records moved more than tenfold down': mv['records_factor_under_0.1']})
        before = pd.Series(t36.business_contributions(df, [(-np.inf, np.inf, 'all')]).T @ w0
                           / t36.business_targets([(-np.inf, np.inf, 'all')])['value'].to_numpy(float))
        unt = {'before': t36.untargeted(df, w0), 'after': t36.untargeted(df, w2)}
        print(pd.Series(out).to_string(), flush=True)
        return out, before, unt

    rows, befores, unts = [], {}, {}
    real, wr = t36.real_2019()
    o, b, u = run_tilt('real PUF (2015 records, grown)', real, wr * returns / wr.sum())
    rows.append(o)
    befores[o['file']], unts[o['file']] = b, u
    for r in RUNS:
        os.environ['GEN_FILE'] = f'{ROOT}/gen/gen_puf_stack_s34_{r.lower()}.parquet'
        g = tiltmod.generated_2019()
        f = g['filer'].to_numpy() == 1
        g = g[f].reset_index(drop=True)
        imp = g['weight'].to_numpy(float)
        o, b, u = run_tilt(f'generated {r}', g, imp * returns / imp.sum())
        rows.append(o)
        befores[o['file']], unts[o['file']] = b, u

    summary = pd.DataFrame(rows)
    summary.to_csv(f'{OUT}/summary.csv', index=False)
    lab = t36.business_targets([(-np.inf, np.inf, 'all')])
    bef = lab[['family', 'side', 'statistic']].copy()
    for k, v in befores.items():
        bef[k] = v.to_numpy()
    keys = list(next(iter(unts.values()))['before'])
    ut = pd.DataFrame({'statistic': keys})
    for k, v in unts.items():
        ut[f'{k}, before'] = [v['before'][x] for x in keys]
        ut[f'{k}, after'] = [v['after'][x] for x in keys]
    md = lambda d, f=',.3f': d.to_markdown(index=False, floatfmt=f)
    text = '\n'.join([
        '# The real PUF and the generated files, tilted to the same 2019 targets', '',
        'Written by `37_real_vs_gen.py`. Filers only. Targets: the 136 IRS targets, and the 136 plus the SOI 2019 '
        'business targets in six AGI groups. The real PUF starts from its own weights, the generated files from '
        'their importance weights, each scaled to the 2019 return count. "Effective sample kept": effective sample '
        'size after the tilt over before (for the real PUF, its own unequal weights are the "before").', '',
        '## A. Fit and cost', '', md(summary.T.reset_index().rename(columns={'index': ''}), ',.4f'), '',
        '## B. Business totals before the tilt: ratio to SOI 2019', '', md(bef), '',
        '## C. Untargeted business statistics (filers), before and after the 136 + business tilt', '', md(ut), ''])
    open(f'{OUT}/real_vs_gen.md', 'w').write(text)
    print(text)


if __name__ == '__main__':
    sys.exit(main())
