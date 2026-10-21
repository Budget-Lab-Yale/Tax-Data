"""
20_diag.py  (minimal example, step 20: where the separate net worth model and the joint one differ)

Arm B's file has far more net worth than arm C's before the tilt, while the
PUF variables of the two files are nearly the same. So the difference is in
the net worth draw: B2 (trained on the SCF records only) against V4 (trained
on both), each asked for net worth as an SCF record would be (SCF label,
married, n_dep, wages, interest, dividends, capital gains visible, other_agi
absent). This file asks both models the same question on the same records:

  real SCF records (table E4, all of them, survey weights): in-sample for
      both models; the check that both reproduce the SCF on its own records
  real PUF records (table E4, PUF weights): what each model would put on the
      actual 2015 PUF, i.e. statistical matching of SCF wealth onto the PUF

For each record, the expected net worth = sum over net worth bands of the
model's probability times the band's weighted mean dollar value in the SCF
(map/V4/map_scf_2015.parquet, 2015 dollars). Groups: wage group, and
whether the record has any interest, dividends or capital gains. For the
PUF records the dollar groups use the PUF's own 2015 amounts; for the SCF
records, the SCF amounts in table E4 (2015 dollars).

Usage: python 20_diag.py V4=<dir> B2=<dir> [more name=dir]
Writes checks/S20/diag_expected_net_worth.csv and .md.
"""
import importlib.util
import os
import sys

import numpy as np
import pandas as pd
import torch

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
os.environ.setdefault('MINIMAL_TABLE', 'E4')
os.environ.setdefault('MINIMAL_RUN', 'V4')
os.environ['MINIMAL_ALL_RECORDS'] = '1'

spec = importlib.util.spec_from_file_location('gen20', os.path.join(HERE, '20_generate.py'))
gen20 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gen20)
m, NC = gen20.m, gen20.NC


@torch.no_grad()
def net_worth_probs(model, recs, chunk=50_000):
    """[records, net worth bands]: the model's net worth distribution for each
    record seen as an SCF record (20_generate.draw_net_worth's view)."""
    j = m.PREDICTED.index('net_worth')
    out = []
    for start in range(0, len(recs), chunk):
        part = recs.iloc[start:start + chunk]
        n = len(part)
        values = torch.full((n, len(m.SLOTS)), -1, dtype=torch.long)
        values[:, 0] = m.SOURCES.index('scf')
        values[:, 1] = m.YEARS.index(2015)
        for v in gen20.GIVEN:
            values[:, NC + m.PREDICTED.index(v)] = torch.as_tensor(part[gen20.col(v)].to_numpy(np.int64))
        values[:, NC + j] = 0
        hidden = torch.zeros(n, len(m.PREDICTED), dtype=torch.bool)
        hidden[:, j] = True
        out.append(m.predict(model, values, hidden)[:, j, :m.N_CHOICES['net_worth']].exp().double().numpy())
    return np.concatenate(out)


def main():
    models = dict(a.split('=', 1) for a in sys.argv[1:])
    t = pd.read_parquet(m.DATA)
    mp = pd.read_parquet(f'{ROOT}/map/V4/map_scf_2015.parquet')
    mp = mp[mp['variable'] == 'net_worth']
    band_mean = (mp['value'] * mp['weight']).groupby(mp['band']).sum() / mp['weight'].groupby(mp['band']).sum()
    value = np.zeros(m.N_CHOICES['net_worth'])
    value[band_mean.index.to_numpy()] = band_mean.to_numpy()
    files = {'SCF records': t[t['source'] == 'scf'].reset_index(drop=True),
             'PUF records': t[t['source'] == 'puf'].reset_index(drop=True)}
    rows = []
    for fname, f in files.items():
        w = f['weight'].to_numpy(float)
        wage = pd.cut(f['wages'], [-np.inf, 0, 25e3, 50e3, 100e3, 200e3, np.inf],
                      labels=['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+']).astype(str).to_numpy()
        inv = np.where((f[['interest', 'dividends', 'capital_gains']].to_numpy(float) != 0).any(1),
                       'any investment income', 'no investment income')
        exp = {}
        for name, d in models.items():
            model = m.load_model('cpu', d)
            exp[name] = net_worth_probs(model, f) @ value
        if fname == 'SCF records':
            exp['actual'] = f['net_worth'].to_numpy(float)
        grouping = [('all', np.full(len(f), 'all'))]
        grouping += [('wages', wage), ('investment income', inv),
                     ('wages x investment income', np.char.add(np.char.add(wage.astype(str), ', '), inv))]
        for gname, g in grouping:
            for lab in sorted(set(g)):
                sel = g == lab
                r = {'records': fname, 'grouping': gname, 'group': lab, 'n': int(sel.sum()),
                     'share of units (%)': 100 * w[sel].sum() / w.sum()}
                for name, e in exp.items():
                    r[f'mean net worth, {name}'] = (e[sel] * w[sel]).sum() / w[sel].sum()
                rows.append(r)
        print(fname, 'done', flush=True)
    out = pd.DataFrame(rows)
    os.makedirs(f'{ROOT}/checks/S20', exist_ok=True)
    out.to_csv(f'{ROOT}/checks/S20/diag_expected_net_worth.csv', index=False)
    open(f'{ROOT}/checks/S20/diag_expected_net_worth.md', 'w').write(out.to_markdown(index=False, floatfmt=',.0f') + '\n')
    print(out.to_string())


if __name__ == '__main__':
    main()
