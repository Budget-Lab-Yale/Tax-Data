"""
21_report.py  (minimal example, step 21: the side-by-side tables for the
priors of 21_PRIORS.md; 2026-10-16)

For v4c (the reference: one SCF wave, 2015 token) and each prior, from its
stack and its tilt's weights file, computed the same way for every column
(20_report.py's helpers; net worth and wages in 2015 dollars):

  1. the move: nats for the 136 IRS targets alone and with the eight wealth
     targets, the effective sample kept, records cut below 0.1 of their start;
  2. wealth before the tilt against the 2019 SCF (all units);
  3. after the tilt: what no target covers (median, top 0.1% share, mean net
     worth by wage group, joint and dependents shares);
  4. the share of weight on each year token before and after the tilt;
  5. the untargeted income relationships of 07_baseline (untargeted_<v>.csv):
     largest and mean gap after the tilt against the 2015 PUF, in points.

Writes checks/V5/report_tables_<set>.md (REPORT_SET, below; the first
round's tables were written as report_tables.md before the setting existed). Run through Slurm:
  sbatch -J rep21 research/rebuild/minimal/21_run_cpu.sbatch report
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = f'{ROOT}/checks/V5'


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


rep = load('rep', '20_report.py')
wt, dollars = rep.wt, rep.dollars
md, kl, ess, nonjoint2 = rep.md, rep.kl, rep.ess, rep.nonjoint2

# REPORT_SET: 'record' (first round: one year per record) or 'nw' (second
# round, plan decision 3 as of 2026-10-16: PUF variables at 2015, the prior
# on net worth's year only). Each writes report_tables_<set>.md.
SET = os.environ.get('REPORT_SET', 'record')
PRIORS = {'record': ['p21_a15', 'p21_a18', 'p21_a21', 'p21_anear', 'p21_awide', 'p21_aeq',
                     'p21_b15', 'p21_b21', 'p21_binterp', 'p21_beq'],
          'rank': ['p21r_a15', 'p21r_a18', 'p21r_a21', 'p21r_anear', 'p21r_awide', 'p21r_aeq',
                   'p21r_b15', 'p21r_b21', 'p21r_binterp', 'p21r_beq'],
          'nw': ['p21n_a15', 'p21n_a18', 'p21n_a21', 'p21n_anear', 'p21n_awide', 'p21n_aeq',
                 'p21n_b15', 'p21n_b21', 'p21n_binterp', 'p21n_beq']}[SET]
ARMS = {'v4c': dict(stack=f'{ROOT}/gen/gen_puf_stack_v4c.parquet', tilt=f'{ROOT}/tilt/v4c_scfbase',
                    tag='v4c', maps=f'{ROOT}/map/V4', by_year='')}
# third round ('rank'): every net worth rank into 2019-survey dollars
TARGET = '2018' if SET == 'rank' else ''
if SET == 'rank':
    ARMS['v4c, 2019 dollars'] = dict(stack=f'{ROOT}/gen/gen_puf_stack_v4c.parquet',
                                     tilt=f'{ROOT}/tilt/v4c_target2019', tag='v4c',
                                     maps=f'{ROOT}/map/V5', by_year='', target='2018')
for p in PRIORS:
    ARMS[p] = dict(stack=f'{ROOT}/gen/gen_puf_stack_{p}.parquet', tilt=f'{ROOT}/tilt/{p}', tag=p,
                   maps=f'{ROOT}/map/V5', by_year='2015,2018,2021', target=TARGET)
WAGE_GROUPS = ['0', '1-25k', '25-50k', '50-100k', '100-200k', '200k+']
# Net worth is reported in nominal 2019 dollars from the third round on
# (John, 2026-10-16): the 2015-dollar values times the 2016 wave's price
# factor over the 2019 wave's (1.2493059 / 1.1592053). The wage groups stay
# in 2015 wage dollars, as in every earlier table.
NOMINAL_2019 = 1.249305937909091 / 1.159205298 if SET == 'rank' else 1.0


def numbers(a):
    os.environ['MINIMAL_MAP_DIR'], os.environ['MINIMAL_NW_MAP_YEARS'] = a['maps'], a['by_year']
    os.environ['MINIMAL_NW_TARGET_MAP_YEAR'] = a.get('target', '')
    stack = pd.read_parquet(a['stack'])
    g15, _ = dollars.draw_dollars(stack, dollars.puf_maps(2015))
    W = pd.read_parquet(f"{a['tilt']}/weights_{a['tag']}.parquet")
    assert len(W) == len(stack) and np.array_equal(W['record_id'].to_numpy(), np.arange(len(stack)))
    w0, w1, w2 = (W[c].to_numpy(float) for c in ['weight_before', 'weight_after_136', 'weight_after'])
    x = g15['net_worth'].to_numpy(float) * NOMINAL_2019
    o = {'move, 136 IRS targets alone (nats)': kl(w0, w1),
         'move, 136 plus wealth (nats)': kl(w0, w2),
         'effective sample kept, 136 plus wealth (%)': 100 * ess(w2) / ess(w0),
         'records below 0.1 of their start weight': int((w2 / w0 < 0.1).sum())}
    for k, w in [('before', w0), ('after', w2)]:
        u = wt.unconditional(x, w)
        for s in ['total net worth ($ trillion)', 'median ($)', 'share negative (%)',
                  'top 1% share (%)', 'top 0.1% share (%)']:
            o[f'{k}: {s}'] = u[s]
        c = wt.conditional(g15, x, w)
        c = c[c['grouping'] == 'wages'].set_index('group')
        for gname in WAGE_GROUPS:
            o[f'{k}: mean net worth, wages {gname} ($)'] = c.loc[gname, 'mean ($)']
        o[f'{k}: joint (%)'] = 100 * w[g15['married'].to_numpy() == 1].sum() / w.sum()
        o[f'{k}: non-joint with 2+ dependents (%)'] = nonjoint2(g15, w)
        yr = stack['net_worth_year' if 'net_worth_year' in stack else 'year'].to_numpy()
        for t in [2015, 2018, 2021]:
            o[f'{k}: weight on token {t} (%)'] = 100 * w[yr == t].sum() / w.sum()
    u = pd.read_csv(f"{a['tilt']}/untargeted_{a['tag']}.csv")
    gap = 100 * (u['posttrain'] - u['actual 2015 PUF']).abs()
    o['untargeted: largest gap after the tilt (points)'] = float(gap.max())
    o['untargeted: mean gap after the tilt (points)'] = float(gap.mean())
    return o


def scf_numbers():
    u, x, w = rep.scf_reference()
    x = x * NOMINAL_2019
    o = {}
    un = wt.unconditional(x, w)
    for s in ['total net worth ($ trillion)', 'median ($)', 'share negative (%)',
              'top 1% share (%)', 'top 0.1% share (%)']:
        o[f'before: {s}'] = o[f'after: {s}'] = un[s]
    c = wt.conditional(u, x, w)
    c = c[c['grouping'] == 'wages'].set_index('group')
    for gname in WAGE_GROUPS:
        o[f'before: mean net worth, wages {gname} ($)'] = o[f'after: mean net worth, wages {gname} ($)'] = \
            c.loc[gname, 'mean ($)']
    return o


def main():
    os.makedirs(OUT, exist_ok=True)
    have = {k: a for k, a in ARMS.items() if os.path.exists(f"{a['tilt']}/weights_{a['tag']}.parquet")}
    print('available:', list(have), flush=True)
    N = {k: numbers(a) for k, a in have.items()}
    N['SCF 2019'] = scf_numbers()
    priors = {k: json.load(open(f'{ROOT}/gen/gen_meta_{k}.json'))['prior'] for k in have if not k.startswith('v4c')}
    cols = list(N)
    rows_all = list(dict.fromkeys(r for o in N.values() for r in o))
    pd.DataFrame({c: N[c] for c in cols}).to_csv(f'{OUT}/report_numbers_{SET}.csv')

    def kind(label):
        if '(nats)' in label:
            return 'nats'
        if '($)' in label:
            return 'dollar'
        if 'records' in label:
            return 'int'
        return 'pct'

    def section(title, keys):
        rows = [(k.split(': ', 1)[-1] if k.startswith(('before: ', 'after: ', 'untargeted: ')) else k,
                 {c: N[c].get(k, np.nan) for c in cols}) for k in keys]
        return [f'## {title}', '', md(rows, cols, {lab: kind(lab) for lab, _ in rows}), '']

    text = ['# Step 21: priors for 2019 (tables)', '',
            'Weights on the year tokens: ' + '; '.join(
                f'{k}: ' + ', '.join(f'{t} {100*p:.0f}%' for t, p in v.items()) for k, v in priors.items()),
            '']
    keys = lambda pre: [r for r in rows_all if r.startswith(pre)]
    text += section('1. The move', [r for r in rows_all if r.startswith(('move', 'effective', 'records'))])
    unit = 'nominal 2019 dollars' if SET == 'rank' else '2015 dollars'
    text += section(f'2. Before the tilt (net worth in {unit}, wage groups in 2015 wage dollars; '
                    'the SCF column is the 2019 wave, all units)',
                    [r for r in keys('before: ') if 'token' not in r])
    text += section('3. After the tilt', [r for r in keys('after: ') if 'token' not in r])
    text += section('4. Weight on each year token (%)', [r for r in rows_all if 'token' in r])
    text += section('5. Untargeted income relationships against the 2015 PUF', keys('untargeted'))
    with open(f'{OUT}/report_tables_{SET}.md', 'w') as f:
        f.write('\n'.join(text) + '\n')
    print('\n'.join(text))


if __name__ == '__main__':
    main()
