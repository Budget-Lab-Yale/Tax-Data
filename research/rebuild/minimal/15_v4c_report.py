"""
15_v4c_report.py  (minimal example, v4c: the side-by-side tables against v4b)

v4c changes two things from v4b (15_BASELINE_V4.md, section "v4c"):
  1. the draw order (design/plan.md, decision 9, 2026-10-16): every PUF
     variable first, married and n_dep included, in a random order, then net
     worth under the SCF label with other_agi left out (03_generate.py,
     MINIMAL_ORDER=own_first);
  2. one base for every wealth count target (decision 17, 2026-10-16;
     07_baseline.py, WEALTH_COUNT_BASE). Two consistent bases are tilted:
     irs_returns (every count the SCF's share times the IRS return count, the
     default) and scf_units (every count the SCF's own weighted count). Which
     becomes the rule is John's decision.

Columns:
  v4b            gen/gen_puf_stack_v4b.parquet, tilt/v4b_allunits/ (old mixed base)
  v4b, IRS base  the same stack tilted to the irs_returns targets, tilt/v4b_onebase/:
                 separates the effect of the target fix from that of the order
  v4c            gen/gen_puf_stack_v4c.parquet, tilt/v4c_allunits/ (irs_returns)
  v4c, SCF base  the same stack, tilt/v4c_scfbase/ (scf_units)
  arm C          20_SEPARATE_MODELS.md, arm C, seed 1 (PUF variables first, married
                 and n_dep after the incomes; old base), for reference

Every number is computed as in 20_report.py (whose functions are reused), from
the arm's stack (2015 dollars from the V4 maps) and its weights file.
Writes checks/V4c/report_tables.md and checks/V4c/eight_targets.csv.
Run through Slurm (15_run_cpu.sbatch v4creport).
"""
import importlib.util
import os

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = f'{ROOT}/checks/V4c'
os.environ.setdefault('MINIMAL_MAP_DIR', f'{ROOT}/map/V4')


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


rep = load('rep', '20_report.py')
base = load('base', '07_baseline.py')
wt, dollars, tiltmod = rep.wt, rep.dollars, rep.tiltmod
md, kl = rep.md, rep.kl

V4B = dict(stack=f'{ROOT}/gen/gen_puf_stack_v4b.parquet', tag='v4b',
           plain=f'{ROOT}/gen/V4b/gen_puf_with_net_worth_ownerlabel_absent.parquet',
           plain2=f'{ROOT}/gen/gen_plain_seed35_v4b.parquet')
ARMS = {
    'v4b': dict(V4B, tilt=f'{ROOT}/tilt/v4b_allunits', base='mixed_v4b'),
    'v4b, IRS base': dict(V4B, tilt=f'{ROOT}/tilt/v4b_onebase', base='irs_returns'),
    'v4c': dict(stack=f'{ROOT}/gen/gen_puf_stack_v4c.parquet', tilt=f'{ROOT}/tilt/v4c_allunits', tag='v4c',
                plain=f'{ROOT}/gen/V4c/gen_puf_with_net_worth_ownerlabel_absent_ownfirst.parquet',
                plain2=f'{ROOT}/gen/gen_plain_seed35_v4c.parquet', base='irs_returns'),
    'v4c, SCF base': dict(stack=f'{ROOT}/gen/gen_puf_stack_v4c.parquet', tilt=f'{ROOT}/tilt/v4c_scfbase',
                          tag='v4c', plain=f'{ROOT}/gen/V4c/gen_puf_with_net_worth_ownerlabel_absent_ownfirst.parquet',
                          plain2=f'{ROOT}/gen/gen_plain_seed35_v4c.parquet', base='scf_units'),
    'arm C': dict(rep.ARMS['C'], base='mixed_v4b'),
}


def eight_targets(stack_path, weights_path, count_base):
    """The eight dollar-line targets under the given base, and the arm's
    values before the tilt, after the 136 alone and after."""
    stack = pd.read_parquet(stack_path)
    g15, _ = dollars.draw_dollars(stack, dollars.puf_maps(2015))
    x = g15['net_worth'].to_numpy(float)
    W = pd.read_parquet(weights_path)
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    targets, threshold = wt.wealth_targets('all')
    base.COUNT_BASE = count_base
    Fq, Tq = base.threshold_form(x, returns, targets, threshold, 'all')
    names = base.threshold_names(threshold, 'all')
    out = {'names': names, 'target': Tq, 'x': x, 'threshold': threshold}
    for k, c in [('pretrain', 'weight_before'), ('after 136', 'weight_after_136'), ('posttrain', 'weight_after')]:
        out[k] = Fq.T @ W[c].to_numpy(float)
    out['w0'], out['w2'] = W['weight_before'].to_numpy(float), W['weight_after'].to_numpy(float)
    return out


def main():
    os.makedirs(OUT, exist_ok=True)
    have = {k: a for k, a in ARMS.items()
            if os.path.exists(a['stack']) and os.path.exists(f"{a['tilt']}/weights_{a['tag']}.parquet")}
    print('arms available:', list(have), flush=True)
    N = {k: rep.arm_numbers(k, a, None) for k, a in have.items()}
    E = {k: eight_targets(a['stack'], f"{a['tilt']}/weights_{a['tag']}.parquet", a['base']) for k, a in have.items()}
    arms = list(N)
    scf_u, scf_x, scf_w = rep.scf_reference()
    text = []

    # 1. the eight targets: values under both bases, consistency
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    targets, threshold = wt.wealth_targets('all')
    tv = {}
    BASES = {'mixed_v4b': 'old mixed base (v4b)', 'irs_returns': 'IRS base', 'scf_units': 'SCF base'}
    for b in BASES:
        base.COUNT_BASE = b
        _, tv[b] = base.threshold_form(np.zeros(1), returns, targets, threshold, 'all')
    names = base.threshold_names(threshold, 'all')
    rows = [((f'{n} (line ${ln:,.0f})' if ln is not None else n)
             + (', millions' if k == 'units' else ', $ trillion'),
             {BASES[b]: tv[b][i] / (1e6 if k == 'units' else 1e12) for b in BASES})
            for i, (n, ln, k) in enumerate(names)]
    text += ['## 1. The eight wealth targets (2015 dollars)', '',
             md(rows, list(BASES.values()), {'*': 'pct2'}), '']
    for b in BASES:
        c = base.consistency(names, tv[b])
        text += [f'### Consistency checks, {BASES[b]}', '', c.to_markdown(index=False), '']
    # mean net worth above each line: implied by the targets, and after each arm's tilt
    rows = []
    for ln in sorted({ln for _, ln, k in names if ln is not None and k == 'net worth'}):
        ic = [i for i, (_, l2, k) in enumerate(names) if l2 == ln and k == 'units'][0]
        ia = [i for i, (_, l2, k) in enumerate(names) if l2 == ln and k == 'net worth'][0]
        r = {f'target, {BASES[b]}': tv[b][ia] / tv[b][ic] for b in BASES}
        for a in arms:
            r[f'{a}, after'] = E[a]['posttrain'][ia] / E[a]['posttrain'][ic]
        rows.append((f'above ${ln:,.0f}', r))
    text += ['### Mean net worth of the units above each line ($, 2015 dollars)', '',
             md(rows, list(rows[0][1]), {'*': 'dollar'}), '']

    # 2. misses of each arm against the targets it was tilted to
    rows8, csv = [], []
    for i, (n, ln, _) in enumerate(names):
        r = {}
        for a in arms:
            e = E[a]
            for k in ['pretrain', 'posttrain']:
                r[f'{a}: {k}'] = 100 * (e[k][i] / e['target'][i] - 1)
            csv.append({'arm': a, 'count base': ARMS[a]['base'], 'target': n, 'line': ln,
                        'target value': e['target'][i], 'pretrain': e['pretrain'][i],
                        'after 136 alone': e['after 136'][i], 'posttrain': e['posttrain'][i]})
        rows8.append((n, r))
    cols = [f'{a}: {k}' for a in arms for k in ['pretrain', 'posttrain']]
    text += ['## 2. The eight targets: miss (%) against the targets each arm was tilted to', '',
             md(rows8, cols, {'*': 'pct2'}), '']
    pd.DataFrame(csv).to_csv(f'{OUT}/eight_targets.csv', index=False)

    # 3. the band between $1 million (2019 dollars) and the SCF's 90th percentile
    q90 = dollars.wquantile(scf_x, scf_w, [0.90])[0]
    lo = threshold
    scf_share = 100 * scf_w[(scf_x > lo) & (scf_x <= q90)].sum() / scf_w.sum()
    rows = []
    for a in arms:
        e = E[a]
        inb = (e['x'] > lo) & (e['x'] <= q90)
        f = e['w2'] / e['w0']
        rows.append((a, {'records in the band': int(inb.sum()),
                         'share before the tilt (%)': 100 * e['w0'][inb].sum() / e['w0'].sum(),
                         'share after the tilt (%)': 100 * e['w2'][inb].sum() / e['w2'].sum(),
                         'records in the band with weight factor below 0.001': int((inb & (f < 1e-3)).sum()),
                         'smallest weight factor in the band': float(f[inb].min()),
                         'median weight factor in the band': float(np.median(f[inb]))}))
    text += [f'## 3. Net worth between ${lo:,.0f} and ${q90:,.0f} (2015 dollars); SCF 2019: {scf_share:.2f}% of units', '',
             md(rows, list(rows[0][1]), {'*': 'g'}), '']

    # 4. move and effective sample
    keys = list(N[arms[0]]['move'])
    rows = [(k, {a: N[a]['move'][k] for a in arms}) for k in keys]
    text += ['## 4. The move and the effective sample kept', '',
             md(rows, arms, {'*': 'nats', 'effective sample kept, 136 alone (%)': 'pct',
                             'effective sample kept, 136 + wealth (%)': 'pct',
                             'records below 0.1x start weight, 136 + wealth': 'int'}), '']

    # 5. wealth before and after
    scf = wt.unconditional(scf_x, scf_w)
    stats = ['units (millions)', 'total net worth ($ trillion)', 'mean ($)', 'median ($)', 'share negative (%)',
             'top 10% share (%)', 'top 1% share (%)', 'top 0.1% share (%)', '90th percentile ($)',
             '99th percentile ($)', '99.9th percentile ($)']
    stats = [s for s in stats if s in scf]
    for stage in ['pretrain', 'posttrain']:
        cols = ['SCF 2019'] + arms
        rows = [(s, {'SCF 2019': scf[s], **{a: N[a]['wealth'][stage][s] for a in arms}}) for s in stats]
        kinds = {'*': 'dollar', 'units (millions)': 'g', 'total net worth ($ trillion)': 'g',
                 **{s: 'pct' for s in stats if '(%)' in s}}
        text += [f'## 5{"a" if stage == "pretrain" else "b"}. Net worth, {stage} (2015 dollars)', '',
                 md(rows, cols, kinds), '']
    tg = list(N[arms[0]]['wealth_targets'])
    for k in ['pretrain miss (%)', 'after the 136 alone (%)', 'after the tilt (%)']:
        rows = [(t_, {a: N[a]['wealth_targets'][t_][k] for a in arms}) for t_ in tg]
        text += [f'### Rank-form wealth statistics against the 2019 SCF: {k}', '', md(rows, arms, {'*': 'pct'}), '']

    # 6. by wage group
    sc = wt.conditional(scf_u, scf_x, scf_w)
    sc = sc[sc['grouping'] == 'wages'].set_index('group')
    for stat, kind in [('mean ($)', 'dollar'), ('median ($)', 'dollar'), ('share above $1m (%)', 'pct'),
                       ('share of units (%)', 'pct')]:
        for stage in ['pretrain', 'posttrain']:
            cols = ['SCF 2019'] + arms
            rows = [(gname, {'SCF 2019': sc.loc[gname, stat],
                             **{a: N[a]['by_wage'][stage].loc[gname, stat] for a in arms}})
                    for gname in wt.ORDER['wages']]
            text += [f'## 6. Net worth by wage group, {stat}, {stage}', '', md(rows, cols, {'*': kind}), '']

    # 7. holders and joint
    rows = []
    for item in ['interest', 'dividends', 'capital_gains']:
        for stage in ['pretrain', 'posttrain']:
            rows.append((f'{item}, {stage}', {a: N[a]['holders'].loc[item, stage] for a in arms}
                         | {'PUF 2015': N[arms[0]]['holders'].loc[item, 'PUF 2015 (weighted)'],
                            'IRS 2019': N[arms[0]]['holders'].loc[item, 'IRS 2019']}))
    for stage in ['pretrain', 'posttrain']:
        rows.append((f'joint returns, {stage}', {a: N[a]['joint'][stage] for a in arms}))
    text += ['## 7. Holder shares and joint returns (% of returns)', '',
             md(rows, arms + ['PUF 2015', 'IRS 2019'], {'*': 'pct2'}), '']

    # 8. redraw
    keys = list(N[arms[0]]['redraw'])
    rows = [(k, {a: N[a]['redraw'][k] for a in arms}) for k in keys]
    text += ['## 8. The redraw (200,000 equal-weight records)', '',
             md(rows, arms, {'*': 'pct2', 'IRS targets within 2 standard errors': 'int',
                             'targets with a standard error': 'int', 'distinct records': 'int',
                             'largest |gap| after the tilt, 136 targets (%)': 'g'}), '']
    for a in arms:
        rd = pd.read_csv(f"{have[a]['tilt']}/redraw_{have[a]['tag']}.csv")
        bad = rd[rd['z'].abs() > 2][['bracket', 'cell', 'value', 'redrawn', 'gap_pct', 'z']]
        text += [f'### {a}: redraw cells beyond 2 standard errors', '', bad.to_markdown(index=False), '']
        lines = f"{have[a]['tilt']}/wealth_lines_{have[a]['tag']}.csv"
        if os.path.exists(lines):
            text += [f'### {a}: the eight targets on the redraw', '',
                     pd.read_csv(lines)[['target', 'target value', 'redraw', 'redraw miss %']].to_markdown(index=False), '']

    # 9. dependents and the untargeted relationships
    u0 = N[arms[0]]['untargeted']

    def pick(u, rel, grp):
        r = u[(u['relationship'] == rel) & (u['group'] == grp)]
        assert len(r) == 1, (rel, grp)
        return r.iloc[0]

    def nj2(u, col):
        return 100 * (pick(u, 'share with 2 dependents, by joint', 'not joint')[col]
                      + pick(u, 'share with 3+ dependents, by joint', 'not joint')[col])
    rows = []
    for stage in ['pretrain', 'posttrain']:
        rows.append((f'non-joint with 2+ dependents, {stage}', {a: nj2(N[a]['untargeted'], stage) for a in arms}
                     | {'PUF 2015': nj2(u0, 'actual 2015 PUF')}))
    for k in range(4):
        for grp in ['joint', 'not joint']:
            rel = f'share with {k}{"+" if k == 3 else ""} dependents, by joint'
            rows.append((f'{grp}, {k}{"+" if k == 3 else ""} dependents, posttrain',
                         {a: 100 * pick(N[a]['untargeted'], rel, grp)['posttrain'] for a in arms}
                         | {'PUF 2015': 100 * pick(u0, rel, grp)['actual 2015 PUF']}))
    other = u0['relationship'].str.startswith('any')
    for stage in ['pretrain', 'posttrain']:
        rows.append((f'dividend and capital gains cells ({int(other.sum())}): mean |gap| to PUF, {stage} (points)',
                     {a: 100 * np.mean(np.abs(N[a]['untargeted'].loc[other, stage].to_numpy()
                                              - u0.loc[other, 'actual 2015 PUF'].to_numpy())) for a in arms}))
        rows.append((f'dividend and capital gains cells: largest |gap| to PUF, {stage} (points)',
                     {a: 100 * np.max(np.abs(N[a]['untargeted'].loc[other, stage].to_numpy()
                                             - u0.loc[other, 'actual 2015 PUF'].to_numpy())) for a in arms}))
    text += ['## 9. Dependents and the relationships no target covers (% of returns)', '',
             md(rows, arms + ['PUF 2015'], {'*': 'pct'}), '']
    full = []
    for a in arms:
        u = N[a]['untargeted'][['relationship', 'group', 'actual 2015 PUF', 'pretrain', 'posttrain']].copy()
        u.insert(0, 'arm', a)
        full.append(u)
    pd.concat(full).to_csv(f'{OUT}/untargeted_by_arm.csv', index=False)

    # 10. stack against plain
    for a in arms:
        if a in ('v4b, IRS base', 'v4c, SCF base'):
            continue
        keys = list(N[a]['plain']['stack'])
        rows = [(k, {c: N[a]['plain'][c][k] for c in ['plain', 'plain2', 'stack']}) for k in keys]
        for _, r in rows:
            r['stack minus plain'] = r['stack'] - r['plain']
            r['second plain seed minus plain'] = r['plain2'] - r['plain']
        text += [f'## 10. {a}: the stack (pretrain weights) against its plain files (seed 3, seed 35)', '',
                 md(rows, ['plain', 'plain2', 'stack', 'stack minus plain', 'second plain seed minus plain'],
                    {'*': 'g'}), '']

    open(f'{OUT}/report_tables.md', 'w').write('\n'.join(text) + '\n')
    print('\n'.join(text))


if __name__ == '__main__':
    main()
