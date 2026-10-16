"""
20_report.py  (minimal example, step 20: the side-by-side tables for arms A, B and C)

Reads the stacks, tilts and held-back scores of 20_SEPARATE_MODELS.md and
writes the tables quoted there. Every number is computed the same way for
every arm, from the arm's stack (dollars from the V4 maps with 04_dollars'
fixed seed, as 07_baseline does) and its weights file:

  A   v4b as it is: gen/gen_puf_stack_v4b.parquet, tilt/v4b_allunits/
  B   separate models: gen/S20/seed1/, tilt/S20/seed1/
  C   the joint model drawn PUF variables first: gen/C20/seed1/, tilt/C20/seed1/
  and, from the second training seed: A (gen/C20/A_seed2), B (gen/S20/seed2),
  C (gen/C20/seed2).
  B long, B long seed 2: arm B with B2 trained for 516 passes, as many
  gradient steps as V4 (gen/S20/long_seed1, long_seed2; tilt/S20/long_seed*).

Writes checks/S20/report_tables.md (all tables), checks/S20/*.csv, and
checks/C20/order_tables.md (the A-against-C tables only).
"""
import importlib.util
import os

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
os.environ.setdefault('MINIMAL_MAP_DIR', f'{ROOT}/map/V4')


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


wt = load('wt', '06_wealth_tables.py')
dollars, tiltmod = wt.dollars, wt.tiltmod
WAGE_2018_TO_2015 = 1.1802474966 / 1.2493          # 07_baseline.WAGE_2018_TO_2015

ARMS = {
    'A': dict(stack=f'{ROOT}/gen/gen_puf_stack_v4b.parquet', tilt=f'{ROOT}/tilt/v4b_allunits', tag='v4b',
              plain=f'{ROOT}/gen/V4b/gen_puf_with_net_worth_ownerlabel_absent.parquet',
              plain2=f'{ROOT}/gen/gen_plain_seed35_v4b.parquet'),
    'B': dict(stack=f'{ROOT}/gen/S20/seed1/gen_puf_stack_s20.parquet', tilt=f'{ROOT}/tilt/S20/seed1', tag='s20',
              plain=f'{ROOT}/gen/S20/seed1/gen_plain_s20.parquet',
              plain2=f'{ROOT}/gen/S20/seed1/gen_plain_seed35_s20.parquet'),
    'C': dict(stack=f'{ROOT}/gen/C20/seed1/gen_puf_stack_c20.parquet', tilt=f'{ROOT}/tilt/C20/seed1', tag='c20',
              plain=f'{ROOT}/gen/C20/seed1/gen_plain_c20.parquet',
              plain2=f'{ROOT}/gen/C20/seed1/gen_plain_seed35_c20.parquet'),
    'A seed 2': dict(stack=f'{ROOT}/gen/C20/A_seed2/gen_puf_stack_a20s2.parquet', tilt=f'{ROOT}/tilt/C20/A_seed2',
                     tag='a20s2', plain=f'{ROOT}/gen/C20/A_seed2/gen_plain_a20s2.parquet',
                     plain2=f'{ROOT}/gen/C20/A_seed2/gen_plain_seed35_a20s2.parquet'),
    'B seed 2': dict(stack=f'{ROOT}/gen/S20/seed2/gen_puf_stack_s20s2.parquet', tilt=f'{ROOT}/tilt/S20/seed2',
                     tag='s20s2', plain=f'{ROOT}/gen/S20/seed2/gen_plain_s20s2.parquet',
                     plain2=f'{ROOT}/gen/S20/seed2/gen_plain_seed35_s20s2.parquet'),
    'B long': dict(stack=f'{ROOT}/gen/S20/long_seed1/gen_puf_stack_s20l.parquet', tilt=f'{ROOT}/tilt/S20/long_seed1',
                   tag='s20l', plain=f'{ROOT}/gen/S20/long_seed1/gen_plain_s20l.parquet',
                   plain2=f'{ROOT}/gen/S20/long_seed1/gen_plain_seed35_s20l.parquet'),
    'B long seed 2': dict(stack=f'{ROOT}/gen/S20/long_seed2/gen_puf_stack_s20ls2.parquet',
                          tilt=f'{ROOT}/tilt/S20/long_seed2', tag='s20ls2',
                          plain=f'{ROOT}/gen/S20/long_seed2/gen_plain_s20ls2.parquet',
                          plain2=f'{ROOT}/gen/S20/long_seed2/gen_plain_seed35_s20ls2.parquet'),
    'C seed 2': dict(stack=f'{ROOT}/gen/C20/seed2/gen_puf_stack_c20s2.parquet', tilt=f'{ROOT}/tilt/C20/seed2',
                     tag='c20s2', plain=f'{ROOT}/gen/C20/seed2/gen_plain_c20s2.parquet',
                     plain2=f'{ROOT}/gen/C20/seed2/gen_plain_seed35_c20s2.parquet'),
}


def fmt(x, kind):
    if isinstance(x, str):
        return x
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return ''
    if kind == 'pct':
        return f'{x:.1f}'
    if kind == 'pct2':
        return f'{x:.2f}'
    if kind == 'dollar':
        return f'{x:,.0f}' if abs(x) < 1e4 else f'{float(f"{x:.3g}"):,.0f}'
    if kind == 'nats':
        return f'{x:.4f}'
    if kind == 'int':
        return f'{int(round(x)):,}'
    return f'{x:.3g}'


def md(rows, cols, kinds):
    """rows: list of (label, {col: value}); kinds: {row label or '*': kind}."""
    out = ['| | ' + ' | '.join(cols) + ' |', '|---|' + '---:|' * len(cols)]
    for label, vals in rows:
        k = kinds.get(label, kinds.get('*'))
        out.append(f'| {label} | ' + ' | '.join(fmt(vals.get(c, np.nan), k) for c in cols) + ' |')
    return '\n'.join(out)


def scf_reference():
    u, x, w, _ = wt.scf2019('all')
    u = u.assign(net_worth=x, wages=u['wages'] * WAGE_2018_TO_2015,
                 dividends=u['dividends'] * WAGE_2018_TO_2015)
    return u, x, w


def kl(w0, w1):
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    pos = p1 > 0
    return float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos])))


def ess(w):
    return w.sum() ** 2 / (w ** 2).sum()


def arm_numbers(name, a, scf):
    out = {}
    stack = pd.read_parquet(a['stack'])
    assert (stack['source'] == 'puf').all()
    g15, _ = dollars.draw_dollars(stack, dollars.puf_maps(2015))
    g19, _ = dollars.draw_dollars(stack, dollars.puf_maps(2019))
    W = pd.read_parquet(f"{a['tilt']}/weights_{a['tag']}.parquet")
    assert len(W) == len(stack) and np.array_equal(W['record_id'].to_numpy(), np.arange(len(stack)))
    assert (W['stratifier'].to_numpy() == stack['stratifier'].to_numpy()).all()
    w0, w1, w2 = (W[c].to_numpy(float) for c in ['weight_before', 'weight_after_136', 'weight_after'])
    x = g15['net_worth'].to_numpy(float)
    # move
    out['move'] = {'136 targets alone (nats)': kl(w0, w1), '136 + wealth (nats)': kl(w0, w2),
                   'effective sample kept, 136 alone (%)': 100 * ess(w1) / ess(w0),
                   'effective sample kept, 136 + wealth (%)': 100 * ess(w2) / ess(w0),
                   'records below 0.1x start weight, 136 + wealth': int((w2 / w0 < 0.1).sum()),
                   'wealth cost on top of the 136 (nats)': kl(w0, w2) - kl(w0, w1)}
    # redraw
    rd = pd.read_csv(f"{a['tilt']}/redraw_{a['tag']}.csv")
    wtg = pd.read_csv(f"{a['tilt']}/wealth_targets_{a['tag']}.csv").set_index('target')
    worst = rd.loc[rd['z'].abs().idxmax()]
    out['redraw'] = {'IRS targets within 2 standard errors': int((rd['z'].abs() <= 2).sum()),
                     'targets with a standard error': int(rd['z'].notna().sum()),
                     'largest |z|': float(rd['z'].abs().max()),
                     'worst cell': f"{worst['bracket']}, {worst['cell']}",
                     'total net worth miss on the redraw (%)': float(wtg.loc['total', 'redraw miss %']),
                     'top 1% net worth miss on the redraw (%)': float(wtg.loc['top 1%', 'redraw miss %']),
                     'distinct records': int(rd['distinct_records'].iloc[0])}
    fit = pd.read_csv(f"{a['tilt']}/fit_{a['tag']}.csv")
    out['redraw']['largest |gap| after the tilt, 136 targets (%)'] = float(fit['gap_after_pct'].abs().max())
    # wealth targets before the tilt (rank form, against the SCF), and after the 136 alone
    out['wealth_targets'] = {t: {'pretrain miss (%)': float(wtg.loc[t, 'pretrain miss %']),
                                 'after the 136 alone (%)': float(wtg.loc[t, 'posttrain 136 only miss %']),
                                 'after the tilt (%)': float(wtg.loc[t, 'posttrain miss %'])}
                             for t in wtg.index}
    # wealth, unconditional
    out['wealth'] = {'pretrain': wt.unconditional(x, w0), 'posttrain': wt.unconditional(x, w2)}
    # by wage group
    out['by_wage'] = {}
    for k, w in [('pretrain', w0), ('posttrain', w2)]:
        c = wt.conditional(g15, x, w)
        out['by_wage'][k] = c[c['grouping'] == 'wages'].set_index('group')
    # holders and joint share
    h = pd.read_csv(f"{a['tilt']}/holders_{a['tag']}.csv").set_index('item')
    out['holders'] = h
    mar = g19['married'].to_numpy()
    out['joint'] = {'pretrain': 100 * w0[mar == 1].sum() / w0.sum(),
                    'posttrain': 100 * w2[mar == 1].sum() / w2.sum()}
    # untargeted relationships
    u = pd.read_csv(f"{a['tilt']}/untargeted_{a['tag']}.csv")
    out['untargeted'] = u
    # plain files: the stack against its own plain file and a second plain seed
    pl = {}
    for k in ['plain', 'plain2']:
        f = pd.read_parquet(a[k])
        f = f[f['source'] == 'puf'].reset_index(drop=True)
        d, _ = dollars.draw_dollars(f, dollars.puf_maps(2015))
        pl[k] = (d, np.ones(len(d)))
    pl['stack'] = (g15, w0)
    out['plain'] = {k: plain_stats(d, w) for k, (d, w) in pl.items()}
    return out


def nonjoint2(d, w):
    nj = d['married'].to_numpy() == 0
    return 100 * w[nj & (d['n_dep'].to_numpy() >= 2)].sum() / w[nj].sum()


def plain_stats(d, w):
    x = d['net_worth'].to_numpy(float)
    u = wt.unconditional(x, w)
    s = {'mean net worth ($)': u['mean ($)'], 'median net worth ($)': u['median ($)'],
         'top 1% share (%)': u['top 1% share (%)'], 'top 0.1% share (%)': u['top 0.1% share (%)'],
         'joint (%)': 100 * w[d['married'].to_numpy() == 1].sum() / w.sum(),
         'non-joint with 2+ dependents (%)': nonjoint2(d, w)}
    for v in ['interest', 'dividends', 'capital_gains']:
        s[f'holders: {v} (%)'] = 100 * w[d[v].to_numpy(float) != 0].sum() / w.sum()
    c = wt.conditional(d, x, w)
    c = c[c['grouping'] == 'wages'].set_index('group')
    for gname in ['0', '25-50k', '200k+']:
        s[f'mean net worth, wages {gname} ($)'] = c.loc[gname, 'mean ($)']
    return s


def heldback_tables():
    path = f'{ROOT}/checks/S20/heldback.csv'
    if not os.path.exists(path):
        return ['(held-back scores not yet computed)']
    h = pd.read_csv(path)
    dif = pd.read_csv(path.replace('.csv', '_differences.csv'))
    text = []
    for score, title in [('one', 'only that variable hidden'), ('chain', 'steps of the whole-record chain'),
                         ('all', 'every slot hidden')]:
        for s, sep in [('puf', 'B1'), ('scf', 'B2')]:
            sub = h[(h['score'] == score) & (h['source'] == s)]
            models = [mm for mm in ['V4', sep, f'{sep}_long', 'V4_s2', f'{sep}_s2', f'{sep}_long_s2']
                      if mm in set(sub['model'])]
            vars_ = list(dict.fromkeys(sub['variable']))
            rows = []
            for v in vars_:
                r = {mm: float(sub[(sub['model'] == mm) & (sub['variable'] == v)]['nats'].iloc[0]) for mm in models}
                for mm in models:
                    if mm.startswith('V4'):
                        continue
                    dd = dif[(dif['model'] == mm) & (dif['source'] == s) & (dif['score'] == score)
                             & (dif['variable'] == v)]
                    if len(dd):
                        r[f'{mm} minus {dd["against"].iloc[0]}'] = float(dd['difference (nats)'].iloc[0])
                        r[f'se ({mm})'] = float(dd['se of difference'].iloc[0])
                if 'V4_s2' in r:
                    r['V4 seed 2 minus V4'] = r['V4_s2'] - r['V4']
                if f'{sep}_s2' in r:
                    r[f'{sep} seed 2 minus {sep}'] = r[f'{sep}_s2'] - r[sep]
                rows.append((v, r))
            cols = list(dict.fromkeys(k for _, r in rows for k in r))
            text += [f'### Held-back cross-entropy (nats), {s.upper()} records, {title}', '',
                     md(rows, cols, {'*': 'nats'}), '']
    return text


def main():
    os.makedirs(f'{ROOT}/checks/S20', exist_ok=True)
    os.makedirs(f'{ROOT}/checks/C20', exist_ok=True)
    scf_u, scf_x, scf_w = scf_reference()
    have = {k: a for k, a in ARMS.items()
            if os.path.exists(a['stack']) and os.path.exists(f"{a['tilt']}/weights_{a['tag']}.parquet")}
    print('arms available:', list(have), flush=True)
    N = {k: arm_numbers(k, a, None) for k, a in have.items()}
    arms = list(N)
    text, order_text = [], []

    # 1. move
    keys = list(next(iter(N.values()))['move'])
    rows = [(k, {a: N[a]['move'][k] for a in arms}) for k in keys]
    t = ['## 1. The move and the effective sample kept', '',
         md(rows, arms, {'*': 'nats', 'effective sample kept, 136 alone (%)': 'pct',
                         'effective sample kept, 136 + wealth (%)': 'pct',
                         'records below 0.1x start weight, 136 + wealth': 'int'}), '']
    text += t
    order_text += t

    # 2. redraw
    keys = list(next(iter(N.values()))['redraw'])
    rows = [(k, {a: N[a]['redraw'][k] for a in arms}) for k in keys]
    t = ['## 2. The redraw (200,000 equal-weight records)', '',
         md(rows, arms, {'*': 'pct2', 'IRS targets within 2 standard errors': 'int',
                         'targets with a standard error': 'int', 'distinct records': 'int',
                         'largest |gap| after the tilt, 136 targets (%)': 'g'}), '']
    text += t
    order_text += t

    # 3. wealth, unconditional
    scf = wt.unconditional(scf_x, scf_w)
    stats = ['units (millions)', 'total net worth ($ trillion)', 'mean ($)', 'median ($)', 'share negative (%)',
             'top 10% share (%)', 'top 1% share (%)', 'top 0.1% share (%)', '99th percentile ($)',
             '99.9th percentile ($)']
    for stage in ['pretrain', 'posttrain']:
        cols = ['SCF 2019'] + arms
        rows = [(s, {'SCF 2019': scf[s], **{a: N[a]['wealth'][stage][s] for a in arms}}) for s in stats]
        rows.append(('total, ratio to the SCF', {'SCF 2019': 1.0, **{a: N[a]['wealth'][stage]['total net worth ($ trillion)']
                                                                      / scf['total net worth ($ trillion)'] for a in arms}}))
        kinds = {'*': 'dollar', 'units (millions)': 'g', 'total net worth ($ trillion)': 'g',
                 'total, ratio to the SCF': 'g', **{s: 'pct' for s in stats if '(%)' in s}}
        t = [f'## 3{"a" if stage == "pretrain" else "b"}. Net worth, {stage} (2015 dollars)', '',
             md(rows, cols, kinds), '']
        text += t
        order_text += t
    # wealth targets before the tilt
    tg = list(next(iter(N.values()))['wealth_targets'])
    for k in ['pretrain miss (%)', 'after the 136 alone (%)']:
        rows = [(t_, {a: N[a]['wealth_targets'][t_][k] for a in arms}) for t_ in tg]
        text += [f'### Wealth targets (rank form), miss against the 2019 SCF: {k}', '', md(rows, arms, {'*': 'pct'}), '']

    # 4. by wage group
    groups = wt.ORDER['wages']
    sc = wt.conditional(scf_u, scf_x, scf_w)
    sc = sc[sc['grouping'] == 'wages'].set_index('group')
    for stat, kind in [('mean ($)', 'dollar'), ('median ($)', 'dollar'), ('share above $1m (%)', 'pct'),
                       ('share of units (%)', 'pct')]:
        for stage in ['pretrain', 'posttrain']:
            cols = ['SCF 2019'] + arms
            rows = [(gname, {'SCF 2019': sc.loc[gname, stat], **{a: N[a]['by_wage'][stage].loc[gname, stat] for a in arms}})
                    for gname in groups]
            t = [f'## 4. Net worth by wage group, {stat}, {stage}', '', md(rows, cols, {'*': kind}), '']
            text += t
            order_text += t

    # 5. holders and joint
    rows = []
    for item in ['interest', 'dividends', 'capital_gains']:
        for stage in ['pretrain', 'posttrain']:
            rows.append((f'{item}, {stage}', {a: N[a]['holders'].loc[item, stage] for a in arms}
                         | {'PUF 2015': N[arms[0]]['holders'].loc[item, 'PUF 2015 (weighted)'],
                            'IRS 2019': N[arms[0]]['holders'].loc[item, 'IRS 2019']}))
    for stage in ['pretrain', 'posttrain']:
        rows.append((f'joint returns, {stage}', {a: N[a]['joint'][stage] for a in arms}))
    t = ['## 5. Holder shares and joint returns (% of returns)', '',
         md(rows, arms + ['PUF 2015', 'IRS 2019'], {'*': 'pct2'}), '']
    text += t
    order_text += t

    # 6. untargeted relationships
    u0 = N[arms[0]]['untargeted']
    keyrows = []
    for a in arms:
        u = N[a]['untargeted']
        assert (u['group'].to_numpy() == u0['group'].to_numpy()).all()
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
    # summary of all the other cells: mean absolute gap to the PUF, percentage points
    other = u0['relationship'].str.startswith('any')
    for stage in ['pretrain', 'posttrain']:
        rows.append((f'dividend and capital gains cells ({int(other.sum())}): mean |gap| to PUF, {stage} (points)',
                     {a: 100 * np.mean(np.abs(N[a]['untargeted'].loc[other, stage].to_numpy()
                                              - u0.loc[other, 'actual 2015 PUF'].to_numpy())) for a in arms}))
    t = ['## 6. Relationships no target covers (% of returns, 2019 dollars)', '',
         md(rows, arms + ['PUF 2015'], {'*': 'pct'}), '']
    text += t
    order_text += t
    full = []
    for a in arms:
        u = N[a]['untargeted'][['relationship', 'group', 'actual 2015 PUF', 'pretrain', 'posttrain']].copy()
        u.insert(0, 'arm', a)
        full.append(u)
    pd.concat(full).to_csv(f'{ROOT}/checks/S20/untargeted_all_arms.csv', index=False)

    # 7. stack against plain
    keys = list(N[arms[0]]['plain']['stack'])
    for a in arms:
        rows = [(k, {c: N[a]['plain'][c][k] for c in ['plain', 'plain2', 'stack']}) for k in keys]
        for _, r in rows:
            r['stack minus plain'] = r['stack'] - r['plain']
            r['second plain seed minus plain'] = r['plain2'] - r['plain']
        text += [f'## 7. Arm {a}: the stack (pretrain weights) against its plain files (seed 3, seed 35)', '',
                 md(rows, ['plain', 'plain2', 'stack', 'stack minus plain', 'second plain seed minus plain'],
                    {'*': 'g'}), '']

    # 8. held-back
    text += ['## 8. Held-back loss, separate models against the joint model V4', ''] + heldback_tables()

    open(f'{ROOT}/checks/S20/report_tables.md', 'w').write('\n'.join(text) + '\n')
    open(f'{ROOT}/checks/C20/order_tables.md', 'w').write('\n'.join(order_text) + '\n')
    print('\n'.join(text))


if __name__ == '__main__':
    main()
