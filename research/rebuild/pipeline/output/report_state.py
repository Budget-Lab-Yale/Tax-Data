"""The state weights' report: tables in levels from a finished state weights folder, written to <out>/out/tables.md
with the CSVs behind them.
"""
import argparse
import glob
import json
import os

import numpy as np
import pandas as pd

from reweight import state_targets as ST

ITEM_NAMES = {'n_bus': 'Schedule C, returns', 'bus_amt': 'Schedule C, net income', 'n_pens': 'Taxable pensions, returns',
              'pens_amt': 'Taxable pensions', 'n_ss': 'Taxable Social Security, returns', 'ss_amt': 'Taxable Social Security',
              'n_fedtax': 'Income tax after credits, returns', 'fedtax_amt': 'Income tax after credits',
              'kg_amt_pos': 'Capital gains, positive part'}
FLOAT = '{:,.1f}'.format


def md(d, floatfmt=FLOAT):
    head = '| ' + ' | '.join(map(str, d.columns)) + ' |\n|' + '|'.join('---' if not np.issubdtype(t, np.number) else '---:' for t in d.dtypes) + '|\n'
    rows = ''
    for _, r in d.iterrows():
        rows += '| ' + ' | '.join(floatfmt(x) if isinstance(x, (float, np.floating)) else str(x) for x in r) + ' |\n'
    return head + rows


def summaries(out):
    return pd.DataFrame([json.load(open(f)) for f in glob.glob(f'{out}/fits/y*/*/summary.json')])


def grid_table(s):
    c = s[(s['items'] == 'comparison') & s['tilted']]
    t = c.sort_values(['year', 'start', 'tolerance', 'tau'])[
        ['year', 'start', 'tolerance', 'tau', 'within_2pct', 'plain_targeted', 'newton_largest_group', 'newton_all_groups', 'all_converged',
         'move_nats', 'heldout plain', 'heldout dollar_weighted', 'heldout return_weighted', 'smallest_state_effective_records',
         'records_over_99pct_in_one_area']].copy()
    t['tolerance'] = (100 * t['tolerance']).map('{:g}%'.format)
    t['tau'] = t['tau'].map('{:g}'.format)
    t['records_over_99pct_in_one_area'] = 100 * t['records_over_99pct_in_one_area']
    return t.rename(columns={'within_2pct': 'targeted cells within 2% (%)', 'plain_targeted': 'targeted average miss (%)',
                             'newton_largest_group': 'Newton steps, largest group', 'newton_all_groups': 'Newton steps, all groups',
                             'all_converged': 'converged', 'move_nats': 'move (nats)', 'heldout plain': 'held-out average miss (%)',
                             'heldout dollar_weighted': 'held-out dollar-weighted miss (%)', 'heldout return_weighted': 'held-out return-weighted miss (%)',
                             'smallest_state_effective_records': 'smallest state, effective records',
                             'records_over_99pct_in_one_area': 'records over 99% in one area (%)'})


def four_starts(s, tolerance, tau):
    """The held-out table: the four starts on 2019 and 2022, tilted at one setting."""
    rows = []
    for y in (2019, 2022):
        for name, st, tilted in [("Iselin's start, untilted", 'iselin', False), ("Iselin's start, tilted", 'iselin', True),
                                 ('CPS start, untilted', 'cps', False), ('CPS start, tilted', 'cps', True)]:
            c = s[(s['year'] == y) & (s['start'] == st) & (s['tilted'] == tilted) & (s['items'] == 'comparison')]
            if tilted:
                c = c[(c['tolerance'] == tolerance) & (c['tau'] == tau)]
            c = c.iloc[0]
            rows.append({'year': y, 'start': name, 'targeted cells within 2% (%)': c['within_2pct'], 'held-out average miss (%)': c['heldout plain'],
                         'held-out dollar-weighted miss (%)': c['heldout dollar_weighted'], 'held-out return-weighted miss (%)': c['heldout return_weighted'],
                         **{f'{ITEM_NAMES[v]} ({"%" if True else ""})': c[f'heldout plain {v}'] for v in ST.HELD_OUT}})
    return pd.DataFrame(rows)


def levels(run, out, year, items=('n_returns', 'agi_amt', 'wages_amt', 'fedtax_amt', 'ss_amt', 'bus_amt', 'kg_amt', 'pens_amt')):
    """Historic Table 2 against the file on the CPS start and on the final shares, by area and by area and AGI group.
    Returns in thousands, dollars in billions."""
    from reweight import state_weights as SW
    df = ST.read_file(f'{run}/{year}/tilt/y{year}/file_{year}.parquet')
    filer = df['filer'].to_numpy() == 1
    f = df[filer].reset_index(drop=True)
    group = ST.agi_group(f['calc_agi'])
    ht2 = ST.read_ht2(year)
    areas = ST.areas(year)
    z = np.load(f'{out}/shares/shares_{year}.npz')
    assert (z['record_id'] == df['record_id'].to_numpy()).all()
    P1 = z['P'][filer]
    pi = SW.read_start(f'{out}/starts/start_{year}.npz', df)[filer]
    P0 = SW.cps_start(pi, group, ht2, areas)
    pin = SW.pins_of(f, year, areas)
    P0[pin >= 0] = 0.0
    P0[np.flatnonzero(pin >= 0), pin[pin >= 0]] = 1.0
    x = ST.item_vectors(f, list(items))
    w = f['weight'].to_numpy(float)
    by_area, by_group = [], []
    for v in items:
        vals = ST.ht2_values(ht2, v)
        if vals is None:
            continue
        unit = 1e3 if ST.ITEMS[v][2] == 'count' else 1e9
        name = 'thousand returns' if unit == 1e3 else '$ billion'
        r0 = np.zeros((10, len(areas)))
        r1 = np.zeros((10, len(areas)))
        for g in range(1, 11):
            s_ = group == g
            r0[g - 1] = (w[s_] * x[v][s_]) @ P0[s_]
            r1[g - 1] = (w[s_] * x[v][s_]) @ P1[s_]
        irs = vals[areas].to_numpy(float)
        by_area.append(pd.DataFrame({'year': year, 'item': v, 'unit': name, 'area': areas, 'IRS': irs.sum(0) / unit, 'file, CPS start': r0.sum(0) / unit,
                                     'file, final shares': r1.sum(0) / unit}))
        by_group.append(pd.DataFrame({'year': year, 'item': v, 'unit': name, 'group': ST.GROUP_NAMES,
                                      'average miss across areas, CPS start (%)': 100 * np.nanmean(np.abs(r0 / np.where(irs == 0, np.nan, irs * (r0.sum(1, keepdims=True) / irs.sum(1, keepdims=True))) - 1), axis=1),
                                      'average miss across areas, final shares (%)': 100 * np.nanmean(np.abs(r1 / np.where(irs == 0, np.nan, irs * (r1.sum(1, keepdims=True) / irs.sum(1, keepdims=True))) - 1), axis=1)}))
    return pd.concat(by_area, ignore_index=True), pd.concat(by_group, ignore_index=True)


def nonfilers(out, year, tag='final'):
    """The non-filers by state, in thousands of adults: the parts of the residual, the file's non-filer adults on the
    start and on the final shares, and the target. Totals are in the last row."""
    t = pd.read_csv(f'{out}/fits/y{year}/{tag}/nonfiler_states.csv', index_col=0)
    f = pd.read_csv(f'{out}/fits/y{year}/{tag}/nonfiler_fit.csv').groupby('state')[['target', 'start', 'final']].sum()
    d = t.join(f)[['cps_adults', 'irs_adults', 'dependent_returns', 'residual', 'target', 'start', 'final']] / 1e3
    tot = d.sum().to_frame('US').T
    return pd.concat([d, tot]).reset_index(names='state').rename(columns={'cps_adults': 'CPS adults', 'irs_adults': 'adults on returns', 'dependent_returns': 'dependent returns',
                                                                         'target': 'file: target', 'start': 'file: start', 'final': 'file: final'})


def by_year(out, s):
    """The final files' fit by year."""
    c = s[s['tag'] == 'final'].sort_values('year')
    return c[['year', 'targets', 'within_2pct_start', 'within_2pct', 'plain_targeted_start', 'plain_targeted', 'largest_miss_se', 'newton_largest_group',
              'newton_all_groups', 'all_converged', 'move_nats', 'heldout plain', 'smallest_state_effective_records', 'records_over_99pct_in_one_area']]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--run', required=True)
    ap.add_argument('--out', required=True)
    ap.add_argument('--tolerance', type=float, required=True)
    ap.add_argument('--tau', type=float, required=True)
    ap.add_argument('--levels', action='store_true')
    a = ap.parse_args()
    s = summaries(a.out)
    os.makedirs(f'{a.out}/out', exist_ok=True)
    s.to_csv(f'{a.out}/out/all_settings.csv', index=False)
    text = ['# Tables\n']
    t = four_starts(s, a.tolerance, a.tau)
    t.to_csv(f'{a.out}/out/heldout_four_starts.csv', index=False)
    text += ['## The four starts, held-out items\n', md(t)]
    g = grid_table(s)
    g.to_csv(f'{a.out}/out/settings_by_result.csv', index=False)
    text += ['## Setting by result\n', md(g)]
    if a.levels:
        for y in (2019, 2022):
            ar, gr = levels(a.run, a.out, y)
            ar.to_csv(f'{a.out}/out/levels_by_area_{y}.csv', index=False)
            gr.to_csv(f'{a.out}/out/levels_by_group_{y}.csv', index=False)
            for it in ('agi_amt', 'fedtax_amt'):
                t = ar[ar['item'] == it].sort_values('IRS', ascending=False).head(12).drop(columns=['year', 'item'])
                text += [f'## {it}, {y}, the twelve largest areas\n', md(t)]
            text += [f'## Average miss across areas by AGI group, {y}\n', md(gr[gr['item'].isin(['agi_amt', 'fedtax_amt'])].drop(columns=['year']))]
            n = nonfilers(a.out, y)
            n.to_csv(f'{a.out}/out/nonfilers_{y}.csv', index=False)
            text += [f'## Non-filers, {y}, thousands of adults, US row and the ten largest differences\n', md(n.tail(1))]
        b = by_year(a.out, s)
        b.to_csv(f'{a.out}/out/final_by_year.csv', index=False)
        text += ['## The final files by year\n', md(b)]
    open(f'{a.out}/out/tables.md', 'w').write('\n'.join(text))
    print('\n'.join(text))


if __name__ == '__main__':
    main()
