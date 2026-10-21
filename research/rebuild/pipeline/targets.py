"""The published IRS numbers: SOI Publication 1304, Tables 1.4 and 1.2, tax
years 2015 and 2019, all returns, by size of AGI.

Read from the shared mirror of every published SOI cell
(settings.json, paths.raw.soi_mirror). Amounts are in dollars (SOI
publishes thousands). A cell SOI blanked and merged into its neighbour for
disclosure stays blank with flag 'combined'; published_cells() puts it back
together. Table 1.2's separate, head-of-household and single returns are
added up as 'nonjoint'.

Writes common.TARGETS/soi.csv: year, table, filing_status, lo, hi, label,
variable, statistic, value, flag. The label of a published total is 'all'.
"""
import os

import numpy as np
import pandas as pd

import common

MIRROR = common.RAW['soi_mirror']
T14 = {  # SOI item -> (variable, statistic)
    'number of returns': ('returns', 'count'),
    'adjusted gross income less deficit': ('agi', 'amount'),
    'salaries and wages > number of returns': ('wages', 'count'),
    'salaries and wages > amount': ('wages', 'amount'),
    'taxable interest > number of returns': ('taxable_interest', 'count'),
    'taxable interest > amount': ('taxable_interest', 'amount'),
    'ordinary dividends > number of returns': ('ordinary_dividends', 'count'),
    'ordinary dividends > amount': ('ordinary_dividends', 'amount'),
    'sales of capital assets reported on form 1040, schedule d > taxable net gain > number of returns':
        ('sch_d_net_gain', 'count'),
    'sales of capital assets reported on form 1040, schedule d > taxable net gain > amount':
        ('sch_d_net_gain', 'amount'),
    'sales of capital assets reported on form 1040, schedule d > taxable net loss > number of returns':
        ('sch_d_net_loss', 'count'),
    'sales of capital assets reported on form 1040, schedule d > taxable net loss > amount':
        ('sch_d_net_loss', 'amount'),
}
BUSINESS = {'sch_c': 'business or profession', 'pship_scorp': 'partnership and s corporation',
            'rent_royalty': 'total rental and royalty', 'estate_trust': 'estate and trust',
            'farm': 'farm', 'farm_rental': 'farm rental'}
for _fam, _name in BUSINESS.items():
    for _side in ('net income', 'net loss'):
        T14[f'{_name} > {_side} > number of returns'] = (f'{_fam}_{_side.replace(" ", "_")}', 'count')
        T14[f'{_name} > {_side} > amount'] = (f'{_fam}_{_side.replace(" ", "_")}', 'amount')
T12_STATUS = {'all returns': 'all',
              'returns of married persons filing jointly and returns of surviving spouses': 'joint',
              'returns of married persons filing separately': 'separate',
              'returns of heads of households': 'head_of_household',
              'returns of single persons': 'single'}
FILES = {'1.4': 'bysize_income_sources.csv', '1.2': 'bysize_marital_status.csv'}


def read(table, years):
    t = pd.read_csv(os.path.join(MIRROR, FILES[table]), dtype=str, keep_default_na=False)
    t = t[t['tax_year'].isin([str(y) for y in years]) & (t['table'] == table) & (t['panel'] == 'All returns')]
    rows = []
    for r in t.itertuples():
        item, status = r.item, 'all'
        if table == '1.2':
            hit = [(code, item[len(p) + 3:]) for p, code in T12_STATUS.items() if item.startswith(p + ' > ')]
            if not hit:
                continue
            status, item = hit[0]
            if item != 'number of returns':
                continue
        if item not in T14:
            continue
        total = r.row_label.endswith('total')
        if total and r.row_label != 'All returns, total':
            continue
        var, stat = T14[item]
        value = np.nan if r.value == '' else float(r.value) * (1000 if stat == 'amount' else 1)
        rows.append({'year': int(r.tax_year), 'table': table, 'filing_status': status,
                     'lo': -np.inf if total or r.row_agi_lo in ('', '-Inf') else float(r.row_agi_lo),
                     'hi': np.inf if total or r.row_agi_hi in ('', 'Inf') else float(r.row_agi_hi),
                     'label': 'all' if total else r.row_label, 'variable': var, 'statistic': stat,
                     'value': value, 'flag': r.flag})
    return pd.DataFrame(rows)


def build():
    t = pd.concat([read('1.4', [2015, 2019]), read('1.2', [2015, 2019])], ignore_index=True)
    blank = t['value'].isna()
    assert (t.loc[blank, 'flag'] == 'combined').all(), t[blank & (t['flag'] != 'combined')]
    nj = t[t['filing_status'].isin(['separate', 'head_of_household', 'single'])]
    nj = nj.groupby(['year', 'table', 'lo', 'hi', 'label', 'variable', 'statistic'], as_index=False)['value'].sum()
    t = pd.concat([t, nj.assign(filing_status='nonjoint', flag='')], ignore_index=True)
    os.makedirs(common.TARGETS, exist_ok=True)
    t.to_csv(f'{common.TARGETS}/soi.csv', index=False)
    return t


def load():
    return pd.read_csv(f'{common.TARGETS}/soi.csv', keep_default_na=False, na_values=[''])


def published_cells(soi, year, variable, stat):
    """An item's Table 1.4 cells by AGI as published, lowest first, as (lo, hi, value).
    A non-blank 'combined' cell holds its own bracket and the blank ones after it,
    so it becomes one cell spanning them."""
    s = soi[(soi['year'] == year) & (soi['table'].astype(str) == '1.4') & (soi['filing_status'] == 'all')
            & (soi['variable'] == variable) & (soi['statistic'] == stat) & (soi['label'] != 'all')]
    out = []
    for r in s.sort_values('lo').itertuples():
        if pd.isna(r.value):
            assert out and out[-1][3], f'{year} {variable} {stat}: a blank cell with no cell to join'
            out[-1] = (out[-1][0], r.hi, out[-1][2], True)
        else:
            out.append((r.lo, r.hi, float(r.value), r.flag == 'combined'))
    return [(lo, hi, v) for lo, hi, v, _ in out]


if __name__ == '__main__':
    print(build().groupby(['year', 'table', 'variable', 'statistic']).size().to_string())
