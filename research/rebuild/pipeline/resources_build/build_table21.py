"""Adds a group of categories to resources/table21_2015_2023.csv from SOI Table 2.1 (itemized deductions), in the
file's seven AGI groups. It replaces that group's rows and keeps every other row byte for byte. It is run by hand.
The groups are state_local (state and local taxes) and charity (cash and non-cash contributions, as itemizers report
them). Each AGI group adds up several published cells; its coefficient of variation (CV) treats the cells' sampling
errors as independent. A group with a missing CV or value is left missing.

    python -m resources_build.build_table21 <state_local|charity> <soi_cv.csv written by prep/soi_cv.py>
"""
import os
import sys

import numpy as np
import pandas as pd

from shared import common

PATH = os.path.join(common.RESOURCES, 'table21_2015_2023.csv')
TAXES = 'itemized deductions > taxes paid deduction > '
SL = TAXES + 'total state and local taxes > '
CHARITY = 'itemized deductions > contributions deduction > '
GROUPS = [(-np.inf, 25000, 'under $25,000'), (25000, 50000, '$25,000–$50,000'), (50000, 75000, '$50,000–$75,000'),
          (75000, 100000, '$75,000–$100,000'), (100000, 200000, '$100,000–$200,000'),
          (200000, 500000, '$200,000–$500,000'), (500000, np.inf, '$500,000 and over')]


def state_local_lines(year, stat):
    """The Table 2.1 lines of state and local taxes in a year, for stat ('count' or 'amount'). The calculator's
    deducted taxes are state and local taxes only, without 'other taxes'. From 2018 on that is the line after the
    $10,000 cap. Before 2018 SOI has no single line: the amount is the sum of the three state and local lines, and the
    count is all returns deducting taxes, which is within 0.3% of the state and local count where both exist."""
    n = 'number of returns' if stat == 'count' else 'amount'
    if year >= 2018:
        return [TAXES + 'limited state and local taxes > ' + n]
    if stat == 'count':
        return [TAXES + 'total > number of returns']
    return [SL + 'state and local income taxes or general sales taxes > total > amount',
            SL + 'real estate taxes > amount', SL + 'personal property taxes > amount']


def charity_lines(part):
    """A function giving the Table 2.1 line of one kind of contribution, for any year and stat."""
    def lines(year, stat):
        return [CHARITY + part + ('number of returns' if stat == 'count' else 'amount')]
    return lines


# group name: {category: the Table 2.1 lines of a year and statistic}
CATEGORIES = {'state_local': {'taxes_state_local': state_local_lines},
              'charity': {'charity_cash': charity_lines('cash contributions > '),
                          'charity_noncash': charity_lines('other than cash contributions > ')}}


def grouped(d, items, stat, lo, hi):
    """The value, the CV and a list of the cells used, for the items' published cells inside the AGI group [lo, hi)
    (d: the year's cells in that range). The items are added up, and their standard errors are combined as if
    independent (the square root of the sum of squares)."""
    parts, refs, bad_any, nocv = [], [], False, False
    for item in items:
        sub = d[d.item == item].sort_values('lo')
        assert len(sub), (item, 'no cells')
        assert sub.iloc[0].lo == lo and sub.iloc[-1].hi == hi and (sub.hi.to_numpy()[:-1] == sub.lo.to_numpy()[1:]).all(), \
            (item, 'incomplete coverage')
        mult = 1000.0 if stat == 'amount' else 1.0
        flags = sub.value_flag.fillna('').astype(str)
        bad_any |= bool((sub.value.isna() & (flags != 'combined')).any())
        combined = flags.str.contains('combined') | sub.cv_flag.fillna('').astype(str).str.contains('combined')
        nocv |= bool(sub.cv_pct.isna().any() or combined.any())
        parts.append((float(sub.value.sum(min_count=1)) * mult,
                      float(np.sqrt(((sub.value * sub.cv_pct / 100) ** 2).sum())) * mult))
        refs.append(f'{item} columns {sorted(set(sub.col_seq.astype(int)))}')
    val = np.nan if bad_any else sum(p[0] for p in parts)
    se = np.sqrt(sum(p[1] ** 2 for p in parts))
    cv = np.nan if (bad_any or nocv or val == 0 or not np.isfinite(val)) else se / abs(val)
    return val, cv, '; '.join(refs)


def main(group, cv_path):
    """Replaces the group's rows of the file with rows built from soi_cv.csv (cv_path), one per category, year and AGI
    group."""
    cats = CATEGORIES[group]
    cv = pd.read_csv(cv_path, keep_default_na=False, na_values=[''], low_memory=False)
    cv = cv[(cv.table.astype(str) == '2.1') & (cv.panel == 'All returns')]
    text = [ln for ln in open(PATH).read().splitlines(keepends=True) if not any(f',{c},' in ln for c in cats)]
    columns = pd.read_csv(PATH, nrows=0).columns
    rows = []
    for cat, lines in cats.items():
        for y in range(2015, 2024):
            d = cv[cv.tax_year == y]
            for lo, hi, group_label in GROUPS:
                cells = d[(d.lo >= lo) & (d.hi <= hi) & ~((d.lo == -np.inf) & (d.hi == np.inf))]
                res = {stat: grouped(cells, lines(y, stat), stat, lo, hi) for stat in ('count', 'amount')}
                rows.append([y, group_label, cat, res['count'][0], res['amount'][0], res['count'][1], res['amount'][1],
                             cv_path, f'count: {res["count"][2]}; amount: {res["amount"][2]}'])
    new = pd.DataFrame(rows, columns=columns)
    if text and not text[-1].endswith('\n'):
        text[-1] += '\n'
    with open(PATH, 'w') as fh:
        fh.writelines(text)
    new.to_csv(PATH, mode='a', header=False, index=False)
    print(new.groupby(['category', 'year'])[['count', 'amount']].sum().to_string())
    print('missing:', new.isna().sum().to_dict())


if __name__ == '__main__':
    main(*sys.argv[1:3])
