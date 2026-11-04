"""Adds the categories 'charity_cash' and 'charity_noncash' to table21_2015_2023.csv (autoresearch, 4 November 2026):
Table 2.1's cash contributions and contributions other than cash, as itemizers report them on Schedule A (before the
limits on the deduction and without carryovers), in the same seven AGI groups and layout as the other categories.

Why. The charitable deduction itemizers claim is a target (category charity), but its split between cash and other
gifts is not. In run3's 2019 file itemizers reported $156 billion of cash gifts and $98 billion of other gifts, against
SOI's $130 billion and $75 billion (2023: $179 and $138 billion against $145 and $116 billion); the deduction's
limits then removed $64 billion, against SOI's $34 billion.

The grouped CV is built as build_table21_state_local.py builds it: the published cells inside the group taken as
independent. Run: python build_table21_charity.py <soi_cv.csv>   (the file is changed in place).
"""
import sys
import numpy as np
import pandas as pd

PATH = __file__.rsplit('/', 1)[0] + '/table21_2015_2023.csv'
P = 'itemized deductions > contributions deduction > '
LINES = {'charity_cash': 'cash contributions > ', 'charity_noncash': 'other than cash contributions > '}
GROUPS = [(-np.inf, 25000, 'under $25,000'), (25000, 50000, '$25,000–$50,000'), (50000, 75000, '$50,000–$75,000'),
          (75000, 100000, '$75,000–$100,000'), (100000, 200000, '$100,000–$200,000'),
          (200000, 500000, '$200,000–$500,000'), (500000, np.inf, '$500,000 and over')]


def lines(year, stat, cat):
    n = 'number of returns' if stat == 'count' else 'amount'
    return [P + LINES[cat] + n]


def main(cv_path):
    cv = pd.read_csv(cv_path, keep_default_na=False, na_values=[''], low_memory=False)
    cv = cv[(cv.table.astype(str) == '2.1') & (cv.panel == 'All returns')]
    # the existing rows are kept byte for byte; only this category's rows are replaced
    text = [ln for ln in open(PATH).read().splitlines(keepends=True) if ',charity_cash,' not in ln and ',charity_noncash,' not in ln]
    old = pd.read_csv(PATH).iloc[0:0]
    rows = []
    for cat in LINES:
      for y in range(2015, 2024):
        d = cv[cv.tax_year == y]
        for lo, hi, group in GROUPS:
            res = {}
            for stat in ('count', 'amount'):
                parts, refs, bad_any, nocv = [], [], False, False
                for item in lines(y, stat, cat):
                    sub = d[(d.item == item) & (d.lo >= lo) & (d.hi <= hi) & ~((d.lo == -np.inf) & (d.hi == np.inf))].sort_values('lo')
                    assert len(sub), (y, group, item, 'no cells')
                    assert sub.iloc[0].lo == lo and sub.iloc[-1].hi == hi and (sub.hi.to_numpy()[:-1] == sub.lo.to_numpy()[1:]).all(), (y, group, item, 'incomplete coverage')
                    mult = 1000.0 if stat == 'amount' else 1.0
                    bad = sub.value.isna() & (sub.value_flag.fillna('').astype(str) != 'combined')
                    bad_any |= bool(bad.any())
                    combined = sub.value_flag.fillna('').astype(str).str.contains('combined') | sub.cv_flag.fillna('').astype(str).str.contains('combined')
                    nocv |= bool(sub.cv_pct.isna().any() or combined.any())
                    parts.append((float(sub.value.sum(min_count=1)) * mult if len(sub) else np.nan,
                                  float(np.sqrt(((sub.value * sub.cv_pct / 100) ** 2).sum())) * mult))
                    refs.append(f'{item} columns {sorted(set(sub.col_seq.astype(int)))}')
                val = np.nan if bad_any else sum(p[0] for p in parts)
                se = np.sqrt(sum(p[1] ** 2 for p in parts))
                cvv = np.nan if (bad_any or nocv or val == 0 or not np.isfinite(val)) else se / abs(val)
                res[stat] = (val, cvv, '; '.join(refs))
            rows.append([y, group, cat, res['count'][0], res['amount'][0], res['count'][1], res['amount'][1],
                         cv_path, f'count: {res["count"][2]}; amount: {res["amount"][2]}'])
    new = pd.DataFrame(rows, columns=old.columns)
    if text and not text[-1].endswith('\n'):
        text[-1] += '\n'
    with open(PATH, 'w') as fh:
        fh.writelines(text)
    new.to_csv(PATH, mode='a', header=False, index=False)
    print(new.groupby(['category', 'year'])[['count', 'amount']].sum().to_string())
    print('missing:', new.isna().sum().to_dict())


if __name__ == '__main__':
    main(sys.argv[1])
