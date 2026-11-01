"""Adds the category 'taxes_state_local' to table21_2015_2023.csv: Table 2.1's state and local taxes, the line the
taxes-paid targets use since decision 54 (1 November 2026), in the same seven AGI groups and the same layout as the
other categories.

Why. Table 2.1's 'taxes paid deduction, total' is the state and local deduction plus 'other taxes' (foreign and similar,
$2 to $3 billion a year). The calculator's claimed_taxes holds state and local taxes only, so the amounts at $500,000 and
over could not be met together with the counts (SOI's average there, about $10,400, is above the $10,000 cap).

Which line.
  2018 to 2023: 'limited state and local taxes' (count and amount): the state and local deduction after the $10,000 cap,
  with no other taxes. This is what the calculator's claimed_taxes is.
  2015 to 2017: there is no cap, and SOI publishes no single state and local line. Amount: the sum of the three state
  and local lines (income or general sales taxes, real estate taxes, personal property taxes), which equals the total
  taxes paid less 'other taxes'. Count: the 'taxes paid deduction, total' count (there is no state and local count; in
  2018 to 2023 the two counts differ by 0.3% or less, the returns that deduct only other taxes).
The grouped CV is built as extract_table21.py builds it: the published cells inside the group taken as independent, the
square root of the sum of squared standard errors over the absolute sum; for the three-line amount the lines' standard
errors too. A group with a missing CV or value is left missing, as there (target_list.published_21 then borrows).

Run: python build_table21_state_local.py <soi_cv.csv>   (the soi_cv file errors.py reads); the file is changed in place.
"""
import sys
import numpy as np
import pandas as pd

PATH = __file__.rsplit('/', 1)[0] + '/table21_2015_2023.csv'
P = 'itemized deductions > taxes paid deduction > '
SL = P + 'total state and local taxes > '
GROUPS = [(-np.inf, 25000, 'under $25,000'), (25000, 50000, '$25,000–$50,000'), (50000, 75000, '$50,000–$75,000'),
          (75000, 100000, '$75,000–$100,000'), (100000, 200000, '$100,000–$200,000'),
          (200000, 500000, '$200,000–$500,000'), (500000, np.inf, '$500,000 and over')]


def lines(year, stat):
    n = 'number of returns' if stat == 'count' else 'amount'
    if year >= 2018:
        return [P + 'limited state and local taxes > ' + n]
    if stat == 'count':
        return [P + 'total > number of returns']
    return [SL + 'state and local income taxes or general sales taxes > total > amount',
            SL + 'real estate taxes > amount', SL + 'personal property taxes > amount']


def main(cv_path):
    cv = pd.read_csv(cv_path, keep_default_na=False, na_values=[''], low_memory=False)
    cv = cv[(cv.table.astype(str) == '2.1') & (cv.panel == 'All returns')]
    # the existing rows are kept byte for byte; only this category's rows are replaced
    text = [ln for ln in open(PATH).read().splitlines(keepends=True) if ',taxes_state_local,' not in ln]
    old = pd.read_csv(PATH).iloc[0:0]
    rows = []
    for y in range(2015, 2024):
        d = cv[cv.tax_year == y]
        for lo, hi, group in GROUPS:
            res = {}
            for stat in ('count', 'amount'):
                parts, refs, bad_any, nocv = [], [], False, False
                for item in lines(y, stat):
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
            rows.append([y, group, 'taxes_state_local', res['count'][0], res['amount'][0], res['count'][1], res['amount'][1],
                         cv_path, f'count: {res["count"][2]}; amount: {res["amount"][2]}'])
    new = pd.DataFrame(rows, columns=old.columns)
    if text and not text[-1].endswith('\n'):
        text[-1] += '\n'
    with open(PATH, 'w') as fh:
        fh.writelines(text)
    new.to_csv(PATH, mode='a', header=False, index=False)
    print(new.groupby('year')[['count', 'amount']].sum().to_string())
    print('missing:', new.isna().sum().to_dict())


if __name__ == '__main__':
    main(sys.argv[1])
