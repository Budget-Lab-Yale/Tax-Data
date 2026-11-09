"""Builds the November filing-season tables into one file. It is run by hand (run.py stage filing_season) when SOI
posts a new table.

Each November, SOI publishes a table of the individual returns the IRS processed through week 47 of the filing year,
by size of AGI. Because late returns are missing, its levels are 2-8% below SOI's final figures. The short layout (tax
years 2015-2017) has returns, AGI, tax after credits and the share of AGI from capital gains. The long layout (from
tax year 2017) has one line per item. This script reads the downloads in resources/filing_season/raw/ and writes
three files there: november_tables.csv (in dollars, on the standard AGI grid), line_mapping.csv (which pipeline item
each line is) and sources.csv.
"""
import os
import re

import numpy as np
import pandas as pd

from shared import common

DIR = os.path.join(common.RESOURCES, 'filing_season')
RAW = os.path.join(DIR, 'raw')
URL = 'https://www.irs.gov/pub/irs-soi/'

# The standard AGI grid (targets_config/catalog.yaml, standard_agi_groups), with the top class $1 million and over.
GRID = [(None, 1, 'no AGI'), (1, 10000, '$1-10k'), (10000, 20000, '$10k-20k'), (20000, 30000, '$20k-30k'),
        (30000, 40000, '$30k-40k'), (40000, 50000, '$40k-50k'), (50000, 75000, '$50k-75k'),
        (75000, 100000, '$75k-100k'), (100000, 200000, '$100k-200k'), (200000, 500000, '$200k-500k'),
        (500000, 1000000, '$500k-1m'), (1000000, None, '$1m and over')]

# Line label (in lower case, without footnote marks or the trailing colon) -> (pipeline item, definition). A line not
# listed here is kept in november_tables.csv with an empty item.
MAPPED = {
    'all returns': ('returns', 'number of returns (Form 1040 and its variants)'),
    'total number of returns': ('returns', 'number of returns (Form 1040 and its variants)'),
    'number of returns': ('returns', 'number of returns (Form 1040 and its variants); the short layout'),
    'adjusted gross income': ('agi', 'adjusted gross income less deficit (Form 1040 line 11 from 2020)'),
    'adjusted gross income less deficit': ('agi', 'adjusted gross income less deficit'),
    'salaries and wages': ('wages', 'wages (Form 1040 line 1; line 1z, total wages, from 2022)'),
    'total wages': ('wages', 'wages, Form 1040 line 1z (the same line as "salaries and wages" in 2022 and 2023)'),
    'taxable interest': ('txbl_int', 'taxable interest (Form 1040 line 2b)'),
    'ordinary dividends': ('dividends', 'ordinary dividends, qualified dividends included (Form 1040 line 3b)'),
    'qualified dividends': ('div_pref', 'qualified dividends, a part of ordinary dividends (Form 1040 line 3a)'),
    "taxable ira's, pensions, and annuities": (
        'txbl_ira_pens_dist', 'taxable IRA distributions and taxable pensions and annuities together (tax years 2017 '
        'and 2018; the file\'s txbl_ira_dist plus txbl_pens_dist)'),
    'taxable income from individual retirement arrangement (ira)': (
        'txbl_ira_dist', 'taxable IRA distributions (Form 1040 line 4b)'),
    'taxable income from pensions, and annuities': (
        'txbl_pens_dist', 'taxable pensions and annuities (Form 1040 line 5b)'),
    'unemployment compensation': ('ui', 'unemployment compensation (Schedule 1 line 7)'),
    'net capital gains / losses': (
        'capital_gains_line13', 'capital gain or loss on Form 1040 (line 7 from 2019): Schedule D\'s net gain, or its '
        'loss as allowed (at most $3,000), or capital gain distributions reported without a Schedule D; the file\'s '
        'capital_gains_line13'),
    'percentage of agi composed of capital gains (times agi)': (
        'capital_gains_line13', 'the short layout\'s share of AGI that is capital gains, times the class\'s AGI: '
        'an amount only, no count of returns'),
    'business net income / loss': ('sole_prop', 'Schedule C net income less loss (Schedule 1 line 3)'),
    'business net income / loss (schedule c)': ('sole_prop', 'Schedule C net income less loss (Schedule 1 line 3)'),
    'rental real estate, royalties, partnerships, s corporations, trusts (schedule e)': (
        'schedule_e', 'Schedule E net income less loss (Schedule 1 line 5): rents and royalties, partnerships, S '
        'corporations, estates and trusts, and farm rental (Form 4835); the file\'s parts of those families, signed'),
    'farm income / losses': ('farm', 'Schedule F net income less loss (Schedule 1 line 6)'),
    'farm income / losses (schedule f)': ('farm', 'Schedule F net income less loss (Schedule 1 line 6)'),
    'taxable social security benefits': ('ss_taxable', 'taxable Social Security benefits (Form 1040 line 6b)'),
    'total statutory adjustments': ('adjustments', 'total statutory adjustments (Schedule 1 part II)'),
    'state and local income tax refunds': ('state_ref', 'state and local income tax refunds (Schedule 1 line 1)'),
}


def clean(label):
    """A line label with its spaces evened out, and its footnote marks and trailing colon removed."""
    s = re.sub(r'\s+', ' ', str(label)).strip()
    s = re.sub(r'\s*\[\d+\]', '', s)
    return s.rstrip(':').strip()


def agi_bounds(label):
    """The (lo, hi) of a published AGI class label: 'No adjusted gross income ...' is (None, 1), below $1;
    '$X under $Y' is (X, Y); '$X or more' is (X, None)."""
    s = re.sub(r'\s+', ' ', str(label)).strip()
    if s.lower().startswith('no adjusted gross income'):
        return None, 1
    m = re.fullmatch(r'\$([\d,]+) under \$([\d,]+)', s)
    if m:
        return int(m.group(1).replace(',', '')), int(m.group(2).replace(',', ''))
    m = re.fullmatch(r'\$([\d,]+) or more', s)
    if m:
        return int(m.group(1).replace(',', '')), None
    raise ValueError(f'not an AGI class label: {label!r}')


def grid_class(lo, hi):
    """The class of the standard grid that a published class lies in. It must lie in exactly one."""
    lo_ = -np.inf if lo is None else lo
    hi_ = np.inf if hi is None else hi
    hits = [g for g in GRID if (-np.inf if g[0] is None else g[0]) <= lo_ and hi_ <= (np.inf if g[1] is None else g[1])]
    assert len(hits) == 1, (lo, hi, hits)
    return hits[0]


def number(v, where):
    """A published cell as a number. Stops if the cell is not a number."""
    if isinstance(v, (int, float)) and not isinstance(v, bool) and np.isfinite(v):
        return float(v)
    raise ValueError(f'{where}: a published cell that is not a number: {v!r}')


def sheet_rows(path):
    """The rows of a workbook's only sheet, as lists of cell values (.xlsx or .xls)."""
    if path.endswith('.xlsx'):
        import openpyxl
        wb = openpyxl.load_workbook(path, data_only=True, read_only=True)
        assert len(wb.worksheets) == 1, path
        return [list(r) for r in wb.worksheets[0].iter_rows(values_only=True)]
    import xlrd
    wb = xlrd.open_workbook(path)
    assert wb.nsheets == 1, path
    sh = wb.sheet_by_index(0)
    return [sh.row_values(i) for i in range(sh.nrows)]


def filing_year(name):
    """The filing year in a file name: '18in...' is 2018."""
    return 2000 + int(name[:2])


def short_layout(name, rows):
    """Reads a table in the short layout (filing years 2016-2018). Its rows are AGI classes; its columns are returns,
    AGI, tax after credits and the share of AGI from capital gains. Returns one row per line, statistic and grid
    class, plus the all-returns total, after checking that the classes add up to the published total."""
    fy = filing_year(name)
    title = str(rows[0][0])
    m = re.search(r'Through (\w+ \d+, \d{4})', title)
    cutoff = m.group(1)
    note = ' '.join(str(r[0]) for r in rows if str(r[0]).startswith('NOTE'))
    ty = int(re.search(r'income earned in (\d{4})', note).group(1))
    assert ty == fy - 1, (name, ty)
    head = [clean(c) for c in rows[2]]
    assert head[1:5] == ['Number of returns', 'Adjusted gross income', 'Income tax after credits',
                         'Percentage of AGI composed of capital gains'], (name, head)
    out, classes, total = [], [], None
    for i, r in enumerate(rows):
        lab = str(r[0]).strip()
        if lab == 'Total':
            total = [number(r[j], f'{name} row {i + 1}') for j in (1, 2, 3)]
        elif lab and (lab.startswith('$') or lab.lower().startswith('no adjusted')):
            lo, hi = agi_bounds(lab)
            vals = [number(r[j], f'{name} row {i + 1}') for j in (1, 2, 3, 4)]
            classes.append((lo, hi, vals))
    assert total is not None and len(classes) == 20, (name, len(classes))
    lines = [('Number of returns', 'returns', 0, 1.0), ('Adjusted gross income less deficit', 'amount', 1, 1000.0),
             ('Income tax after credits', 'amount', 2, 1000.0)]
    for line, stat, j, scale in lines:
        s = sum(v[j] for _, _, v in classes)
        assert abs(s - total[j]) <= max(25.0, 1e-6 * abs(total[j])), (name, line, s, total[j])
        out.append(dict(line=line, statistic=stat, agi_class='all', agi_lo=None, agi_hi=None, value=total[j] * scale,
                        cells=1, note=''))
        for g in GRID:
            sub = [v[j] for lo, hi, v in classes if grid_class(lo, hi) == g]
            assert sub, (name, g)
            out.append(dict(line=line, statistic=stat, agi_class=g[2], agi_lo=g[0], agi_hi=g[1],
                            value=sum(sub) * scale, cells=len(sub), note=''))
    # Capital gains: the share of each published class's AGI, times that AGI (in thousands, so times 1,000).
    line = 'Percentage of AGI composed of capital gains (times AGI)'
    how = 'each published class\'s share of AGI times its AGI, added up'
    tot = sum(v[1] * v[3] / 100.0 for _, _, v in classes) * 1000.0
    out.append(dict(line=line, statistic='amount', agi_class='all', agi_lo=None, agi_hi=None, value=tot,
                    cells=len(classes), note=how))
    for g in GRID:
        sub = [v[1] * v[3] / 100.0 for lo, hi, v in classes if grid_class(lo, hi) == g]
        out.append(dict(line=line, statistic='amount', agi_class=g[2], agi_lo=g[0], agi_hi=g[1],
                        value=sum(sub) * 1000.0, cells=len(sub), note=how))
    meta = dict(tax_year=ty, filing_year=fy, source_file=name, layout='short', cutoff=cutoff)
    return [{**meta, **r} for r in out]


def long_layout(name, rows):
    """Reads a table in the long layout (the expanded table, from filing year 2018 on). Its rows are one line per
    item and statistic; its columns are the total and the AGI classes. Returns one row per line, statistic and grid
    class, plus the all-returns total, after checking that the classes add up to the published total."""
    fy = filing_year(name)
    title = re.sub(r'\s+', ' ', str(rows[0][0]))
    ty = int(re.search(r'Tax Year (\d{4})', title).group(1))
    assert ty == fy - 1, (name, ty)
    cutoff = re.search(r'Cycle 47, (\w+ \d+, \d{4})', title).group(1)
    # Find the header row with the class labels, and the row of column numbers 1-17 under it.
    hrow = next(i for i, r in enumerate(rows) if any('under' in str(c) for c in r))
    cols = {}
    for j, c in enumerate(rows[hrow]):
        s = re.sub(r'\s+', ' ', str(c or '')).strip()
        if s.startswith('$') or s.lower().startswith('no adjusted'):
            cols[j] = agi_bounds(s)
    assert len(cols) == 16, (name, cols)
    first = min(cols)
    tcol = first - 1
    nrow = next(i for i, r in enumerate(rows) if i > hrow and r[tcol] in (1, 1.0) and r[first] in (2, 2.0))
    # Read each line. A label can name the line itself, or be "Amount" or "Number of returns" under a line named on
    # the row above; current holds that line's name.
    out, current = [], None
    for i in range(nrow + 1, len(rows)):
        r = rows[i]
        raw = '' if r[0] is None else str(r[0])
        lab = raw.strip()
        if not lab:
            assert all(c in (None, '', ' ') for c in r[1:first + 16]), (name, i + 1, r)
            continue
        if lab.startswith('**') or lab.startswith('[') or lab.startswith('Note') or lab.startswith('Source'):
            break
        if lab == 'Number of returns:':
            current = 'Number of returns'
            continue
        if '\n' in raw and clean(raw.split('\n')[-1]) == 'Number of returns':
            current = clean(raw.split('\n')[0])
            line, stat = current, 'returns'
        elif clean(lab) == 'Amount':
            assert current and current != 'Number of returns', (name, i + 1)
            line, stat = current, 'amount'
        elif lab.endswith(': Amount'):
            line, stat = clean(lab[:-len(': Amount')]), 'amount'
            current = None
        elif current == 'Number of returns' and raw.startswith(' '):
            line, stat = clean(lab), 'returns'
        elif clean(lab).lower() in ('adjusted gross income', 'adjusted gross income less deficit'):
            line, stat = clean(lab), 'amount'
            current = None
        elif clean(lab).lower() in ('total number of returns', 'number of electronically filed returns'):
            line, stat = clean(lab), 'returns'
            current = None
        elif clean(lab).lower() == 'number of exemptions':
            line, stat = clean(lab), 'count'
            current = None
        else:
            raise ValueError(f'{name} row {i + 1}: a line the reader does not know: {lab!r}')
        scale = 1000.0 if stat == 'amount' else 1.0
        total = number(r[tcol], f'{name} row {i + 1} total')
        vals = {j: number(r[j], f'{name} row {i + 1} column {j}') for j in cols}
        s = sum(vals.values())
        assert abs(s - total) <= max(25.0, 1e-5 * abs(total)), (name, i + 1, line, s, total)
        out.append(dict(line=line, statistic=stat, agi_class='all', agi_lo=None, agi_hi=None, value=total * scale,
                        cells=1, note=''))
        for g in GRID:
            sub = [vals[j] for j, (lo, hi) in cols.items() if grid_class(lo, hi) == g]
            assert sub, (name, g)
            out.append(dict(line=line, statistic=stat, agi_class=g[2], agi_lo=g[0], agi_hi=g[1],
                            value=sum(sub) * scale, cells=len(sub), note=''))
    meta = dict(tax_year=ty, filing_year=fy, source_file=name, layout='long', cutoff=cutoff)
    return [{**meta, **r} for r in out]


def main():
    # 1. Read every downloaded table, in either layout.
    files = sorted(f for f in os.listdir(RAW) if re.fullmatch(r'\d\dinweek47(expanded)?\.xlsx?', f))
    rows = []
    for f in files:
        sheet = sheet_rows(os.path.join(RAW, f))
        if str(sheet[0][0]).startswith('Cumulative'):
            rows += short_layout(f, sheet)
        else:
            rows += long_layout(f, sheet)
    t = pd.DataFrame(rows)
    t['item'] = [MAPPED.get(l.lower(), ('', ''))[0] for l in t['line']]
    # 2. Check that there is one row per tax year, layout, line, statistic and class, and write november_tables.csv.
    key = ['tax_year', 'layout', 'line', 'statistic', 'agi_class']
    dup = t[t.duplicated(key, keep=False)]
    assert dup.empty, dup[key].drop_duplicates().to_string()
    cols = ['tax_year', 'filing_year', 'source_file', 'layout', 'cutoff', 'line', 'item', 'statistic', 'agi_class',
            'agi_lo', 'agi_hi', 'value', 'cells', 'note']
    t = t[cols]
    t.to_csv(os.path.join(DIR, 'november_tables.csv'), index=False)
    common.say(f'november_tables.csv: {len(t)} rows, tax years {sorted(t.tax_year.unique())}')

    # 3. The mapping: every mapped line of every year, with its definition and the labels it has had over the years.
    m = t[t['item'] != ''][['item', 'tax_year', 'layout', 'line', 'statistic']].drop_duplicates()
    m = m.groupby(['item', 'tax_year', 'layout', 'line'])['statistic'].apply(lambda s: ' and '.join(sorted(s)))
    m = m.reset_index().rename(columns={'statistic': 'published'})
    m['definition'] = [MAPPED[l.lower()][1] for l in m['line']]
    labels = m.groupby('item')['line'].apply(lambda s: sorted(set(s)))
    m['label_changes'] = [('; '.join(f'"{l}" in ' + ', '.join(str(y) for y in sorted(m[(m.item == i) & (m.line == l)]
                                                                                          .tax_year.unique()))
                                     for l in labels[i]) if len(labels[i]) > 1 else '') for i in m['item']]
    m = m.sort_values(['item', 'tax_year', 'layout'])
    m.to_csv(os.path.join(DIR, 'line_mapping.csv'), index=False)
    common.say(f'line_mapping.csv: {len(m)} rows, {m.item.nunique()} items')
    missing = sorted(set(MAPPED.values()) - set((MAPPED[l.lower()] for l in m['line'])))
    for item, _ in missing:
        common.say(f'  (a mapped label found in no table: {item})')

    # 4. The sources: each file, its tax year and cutoff date, and where it was downloaded from.
    src = t[['tax_year', 'filing_year', 'source_file', 'layout', 'cutoff']].drop_duplicates().copy()
    src['url'] = URL + src['source_file']
    src['downloaded'] = '2026-11-04'
    src.to_csv(os.path.join(DIR, 'sources.csv'), index=False)
    common.say(src.to_string(index=False))


if __name__ == '__main__':
    main()
