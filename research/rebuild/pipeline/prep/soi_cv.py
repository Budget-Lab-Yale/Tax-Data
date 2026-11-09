"""SOI's published coefficients of variation: the CV workbooks in <out>/targets/soi_cv/raw/ (downloaded by hand; see
reweight/errors.py) parsed into <out>/targets/soi_cv/soi_cv.csv, one row per published cell with its value from the SOI
mirror and its CV, and soi_cv_parse_log.csv. The standard errors of the reweighting read soi_cv.csv (errors.soi_cells).

  python -m prep.soi_cv
"""
import glob
import os
import re

import numpy as np
import pandas as pd

from shared import common
from prep import soi as TG

CV_DIR = f'{common.TARGETS}/soi_cv'
CV_TABLES = {'14': ('1.4', 'income_sources'), '21': ('2.1', 'itemized_deductions'), '33': ('3.3', 'tax_liability')}


def agi_range(label):
    """(lo, hi) of an SOI row label: 'No adjusted gross income' (-inf, 1); '$1 under $5,000'; 'Under $5,000'
    (-inf, 5,000); '$10,000,000 or more'; a panel total (-inf, inf). None if the label is not an AGI class."""
    s = re.sub(r'\[\d+\]|\(\d+\)|\*', '', str(label)).strip().lower()
    if s.endswith(', total') or s == 'all':
        return (-np.inf, np.inf)
    s = re.sub(r'\s+', ' ', s.replace('$', '').replace(',', ''))
    if s.startswith('no adjusted gross income'):
        return (-np.inf, 1.0)
    for pat, f in [(r'(\d+) under (\d+)', lambda a, b: (float(a), float(b))),
                   (r'under (\d+)', lambda b: (-np.inf, float(b))),
                   (r'(\d+) or more', lambda a: (float(a), np.inf))]:
        m = re.fullmatch(pat, s)
        if m:
            return f(*m.groups())
    return None


def read_cv_workbook(path):
    """One CV workbook's cells: panel, row_label, lo, hi, col_seq (SOI's column number), cv_pct, cv_flag. The column
    numbers are the header row that numbers the columns (1), (2), ...; a row labelled '<panel>, total' opens a panel.
    SOI marks a combined cell by its number format ('** '), as the mirror's reader does; a cell with no number has
    no CV (cv_pct missing)."""
    import xlrd
    book = xlrd.open_workbook(path, formatting_info=True)
    sh = book.sheet_by_index(0)

    def colnum(v):
        m = re.fullmatch(r'\(?(\d+)\)?', str(v).strip().replace('.0', ''))
        return int(m.group(1)) if m else None
    best, cols = -1, {}
    for r in range(min(sh.nrows, 30)):
        found = {c: colnum(sh.cell_value(r, c)) for c in range(1, sh.ncols)
                 if sh.cell_type(r, c) in (xlrd.XL_CELL_TEXT, xlrd.XL_CELL_NUMBER)}
        found = {c: n for c, n in found.items() if n is not None}
        if len(found) > len(cols) and sorted(found.values())[:3] == [1, 2, 3]:
            best, cols = r, found
    assert len(cols) >= 3, f'{path}: no row of column numbers'
    # SOI's column numbers run 1, 2, 3, ... left to right. One misprint is known (TY2018 Table 3.3 CV prints column
    # 111 as "11"): a single number out of a run whose neighbours agree on it is set to what they imply.
    order = sorted(cols)
    for a, b, c in zip(order, order[1:], order[2:]):
        if cols[b] != cols[a] + 1 and cols[c] == cols[a] + 2:
            common.say(f'  {os.path.basename(path)}: column number {cols[b]} between {cols[a]} and {cols[c]} read as '
                       f'{cols[a] + 1}')
            cols[b] = cols[a] + 1
    assert len(set(cols.values())) == len(cols), f'{path}: a column number printed twice'

    def flag(r, c):
        fmt = book.format_map[book.xf_list[sh.cell_xf_index(r, c)].format_key].format_str
        m = re.match(r'^"(\*+)\s*"', fmt)
        return '' if not m else ('combined' if len(m.group(1)) >= 2 else 'caution')
    rows, panel = [], None
    for r in range(best + 1, sh.nrows):
        label = str(sh.cell_value(r, 0)).strip()
        if label.endswith(', total'):
            panel = label[:-len(', total')]
        rng = agi_range(label) if panel else None
        if rng is None:
            continue
        for c, n in cols.items():
            t = sh.cell_type(r, c)
            if t == xlrd.XL_CELL_NUMBER:
                v, fl = float(sh.cell_value(r, c)), flag(r, c)
                # a zero under the combined format is a cell folded into its neighbour: no CV of its own
                rows.append((panel, label, rng[0], rng[1], n, np.nan if fl == 'combined' and v == 0 else v, fl))
            elif t == xlrd.XL_CELL_TEXT and str(sh.cell_value(r, c)).strip():
                txt = str(sh.cell_value(r, c)).strip()
                try:   # some workbooks (TY2020 Tables 2.1 and 3.3) store the numbers as text
                    rows.append((panel, label, rng[0], rng[1], n, float(txt.replace(',', '')), ''))
                except ValueError:
                    rows.append((panel, label, rng[0], rng[1], n, np.nan, 'combined' if txt.startswith('**') else txt))
    return pd.DataFrame(rows, columns=['panel', 'row_label', 'lo', 'hi', 'col_seq', 'cv_pct', 'cv_flag'])


def build_soi_cv():
    """Parses every workbook in <out>/targets/soi_cv/raw and joins each CV to its published cell in the mirror; writes
    soi_cv.csv (one row per published cell of a year with a CV workbook: tax_year, table, panel, row_label, lo, hi,
    col_seq, item, statistic, value, value_flag, cv_pct, cv_flag) and soi_cv_parse_log.csv."""
    files = sorted(glob.glob(f'{CV_DIR}/raw/[0-9][0-9]in[0-9][0-9]cv.xls'))
    assert files, f'no CV workbooks in {CV_DIR}/raw'
    mirror = {}
    log, parts = [], []
    for p in files:
        f = os.path.basename(p)
        yy, t = int(f[:2]), f[4:6]
        year = 1900 + yy if yy > 50 else 2000 + yy
        table, family = CV_TABLES[t]
        try:
            cv = read_cv_workbook(p)
        except Exception as e:   # one old file (TY1997 1.4) does not open; it is logged, not used
            log.append({'file': f, 'tax_year': year, 'table': table, 'parsed': False, 'cells': 0, 'joined': 0,
                        'note': str(e)[:200]})
            continue
        if family not in mirror:
            m = pd.read_csv(f'{TG.MIRROR}/bysize_{family}.csv', dtype={'value': float}, keep_default_na=False,
                            na_values=[''], low_memory=False)
            rng = m['row_label'].map(agi_range)
            m = m[rng.notna()].copy()
            m['lo'], m['hi'] = [r[0] for r in rng[rng.notna()]], [r[1] for r in rng[rng.notna()]]
            mirror[family] = m
        m = mirror[family]
        m = m[(m.tax_year == year) & (m.table.astype(str) == table)]
        if not len(m):
            log.append({'file': f, 'tax_year': year, 'table': table, 'parsed': True, 'cells': len(cv), 'joined': 0,
                        'note': 'year not in the mirror'})
            continue
        j = m.merge(cv[['panel', 'lo', 'hi', 'col_seq', 'cv_pct', 'cv_flag', 'row_label']].rename(
            columns={'row_label': 'cv_row_label'}), on=['panel', 'lo', 'hi', 'col_seq'], how='left')
        assert len(j) == len(m), f'{f}: a cell joined twice'
        j['statistic'] = np.where(j['col_label'].str.lower().str.contains('number of returns'), 'count', 'amount')
        parts.append(j.assign(table=table)[['tax_year', 'table', 'panel', 'row_label', 'lo', 'hi', 'col_seq', 'item',
                                            'statistic', 'value', 'flag', 'cv_pct', 'cv_flag']]
                     .rename(columns={'flag': 'value_flag'}))
        log.append({'file': f, 'tax_year': year, 'table': table, 'parsed': True, 'cells': len(cv),
                    'joined': int(j['cv_pct'].notna().sum()),
                    'note': f"{int((j['value'].notna() & j['cv_pct'].isna()).sum())} published values without a CV"})
    out = pd.concat(parts, ignore_index=True)
    out.to_csv(f'{CV_DIR}/soi_cv.csv', index=False)
    log = pd.DataFrame(log)
    log.to_csv(f'{CV_DIR}/soi_cv_parse_log.csv', index=False)
    common.say(log.to_string())
    v = out['value'].notna() & (out['value'] != 0)
    common.say(f'{len(out)} published cells; {int(v.sum())} with a nonzero value, of which '
               f"{int((v & out['cv_pct'].isna()).sum())} have no CV (flags: "
               f"{out.loc[v & out['cv_pct'].isna(), 'value_flag'].value_counts().to_dict()}) and "
               f"{int((v & (out['cv_pct'] == 0)).sum())} a CV of 0 "
               f"({int((v & (out['cv_pct'] == 0) & (out['lo'] >= 1e7)).sum())} of them in the $10,000,000-or-more class)")
    return out


if __name__ == '__main__':
    build_soi_cv()
