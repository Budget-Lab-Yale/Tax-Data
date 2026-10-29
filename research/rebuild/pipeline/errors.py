"""errors.py: each target's standard error (TARGETS.md section 9).

Errors matter only when the targets cannot all be hit: they decide which target gives way. So they need to be roughly
right, and only three kinds are computed; everything else is a judgment number with a written reason.

1. SOI tables that publish coefficients of variation (Tables 1.4, 2.1 and 3.3; error_kind 'soi_cv'). SOI does not
   link the CV workbooks from any page; they sit at https://www.irs.gov/pub/irs-soi/{yy}in14cv.xls, {yy}in21cv.xls
   and {yy}in33cv.xls (found on 22 October 2026, explore/2026-10-22/targets/irs_data/REPORT.md). The workbooks are
   kept in <soi_cv_dir>/raw/ with a manifest of where each came from, and `python errors.py soi_cv` parses them
   into <soi_cv_dir>/soi_cv.csv: one row per published cell, its value from the shared SOI mirror (the same cells
   targets.py reads) and its CV. A CV workbook is laid out like its table (same AGI rows, same column numbers), so a
   CV is joined to its cell by tax year, panel, AGI class and column number.
   - A target that adds up published cells (an AGI bracket merged from several of SOI's, a business group of several
     classes, the capital gains net of gains and losses) gets the standard error of the sum, the cells taken as
     independent: the square root of the sum of the cells' squared standard errors (value x CV / 100).
   - A cell SOI blanked and folded into its neighbour for disclosure ('combined') has no value and no CV of its own;
     its dollars and returns are in the receiving cell, whose CV is taken to be the CV of the combined estimate.
   - A CV of exactly 0 is kept as 0 where SOI samples every return (the $10,000,000-or-more class): there is no
     sampling error there. A target resting only on such cells (the $10M-and-over bracket, kept separate from 25
     October) gets a judgment error instead, settings full_sample_se (default 1%) of its value, since a target
     with no error could never give way.
2. Other SOI tables from the same sample (Table 1.6, Table 1.2's joint returns, any 1.4-like table without CVs;
   'soi_borrowed'): each cell borrows the CV of the Table 1.4 cells of the same year closest to it. For a count, the
   Table 1.4 count cells (number of returns with an item, any AGI class or the total row) closest in number of
   returns; for an amount, the amount cells closest in amount and in their number of returns (distance in logs). The
   CV used is the median of the closest few (settings borrow_neighbours), not the single closest, so one odd cell
   does not decide it. Cells sampled in full (CV 0) and cells without a CV are not lent.
3. Survey targets ('survey_replicate'): the sampling error from the survey's own replicate weights, carried to the
   tax units by the same rule as the main weight (a tax unit's replicate weight is its household's or interview's,
   as its weight is).
   - SCF (the wealth and wealth grid targets): computed with their values by scf_targets.py, over the year's mix of
     SCF waves (TARGETS.md S9). Within each wave, the Federal Reserve's method (codebook, "Replicate weights and
     multiplicity factors"): the replicate weights are WT1Bi x MMi (missing values are 0, as in the Fed's own
     program; scf_replicates below), for the first implicate only; sampling variance = sum over the 999
     replicates of (estimate - mean of the replicate estimates)^2 / 998, on the first implicate; imputation
     variance = the variance of the five implicates' estimates (divisor 4); total variance = sampling + (1 + 1/5)
     x imputation. The percentile lines the targets are defined by are held at their full-sample values, because
     the file is asked for the share above those dollar lines. The waves are independent samples, so the pooled
     variance is the sum over waves of share^2 x the wave's variance.
   - CEX (the consumption averages): the BLS's balanced repeated replication with 44 replicate weights
     (WTREP01-WTREP44 in the FMLI files, standard method up to the 2024 data): variance = sum over the 44 replicates
     of (replicate estimate - full-sample estimate)^2 / 44.
   - CPS (the overtime averages, since 28 October): Census's successive difference replication with the ASEC's 160
     person replicate weights (REPWTP1-REPWTP160, the unit head's, from settings paths.raw.cps_repwt): variance =
     4 / 160 x the sum over the replicates of (replicate estimate - full-sample estimate)^2; over the year's mix of
     CPS years, as the CEX's.
4. Everything else ('judgment'): settings, each with its one-line reason (the non-filer count and the people in
   non-filer units, which come from the filing stage's calibration).
A definition gap (settings definition_gap, by kind, a share of the value; none set today) adds to the sampling error
as the square root of the sum of squares. Identities (a row with identity true) have error 0 ('identity').

Interface: standard_errors(rows, year) -> one row per target, keyed by (kind, group, target) as in the run's
fit.csv, with se in the units of the published statistic (on the average itself for an average, on the share for a
share, in dollars or returns for a total), error_kind and se_reason.

Data, downloaded once (public; to scratch, never to the shared raw-data folder; the login node has the network):
  <soi_cv_dir>/raw/       the CV workbooks, with manifest.csv (file, URL, sha256):
                          curl -sSLO https://www.irs.gov/pub/irs-soi/19in14cv.xls   (and so on for each year and table)
  <scf_replicate_dir>/    each SCF wave's replicate weights, one folder per wave in the year's mix ({wave} in the
                          path; default <out>/data/scf_rw/<wave>): p16_rw1.dta, p19_rw1.dta, p22_rw1.dta for the
                          2016, 2019 and 2022 waves:
                          curl -sSLO https://www.federalreserve.gov/econres/files/scf2019rw1s.zip; unzip scf2019rw1s.zip
                          (and scf2016rw1s.zip, scf2022rw1s.zip). Check that unzip succeeds: one download of the
                          2022 file was cut short (26 October 2026). The 2016 and 2022 files were fetched on 26
                          October 2026 from the Federal Reserve; sha256 of the .dta files:
                          p16_rw1.dta 936201623d499a61d39461c5383f1b62fee9411f0d208c719298952e3b56ab0e,
                          p19_rw1.dta 0cfe01b806045c0fc3abe8b4902804ee05dda47da490c05397b1101f6e221739,
                          p22_rw1.dta 1f0222848253ec9d42ad614f1a28bb215d3ae8de3b17a66c53308d81066e91c2.
  The CEX's replicate weights are read from the FMLI files in the shared raw-data folder (settings paths.raw.cex).
Settings (section errors, each with a default): soi_cv_dir, scf_replicate_dir ({wave} in the path), borrow_neighbours,
nonfiler_count_se, nonfiler_people_population_se, definition_gap.

Usage (through Slurm, from the repository root):
  python errors.py soi_cv                          parse the CV workbooks into <soi_cv_dir>/soi_cv.csv
  python errors.py errors <rows.csv> <out folder>  the errors of the targets listed in rows.csv (a fit.csv or a
                                                   targets.csv): errors_<target year>.csv and errors_summary.md
"""
import glob
import os
import re
import sys

import numpy as np
import pandas as pd

import common
import targets as TG

Y = common.TARGET_YEAR
S = common.SETTINGS.get('errors', {})
S_TILT = common.SETTINGS['tilt']
CV_DIR = S.get('soi_cv_dir', f'{common.TARGETS}/soi_cv')
BORROW_K = int(S.get('borrow_neighbours', 5))
SCF_RW_DIR = S.get('scf_replicate_dir', f'{common.OUT}/data/scf_rw/{{wave}}')
JUDGMENT = {   # share of the value unless said otherwise; each with its reason
    'nonfiler_count': S.get('nonfiler_count_se', 0.06),
    'nonfiler_people': S.get('nonfiler_people_population_se', 0.005)}
DEFINITION_GAP = S.get('definition_gap', {})        # kind -> share of the value; none today
# A Table 1.4 target resting only on cells SOI samples in full (the $10,000,000-or-more bracket, CV 0): a judgment
# share of the value, since a target with no error cannot give way (TARGETS.md section 9, "everything else").
FULL_SAMPLE_SE = float(S.get('full_sample_se', 0.01))
AGES = common.AGE_LABELS[:common.ADULT_RANGES]
INF = np.inf

# Which published statistic is each IRS target, in Table 1.4's terms (variable, statistic, sign).
# Table 1.4 lines SOI renamed, by their old names (targets.T14's): from tax year 2022 wages are "total wages > total"
RENAMED = {'total wages > total > number of returns': 'salaries and wages > number of returns',
           'total wages > total > amount': 'salaries and wages > amount'}
IRS_PARTS = {'returns': [('returns', 'count', 1)],
             'wages count': [('wages', 'count', 1)], 'wages amount': [('wages', 'amount', 1)],
             'interest count': [('taxable_interest', 'count', 1)],
             'interest amount': [('taxable_interest', 'amount', 1)],
             'dividends count': [('ordinary_dividends', 'count', 1)],
             'dividends amount': [('ordinary_dividends', 'amount', 1)],
             'capital_gains count': [('sch_d_net_gain', 'count', 1), ('sch_d_net_loss', 'count', 1)],
             'capital_gains amount': [('sch_d_net_gain', 'amount', 1), ('sch_d_net_loss', 'amount', -1)],
             # (28 October, the calculator experiment) the tax calculator's two lines, named by a separate configuration
             'calc_taxable_income count': [('taxable_income', 'count', 1)],
             'calc_taxable_income amount': [('taxable_income', 'amount', 1)],
             'calc_income_tax_bc count': [('income_tax_before_credits', 'count', 1)],
             'calc_income_tax_bc amount': [('income_tax_before_credits', 'amount', 1)]}


# ---- 1. SOI's published coefficients of variation ------------------------------------------------
CV_TABLES = {'14': ('1.4', 'income_sources'), '21': ('2.1', 'itemized_deductions'), '33': ('3.3', 'tax_liability')}


def agi_range(label):
    """(lo, hi) of an SOI row label: 'No adjusted gross income' (-inf, 1); '$1 under $5,000'; 'Under $5,000'
    (-inf, 5,000); '$10,000,000 or more'; a panel total (-inf, inf). None if the label is not an AGI class."""
    s = re.sub(r'\[\d+\]|\(\d+\)|\*', '', str(label)).strip().lower()
    if s.endswith(', total') or s == 'all':
        return (-INF, INF)
    s = re.sub(r'\s+', ' ', s.replace('$', '').replace(',', ''))
    if s.startswith('no adjusted gross income'):
        return (-INF, 1.0)
    for pat, f in [(r'(\d+) under (\d+)', lambda a, b: (float(a), float(b))),
                   (r'under (\d+)', lambda b: (-INF, float(b))),
                   (r'(\d+) or more', lambda a: (float(a), INF))]:
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
    """Parses every workbook in <soi_cv_dir>/raw and joins each CV to its published cell in the mirror; writes
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


_CV = None


def soi_cells(year):
    """Table 1.4's all-returns cells of one year with their CVs, in targets.py's terms: lo, hi, variable, statistic
    (for the items targets.py names, else the mirror's item), value (dollars for amounts), cv_pct, se. A year with no
    CV workbook (a projection year) takes the CVs of the latest year that has one, said in the reason."""
    global _CV
    if _CV is None:
        _CV = pd.read_csv(f'{CV_DIR}/soi_cv.csv', keep_default_na=False, na_values=[''], low_memory=False)
    c = _CV[(_CV.table.astype(str) == '1.4') & (_CV.panel == 'All returns')]
    have = sorted(c.tax_year.unique())
    use = year if year in have else max(y for y in have if y <= year) if any(y <= year for y in have) else min(have)
    c = c[c.tax_year == use].copy()
    known = c['item'].replace(RENAMED).map(TG.T14)
    c['variable'] = [k[0] if isinstance(k, tuple) else i for k, i in zip(known, c['item'])]
    c['value'] = c['value'] * np.where(c['statistic'] == 'amount', 1000.0, 1.0)   # SOI publishes thousands
    c['se'] = c['value'] * c['cv_pct'] / 100
    c['total_row'] = ~np.isfinite(c['lo']) & ~np.isfinite(c['hi'])
    return c, use


def sum_error(cells, parts, lo, hi):
    """The standard error of a sum of published cells (independent): the cells of each (variable, statistic, sign)
    with lo >= lo and hi <= hi, AGI classes only. Returns (se, sum of the cells' values, cells used, note)."""
    t = cells[~cells.total_row] if np.isfinite(lo) or np.isfinite(hi) else cells[cells.total_row]
    var, total, n, notes = 0.0, 0.0, 0, []
    for v, st, sign in parts:
        s = t[(t.variable == v) & (t.statistic == st) & (t.lo >= lo) & (t.hi <= hi) & t.value.notna()]
        assert len(s), f'no published cell for {v} {st} in [{lo}, {hi})'
        miss = s[s.cv_pct.isna() & (s.value != 0)]
        if len(miss):   # a published value without a CV: it borrows one (none in 2019's targets)
            for r in miss.itertuples():
                cv, _ = borrow_cv(cells, r.value if st == 'count' else None, r.value if st == 'amount' else None)
                var += (r.value * cv / 100) ** 2
            notes.append(f'{len(miss)} cell(s) without a published CV borrowed one')
        var += float((s.se.fillna(0) ** 2).sum())
        total += sign * float(s.value.sum())
        n += len(s)
        if (s.cv_pct == 0).any() and (s.value != 0).any():
            notes.append('includes cells SOI samples in full (CV 0)')
    return np.sqrt(var), total, n, '; '.join(sorted(set(notes)))


def borrow_cv(cells, count=None, amount=None, returns=None):
    """The CV a cell of another SOI table borrows from Table 1.4 (same year): the median CV of the borrow_neighbours
    closest count cells in log number of returns (a count), or the closest amount cells in log amount and log number
    of returns of the same item (an amount; without returns, in amount alone). Cells with a CV of 0 or none are not
    lent. Returns (cv_pct, description of the donors)."""
    d = cells[(cells.cv_pct > 0) & (cells.value > 0)]
    if count is not None:
        d = d[d.statistic == 'count']
        dist = np.abs(np.log(d.value) - np.log(count))
    else:
        d = d[d.statistic == 'amount'].copy()
        n = cells[(cells.statistic == 'count')].set_index(['lo', 'hi', 'item'])['value']
        d['returns'] = [n.get((r.lo, r.hi, r.item.replace(' > amount', ' > number of returns')), np.nan)
                        for r in d.itertuples()]
        dist = (np.log(d.value) - np.log(amount)) ** 2
        if returns is not None:
            d = d[d.returns > 0]
            dist = dist[d.index] + (np.log(d.returns) - np.log(returns)) ** 2
        dist = np.sqrt(dist)
    near = d.loc[dist.sort_values(kind='stable').index[:BORROW_K]]
    cv = float(near.cv_pct.median())
    what = '; '.join(f'{r.item} in {r.row_label} ({r.value:,.0f}, CV {r.cv_pct:.2f}%)' for r in near.itertuples())
    return cv, what


def business_groups():
    """{label: (lo, hi)} of every business grouping in the targets catalog (targets_config/catalog.yaml: the
    groupings with edges on AGI)."""
    import yaml
    cat = yaml.safe_load(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'targets_config', 'catalog.yaml')))
    edges = [e for g in cat['groupings'].values() if g.get('variable') == 'agi' and 'edges' in g for e in g['edges']]
    out = {}
    for lo, hi, lab in edges:
        r = (-INF if lo is None else lo, INF if hi is None else hi)
        assert out.get(lab, r) == r, f'business group {lab}: two AGI ranges'
        out[lab] = r
    return out


def group_range(kind, group, lo=np.nan, hi=np.nan):
    """The AGI range of an SOI target's group: the row's own range (agi_lo, agi_hi, as target_list.expand writes it)
    when it has one; else read from the label: an SOI bracket label (merged top brackets end '... or more'), a
    business group of the targets catalog (merged groups 'A to B'), or 'all'."""
    if not (np.isnan(lo) or np.isnan(hi)):
        return (float(lo), float(hi))
    if group == 'all':
        return (-INF, INF)
    if kind == 'business':
        g = business_groups()
        ends = group.split(' to ')
        assert ends[0] in g and ends[-1] in g, f'business group {group}: not in the targets catalog'
        return (g[ends[0]][0], g[ends[-1]][1])
    r = agi_range(group)
    assert r is not None, f'{group}: not an AGI class'
    return r


def soi_errors(rows, year):
    """Table 1.4 targets (published CVs) and the counts of other SOI tables (borrowed CVs)."""
    cells, cv_year = soi_cells(year)
    yr = '' if cv_year == year else f' (the {cv_year} CVs: no CV workbook for {year})'
    out = []
    for r in rows.itertuples():
        if r.kind == 'IRS' and r.target == 'joint returns' or r.kind in ('age', 'dependent filers'):
            cv, what = borrow_cv(cells, count=r.value)
            table = {'IRS': '1.2', 'age': '1.6', 'dependent filers': '1.7'}[r.kind]
            out.append((r.Index, r.value * cv / 100, 'soi_borrowed',
                        f'Table {table} count, CV {cv:.2f}%: median of the {BORROW_K} Table 1.4 count cells closest in '
                        f'returns{yr}: {what}'))
            continue
        if r.kind == 'IRS':
            parts = IRS_PARTS[r.target]
        else:   # business: '<family> <net_income|net_loss> <count|amount>'
            fam_side, st = r.target.rsplit(' ', 1)
            fam, side = fam_side.split(' ')
            parts = [(f'{fam}_{side}', st, 1)]
        lo, hi = group_range(r.kind, r.group, getattr(r, 'agi_lo', np.nan), getattr(r, 'agi_hi', np.nan))
        se, total, n, note = sum_error(cells, parts, lo, hi)
        if abs(total - r.value) > 1e-6 * max(abs(r.value), 1):
            note = (note + '; ' if note else '') + f'cells add to {total:,.0f}, not the target {r.value:,.0f}'
        how = 'one published cell' if n == 1 else f'sum of {n} published cells, taken as independent'
        if se == 0 and r.value != 0:
            # only cells SOI samples in full ($10,000,000 or more): no sampling error, so a judgment number
            out.append((r.Index, FULL_SAMPLE_SE * abs(r.value), 'judgment',
                        f'{100 * FULL_SAMPLE_SE:.1f}% of the value (settings errors.full_sample_se): SOI samples every '
                        f'return here (published CV 0, {how}{yr}), so there is no sampling error; the number stands '
                        'for the gap between the file\'s definitions and SOI\'s (the next bracket down, $5M-10M, has '
                        'CVs of 0.6-2.9% on these items in 2019)'))
            continue
        out.append((r.Index, se, 'soi_cv', f'Table 1.4 CV, {how}{yr}' + (f'; {note}' if note else '')))
    return out


# ---- 3. Survey targets: replicate weights ------------------------------------------------------------
def scf_replicates(wave, units):
    """Each SCF tax unit's 999 replicate weights (first implicate only): its household's WT1Bi x MMi, missing values
    0 as in the Fed's program, the same rule as the main weight (every unit of a household carries the household's
    weight). Returns (units of the first implicate, replicate weights: units x 999)."""
    path = f"{SCF_RW_DIR.format(wave=wave)}/p{wave % 100:02d}_rw1.dta"
    rw = pd.read_stata(path)
    rw.columns = [c.lower() for c in rw.columns]
    imp = rw['y1'].astype(np.int64) - 10 * rw['yy1'].astype(np.int64)
    if not (imp == 1).all():
        common.say(f'  replicate file: Y1 - 10 x YY1 takes values {imp.value_counts().to_dict()}; joined on Y1')
    R = 999
    wt = rw[[f'wt1b{i}' for i in range(1, R + 1)]].to_numpy(float)
    mm = rw[[f'mm{i}' for i in range(1, R + 1)]].to_numpy(float)
    na = np.isnan(wt) | np.isnan(mm)
    common.say(f'SCF {wave} replicate weights: {len(rw)} households; {na.mean():.3f} of household x replicate cells '
               'missing (not drawn in that replicate), set to 0 as the Fed does')
    rep = np.maximum(0, np.nan_to_num(wt)) * np.maximum(0, np.nan_to_num(mm))
    u1 = units[units['implicate'] == 1].reset_index(drop=True)
    # the units' household is the SCF case YY1 (build_scf.R), plus build_table.py's id offset for the wave, a
    # multiple of 1e8; the replicate file's Y1 is YY1 x 10 + 1
    yy1 = np.round(u1['household_id'].to_numpy(float) % 1e8).astype(np.int64)
    idx = pd.Index(rw['y1'].astype(np.int64)).get_indexer(yy1 * 10 + 1)
    assert (idx >= 0).all(), f'{(idx < 0).sum()} SCF units of the first implicate with no replicate weights'
    one = u1.groupby('household_id')['weight'].agg(lambda x: x.max() - x.min() <= 1e-9 * x.max())
    assert one.all(), 'SCF units of one household with different weights'
    common.say(f'  units of the first implicate {len(u1)}; main weight (x 5) {5 * u1.weight.sum() / 1e6:.1f} m, '
               f'replicate weights {rep[idx].sum(0).mean() / 1e6:.1f} m on average '
               f'(range {rep[idx].sum(0).min() / 1e6:.1f}-{rep[idx].sum(0).max() / 1e6:.1f} m)')
    return u1, rep[idx]


def scf_errors(rows, year):
    """The SCF targets (kinds 'wealth', 'wealth grid' and 'auto loans'): the standard errors scf_targets.py computed with their
    values, over the year's mix of SCF waves. They are recorded when target_list.expand builds the year's targets;
    for a list read from a file (python errors.py errors), the year's targets are built first."""
    import scf_targets
    import target_list
    keys = list(zip(rows['kind'], rows['group'], rows['target']))
    if any(k not in scf_targets.RESULTS for k in keys):
        target_list.expand(year)
    out = []
    for i, k in zip(rows.index, keys):
        assert k in scf_targets.RESULTS, f'SCF target {k}: not among the {year} targets'
        d = scf_targets.RESULTS[k]
        note = '' if abs(d['value'] - rows.loc[i, 'value']) <= 1e-9 * max(abs(d['value']), 1) else \
            f'; computed value {d["value"]:,.6g}, not {rows.loc[i, "value"]:,.6g}'
        out.append((i, d['se'], 'survey_replicate', d['reason'] + note))
    return out


def cex_replicates(year, units):
    """Each CEX tax unit's 44 replicate weights: its interview's WTREPnn / 4 x MO_SCOPE / 3, the same rule as the main
    weight (build_cex.R's FINLWT21 / 4 x MO_SCOPE / 3), so weight x WTREPnn / FINLWT21."""
    import roster_cex
    reps = [f'WTREP{i:02d}' for i in range(1, 45)]
    f = roster_cex._read('fmli', year, ['NEWID', 'FINLWT21'] + reps)
    assert not f.NEWID.duplicated().any()
    f = f.set_index('NEWID')
    check = roster_cex.interview_weights(year).set_index('NEWID')['weight']
    nid = units['newid'].to_numpy()
    assert np.isin(nid, f.index).all(), 'CEX units with no FMLI record'
    assert np.allclose(check.loc[nid].to_numpy(), units['weight'].to_numpy(), rtol=1e-9), \
        "the units' weights are not FINLWT21 / 4 x MO_SCOPE / 3"
    W = f.loc[nid, reps].to_numpy(float)
    na = np.isnan(W)
    # Balanced repeated replication: each replicate is a half-sample, so about half the interviews are missing from
    # each replicate (a blank weight, which is weight 0) and the other half carry about twice their weight. Checked
    # here: the share blank per replicate is about one half, and each replicate's weights add up to about the
    # population, as the main weight does.
    rep = np.nan_to_num(units['weight'].to_numpy(float)[:, None] * W / f.loc[nid, 'FINLWT21'].to_numpy(float)[:, None])
    share = na.mean(0)
    tot = rep.sum(0) / units['weight'].sum()
    common.say(f'CEX {year} replicate weights: {len(units)} unit rows, {len(np.unique(nid))} interviews; blank '
               f'(not in the half-sample) in each replicate: {share.min():.3f}-{share.max():.3f} of unit rows; each '
               f"replicate's total over the main weight's: {tot.min():.3f}-{tot.max():.3f}")
    assert (np.abs(share - 0.5) < 0.05).all() and (np.abs(tot - 1) < 0.1).all(), 'CEX replicate weights: not half-samples'
    return rep


def consumption_errors(rows, year):
    """The consumption targets (target_list's consumption entries): each category's average per tax unit over all units and in
    each cell of the primary filer's adult age range by married, over the year's mix of CEX years (survey_mix.py):
    each year's standard error from its own 44 replicate weights, on its spending in the target year's dollars (the
    maps' factors, so a replicate's average is grown as the full-sample one is); the years are independent samples,
    so the pooled variance is the sum over years of share^2 x the year's variance."""
    import survey_mix
    parts = []
    for y, share in survey_mix.mix('cex', year):
        u = survey_mix.grown_units('cex', y)
        parts.append((y, share, u, u['weight'].to_numpy(float), cex_replicates(y, u)))
    out = []
    for r in rows.itertuples():
        g = r.group.split(', ', 1)[1]
        var, value, notes = 0.0, 0.0, []
        for y, share, u, w, rep in parts:
            if g == 'all':
                mu = np.ones(len(u), bool)
            else:
                m = re.fullmatch(r'age (.+), (married|not married)', g)
                assert m, f'consumption target {r.group}: unknown cell'
                mu = ((u['age_range'] == AGES.index(m.group(1))) & (u['married'] == int(m.group(2) == 'married'))).to_numpy()
            assert mu.any(), f'consumption target {r.group}: no CEX {y} units in the cell'
            v = u[r.target.split(',')[0]].to_numpy(float)
            full = (w * v)[mu].sum() / w[mu].sum()
            reps = (rep[mu] * v[mu, None]).sum(0) / rep[mu].sum(0)
            var += share ** 2 * ((reps - full) ** 2).sum() / 44
            value += share * full
            notes.append(f'{y} {100 * share:.2f}%: {int(mu.sum())} unit rows')
        note = '' if abs(value - r.value) <= 1e-9 * abs(r.value) else f'; pooled full-sample value {value:,.6g}, not {r.value:,.6g}'
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CEX year mix, replicate weights (44, BLS formula) year by year, variance = sum of share^2 x the '
                    f'year\'s; {"; ".join(notes)}{note}'))
    return out


def _ipums_fixed_width(folder, fields):
    """Columns of an IPUMS extract in fixed-width form (the .dat.gz and its DDI .xml in folder), read without ipumsr:
    each field's start, width and implied decimals from the DDI. Returns {field: float array}. Every character of
    every field read must be a digit (checked), so a blank or a sign cannot be misread."""
    import gzip
    import xml.etree.ElementTree as ET
    xml, = glob.glob(f'{folder}/*.xml')
    dat, = glob.glob(f'{folder}/*.dat.gz')
    root = ET.parse(xml).getroot()
    spec = {}
    for v in root.iter():
        if v.tag.split('}')[-1] == 'var' and v.get('ID') in fields:
            loc = next(c for c in v if c.tag.split('}')[-1] == 'location')
            spec[v.get('ID')] = (int(loc.get('StartPos')) - 1, int(loc.get('width')), int(v.get('dcml') or 0))
    missing = set(fields) - set(spec)
    assert not missing, f'{xml}: no {sorted(missing)}'
    with gzip.open(dat, 'rb') as fh:
        raw = fh.read()
    n_line = raw.index(b'\n') + 1
    assert len(raw) % n_line == 0, f'{dat}: lines of unequal length'
    a = np.frombuffer(raw, dtype=np.uint8).reshape(-1, n_line)
    out = {}
    for name, (start, width, dcml) in spec.items():
        d = a[:, start:start + width].astype(np.int64) - 48
        ok = ((d >= 0) & (d <= 9)).all(1)
        val = (np.where(ok[:, None], d, 0) @ (10 ** np.arange(width - 1, -1, -1, dtype=np.int64))).astype(float)
        if not ok.all():
            # (30 October) a field that is not all digits must still be a whole number written out, a sign and leading
            # blanks allowed (the March 2016 ASEC's replicate weights have negative values); anything else stops
            for r in np.where(~ok)[0]:
                txt = bytes(a[r, start:start + width]).decode('ascii')
                assert re.fullmatch(r' *-?[0-9]+', txt), f'{dat}: {name} holds {txt!r}, not a number'
                val[r] = float(int(txt))
            common.say(f'{dat}: {name}: {int((~ok).sum())} values with a sign or blanks read as numbers')
        out[name] = val / 10 ** dcml
    return out


CPS_REPLICATES = 160   # the ASEC's person replicate weights, successive difference replication


def cps_replicates(year, units):
    """Each CPS tax unit's 160 replicate weights. A unit's weight is its head's ASECWT (build_cps.R; the head's PERNUM
    is the last two digits of build_cps.R's record_id), so its replicate weights are the head's REPWTP1-REPWTP160 times
    the unit's weight over the head's ASECWT (1 where they agree; checked and printed). Read from the shared replicate
    weights layer (settings paths.raw.cps_repwt, sample cps<year + 1>_03s: the ASEC of March of the next year)."""
    import build_table as BT
    folder = f"{common.RAW['cps_repwt']}/cps{year + 1}_03s"
    reps = [f'REPWTP{i}' for i in range(1, CPS_REPLICATES + 1)]
    f = _ipums_fixed_width(folder, ['SERIAL', 'PERNUM', 'ASECWT'] + reps)
    key = f['SERIAL'].astype(np.int64) * 100 + f['PERNUM'].astype(np.int64)
    assert len(np.unique(key)) == len(key), f'{folder}: SERIAL x 100 + PERNUM is not unique'
    order = np.argsort(key)
    sk = key[order]
    rid = np.rint(units['record_id'].to_numpy(float) - BT.CPS_ID_OFFSET - (year - 2000) * 1e9).astype(np.int64)
    pos = np.minimum(np.searchsorted(sk, rid), len(sk) - 1)
    assert (sk[pos] == rid).all(), f'CPS {year}: {int((sk[pos] != rid).sum())} units whose head is not in {folder}'
    idx = order[pos]
    asecwt = f['ASECWT'][idx]
    assert (asecwt > 0).all(), f'CPS {year}: a unit head with no ASEC weight'
    ratio = units['weight'].to_numpy(float) / asecwt
    off = np.abs(ratio - 1) > 1e-6
    W = np.column_stack([f[r][idx] for r in reps]) * ratio[:, None]
    tot = W.sum(0) / units['weight'].sum()
    common.say(f'CPS {year} replicate weights ({folder}): {len(units):,} units; the unit\'s weight equals its head\'s '
               f'ASECWT on all but {int(off.sum()):,}'
               + (f' (ratio {ratio[off].min():.4f}-{ratio[off].max():.4f} there)' if off.any() else '')
               + f'; each replicate\'s total over the main weight\'s: {tot.min():.3f}-{tot.max():.3f}')
    assert (W >= 0).all() and (np.abs(tot - 1) < 0.1).all(), f'CPS {year}: replicate weights out of line'
    return W


def overtime_errors(rows, year):
    """The CPS overtime targets (kind 'overtime'): the share of units with overtime and overtime per unit, over all
    units and by the unit's wages, over the year's mix of CPS years (target_list.cps_mix_rows), on the units asked
    the earnings questions. Each year's standard error from the ASEC's 160 person replicate weights (the head's,
    cps_replicates), by Census's successive difference replication formula: variance = 4 / 160 x the sum over the
    replicates of (replicate estimate - full-sample estimate)^2; each replicate's average recomputed in full (its
    weights in numerator and denominator). The years are independent samples, so the pooled variance is the sum over
    years of share^2 x the year's variance. The cell is read from the row (range_variable, range_lo, range_hi)."""
    import survey_mix
    import target_list as TLS
    parts = []
    for y, share in survey_mix.mix('cps', year):
        u = TLS.cps_overtime_units(y)
        parts.append((y, share, u, u['weight'].to_numpy(float), cps_replicates(y, u)))
    # a list without the range columns (a fit.csv): the cell from its label, by the catalog's dollar ranges
    by_label = {lab: (g['variable'], -np.inf if lo is None else lo, np.inf if hi is None else hi)
                for g in TLS.CAT['groupings'].values() if 'edges' in g and g['variable'] != 'agi'
                for lo, hi, lab in g['edges']}
    out = []
    for r in rows.itertuples():
        rv = getattr(r, 'range_variable', '')
        rng = (rv, r.range_lo, r.range_hi) if isinstance(rv, str) and rv else by_label.get(r.group.split(', ', 1)[1])
        var, value, notes = 0.0, 0.0, []
        for y, share, u, w, rep in parts:
            if rng:
                xg = TLS.record_value(u, rng[0])
                mu = (xg >= rng[1]) & (xg < rng[2])
            else:
                assert r.group.endswith(', all'), f'overtime target {r.group}: a cell other than all with no range'
                mu = np.ones(len(u), bool)
            assert mu.any(), f'overtime target {r.group}: no CPS {y} units in the cell'
            v = TLS.record_value(u, r.variable)
            full = (w * v)[mu].sum() / w[mu].sum()
            reps = (rep[mu] * v[mu, None]).sum(0) / rep[mu].sum(0)
            var += share ** 2 * 4 / CPS_REPLICATES * ((reps - full) ** 2).sum()
            value += share * full
            notes.append(f'{y} {100 * share:.2f}%: {int(mu.sum())} asked units')
        note = '' if abs(value - r.value) <= 1e-9 * abs(r.value) else f'; pooled full-sample value {value:,.6g}, not {r.value:,.6g}'
        out.append((r.Index, float(np.sqrt(var)), 'survey_replicate',
                    'CPS year mix, the ASEC\'s 160 person replicate weights (the unit head\'s), successive difference '
                    'replication (variance = 4/160 x sum of squared deviations) year by year, variance = sum of '
                    f'share^2 x the year\'s; {"; ".join(notes)}{note}'))
    return out


# ---- 4. Judgment -----------------------------------------------------------------------------------
def nonfiler_errors(rows, year):
    """The non-filer count and the people in non-filer units (the filing stage's calibration targets)."""
    def census(y):
        """The filing stage's population by band in year y; a year with no CPS year of its own (30 October): the
        geometric mean of the CPS years either side, or past the last CPS year the last one grown with the
        population."""
        p = f"{common.DATA}/{S_TILT['nonfiler_targets_dir']}/{y}_nonfilers_vs_targets.csv"
        if os.path.exists(p):
            return pd.read_csv(p).set_index('band')['census_adults']
        import growth as GR
        import target_list as TLS
        have = TLS.cps_years()
        if y > max(have):
            return census(max(have)) * GR.population(y) / GR.population(max(have))
        lo, hi = TLS.cps_neighbours(y)
        return np.sqrt(census(lo) * census(hi))
    need = rows.kind.eq('non-filer people').any()
    t = census(year).to_frame('census_adults') if need else None
    out = []
    for r in rows.itertuples():
        if r.kind == 'non-filers':
            s = JUDGMENT['nonfiler_count']
            out.append((r.Index, s * r.value, 'judgment',
                        f'{100 * s:.0f}% of the count (settings errors.nonfiler_count_se): the non-filer units hold '
                        'the residual adults, about 253 million CPS adults less 209 million on returns and 17 million '
                        'dependents (2019); half a percent of the adults, 1.3 million, is about 6% of the count'))
            continue
        band = 'children under 17' if r.group == 'non-filers, children under 17' else \
            r.group.replace('non-filers, heads and spouses aged ', '')
        pop = 1e6 * t.loc[band, 'census_adults']
        s = JUDGMENT['nonfiler_people']
        who = 'CPS children under 17' if band == 'children under 17' else f"the band's CPS adults"
        out.append((r.Index, s * pop, 'judgment',
                    f'{100 * s:.1f}% of {who} ({pop / 1e6:.1f} m; settings errors.nonfiler_people_population_se): '
                    'the target is that population less the people on returns, each known to about half a percent, '
                    'so its error scales with the population, not with the residual'))
    return out


# ---- the interface -----------------------------------------------------------------------------------
def published_value(rows):
    """The published statistic of each row: for an average or share, the average or share itself (in 'average'
    where a row has one, else in 'value', as fit.csv and targets.csv carry it), else the value."""
    v = rows['value'].astype(float)
    if 'average' in rows:
        v = rows['average'].astype(float).where(rows['average'].notna(), v)
    return v


def standard_errors(rows, year=Y):
    """One row per target of rows (columns kind, group, target, value; identity optional): kind, group, target, se,
    error_kind, se_reason. se is in the published statistic's units (the average itself for an average, the share
    for a share). A kind this module does not know gets no error (error_kind 'none') and is listed in the summary."""
    rows = rows.reset_index(drop=True).assign(value=lambda d: published_value(d))
    res = {}
    by = {'soi': rows.kind.isin(['IRS', 'age', 'business', 'dependent filers']),
          'scf': rows.kind.isin(['wealth', 'wealth grid', 'auto loans']), 'consumption': rows.kind == 'consumption',
          'nonfiler': rows.kind.isin(['non-filers', 'non-filer people']), 'overtime': rows.kind == 'overtime'}
    for name, f in [('soi', soi_errors), ('scf', scf_errors), ('consumption', consumption_errors),
                    ('nonfiler', nonfiler_errors), ('overtime', overtime_errors)]:
        if by[name].any():
            for i, se, kind, why in f(rows[by[name]], year):
                res[i] = (se, kind, why)
    out = rows[['kind', 'group', 'target']].copy()
    out['se'] = [res.get(i, (np.nan,))[0] for i in out.index]
    out['error_kind'] = [res.get(i, (None, 'none'))[1] for i in out.index]
    out['se_reason'] = [res.get(i, (None, None, 'no error source'))[2] for i in out.index]
    for k, gap in DEFINITION_GAP.items():   # a definition gap: square root of the sum of squares
        m = out.kind == k
        out.loc[m, 'se'] = np.sqrt(out.loc[m, 'se'] ** 2 + (gap * rows.loc[m, 'value']) ** 2)
        out.loc[m, 'se_reason'] += f'; plus a definition gap of {100 * gap:.1f}% (settings errors.definition_gap)'
    if 'identity' in rows:
        m = rows['identity'].astype(bool)
        out.loc[m, ['se', 'error_kind', 'se_reason']] = [0.0, 'identity', 'an identity: hit exactly']
    return out


def summary(rows, e):
    """By kind: the median and range of se as a % of the value, and the targets with no error source."""
    pct = 100 * e['se'] / published_value(rows).abs()
    t = pd.DataFrame({'kind': e['kind'], 'error_kind': e['error_kind'], 'pct': pct})
    s = t.groupby(['kind', 'error_kind'])['pct'].agg(targets='size', median='median', min='min', max='max')
    none = e[e['error_kind'] == 'none']
    zero = e[(e['se'] == 0) & (e['error_kind'] != 'identity')]
    top = e.assign(se_pct=pct).sort_values('se_pct', ascending=False).groupby('kind').head(3)
    text = [f'# Standard errors of the {Y} targets', '', f'{len(e)} targets. Standard error as a % of the value '
            '(for an average or share, of the average or share):', '', s.to_markdown(floatfmt='.3f'), '',
            f'Targets with no error source: {len(none)}' + ('' if none.empty else '\n\n' + none.to_markdown(index=False)),
            '', f'Targets with an error of exactly 0 that are not identities: {len(zero)}', '',
            'The three noisiest targets of each kind:', '',
            top[['kind', 'group', 'target', 'se_pct', 'error_kind']].to_markdown(index=False, floatfmt='.2f')]
    return '\n'.join(text)


def main():
    if sys.argv[1] == 'soi_cv':
        build_soi_cv()
        return
    assert sys.argv[1] == 'errors'
    rows = pd.read_csv(sys.argv[2])
    out_dir = sys.argv[3]
    os.makedirs(out_dir, exist_ok=True)
    e = standard_errors(rows, Y)
    e.insert(3, 'value', published_value(rows))
    e.insert(5, 'se_pct', 100 * e['se'] / e['value'].abs())
    e.to_csv(f'{out_dir}/errors_{Y}.csv', index=False)
    text = summary(rows, e)
    open(f'{out_dir}/errors_summary.md', 'w').write(text + '\n')
    common.say(text)


if __name__ == '__main__':
    main()
