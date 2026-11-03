"""Every published SOI number the pipeline uses, from the shared mirror of SOI
cells (settings.json, paths.raw.soi_mirror). Amounts are in dollars (SOI
publishes thousands).

1. Targets: Publication 1304, Tables 1.4 and 1.2 (and, since 30 October, Table 3.3's
   credits and taxes), the PUF year and the target year, all returns, by size of AGI. A cell SOI blanked and merged into its
   neighbour for disclosure stays blank with flag 'combined'; published_cells()
   puts it back together. Table 1.2's separate, head-of-household and single
   returns are added up as 'nonjoint'. Written to common.TARGETS/soi.csv: year,
   table, filing_status, lo, hi, label, variable, statistic, value, flag (the
   label of a published total is 'all'); read back with load(), and a return
   count with returns().
2. Totals: Table 1.4's all-returns totals for every year, read directly by
   totals(); soi_item() and per_return_growth() look items up in them by
   pattern, so a line SOI renamed (wages in tax year 2022) is still found.
3. Table 1.6: returns by filing status and the primary filer's age, all AGI,
   every year (table_1_6), for the tax data (translate.py).
4. FAMILIES: the business families, each with its SOI line and its PUF parts
   (which families the tilt targets is set in targets_config/catalog.yaml).
"""
import os
import re
from functools import lru_cache

import numpy as np
import pandas as pd

import common

MIRROR = common.RAW['soi_mirror']
FAMILIES = {  # family: SOI Table 1.4 line, PUF income parts, PUF loss parts (stored positive)
    'sole_prop': {'soi': 'business or profession', 'income': ['sole_prop'], 'loss': []},
    'pship_scorp': {'soi': 'partnership and s corporation',
                    'income': ['part_passive', 'part_active', 'scorp_passive', 'scorp_active'],
                    'loss': ['part_passive_loss', 'part_active_loss', 'part_179', 'scorp_passive_loss', 'scorp_active_loss',
                             'scorp_179']},
    'rent_royalty': {'soi': 'total rental and royalty', 'income': ['rent'], 'loss': ['rent_loss']},
    'estate_trust': {'soi': 'estate and trust', 'income': ['estate'], 'loss': ['estate_loss']},
    'farm': {'soi': 'farm', 'income': ['farm'], 'loss': []},
    'farm_rental': {'soi': 'farm rental', 'income': ['farm_rent'], 'loss': []},
    # From tax year 2021 Table 1.4 publishes partnership and S corporation income separately, with no combined line
    # (30 October, targets for every year): the two halves of pship_scorp, each netted on its own, for the years that
    # publish them (targets_config/<year>/targets.yaml names them in business_top from 2021)
    'partnership': {'soi': 'partnership', 'income': ['part_passive', 'part_active'],
                    'loss': ['part_passive_loss', 'part_active_loss', 'part_179']},
    'scorp': {'soi': 's corporation', 'income': ['scorp_passive', 'scorp_active'],
              'loss': ['scorp_passive_loss', 'scorp_active_loss', 'scorp_179']}}
T14 = {  # SOI item -> (variable, statistic)
    'number of returns': ('returns', 'count'),
    'social security benefits > total > number of returns': ('gross_ss', 'count'),
    'social security benefits > total > amount': ('gross_ss', 'amount'),
    'adjusted gross income less deficit': ('agi', 'amount'),
    'salaries and wages > number of returns': ('wages', 'count'),
    'salaries and wages > amount': ('wages', 'amount'),
    # SOI's name for the same line (Form 1040 line 1z, total wages) from tax year 2022 (30 October)
    'total wages > total > number of returns': ('wages', 'count'),
    'total wages > total > amount': ('wages', 'amount'),
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
    # (28 October, the calculator experiment) two lines the tax calculator computes; read for a test, targeted by no
    # default configuration
    'taxable income > number of returns': ('taxable_income', 'count'),
    'taxable income > amount': ('taxable_income', 'amount'),
    'income tax before credits > number of returns': ('income_tax_before_credits', 'count'),
    'income tax before credits > amount': ('income_tax_before_credits', 'amount'),
    # (30 October, taxes computed inside the model, taxes.py) Table 1.4's other lines the calculator computes
    'total itemized deductions > number of returns': ('itemized_deductions', 'count'),
    'total itemized deductions > amount': ('itemized_deductions', 'amount'),
    'qualified business income deduction > number of returns': ('qbi_deduction', 'count'),
    'qualified business income deduction > amount': ('qbi_deduction', 'amount'),
    'alternative minimum tax > number of returns': ('alternative_minimum_tax', 'count'),
    'alternative minimum tax > amount': ('alternative_minimum_tax', 'amount'),
}
# (30 October, taxes.py) Table 3.3, tax liability, tax credits and tax payments by size of AGI: the credits and taxes
# the calculator computes (SOI item -> (variable, statistic)). The earned income credit is the whole credit (the part
# used against tax and the refundable part); income tax after credits is income tax before credits less every credit
# used against it; total income tax adds the net investment income tax and the other income taxes.
T33 = {
    'tax credits > child and other dependent tax credit > number of returns': ('ctc_nonrefundable', 'count'),
    'tax credits > child and other dependent tax credit > amount': ('ctc_nonrefundable', 'amount'),
    'tax credits > total refundable credits > additional child tax credit > number of returns':
        ('ctc_refundable', 'count'),
    'tax credits > total refundable credits > additional child tax credit > amount': ('ctc_refundable', 'amount'),
    # the same two lines under their names in other years: "child tax credit" in 2015-2017; from 2021 the refundable
    # part is "refundable child tax credit or additional child tax credit"
    'tax credits > child tax credit > number of returns': ('ctc_nonrefundable', 'count'),
    'tax credits > child tax credit > amount': ('ctc_nonrefundable', 'amount'),
    'tax credits > total refundable credits > refundable child tax credit or additional child tax credit > number of '
    'returns': ('ctc_refundable', 'count'),
    'tax credits > total refundable credits > refundable child tax credit or additional child tax credit > amount':
        ('ctc_refundable', 'amount'),
    'tax credits > total refundable credits > earned income credit > number of returns': ('eitc', 'count'),
    'tax credits > total refundable credits > earned income credit > amount': ('eitc', 'amount'),
    'income tax after credits > number of returns': ('income_tax_after_credits', 'count'),
    'income tax after credits > amount': ('income_tax_after_credits', 'amount'),
    'total income tax > number of returns': ('total_income_tax', 'count'),
    'total income tax > amount': ('total_income_tax', 'amount'),
}
for _fam, _f in FAMILIES.items():
    for _side in ('net income', 'net loss'):
        T14[f'{_f["soi"]} > {_side} > number of returns'] = (f'{_fam}_{_side.replace(" ", "_")}', 'count')
        T14[f'{_f["soi"]} > {_side} > amount'] = (f'{_fam}_{_side.replace(" ", "_")}', 'amount')
T12_STATUS = {'all returns': 'all',
              'returns of married persons filing jointly and returns of surviving spouses': 'joint',
              'returns of married persons filing separately': 'separate',
              'returns of heads of households': 'head_of_household',
              'returns of single persons': 'single'}
FILES = {'1.4': 'bysize_income_sources.csv', '1.2': 'bysize_marital_status.csv', '3.3': 'bysize_tax_liability.csv'}
TABLE_ITEMS = {'1.4': T14, '1.2': T14, '3.3': T33}   # each table's items (Table 1.2: its returns, by T14's name)


# ---- 1. the targets -----------------------------------------------------------------------
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
        if item not in TABLE_ITEMS[table]:
            continue
        total = r.row_label.endswith('total')
        if total and r.row_label != 'All returns, total':
            continue
        var, stat = TABLE_ITEMS[table][item]
        value = np.nan if r.value == '' else float(r.value) * (1000 if stat == 'amount' else 1)
        rows.append({'year': int(r.tax_year), 'table': table, 'filing_status': status,
                     'lo': -np.inf if total or r.row_agi_lo in ('', '-Inf') else float(r.row_agi_lo),
                     'hi': np.inf if total or r.row_agi_hi in ('', 'Inf') else float(r.row_agi_hi),
                     'label': 'all' if total else r.row_label, 'variable': var, 'statistic': stat,
                     'value': value, 'flag': r.flag})
    return pd.DataFrame(rows)


def build():
    # past SOI's last year (30 October), the last year is read too: returns() carries it forward
    years = sorted({common.PUF_YEAR, common.TARGET_YEAR, min(common.TARGET_YEAR, total_years()[-1])})
    t = pd.concat([read('1.4', years), read('1.2', years), read('3.3', years)], ignore_index=True)
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


def returns(soi, year, table='1.4', status='all'):
    """The number of returns, all AGI, in one table and filing status. Past SOI's last year (30 October): the last
    year's count times CBO's growth in the number of returns (all_returns), the mix of filing statuses kept."""
    last = total_years()[-1]
    if year > last:
        return returns(soi, last, table, status) * all_returns(year) / all_returns(last)
    s = soi[(soi.year == year) & (soi.table.astype(str) == table) & (soi.filing_status == status)
            & (soi.label == 'all') & (soi.variable == 'returns')]
    return s['value'].item()


def published_cells(soi, year, variable, stat, table='1.4'):
    """An item's cells by AGI as published in a table (Table 1.4 unless said), lowest first, as (lo, hi, value).
    A non-blank 'combined' cell holds its own bracket and the blank ones after it,
    so it becomes one cell spanning them."""
    s = soi[(soi['year'] == year) & (soi['table'].astype(str) == table) & (soi['filing_status'] == 'all')
            & (soi['variable'] == variable) & (soi['statistic'] == stat) & (soi['label'] != 'all')]
    out = []
    for r in s.sort_values('lo').itertuples():
        if pd.isna(r.value):
            assert out and out[-1][3], f'{year} {variable} {stat}: a blank cell with no cell to join'
            out[-1] = (out[-1][0], r.hi, out[-1][2], True)
        else:
            out.append((r.lo, r.hi, float(r.value), r.flag == 'combined'))
    return [(lo, hi, v) for lo, hi, v, _ in out]


# ---- 2. totals for growth ------------------------------------------------------------------
NET = lambda name: [(name + ' > net income', 1), (name + r' > net loss.*', -1)]
SOI_ITEMS = {  # item: list of (SOI item pattern, sign)
    'wages': [('(salaries and wages|total wages > total)', 1)],   # SOI's name from tax year 2022
    'txbl_int': [('taxable interest', 1)],
    'dividends': [('ordinary dividends', 1)],
    'sch_d_net_gain': [('sales of capital assets reported on form 1040, schedule d > taxable net gain', 1)],
    'kg_dist': [('capital gain distributions reported on form 1040', 1)],
    'sch_cf_income': NET('business or profession') + NET('farm'),
    'sch_e_income': NET('partnership and s corporation') + NET('rent') + NET('royalty') + NET('estate and trust')
    + NET('farm rental'),
    # (28 October) Schedule E's two parts the SCF shares: rents and royalties, and the rest
    'sche_rent': NET('rent') + NET('royalty'),
    'sche_rest': NET('partnership and s corporation') + NET('estate and trust') + NET('farm rental'),
    'business_income': NET('business or profession') + NET('partnership and s corporation') + NET('farm')
    + NET('rent') + NET('royalty') + NET('estate and trust') + NET('farm rental'),
    'other_gains': [('sales of property other than capital assets > net gain', 1),
                    ('sales of property other than capital assets > net loss', -1)],
    'txbl_ira_dist': [(r'(taxable ira distributions|taxable individual retirement arrangement \(ira\) distributions'
                     r'|individual retirement arrangement.*taxable.*)', 1)],
    'txbl_pens_dist': [('pensions and annuities > taxable', 1)],
    'ss_taxable': [('social security benefits > taxable', 1)], 'ui': [('unemployment compensation', 1)],
    'alimony': [('alimony received', 1)], 'state_ref': [('state income tax refunds', 1)],
    'adjustments': [('statutory adjustments > total', 1)],
    'retirement_gross': [('social security benefits > total', 1), ('pensions and annuities > total', 1)],
    'gross_ss': [('social security benefits > total', 1)],
    'exempt_int': [('tax exempt interest', 1)], 'other_agi': NET('other income'),
    # stage 3 (27 October): the two parts of ordinary dividends and the eleven statutory adjustments, each by its own
    # Table 1.4 line (the part "ordinary less qualified" has no line of its own: ordinary dividends')
    'div_pref': [('qualified dividends', 1)], 'div_ord': [('ordinary dividends', 1)],
    **{v: [('statutory adjustments > ' + line, 1)] for v, line in {
        'trad_contr_ira': 'ira payments', 'sl_int_ded': 'student loan interest deduction',
        'ed_exp': 'educator expenses deduction', 'tuition_ded': 'tuition and fees deduction',
        'dpad': 'domestic production activities deduction', 'se_tax_ded': 'deductible part of self employment tax',
        'se_health': 'self employed health insurance deduction', 'hsa_contr': 'health savings account deduction',
        'keogh_contr': 'payments to a keogh plan', 'early_penalty': 'penalty on early withdrawal of savings',
        'alimony_exp': 'alimony paid'}.items()}}
_TOTALS = None


def totals():
    """{(tax year, SOI item): value} for Table 1.4's all-returns totals, every year in the mirror (read once)."""
    global _TOTALS
    if _TOTALS is None:
        t = pd.read_csv(os.path.join(MIRROR, FILES['1.4']), dtype=str)
        t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')]
        _TOTALS = {(int(r.tax_year), r.item.lower().strip()): float(r.value) for r in t.itertuples()}
    return _TOTALS


def total_years():
    return sorted({y for y, _ in totals()})


# Lines SOI later published in pieces: from tax year 2021, partnership and S corporation income separately. A year
# without the combined line adds the pieces up; its return count then counts a return with both twice.
SPLIT = {'partnership and s corporation': ['partnership', 's corporation']}
_GAPS = set()
_HELD = set()   # (line, year) pairs per_return_growth held at an earlier year, each said once


def soi_get(y, pattern, stat):
    val = totals()
    hit = [n for (yy, n) in val if yy == y and re.fullmatch(pattern + ' > ' + stat, n)]
    if len(hit) != 1:
        assert not hit, (y, pattern, hit)
        for whole, pieces in SPLIT.items():
            if pattern.startswith(whole + ' > '):
                return sum(soi_get(y, p + pattern[len(whole):], stat) for p in pieces)
        # a line missing in one year only (pensions in 2018, published with IRAs): the geometric mean of the years
        # either side, a placeholder
        near = [[n for (yy, n) in val if yy == b and re.fullmatch(pattern + ' > ' + stat, n)] for b in (y - 1, y + 1)]
        if all(len(h) == 1 for h in near):
            if (y, pattern, stat) not in _GAPS:
                _GAPS.add((y, pattern, stat))
                common.say(f'SOI {y}: no line {pattern} > {stat}; the geometric mean of {y - 1} and {y + 1}')
            return float(np.sqrt(val[(y - 1, near[0][0])] * val[(y + 1, near[1][0])])) * (1000.0 if stat == 'amount' else 1.0)
        raise KeyError(f'{y}: no SOI item {pattern} > {stat}')
    return val[(y, hit[0])] * (1000.0 if stat == 'amount' else 1.0)


@lru_cache(maxsize=None)
def combined_partnership_count(y, side):
    """Table 4.3's combined count, excluding dependent filers, without counting a return twice."""
    path = os.path.join(common.SETTINGS['paths']['raw']['soi_tables'], 'pctile_top_items.csv')
    t = pd.read_csv(path)
    m = ((t.tax_year == y) & (t.pctile_group == 'all returns') &
         (t.item == 'partnership and s corporation > ' + side) & (t.measure == 'returns'))
    values = t.loc[m, 'value']
    assert len(values) == 1 and pd.notna(values.iloc[0]), f'{path}: missing combined {side} count for {y}'
    return float(values.iloc[0])


def soi_item(y, item, base=None, rows=None, absolute=False, combined_counts=False):
    """Amount and returns of an item in year y, summed over its SOI rows; with
    base, only the rows published in both years (so a ratio compares like with like)."""
    amt = cnt = 0.0
    for p, s in ([(r, 1) for r in rows] if rows is not None else SOI_ITEMS[item]):
        try:
            a, c = soi_get(y, p, 'amount'), soi_get(y, p, 'number of returns')
            if base is not None:
                soi_get(base, p, 'amount')
        except KeyError:
            continue
        if combined_counts and p.startswith('partnership and s corporation > net '):
            # Table 1.4 splits these lines from 2021; adding the two counts double-counts returns with both.
            # Table 4.3 publishes the combined line for all independent filers in both the base and target years.
            side = 'net loss' if 'net loss' in p else 'net income'
            c = combined_partnership_count(y, side)
        amt, cnt = amt + (abs(a) if absolute else s * a), cnt + c
    return amt, cnt


def carried(f, y, concept=None):
    """f(y) for a year in Table 1.4; past its last year, f(last) times CBO's growth of the concept per return (CBO's
    February 2026 individual income tax projections, cbo_growth; 30 October, TARGETS.md section 3: our own last level
    times an outside change). A concept with no projection series stops instead of repeating a past growth rate."""
    last = total_years()[-1]
    if y <= last:
        return f(y)
    if concept is not None:
        return f(last) * cbo_growth(concept, last, y)
    raise ValueError(f'No projection series for {y}: SOI ends in {last}; '
                     'repeating the final observed growth rate is not permitted')


# ---- CBO's projections, past SOI's last year (30 October) -------------------------------------------------------
# CBO's February 2026 individual income tax details (Revenue Projections by Category, sheet 3; calendar years 2022-2036;
# research/rebuild/resources/growth/cbo_individual_income_tax.csv). Each SOI item grows past SOI's last year with the
# CBO line closest to it, per return (CBO's amount over CBO's number of returns): CBO publishes no number with each item.
CBO_CONCEPT = {
    'wages': 'Salaries and wages', 'txbl_int': 'Taxable interest and ordinary dividends',
    'div_ord': 'Taxable interest and ordinary dividends', 'dividends': 'Qualified dividends',
    'div_pref': 'Qualified dividends', 'sch_d_net_gain': 'Capital gain or loss', 'kg_dist': 'Capital gain or loss',
    'business_income': 'Net business income', 'sch_cf_income': 'Net business income', 'sch_e_income': 'Net business income',
    'sche_rent': 'Net business income', 'sche_rest': 'Net business income',
    'txbl_pens_dist': 'Taxable pensions and annuities', 'txbl_ira_dist': 'Taxable pensions and annuities',
    'ss_taxable': 'Taxable Social Security benefits', 'adjustments': 'Subtract statutory adjustments'}
_CBO = None


def cbo_level(concept, y):
    """CBO's projected level of a concept (its line's name starts with concept) in calendar year y."""
    global _CBO
    if _CBO is None:
        _CBO = pd.read_csv(os.path.join(os.path.dirname(common.HERE), 'resources', 'growth', 'cbo_individual_income_tax.csv'))
    d = _CBO[_CBO['concept'].str.startswith(concept) & (_CBO['year'] == y)]
    assert len(d) == 1, f'CBO line {concept!r} in {y}: {len(d)} rows (CBO publishes 2022-2036)'
    return float(d['value'].iloc[0])


def cbo_growth(concept, a, b):
    """CBO's growth of a concept per return from year a to b (a count, 'Number of returns', as itself)."""
    if concept == 'Number of returns':
        return cbo_level(concept, b) / cbo_level(concept, a)
    per = lambda y: cbo_level(concept, y) / cbo_level('Number of returns', y)
    return per(b) / per(a)


def per_return_growth(item, rows=None, absolute=False, year=None):
    """Growth in dollars per return with the item from the PUF year to year (default the target year)."""
    a = common.PUF_YEAR

    def g(b):
        combined = b >= 2021
        (a0, c0), (a1, c1) = (soi_item(a, item, b, rows, absolute, combined),
                              soi_item(b, item, a, rows, absolute, combined))
        if not (c1 > 0 and a1 != 0):
            # (30 October) a line SOI stopped publishing after the law repealed the item (the domestic production
            # activities deduction from 2018-2020, the tuition and fees deduction from 2021): not grown past the last
            # year it was published; the calculator applies the year's law (the item is zero under it)
            prev = [y for y in total_years() if a < y < b and soi_item(y, item, a, rows, absolute)[1] > 0]
            assert prev, f'{item or rows}: no SOI amount in {b} and none between {a} and {b}'
            # (review, 30 October) said in the log once per line and year: this path takes any line with no amount
            # in year b, not only the two repealed deductions it was written for, so it must not pass unseen
            if (str(item or rows), b) not in _HELD:
                _HELD.add((str(item or rows), b))
                common.say(f'SOI {b}: no amount for {item or rows}; held at its growth to {prev[-1]}, the last year '
                           f'it was published (a repealed item, zero under the year\'s law)')
            return g(prev[-1])
        return (a1 / c1) / (a0 / c0)
    if item == 'gross_ss':
        # (2 November) gross benefits past SOI's last year grow with SSA's and CBO's benefits paid, not CBO's taxable
        # benefits (taxable grows faster: the thresholds are not indexed); projection_growth.gross_ss_between stops the
        # run if a year has no source
        import projection_growth as PG
        last = total_years()[-1]
        y = common.TARGET_YEAR if year is None else year
        return g(y) if y <= last else g(last) * PG.gross_ss_between(last, y)
    concept = CBO_CONCEPT.get(item, 'Net business income' if rows is not None and any(
        k in r for r in rows for k in ('partnership', 'business', 'farm', 'rent', 'royalty', 'estate')) else
        'Adjusted gross income')
    return carried(g, common.TARGET_YEAR if year is None else year, concept)


def all_returns(y):
    """Table 1.4's number of returns in year y (past its last year, carried forward)."""
    return carried(lambda b: totals()[(b, 'number of returns')], y, 'Number of returns')


# ---- 3. Table 1.6: returns by filing status and the primary filer's age -----------------------
T16_PANELS = {'joint': ['Returns of married persons filing jointly and returns of surviving spouses',
                        'Returns of married persons filing jointly', 'Returns of surviving spouses'],   # before 2015: two panels
              'separate': ['Returns of married persons filing separately'],
              'head_of_household': ['Returns of heads of households'], 'single': ['Returns of single persons']}
T16_ROWS = ['Under 26', '26 under 35', '35 under 45', '45 under 55', '55 under 65', '65 and over']   # the adult ranges
_T16 = None


def table_1_6(y):
    """{(filing status, adult age range 0-5): returns}, all AGI, year y (past the last year, carried forward)."""
    global _T16
    if _T16 is None:
        t = pd.read_csv(common.SETTINGS['ages']['table_1_6'], dtype=str, keep_default_na=False)
        status = {p: s for s, ps in T16_PANELS.items() for p in ps}
        t = t[(t.col_seq == '1') & t.panel.isin(status) & t.row_label.isin(T16_ROWS)]
        assert (t.value != '').all(), 'Table 1.6: a blank all-AGI cell'
        t = t.assign(status=t.panel.map(status), year=t.tax_year.astype(int), value=t.value.astype(float))
        _T16 = t.groupby(['year', 'status', 'row_label'])['value'].sum().to_dict()
    # past the last year: each cell grows with CBO's number of returns (30 October), so the last year's mix of filing
    # status and age is kept (no projection of returns by age exists)
    return {(s, k): carried(lambda b: _T16[(b, s, row)], y, 'Number of returns') for s in T16_PANELS
            for k, row in enumerate(T16_ROWS)}


if __name__ == '__main__':
    print(build().groupby(['year', 'table', 'variable', 'statistic']).size().to_string())
