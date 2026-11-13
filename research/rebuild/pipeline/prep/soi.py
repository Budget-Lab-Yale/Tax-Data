"""Data preparation: every published SOI number the pipeline uses, read from the shared mirror of SOI's tables.

SOI publishes amounts in thousands of dollars; this module converts them to dollars. build() writes the reweighting's
SOI targets by size of AGI (Tables 1.4, 1.2 and 3.3, for the PUF year and the target year) to targets/soi.csv. The
other functions are lookups that later stages call:
- per_return_growth: an item's growth in dollars per return with the item, from Table 1.4's all-returns totals;
- all_returns: the number of returns;
- table_1_6: the number of returns by filing status and the primary filer's age range.
Past SOI's last published year, an item grows with the closest line of CBO's projections, per return; past the last
CBO year used, with an aggregate from Macro-Projections. FEATURES.md, "SOI tables and their errors", describes the
module's use.
"""
import os
import re
from functools import lru_cache

import numpy as np
import pandas as pd

from shared import common

MIRROR = common.RAW['soi_mirror']
# The business income families: each family's Table 1.4 line, and its PUF parts with income and with losses (losses
# are stored as positive amounts).
FAMILIES = {
    'sole_prop': {'soi': 'business or profession', 'income': ['sole_prop'], 'loss': []},
    'pship_scorp': {'soi': 'partnership and s corporation',
                    'income': ['part_passive', 'part_active', 'scorp_passive', 'scorp_active'],
                    'loss': ['part_passive_loss', 'part_active_loss', 'part_179', 'scorp_passive_loss', 'scorp_active_loss',
                             'scorp_179']},
    'rent_royalty': {'soi': 'total rental and royalty', 'income': ['rent'], 'loss': ['rent_loss']},
    'estate_trust': {'soi': 'estate and trust', 'income': ['estate'], 'loss': ['estate_loss']},
    'farm': {'soi': 'farm', 'income': ['farm'], 'loss': []},
    'farm_rental': {'soi': 'farm rental', 'income': ['farm_rent'], 'loss': []},
    # the two halves of pship_scorp, which Table 1.4 publishes separately from tax year 2021 on
    'partnership': {'soi': 'partnership', 'income': ['part_passive', 'part_active'],
                    'loss': ['part_passive_loss', 'part_active_loss', 'part_179']},
    'scorp': {'soi': 's corporation', 'income': ['scorp_passive', 'scorp_active'],
              'loss': ['scorp_passive_loss', 'scorp_active_loss', 'scorp_179']}}
# The Table 1.4 lines read for the targets: SOI's line -> (the pipeline's name, statistic). Lines named differently
# in some years appear once under each name.
T14 = {
    'number of returns': ('returns', 'count'),
    'social security benefits > total > number of returns': ('gross_ss', 'count'),
    'social security benefits > total > amount': ('gross_ss', 'amount'),
    'adjusted gross income less deficit': ('agi', 'amount'),
    'salaries and wages > number of returns': ('wages', 'count'),
    'salaries and wages > amount': ('wages', 'amount'),
    # SOI's name for the same line from tax year 2022 on (Form 1040 line 1z, total wages)
    'total wages > total > number of returns': ('wages', 'count'),
    'total wages > total > amount': ('wages', 'amount'),
    'taxable interest > number of returns': ('taxable_interest', 'count'),
    'taxable interest > amount': ('taxable_interest', 'amount'),
    'statutory adjustments > total > number of returns': ('statutory_adjustments', 'count'),
    'statutory adjustments > total > amount': ('statutory_adjustments', 'amount'),
    'tax exempt interest > number of returns': ('tax_exempt_interest', 'count'),
    'tax exempt interest > amount': ('tax_exempt_interest', 'amount'),
    'unemployment compensation > number of returns': ('unemployment', 'count'),
    'unemployment compensation > amount': ('unemployment', 'amount'),
    'taxable individual retirement arrangement (ira) distributions > number of returns': ('taxable_ira', 'count'),
    'taxable individual retirement arrangement (ira) distributions > amount': ('taxable_ira', 'amount'),
    'pensions and annuities > total > number of returns': ('total_pensions', 'count'),
    'pensions and annuities > total > amount': ('total_pensions', 'amount'),
    'pensions and annuities > taxable > number of returns': ('taxable_pensions', 'count'),
    'pensions and annuities > taxable > amount': ('taxable_pensions', 'amount'),
    'capital gain distributions reported on form 1040 > number of returns': ('capital_gain_distributions', 'count'),
    'capital gain distributions reported on form 1040 > amount': ('capital_gain_distributions', 'amount'),
    'qualified dividends > number of returns': ('qualified_dividends', 'count'),
    'qualified dividends > amount': ('qualified_dividends', 'amount'),
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
    # the lines the tax calculator computes (taxes.py)
    'taxable income > number of returns': ('taxable_income', 'count'),
    'taxable income > amount': ('taxable_income', 'amount'),
    'income tax before credits > number of returns': ('income_tax_before_credits', 'count'),
    'income tax before credits > amount': ('income_tax_before_credits', 'amount'),
    'total itemized deductions > number of returns': ('itemized_deductions', 'count'),
    'total itemized deductions > amount': ('itemized_deductions', 'amount'),
    'qualified business income deduction > number of returns': ('qbi_deduction', 'count'),
    'qualified business income deduction > amount': ('qbi_deduction', 'amount'),
    'alternative minimum tax > number of returns': ('alternative_minimum_tax', 'count'),
    'alternative minimum tax > amount': ('alternative_minimum_tax', 'amount'),
    'sales of property other than capital assets > net gain > number of returns': ('other_gains_net_gain', 'count'),
    'sales of property other than capital assets > net gain > amount': ('other_gains_net_gain', 'amount'),
    'sales of property other than capital assets > net loss > number of returns': ('other_gains_net_loss', 'count'),
    'sales of property other than capital assets > net loss > amount': ('other_gains_net_loss', 'amount'),
    'state income tax refunds > number of returns': ('state_refunds', 'count'),
    'state income tax refunds > amount': ('state_refunds', 'amount'),
    'alimony received > number of returns': ('alimony_received', 'count'),
    'alimony received > amount': ('alimony_received', 'amount'),
    'statutory adjustments > alimony paid > number of returns': ('alimony_paid', 'count'),
    'statutory adjustments > alimony paid > amount': ('alimony_paid', 'amount'),
}
# Table 3.3 (tax liability, credits and payments by size of AGI): the credits and taxes that the calculator
# computes. The earned income credit is the whole credit: the part used against tax and the refundable part.
T33 = {
    'tax credits > child and other dependent tax credit > number of returns': ('ctc_nonrefundable', 'count'),
    'tax credits > child and other dependent tax credit > amount': ('ctc_nonrefundable', 'amount'),
    'tax credits > total refundable credits > additional child tax credit > number of returns':
        ('ctc_refundable', 'count'),
    'tax credits > total refundable credits > additional child tax credit > amount': ('ctc_refundable', 'amount'),
    # The same two lines under their names in other years: "child tax credit" in 2015 to 2017. From 2021 on, the
    # refundable part is "refundable child tax credit or additional child tax credit".
    'tax credits > child tax credit > number of returns': ('ctc_nonrefundable', 'count'),
    'tax credits > child tax credit > amount': ('ctc_nonrefundable', 'amount'),
    'tax credits > total refundable credits > refundable child tax credit or additional child tax credit > number of '
    'returns': ('ctc_refundable', 'count'),
    'tax credits > total refundable credits > refundable child tax credit or additional child tax credit > amount':
        ('ctc_refundable', 'amount'),
    'tax credits > total refundable credits > earned income credit > number of returns': ('eitc', 'count'),
    'tax credits > total refundable credits > earned income credit > amount': ('eitc', 'amount'),
    'tax credits > credit for child and dependent care > number of returns': ('child_care_credit', 'count'),
    'tax credits > credit for child and dependent care > amount': ('child_care_credit', 'amount'),
    'tax credits > retirement savings contribution credit > number of returns': ('savers_credit', 'count'),
    'tax credits > retirement savings contribution credit > amount': ('savers_credit', 'amount'),
    'tax credits > nonrefundable education credit > number of returns': ('education_credit_nonrefundable', 'count'),
    'tax credits > nonrefundable education credit > amount': ('education_credit_nonrefundable', 'amount'),
    'tax credits > total refundable credits > american opportunity credit > number of returns': ('aotc_refundable', 'count'),
    'tax credits > total refundable credits > american opportunity credit > amount': ('aotc_refundable', 'amount'),
    'all other taxes > self employment tax > number of returns': ('self_employment_tax', 'count'),
    'all other taxes > self employment tax > amount': ('self_employment_tax', 'amount'),
    'income tax after credits > number of returns': ('income_tax_after_credits', 'count'),
    'income tax after credits > amount': ('income_tax_after_credits', 'amount'),
    'total income tax > number of returns': ('total_income_tax', 'count'),
    'total income tax > amount': ('total_income_tax', 'amount'),
}
# Each business income family's net income and net loss lines.
for _fam, _f in FAMILIES.items():
    for _side in ('net income', 'net loss'):
        T14[f'{_f["soi"]} > {_side} > number of returns'] = (f'{_fam}_{_side.replace(" ", "_")}', 'count')
        T14[f'{_f["soi"]} > {_side} > amount'] = (f'{_fam}_{_side.replace(" ", "_")}', 'amount')
# Table 1.2's panels: the filing status of each.
T12_STATUS = {'all returns': 'all',
              'returns of married persons filing jointly and returns of surviving spouses': 'joint',
              'returns of married persons filing separately': 'separate',
              'returns of heads of households': 'head_of_household',
              'returns of single persons': 'single'}
# The mirror's file for each table.
FILES = {'1.4': 'bysize_income_sources.csv', '1.2': 'bysize_marital_status.csv', '3.3': 'bysize_tax_liability.csv'}
TABLE_ITEMS = {'1.4': T14, '1.2': T14, '3.3': T33}   # each table's lines (Table 1.2: only its returns, by T14's name)


# ---- The targets ----

def read(table, years):
    """One table's lines in T14 or T33, for the given years, all returns: one row per AGI class (and filing status,
    for Table 1.2) with the class's bounds, the variable, the statistic, the value in dollars or returns, and SOI's
    flag. The all-returns total is kept with label 'all'; other totals are skipped."""
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
    """Writes the targets of the PUF year and the target year to targets/soi.csv, with the filing statuses other than
    joint summed as 'nonjoint'. When the target year is past SOI's last year, the last year is read too, because
    returns() carries it forward."""
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
    """Reads targets/soi.csv."""
    return pd.read_csv(f'{common.TARGETS}/soi.csv', keep_default_na=False, na_values=[''])


def returns(soi, year, table='1.4', status='all'):
    """The number of returns of all AGI classes, in one table and filing status. Past SOI's last year it is the last
    year's count times CBO's growth in the number of returns (all_returns), so the mix of filing statuses is kept."""
    last = total_years()[-1]
    if year > last:
        return returns(soi, last, table, status) * all_returns(year) / all_returns(last)
    s = soi[(soi.year == year) & (soi.table.astype(str) == table) & (soi.filing_status == status)
            & (soi.label == 'all') & (soi.variable == 'returns')]
    return s['value'].item()


def published_cells(soi, year, variable, stat, table='1.4', status='all'):
    """An item's cells by size of AGI as published in a table, lowest first, as (lo, hi, value). SOI blanks some cells
    to protect taxpayers' privacy and adds their amounts to a neighbouring cell flagged 'combined'. A blanked cell is
    merged with that cell into one cell spanning both."""
    s = soi[(soi['year'] == year) & (soi['table'].astype(str) == table) & (soi['filing_status'] == status)
            & (soi['variable'] == variable) & (soi['statistic'] == stat) & (soi['label'] != 'all')]
    out = []
    pending = None   # a blank cell whose amount SOI put in a later 'combined' cell
    for r in s.sort_values('lo').itertuples():
        if pd.isna(r.value):
            if pending is not None or not out or not out[-1][3]:
                # A blank cell with no combined cell below it is held in the next combined cell above it (Table 3.3,
                # 2015: the child care credit's $5,000-10,000 cell is in the $10,000-15,000 cell).
                pending = (r.lo if pending is None else pending[0], 0.0 if pending is None else pending[1])
                continue
            out[-1] = (out[-1][0], r.hi, out[-1][2], True)
        elif pending is not None:
            # Every cell from the blank one up to the first combined cell becomes one cell (Table 3.3, 2023: the child
            # care credit's no-AGI cell is in the $10,000-15,000 cell, with the zero cells between them).
            pending = (pending[0], pending[1] + float(r.value))
            if r.flag == 'combined':
                out.append((pending[0], r.hi, pending[1], True))
                pending = None
        else:
            out.append((r.lo, r.hi, float(r.value), r.flag == 'combined'))
    assert pending is None, f'{year} {variable} {stat}: a blank lowest cell and no combined cell above it'
    return [(lo, hi, v) for lo, hi, v, _ in out]


# ---- Growth per return, from Table 1.4's totals ----
# An item's growth from one year to another is the change in its dollars per return with the item: the amount over
# the number of returns, both from Table 1.4's all-returns total. An item can be a signed sum of several lines.

NET = lambda name: [(name + ' > net income', 1), (name + r' > net loss.*', -1)]   # net income less net loss
# Each item's lines, as a list of (pattern of SOI's line name, sign) or (pattern, sign, whether the line's returns are
# counted; yes if not given).
SOI_ITEMS = {
    'wages': [('(salaries and wages|total wages > total)', 1)],   # the second is SOI's name from tax year 2022 on
    'txbl_int': [('taxable interest', 1)],
    'dividends': [('ordinary dividends', 1)],
    'sch_d_net_gain': [('sales of capital assets reported on form 1040, schedule d > taxable net gain', 1)],
    'kg_dist': [('capital gain distributions reported on form 1040', 1)],
    'sch_cf_income': NET('business or profession') + NET('farm'),
    'sch_e_income': NET('partnership and s corporation') + NET('rent') + NET('royalty') + NET('estate and trust')
    + NET('farm rental'),
    # the two parts of Schedule E that the SCF shares: rents and royalties, and the rest
    'sche_rent': NET('rent') + NET('royalty'),
    'sche_rest': NET('partnership and s corporation') + NET('estate and trust') + NET('farm rental'),
    'business_income': NET('business or profession') + NET('partnership and s corporation') + NET('farm')
    + NET('rent') + NET('royalty') + NET('estate and trust') + NET('farm rental'),
    'other_gains': [('sales of property other than capital assets > net gain', 1),
                    ('sales of property other than capital assets > net loss', -1)],
    'txbl_ira_dist': [(r'(taxable ira distributions|taxable individual retirement arrangement \(ira\) distributions'
                     r'|individual retirement arrangement.*taxable.*)', 1)],
    'txbl_pens_dist': [('pensions and annuities > taxable', 1)],
    # The non-taxable rest of pensions: total less taxable pensions and annuities, per return with total pensions.
    # The taxable line's returns are not added, because they are among the total's returns.
    'nontxbl_pens_dist': [('pensions and annuities > total', 1), ('pensions and annuities > taxable', -1, False)],
    'ui': [('unemployment compensation', 1)],
    'alimony': [('alimony received', 1)], 'state_ref': [('state income tax refunds', 1)],
    'adjustments': [('statutory adjustments > total', 1)],
    'gross_ss': [('social security benefits > total', 1)],
    'exempt_int': [('tax exempt interest', 1)], 'other_agi': NET('other income'),
    # The two parts of ordinary dividends and the eleven statutory adjustments, each by its own Table 1.4 line. The
    # part "ordinary less qualified dividends" has no line of its own; it grows with ordinary dividends.
    'div_pref': [('qualified dividends', 1)], 'div_ord': [('ordinary dividends', 1)],
    **{v: [('statutory adjustments > ' + line, 1)] for v, line in {
        'trad_contr_ira': 'ira payments', 'sl_int_ded': 'student loan interest deduction',
        'ed_exp': 'educator expenses deduction', 'tuition_ded': 'tuition and fees deduction',
        'dpad': 'domestic production activities deduction', 'se_tax_ded': 'deductible part of self employment tax',
        'se_health': 'self employed health insurance deduction', 'hsa_contr': 'health savings account deduction',
        'keogh_contr': 'payments to a keogh plan', 'early_penalty': 'penalty on early withdrawal of savings',
        'alimony_exp': 'alimony paid'}.items()}}
_TOTALS = None   # Table 1.4's totals, read once


def totals():
    """{(tax year, SOI item): value} for Table 1.4's all-returns totals, every year in the mirror (read once)."""
    global _TOTALS
    if _TOTALS is None:
        t = pd.read_csv(os.path.join(MIRROR, FILES['1.4']), dtype=str)
        t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')]
        _TOTALS = {(int(r.tax_year), r.item.lower().strip()): float(r.value) for r in t.itertuples()}
    return _TOTALS


def total_years():
    """The tax years that Table 1.4's totals cover, in order."""
    return sorted({y for y, _ in totals()})


# Lines SOI publishes in pieces in some years (partnership and S corporation income from tax year 2021 on). In a year
# without the combined line, the pieces are added up. The count of returns then counts a return with both pieces twice.
SPLIT = {'partnership and s corporation': ['partnership', 's corporation']}
_GAPS = set()   # (year, line, statistic) filled from the years either side, each printed once
_HELD = set()   # (line, year) held at an earlier year's growth by soi_growth, each printed once


def soi_get(y, pattern, stat):
    """The all-returns total of the one Table 1.4 line in year y whose name matches pattern, for stat ('amount' or
    'number of returns'), in dollars or returns. A line SOI publishes in pieces that year is the sum of its pieces. A
    line missing in that year only is the geometric mean of the years before and after."""
    val = totals()
    hit = [n for (yy, n) in val if yy == y and re.fullmatch(pattern + ' > ' + stat, n)]
    if len(hit) != 1:
        assert not hit, (y, pattern, hit)
        for whole, pieces in SPLIT.items():
            if pattern.startswith(whole + ' > '):
                return sum(soi_get(y, p + pattern[len(whole):], stat) for p in pieces)
        # A line missing in one year only (pensions in 2018, published with IRAs) is the geometric mean of the years
        # either side.
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
    """Table 4.3's count of returns with partnership or S corporation net income (or net loss, by side), dependent
    filers left out. Unlike the sum of Table 1.4's two lines, it counts a return with both only once."""
    path = os.path.join(common.SETTINGS['paths']['raw']['soi_tables'], 'pctile_top_items.csv')
    t = pd.read_csv(path)
    m = ((t.tax_year == y) & (t.pctile_group == 'all returns') &
         (t.item == 'partnership and s corporation > ' + side) & (t.measure == 'returns'))
    values = t.loc[m, 'value']
    assert len(values) == 1 and pd.notna(values.iloc[0]), f'{path}: missing combined {side} count for {y}'
    return float(values.iloc[0])


def soi_item(y, item, base=None, rows=None, absolute=False, combined_counts=False):
    """The amount and the number of returns of an item in year y, summed over its lines (rows, if given, else the
    item's lines in SOI_ITEMS). With base, only the lines published in both years are summed, so that a ratio
    compares like with like. With absolute, amounts are summed without their signs. With combined_counts, the
    partnership and S corporation counts come from Table 4.3."""
    amt = cnt = 0.0
    for p, s, *counted in ([(r, 1) for r in rows] if rows is not None else SOI_ITEMS[item]):
        # A line that is not published in year y, or in base when it is given, is skipped.
        try:
            a, c = soi_get(y, p, 'amount'), soi_get(y, p, 'number of returns')
            if counted and not counted[0]:
                c = 0.0          # a line subtracted from another whose returns already count it
            if base is not None:
                soi_get(base, p, 'amount')
        except KeyError:
            continue
        if combined_counts and p.startswith('partnership and s corporation > net '):
            # Table 1.4 splits these lines from 2021 on, and adding the two counts would count a return with both
            # twice. Table 4.3 publishes the combined count, for filers other than dependent filers.
            side = 'net loss' if 'net loss' in p else 'net income'
            c = combined_partnership_count(y, side)
        amt, cnt = amt + (abs(a) if absolute else s * a), cnt + c
    return amt, cnt


def carried(f, y, concept=None):
    """f(y) for a year that Table 1.4 covers. Past its last year, f(last year) times CBO's growth of the concept per
    return. Without a concept the run stops: a past growth rate is never repeated."""
    last = total_years()[-1]
    if y <= last:
        return f(y)
    if concept is not None:
        return f(last) * cbo_growth(concept, last, y)
    raise ValueError(f'No projection series for {y}: SOI ends in {last}; '
                     'repeating the final observed growth rate is not permitted')


# ---- CBO's projections, past SOI's last year ----
# CBO's February 2026 individual income tax details (Revenue Projections by Category, sheet 3; calendar years
# 2022-2036; resources/growth/cbo_individual_income_tax.csv). Each SOI item grows with the closest CBO line per return:
# CBO's amount over CBO's total number of returns. CBO publishes no count of the returns with each item.
CBO_CONCEPT = {
    'wages': 'Salaries and wages', 'txbl_int': 'Taxable interest and ordinary dividends',
    'div_ord': 'Taxable interest and ordinary dividends', 'dividends': 'Qualified dividends',
    'div_pref': 'Qualified dividends', 'sch_d_net_gain': 'Capital gain or loss', 'kg_dist': 'Capital gain or loss',
    'business_income': 'Net business income', 'sch_cf_income': 'Net business income', 'sch_e_income': 'Net business income',
    'sche_rent': 'Net business income', 'sche_rest': 'Net business income',
    'txbl_pens_dist': 'Taxable pensions and annuities', 'txbl_ira_dist': 'Taxable pensions and annuities',
    # CBO publishes no line for total or non-taxable pensions; the taxable line is the closest
    'nontxbl_pens_dist': 'Taxable pensions and annuities',
    'adjustments': 'Subtract statutory adjustments'}
_CBO = None   # CBO's projections, read once


def cbo_level(concept, y):
    """CBO's projected level of a concept (its line's name starts with concept) in calendar year y."""
    global _CBO
    if _CBO is None:
        _CBO = pd.read_csv(os.path.join(common.RESOURCES, 'growth', 'cbo_individual_income_tax.csv'))
    d = _CBO[_CBO['concept'].str.startswith(concept) & (_CBO['year'] == y)]
    assert len(d) == 1, f'CBO line {concept!r} in {y}: {len(d)} rows (CBO publishes 2022-2036)'
    return float(d['value'].iloc[0])


# Past the last CBO year used (settings tilt.projection.cbo_last_year), each concept grows with a Macro-Projections
# aggregate per return, as in the production pipeline's long-run projection. The number of returns grows with the
# number of adults, a married couple counted once.
CBO_LONG_RUN = {
    'Salaries and wages': 'gdp_wages', 'Taxable interest and ordinary dividends': 'gdp_interest',
    'Qualified dividends': 'gdp_corp', 'Capital gain or loss': 'gdp_corp',
    'Net business income': 'gdp_proprietors + gdp_rent + gdp_corp', 'Taxable pensions and annuities': 'outlays_mand_oasdi',
    'Subtract statutory adjustments': 'gdp', 'Adjusted gross income': 'gdp'}


def cbo_growth(concept, a, b):
    """CBO's growth of a concept per return from year a to year b. For the concept 'Number of returns' it is the growth
    of the count itself. Past the last CBO year used, the growth of the concept's long-run aggregate (CBO_LONG_RUN)."""
    from scale import projection_growth as PG
    L = PG.cbo_last_year()
    if b > L:
        x = max(a, L)
        head = cbo_growth(concept, a, x) if x > a else 1.0
        if concept == 'Number of returns':
            return head * PG.long_run_growth('units', x, b)
        if concept not in CBO_LONG_RUN:
            raise ValueError(f'CBO concept {concept!r}: no long-run aggregate past {L} (targets.CBO_LONG_RUN)')
        return head * PG.long_run_growth(CBO_LONG_RUN[concept], x, b, per='units')
    if concept == 'Number of returns':
        return cbo_level(concept, b) / cbo_level(concept, a)
    per = lambda y: cbo_level(concept, y) / cbo_level('Number of returns', y)
    return per(b) / per(a)


def soi_growth(item, rows, absolute, b):
    """An item's growth in dollars per return with the item, from the PUF year to year b, from Table 1.4's totals
    alone (b must be a year that Table 1.4 covers). item, rows and absolute are as in soi_item. From 2021 on, the
    partnership and S corporation counts come from Table 4.3."""
    a = common.PUF_YEAR
    combined = b >= 2021
    (a0, c0), (a1, c1) = (soi_item(a, item, b, rows, absolute, combined),
                          soi_item(b, item, a, rows, absolute, combined))
    if not (c1 > 0 and a1 != 0):
        # SOI stops publishing an item's line after the item is repealed (the domestic production activities
        # deduction from 2018, the tuition and fees deduction from 2021). The item is then held at its growth to the
        # last year published. The calculator applies the year's law, under which the item is zero.
        prev = [y for y in total_years() if a < y < b and soi_item(y, item, a, rows, absolute)[1] > 0]
        assert prev, f'{item or rows}: no SOI amount in {b} and none between {a} and {b}'
        # Printed once for each line and year, because every line with no amount in year b takes this path.
        if (str(item or rows), b) not in _HELD:
            _HELD.add((str(item or rows), b))
            common.say(f'SOI {b}: no amount for {item or rows}; held at its growth to {prev[-1]}, the last year '
                       f'it was published (a repealed item, zero under the year\'s law)')
        return soi_growth(item, rows, absolute, prev[-1])
    return (a1 / c1) / (a0 / c0)


def per_return_growth(item, rows=None, absolute=False, year=None):
    """An item's growth in dollars per return with the item, from the PUF year to year (by default the target year).
    The item is a name in SOI_ITEMS, or None with rows, a list of Table 1.4 line patterns. Past Table 1.4's last year
    the item grows with the closest CBO line (CBO_CONCEPT); gross Social Security grows with benefits paid."""
    g = lambda b: soi_growth(item, rows, absolute, b)
    if item == 'gross_ss':
        # Past SOI's last year, gross benefits grow with benefits paid, not with CBO's taxable benefits. Taxable
        # benefits grow faster, because the worksheet's thresholds are not indexed.
        from scale import projection_growth as PG
        last = total_years()[-1]
        y = common.TARGET_YEAR if year is None else year
        return g(y) if y <= last else g(last) * PG.gross_ss_between(last, y)
    concept = CBO_CONCEPT.get(item, 'Net business income' if rows is not None and any(
        k in r for r in rows for k in ('partnership', 'business', 'farm', 'rent', 'royalty', 'estate')) else
        'Adjusted gross income')
    return carried(g, common.TARGET_YEAR if year is None else year, concept)


def all_returns(y):
    """Table 1.4's number of returns in year y. Past the table's last year it is carried forward by CBO's growth in
    the number of returns."""
    return carried(lambda b: totals()[(b, 'number of returns')], y, 'Number of returns')


# ---- Table 1.4A: net short-term and long-term capital losses ----
# Schedule D's net short-term and long-term losses (lines 7 and 15 where negative, loss carryovers included) are
# grown with their own dollars per return with the loss, from Table 1.4A's all-returns totals. Past the table's last
# year they grow with CBO's capital gain or loss per return, so the last year's ratio of losses to gains is kept.
T14A_ITEMS = ('net short term capital loss', 'net long term capital loss')
_T14A = None   # Table 1.4A's all-returns totals, read once


def table_1_4a():
    """{(tax year, SOI item): value} for Table 1.4A's all-returns totals (read once)."""
    global _T14A
    if _T14A is None:
        t = pd.read_csv(os.path.join(MIRROR, 'bysize_capital_assets.csv'), dtype=str)
        t = t[(t['table'] == '1.4A') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')]
        _T14A = {(int(r.tax_year), r.item.lower().strip()): float(r.value) for r in t.itertuples()}
    return _T14A


def loss_growth(item, year):
    """The growth of a net capital loss (T14A_ITEMS) in dollars per return with the loss, from the PUF year to year."""
    assert item in T14A_ITEMS, f'Table 1.4A item {item!r}: not one of {T14A_ITEMS}'
    t = table_1_4a()
    per = lambda b: t[(b, item + ' > amount')] / t[(b, item + ' > number of returns')]
    last = max(y for y, n in t if n == item + ' > amount')
    if year <= last:
        return per(year) / per(common.PUF_YEAR)
    return per(last) / per(common.PUF_YEAR) * cbo_growth('Capital gain or loss', last, year)


# ---- Table 1.6: returns by filing status and the primary filer's age ----
# Before 2015, joint returns and returns of surviving spouses are two panels; they are added together.
T16_PANELS = {'joint': ['Returns of married persons filing jointly and returns of surviving spouses',
                        'Returns of married persons filing jointly', 'Returns of surviving spouses'],
              'separate': ['Returns of married persons filing separately'],
              'head_of_household': ['Returns of heads of households'], 'single': ['Returns of single persons']}
T16_ROWS = ['Under 26', '26 under 35', '35 under 45', '45 under 55', '55 under 65', '65 and over']   # the adult ranges
_T16 = None   # Table 1.6, read once


def table_1_6(y):
    """{(filing status, adult age range 0-5): number of returns}, all AGI classes, in year y. Past the table's last
    year each cell grows with CBO's number of returns, so the last year's mix of filing status and age is kept. No
    projection of returns by age exists."""
    global _T16
    if _T16 is None:
        t = pd.read_csv(common.SETTINGS['ages']['table_1_6'], dtype=str, keep_default_na=False)
        status = {p: s for s, ps in T16_PANELS.items() for p in ps}
        t = t[(t.col_seq == '1') & t.panel.isin(status) & t.row_label.isin(T16_ROWS)]
        assert (t.value != '').all(), 'Table 1.6: a blank all-AGI cell'
        t = t.assign(status=t.panel.map(status), year=t.tax_year.astype(int), value=t.value.astype(float))
        _T16 = t.groupby(['year', 'status', 'row_label'])['value'].sum().to_dict()
    return {(s, k): carried(lambda b: _T16[(b, s, row)], y, 'Number of returns') for s in T16_PANELS
            for k, row in enumerate(T16_ROWS)}


if __name__ == '__main__':
    print(build().groupby(['year', 'table', 'variable', 'statistic']).size().to_string())
