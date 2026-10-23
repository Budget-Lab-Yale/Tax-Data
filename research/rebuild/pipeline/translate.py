"""Translation: the table, function 1 and the report (settings "translation").

The table has one row per survey and variable it shares with the PUF. Every
row is compared with, and translated against, the tax data for the survey's
year (build_table.py, tax_data): PUF filers other than dependent filers, their
weights scaled by SOI Table 1.6 (filing status x age range, year y's returns
over the PUF year's in each cell), every dollar item grown per return with the
item, that year's non-filer tax units appended.

  definition fixes  loss_cap: losses read as no more than this (capital losses
                    past $3,000 are not on the return).
  function 1        amounts (amounts()): one setting, the dollar line, in
                    PUF-year dollars grown to the survey's year as the tax data
                    is. An amount between minus the line and the line keeps
                    its dollars. A positive amount above the line gets the tax
                    data's positive amount with the same number of units
                    (weighted) above it; a loss below minus the line, the tax
                    data's loss with the same number of units below it. No line
                    is dollars throughout; a line of zero is rank throughout.
                    The result is in the survey year's tax-data dollars, cut on
                    the PUF's band cut points grown to that year.
  function 2        holding: the two-slot fill-in (fillin.py), run in training
                    on function 1's bands, for rows with fill_in.

The report (report_rows()): for each row and survey year, the survey as
reported, after function 1 and the tax data, each as: units, the share with
none, the share with a loss, millions of units in size ranges (report_lines),
millions above the positive lines, the mean per unit, and the top 1% and 10%
of units' shares of the positive dollars. Categories: the share in each value.
Written by build_table.py to common.DATA/translation_report.csv;
report_translation.py adds the stage after function 2 from a trained run and
writes all four stages to model/<run>/translation_report.md.
"""
import numpy as np
import pandas as pd

import common

S = common.SETTINGS['translation']
TABLE = S['table']
CATEGORY_VARIABLES = ['married', 'n_dep', 'age_range', 'k1_owner', 'filer', 'age_primary', 'age_spouse']
# Exact ages (value 0 none, else the age plus 1, 80 and over as 80): the tax data has none, so the report compares each
# survey year with the CPS of the nearest year, in the groups AGE_GROUPS (lower edge inclusive, years).
EXACT_AGES = ['age_primary', 'age_spouse']
AGE_GROUPS = [(0, 26, 'under 26'), (26, 35, '26-34'), (35, 45, '35-44'), (45, 55, '45-54'), (55, 65, '55-64'),
              (65, 70, '65-69'), (70, 75, '70-74'), (75, 80, '75-79'), (80, 200, '80 and over')]
_seen = set()
for _r in TABLE:
    _key = (_r['survey'], _r['variable'])
    assert _key not in _seen, f'translation table: {_key} twice'
    _seen.add(_key)
    if _r['variable'] in CATEGORY_VARIABLES:
        assert not {'dollar_line', 'loss_cap', 'fill_in'} & set(_r), f'{_key}: a category has no amount settings'
    else:
        assert {'dollar_line', 'loss_cap', 'fill_in'} <= set(_r), f'{_key}: an amount row needs all three settings'
        assert not (_r['fill_in'] and _r['type_for_fill_in']), f'{_key}: a type variable cannot have the fill-in'
    assert _r.get('reason'), f'{_key}: no reason'


def rows(survey=None, amounts_only=False):
    return [r for r in TABLE if (survey is None or r['survey'] == survey)
            and not (amounts_only and r['variable'] in CATEGORY_VARIABLES)]


def filled():
    """The rows with function 2."""
    return [r for r in TABLE if r.get('fill_in')]


def fill_type():
    """The variables function 2 conditions on (with the survey year)."""
    return list(dict.fromkeys(r['variable'] for r in TABLE
                              if r['type_for_fill_in'] and r['survey'] in {f['survey'] for f in filled()}))


def definition_fixes(x, row):
    """The row's definition fixes applied to the survey's amount (its items summed)."""
    if row['loss_cap'] is not None:
        x = np.where(x < row['loss_cap'], row['loss_cap'], x)
    return x


def _by_count(x, w, tx, tw):
    """For each x (all one sign, as magnitudes), the tax data's magnitude with the same weighted count above it."""
    o = np.argsort(-tx, kind='stable')
    t_val, t_above = tx[o], np.cumsum(tw[o]) - tw[o] / 2
    so = np.argsort(-x, kind='stable')
    above = np.empty(len(x))
    above[so] = np.cumsum(w[so]) - w[so] / 2
    return np.interp(above, t_above, t_val), above > t_above[-1]


def amounts(x, w, tax_x, tax_w, line):
    """Function 1: x (one survey year, survey-year dollars) on the tax data's scale beyond the dollar line (in the
    same year's dollars; None: unchanged). Also the weighted count of units placed past the tax data's last holder
    of their sign (they get its smallest amount)."""
    out = x.astype(float).copy()
    beyond = 0.0
    if line is None:
        return out, beyond
    for sign in (1, -1):
        sel = sign * x > line
        tsel = sign * tax_x > 0
        if not sel.any():
            continue
        assert tsel.any(), 'survey amounts of a sign the tax data does not have'
        # rank over all of the survey's units of this sign, so a unit's count includes those within the line
        mine = sign * x > 0
        val, past = _by_count(sign * x[mine], w[mine], sign * tax_x[tsel], tax_w[tsel])
        new = np.zeros(len(x))
        new[mine] = sign * val
        out[sel] = new[sel]
        beyond += float(w[mine][past & sel[mine]].sum())
    return out, beyond


# ---- the report ------------------------------------------------------------------------------
LINES = S['report_lines']


def _dollars(x):
    return f'-${-x:,.0f}' if x < 0 else f'${x:,.0f}'


def _ranges():
    neg = [l for l in LINES if l < 0]
    pos = [l for l in LINES if l > 0]
    out = [(-np.inf, neg[0], f'below {_dollars(neg[0])}')] if neg else [(-np.inf, 0, 'a loss')]
    out += [(a, b, f'{_dollars(a)} to {_dollars(b)}') for a, b in zip(neg, neg[1:] + [0])]
    out += [(a, b, f'{_dollars(a)} to {_dollars(b)}') for a, b in zip([0] + pos[:-1], pos)]
    out.append((pos[-1], np.inf, f'{_dollars(pos[-1])} and over'))
    return out


def describe(x, w):
    """The report's statistics of an amount (weights in units)."""
    W = w.sum()
    d = {'units (millions)': W / 1e6, 'none (%)': 100 * w[x == 0].sum() / W, 'a loss (%)': 100 * w[x < 0].sum() / W}
    for lo, hi, lab in _ranges():
        m = (x > lo) & (x < hi) if lo == 0 else (x >= lo) & (x < hi)
        d[f'{lab} (millions)'] = w[m & (x != 0)].sum() / 1e6
    for l in [l for l in LINES if l > 0]:
        d[f'above {_dollars(l)} (millions)'] = w[x >= l].sum() / 1e6
    d['mean per unit ($)'] = float((w * x).sum() / W)
    pos = np.where(x > 0, x, 0.0)
    total = (w * pos).sum()
    o = np.argsort(-pos, kind='stable')
    cw, cx = np.cumsum(w[o]) / W, np.cumsum((w * pos)[o])
    for p in (1, 10):
        k = np.searchsorted(cw, p / 100)
        d[f'top {p}% of units, share of positive dollars (%)'] = 100 * cx[min(k, len(cx) - 1)] / total if total > 0 else np.nan
    return d


def describe_category(x, w, probs=None):
    """Share of units in each value; probs (n, values) where a record's value is a probability spread."""
    if probs is None:
        vals = sorted(np.unique(x[x >= 0]).astype(int))
        probs = np.column_stack([(x == v).astype(float) for v in vals])
    else:
        vals = range(probs.shape[1])
    return {f'value {v} (%)': 100 * (w * probs[:, i]).sum() / w.sum() for i, v in enumerate(vals)}


def describe_age(x, w):
    """An exact-age slot: the share with none, and among units with an age, the share in each age group and the mean
    age (80 and over counted as 80)."""
    x, w = np.asarray(x, float), np.asarray(w, float)
    has = x > 0
    age = x - 1
    d = {'none (%)': 100 * w[~has].sum() / w.sum()}
    for lo, hi, lab in AGE_GROUPS:
        d[f'{lab} (% of units with an age)'] = 100 * w[has & (age >= lo) & (age < hi)].sum() / w[has].sum()
    d['mean age'] = float((w[has] * age[has]).sum() / w[has].sum())
    return d


def report_rows(row, year, stages):
    """Long rows of the report: stages = {stage: dict of statistics}."""
    return [{'survey': row['survey'], 'variable': row['variable'], 'year': year, 'stage': st, 'statistic': k, 'value': v}
            for st, d in stages.items() for k, v in d.items()]


def say_report(rep):
    """The report, one block per row and year, stages side by side."""
    for (s, v, y), g in rep.groupby(['survey', 'variable', 'year'], sort=False):
        wide = g.pivot_table(index='statistic', columns='stage', values='value', sort=False)
        common.say(f'--- {s} {y} {v}\n' + wide.round(2).to_string())
