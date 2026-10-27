"""Translation: the table, function 1 and the report (settings "translation").

The table has one row per survey and variable it shares with the PUF. Every
row is compared with, and translated against, the tax data for the survey's
year (build_table.py, tax_data): PUF filers other than dependent filers, their
weights scaled by SOI Table 1.6 (filing status x age range, year y's returns
over the PUF year's in each cell), every dollar item grown per return with the
item, and the CPS units of that year (or the nearest CPS year), each weighted by
its chance of not filing.

  items             the survey's columns, summed; none (an empty list) is zero:
                    an item the survey does not ask.
  definition fixes  in this order: first_year, the survey asks the item from
                    this tax year (earlier years read as zero); part, the
                    positive part ("positive") or the size of the loss
                    ("loss", stored positive as the PUF's loss fields are);
                    loss_cap, losses read as no more than this (capital losses
                    past $3,000 are not on the return); less_traditional_
                    deferrals (since 28 October), the link wage: the column of
                    each unit's elective deferrals (traditional plus Roth),
                    whose traditional part, elective x (1 - the Roth share of
                    the year and of joint or non-joint returns, settings
                    deferrals), is subtracted; missing where the survey does
                    not ask (nothing subtracted).
  function 1        amounts (amounts()): the dollar line, in PUF-year dollars
                    grown to the survey's year as the tax data is, and the
                    match beyond it. An amount between minus the line and the
                    line keeps its dollars. Beyond the line, match "count" (the
                    default): a positive amount above the line gets the tax
                    data's positive amount with the same number of units
                    (weighted) above it; a loss below minus the line, the tax
                    data's loss with the same number of units below it. Match
                    "percentile" (since 28 October; wages): the survey's unit at
                    the p-th percentile of its units beyond the line gets the
                    tax data's p-th percentile of its units beyond the line, so
                    the survey's share beyond the line is kept, the order of
                    units is kept and the map is continuous at the line (the
                    count match with a line above zero counts a unit's place
                    among all the survey's holders, so it can put a unit just
                    above the line below the line: explore/2026-10-27/mapping/
                    AUDIT.md). Equal survey amounts get one value. No line is
                    dollars throughout; a line of zero is rank throughout. The
                    result is in the survey year's tax-data dollars, cut on
                    the PUF's band cut points grown to that year.
  function 2        holding: the two-slot fill-in (fillin.py), run in training
                    on function 1's bands, for rows with fill_in.

Rows shared between two surveys and not with the PUF (shared_with: the survey that owns the slot; the SIPP's
spouse_wage_share, which the CPS owns) have no tax data to be translated against: they are taken as reported and cut
on the owner's cut points of the nearest year (build_table.shared_between_surveys), and the report compares the two
surveys (describe_share). rows(amounts_only=True) leaves them out.

The report (report_rows()): for each row and survey year, the survey as
reported (its items summed), the link (after the definition fixes: for wages,
gross wages less the traditional deferrals the survey reports), after function
1 and the tax data, each as: units, the share with none, the share with a loss,
millions of units in size ranges (report_lines), millions above the positive
lines, the mean per unit, holders' amounts at percentiles (p10 to p99), and the
top 1% and 10% of units' shares of the positive dollars; after function 1 also
how far function 1 moved the link amounts (move()). Categories: the share in
each value. Written by build_table.py to common.DATA/translation_report.csv;
report_translation.py adds the stage after function 2 from a trained run and
writes all stages to model/<run>/translation_report.md.
"""
import numpy as np
import pandas as pd

import common

S = common.SETTINGS['translation']
TABLE = S['table']
CATEGORY_VARIABLES = ['married', 'n_dep', 'age_range', 'k1_owner', 'filer', 'age_primary', 'age_spouse', 'mars', 'n_ctc',
                      'n_eitc']
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
    if _r.get('shared_with'):
        assert not {'dollar_line', 'loss_cap', 'fill_in'} & set(_r), f'{_key}: a row shared with a survey has no amount settings'
        assert not _r['type_for_fill_in'], f'{_key}: a row shared with a survey is not a type for the fill-in'
    elif _r['variable'] in CATEGORY_VARIABLES:
        assert not {'dollar_line', 'loss_cap', 'fill_in'} & set(_r), f'{_key}: a category has no amount settings'
    else:
        assert {'dollar_line', 'loss_cap', 'fill_in'} <= set(_r), f'{_key}: an amount row needs all three settings'
        assert not (_r['fill_in'] and _r['type_for_fill_in']), f'{_key}: a type variable cannot have the fill-in'
        assert _r.get('part') in (None, 'positive', 'loss'), f'{_key}: part is positive, loss or absent'
        # rank_against (stage 3, 27 October): function 1 matches the survey's amount to the tax data's amount of this
        # PUF variable, a computed total, instead of the row's own: a survey that reports only a total puts it in one
        # of the total's parts (the CPS's dividends, in qualified dividends) without changing its translated dollars
        assert _r.get('rank_against') is None or isinstance(_r['rank_against'], str), f'{_key}: rank_against is a name'
        # match (28 October): how amounts beyond the dollar line are matched; a match needs a line
        assert _r.get('match', 'count') in ('count', 'percentile'), f'{_key}: match is count or percentile'
        assert _r.get('match') is None or _r['dollar_line'] is not None, f'{_key}: a match with no dollar line'
        # less_traditional_deferrals (28 October): a column name, the unit's elective deferrals
        assert _r.get('less_traditional_deferrals') is None or isinstance(_r['less_traditional_deferrals'], str), \
            f'{_key}: less_traditional_deferrals is a column name'
    assert _r.get('reason'), f'{_key}: no reason'

DEFERRALS = common.SETTINGS['deferrals']


def rows(survey=None, amounts_only=False):
    """The table's rows (of one survey); amounts_only: the amounts shared with the PUF."""
    return [r for r in TABLE if (survey is None or r['survey'] == survey)
            and not (amounts_only and (r['variable'] in CATEGORY_VARIABLES or r.get('shared_with')))]


def shared_rows(survey=None):
    """The rows shared between two surveys and not with the PUF (shared_with)."""
    return [r for r in TABLE if r.get('shared_with') and (survey is None or r['survey'] == survey)]


def filled():
    """The rows with function 2."""
    return [r for r in TABLE if r.get('fill_in')]


def fill_type():
    """The variables function 2 conditions on (with the survey year)."""
    return list(dict.fromkeys(r['variable'] for r in TABLE
                              if r['type_for_fill_in'] and r['survey'] in {f['survey'] for f in filled()}))


def columns(row):
    """The survey columns a row reads: its items and its deferral column."""
    return list(row['items']) + ([row['less_traditional_deferrals']] if row.get('less_traditional_deferrals') else [])


def roth_share(year, married):
    """The Roth share of elective deferrals in tax year year (settings deferrals.roth_share, SOI's W-2 statistics; the
    W-2 year nearest year, the earlier on a tie), for each unit: joint returns where married is 1, else non-joint."""
    have = sorted(int(y) for y in DEFERRALS['roth_share'])
    wy = str(min(have, key=lambda c: (abs(c - year), c)))
    r = DEFERRALS['roth_share'][wy]
    return np.where(np.asarray(married) == 1, r['joint'], r['nonjoint'])


def traditional_deferrals(elective, year, married):
    """The traditional part of elective deferrals (traditional plus Roth): elective x (1 - the Roth share). Missing
    (not asked) stays missing."""
    return np.asarray(elective, float) * (1 - roth_share(year, married))


def reported_amount(d, row, year):
    """The row's items (d's raw_<item> columns) summed, as the survey reports them; zero with no items."""
    x = np.zeros(len(d))
    for c in row['items']:
        x = x + d[f'raw_{c}'].to_numpy(float)
    assert not np.isnan(x).any(), f'{row["survey"]} {year} {row["variable"]}: missing values'
    return x


def deferrals_subtracted(d, row, year):
    """The traditional deferrals the row's definition fix subtracts from each unit (zero where the survey does not ask:
    the deferral column is missing there), and the units not asked (a boolean array). None for a row without the fix."""
    c = row.get('less_traditional_deferrals')
    if not c:
        return None, None
    e = d[f'raw_{c}'].to_numpy(float)
    asked = ~np.isnan(e)
    assert (e[asked] >= 0).all(), f'{row["survey"]} {year} {c}: deferrals below zero'
    t = np.where(asked, traditional_deferrals(np.where(asked, e, 0.0), year, d['married'].to_numpy()), 0.0)
    return t, ~asked


def survey_amount(d, row, year):
    """The row's amount on the survey's records d (one survey year): its items (d's raw_<item> columns) summed, zero
    with no items, then the definition fixes."""
    x = reported_amount(d, row, year)
    t, _ = deferrals_subtracted(d, row, year)
    if t is not None:
        assert (t <= x + 1e-6).all(), f'{row["survey"]} {year} {row["variable"]}: deferrals above the amount'
        x = np.maximum(x - t, 0.0)
    return definition_fixes(x, row, year)


def definition_fixes(x, row, year):
    """The row's definition fixes applied to the survey's amount (its items summed, less any traditional deferrals) in
    tax year year."""
    if row.get('first_year') is not None and year < row['first_year']:
        x = np.zeros(len(x))
    if row.get('part') == 'positive':
        x = np.maximum(x, 0.0)
    elif row.get('part') == 'loss':
        x = np.maximum(-x, 0.0)
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


def _mid_percentile(x, w):
    """Each x's weighted percentile (0-1) among the x: the middle of its weight, equal values sharing one percentile
    (the middle of their joint weight)."""
    vals, inv = np.unique(x, return_inverse=True)
    wv = np.bincount(inv, weights=w, minlength=len(vals))
    mid = (np.cumsum(wv) - wv / 2) / wv.sum()
    return mid[inv]


def _by_percentile(x, w, tx, tw):
    """For each x (all one sign, as magnitudes), the tax data's magnitude at the same weighted percentile among its
    own (tx); equal x get one value. Linear between the tax data's mid-points, flat beyond its first and last."""
    o = np.argsort(tx, kind='stable')
    t_val, t_pct = tx[o], (np.cumsum(tw[o]) - tw[o] / 2) / tw.sum()
    return np.interp(_mid_percentile(x, w), t_pct, t_val)


def amounts(x, w, tax_x, tax_w, line, match='count'):
    """Function 1: x (one survey year, survey-year dollars) on the tax data's scale beyond the dollar line (in the
    same year's dollars; None: unchanged), by match (count from the top, or percentile among the units beyond the
    line). Also the weighted count of units placed past the tax data's last holder of their sign (count only: they
    get its smallest amount)."""
    out = x.astype(float).copy()
    beyond = 0.0
    if line is None:
        return out, beyond
    for sign in (1, -1):
        sel = sign * x > line
        if not sel.any():
            continue
        if match == 'percentile':
            # among the units beyond the line in each data set: the survey's share beyond the line is kept, and the
            # order of units and continuity at the line with it
            tsel = sign * tax_x > line
            assert tsel.any(), 'survey amounts beyond a line the tax data has none beyond'
            out[sel] = sign * _by_percentile(sign * x[sel], w[sel], sign * tax_x[tsel], tax_w[tsel])
            o = np.argsort(sign * x[sel], kind='stable')
            assert (np.diff(sign * out[sel][o]) >= -1e-9 * np.abs(out[sel][o][1:])).all(), 'the order of units not kept'
            assert (sign * out[sel] >= line).all(), 'a unit beyond the line placed within it'
            continue
        tsel = sign * tax_x > 0
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
PERCENTILES = [10, 25, 50, 75, 90, 99]


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
    d['with a positive amount (%)'] = 100 * w[x > 0].sum() / W
    # holders' amounts: percentiles among the units with a positive amount (since 28 October)
    h = x > 0
    hq = common.wquantile(x[h], w[h], [p / 100 for p in PERCENTILES]) if h.any() else [np.nan] * len(PERCENTILES)
    for p, q in zip(PERCENTILES, hq):
        d[f'holders p{p} ($)'] = float(q)
    pos = np.where(x > 0, x, 0.0)
    total = (w * pos).sum()
    o = np.argsort(-pos, kind='stable')
    cw, cx = np.cumsum(w[o]) / W, np.cumsum((w * pos)[o])
    for p in (1, 10):
        k = np.searchsorted(cw, p / 100)
        d[f'top {p}% of units, share of positive dollars (%)'] = 100 * cx[min(k, len(cx) - 1)] / total if total > 0 else np.nan
    return d


def move(link, mapped, w):
    """How far function 1 moved the link amounts, among the units with a positive link amount: the total, the weighted
    median of each unit's mapped over link, and the shares of those units moved down or up by more than 10%."""
    h = link > 0
    if not h.any():
        return {}
    r, wh = mapped[h] / link[h], w[h]
    return {'move: total, mapped over link (%)': 100 * ((wh * mapped[h]).sum() / (wh * link[h]).sum() - 1),
            'move: median unit, mapped over link (%)': 100 * (float(common.wquantile(r, wh, [0.5])[0]) - 1),
            'move: units down by more than 10% (%)': 100 * wh[r < 0.9].sum() / wh.sum(),
            'move: units up by more than 10% (%)': 100 * wh[r > 1.1].sum() / wh.sum()}


def describe_share(x, w):
    """A share from 0 to 1, recorded on some units only (absent, NaN, elsewhere): units, the share of units with it
    recorded, and among those the share at 0 and at 1, the mean, and the 10th, 50th and 90th percentiles."""
    x, w = np.asarray(x, float), np.asarray(w, float)
    ok = ~np.isnan(x)
    d = {'units (millions)': w.sum() / 1e6, 'recorded (%)': 100 * w[ok].sum() / w.sum()}
    if ok.any():
        xs, ws = x[ok], w[ok]
        d['at 0 (% of recorded)'] = 100 * ws[xs == 0].sum() / ws.sum()
        d['at 1 (% of recorded)'] = 100 * ws[xs == 1].sum() / ws.sum()
        d['mean'] = float((xs * ws).sum() / ws.sum())
        for p, q in zip((10, 50, 90), common.wquantile(xs, ws, [0.1, 0.5, 0.9])):
            d[f'p{p}'] = float(q)
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
