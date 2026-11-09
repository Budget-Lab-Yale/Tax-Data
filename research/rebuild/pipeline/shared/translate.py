"""The translation table, function 1 and the translation report.

Translation puts a survey's amounts on the PUF's terms. The translation table (settings "translation") has one row per
survey variable that is shared with the PUF. A row says which survey items make the amount, the fixes to its
definition, and the dollar line beyond which the survey's amounts are replaced. Each row's survey amount is compared
with the tax data for the survey's year: the PUF's filers, reweighted and grown to that year, plus the CPS's
non-filers. Function 1 (amounts) keeps an amount within the dollar line. Beyond the line it gives the amount the tax
data's amount at the same position: the same weighted count of units above it (counted from the top), or the same
percentile. Function 2, the fill-in, gives amounts to some of the survey's zeros; it is in pretrain/fillin.py.
FEATURES.md, "Translation: putting the surveys on the PUF's terms", describes the method.
"""
import numpy as np

from shared import common

S = common.SETTINGS['translation']
TABLE = S['table']
MATCHES = ('count', 'percentile', 'percentile_from_top')
# The shared variables that are categories, not amounts.
CATEGORY_VARIABLES = ['married', 'n_dep', 'age_range', 'k1_owner', 'filer', 'age1', 'age2', 'filing_status', 'n_dep_ctc',
                      'n_dep_eitc']
# Exact ages (0 for none, else the age plus 1, with 80 meaning 80 and over). The tax data has no exact ages, so the
# report compares each survey year with the CPS of the nearest year, in AGE_GROUPS (the lower edge is in the group).
EXACT_AGES = ['age1', 'age2']
AGE_GROUPS = [(0, 26, 'under 26'), (26, 35, '26-34'), (35, 45, '35-44'), (45, 55, '45-54'), (55, 65, '55-64'),
              (65, 70, '65-69'), (70, 75, '70-74'), (75, 80, '75-79'), (80, 200, '80 and over')]
# Check the translation table's rows when the module is loaded. Each survey and variable appears once. A row shared
# between two surveys, or a category, has no amount settings. An amount row has a dollar line, a loss cap and a
# fill-in setting, and its other settings are of the right kind.
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
        if _r['fill_in']:
            assert set(_r['fill_in']) <= {'size_ranges', 'type_extra'}, f'{_key}: fill_in has size_ranges, type_extra'
            assert all(v in CATEGORY_VARIABLES for v in _r['fill_in'].get('type_extra', [])), \
                f'{_key}: type_extra names categories'
        assert _r.get('part') in (None, 'positive', 'loss'), f'{_key}: part is positive, loss or absent'
        # rank_against: the PUF variable, or the list of variables summed, that the survey amount is matched against
        # in place of the row's own. A survey that reports only a total (the CPS's dividends) is matched against the
        # PUF's total and stored in one of its parts.
        _ra = _r.get('rank_against')
        assert _ra is None or isinstance(_ra, str) or (isinstance(_ra, list) and all(isinstance(x, str) for x in _ra)), \
            f'{_key}: rank_against is a name or a list of names'
        # match: how amounts beyond the dollar line are matched
        assert _r.get('match', 'count') in MATCHES, f'{_key}: match is one of {MATCHES}'
        assert _r.get('match') is None or _r['dollar_line'] is not None, f'{_key}: a match with no dollar line'
        # Beyond a line above zero, amounts must be matched by percentile. With a count from the top, a unit beyond
        # the line could end up with less than a unit within the line, which keeps its own dollars.
        assert not (_r['dollar_line'] and _r.get('match', 'count') == 'count'), \
            f'{_key}: a dollar line above zero needs a percentile match (count from the top breaks the order at the line)'
        # loss_line: the dollar line for losses, in nominal dollars, in place of dollar_line. The SCF's capital gains
        # use it, because losses on the return stop at the $3,000 limit while the slot holds the loss before the limit.
        assert _r.get('loss_line') is None or (_r['dollar_line'] is not None and _r['loss_line'] >= 0), \
            f'{_key}: loss_line is a number of dollars at or above 0, with a dollar line'
        # less_traditional_deferrals: the column of the unit's elective deferrals, whose traditional (pre-tax) part is
        # subtracted from the amount
        assert _r.get('less_traditional_deferrals') is None or isinstance(_r['less_traditional_deferrals'], str), \
            f'{_key}: less_traditional_deferrals is a column name'
    assert _r.get('reason'), f'{_key}: no reason'

DEFERRALS = common.SETTINGS['deferrals']   # the deferral limits and the Roth shares


def rows(survey=None, amounts_only=False):
    """The table's rows (of one survey, if given). With amounts_only, only the amounts shared with the PUF."""
    return [r for r in TABLE if (survey is None or r['survey'] == survey)
            and not (amounts_only and (r['variable'] in CATEGORY_VARIABLES or r.get('shared_with')))]


def shared_rows(survey=None):
    """The rows shared between two surveys and not with the PUF (shared_with)."""
    return [r for r in TABLE if r.get('shared_with') and (survey is None or r['survey'] == survey)]


def filled():
    """The rows that function 2, the fill-in, applies to."""
    return [r for r in TABLE if r.get('fill_in') and in_training(r)]


def fill_type(row):
    """The variables that function 2 conditions on for a row, besides the survey year: the survey's variables marked
    type_for_fill_in, other than the row's own, plus the row's type_extra."""
    own = [r['variable'] for r in TABLE if r['survey'] == row['survey'] and r['type_for_fill_in']
           and r['variable'] != row['variable']]
    return list(dict.fromkeys(own + list(row['fill_in'].get('type_extra', []))))


def against(row):
    """The PUF variables that a row's amount is compared with, summed: rank_against, or else the row's own
    variable."""
    a = row.get('rank_against') or row['variable']
    return [a] if isinstance(a, str) else list(a)


def against_name(row):
    """The tax data's column for against(row): the variable, or its parts joined by '+'."""
    return '+'.join(against(row))


def not_asked(row, year):
    """Whether year is before the row's first_year, so that the survey did not ask the item. The amount is then
    missing in training, not zero."""
    return row.get('first_year') is not None and year < row['first_year']


def in_training(row):
    """Whether the row's item is shown in training at all. A row with in_training false still gives the tax data's
    non-filers their amounts, but it is missing on the survey's training rows in every year."""
    return bool(row.get('in_training', True))


def not_shown(row, year):
    """Whether the row's item is missing on the survey's training rows of the year."""
    return not_asked(row, year) or not in_training(row)


def fills_in(row, year):
    """Whether the row's fill-in runs in the survey year: from its first_year on, and only on a row shown in
    training."""
    return bool(row.get('fill_in')) and in_training(row) and (row.get('first_year') is None or year >= row['first_year'])


def columns(row):
    """The survey columns a row reads: its items and its deferral column."""
    return list(row['items']) + ([row['less_traditional_deferrals']] if row.get('less_traditional_deferrals') else [])


def roth_share(year, married):
    """The Roth share of elective deferrals for each unit, from SOI's W-2 statistics for the nearest year (the earlier
    one on a tie): the joint share where married is 1, else the share for other returns."""
    have = sorted(int(y) for y in DEFERRALS['roth_share'])
    wy = str(min(have, key=lambda c: (abs(c - year), c)))
    r = DEFERRALS['roth_share'][wy]
    return np.where(np.asarray(married) == 1, r['joint'], r['nonjoint'])


def traditional_deferrals(elective, year, married):
    """The traditional part of elective deferrals, which are traditional plus Roth. A missing value stays missing."""
    return np.asarray(elective, float) * (1 - roth_share(year, married))


def reported_amount(d, row, year):
    """The row's items summed as the survey reports them: zero with no items, and missing in a year the survey does
    not ask the item."""
    if not_asked(row, year):
        return np.full(len(d), np.nan)
    x = np.zeros(len(d))
    for c in row['items']:
        x = x + d[f'raw_{c}'].to_numpy(float)
    assert not np.isnan(x).any(), f'{row["survey"]} {year} {row["variable"]}: missing values'
    return x


def deferrals_subtracted(d, row, year):
    """The traditional deferrals subtracted from each unit (zero where the survey does not ask), and which units were
    not asked. Returns (None, None) for a row without the subtraction."""
    c = row.get('less_traditional_deferrals')
    if not c:
        return None, None
    e = d[f'raw_{c}'].to_numpy(float)
    asked = ~np.isnan(e)
    assert (e[asked] >= 0).all(), f'{row["survey"]} {year} {c}: deferrals below zero'
    t = np.where(asked, traditional_deferrals(np.where(asked, e, 0.0), year, d['married'].to_numpy()), 0.0)
    return t, ~asked


def survey_amount(d, row, year):
    """The row's amount on the survey's records d of one year: its items summed, less traditional deferrals (not below
    zero), then the definition fixes."""
    if not_asked(row, year):
        return np.full(len(d), np.nan)
    x = reported_amount(d, row, year)
    t, _ = deferrals_subtracted(d, row, year)
    if t is not None:
        assert (t <= x + 1e-6).all(), f'{row["survey"]} {year} {row["variable"]}: deferrals above the amount'
        x = np.maximum(x - t, 0.0)
    return definition_fixes(x, row, year)


def definition_fixes(x, row, year):
    """The row's definition fixes: the positive part, or the size of the loss (by part), then losses held to
    loss_cap."""
    if not_asked(row, year):
        return np.full(len(x), np.nan)
    if row.get('part') == 'positive':
        x = np.maximum(x, 0.0)
    elif row.get('part') == 'loss':
        x = np.maximum(-x, 0.0)
    if row['loss_cap'] is not None:
        x = np.where(x < row['loss_cap'], row['loss_cap'], x)
    return x


def _by_count(x, w, tx, tw):
    """For each survey amount x (all of one sign, as magnitudes), the tax data's magnitude with the same weighted
    count of units above it. Each unit's count is taken at the middle of its own weight, and the tax data's amounts are
    interpolated between units. Also returns which survey units have more units above them than the tax data has
    holders."""
    o = np.argsort(-tx, kind='stable')
    t_val, t_above = tx[o], np.cumsum(tw[o]) - tw[o] / 2
    so = np.argsort(-x, kind='stable')
    above = np.empty(len(x))
    above[so] = np.cumsum(w[so]) - w[so] / 2
    return np.interp(above, t_above, t_val), above > t_above[-1]


def _mid_percentile(x, w):
    """Each x's weighted percentile (0-1) among the x, taken at the middle of its weight. Equal values share one
    percentile, the middle of their joint weight."""
    vals, inv = np.unique(x, return_inverse=True)
    wv = np.bincount(inv, weights=w, minlength=len(vals))
    mid = (np.cumsum(wv) - wv / 2) / wv.sum()
    return mid[inv]


def _at_percentile(p, tx, tw):
    """The tax data's magnitude (tx, all of one sign, with weights tw) at the weighted percentiles p (0-1) among its
    own units. It is linear between the middles of the tax data's units, and flat beyond the first and last."""
    o = np.argsort(tx, kind='stable')
    t_val, t_pct = tx[o], (np.cumsum(tw[o]) - tw[o] / 2) / tw.sum()
    return np.interp(p, t_pct, t_val)


def amounts(x, w, tax_x, tax_w, line, match='count', loss_line=None):
    """Function 1: the survey amounts x (weights w), in survey-year dollars, put on the tax data's scale beyond the
    dollar line. A line of None leaves them unchanged. Losses are handled the same way, as magnitudes, beyond
    loss_line if given, else beyond line. match is one of:
    - 'count': a count from the top over every survey unit of the sign (the default, used with a line of zero only);
    - 'percentile': the percentile among the units beyond the line, in each data set;
    - 'percentile_from_top': a percentile counted as a share of all units, from the top.
    Also returns the weighted count of survey units placed past the tax data's last holder of their sign. They get the
    tax data's smallest amount."""
    out = x.astype(float).copy()
    beyond = 0.0
    if line is None:
        return out, beyond
    lines = {1: line, -1: line if loss_line is None else loss_line}
    for sign in (1, -1):
        line = lines[sign]
        sel = sign * x > line
        if not sel.any():
            continue
        if match in ('percentile', 'percentile_from_top'):
            # Matching percentiles among the units beyond the line in each data set keeps the survey's share of units
            # beyond the line, the order of the units, and continuity at the line.
            tsel = sign * tax_x > line
            assert tsel.any(), 'survey amounts beyond a line the tax data has none beyond'
            p = _mid_percentile(sign * x[sel], w[sel])
            if match == 'percentile_from_top':
                # The units that the survey lacks beyond the line are taken to be the smallest ones. Each survey unit
                # gets the tax data's amount with the same share of all units above it. The tax data's smallest
                # amounts beyond the line are left to function 2.
                s_share, t_share = w[sel].sum() / w.sum(), tax_w[tsel].sum() / tax_w.sum()
                p = 1 - (1 - p) * s_share / t_share
                beyond += float(w[sel][p < 0].sum())
                p = np.maximum(p, 0.0)
            out[sel] = sign * _at_percentile(p, sign * tax_x[tsel], tax_w[tsel])
            o = np.argsort(sign * x[sel], kind='stable')
            assert (np.diff(sign * out[sel][o]) >= -1e-9 * np.abs(out[sel][o][1:])).all(), 'the order of units not kept'
            assert (sign * out[sel] >= line).all(), 'a unit beyond the line placed within it'
            continue
        tsel = sign * tax_x > 0
        assert tsel.any(), 'survey amounts of a sign the tax data does not have'
        # Count from the top over all the survey's units of this sign, including those within the line.
        mine = sign * x > 0
        val, past = _by_count(sign * x[mine], w[mine], sign * tax_x[tsel], tax_w[tsel])
        new = np.zeros(len(x))
        new[mine] = sign * val
        out[sel] = new[sel]
        beyond += float(w[mine][past & sel[mine]].sum())
    return out, beyond


# ---- The report ----
LINES = S['report_lines']                 # the dollar amounts that bound the report's size ranges
PERCENTILES = [10, 25, 50, 75, 90, 99]


def _dollars(x):
    """A dollar amount as text: '$1,000' or '-$1,000'."""
    return f'-${-x:,.0f}' if x < 0 else f'${x:,.0f}'


def _ranges():
    """The report's size ranges, (lo, hi, label), from LINES: the losses, then the positive amounts."""
    neg = [l for l in LINES if l < 0]
    pos = [l for l in LINES if l > 0]
    out = [(-np.inf, neg[0], f'below {_dollars(neg[0])}')] if neg else [(-np.inf, 0, 'a loss')]
    out += [(a, b, f'{_dollars(a)} to {_dollars(b)}') for a, b in zip(neg, neg[1:] + [0])]
    out += [(a, b, f'{_dollars(a)} to {_dollars(b)}') for a, b in zip([0] + pos[:-1], pos)]
    out.append((pos[-1], np.inf, f'{_dollars(pos[-1])} and over'))
    return out


def describe(x, w):
    """The report's statistics of an amount x with weights w (in units): the units, the shares with none and with a
    loss, the units in each size range and above each line, the mean, the percentiles among holders, and the top 1%'s
    and top 10%'s shares of positive dollars."""
    W = w.sum()
    d = {'units (millions)': W / 1e6, 'none (%)': 100 * w[x == 0].sum() / W, 'a loss (%)': 100 * w[x < 0].sum() / W}
    for lo, hi, lab in _ranges():
        m = (x > lo) & (x < hi) if lo == 0 else (x >= lo) & (x < hi)
        d[f'{lab} (millions)'] = w[m & (x != 0)].sum() / 1e6
    for l in [l for l in LINES if l > 0]:
        d[f'above {_dollars(l)} (millions)'] = w[x >= l].sum() / 1e6
    d['mean per unit ($)'] = float((w * x).sum() / W)
    d['with a positive amount (%)'] = 100 * w[x > 0].sum() / W
    # percentiles among the units with a positive amount (the holders)
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


def shortfall(row, year, x, w, tx, tw, fp, fn):
    """What function 2 has to fill for a row. For each of the row's size ranges (in PUF-year dollars, grown by fp for
    the positive edges and fn for the negative ones): the survey's share of units after function 1, the tax data's
    share, and the shortfall between them. Function 2 fills a shortfall by giving amounts to some of the survey's
    zeros; it cannot remove a surplus."""
    grow = lambda e: e * (fp if e > 0 else fn)
    out = [('none', x == 0, tx == 0)]
    for lo, hi in row['fill_in']['size_ranges']:
        a = -np.inf if lo is None else grow(lo)
        b = np.inf if hi is None else grow(hi)
        lab = f'{_dollars(a) if np.isfinite(a) else "below"} to {_dollars(b) if np.isfinite(b) else "and over"}'
        sel = (lambda z: (z > 0) & (z < b)) if lo == 0 else (lambda z: (z >= a) & (z < b) & (z != 0))
        out.append((lab, sel(x), sel(tx)))
    rows = []
    for lab, m, tm in out:
        s, t = 100 * w[m].sum() / w.sum(), 100 * tw[tm].sum() / tw.sum()
        rows.append({'survey': row['survey'], 'variable': row['variable'], 'year': year, 'range': lab,
                     'survey after function 1 (%)': s, 'tax data (%)': t, 'shortfall (points)': t - s,
                     'survey units (millions)': w[m].sum() / 1e6, 'tax data units (millions)': tw[tm].sum() / 1e6})
    return rows


def move(link, mapped, w):
    """How far function 1 moved the amounts, among the units with a positive amount before it ("link", the amount
    after the definition fixes): the change in the total, the weighted median of each unit's ratio of after to before,
    and the shares of those units moved down or up by more than 10%."""
    h = link > 0
    if not h.any():
        return {}
    r, wh = mapped[h] / link[h], w[h]
    return {'move: total, mapped over link (%)': 100 * ((wh * mapped[h]).sum() / (wh * link[h]).sum() - 1),
            'move: median unit, mapped over link (%)': 100 * (float(common.wquantile(r, wh, [0.5])[0]) - 1),
            'move: units down by more than 10% (%)': 100 * wh[r < 0.9].sum() / wh.sum(),
            'move: units up by more than 10% (%)': 100 * wh[r > 1.1].sum() / wh.sum()}


def describe_share(x, w):
    """Statistics of a share from 0 to 1 that is missing on some units: the share of units with it recorded, and among
    those the shares at 0 and at 1, the mean, and the 10th, 50th and 90th percentiles."""
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
    """The share of units with each value. Where records carry probabilities of each value instead, probs gives them
    as an (n, values) array."""
    if probs is None:
        vals = sorted(np.unique(x[x >= 0]).astype(int))
        probs = np.column_stack([(x == v).astype(float) for v in vals])
    else:
        vals = range(probs.shape[1])
    return {f'value {v} (%)': 100 * (w * probs[:, i]).sum() / w.sum() for i, v in enumerate(vals)}


def describe_age(x, w):
    """Statistics of an exact-age slot: the share with no age, and among the units with an age, the share in each age
    group and the mean age (80 and over counted as 80)."""
    x, w = np.asarray(x, float), np.asarray(w, float)
    has = x > 0
    age = x - 1
    d = {'none (%)': 100 * w[~has].sum() / w.sum()}
    for lo, hi, lab in AGE_GROUPS:
        d[f'{lab} (% of units with an age)'] = 100 * w[has & (age >= lo) & (age < hi)].sum() / w[has].sum()
    d['mean age'] = float((w[has] * age[has]).sum() / w[has].sum())
    return d


def report_rows(row, year, stages):
    """The report's rows for one survey row and year, one per stage and statistic. stages = {stage: {statistic:
    value}}."""
    return [{'survey': row['survey'], 'variable': row['variable'], 'year': year, 'stage': st, 'statistic': k, 'value': v}
            for st, d in stages.items() for k, v in d.items()]


def say_report(rep):
    """Prints the report: one block per survey, variable and year, with the stages side by side."""
    for (s, v, y), g in rep.groupby(['survey', 'variable', 'year'], sort=False):
        wide = g.pivot_table(index='statistic', columns='stage', values='value', sort=False)
        common.say(f'--- {s} {y} {v}\n' + wide.round(2).to_string())
