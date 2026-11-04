"""scf_targets.py: the SCF targets' values and standard errors, from the year's mix of SCF waves (TARGETS.md S9).

Every SCF target (kinds 'wealth', 'wealth grid' and 'auto loans' in targets_config/) is computed on the same waves, with the same
shares, as the generated file draws its balance sheet from: the year file's prior, "balance sheet" (2019: the 2019
wave 54.65%, the 2022 wave 33.15%, the 2016 wave 12.20%).

Each wave, on its own:
  - its units are its rows of the training table (source scf, the wave's income year, all five implicates);
  - its balance sheet is grown per tax unit to the target year's wave, item by item, by the factors the dollar
    maps use (build_table.scf_growth, written to <maps>/scf_growth_factors.csv). The home mortgage moves with the
    home, because the file draws it as a share of the home's value. Net worth is the grown assets less the grown
    debts (the file's net_worth_parts). So the targets and the file have one dollar concept;
  - its weights are scaled so that its units add up to the target year's wave's: growth per tax unit carries
    dollars per unit, and the file's number of units is set by SOI's returns and the non-filer count, not by the
    wave. Shares do not depend on this scaling;
  - its lines are its own: the income rank groups by its own weighted percentiles of the seven income items (in
    its own income year, so its ranks are its own), and the net worth lines at its own weighted percentiles of
    its grown net worth;
  - its value and its standard error in each cell are errors.py's, for one wave: the 999 replicate weights on the
    first implicate for the sampling variance, the five implicates for the imputation variance (an implicate's
    totals on 5 x its weights), total = sampling + 1.2 x imputation, every line held at its full-sample value.
The pool: value = sum over waves of share x value_w; the waves are independent samples, so variance = sum over
waves of share^2 x variance_w. The file needs one dollar line for each net worth percentile: it gets the pooled
line, sum over waves of share x line_w. (The income rank groups need none: the file ranks its own units.)

Interface: rows(cells, v, stats, year), called by target_list.survey_rows for the SCF, returns its rows, each with
its standard error in 'pooled'; target_list.expand_entry records them in RESULTS by (kind, group, target), and
errors.scf_errors reads them from there. details() is one row per target and wave, for the record (tilt.py writes
it as scf_targets_by_wave.csv). reference(year) is the pooled SCF itself (its units, net worth and weights), for
the tilt's summary.
"""
import numpy as np
import pandas as pd

import common
import dollars as D

RESULTS = {}     # (kind, group, target) -> {'se', 'reason', 'value', 'line', 'line_hi', 'per_wave': [...]}
_FRAMES = {}
_TABLE = {}


def mix(year):
    """[(wave, share)] for the year's SCF targets: the waves and shares of the year's prior "balance sheet", the
    file's own (survey_mix.prior_shares; the shares divided by their sum, as generate.scf_waves does)."""
    import survey_mix
    return survey_mix.prior_shares(year, 'balance sheet', 'scf')


def _table(cols):
    """The training table's SCF rows, with the columns cols (read once per set of columns)."""
    key = tuple(cols)
    if key not in _TABLE:
        t = pd.read_parquet(common.TABLE, columns=list(dict.fromkeys(['source', 'year', 'record_id'] + list(cols))))
        _TABLE[key] = t[t.source == 'scf'].reset_index(drop=True)
    return _TABLE[key]


def growth_factors(wave, year):
    """{item: factor} from the wave to the target year's wave, as the dollar maps grow it (build_table.build_maps)."""
    assert year == common.TARGET_YEAR, f'SCF targets for {year}: the maps grow the SCF to {common.TARGET_YEAR} only'
    g = pd.read_csv(f'{common.MAPS}/scf_growth_factors.csv')
    g = g[g['wave'] == wave]
    f = dict(zip(g['variable'], g['factor to the target year'].astype(float)))
    assert f, f'SCF wave {wave}: no growth factors in {common.MAPS}/scf_growth_factors.csv'
    bad = [v for v, x in f.items() if not np.isfinite(x)]
    assert not bad, f'SCF wave {wave}: no growth factor for {bad} (no dollar map either), so it cannot be in the mix'
    # (1 November, decision 76) a projection year's file is its start grown from the 2023 file by the CBO and macro series
    # per return (tilt.grow_dollars, projection_growth.plan_factors), but the maps hold the factors to 2023 only, so the
    # SCF targets stayed at 2023's dollars. They are grown on from 2023 to the year by the factors the start's records get.
    ext = projection_extension(year, list(f))
    for v, x in ext.items():
        f[v] = f[v] * x
    return f


_EXT = {}
_UG = {}


def unit_growth(year):
    """The growth of the file's number of units from the start's year to a projection year: the start's own units over
    their weighted count in the start year, as population_cells scales them (the start's weights; tilt.py)."""
    import target_list as TLS
    import projection_demographics as PD
    spec = TLS.start_spec(year)
    if spec['weights'] == 'persons':   # (3 November, agent A) the units after step 1 (projection_steps)
        import projection_steps as PS
        return PS.unit_growth()
    assert spec['mode'] == 'from_year' and spec['weights'] == 'population_cells', f'{year}: unit growth needs population_cells'
    if year not in _UG:
        prev = pd.read_parquet(f"{common.OUT}/tilt/{spec['run']}/file_{spec['year']}.parquet",
                               columns=['record_id', 'weight', 'filer', 'married', 'filing_status', 'age_range', 'age1'])
        scaled, _ = PD.population_cells(prev, year, from_year=spec['year'])
        _UG[year] = float(np.asarray(scaled).sum() / prev['weight'].sum())
        common.say(f'the file\'s units grow {_UG[year]:.5f} from {spec["year"]} to {year} (the SCF targets\' units follow)')
    return _UG[year]


def projection_extension(year, items):
    """{item: factor} from the start file's year (2023) to a projection year, for the SCF items, exactly the factors
    tilt.grow_dollars gives the same columns of the start: CBO's and the macro series per return (the Financial Accounts'
    category per return for the balance sheet), and for an item with no such rule the SCF's own growth per tax unit
    (build_table.scf_growth, as grow_dollars's fallback). Empty for a year whose start is generated.
    The mortgage's share of the home (w_primary_ltv) takes the factor the start's share gets (the mortgage's Financial
    Accounts series over the home's), so the target frame's mortgage (home times share) moves as the start's does."""
    import target_list as TLS
    spec = TLS.start_spec(year)
    if spec['mode'] != 'from_year':
        return {}
    if spec['dollars'] == 'item_factor':
        # (3 November, agent A) the start's step 2 factors (projection_steps.scf_factor: a 'total' item's growth in total
        # over the units' growth, as the SCF units grow with the file's; a 'per_unit' item's own factor); the mortgage's
        # share as the start's
        import projection_steps as PS
        out = {v: PS.scf_factor(v) for v in items if v in PS.STATE['items']}
        common.say(f'SCF targets grown on from {spec["year"]} to {year} by the start\'s step 2 factors: {len(out)} items')
        return out
    assert spec['dollars'] == 'cbo_per_return', f'{year}: the SCF targets are grown for the cbo_per_return start only, not {spec["dollars"]!r}'
    assert year <= 2036, f'{year}: CBO per-return growth ends in 2036; the SCF targets past it need the demographic return growth'
    s = spec['year']
    if (s, year) not in _EXT:
        import build_table as BT
        import projection_growth as PG
        names = [v for v in BT.SCF_AMOUNTS if v not in BT.SCF_RATIOS]
        got, problems = PG.plan_factors(names, year, None, strict=False, from_year=s)
        out = dict(zip(got['column'], got['factor'].astype(float)))
        late = [v for v in names if v not in out]
        t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight'] + BT.SCF_AMOUNTS)
        fb = BT.scf_growth(t[t.source == 'scf'], s - 1, year - 1)
        for v in late:
            out[v] = float(fb[v])
        bad = [v for v, x in out.items() if not (np.isfinite(x) and x > 0)]
        assert not bad, f'SCF targets {s} to {year}: no growth factor for {bad}'
        # the file's mortgage is its home times the share, grown by the SCF extension's ratio (grow_dollars, SCF_RATIOS)
        out['w_primary_ltv'] = float(fb['w_primary_ltv'])
        _EXT[(s, year)] = out
        common.say(f'SCF targets grown on from {s} to {year} as the start is: {len(names) - len(late)} items by the projection '
                   f'series per return, {len(late)} by the SCF growth per tax unit ({", ".join(late)})')
    return {v: _EXT[(s, year)][v] for v in items if v in _EXT[(s, year)]}


def frame(wave, year):
    """The wave's units in the target year's dollars: (units, replicate weights of the first implicate's units,
    mask of the first implicate's rows, the weights' scale)."""
    key = (wave, year)
    if key in _FRAMES:
        return _FRAMES[key]
    import errors as E
    f = growth_factors(wave, year)
    # The mortgage is the home times the ltv; the ltv reflects their different growth after 2022. The elective deferrals and the spouse's share of
    # them (since 28 October; build_table.SCF_NOT_ASKED) are missing where the SCF does not ask and no SCF target reads
    # them (their targets are the W-2 statistics'), so they are left out here; so is the retirement accounts' stock share
    # (since 29 October), a share missing outside the PEU that no SCF target reads
    # (3 November) and the inheritances received this year, missing outside the PEU, which no SCF target reads either
    import build_table as BT
    not_asked = list(BT.SCF_NOT_ASKED)
    items = [v for v in f if v != 'w_primary_ltv' and v not in not_asked]
    cols = list(dict.fromkeys(items + ['value.primary_mortgage'] + list(D.NET_WORTH_PARTS) + ['wages']))
    t = _table(['household_id', 'implicate', 'weight', 'age_range', 'age1'] + cols)
    u = t[t.year == wave - 1].reset_index(drop=True)
    assert len(u), f'SCF wave {wave}: no rows in the training table'
    na = u[cols + ['weight', 'household_id', 'implicate', 'age1']].isna().sum()
    assert na.sum() == 0, f'SCF wave {wave}: missing values\n{na[na > 0]}'
    assert (u['weight'] > 0).all(), f'SCF wave {wave}: a unit with no positive weight'
    assert set(u['implicate']) == {1, 2, 3, 4, 5}, f'SCF wave {wave}: implicates {sorted(set(u.implicate))}'
    assert (u['age_range'] < common.ADULT_RANGES).all(), f'SCF wave {wave}: a dependent filer among the units'
    # (2 November 2026) the primary filer's age in years, as the file holds it (catalog primary_age): the table stores
    # age plus 1, 0 for none (build_table.AGE_SLOTS); every SCF unit has a primary filer
    assert (u['age1'] > 0).all(), f'SCF wave {wave}: {int((u["age1"] <= 0).sum())} units with no primary filer age'
    u['age1'] = u['age1'].to_numpy(float) - 1
    for v in items:
        u[v] = u[v].to_numpy(float) * f[v]
    u['value.primary_mortgage'] = u['value.primary_mortgage'].to_numpy(float) * f['value.primary_home'] * f['w_primary_ltv']
    u['net_worth'] = D.signed_sum(u, D.NET_WORTH_PARTS)
    # the target year's wave's number of units; a year with no wave of its own (30 October): interpolated between the
    # waves either side, or after the last wave grown with the population (build_table.scf_units_at)
    import build_table as BT
    import target_list as TLS
    # A projection year (1 November, decision 76): the units of the start's own year (2023's wave) times the growth of the
    # file's units from that year (the start's weights are the file's weights times the population cells' growth,
    # projection_demographics.population_cells), not the Social Security area population: the file's units grow by cells
    # (age and joint status; 2023 to 2030 +6.5% against the population's +3.3%), and the start's dollars per unit are grown
    # on that basis, so targets that scaled units by the population stood 4% too low in total by 2030.
    spec = TLS.start_spec(year)
    if spec['mode'] == 'from_year':
        units = BT.scf_units_at(_table(['weight']), spec['year'] - 1) * unit_growth(year)
    else:
        units = BT.scf_units_at(_table(['weight']), year - 1)
    scale = units / float(u['weight'].sum())
    u['weight'] = u['weight'].to_numpy(float) * scale
    u = u.sort_values(['implicate', 'record_id'], kind='stable').reset_index(drop=True)
    u1, rep = E.scf_replicates(wave, u)   # settings errors.scf_replicate_dir, one folder per wave
    first = (u['implicate'] == 1).to_numpy()
    assert np.array_equal(u1['record_id'].to_numpy(), u.loc[first, 'record_id'].to_numpy()), 'implicate 1 out of order'
    rep = rep * scale
    common.say(f'SCF wave {wave} for the {year} targets: {len(u):,} unit rows, {u.household_id.nunique():,} '
               f'households; weights scaled by {scale:.5f} to {u.weight.sum() / 1e6:.3f} million units; net worth, '
               f'grown, ${(u.weight * u.net_worth).sum() / 1e12:.2f} trillion')
    _FRAMES[key] = (u, rep, first, scale)
    return _FRAMES[key]


def rank_income(wave, year, parts):
    """The wave's units' value of a ranking variable: the sum of its items (catalog survey_parts) on the units'
    table rows, in the wave's own income year (not grown: each wave ranks its own units). A missing item is an
    error, never dropped."""
    key = (wave, year, tuple(parts))
    if key not in _FRAMES:
        u = frame(wave, year)[0]
        t = _table(list(parts))
        t = t[t.year == wave - 1]
        j = u[['record_id']].merge(t[['record_id'] + list(parts)], on='record_id', how='left', validate='one_to_one')
        nan = int(j[list(parts)].isna().any(axis=1).sum())
        assert nan == 0, f'SCF wave {wave}: {nan} unit rows with an income item missing ({parts})'
        _FRAMES[key] = j[list(parts)].to_numpy(float).sum(1)
    return _FRAMES[key]


def _cell_spec(c):
    """(rank (variable, lo, hi) or None, the line's variable, lower line spec, upper line spec) of a target_list
    cell; a spec is ('percentile', p) or ('dollars', d) or None (no line)."""
    if c.get('band') is not None:
        var, lo, hi = c['band']
        return c.get('rank'), var, (tuple(lo) if lo else None), (tuple(hi) if hi else None)
    if c['line'] is not None:
        var, kind, num, _ = c['line']
        return c.get('rank'), var, (kind, num), None
    return c.get('rank'), None, None, None


def _dollar_line(x, w, spec):
    if spec is None:
        return None
    kind, num = spec[0], float(spec[1])
    return float(common.wquantile(x, w, [num / 100])[0]) if kind == 'percentile' else num


def wave_cell(wave, year, c, v, st, ceiling=None):
    """One wave's value of statistic st (share, amount or average) of variable v in cell c, with its variance, its lines and
    the SCF households behind it. ceiling (29 October): None, or a net worth in nominal target-year dollars; then the
    statistic is on the wave's units below it (grown net worth), a domain kept inside the full sample, so the replicate
    weights and implicates are used as they are: the income rank lines and the net worth percentile lines are that
    population's own percentiles, a share's denominator is its units, and the weights are not scaled back up."""
    import target_list as TLS
    rank, line_var, lo_spec, hi_spec = _cell_spec(c)
    u, rep, first, _ = frame(wave, year)
    w = u['weight'].to_numpy(float)
    imp = u['implicate'].to_numpy()
    hh = u['household_id'].to_numpy()
    assert not c['filters'], f'SCF targets: a cell with a filter {c["filters"]}'
    below = np.ones(len(u), bool) if ceiling is None else u['net_worth'].to_numpy(float) < ceiling
    m = below.copy()
    if c.get('range'):
        # (2 November 2026) a range of a variable other than AGI, low inclusive (the primary's age group): the cell is
        # the units in the range, a domain of the full sample, so the replicate weights and implicates are used as
        # they are. Applied before any income rank or net worth line, so lines would be the range's own (no entry
        # crosses a range with a line today)
        rv, rlo, rhi = c['range']
        xg = TLS.record_value(u, rv)
        assert np.isfinite(xg).all(), f'SCF wave {wave}: {rv} missing on {int((~np.isfinite(xg)).sum())} units'
        m &= (xg >= rlo) & (xg < rhi)
        assert c.get('rank') is None and c.get('band') is None and c['line'] is None, \
            f'SCF cell {c["label"]}: a range crossed with an income rank or a net worth line is not built'
    rl = (np.nan, np.nan)
    if rank is not None:
        inc = rank_income(wave, year, TLS.variable_def(rank[0])['survey_parts'])
        rl = TLS.rank_dollar_lines(inc[m], w[m], rank[1], rank[2])
        m &= (inc >= rl[0]) & (inc < rl[1])
    lo = hi = None
    cell = m
    if line_var:
        xl = TLS.record_value(u, line_var)
        lo, hi = _dollar_line(xl[below], w[below], lo_spec), _dollar_line(xl[below], w[below], hi_spec)
        cell = m & ((xl > lo) if lo is not None else True) & ((xl <= hi) if hi is not None else True)
    y = TLS.record_value(u, v) if v else np.zeros(len(u))
    if st == 'share':
        assert lo is not None or hi is not None, f'SCF cell {c["label"]}: a share needs a line'
        stat = lambda cc, mm, yy, ww: (ww * cc).sum(-1) / (ww * mm).sum(-1)
        sc = 1.0
    elif st == 'count':
        assert v, f'SCF cell {c["label"]}: a count needs a variable'
        stat = lambda cc, mm, yy, ww: (ww * cc * (yy != 0)).sum(-1)
        sc = 5.0
    elif st == 'amount':
        stat = lambda cc, mm, yy, ww: (ww * cc * yy).sum(-1)
        sc = 5.0
    elif st == 'average':
        # the variable's average over the cell's units (auto loan interest, 27 October): a share of units with any is
        # the average of a 0/1 indicator
        stat = lambda cc, mm, yy, ww: (ww * cc * yy).sum(-1) / (ww * cc).sum(-1)
        sc = 1.0
    else:
        raise NotImplementedError(f'SCF targets: statistic {st}')
    full = float(stat(cell, m, y, w))
    by_imp = np.array([float(stat(cell[imp == k], m[imp == k], y[imp == k], sc * w[imp == k])) for k in range(1, 6)])
    reps = stat(cell[first], m[first], y[first], rep.T)
    v_samp = float(((reps - reps.mean()) ** 2).sum() / (len(reps) - 1))
    v_imp = float(((by_imp - by_imp.mean()) ** 2).sum() / 4)
    n_hh = [len(np.unique(hh[cell & (imp == k)])) for k in range(1, 6)]
    return {'wave': wave, 'value': full, 'var': v_samp + 1.2 * v_imp, 'se': float(np.sqrt(v_samp + 1.2 * v_imp)),
            'se_sampling': float(np.sqrt(v_samp)), 'se_imputation': float(np.sqrt(v_imp)),
            'line': lo, 'line_hi': hi, 'rank_line_lo': rl[0], 'rank_line_hi': rl[1],
            'households_any_implicate': len(np.unique(hh[cell])), 'households_min_implicate': min(n_hh),
            'households_max_implicate': max(n_hh), 'households_in_income_group': len(np.unique(hh[m])),
            'replicates_with_zero_in_cell': int((reps == 0).sum())}


def rows(cells, v, stats, year, ceiling=None):
    """target_list.survey_rows for the SCF: one row per cell x statistic, with the pooled value, the pooled dollar
    lines for the file, and its standard error in 'pooled'. ceiling: the population's net worth ceiling (wave_cell)."""
    sh = dict(mix(year))
    out = []
    for c in cells:
        for st in stats:
            per = [wave_cell(wv, year, c, v, st, ceiling) for wv in sh]
            val = sum(sh[p['wave']] * p['value'] for p in per)
            var = sum(sh[p['wave']] ** 2 * p['var'] for p in per)
            pool = lambda k: None if per[0][k] is None else float(sum(sh[p['wave']] * p[k] for p in per))
            line, line_hi = pool('line'), pool('line_hi')
            rl = tuple(float(sum(sh[p['wave']] * p[k] for p in per)) if c.get('rank') else np.nan
                       for k in ('rank_line_lo', 'rank_line_hi'))
            desc = '; '.join(f"wave {p['wave']} ({100 * sh[p['wave']]:.2f}%): {p['value']:,.6g}"
                             + ('' if p['line'] is None else f", line {p['line']:,.0f}")
                             + ('' if p['line_hi'] is None else f", upper line {p['line_hi']:,.0f}")
                             + f", {p['households_any_implicate']} households" for p in per)
            why = ('SCF waves pooled, independent samples: variance = sum of share^2 x the wave\'s variance; each wave '
                   'from its 999 replicate weights (first implicate) and its five implicates: '
                   + '; '.join(f"wave {p['wave']} se {p['se']:,.4g} (sampling {p['se_sampling']:,.4g}, imputation "
                               f"{p['se_imputation']:,.4g})" for p in per))
            below = '' if ceiling is None else f', units with net worth below ${ceiling:,.0f} only'
            out.append({'cell': c['label'], 'cell_spec': c, 'statistic': st, 'value': val, 'line': line,
                        'line_variable': _cell_spec(c)[1] or '', 'rank_lines': rl, 'line_hi': line_hi,
                        'source': f'SCF waves pooled with the year\'s shares, each wave\'s balance sheet grown per tax '
                                  f'unit to the {year} wave and its own lines{below}: {desc}',
                        'pooled': {'se': float(np.sqrt(var)), 'reason': why, 'value': val, 'line': line,
                                   'line_hi': line_hi, 'shares': sh, 'per_wave': per}})
    return out


def details():
    """One row per SCF target and wave: the pooled value, standard error and lines, and the wave's own."""
    out = []
    for (kind, group, target), d in RESULTS.items():
        for p in d['per_wave']:
            out.append({'kind': kind, 'group': group, 'target': target, 'pooled_value': d['value'],
                        'pooled_se': d['se'], 'pooled_line': d['line'], 'pooled_line_hi': d['line_hi'],
                        'share': d['shares'][p['wave']], **{k: x for k, x in p.items() if k != 'var'}})
    return pd.DataFrame(out)


def reference(year):
    """The pooled SCF of the year's targets: (units of every wave in the mix, net worth, weights times the wave's
    share), for the tilt's summary."""
    us, x, w = [], [], []
    for wave, share in mix(year):
        u = frame(wave, year)[0]
        us.append(u)
        x.append(u['net_worth'].to_numpy(float))
        w.append(share * u['weight'].to_numpy(float))
    return pd.concat(us, ignore_index=True), np.concatenate(x), np.concatenate(w)
