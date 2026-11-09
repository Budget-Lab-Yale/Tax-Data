"""The SCF targets' values and standard errors, over the year's mix of SCF waves (the waves and shares the file draws
its balance sheet from). Each wave is grown per tax unit to the target year's wave by the dollar maps' factors and its
weights are scaled to the target year's units; its income ranks and net worth lines are its own, and its value and
variance come from its replicate weights and implicates. The pooled value is the share-weighted sum over waves, the
pooled variance the sum with squared shares. See TARGETS.md, "SCF targets".
"""
import numpy as np
import pandas as pd

from shared import common
from shared import dollars as D

# (kind, group, target) -> {'se', 'reason', 'value', 'line', 'line_hi', 'per_wave': [...]}; filled by
# target_list.expand_entry, read by errors.scf_errors
RESULTS = {}
_FRAMES = {}
_TABLE = {}
_EXT = {}


def mix(year):
    """[(wave, share)] for the year's SCF targets: the waves and shares of the year's prior "balance sheet", as the file
    uses them."""
    from shared import survey_mix
    return survey_mix.prior_shares(year, 'balance sheet', 'scf')


def _table(cols):
    """The training table's SCF rows, with the columns cols (read once per set of columns)."""
    key = tuple(cols)
    if key not in _TABLE:
        t = pd.read_parquet(common.TABLE, columns=list(dict.fromkeys(['source', 'year', 'record_id'] + list(cols))))
        _TABLE[key] = t[t.source == 'scf'].reset_index(drop=True)
    return _TABLE[key]


def growth_factors(wave, year):
    """{item: factor} from the wave to the target year's wave, as the dollar maps grow it (maps.build_maps)."""
    assert year == common.TARGET_YEAR, f'SCF targets for {year}: the maps grow the SCF to {common.TARGET_YEAR} only'
    g = pd.read_csv(f'{common.MAPS}/scf_growth_factors.csv')
    g = g[g['wave'] == wave]
    f = dict(zip(g['variable'], g['factor to the target year'].astype(float)))
    assert f, f'SCF wave {wave}: no growth factors in {common.MAPS}/scf_growth_factors.csv'
    bad = [v for v, x in f.items() if not np.isfinite(x)]
    assert not bad, f'SCF wave {wave}: no growth factor for {bad} (no dollar map either), so it cannot be in the mix'
    # A projection year's file is its start grown item by item, but the maps hold the factors to the start's year only;
    # the SCF targets are grown on from there by the factors the start's records get.
    ext = projection_extension(year, list(f))
    for v, x in ext.items():
        f[v] = f[v] * x
    chain, _ = jump_off_chain(year)
    for v in f:
        if chain is not None and v in ext:
            f[v] = f[v] * chain[v]
    return f


def jump_off_chain(year):
    """For a year whose start is the jump-off year's file: the jump-off year's own SCF growth from the base year,
    ({item: factor}, units' growth), read from that year's tilt folder, so the SCF targets are the base year's frame
    grown to the jump-off year and on from it. (None, 1.0) for any other start."""
    from reweight import target_list as TLS
    spec = TLS.start_spec(year)
    if not spec.get('jump_off'):
        return None, 1.0
    if 'chain' not in _EXT:
        d = f"{common.OUT}/tilt/{spec['run']}"
        it = pd.read_csv(f'{d}/projection_step2_items.csv')
        g = pd.read_csv(f'{d}/projection_return_groups.csv')
        ug = float(g['units after step 1'].sum() / g['units 2023'].sum())
        assert np.isfinite(ug) and ug > 0, f'{d}: the units\' growth {ug}'
        chain = {r.column: (r['growth positive'] / ug if r.kind == 'total' else r['factor positive'])
                 for _, r in it.iterrows()}
        bad = [v for v, x in chain.items() if not (np.isfinite(x) and x > 0)]
        assert not bad, f'{d}: no SCF growth factor for {bad}'
        _EXT['chain'] = (chain, ug)
        common.say(f"SCF targets: the jump-off year {spec['year']}'s own growth from the base year taken over "
                   f"({len(chain)} items; units {ug:.5f})")
    return _EXT['chain']


def unit_growth(year):
    """The growth of the file's number of units from the start's year to a projection year: the units after step 1
    (projection_steps)."""
    from reweight import target_list as TLS
    spec = TLS.start_spec(year)
    assert spec['weights'] == 'persons', f'{year}: unit growth needs a start scaled by persons'
    from scale import projection_steps as PS
    return PS.unit_growth()


def projection_extension(year, items):
    """{item: factor} from the start file's year to a projection year, for the SCF items: the start's step 2 factors
    (projection_steps.scf_factor). Empty for a year whose start is generated."""
    from reweight import target_list as TLS
    spec = TLS.start_spec(year)
    if spec['mode'] != 'from_year':
        return {}
    assert spec['dollars'] == 'item_factor', f'{year}: the SCF targets are grown for the item_factor start only'
    from scale import projection_steps as PS
    out = {v: PS.scf_factor(v) for v in items if v in PS.STATE['items']}
    common.say(f'SCF targets grown on from {spec["year"]} to {year} by the start\'s step 2 factors: {len(out)} items')
    return out


def frame(wave, year):
    """The wave's units in the target year's dollars: (units, replicate weights of the first implicate's units,
    mask of the first implicate's rows, the weights' scale)."""
    key = (wave, year)
    if key in _FRAMES:
        return _FRAMES[key]
    from reweight import errors as E
    from prep import build_table as BT
    from scale import maps as MP
    from reweight import target_list as TLS
    f = growth_factors(wave, year)
    # The mortgage is the home times the loan-to-value share, whose factor carries their different growth after the last
    # wave. The items the SCF does not ask of every unit are missing outside the primary economic unit and no SCF target
    # reads them, so they are left out.
    items = [v for v in f if v != 'w_primary_ltv' and v not in BT.SCF_NOT_ASKED]
    cols = list(dict.fromkeys(items + ['value.primary_mortgage'] + list(D.NET_WORTH_PARTS) + ['wages']))
    t = _table(['household_id', 'implicate', 'weight', 'age_range', 'age1'] + cols)
    u = t[t.year == wave - 1].reset_index(drop=True)
    assert len(u), f'SCF wave {wave}: no rows in the training table'
    na = u[cols + ['weight', 'household_id', 'implicate', 'age1']].isna().sum()
    assert na.sum() == 0, f'SCF wave {wave}: missing values\n{na[na > 0]}'
    assert (u['weight'] > 0).all(), f'SCF wave {wave}: a unit with no positive weight'
    assert set(u['implicate']) == {1, 2, 3, 4, 5}, f'SCF wave {wave}: implicates {sorted(set(u.implicate))}'
    assert (u['age_range'] < common.ADULT_RANGES).all(), f'SCF wave {wave}: a dependent filer among the units'
    # the table stores the primary filer's age plus 1 (0 for none); every SCF unit has a primary filer
    assert (u['age1'] > 0).all(), f'SCF wave {wave}: {int((u["age1"] <= 0).sum())} units with no primary filer age'
    u['age1'] = u['age1'].to_numpy(float) - 1
    for v in items:
        u[v] = u[v].to_numpy(float) * f[v]
    u['value.primary_mortgage'] = u['value.primary_mortgage'].to_numpy(float) * f['value.primary_home'] * f['w_primary_ltv']
    u['net_worth'] = D.signed_sum(u, D.NET_WORTH_PARTS)
    # The target year's wave's number of units. A projection year takes the units of the start's own year times the
    # growth of the file's units in step 1, not of the population, because the file's units grow by age and joint
    # status cells and the start's dollars per unit are grown on that basis.
    spec = TLS.start_spec(year)
    if spec.get('jump_off'):   # the base year's units, grown to the jump-off year and on from it
        from scale import projection_steps as PS
        units = MP.scf_units_at(_table(['weight']), int(PS.FS['base_year']) - 1) * jump_off_chain(year)[1] * unit_growth(year)
    elif spec['mode'] == 'from_year':
        units = MP.scf_units_at(_table(['weight']), spec['year'] - 1) * unit_growth(year)
    else:
        units = MP.scf_units_at(_table(['weight']), year - 1)
    scale = units / float(u['weight'].sum())
    u['weight'] = u['weight'].to_numpy(float) * scale
    u = u.sort_values(['implicate', 'record_id'], kind='stable').reset_index(drop=True)
    u1, rep = E.scf_replicates(wave, u)   # <out>/data/scf_rw/<wave>
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
    """One wave's value of statistic st (share, count, amount or average) of variable v in cell c, with its variance,
    lines and the SCF households behind it. ceiling: None, or a net worth in target-year dollars; the statistic is then
    on the units below it, a domain of the full sample, so the replicate weights and implicates are used as they are and
    the lines are that population's own percentiles."""
    from reweight import target_list as TLS
    rank, line_var, lo_spec, hi_spec = _cell_spec(c)
    u, rep, first, _ = frame(wave, year)
    w = u['weight'].to_numpy(float)
    imp = u['implicate'].to_numpy()
    hh = u['household_id'].to_numpy()
    assert not c['filters'], f'SCF targets: a cell with a filter {c["filters"]}'
    below = np.ones(len(u), bool) if ceiling is None else u['net_worth'].to_numpy(float) < ceiling
    m = below.copy()
    if c.get('range'):
        # a range of a variable other than AGI, low end inclusive (the primary's age group): a domain of the full
        # sample, like the ceiling
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
        # the variable's average over the cell's units (auto loan interest): a share of units with any is
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
