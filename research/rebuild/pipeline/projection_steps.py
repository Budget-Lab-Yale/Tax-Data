"""The projection years (2024-2035) as John designed them on 3 November 2026 (scratch
explore/2026-11-02/projection_design/DESIGN.md, "Agreed on 3 November"). Every projection year starts from the 2023 file
and the years are independent of one another. Three steps, switched on by a year's targets.yaml with
start_scaling {weights: persons, dollars: item_factor}:

1. A calibration to exact person counts (step1). The 2023 weights are moved, as little as possible (the smallest
   Kullback-Leibler move, solver.solve with every count an identity), so that the people in the file match exact counts
   by marital status and single year of age (0 to 79, then 80 and over). Every member of a unit carries the unit's
   weight: the primary filer and, on a joint return, the spouse (married), the primary of any other unit (unmarried),
   and every dependent with an age (unmarried). A dependent filer's own record counts only its dependents: the filer
   himself is counted on the return that claims him. A married person under 16 is counted unmarried (CBO's population
   has no married person under 16). Each count is the 2023 file's own count times CBO's growth of the same people
   (the Social Security area population of the Macro-Projections interface, settings tilt.projection.population_dir).
   The dependent filers' own six cells (age range x married) are exact too, each the 2023 count times its population
   growth (projection_demographics.population_cells, the rule used before), because the person counts do not see the
   dependent filers themselves.
2. One dollar factor per item (step2_factor, called by tilt.grow_dollars). Each item has a row in the items table
   (settings tilt.projection.items_table; ITEMS_COLUMNS). Its series gives either the growth of the item's total from
   2023 (kind 'total') or the growth per record (kind 'per_unit'). For kind 'total' the factor is that growth times
   the item's 2023 total over the total the step 1 weights give the 2023 amounts, so the item's total on the start
   grows exactly at the series and population change is not counted twice; gains and losses (positive and negative
   values) each get their own factor. For kind 'per_unit' the factor is the series' growth itself, and the total moves
   also with the number of holders the step 1 weights give.
3. The main reweighting (tilt.main, solver.solve) with, among the year's targets.yaml entries, the outside sources
   defined here (outside_rows): 'projection persons' (the step 1 counts, exact), 'projection returns' (filers in each
   group of the primary filer's age and marital status: the 2023 file's share of the group's units that file times
   the group's units after step 1, exact) and 'projection items' (each targeted item's total on the start after step
   2, gains and losses separately, a judgment error of the item's relative error). The itemizer and AMT counts and the
   wealth targets are ordinary entries of targets.yaml.

The Forbes list. The 2023 file's list records are left out of every step (the top step replaces them by the year's
list, whose weights are held fixed). Each target is then the 2023 file's level without them, grown, plus the year's
list's own fixed contribution, so the start meets it exactly before the main reweighting.

How a series is swapped (for agent B and later work): edit the item's row of the items table, or point settings
tilt.projection.items_table at a copy. series is 'current' (the rule of today's code: the CBO or macro series per
return of projection_growth.plan_factors, turned back into a growth in total by CBO's growth in returns, kind
'total'; or, for an item with no such rule, growth.yaml's per-return rule, kind 'per_unit'), 'file:<csv>:<column>'
(a csv under research/rebuild/resources with a 'year' column; the growth is the column's value in the year over its
value in 2023, both present and positive, never extended past the data), 'same_as:<column>' (the factors of another
item), or 'none' (not grown). kind is 'auto' (with 'current': today's kind; with 'same_as' kind is not used), 'total'
or 'per_unit'. target is 1 or 0; error is
the relative standard error of the item's total target.
"""
import os

import numpy as np
import pandas as pd

import common

HERE = os.path.dirname(os.path.abspath(__file__))
RESOURCES = os.path.join(os.path.dirname(HERE), 'resources')
P = common.SETTINGS['tilt'].get('projection', {})
TOP_AGE = 80          # ages 80 and over are one cell (the file caps the primary's and spouse's ages at 80)
MARRIED_FROM = 16     # CBO's population has no married person under 16
ITEMS_COLUMNS = ['column', 'series', 'kind', 'target', 'error', 'note']
PREFIX = {'persons': 'pp__', 'returns': 'pr__', 'item_pos': 'pi+__', 'item_neg': 'pi-__'}

STATE = {}   # this run's year: set by tilt (the 2023 file, its weights, step 1 and 2 results, the start)


# ---- people by marital status and single year of age ---------------------------------------------------------
def person_matrix(df):
    """(records x 2 (TOP_AGE + 1)) people counts: column married x (TOP_AGE + 1) + age (see the module docstring)."""
    n = len(df)
    M = np.zeros((n, 2 * (TOP_AGE + 1)), np.float32)
    own = df['age_range'].to_numpy() < common.ADULT_RANGES
    mar = df['married'].to_numpy() == 1
    rows = np.arange(n)
    for col, use in [('age1', own), ('age2', own & mar)]:
        a = df[col].to_numpy(float)
        assert np.isfinite(a).all(), f'{col}: missing ages'
        ok = use & (a >= 0)
        aa = np.minimum(a, TOP_AGE).astype(int)
        m_ = mar & (a >= MARRIED_FROM)
        np.add.at(M, (rows[ok], (m_[ok] * (TOP_AGE + 1) + aa[ok])), 1.0)
    for col in ['dep_age1', 'dep_age2', 'dep_age3']:
        a = df[col].to_numpy(float)
        assert np.isfinite(a).all(), f'{col}: missing ages'
        ok = a >= 0
        np.add.at(M, (rows[ok], np.minimum(a[ok], TOP_AGE).astype(int)), 1.0)
    own_missing = own & (df['age1'].to_numpy(float) < 0)
    assert not own_missing.any(), f'{int(own_missing.sum())} units other than dependent filers with no primary age'
    return M


def cell_label(k):
    m, a = divmod(int(k), TOP_AGE + 1)
    return f"{'married' if m else 'not married'}, age {a}{' and over' if a >= TOP_AGE else ''}"


_POP = {}


def cbo_population(year, married, age):
    """CBO's Social Security area population (settings tilt.projection.population_dir: historical.csv and
    projections.csv, married_<age> and unmarried_<age>, 100 meaning 100 and over) of one marital status at one age, or
    at TOP_AGE and over. A year in both files must hold the same numbers."""
    if 'pop' not in _POP:
        d = P['population_dir']
        h, p = pd.read_csv(os.path.join(d, 'historical.csv')), pd.read_csv(os.path.join(d, 'projections.csv'))
        both = pd.concat([h, p])
        cols = [c for c in both.columns if c.startswith(('married_', 'unmarried_'))]
        for y_ in both[both.duplicated('year', keep=False)]['year'].unique():
            a_ = both[both.year == y_][cols].to_numpy(float)
            assert np.allclose(a_[0], a_[1:], rtol=1e-9), f'population {y_}: historical and projections differ'
        _POP['pop'] = both.drop_duplicates('year').set_index('year')
    t = _POP['pop']
    assert int(year) in t.index, f'CBO population: no {year}'
    pre = 'married_' if married else 'unmarried_'
    ages = range(age, 101) if age >= TOP_AGE else [age]
    v = t.loc[int(year), [f'{pre}{a}' for a in ages]].to_numpy(float)
    assert np.isfinite(v).all() and (v >= 0).all(), f'population {year} {pre}{age}: missing or negative'
    return float(v.sum())


# ---- return groups: the primary filer's age band and marital status ---------------------------------------------
def return_group(df):
    """Each record's return group: for units other than dependent filers, the primary's age band (settings
    tilt.projection.return_age_edges, ages capped at 80) x married; for dependent filers their age range x married."""
    edges = list(P['return_age_edges'])
    rng = df['age_range'].to_numpy()
    a = np.minimum(df['age1'].to_numpy(float), TOP_AGE)
    band = np.where(rng < common.ADULT_RANGES, np.searchsorted(edges, a, side='right'),
                    len(edges) + 1 + (rng - common.ADULT_RANGES))
    return 2 * band + (df['married'].to_numpy() == 1)


def group_label(g):
    edges = list(P['return_age_edges'])
    band, m = divmod(int(g), 2)
    if band <= len(edges):
        lo = 0 if band == 0 else edges[band - 1]
        name = (f'under {edges[0]}' if band == 0 else f'{lo} and over' if band == len(edges) else f'{lo}-{edges[band] - 1}')
        name = f'primary aged {name}'
    else:
        name = f'dependent filers {common.AGE_LABELS[band - len(edges) - 1 + common.ADULT_RANGES]}'
    return f"{name}, {'married' if m else 'not married'}"


# ---- step 1 ---------------------------------------------------------------------------------------------------
def step1(df, w, year, s):
    """The step 1 weights of the s file df (weights w) for year: see the module docstring. Records of the s file's
    Forbes list (billionaire == 1) keep their weight (the top step removes them). Returns the weights and a dict for
    the summary; sets STATE."""
    import projection_demographics as PD
    import solver as SV
    keep = (df['billionaire'].to_numpy() != 1) if 'billionaire' in df else np.ones(len(df), bool)
    M = person_matrix(df)
    lev = w[keep] @ M[keep]
    cells, T, F_cols, labels = [], [], [], []
    for k in range(M.shape[1]):
        if lev[k] == 0:
            continue
        m, a = divmod(k, TOP_AGE + 1)
        base = cbo_population(s, m, a)
        assert base > 0, f'{cell_label(k)}: the {s} file has {lev[k]:,.0f} people, CBO none'
        g = cbo_population(year, m, a) / base
        cells.append({'cell': cell_label(k), 'column': k, 'base': lev[k], 'growth': g, 'target': lev[k] * g})
    # the dependent filers' own cells, by today's population factors
    scaled, _ = PD.population_cells(df.assign(weight=w), year, from_year=s)
    scaled = np.asarray(scaled, float)
    rng, mar = df['age_range'].to_numpy(), df['married'].to_numpy()
    dep_cells = []
    for r in common.DEPENDENT_RANGES:
        for m in (0, 1):
            c = keep & (rng == r) & (mar == m)
            if w[c].sum() > 0:
                dep_cells.append({'cell': f'dependent filers {common.AGE_LABELS[r]}, {"married" if m else "not married"}',
                                  'column': -1, 'mask': c, 'base': w[c].sum(), 'growth': scaled[c].sum() / w[c].sum(),
                                  'target': scaled[c].sum()})
    F = np.column_stack([M[keep][:, [c['column'] for c in cells]].astype(float)] +
                        [c['mask'][keep].astype(float) for c in dep_cells])
    T = np.array([c['target'] for c in cells] + [c['target'] for c in dep_cells])
    m_ = len(T)
    S_tilt = common.SETTINGS['tilt']
    w_k, info = SV.solve(w[keep], F, T, np.zeros(m_), np.ones(m_, bool), [None] * m_,
                         {**S_tilt, 'solver_importance': np.ones(m_)})
    assert info['converged'], f'step 1 did not converge: {info["how"]}'
    got = F.T @ w_k
    assert np.allclose(got, T, rtol=1e-7), f'step 1: largest miss {np.max(np.abs(got / T - 1)):.2e}'
    w1 = w.copy()
    w1[keep] = w_k
    ess = SV.effective_sample
    f = np.where(w[keep] > 0, w_k / np.where(w[keep] > 0, w[keep], 1), 1.0)
    table = pd.DataFrame([{k: v for k, v in c.items() if k != 'mask'} for c in cells + dep_cells])
    table['after_step1'] = got
    out = {'converged': info['converged'], 'how': info['how'], 'Newton steps': info['newton_steps'],
           'cells': m_, 'KL move (nats)': info['kl_move'] if 'kl_move' in info else SV.kl_move(w[keep], w_k),
           'effective sample kept (%)': 100 * ess(w_k) / ess(w[keep]),
           'weight factor, 1st percentile': float(np.quantile(f, 0.01)),
           'weight factor, 99th percentile': float(np.quantile(f, 0.99)),
           'units, 2023 file (millions)': w[keep].sum() / 1e6, 'units after step 1 (millions)': w_k.sum() / 1e6}
    common.say(f"step 1 (people by age and marital status, {m_} exact cells): {info['how']}; effective sample kept "
               f"{out['effective sample kept (%)']:.2f}%; units {out['units, 2023 file (millions)']:.2f} -> "
               f"{out['units after step 1 (millions)']:.2f} million")
    # returns: the 2023 share of each group's units that file
    grp = return_group(df)
    fil = df['filer'].to_numpy() == 1
    shares = {}
    for g in np.unique(grp[keep]):
        c = keep & (grp == g)
        if w[c].sum() > 0:
            shares[int(g)] = {'units 2023': w[c].sum(), 'filers 2023': w[c & fil].sum(),
                              'units after step 1': w1[c].sum(), 'filers after step 1': w1[c & fil].sum()}
    STATE.clear()
    STATE.update(year=int(year), base_year=int(s), record_id=df['record_id'].to_numpy(), keep=keep, w_base=w.copy(),
                 w1=w1, person_targets=table, step1=out, return_shares=shares, items={})
    return w1, out


# ---- step 2 ---------------------------------------------------------------------------------------------------
_TABLE = {}


def items_table():
    """The items table (settings tilt.projection.items_table, relative to the pipeline folder or absolute)."""
    if 't' not in _TABLE:
        p = P['items_table']
        p = p if os.path.isabs(p) else os.path.join(HERE, p)
        t = pd.read_csv(p, dtype={'column': str, 'series': str, 'kind': str, 'note': str}, keep_default_na=False)
        assert list(t.columns) == ITEMS_COLUMNS, f'{p}: columns {list(t.columns)}, not {ITEMS_COLUMNS}'
        assert t['column'].is_unique, f'{p}: a column twice'
        assert t['kind'].isin(['auto', 'total', 'per_unit']).all(), f'{p}: kind is auto, total or per_unit'
        assert t['target'].isin([0, 1]).all(), f'{p}: target is 0 or 1'
        bad = t[(t.target == 1) & ~(pd.to_numeric(t['error'], errors='coerce') > 0)]
        assert bad.empty, f'{p}: targeted items with no positive error: {bad.column.tolist()}'
        bad = t[(t.kind == 'auto') & ~((t.series == 'current') | t.series.str.startswith('same_as:'))]
        assert bad.empty, f'{p}: kind auto is only for series current or same_as: {bad.column.tolist()}'
        _TABLE['t'] = t.set_index('column')
        _TABLE['path'] = p
    return _TABLE['t']


def series_growth(spec, s, t):
    """A 'file:<csv>:<column>' series' growth from year s to year t (both in the file, positive; never extended)."""
    _, path, col = spec.split(':', 2)
    d = pd.read_csv(os.path.join(RESOURCES, path))
    assert 'year' in d and col in d, f'{path}: needs columns year and {col}'
    d = d.set_index('year')[col]
    for y in (s, t):
        assert y in d.index and np.isfinite(d.loc[y]) and d.loc[y] > 0, f'{path} {col}: no positive value for {y}'
    return float(d.loc[t] / d.loc[s]), f'{path}: {col}, {s} to {t}'


def step2_factor(col, x, fp_cur, fn_cur, kind_cur, series_cur, s, t, R):
    """Step 2's factors (positive values, negative values) for column col, amounts x of the s file: see the module
    docstring. fp_cur, fn_cur, kind_cur, series_cur: today's rule for the column (tilt.grow_dollars). R: CBO's growth
    in the number of returns from s to t (turns today's per-return series back into a growth in total). Returns
    (fp, fn, description); records the item in STATE['items']. A 'same_as' row is handled by the caller."""
    tab = items_table()
    assert col in tab.index, f'{col}: grown by the projection start but not in the items table {_TABLE["path"]}'
    row = tab.loc[col]
    series, kind = row['series'], row['kind']
    if series == 'current':
        kind = kind_cur if kind == 'auto' else kind
        if kind == 'total':
            G = (fp_cur * R, fn_cur * R)
            what = f'{series_cur}; times CBO growth in returns {R:.5f} = growth in total'
        else:
            G = (fp_cur, fn_cur)
            what = series_cur
    elif series == 'none':
        assert kind == 'per_unit', f'{col}: series none is kind per_unit'
        G, what = (1.0, 1.0), 'not grown'
    elif series.startswith('file:'):
        g, what = series_growth(series, s, t)
        G = (g, g)
    else:
        raise ValueError(f'{col}: unknown series {series!r}')
    keep, w0, w1 = STATE['keep'], STATE['w_base'], STATE['w1']
    rec = {'column': col, 'series': series, 'kind': kind, 'source': what, 'target': int(row['target']),
           'error': row['error'], 'growth positive': G[0], 'growth negative': G[1]}
    fs = list(G)
    for i, sign in enumerate((1, -1)):
        m = keep & (sign * x > 0)
        t23, t1 = float(w0[m] @ x[m]), float(w1[m] @ x[m])
        rec[f'2023 total {"positive" if sign == 1 else "negative"}'] = t23
        rec[f'records {"positive" if sign == 1 else "negative"}'] = int(m.sum())
        if kind == 'total' and m.any() and t1 != 0:
            fs[i] = G[i] * t23 / t1
        rec[f'factor {"positive" if sign == 1 else "negative"}'] = fs[i]
    assert all(np.isfinite(f_) and f_ > 0 for f_ in fs), f'{col}: step 2 factors {fs}'
    STATE['items'][col] = rec
    return fs[0], fs[1], f'{kind}: {what}'


def same_as(col):
    """The column a 'same_as:<column>' row points to, or None."""
    sr = items_table().loc[col, 'series'] if col in items_table().index else ''
    return sr.split(':', 1)[1] if sr.startswith('same_as:') else None


def record_same_as(col, other, x):
    r = dict(STATE['items'][other])
    keep, w0 = STATE['keep'], STATE['w_base']
    # the other column's factors record by record: on this column's records that is a growth per record (kind
    # per_unit), whatever the other column's kind
    r.update(column=col, series=f'same_as:{other}', kind='per_unit', target=int(items_table().loc[col, 'target']),
             error=items_table().loc[col, 'error'], source=f"the factors of {other} record by record ({r['source'][:120]})",
             **{'growth positive': r['factor positive'], 'growth negative': r['factor negative']})
    for sign, nm in ((1, 'positive'), (-1, 'negative')):
        m = keep & (sign * x > 0)
        r[f'2023 total {nm}'], r[f'records {nm}'] = float(w0[m] @ x[m]), int(m.sum())
    STATE['items'][col] = r


def scf_factor(v):
    """(SCF targets, scf_targets.projection_extension) the factor for a balance sheet item on the SCF's units: kind
    total, the growth in total over the growth of the file's units after step 1 (the SCF units grow with them); kind
    per_unit, the step 2 factor itself."""
    r = STATE['items'][v]
    if r['kind'] == 'total':
        return r['growth positive'] / unit_growth()
    return r['factor positive']


def unit_growth():
    k = STATE['keep']
    return float(STATE['w1'][k].sum() / STATE['w_base'][k].sum())


# ---- step 3: the targets ---------------------------------------------------------------------------------------
def handles(v):
    return isinstance(v, str) and v.startswith(tuple(PREFIX.values()))


def variable_def(v):
    for kind, pre in PREFIX.items():
        if v.startswith(pre):
            return {'short': v[len(pre):], 'projection': kind, 'what': f'projection step 3, {kind}'}
    raise KeyError(v)


_CACHE = {}


def record_value(df, v):
    kind = variable_def(v)['projection']
    key = v[len(PREFIX[kind]):]
    if kind == 'persons':
        ck = ('persons', id(df), len(df))
        if _CACHE.get('pk') != ck:
            _CACHE.update(pk=ck, persons=person_matrix(df))
        return _CACHE['persons'][:, int(key)].astype(float)
    if kind == 'returns':
        return ((df['filer'].to_numpy() == 1) & (return_group(df) == int(key))).astype(float)
    x = df[key].to_numpy(float)
    assert np.isfinite(x).all(), f'{key}: {int((~np.isfinite(x)).sum())} missing values'
    return np.maximum(x, 0) if kind == 'item_pos' else np.maximum(-x, 0)


def _start():
    assert STATE.get('start') is not None, 'projection targets need the start (tilt sets PS.STATE["start"])'
    return STATE['start']


def _list_part(df, w, fixed, x):
    return float((w[fixed] * x[fixed]).sum())


def _row(label, value, variable, source):
    return {'cell': label, 'statistic': 'amount', 'value': float(value), 'variable': variable, 'source': source,
            'cell_spec': {'parts': {}, 'filters': [], 'label': label, 'agi': None, 'line': None}}


def outside_rows(src, year):
    """The rows of an outside source of this module (target_list.outside_rows)."""
    assert STATE.get('year') == int(year), f'{src}: step 1 was not run for {year}'
    df, w, fixed = _start()
    lst = df['billionaire'].to_numpy() == 1 if 'billionaire' in df else np.zeros(len(df), bool)
    assert not (lst & ~fixed).any(), 'a Forbes list record is not held fixed'
    out = []
    if src == 'projection persons':
        for r in STATE['person_targets'].itertuples(index=False):
            if r.column < 0:
                continue   # the dependent filers' cells: covered by their returns groups (every dependent filer files)
            v = f"{PREFIX['persons']}{int(r.column)}"
            lp = _list_part(df, w, lst, record_value(df, v))
            out.append(_row(r.cell, r.target + lp, v,
                            f"the 2023 file's {r.base:,.0f} people (Forbes list left out) times CBO's growth of the "
                            f"same people {r.growth:.5f} (Macro-Projections), plus the year's list {lp:,.0f}"))
    elif src == 'projection returns':
        for g, d in STATE['return_shares'].items():
            v = f"{PREFIX['returns']}{g}"
            lp = _list_part(df, w, lst, record_value(df, v))
            share = d['filers 2023'] / d['units 2023']
            out.append(_row(group_label(g), share * d['units after step 1'] + lp, v,
                            f"the 2023 file's filing share {share:.5f} ({d['filers 2023']:,.0f} of {d['units 2023']:,.0f} "
                            f"units) times the group's units after step 1, {d['units after step 1']:,.0f}, plus the "
                            f"year's list {lp:,.0f}"))
    elif src == 'projection items':
        mn_rec, mn_usd = int(P['item_min_records']), float(P['item_min_dollars'])
        for col, r in STATE['items'].items():
            if not r['target']:
                continue
            for nm, pre in (('positive', 'item_pos'), ('negative', 'item_neg')):
                if r[f'records {nm}'] < mn_rec or abs(r[f'2023 total {nm}']) < mn_usd:
                    continue
                v = f'{PREFIX[pre]}{col}'
                x = record_value(df, v)
                val = float(w @ x)
                lp = _list_part(df, w, lst, x)
                expect = abs(r[f'2023 total {nm}']) * r[f'growth {nm}'] if r['kind'] == 'total' else None
                if expect is not None:   # the start must sit on its series (step 2), the list apart
                    assert abs((val - lp) / expect - 1) < 1e-6, \
                        f'{col} {nm}: the start {val - lp:,.0f} is not the 2023 total times its growth {expect:,.0f}'
                label = f"{col}{' (losses)' if nm == 'negative' else ''}"
                out.append(_row(label, val, v, f"the start's total after step 2 ({r['kind']}: {r['source'][:160]}); "
                                               f"the year's list {lp:,.0f}") | {'se': float(r['error']) * val})
    else:
        raise KeyError(src)
    return out


def write(folder):
    """step1_cells.csv, step2_items.csv and the summary lines."""
    STATE['person_targets'].to_csv(f'{folder}/projection_step1_cells.csv', index=False)
    items = pd.DataFrame(list(STATE['items'].values()))
    items.to_csv(f'{folder}/projection_step2_items.csv', index=False)
    sh = pd.DataFrame([{'group': group_label(g), **d} for g, d in STATE['return_shares'].items()])
    sh.to_csv(f'{folder}/projection_return_groups.csv', index=False)
    lines = ['## The projection start (projection_steps.py): step 1, people by age and marital status', '',
             pd.Series(STATE['step1']).to_frame('').to_markdown(), '',
             f"Step 2: {len(items)} columns; kinds {items['kind'].value_counts().to_dict()}; targeted "
             f"{int(items['target'].sum())} (projection_step2_items.csv). Items table: {_TABLE.get('path')}", '']
    return lines
