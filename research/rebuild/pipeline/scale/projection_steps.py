"""The projection years: three steps that turn an earlier year's reweighted file into a projection year's start and
targets.

1. People. The weights are calibrated to the projection year's people by marital status and single year of age (step1).
2. Dollars. Each dollar item gets one growth factor (step2_factor, called by start.grow_dollars).
3. Targets. The module supplies the reweighting's targets from these steps (outside_rows): the people, the returns in
   each group of age and marital status, and each targeted item's total.

Two rules apply in particular years. The first projection year blends SOI's November filing-season tables with the
national series (the "filing-season blend"). After the jump-off year, taxable interest grows with interest rates.
FEATURES.md, "4. Scaling to the target year", "Projection years", describes the method.
"""
import os
from functools import lru_cache

import numpy as np
import pandas as pd

from shared import common
from prep import soi as TG
from reweight import solver as SV
from scale import projection_demographics as PD

RESOURCES = common.RESOURCES
P = common.SETTINGS['tilt']['projection']
TOP_AGE = 80          # ages 80 and over are one cell (the file caps the primary filer's and spouse's ages at 80)
# Married people under 18 count as unmarried, in the file and in CBO's population. CBO's population has no married
# person under 16, so those cells could not be met.
MARRIED_FROM = 18
ITEMS_COLUMNS = ['column', 'series', 'kind', 'target', 'error', 'note']
ADD_COLUMNS = ['column', 'first_year', 'last_year', 'percent', 'note']
# The prefixes of the variable names this module defines for its targets (people, returns groups, an item's positive
# and negative values, filing-season items).
PREFIX = {'persons': 'pp__', 'returns': 'pr__', 'item_pos': 'pi+__', 'item_neg': 'pi-__', 'filing_season': 'fs__'}

STATE = {}   # for this run's year: the base file, its weights, the results of steps 1 and 2, and the start


def _path(p):
    """A settings path, relative to the pipeline folder unless absolute."""
    return p if os.path.isabs(p) else os.path.join(common.PIPELINE, p)


# ---- People by marital status and single year of age ----

def person_matrix(df):
    """The number of people of each marital status and age in each record, as an array of records by
    2 x (TOP_AGE + 1) cells. The column of a cell is married x (TOP_AGE + 1) + age. The primary filer, a joint
    return's spouse and every dependent each count once. A dependent filer's own record counts only its dependents."""
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
    """The label of column k of person_matrix, such as 'married, age 40'."""
    m, a = divmod(int(k), TOP_AGE + 1)
    return f"{'married' if m else 'not married'}, age {a}{' and over' if a >= TOP_AGE else ''}"


@lru_cache(maxsize=None)
def _population_table():
    """CBO's population by year (settings tilt.projection.population_dir), from the historical and the projections
    files together. A year in both files must have the same numbers in each."""
    d = P['population_dir']
    both = pd.concat([pd.read_csv(os.path.join(d, 'historical.csv')), pd.read_csv(os.path.join(d, 'projections.csv'))])
    cols = [c for c in both.columns if c.startswith(('married_', 'unmarried_'))]
    for y_ in both[both.duplicated('year', keep=False)]['year'].unique():
        a_ = both[both.year == y_][cols].to_numpy(float)
        assert np.allclose(a_[0], a_[1:], rtol=1e-9), f'population {y_}: historical and projections differ'
    return both.drop_duplicates('year').set_index('year')


def cbo_population(year, married, age):
    """CBO's Social Security area population of one marital status at one age, or at TOP_AGE and over."""
    t = _population_table()
    assert int(year) in t.index, f'CBO population: no {year}'
    ages = range(age, 101) if age >= TOP_AGE else [age]
    assert not (married and age < MARRIED_FROM), f'no married cell under {MARRIED_FROM} (age {age})'
    pres = ['unmarried_', 'married_'] if age < MARRIED_FROM else ['married_' if married else 'unmarried_']
    v = t.loc[int(year), [f'{pre}{a}' for pre in pres for a in ages]].to_numpy(float)
    assert np.isfinite(v).all() and (v >= 0).all(), f'population {year} {pres} {age}: missing or negative'
    return float(v.sum())


# ---- Return groups: the primary filer's age band and marital status ----

def return_group(df):
    """Each record's return group, a number. For units other than dependent filers, the group is the primary filer's
    age band (settings tilt.projection.return_age_edges, ages capped at 80) by marital status. For dependent filers it
    is their age range by marital status."""
    edges = list(P['return_age_edges'])
    rng = df['age_range'].to_numpy()
    a = np.minimum(df['age1'].to_numpy(float), TOP_AGE)
    band = np.where(rng < common.ADULT_RANGES, np.searchsorted(edges, a, side='right'),
                    len(edges) + 1 + (rng - common.ADULT_RANGES))
    return 2 * band + (df['married'].to_numpy() == 1)


def group_label(g):
    """The label of return group g, such as 'primary aged 35-44, married'."""
    edges = list(P['return_age_edges'])
    band, m = divmod(int(g), 2)
    if band <= len(edges):
        lo = 0 if band == 0 else edges[band - 1]
        name = (f'under {edges[0]}' if band == 0 else f'{lo} and over' if band == len(edges) else f'{lo}-{edges[band] - 1}')
        name = f'primary aged {name}'
    else:
        name = f'dependent filers {common.AGE_LABELS[band - len(edges) - 1 + common.ADULT_RANGES]}'
    return f"{name}, {'married' if m else 'not married'}"


# ---- Step 1: the weights calibrated to people ----

def step1(df, w, year, s):
    """The step 1 weights for target year year, from the year-s file df with weights w. They are the weights with the
    smallest Kullback-Leibler move from w that meet two sets of counts exactly: the people by marital status and single
    year of age, and the dependent filers' six cells. Each count is the year-s file's count times CBO's growth of the
    same people. The Forbes list's records keep their weights, because the top step replaces them. Returns the weights
    and a summary dict, and fills STATE."""
    # 1. The people targets: each cell's count on the year-s file times CBO's growth of the same people.
    keep = (df['billionaire'].to_numpy() != 1) if 'billionaire' in df else np.ones(len(df), bool)
    M = person_matrix(df)
    lev = w[keep] @ M[keep]
    cells = []
    for k in np.flatnonzero(lev).tolist():
        m, a = divmod(k, TOP_AGE + 1)
        base = cbo_population(s, m, a)
        assert base > 0, f'{cell_label(k)}: the {s} file has {lev[k]:,.0f} people, CBO none'
        g = cbo_population(year, m, a) / base
        cells.append({'cell': cell_label(k), 'column': k, 'base': lev[k], 'growth': g, 'target': lev[k] * g})
    # 2. The dependent filers' six cells (age range by marital status), each grown with its population. The people
    #    counts do not include the dependent filers themselves.
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
    # 3. Solve for the weights that meet every count exactly, and check them.
    F = np.column_stack([M[keep][:, [c['column'] for c in cells]].astype(float)] +
                        [c['mask'][keep].astype(float) for c in dep_cells])
    T = np.array([c['target'] for c in cells] + [c['target'] for c in dep_cells])
    m_ = len(T)
    w_k, info = SV.solve(w[keep], F, T, np.zeros(m_), np.ones(m_, bool), [None] * m_,
                         {**common.SETTINGS['tilt'], 'solver_importance': np.ones(m_)})
    assert info['converged'], f'step 1 did not converge: {info["how"]}'
    got = F.T @ w_k
    assert np.allclose(got, T, rtol=1e-7), f'step 1: largest miss {np.max(np.abs(got / T - 1)):.2e}'
    w1 = w.copy()
    w1[keep] = w_k
    f = np.where(w[keep] > 0, w_k / np.where(w[keep] > 0, w[keep], 1), 1.0)
    table = pd.DataFrame([{k: v for k, v in c.items() if k != 'mask'} for c in cells + dep_cells])
    table['after_step1'] = got
    out = {'converged': info['converged'], 'how': info['how'], 'Newton steps': info['newton_steps'],
           'cells': m_, 'KL move (nats)': info['kl_move'] if 'kl_move' in info else SV.kl_move(w[keep], w_k),
           'effective sample kept (%)': 100 * SV.effective_sample(w_k) / SV.effective_sample(w[keep]),
           'weight factor, 1st percentile': float(np.quantile(f, 0.01)),
           'weight factor, 99th percentile': float(np.quantile(f, 0.99)),
           'units, 2023 file (millions)': w[keep].sum() / 1e6, 'units after step 1 (millions)': w_k.sum() / 1e6}
    common.say(f"step 1 (people by age and marital status, {m_} exact cells): {info['how']}; effective sample kept "
               f"{out['effective sample kept (%)']:.2f}%; units {out['units, 2023 file (millions)']:.2f} -> "
               f"{out['units after step 1 (millions)']:.2f} million")
    # 4. For the returns targets: the base file's share of each return group's units that file.
    grp = return_group(df)
    fil = df['filer'].to_numpy() == 1
    shares = {}
    for g in np.unique(grp[keep]):
        c = keep & (grp == g)
        if w[c].sum() > 0:
            shares[int(g)] = {'units 2023': w[c].sum(), 'filers 2023': w[c & fil].sum(),
                              'units after step 1': w1[c].sum(), 'filers after step 1': w1[c & fil].sum()}
    # 5. Keep the results for steps 2 and 3, with what the filing-season blend and the interest rule need.
    STATE.clear()
    STATE.update(year=int(year), base_year=int(s), record_id=df['record_id'].to_numpy(), keep=keep, w_base=w.copy(),
                 w1=w1, person_targets=table, step1=out, return_shares=shares, items={}, add_factors=[])
    if fs_active(year, s):   # the filing-season blend needs the base file's own items by AGI group
        STATE['df_base'] = df
        STATE['fs'] = {'blend': {}, 'columns': {}}
    if ig_active(s):         # the split of non-qualified dividends on the start file
        STATE['interest'] = ig_split(df, w, s)
    return w1, out


# ---- Step 2: one growth factor for each dollar item ----

@lru_cache(maxsize=None)
def items_table():
    """The items table (settings tilt.projection.items_table), checked. One row per grown column, with its series
    ('current', 'file:<csv>:<column>', 'same_as:<column>' or 'none'), its kind ('total' or 'per_unit'), whether its
    total is targeted, and the relative error of that target."""
    p = _path(P['items_table'])
    t = pd.read_csv(p, dtype={'column': str, 'series': str, 'kind': str, 'note': str}, keep_default_na=False)
    assert list(t.columns) == ITEMS_COLUMNS, f'{p}: columns {list(t.columns)}, not {ITEMS_COLUMNS}'
    assert t['column'].is_unique, f'{p}: a column twice'
    assert t['kind'].isin(['auto', 'total', 'per_unit']).all(), f'{p}: kind is auto, total or per_unit'
    assert t['target'].isin([0, 1]).all(), f'{p}: target is 0 or 1'
    bad = t[(t.target == 1) & ~(pd.to_numeric(t['error'], errors='coerce') > 0)]
    assert bad.empty, f'{p}: targeted items with no positive error: {bad.column.tolist()}'
    bad = t[(t.kind == 'auto') & ~t.series.str.startswith('same_as:')]
    assert bad.empty, f'{p}: kind auto is only for series same_as: {bad.column.tolist()}'
    bad = t[(t.series == 'current') & (t.kind != 'per_unit')]
    assert bad.empty, (f'{p}: rows with series current must be kind per_unit (a growth.yaml or SCF rule per unit); '
                       f'a CBO series goes in a series file: {bad.column.tolist()}')
    return t.set_index('column')


@lru_cache(maxsize=None)
def add_factors_table():
    """The add factors table (settings tilt.projection.add_factors_table), checked. An add factor is extra growth, in
    percent a year, on top of an item's series, set by hand. The table has one row per item and range of years. Every
    column must be in the items table, no year of an item may be covered twice, and every non-zero percent must have a
    note."""
    p = _path(P['add_factors_table'])
    t = pd.read_csv(p, dtype={'column': str, 'note': str}, keep_default_na=False)
    assert list(t.columns) == ADD_COLUMNS, f'{p}: columns {list(t.columns)}, not {ADD_COLUMNS}'
    for c in ('first_year', 'last_year', 'percent'):
        v = pd.to_numeric(t[c], errors='coerce')
        assert v.notna().all() and np.isfinite(v).all(), f'{p}: {c} missing or not a number on rows {t.index[v.isna()].tolist()}'
        t[c] = v
    assert (t.first_year == t.first_year.round()).all() and (t.last_year == t.last_year.round()).all(), \
        f'{p}: years must be whole numbers'
    t[['first_year', 'last_year']] = t[['first_year', 'last_year']].astype(int)
    assert (t.first_year <= t.last_year).all(), f'{p}: first_year after last_year'
    assert (t.percent > -100).all(), f'{p}: a percent of -100 or less'
    bad = t[(t.percent != 0) & (t.note.str.strip() == '')]
    assert bad.empty, f'{p}: non-zero add factors with no note (who, when, why): {bad.column.tolist()}'
    missing = sorted(set(t.column) - set(items_table().index))
    assert not missing, f'{p}: columns not in the items table {_path(P["items_table"])}: {missing}'
    for col, g in t.groupby('column'):
        years = [y for r in g.itertuples() for y in range(r.first_year, r.last_year + 1)]
        assert len(years) == len(set(years)), f'{p}: {col} has a year in two rows'
    return t


def add_factor(col, s, t):
    """Column col's add factor from base year s to year t: the product of (1 + percent / 100) over the years s + 1 to
    t that its rows of the add factors table cover (exactly 1 when there are none). Returns (factor, the rows in
    force)."""
    tab = add_factors_table()
    # A row that covers years up to the start year s counts only its years after s. The start's file already carries
    # the add factors of the earlier years.
    f, used = 1.0, []
    for r in tab[tab.column == col].itertuples():
        n = len(range(max(r.first_year, s + 1), min(r.last_year, t) + 1))
        if n > 0 and r.percent != 0:
            f *= (1 + r.percent / 100) ** n
            used.append({'column': col, 'first_year': r.first_year, 'last_year': r.last_year, 'percent': r.percent,
                         'years in force': n, 'note': r.note})
    return f, used


def series_growth(spec, s, t):
    """The growth of a 'file:<csv>:<column>' series from year s to year t. Both years must be in the file with positive
    values; the series is never extended."""
    _, path, col = spec.split(':', 2)
    d = pd.read_csv(os.path.join(RESOURCES, path))
    assert 'year' in d and col in d, f'{path}: needs columns year and {col}'
    d = d.set_index('year')[col]
    for y in (s, t):
        assert y in d.index and np.isfinite(d.loc[y]) and d.loc[y] > 0, f'{path} {col}: no positive value for {y}'
    return float(d.loc[t] / d.loc[s]), f'{path}: {col}, {s} to {t}'


def step2_factor(col, x, fp_cur, fn_cur, series_cur, s, t):
    """Step 2's factors for column col (for positive values and for negative values), with x the column's amounts on
    the year-s file. The growth is the row's series, times the filing-season blend and the add factors where they
    apply. For kind 'total' the factor also carries the base file's total over the total on the step 1 weights. The
    item's total then grows exactly at its series, and population growth is not counted twice. fp_cur, fn_cur and
    series_cur are the per-unit rule that start.grow_dollars uses for a 'current' row. Returns (fp, fn, description)
    and records the item in STATE['items']."""
    tab = items_table()
    assert col in tab.index, f'{col}: grown by the projection start but not in the items table {_path(P["items_table"])}'
    row = tab.loc[col]
    series, kind = row['series'], row['kind']
    if series == 'current':
        G, what = (fp_cur, fn_cur), series_cur
    elif series == 'none':
        assert kind == 'per_unit', f'{col}: series none is kind per_unit'
        G, what = (1.0, 1.0), 'not grown'
    elif series.startswith('file:'):
        g, what = series_growth(series, s, t)
        G = (g, g)
    else:
        raise ValueError(f'{col}: unknown series {series!r}')
    if ig_active(s) and col in IG_COLUMNS:   # the interest rule replaces the row's series
        assert kind == 'total' and series.startswith('file:'), f'{col}: the interest rule needs a file series of kind total'
        g, what = ig_growth(col, s, t)
        G = (g, g)
    fsm = fs_column_factor(col, int(t))
    if fsm is not None:   # the filing-season blend, carried into every later year
        assert kind == 'total' and series.startswith('file:'), f'{col}: the filing-season blend needs a file series of kind total'
        G = (G[0] * fsm[0], G[1] * fsm[0])
        what = f'{what}; times the filing-season blend {fsm[0]:.5f} ({fsm[1]})'
    A, used = add_factor(col, s, t)
    if used:   # with no add factor in force, G is left exactly as it was
        G = (G[0] * A, G[1] * A)
        spans = ', '.join('%+g%% a year %d-%d' % (u['percent'], u['first_year'], u['last_year']) for u in used)
        what = f'{what}; times add factors {A:.5f} ({spans})'
        STATE['add_factors'] += used
    keep, w0, w1 = STATE['keep'], STATE['w_base'], STATE['w1']
    rec = {'column': col, 'series': series, 'kind': kind, 'source': what, 'target': int(row['target']),
           'error': row['error'], 'add factor': A, 'filing season factor': 1.0 if fsm is None else fsm[0],
           'growth positive': G[0], 'growth negative': G[1]}
    fs = list(G)
    for i, (sign, nm) in enumerate(((1, 'positive'), (-1, 'negative'))):
        m = keep & (sign * x > 0)
        t23, t1 = float(w0[m] @ x[m]), float(w1[m] @ x[m])
        rec[f'2023 total {nm}'] = t23
        rec[f'records {nm}'] = int(m.sum())
        if kind == 'total' and m.any() and t1 != 0:
            fs[i] = G[i] * t23 / t1
        rec[f'factor {nm}'] = fs[i]
    assert all(np.isfinite(f_) and f_ > 0 for f_ in fs), f'{col}: step 2 factors {fs}'
    STATE['items'][col] = rec
    return fs[0], fs[1], f'{kind}: {what}'


def same_as(col):
    """The column a 'same_as:<column>' row points to, or None."""
    sr = items_table().loc[col, 'series'] if col in items_table().index else ''
    return sr.split(':', 1)[1] if sr.startswith('same_as:') else None


def check_add_factors_applied(t):
    """A check after step 2: every item with a non-zero add factor in force by year t was grown by the start.
    Otherwise the table names a column the file does not have, and its add factor would be lost without notice."""
    tab = add_factors_table()
    live = tab[(tab.percent != 0) & (tab.first_year <= t)]
    lost = sorted(set(live.column) - set(STATE['items']))
    assert not lost, f'{_path(P["add_factors_table"])}: add factors in force for {t} on columns step 2 did not grow: {lost}'


def same_as_add_factor(col, s, t):
    """A same_as column's own add factor, applied on top of the other column's factors (which already carry the other
    column's add factors). Returns the factor and records the rows in force."""
    A, used = add_factor(col, s, t)
    STATE['add_factors'] += used
    return A


def record_same_as(col, other, x, A=1.0):
    """Records a same_as column in STATE['items']: it takes the other column's factors record by record, times its own
    add factor A."""
    r = dict(STATE['items'][other])
    keep, w0 = STATE['keep'], STATE['w_base']
    # On this column's records the other column's factors are a growth per record (kind per_unit), whatever the other
    # column's kind.
    src = f"the factors of {other} record by record ({r['source'][:120]})"
    if A != 1.0:
        src += f'; times its own add factors {A:.5f}'
    r.update(column=col, series=f'same_as:{other}', kind='per_unit', target=int(items_table().loc[col, 'target']),
             error=items_table().loc[col, 'error'], source=src, **{'add factor': r['add factor'] * A},
             **{'growth positive': r['factor positive'] * A, 'growth negative': r['factor negative'] * A})
    for sign, nm in ((1, 'positive'), (-1, 'negative')):
        m = keep & (sign * x > 0)
        r[f'2023 total {nm}'], r[f'records {nm}'] = float(w0[m] @ x[m]), int(m.sum())
    STATE['items'][col] = r


def scf_factor(v):
    """The factor for a balance-sheet item on the SCF's units, for scf_targets.projection_extension. For kind total it
    is the growth in the total over the growth of the file's units after step 1, because the SCF's units grow with
    the file's. For kind per_unit it is the step 2 factor itself."""
    r = STATE['items'][v]
    if r['kind'] == 'total':
        return r['growth positive'] / unit_growth()
    return r['factor positive']


def unit_growth():
    """The growth in the file's number of units in step 1, Forbes list left out."""
    k = STATE['keep']
    return float(STATE['w1'][k].sum() / STATE['w_base'][k].sum())


# ---- Step 3: the targets ----

def handles(v):
    """Whether v is one of this module's target variables (its name starts with a PREFIX)."""
    return isinstance(v, str) and v.startswith(tuple(PREFIX.values()))


def variable_def(v):
    """The definition of one of this module's target variables, as target_list.variable_def returns it."""
    for kind, pre in PREFIX.items():
        if v.startswith(pre):
            return {'short': v[len(pre):], 'projection': kind, 'what': f'projection step 3, {kind}'}
    raise KeyError(v)


_PERSONS = {}   # the person matrix of the frame last asked for (each people target reads one of its columns)


def record_value(df, v):
    """Each record's value of one of this module's target variables: its people in one cell, 1 if it files in one
    return group, its value of a filing-season item, or the positive or negative part of an item."""
    kind = variable_def(v)['projection']
    key = v[len(PREFIX[kind]):]
    if kind == 'persons':
        if _PERSONS.get('frame') != (id(df), len(df)):
            _PERSONS.update(frame=(id(df), len(df)), matrix=person_matrix(df))
        return _PERSONS['matrix'][:, int(key)].astype(float)
    if kind == 'returns':
        return ((df['filer'].to_numpy() == 1) & (return_group(df) == int(key))).astype(float)
    if kind == 'filing_season':
        return fs_value(df, key)
    x = df[key].to_numpy(float)
    assert np.isfinite(x).all(), f'{key}: {int((~np.isfinite(x)).sum())} missing values'
    return np.maximum(x, 0) if kind == 'item_pos' else np.maximum(-x, 0)


def _row(label, value, variable, source):
    """A target row of an amount, for target_list."""
    return {'cell': label, 'statistic': 'amount', 'value': float(value), 'variable': variable, 'source': source,
            'cell_spec': {'parts': {}, 'filters': [], 'label': label, 'agi': None, 'line': None}}


def _start():
    """The start that the reweighting stored in STATE['start'] (the records and their weights) and a mask of the Forbes
    list's records. Every list record must be held fixed."""
    assert STATE.get('start') is not None, 'projection targets need the start (tilt sets PS.STATE["start"])'
    df, w, fixed = STATE['start']
    lst = df['billionaire'].to_numpy() == 1 if 'billionaire' in df else np.zeros(len(df), bool)
    assert not (lst & ~fixed).any(), 'a Forbes list record is not held fixed'
    return df, w, lst


def outside_rows(src, year):
    """The target rows of one of this module's outside sources (called by target_list.outside_rows). Every target adds
    the year's Forbes list's contribution to the value grown from the base file.

    - 'projection persons': the people by marital status and age, from step 1.
    - 'projection returns': the returns in each return group, at the base file's filing share times the group's units
      after step 1.
    - 'projection items': each targeted item's total after step 2, positive and negative values separately, where each
      has enough records and dollars.
    - 'projection filing season': the filing-season targets by AGI group (fs_rows)."""
    assert STATE.get('year') == int(year), f'{src}: step 1 was not run for {year}'
    df, w, lst = _start()
    out = []
    if src == 'projection persons':
        for r in STATE['person_targets'].itertuples(index=False):
            if r.column < 0:
                continue   # the dependent filers' cells: their return groups cover them (every dependent filer files)
            v = f"{PREFIX['persons']}{int(r.column)}"
            lp = float((w[lst] * record_value(df, v)[lst]).sum())
            out.append(_row(r.cell, r.target + lp, v,
                            f"the 2023 file's {r.base:,.0f} people (Forbes list left out) times CBO's growth of the "
                            f"same people {r.growth:.5f} (Macro-Projections), plus the year's list {lp:,.0f}"))
    elif src == 'projection returns':
        for g, d in STATE['return_shares'].items():
            v = f"{PREFIX['returns']}{g}"
            lp = float((w[lst] * record_value(df, v)[lst]).sum())
            share = d['filers 2023'] / d['units 2023']
            out.append(_row(group_label(g), share * d['units after step 1'] + lp, v,
                            f"the 2023 file's filing share {share:.5f} ({d['filers 2023']:,.0f} of {d['units 2023']:,.0f} "
                            f"units) times the group's units after step 1, {d['units after step 1']:,.0f}, plus the "
                            f"year's list {lp:,.0f}"))
    elif src == 'projection items':
        mn_rec, mn_usd = int(P['item_min_records']), float(P['item_min_dollars'])
        replaced = fs_replaced_columns(year)
        for col, r in STATE['items'].items():
            if not r['target'] or col in replaced:
                continue   # replaced by the item's filing-season targets by AGI group
            for nm, pre in (('positive', 'item_pos'), ('negative', 'item_neg')):
                if r[f'records {nm}'] < mn_rec or abs(r[f'2023 total {nm}']) < mn_usd:
                    continue
                v = f'{PREFIX[pre]}{col}'
                x = record_value(df, v)
                val = float(w @ x)
                lp = float((w[lst] * x[lst]).sum())
                if r['kind'] == 'total':   # without the list, the start's total must equal its series' growth (step 2)
                    expect = abs(r[f'2023 total {nm}']) * r[f'growth {nm}']
                    assert abs((val - lp) / expect - 1) < 1e-6, \
                        f'{col} {nm}: the start {val - lp:,.0f} is not the 2023 total times its growth {expect:,.0f}'
                label = f"{col}{' (losses)' if nm == 'negative' else ''}"
                out.append(_row(label, val, v, f"the start's total after step 2 ({r['kind']}: {r['source'][:160]}); "
                                               f"the year's list {lp:,.0f}") | {'se': float(r['error']) * val})
    elif src == 'projection filing season':
        out = fs_rows(int(year))
    else:
        raise KeyError(src)
    return out


def write(folder):
    """Writes the tables of step 1, step 2, the return groups and the add factors, and returns the lines for
    summary.md."""
    STATE['person_targets'].to_csv(f'{folder}/projection_step1_cells.csv', index=False)
    items = pd.DataFrame(list(STATE['items'].values()))
    items.to_csv(f'{folder}/projection_step2_items.csv', index=False)
    sh = pd.DataFrame([{'group': group_label(g), **d} for g, d in STATE['return_shares'].items()])
    sh.to_csv(f'{folder}/projection_return_groups.csv', index=False)
    lines = ['## The projection start (projection_steps.py): step 1, people by age and marital status', '',
             pd.Series(STATE['step1']).to_frame('').to_markdown(), '',
             f"Step 2: {len(items)} columns; kinds {items['kind'].value_counts().to_dict()}; targeted "
             f"{int(items['target'].sum())} (projection_step2_items.csv). Items table: {_path(P['items_table'])}", '']
    if STATE['add_factors']:
        af = pd.DataFrame(STATE['add_factors'])
        af['factor to this year'] = (1 + af['percent'] / 100) ** af['years in force']
        af.to_csv(f'{folder}/projection_add_factors.csv', index=False)
        lines += [f"Add factors in force, on top of each item's series ({len(af)} rows; add factors table "
                  f"{_path(P['add_factors_table'])}; projection_add_factors.csv):", '',
                  af.to_markdown(index=False, floatfmt='.5f'), '']
    else:
        lines += [f"Add factors: none in force (add factors table {_path(P['add_factors_table'])}).", '']
    return lines + fs_write(folder) + ig_write(folder)


# ---- The filing-season blend for the first projection year ----
# The growth from the last year of final SOI tables to the first projection year blends two sources: SOI's November
# filing-season tables and the item's national series. Each item has a trust weight on the November tables. Step 2
# gives each column one more national factor, so that its total grows at the blended rate. Step 3 replaces each
# trusted item's national total with targets by AGI group. FEATURES.md, "The first projection year from SOI's November
# filing-season tables", describes the rule.
FS = P['filing_season']
FS_LEVELS = {'full': 1.0, 'mostly trust': 2.0 / 3.0, 'mostly not trust': 1.0 / 3.0, 'none': 0.0}   # trust weights
# The items of the November tables. fs_columns gives the columns of the items table that each one grows.
FS_ITEMS = ['returns', 'wages', 'txbl_int', 'dividends', 'div_pref', 'txbl_ira_dist', 'txbl_pens_dist',
            'capital_gains_line13', 'schedule_e', 'sole_prop', 'farm', 'ui']
# The AGI groups, lower edge included and upper edge not: the standard grid, with no AGI and $1-10,000 merged and
# $1 million and over at the top. Schedule E merges every group below $75,000.
FS_EDGES = [(-np.inf, 10000, 'under $10k'), (10000, 20000, '$10k-20k'), (20000, 30000, '$20k-30k'),
            (30000, 40000, '$30k-40k'), (40000, 50000, '$40k-50k'), (50000, 75000, '$50k-75k'),
            (75000, 100000, '$75k-100k'), (100000, 200000, '$100k-200k'), (200000, 500000, '$200k-500k'),
            (500000, 1000000, '$500k-1m'), (1000000, np.inf, '$1m and over')]
FS_EDGES_E = [(-np.inf, 75000, 'under $75k')] + FS_EDGES[6:]


def fs_active(year, s=None):
    """Whether the blend applies: to a year at or after the filing-season year whose start is the base year's file (s,
    or else this run's step 1 start). After the jump-off year the start is the jump-off year's file, which already
    carries the blend."""
    s = STATE.get('base_year') if s is None else s
    return int(year) >= int(FS['year']) and s is not None and int(s) == int(FS['base_year'])


def jump_off(year, s):
    """The start year of a projection year whose targets.yaml says from_year s. Every year after the jump-off year
    starts from the jump-off year, the last year with IRS data (counting the November tables). Otherwise the start
    year is s."""
    if int(s) == int(FS['base_year']) and int(year) > int(FS['year']):
        return int(FS['year'])
    return int(s)


def fs_e_parts():
    """The parts of Schedule E income, each with its sign."""
    from prep import build_table as BT
    return [(p, BT.PARTS[p][1]) for p in BT.E_PARTS]


def fs_columns(item):
    """The columns of the items table that a November item grows."""
    if item == 'schedule_e':
        return [p for p, _ in fs_e_parts()]
    return {'returns': [], 'dividends': ['div_ord'], 'capital_gains_line13': ['kg_st', 'kg_lt', 'kg_dist']}.get(item, [item])


@lru_cache(maxsize=None)
def fs_trust_table():
    """The trust table (settings filing_season.trust_table), checked: one row per November item, with its trust level
    and the reason for it."""
    p = _path(FS['trust_table'])
    t = pd.read_csv(p, keep_default_na=False)
    assert list(t.columns) == ['item', 'level', 'reason'], f'{p}: columns item, level, reason'
    assert t['item'].is_unique and set(t['item']) == set(FS_ITEMS), \
        f'{p}: one row for each of {FS_ITEMS}; has {t["item"].tolist()}'
    bad = t[~t['level'].isin(FS_LEVELS)]
    assert bad.empty, f'{p}: levels are {list(FS_LEVELS)}; not {bad["level"].tolist()}'
    assert (t['reason'].str.strip() != '').all(), f'{p}: every level needs a reason'
    return t.set_index('item')


def fs_trust():
    """Each item's trust weight on the November tables, as a dict."""
    return {item: FS_LEVELS[level] for item, level in fs_trust_table()['level'].items()}


def fs_edges(item):
    """The AGI groups of an item's targets."""
    return FS_EDGES_E if item == 'schedule_e' else FS_EDGES


def fs_group_of_class(item, lo):
    """The target group in which a class of the November tables falls, given the class's lower edge (None for no
    AGI)."""
    lo_ = -np.inf if (lo is None or (isinstance(lo, float) and np.isnan(lo))) else float(lo)
    hit = [lab for a, b, lab in fs_edges(item) if a <= lo_ < b]
    assert len(hit) == 1, (item, lo)
    return hit[0]


@lru_cache(maxsize=None)
def fs_tables():
    """The November tables (settings filing_season.tables), in the long layout."""
    t = pd.read_csv(_path(FS['tables']), keep_default_na=False)
    return t[t['layout'] == 'long']


@lru_cache(maxsize=None)
def fs_november(item, stat):
    """An item's levels in the November tables of the base year and of the filing-season year, by target group and
    for 'all', as a dict of group: [base-year level, year level]."""
    t = fs_tables()
    stat_ = 'returns' if stat in ('count', 'units') else 'amount'
    out = {}
    for y in (int(FS['base_year']), int(FS['year'])):
        s = t[(t.tax_year == y) & (t['item'] == item) & (t.statistic == stat_)]
        assert len(s) == 13, f'November tables {y}: {item} {stat_} has {len(s)} rows, not 13 (the total and 12 classes)'
        for r in s.itertuples():
            g = 'all' if r.agi_class == 'all' else fs_group_of_class(item, None if r.agi_lo == '' else float(r.agi_lo))
            out.setdefault(g, [0.0, 0.0])[y != int(FS['base_year'])] += float(r.value)
    assert all(v[0] != 0 for v in out.values()), f'{item} {stat}: a group with nothing in {FS["base_year"]}'
    return out


@lru_cache(maxsize=None)
def fs_errors():
    """The table of the November items' errors from the backtest (settings filing_season.errors_table)."""
    return pd.read_csv(_path(FS['errors_table']))


def fs_error(item, stat, group):
    """The error of an item's target in one group, as (kind, number): ('relative', the relative error, at least
    min_error) or ('dollars', a share of the item's national total). A group with no error from the backtest stops the
    run."""
    e = fs_errors()
    s = e[(e['item'] == item) & (e.statistic == ('returns' if stat in ('count', 'units') else 'amount')) &
          (e['group'] == group)]
    assert len(s) == 1, f'{_path(FS["errors_table"])}: no error for {item} {stat} {group}'
    r = s.iloc[0]
    if r.error_kind == 'dollars':
        assert np.isfinite(r.error_share_of_national) and r.error_share_of_national > 0, (item, group)
        return 'dollars', float(r.error_share_of_national)
    assert np.isfinite(r.error) and r.error >= 0, (item, group)
    return 'relative', max(float(r.error), float(FS['min_error']))


def fs_value(df, item):
    """Each record's value of a filing-season item, in the frame's dollars."""
    if item == 'returns':
        return np.ones(len(df))
    if item == 'capital_gains_line13' and item not in df:
        x = df['capital_gains'].to_numpy(float) + df['kg_dist'].to_numpy(float)
    elif item == 'schedule_e':
        x = sum(sign * df[p].to_numpy(float) for p, sign in fs_e_parts())
    else:
        x = df[item].to_numpy(float)
    assert np.isfinite(x).all(), f'{item}: {int((~np.isfinite(x)).sum())} missing values'
    return x


def fs_series_growth(col):
    """The growth of the column's own series from the base year to the filing-season year. Its add factor over those
    years must be 1."""
    tab = items_table()
    assert col in tab.index and tab.loc[col, 'series'].startswith('file:'), f'{col}: a file series in the items table'
    s, t = int(FS['base_year']), int(FS['year'])
    g, _ = series_growth(tab.loc[col, 'series'], s, t)
    A, _ = add_factor(col, s, t)
    assert A == 1.0, f'{col}: an add factor in {t}; the filing-season blend assumes none'
    return g


def fs_base_total(x):
    """The national total of the record values x on the base year's file (all units, Forbes list left out)."""
    k, w0 = STATE['keep'], STATE['w_base']
    return float(w0[k] @ x[k])


def fs_blend(item):
    """The item's national growth from the base year to the filing-season year, as a dict: in the November tables
    ('november'), by CBO's series ('cbo'), and blended with the trust weight w ('blend' = w x november + (1 - w) x
    cbo)."""
    b = STATE['fs']['blend']
    if item not in b:
        w = fs_trust()[item]
        nov = fs_november(item, 'amount' if item != 'returns' else 'units')['all']
        N = nov[1] / nov[0]
        if item == 'returns':
            C = N                                    # returns set shares only; the CBO part would not change them
        elif item == 'dividends':
            C = fs_series_growth('div_ord')          # both parts of dividends grow by CBO's qualified dividend series
            assert abs(C / fs_series_growth('div_pref') - 1) < 1e-12, 'dividends: the two parts have different series'
        elif item == 'capital_gains_line13':
            C, _ = series_growth(FS['series']['capital_gains_line13'], int(FS['base_year']), int(FS['year']))
        elif item == 'schedule_e':
            df = STATE['df_base']
            tot = [(sign * fs_base_total(df[p].to_numpy(float)), fs_series_growth(p)) for p, sign in fs_e_parts()]
            C = sum(v * g for v, g in tot) / sum(v for v, _ in tot)
        else:
            C = fs_series_growth(fs_columns(item)[0])
        b[item] = {'item': item, 'level': fs_trust_table().loc[item, 'level'], 'w': w, 'november': N, 'cbo': C,
                   'blend': w * N + (1 - w) * C}
    return b[item]


def fs_column_factor(col, t):
    """The filing-season blend's factor on column col in year t, with a description, or None when the blend does not
    apply or the item has no trust."""
    if not fs_active(t):
        return None
    cols = STATE['fs']['columns']
    if col not in cols:
        cols[col] = None
        for item in FS_ITEMS:
            if col not in fs_columns(item) or fs_trust()[item] == 0:
                continue
            bl = fs_blend(item)
            g = fs_series_growth(col)
            if item == 'dividends':
                df = STATE['df_base']
                q, o = fs_base_total(df['div_pref'].to_numpy(float)), fs_base_total(df['div_ord'].to_numpy(float))
                gq = fs_blend('div_pref')['blend'] if fs_trust()['div_pref'] > 0 else fs_series_growth('div_pref')
                g_ord = (bl['blend'] * (q + o) - gq * q) / o
                cols[col] = (g_ord / g, f"ordinary dividends' blend {bl['blend']:.5f} given qualified dividends' "
                                        f"{gq:.5f}: the non-qualified part grows {g_ord:.5f}, its series {g:.5f}")
            elif item == 'capital_gains_line13':
                cols[col] = (bl['blend'] / bl['cbo'], f"Form 1040 net gain or loss: blend {bl['blend']:.5f} over CBO's "
                                                      f"capital gains line {bl['cbo']:.5f}")
            elif item == 'schedule_e':
                cols[col] = (bl['blend'] / g, f"Schedule E's common factor {bl['blend']:.5f} (blend; CBO's rules now "
                                              f"{bl['cbo']:.5f} on the signed sum) over the part's series {g:.5f}")
            else:
                cols[col] = (bl['blend'] / g, f"blend {bl['blend']:.5f} (w {bl['w']:.3f}, November {bl['november']:.5f}, "
                                              f"CBO {bl['cbo']:.5f}) over the series {g:.5f}")
            assert np.isfinite(cols[col][0]) and cols[col][0] > 0, (col, cols[col])
    return cols[col]


def fs_replaced_columns(year):
    """The columns whose national total target is replaced by targets by AGI group (in the filing-season year
    only)."""
    if not (fs_active(year) and int(year) == int(FS['year'])):
        return set()
    return {c for item in FS_ITEMS if fs_trust()[item] > 0 for c in fs_columns(item)}


def _fs_row(out, item, stat, lab, lo, hi, value, se, source):
    """Adds to out the target row of one item and statistic in one AGI group, with its value, standard error and
    source."""
    v = f"{PREFIX['filing_season']}{item}"
    out.append({'cell': lab, 'statistic': stat, 'value': float(value), 'variable': v, 'source': source,
                'se': float(se), 'agi': (lo, hi),
                'cell_spec': {'parts': {}, 'filters': [], 'label': lab, 'agi': None, 'line': None}})


def fs_rows(year):
    """The target rows of the outside source 'projection filing season', in the filing-season year only. For each
    trusted item, its blended total by AGI group among filers, and for fully trusted items also the number of returns
    with it."""
    if not (fs_active(year) and int(year) == int(FS['year'])):
        return []
    from shared import dollars as D
    df, w, lst = _start()
    b = STATE['df_base']
    k, w0 = STATE['keep'], STATE['w_base']
    fb, fs_ = b['filer'].to_numpy() == 1, df['filer'].to_numpy() == 1
    a0, a1 = D.agi(b), D.agi(df)
    out, table = [], []

    # 1. The number of returns. Its total is set by the exact targets of the return groups, so here the blend only
    #    shares that total across AGI groups.
    T = sum(d['filers 2023'] / d['units 2023'] * d['units after step 1'] for d in STATE['return_shares'].values())
    bl = fs_blend('returns')
    nov = fs_november('returns', 'units')
    base = {lab: float(w0[k & fb & (a0 >= lo) & (a0 < hi)].sum()) for lo, hi, lab in FS_EDGES}
    grown = {lab: base[lab] * (bl['w'] * nov[lab][1] / nov[lab][0] + (1 - bl['w']) * bl['cbo']) for lab in base}
    for lo, hi, lab in FS_EDGES:
        share = grown[lab] / sum(grown.values())
        lp = float(w[lst & fs_ & (a1 >= lo) & (a1 < hi)].sum())
        kind, e = fs_error('returns', 'units', lab)
        val = share * T + lp
        _fs_row(out, 'returns', 'units', lab, lo, hi, val, e * val,
                f"share {share:.5f} of the {T:,.0f} returns the exact returns cells set: the {FS['base_year']} file's "
                f"{base[lab]:,.0f} returns in the group times the November growth {nov[lab][1] / nov[lab][0]:.5f}, as a "
                f"share; plus the year's list {lp:,.0f}")
        table.append({'item': 'returns', 'statistic': 'units', 'group': lab, 'base': base[lab],
                      'november growth': nov[lab][1] / nov[lab][0], 'growth': share * T / base[lab] if base[lab] else np.nan,
                      'value': val, 'se': e * val})
    # 2. Each trusted item's amount, and for fully trusted items the count, in each AGI group: the base file's value
    #    times the blended growth, plus the year's Forbes list.
    for item in FS_ITEMS:
        if item == 'returns' or fs_trust()[item] == 0:
            continue
        bl = fs_blend(item)
        x0, x1 = fs_value(b, item), fs_value(df, item)
        stats = [('amount', x0, x1)] + ([('count', (x0 != 0).astype(float), (x1 != 0).astype(float))]
                                        if bl['w'] == 1.0 else [])
        for stat, v0, v1 in stats:
            nov = fs_november(item, stat)
            nat = abs(float(w0[k & fb] @ v0[k & fb]) * bl['blend'])
            for lo, hi, lab in fs_edges(item):
                m0 = k & fb & (a0 >= lo) & (a0 < hi)
                base_g = float(w0[m0] @ v0[m0])
                ng = nov[lab][1] / nov[lab][0]
                g = bl['w'] * ng + (1 - bl['w']) * (bl['cbo'] if stat == 'amount' else 1.0)
                lp_m = lst & fs_ & (a1 >= lo) & (a1 < hi)
                lp = float(w[lp_m] @ v1[lp_m])
                val = base_g * g + lp
                kind, e = fs_error(item, stat, lab)
                se = e * nat if kind == 'dollars' else e * abs(val)
                _fs_row(out, item, stat, lab, lo, hi, val, se,
                        f"the {FS['base_year']} file's {base_g:,.0f} in the group (filers, Forbes list left out) times "
                        f"{g:.5f} = {bl['w']:.3f} x November {ng:.5f} + {1 - bl['w']:.3f} x CBO's national "
                        f"{bl['cbo'] if stat == 'amount' else 1.0:.5f}; plus the year's list {lp:,.0f}; error "
                        f"{'%.4f of the national total' % e if kind == 'dollars' else '%.4f' % e}")
                table.append({'item': item, 'statistic': stat, 'group': lab, 'base': base_g, 'november growth': ng,
                              'growth': g, 'value': val, 'se': se})
    STATE['fs']['targets'] = table
    return out


def fs_write(folder):
    """Writes projection_filing_season.csv (the blends), projection_filing_season_columns.csv (the step 2 factors)
    and projection_filing_season_targets.csv (the targets by AGI group), and returns the blend's lines for
    summary.md."""
    if not fs_active(STATE.get('year', 0)):
        if int(STATE.get('base_year', 0)) == int(FS['year']):
            how = (f"the start is the jump-off year's file ({FS['year']}), which carries the blend; every item grows "
                   f"from it by its series")
        else:
            how = 'not used before ' + str(FS['year'])
        return [f"Filing-season blend (settings tilt.projection.filing_season): {how}.", '']
    bl = pd.DataFrame(list(STATE['fs']['blend'].values()))
    cols = pd.DataFrame([{'column': c, 'factor': v[0], 'how': v[1]} for c, v in STATE['fs']['columns'].items()
                         if v is not None])
    bl.to_csv(f'{folder}/projection_filing_season.csv', index=False)
    cols.to_csv(f'{folder}/projection_filing_season_columns.csv', index=False)
    lines = [f"## The filing-season blend (November tables {FS['base_year']} and {FS['year']}; trust table "
             f"{_path(FS['trust_table'])})", '', bl.to_markdown(index=False, floatfmt='.5f'), '',
             f"Step 2 factors on {len(cols)} columns (projection_filing_season_columns.csv).", '']
    if 'targets' in STATE['fs']:
        tg = pd.DataFrame(STATE['fs']['targets'])
        tg.to_csv(f'{folder}/projection_filing_season_targets.csv', index=False)
        lines += [f"{len(tg)} targets by AGI group (projection_filing_season_targets.csv), replacing the national totals "
                  f"of {sorted(fs_replaced_columns(STATE['year']))}.", '']
    return lines


# ---- Taxable interest and the rate-like part of non-qualified dividends ----
# Taxable interest on returns moves with short-term interest rates: it tripled from 2022 to 2024 as short rates rose,
# and the start file already reflects the high rates. For a start at or after the jump-off year it grows with nominal
# GDP times the 3-month Treasury bill rate. Non-qualified dividends are split on the start file into two parts. The
# stock-like part keeps the column's series; the rate-like part grows as taxable interest does. The column grows by the
# blend of the two, weighted by their shares. FEATURES.md, "Taxable interest after the jump-off year", describes the
# rule.
IG = P['interest_growth']
IG_COLUMNS = ('txbl_int', 'div_ord')


def ig_active(s):
    """Whether the rule applies: to a year whose start year s is at or after from_year. from_year must be after the
    filing-season base year, so that the rule and the blend never apply to the same year."""
    assert int(IG['from_year']) > int(FS['base_year']), \
        'tilt.projection.interest_growth.from_year must be after the filing-season base year'
    return int(s) >= int(IG['from_year'])


def ig_split(df, w, s):
    """The two parts of the start file's non-qualified dividends. The stock-like part is SOI's non-qualified dividends
    in the split year, grown with qualified dividends to the start file's filers. The rate-like part is the rest. df
    and w are the start file and its own weights, before step 1."""
    y0 = int(IG['split_year'])
    assert y0 < int(s), f'interest_growth.split_year {y0} must be before the start year {s}'
    ordinary0, q0 = TG.soi_get(y0, 'ordinary dividends', 'amount'), TG.soi_get(y0, 'qualified dividends', 'amount')
    d0 = ordinary0 - q0
    fil = df['filer'].to_numpy() == 1
    q1 = float(w[fil] @ df['div_pref'].to_numpy(float)[fil])
    d1 = float(w[fil] @ df['div_ord'].to_numpy(float)[fil])
    d1_all = float(w @ df['div_ord'].to_numpy(float))
    stock = d0 * q1 / q0
    share = stock / d1
    assert 0 < share < 1, f'interest rule: stock-like share {share:.4f} of non-qualified dividends is not between 0 and 1'
    out = {'split year': y0, 'start year': int(s),
           f'SOI {y0} ordinary dividends': ordinary0, f'SOI {y0} qualified dividends': q0,
           f'SOI {y0} non-qualified dividends': d0,
           f'{s} file qualified dividends, filers': q1, f'{s} file non-qualified dividends, filers': d1,
           f'{s} file non-qualified dividends, all units': d1_all,
           f'growth of qualified dividends {y0}-{s}': q1 / q0,
           f'{s} stock-like part': stock, f'{s} rate-like part': d1 - stock, 'stock-like share': share}
    common.say(f"interest rule: non-qualified dividends {s}, ${d1 / 1e9:,.1f} billion on filers: "
               f"stock-like ${stock / 1e9:,.1f} billion (SOI {y0} ${d0 / 1e9:,.1f} billion times qualified dividends' "
               f"growth {q1 / q0:.5f}), share {share:.5f}; rate-like ${(d1 - stock) / 1e9:,.1f} billion")
    return out


def ig_growth(col, s, t):
    """A column's growth from year s to year t under the rule, and a description."""
    gr, what_r = series_growth(IG['rate_series'], s, t)
    if col == 'txbl_int':
        return gr, f'interest rule: {what_r} (nominal GDP times the 3-month bill rate)'
    assert col == 'div_ord', col
    sh = STATE['interest']['stock-like share']
    gq, what_q = series_growth(items_table().loc[col, 'series'], s, t)
    g = sh * gq + (1 - sh) * gr
    STATE['interest'].update({f'growth of the stock-like part {s}-{t}': gq, f'growth of the rate-like part {s}-{t}': gr,
                              f'growth of non-qualified dividends {s}-{t}': g})
    return g, (f'interest rule: {sh:.5f} x {what_q} ({gq:.5f}) + {1 - sh:.5f} x {what_r} ({gr:.5f}), the stock-like '
               f'and rate-like parts')


def ig_write(folder):
    """Writes projection_interest.csv and returns the rule's lines for summary.md."""
    if not ig_active(STATE.get('base_year', 0)):
        how = f"not used for a start before {IG['from_year']}"
        return [f'Interest rule (settings tilt.projection.interest_growth): {how}.', '']
    r = STATE['interest']
    pd.Series(r).to_frame('value').to_csv(f'{folder}/projection_interest.csv')
    return ["## Taxable interest and non-qualified dividends (settings tilt.projection.interest_growth)", '',
            f"Taxable interest grows from {r['start year']} by {IG['rate_series']} (nominal GDP times the 3-month "
            f"Treasury bill rate). Non-qualified dividends grow by the blend of their items-table series (the "
            f"stock-like part) and that series (the rate-like part), weighted by the start file's split "
            f"(projection_interest.csv):", '',
            pd.Series(r).to_frame('value').to_markdown(floatfmt=',.5f'), '']
