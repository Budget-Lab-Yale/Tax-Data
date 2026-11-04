"""The projection years (2024-2035) as John designed them on 3 November 2026 (scratch
explore/2026-11-02/projection_design/DESIGN.md, "Agreed on 3 November"). Every projection year starts from the 2023 file
and the years are independent of one another. Three steps, switched on by a year's targets.yaml with
start_scaling {weights: persons, dollars: item_factor}:

1. A calibration to exact person counts (step1). The 2023 weights are moved, as little as possible (the smallest
   Kullback-Leibler move, solver.solve with every count an identity), so that the people in the file match exact counts
   by marital status and single year of age (0 to 79, then 80 and over). Every member of a unit carries the unit's
   weight: the primary filer and, on a joint return, the spouse (married), the primary of any other unit (unmarried),
   and every dependent with an age (unmarried). A dependent filer's own record counts only its dependents: the filer
   himself is counted on the return that claims him. A married person under 18 is counted unmarried, in the file and in
   CBO's population (whose married and unmarried counts at those ages are added; John, 4 November 2026: CBO's
   population has no married person under 16, and none aged 16 in 2070-2072, which made that cell unreachable). Each
   count is the 2023 file's own count times CBO's growth of the same people
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

Add factors (John, 3 November; add_factors_table, add_factor). A second table (settings
tilt.projection.add_factors_table) adds extra growth, in percent a year, on top of an item's series: one row per item
and range of years (column, first_year, last_year, percent, note). The item's growth from the base year s to year t is
its series' growth times the product of (1 + percent / 100) over the years s + 1 to t that the item's rows cover. It
enters step 2 as part of the series' growth, so the step 2 factor, the item's total target in step 3 and the SCF
targets' growth (scf_factor) all include it. A same_as item takes the other column's factors, add factors included,
times its own add factors. The table may be empty (a header only): every item's add factor is then exactly 1 and the
year is what it was without the table. Every row with a non-zero percent needs a note saying who set it, when and why;
the year's report lists the rows in force.
"""
import os

import numpy as np
import pandas as pd

import common

HERE = os.path.dirname(os.path.abspath(__file__))
RESOURCES = os.path.join(os.path.dirname(HERE), 'resources')
P = common.SETTINGS['tilt'].get('projection', {})
TOP_AGE = 80          # ages 80 and over are one cell (the file caps the primary's and spouse's ages at 80)
MARRIED_FROM = 18     # married people under 18 counted unmarried, in the file and in CBO's population (John, 4 Nov 2026)
ITEMS_COLUMNS = ['column', 'series', 'kind', 'target', 'error', 'note']
ADD_COLUMNS = ['column', 'first_year', 'last_year', 'percent', 'note']
PREFIX = {'persons': 'pp__', 'returns': 'pr__', 'item_pos': 'pi+__', 'item_neg': 'pi-__', 'filing_season': 'fs__'}

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
    ages = range(age, 101) if age >= TOP_AGE else [age]
    assert not (married and age < MARRIED_FROM), f'no married cell under {MARRIED_FROM} (age {age})'
    pres = ['unmarried_', 'married_'] if age < MARRIED_FROM else ['married_' if married else 'unmarried_']
    v = t.loc[int(year), [f'{pre}{a}' for pre in pres for a in ages]].to_numpy(float)
    assert np.isfinite(v).all() and (v >= 0).all(), f'population {year} {pres} {age}: missing or negative'
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
                 w1=w1, person_targets=table, step1=out, return_shares=shares, items={}, add_factors=[])
    if fs_active(year):   # the November filing-season tables (below): the 2023 file's own items by its AGI groups
        assert int(s) == int(FS['base_year']), f'filing_season: the start is {s}, the setting says {FS["base_year"]}'
        STATE['df_base'] = df
        STATE['fs'] = {'blend': {}, 'columns': {}}
    return w1, out


# ---- step 2 ---------------------------------------------------------------------------------------------------
_TABLE = {}
_ADD = {}


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


def add_factors_table():
    """The add factors table (settings tilt.projection.add_factors_table, relative to the pipeline folder or absolute;
    see the module docstring). Every column must be in the items table, every year range sensible, no item's years
    covered twice, and every non-zero percent must carry a note."""
    if 't' not in _ADD:
        p = P['add_factors_table']
        p = p if os.path.isabs(p) else os.path.join(HERE, p)
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
        assert not missing, f'{p}: columns not in the items table {_TABLE["path"]}: {missing}'
        for col, g in t.groupby('column'):
            years = [y for r in g.itertuples() for y in range(r.first_year, r.last_year + 1)]
            assert len(years) == len(set(years)), f'{p}: {col} has a year in two rows'
        _ADD['t'] = t
        _ADD['path'] = p
    return _ADD['t']


def add_factor(col, s, t):
    """Column col's add factor from base year s to year t: the product of (1 + percent / 100) over the years s + 1 to
    t covered by its rows of the add factors table (exactly 1 with none). Returns (factor, rows in force)."""
    tab = add_factors_table()
    rows = tab[tab.column == col]
    early = rows[rows.first_year <= s]
    assert early.empty, f'{_ADD["path"]}: {col} has add factors for years up to the base year {s}'
    f, used = 1.0, []
    for r in rows.itertuples():
        n = len(range(max(r.first_year, s + 1), min(r.last_year, t) + 1))
        if n > 0 and r.percent != 0:
            f *= (1 + r.percent / 100) ** n
            used.append({'column': col, 'first_year': r.first_year, 'last_year': r.last_year, 'percent': r.percent,
                         'years in force': n, 'note': r.note})
    return f, used


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
    fsm = fs_column_factor(col, int(t))
    if fsm is not None:   # (4 November, John) the November table's blend for 2024, carried to every later year
        assert kind == 'total' and series.startswith('file:'), f'{col}: the filing-season blend needs a file series of kind total'
        G = (G[0] * fsm[0], G[1] * fsm[0])
        what = f'{what}; times the filing-season blend {fsm[0]:.5f} ({fsm[1]})'
    A, used = add_factor(col, s, t)
    if used:   # the add factors on top of the series (module docstring); with none, G is left exactly as it was
        G = (G[0] * A, G[1] * A)
        spans = ', '.join('%+g%% a year %d-%d' % (u['percent'], u['first_year'], u['last_year']) for u in used)
        what = f'{what}; times add factors {A:.5f} ({spans})'
        STATE['add_factors'] += used
    keep, w0, w1 = STATE['keep'], STATE['w_base'], STATE['w1']
    rec = {'column': col, 'series': series, 'kind': kind, 'source': what, 'target': int(row['target']),
           'error': row['error'], 'add factor': A, 'filing season factor': 1.0 if fsm is None else fsm[0],
           'growth positive': G[0], 'growth negative': G[1]}
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


def check_add_factors_applied(t):
    """After step 2: every item with a non-zero add factor in force by year t was grown by the start (else the table
    names a column the file does not carry, and its add factor would be silently lost)."""
    tab = add_factors_table()
    live = tab[(tab.percent != 0) & (tab.first_year <= t)]
    lost = sorted(set(live.column) - set(STATE['items']))
    assert not lost, f'{_ADD["path"]}: add factors in force for {t} on columns step 2 did not grow: {lost}'


def same_as_add_factor(col, s, t):
    """A same_as column's own add factor (on top of the other column's factors, which carry the other's add factors).
    Returns the factor; records the rows in force."""
    A, used = add_factor(col, s, t)
    STATE['add_factors'] += used
    return A


def record_same_as(col, other, x, A=1.0):
    """Records a same_as column: the other column's factors record by record, times the column's own add factor A
    (same_as_add_factor; exactly 1 with none)."""
    r = dict(STATE['items'][other])
    keep, w0 = STATE['keep'], STATE['w_base']
    # the other column's factors record by record: on this column's records that is a growth per record (kind
    # per_unit), whatever the other column's kind
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
    if kind == 'filing_season':
        return fs_value(df, key)
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
        replaced = fs_replaced_columns(year)
        for col, r in STATE['items'].items():
            if not r['target'] or col in replaced:
                continue   # (4 November) replaced: the item's targets by AGI group (projection filing season)
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
    elif src == 'projection filing season':
        out = fs_rows(int(year))
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
    add_factors_table()
    if STATE['add_factors']:
        af = pd.DataFrame(STATE['add_factors'])
        af['factor to this year'] = (1 + af['percent'] / 100) ** af['years in force']
        af.to_csv(f'{folder}/projection_add_factors.csv', index=False)
        lines += [f"Add factors in force, on top of each item's series ({len(af)} rows; add factors table "
                  f"{_ADD['path']}; projection_add_factors.csv):", '',
                  af.to_markdown(index=False, floatfmt='.5f'), '']
    else:
        lines += [f"Add factors: none in force (add factors table {_ADD['path']}).", '']
    lines += fs_write(folder)
    return lines


# ---- the November filing-season tables for the first projection year (John, 4 November 2026) -----------------------
# Scratch explore/2026-11-04/nowcast/PLAN.md, with "Decisions after the backtest"; the backtest is BACKTEST.md there.
# Settings tilt.projection.filing_season: enabled; base_year (the last year of final SOI tables, 2023); year (the
# first projection year, 2024, whose growth comes from SOI's November tables for base_year and year); tables (the
# November tables as read, build_filing_season.py); trust_table and errors_table (targets_config); min_error; series
# (CBO's national series for items with no items-table column of their own: capital gains).
#   1. Trust. Each item has one of four levels in the trust table: full (weight 1), mostly trust (2/3), mostly not
#      trust (1/3), none (0).
#   2. The blend. Item i's growth from base_year to year in AGI group g is w x (the November tables' growth in g) +
#      (1 - w) x (CBO's national growth, the item's series now). An item with no trust is exactly today's rule.
#   3. Step 2 stays national: each of the item's columns gets one more factor, so that the start's national total
#      grows from base_year to year at the blended national growth (w x November + (1 - w) x CBO, nationally); the
#      factor multiplies the column's series in every later year too, so year t's growth is the blended growth to
#      year times the series' growth from year to t. Ordinary dividends: the non-qualified part (div_ord) gets the
#      factor that makes ordinary dividends (div_pref + div_ord) grow at their blend, given qualified dividends'.
#      Capital gains: the pieces before the loss limit (kg_st, kg_lt, kg_dist) by the ratio of the blended growth of
#      Form 1040's net gain or loss to CBO's capital gains line. Schedule E: every part by one common factor, so the
#      signed sum grows at its blend.
#   4. Step 3, in year only: each item's blended total in each AGI group of the year (and, for items of full trust,
#      the number of returns with the item in each group) on filers, replacing the item's national total target; the
#      number of returns as shares across the groups, their total set by the exact returns cells. Each value is the
#      base_year file's own total in the group (Forbes list left out) times the blended growth, plus the year's list.
#      Errors from the backtest (errors_table), at least min_error; Schedule E's lowest group in dollars.
#   5. Groups: the standard AGI grid with no AGI and $1-10,000 as one group, under $10,000, and $1 million and over as
#      the top group; for Schedule E every group below $75,000 is one group.
FS = P.get('filing_season', {})
FS_LEVELS = {'full': 1.0, 'mostly trust': 2.0 / 3.0, 'mostly not trust': 1.0 / 3.0, 'none': 0.0}
# item -> the items-table columns it grows (returns: none, its targets are shares)
FS_ITEMS = ['returns', 'wages', 'txbl_int', 'dividends', 'div_pref', 'txbl_ira_dist', 'txbl_pens_dist',
            'capital_gains_line13', 'schedule_e', 'sole_prop', 'farm', 'ui']
FS_EDGES = [(None, 10000, 'under $10k'), (10000, 20000, '$10k-20k'), (20000, 30000, '$20k-30k'),
            (30000, 40000, '$30k-40k'), (40000, 50000, '$40k-50k'), (50000, 75000, '$50k-75k'),
            (75000, 100000, '$75k-100k'), (100000, 200000, '$100k-200k'), (200000, 500000, '$200k-500k'),
            (500000, 1000000, '$500k-1m'), (1000000, None, '$1m and over')]
FS_EDGES_E = [(None, 75000, 'under $75k')] + FS_EDGES[6:]
_FS = {}


def fs_active(year):
    return bool(FS.get('enabled', False)) and int(year) >= int(FS['year'])


def fs_e_parts():
    import build_table as BT
    return [(p, BT.PARTS[p][1]) for p in BT.E_PARTS]


def fs_columns(item):
    if item == 'schedule_e':
        return [p for p, _ in fs_e_parts()]
    return {'returns': [], 'dividends': ['div_ord'], 'capital_gains_line13': ['kg_st', 'kg_lt', 'kg_dist']}.get(item, [item])


def fs_trust():
    """{item: weight} from the trust table (settings filing_season.trust_table, relative to the pipeline folder)."""
    if 'trust' not in _FS:
        p = FS['trust_table']
        p = p if os.path.isabs(p) else os.path.join(HERE, p)
        t = pd.read_csv(p, keep_default_na=False)
        assert list(t.columns) == ['item', 'level', 'reason'], f'{p}: columns item, level, reason'
        assert t['item'].is_unique and set(t['item']) == set(FS_ITEMS), \
            f'{p}: one row for each of {FS_ITEMS}; has {t["item"].tolist()}'
        bad = t[~t['level'].isin(FS_LEVELS)]
        assert bad.empty, f'{p}: levels are {list(FS_LEVELS)}; not {bad["level"].tolist()}'
        assert (t['reason'].str.strip() != '').all(), f'{p}: every level needs a reason'
        _FS['trust'] = {r.item: FS_LEVELS[r.level] for r in t.itertuples()}
        _FS['trust_levels'] = dict(zip(t['item'], t['level']))
        _FS['trust_path'] = p
    return _FS['trust']


def fs_edges(item):
    return FS_EDGES_E if item == 'schedule_e' else FS_EDGES


def fs_group_of_class(item, lo):
    """The target group a class of the November tables (its lower edge; None for no AGI) falls in."""
    lo_ = -np.inf if (lo is None or (isinstance(lo, float) and np.isnan(lo))) else float(lo)
    hit = [lab for a, b, lab in fs_edges(item) if (-np.inf if a is None else a) <= lo_ < (np.inf if b is None else b)]
    assert len(hit) == 1, (item, lo)
    return hit[0]


def fs_november(item, stat):
    """{group: (level base_year, level year)} and 'all' of an item in the November tables (the long layout)."""
    key = (item, stat)
    if key not in _FS:
        if 'tables' not in _FS:
            p = FS['tables']
            p = p if os.path.isabs(p) else os.path.join(HERE, p)
            t = pd.read_csv(p, keep_default_na=False)
            t = t[t['layout'] == 'long']
            _FS['tables'], _FS['tables_path'] = t, p
        t = _FS['tables']
        stat_ = 'returns' if stat in ('count', 'units') else 'amount'
        out = {}
        for y in (int(FS['base_year']), int(FS['year'])):
            s = t[(t.tax_year == y) & (t['item'] == item) & (t.statistic == stat_)]
            assert len(s) == 13, f'November tables {y}: {item} {stat_} has {len(s)} rows, not 13 (the total and 12 classes)'
            for r in s.itertuples():
                g = 'all' if r.agi_class == 'all' else fs_group_of_class(item, None if r.agi_lo == '' else float(r.agi_lo))
                out.setdefault(g, [0.0, 0.0])[y != int(FS['base_year'])] += float(r.value)
        assert all(v[0] != 0 for v in out.values()), f'{item} {stat}: a group with nothing in {FS["base_year"]}'
        _FS[key] = out
    return _FS[key]


def fs_errors():
    if 'errors' not in _FS:
        p = FS['errors_table']
        p = p if os.path.isabs(p) else os.path.join(HERE, p)
        e = pd.read_csv(p)
        _FS['errors'], _FS['errors_path'] = e, p
    return _FS['errors']


def fs_error(item, stat, group):
    """(kind, number): ('relative', the relative error, at least min_error) or ('dollars', a share of the item's
    national total). A group with no backtest error stops the run."""
    e = fs_errors()
    s = e[(e['item'] == item) & (e.statistic == ('returns' if stat in ('count', 'units') else 'amount')) &
          (e['group'] == group)]
    assert len(s) == 1, f'{_FS["errors_path"]}: no error for {item} {stat} {group}'
    r = s.iloc[0]
    if r.error_kind == 'dollars':
        assert np.isfinite(r.error_share_of_national) and r.error_share_of_national > 0, (item, group)
        return 'dollars', float(r.error_share_of_national)
    assert np.isfinite(r.error) and r.error >= 0, (item, group)
    return 'relative', max(float(r.error), float(FS['min_error']))


def fs_value(df, item):
    """A record's value of a filing-season item (in the frame's dollars)."""
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
    """The column's own series' growth from base_year to year, and its add factor over those years (must be 1)."""
    tab = items_table()
    assert col in tab.index and tab.loc[col, 'series'].startswith('file:'), f'{col}: a file series in the items table'
    s, t = int(FS['base_year']), int(FS['year'])
    g, _ = series_growth(tab.loc[col, 'series'], s, t)
    A, _ = add_factor(col, s, t)
    assert A == 1.0, f'{col}: an add factor in {t}; the filing-season blend assumes none'
    return g


def fs_base_total(x):
    """The base_year file's national total of record values x (all units, Forbes list left out)."""
    k, w0 = STATE['keep'], STATE['w_base']
    return float(w0[k] @ x[k])


def fs_blend(item):
    """{'w', 'november', 'cbo', 'blend'}: the item's national growth from base_year to year in the November tables,
    CBO's (the item's series now) and the blend."""
    b = STATE['fs']['blend']
    if item not in b:
        w = fs_trust()[item]
        nov = fs_november(item, 'amount' if item != 'returns' else 'units')['all']
        N = nov[1] / nov[0]
        if item == 'returns':
            C = N                                    # shares only: the CBO part leaves every group's share unchanged
        elif item == 'dividends':
            C = fs_series_growth('div_ord')          # both parts of dividends grow by CBO's qualified dividends now
            assert abs(C / fs_series_growth('div_pref') - 1) < 1e-12, 'dividends: the two parts have different series'
        elif item == 'capital_gains_line13':
            C, _ = series_growth(FS['series']['capital_gains_line13'], int(FS['base_year']), int(FS['year']))
        elif item == 'schedule_e':
            df = STATE['df_base']
            tot = [(sign * fs_base_total(df[p].to_numpy(float)), fs_series_growth(p)) for p, sign in fs_e_parts()]
            C = sum(v * g for v, g in tot) / sum(v for v, _ in tot)
        else:
            C = fs_series_growth(fs_columns(item)[0])
        b[item] = {'item': item, 'level': _FS['trust_levels'][item], 'w': w, 'november': N, 'cbo': C,
                   'blend': w * N + (1 - w) * C}
    return b[item]


def fs_column_factor(col, t):
    """(factor, description) of the filing-season blend on column col in year t, or None (not active, or an item with
    no trust: exactly today's rule)."""
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
    """The columns whose national total target the group targets replace (in the filing-season year only)."""
    if not (fs_active(year) and int(year) == int(FS['year'])):
        return set()
    return {c for item in FS_ITEMS if fs_trust()[item] > 0 for c in fs_columns(item)}


def fs_rows(year):
    """The rows of outside source 'projection filing season' (module section above, point 4); none unless active in
    the filing-season year."""
    if not (fs_active(year) and int(year) == int(FS['year'])):
        return []
    import dollars as D
    df, w, fixed = _start()
    lst = df['billionaire'].to_numpy() == 1 if 'billionaire' in df else np.zeros(len(df), bool)
    b = STATE['df_base']
    k, w0 = STATE['keep'], STATE['w_base']
    fb, fs_ = b['filer'].to_numpy() == 1, df['filer'].to_numpy() == 1
    a0, a1 = D.agi(b), D.agi(df)
    out, table = [], []

    def row(item, stat, lab, lo, hi, value, se, source):
        v = f"{PREFIX['filing_season']}{item}"
        out.append({'cell': lab, 'statistic': stat, 'value': float(value), 'variable': v, 'source': source,
                    'se': float(se), 'agi': (-np.inf if lo is None else lo, np.inf if hi is None else hi),
                    'cell_spec': {'parts': {}, 'filters': [], 'label': lab, 'agi': None, 'line': None}})

    def in_g(a, lo, hi):
        return (a >= (-np.inf if lo is None else lo)) & (a < (np.inf if hi is None else hi))

    # the number of returns: shares across groups of the total the exact returns cells set
    T = sum(d['filers 2023'] / d['units 2023'] * d['units after step 1'] for d in STATE['return_shares'].values())
    bl = fs_blend('returns')
    nov = fs_november('returns', 'units')
    base = {lab: float(w0[k & fb & in_g(a0, lo, hi)].sum()) for lo, hi, lab in FS_EDGES}
    grown = {lab: base[lab] * (bl['w'] * nov[lab][1] / nov[lab][0] + (1 - bl['w']) * bl['cbo']) for lab in base}
    for lo, hi, lab in FS_EDGES:
        share = grown[lab] / sum(grown.values())
        lp = float(w[lst & fs_ & in_g(a1, lo, hi)].sum())
        kind, e = fs_error('returns', 'units', lab)
        val = share * T + lp
        row('returns', 'units', lab, lo, hi, val, e * val,
            f"share {share:.5f} of the {T:,.0f} returns the exact returns cells set: the {FS['base_year']} file's "
            f"{base[lab]:,.0f} returns in the group times the November growth {nov[lab][1] / nov[lab][0]:.5f}, as a "
            f"share; plus the year's list {lp:,.0f}")
        table.append({'item': 'returns', 'statistic': 'units', 'group': lab, 'base': base[lab],
                      'november growth': nov[lab][1] / nov[lab][0], 'growth': share * T / base[lab] if base[lab] else np.nan,
                      'value': val, 'se': e * val})
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
                m0 = k & fb & in_g(a0, lo, hi)
                base_g = float(w0[m0] @ v0[m0])
                ng = nov[lab][1] / nov[lab][0]
                g = bl['w'] * ng + (1 - bl['w']) * (bl['cbo'] if stat == 'amount' else 1.0)
                if stat == 'count' and bl['w'] < 1:
                    raise AssertionError('counts are targeted only for items of full trust')
                lp_m = lst & fs_ & in_g(a1, lo, hi)
                lp = float(w[lp_m] @ v1[lp_m])
                val = base_g * g + lp
                kind, e = fs_error(item, stat, lab)
                se = e * nat if kind == 'dollars' else e * abs(val)
                row(item, stat, lab, lo, hi, val, se,
                    f"the {FS['base_year']} file's {base_g:,.0f} in the group (filers, Forbes list left out) times "
                    f"{g:.5f} = {bl['w']:.3f} x November {ng:.5f} + {1 - bl['w']:.3f} x CBO's national "
                    f"{bl['cbo'] if stat == 'amount' else 1.0:.5f}; plus the year's list {lp:,.0f}; error "
                    f"{'%.4f of the national total' % e if kind == 'dollars' else '%.4f' % e}")
                table.append({'item': item, 'statistic': stat, 'group': lab, 'base': base_g, 'november growth': ng,
                              'growth': g, 'value': val, 'se': se})
    STATE['fs']['targets'] = table
    return out


def fs_write(folder):
    """The filing-season lines of the summary, and projection_filing_season.csv (the blends) and
    projection_filing_season_targets.csv (the group targets)."""
    if not fs_active(STATE.get('year', 0)):
        return [f"Filing-season blend (settings tilt.projection.filing_season): "
                f"{'off' if not FS.get('enabled', False) else 'not used before ' + str(FS['year'])}.", '']
    bl = pd.DataFrame(list(STATE['fs']['blend'].values()))
    cols = pd.DataFrame([{'column': c, 'factor': v[0], 'how': v[1]} for c, v in STATE['fs']['columns'].items()
                         if v is not None])
    bl.to_csv(f'{folder}/projection_filing_season.csv', index=False)
    cols.to_csv(f'{folder}/projection_filing_season_columns.csv', index=False)
    lines = [f"## The filing-season blend (November tables {FS['base_year']} and {FS['year']}; trust table "
             f"{_FS['trust_path']})", '', bl.to_markdown(index=False, floatfmt='.5f'), '',
             f"Step 2 factors on {len(cols)} columns (projection_filing_season_columns.csv).", '']
    if 'targets' in STATE['fs']:
        tg = pd.DataFrame(STATE['fs']['targets'])
        tg.to_csv(f'{folder}/projection_filing_season_targets.csv', index=False)
        lines += [f"{len(tg)} targets by AGI group (projection_filing_season_targets.csv), replacing the national totals "
                  f"of {sorted(fs_replaced_columns(STATE['year']))}.", '']
    return lines
