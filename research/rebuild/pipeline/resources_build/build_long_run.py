"""The long-run projection years, after CBO's last year used (settings tilt.projection.cbo_last_year) to 2075.

Every series the projection years read continues at the growth of one Macro-Projections aggregate, as its row of
resources/projection_long_run_mapping.csv says: in total, per unit (adults, a married couple counted once), per
person, or as a fading trend. Writes growth/macro_long_run.csv and adds the later rows to the Z.1, population and
projection_series_*.csv files, keeping the earlier rows byte for byte.

    python -m resources_build.build_long_run [<Macro-Projections folder>]
"""
import csv
import os
import re
import sys
import zipfile

import numpy as np
import pandas as pd

from shared import common

RES = common.RESOURCES
HERE = os.path.join(RES, 'growth')
P = common.SETTINGS['tilt']['projection']
MP = sys.argv[1] if len(sys.argv) > 1 else P['population_dir']
L = int(P['cbo_last_year'])
LAST = 2075
AGG = ['gdp', 'gdp_c', 'gdp_i', 'gdp_wages', 'gdp_proprietors', 'gdp_rent', 'gdp_interest', 'gdp_div', 'gdp_corp',
       'outlays_mand_oasdi', 'cpiu', 'pce_deflator']

# ---- 1. the aggregates ------------------------------------------------------------------------------------------
h, p = pd.read_csv(os.path.join(MP, 'historical.csv')), pd.read_csv(os.path.join(MP, 'projections.csv'))
m = pd.concat([h, p[~p.year.isin(h.year)]]).set_index('year').sort_index()
married = [c for c in m.columns if c.startswith('married_')]
unmarried = [c for c in m.columns if c.startswith('unmarried_')]
age = lambda c: int(c.split('_')[1])
m['units'] = m[[c for c in unmarried if age(c) >= 18]].sum(1) + m[married].sum(1) / 2
m['population'] = m[married + unmarried].sum(1)
YEARS = list(range(2026, LAST + 1))
lr = m.loc[YEARS, AGG + ['units', 'population']]
assert np.isfinite(lr.to_numpy(float)).all() and (lr.to_numpy(float) > 0).all(), 'an aggregate missing or not positive'
lr = lr.reset_index()
lr['note'] = f'Macro-Projections {MP}'
lr.to_csv(os.path.join(HERE, 'macro_long_run.csv'), index=False)
print(f'macro_long_run.csv: {YEARS[0]}-{LAST}, {len(AGG)} aggregates, units and population')
lev = lr.set_index('year')


def agg(name, y):
    # a sum of terms, each a product of Macro-Projections columns ('gdp * tsy_3m': nominal GDP times the 3-month bill
    # rate, the taxable interest series of build_interest_series.py). Read from the whole interface,
    # so a rate need not be one of AGG; a column of AGG has the same value in both
    return float(sum(np.prod([float(m.loc[y, f.strip()]) for f in c.split('*')]) for c in name.split('+')))


# ---- 2. the Z.1 and population files past 2060 ------------------------------------------------------------------
def extend_lines(path, rows_after):
    """Keep the file's lines up to 2060 as they are, line ends included (carriage return and line feed, csv's
    default), and write the later rows the same way."""
    with open(path, newline='') as f:
        raw = f.read()
    eol = '\r\n' if raw.endswith('\r\n') else '\n'
    lines = raw.split(eol)[:-1]
    keep = [lines[0]] + [ln for ln in lines[1:] if int(ln.split(',')[0]) <= 2060]
    assert int(keep[-1].split(',')[0]) == 2060, f'{path}: does not reach 2060'
    with open(path, 'w', newline='') as f:
        f.write(eol.join(keep) + eol)
        w = csv.writer(f, lineterminator=eol)
        for r in rows_after:
            w.writerow(r)


for name in ['z1_household_net_worth.csv'] + sorted(f for f in os.listdir(HERE) if f.startswith('z1_scf_')):
    path = os.path.join(HERE, name)
    d = pd.read_csv(path).set_index('year')
    base = float(d.loc[2025, 'value'])
    note = d.loc[2060, 'note']
    gdp = lambda y: float(m.loc[y, 'gdp'])
    extend_lines(path, [[y, f'{base * gdp(y) / gdp(2025):.0f}', note] for y in range(2061, LAST + 1)])
path = os.path.join(HERE, 'ss_area_population.csv')
note = pd.read_csv(path).set_index('year').loc[2060, 'note']
extend_lines(path, [[y, f'{m.loc[y, "population"]:.0f}', note] for y in range(2061, LAST + 1)])
print('Z.1 and population files: rows 2061-2075 added')

# ---- 3. the items table's series -----------------------------------------------------------------------------------
MAP = pd.read_csv(os.path.join(RES, 'projection_long_run_mapping.csv'), keep_default_na=False)
assert not MAP.duplicated(['file', 'column']).any(), 'mapping: a column twice'


def continued(col, kind, aggregate, per, v):
    """The column's values L+1..LAST from its value v[L] (v: {year: value})."""
    out = {}
    for y in range(L + 1, LAST + 1):
        if aggregate == 'fade':
            f = v[L] / v[L - 1]
            out[y] = v[L] * f ** (y - L)
            continue
        g = agg(aggregate, y) / agg(aggregate, L)
        if per:
            g /= agg(per, y) / agg(per, L)
        out[y] = v[L] * g
    return out


def write_series(name, base_lines, values):
    """base_lines: the file's header and rows up to L, kept as they are; values: {column: {year: value}} after L."""
    header = base_lines[0].split(',')
    with open(os.path.join(RES, name), 'w', newline='') as f:
        f.write('\n'.join(base_lines) + '\n')
        for y in range(L + 1, LAST + 1):
            f.write(','.join([str(y)] + [repr(float(values[c][y])) for c in header[1:]]) + '\n')


for name in ('projection_series_b.csv', 'projection_series_med.csv'):
    with open(os.path.join(RES, name)) as f:
        lines = f.read().splitlines()
    base = [lines[0]] + [ln for ln in lines[1:] if int(ln.split(',')[0]) <= L]
    d = pd.read_csv(os.path.join(RES, name))
    d = d[d.year <= L].set_index('year')
    rows = MAP[MAP.file == name].set_index('column')
    missing = sorted(set(d.columns) - set(rows.index))
    assert not missing, f'{name}: no long-run aggregate for {missing}'
    vals = {c: continued(c, rows.loc[c, 'kind'], rows.loc[c, 'aggregate'], rows.loc[c, 'per'], d[c].to_dict())
            for c in d.columns}
    write_series(name, base, vals)
    print(f'{name}: rows {L + 1}-{LAST} added for {len(d.columns)} columns')

# ---- 4. the series of the items that follow the Z.1 or NIPA, then CBO --------------------------------------------
# projection_series_current.csv: for each items-table row on series 'current' (the balance sheet, pass-through gains
# and the consumption categories), the growth in total from 2023. Each year's growth per return is the series' growth
# over CBO's growth in the number of returns; resources/projection_growth_mapping.csv names the series, by the item's
# old name where it has one (Z.1 fourth-quarter stocks and NIPA PCE categories through 2025, CBO's GDP and total PCE
# after). The years' factors are compounded, then multiplied by CBO's growth in returns over the same years.
from scale.projection_growth import cbo_return_growth  # noqa: E402

GROWTH_MAP = pd.read_csv(os.path.join(RES, 'projection_growth_mapping.csv'))
CBO_ECON = pd.read_csv(os.path.join(RES, 'cbo_economic.csv'), dtype={'date': str}).set_index(['variable', 'date'])['value']
NIPA_PCE = pd.read_csv(os.path.join(RES, 'growth', 'nipa_pce_categories.csv')).set_index('year')
with zipfile.ZipFile(common.RAW['z1']) as archive:
    Z1 = [pd.read_csv(archive.open(n)).set_index('date') for n in ('csv/S1M_b.csv', 'csv/F4_3_s.csv')]


def z1_level(expr, y):
    """The fourth-quarter level of a Z.1 expression 'FL... + LM... minus FL...', summed term by term."""
    parts = re.split(r' (\+|minus) ', expr)
    terms = [(parts[0], 1)] + [(parts[i + 1], 1 if parts[i] == '+' else -1) for i in range(1, len(parts), 2)]
    level = 0.
    for series, sign in terms:
        frames = [d for d in Z1 if series in d.columns and f'{y}:Q4' in d.index]
        assert frames, f'Z.1 series {series} is absent for {y}'
        level += sign * float(frames[0].loc[f'{y}:Q4', series])
    return level


def per_return(col, y):
    """The column's growth per return from y - 1 to y."""
    if col.startswith('c_'):
        a, b = (float(NIPA_PCE.loc[y - 1, col]), float(NIPA_PCE.loc[y, col])) if y <= 2025 else \
            (float(CBO_ECON[('pce', str(y - 1))]), float(CBO_ECON[('pce', str(y))]))
    else:
        key = GROWTH_MAP.loc[(GROWTH_MAP.renamed_item == col) | (GROWTH_MAP.old_item == col), 'old_item'].unique()
        assert len(key) == 1, f'{col}: {len(key)} mapping keys {list(key)}'
        rule = GROWTH_MAP[(GROWTH_MAP.old_item == key[0]) & (GROWTH_MAP.source == 'Federal Reserve Z.1 and CBO')]
        assert len(rule) == 1, f'{col}: expected one Z.1 rule, found {len(rule)}'
        expr = rule.iloc[0]['series'].split(' through 2025; then CBO gdp')[0]
        a, b = (z1_level(expr, y - 1), z1_level(expr, y)) if y <= 2025 else \
            (float(CBO_ECON[('gdp', str(y - 1))]), float(CBO_ECON[('gdp', str(y))]))
    assert np.isfinite(a) and np.isfinite(b) and a > 0 and b > 0, f'{col} {y}: levels {a}, {b}'
    return (b / a) / cbo_return_growth(y)


rows = MAP[MAP.file == 'projection_series_current.csv'].set_index('column')
cols = list(rows.index)
g = {c: {2023: 1.0} for c in cols}
for t in range(2024, L + 1):
    R = float(np.prod([cbo_return_growth(y) for y in range(2024, t + 1)]))
    for c in cols:
        f = per_return(c, 2024)
        for y in range(2025, t + 1):
            f = f * per_return(c, y)
        g[c][t] = f * R
vals = {c: continued(c, rows.loc[c, 'kind'], rows.loc[c, 'aggregate'], rows.loc[c, 'per'], g[c]) for c in cols}
with open(os.path.join(RES, 'projection_series_current.csv'), 'w', newline='') as f:
    f.write(','.join(['year'] + cols) + '\n')
    for y in range(2023, LAST + 1):
        f.write(','.join([str(y)] + [repr(float(g[c][y] if y <= L else vals[c][y])) for c in cols]) + '\n')
print(f'projection_series_current.csv: {len(cols)} columns, 2023-{LAST}')
