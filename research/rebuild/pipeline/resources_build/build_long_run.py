"""The long-run projection years, after CBO's last year used (settings tilt.projection.cbo_last_year) to 2075.

Every series the projection years read continues at the growth of one Macro-Projections aggregate, as its row of
resources/projection_long_run_mapping.csv says: in total, per unit (adults, a married couple counted once), per
person, or as a fading trend. Writes growth/macro_long_run.csv and adds the later rows to the Z.1, population and
projection_series_*.csv files, keeping the earlier rows byte for byte.

    python -m resources_build.build_long_run [<Macro-Projections folder>]
"""
import csv
import os
import sys

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

# projection_series_current.csv: the growth in total of the items-table rows that follow a CBO or Federal Reserve
# series. This section calls projection_growth.plan_factors, which the pipeline does not have, so it stops here; the
# pipeline reads the committed projection_series_current.csv.
from scale import projection_growth as PG  # noqa: E402
rows = MAP[MAP.file == 'projection_series_current.csv'].set_index('column')
cols = list(rows.index)
g = {c: {2023: 1.0} for c in cols}
for t in range(2024, L + 1):
    got, probs = PG.plan_factors(cols, t, None, strict=False, from_year=2023)
    assert not probs, f'{t}: today\'s rule does not resolve {probs}'
    R = float(np.prod([PG.cbo_return_growth(y) for y in range(2024, t + 1)]))   # as tilt.grow_dollars, item mode
    for r in got.itertuples():
        g[r.column][t] = float(r.factor) * R
vals = {c: continued(c, rows.loc[c, 'kind'], rows.loc[c, 'aggregate'], rows.loc[c, 'per'], g[c]) for c in cols}
with open(os.path.join(RES, 'projection_series_current.csv'), 'w', newline='') as f:
    f.write(','.join(['year'] + cols) + '\n')
    for y in range(2023, LAST + 1):
        f.write(','.join([str(y)] + [repr(float(g[c][y] if y <= L else vals[c][y])) for c in cols]) + '\n')
print(f'projection_series_current.csv: {len(cols)} columns, 2023-{LAST}')
