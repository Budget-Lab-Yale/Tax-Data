"""The long-run projection years, 2036-2075 (4 November 2026, branch rebuild-longrun; scratch
explore/2026-11-04/longrun/). Usage, through Slurm, from the repository root with the pipeline on PYTHONPATH:

    python research/rebuild/resources/growth/build_long_run.py [<Macro-Projections folder>]

(default: settings tilt.projection.population_dir, the Macro-Projections interface the person counts come from).
Every series the projection years read continues past CBO's last year used (settings tilt.projection.cbo_last_year,
2035) at the growth of one Macro-Projections aggregate (production's long-run mapping, src/project_puf.R income_far).
Writes, all from the same interface:

  growth/macro_long_run.csv      the aggregates, 2026-2075, with 'units' (people aged 18 and over, a married couple
                                 counted once: unmarried people 18 and over plus half of all married people) and
                                 'population' (every age). Read by projection_growth.long_run_growth, which continues
                                 the series the growth.yaml rules read and the count of returns.
  growth/z1_household_net_worth.csv, growth/z1_scf_*.csv, growth/ss_area_population.csv
                                 rows 2061-2075 added by each file's own rule (past 2025 the Z.1 levels at their 2025
                                 ratio to nominal GDP; the population summed from the interface); rows to 2060 kept
                                 as they are.
  projection_series_b.csv, projection_series_med.csv
                                 rows 2036-2075 added; the rows 2023-2035 are kept byte for byte. (Column
                                 int_gdp_bill is written for every year by build_interest_series.py; its mapping row,
                                 gdp * tsy_3m, continues it here by the same rule.)
  projection_series_current.csv  one column per items-table row whose rule was 'current' and resolved, in every year
                                 2024-2035, to a CBO or Federal Reserve series (projection_growth.plan_factors): its
                                 growth in total from 2023 exactly as tilt.grow_dollars computed it (the factor per
                                 return times CBO's growth in returns), then continued like the others.
Each column's aggregate is its row of resources/projection_long_run_mapping.csv: kind 'total', the column's 2035 value
times the aggregate's growth from 2035; kind 'per_unit', divided by the growth of 'units' or 'population' (column
per) when the series is per return or per person, or by nothing for a price; 'fade' continues the column's own
yearly decline (alimony, John's decision 2 of 3 November).
"""
import csv
import os
import sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
RES = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(os.path.dirname(RES), 'pipeline'))
from shared import common  # noqa: E402

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
    # rate, the taxable interest series of build_interest_series.py; 5 November 2026). Read from the whole interface,
    # so a rate need not be one of AGG; a column of AGG has the same value in both
    return float(sum(np.prod([float(m.loc[y, f.strip()]) for f in c.split('*')]) for c in name.split('+')))


# ---- 2. the Z.1 and population files past 2060 ------------------------------------------------------------------
def extend_lines(path, rows_after):
    """Keep the file's lines up to its year 2060 as they are (their line ends too: these files were written by csv's
    default, carriage return and line feed) and write the rows after it the same way."""
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

# today's 'current' rows that resolve to a CBO or Federal Reserve series in every year 2024-L: their growth in total
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
