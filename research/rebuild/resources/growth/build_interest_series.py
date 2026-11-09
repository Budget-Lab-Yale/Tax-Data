"""The growth of taxable interest after the jump-off year (John, 5 November 2026; branch rebuild-interest-growth;
scratch explore/2026-11-05/interest_check/FINDINGS.md and explore/2026-11-05/interest_growth/). Usage, through Slurm,
from the repository root with the pipeline on PYTHONPATH:

    python research/rebuild/resources/growth/build_interest_series.py [<Macro-Projections folder>]

(default: settings tilt.projection.population_dir, the Macro-Projections interface the long-run series come from).

The interest on tax returns behaves like short-term money (taxable interest tripled from 2022 to 2024 as short rates
rose), so after 2024 it grows as the assets that earn it times the yield they earn: nominal GDP times the 3-month
Treasury bill rate, each relative to its own value in an earlier year. Writes one column, int_gdp_bill, into
resources/projection_series_b.csv (2023 = 1, every year 2023-2075):

  2023 to cbo_last_year (2035)  CBO's February 2026 economic projections, calendar years (resources/cbo_economic.csv:
                                gdp, treasury_bill_rate_3mo), the same vintage as every other series in the file;
  after cbo_last_year           the 2035 value times the growth of nominal GDP times the 3-month bill rate in the
                                Macro-Projections interface (gdp, tsy_3m; built from the same CBO vintage, and
                                carrying CBO's long-term bill rate to 2075).

Every other column of the file is kept byte for byte (the new column is the last one; a second run replaces it). The
long-run mapping (resources/projection_long_run_mapping.csv) has the matching row (aggregate gdp * tsy_3m), so
build_long_run.py continues the column past 2035 by the same rule.
"""
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
FIRST, LAST = 2023, 2075
COL = 'int_gdp_bill'
SERIES = os.path.join(RES, 'projection_series_b.csv')

# ---- 1. CBO's February 2026 economic projections, 2023 to cbo_last_year -------------------------------------------
e = pd.read_csv(os.path.join(RES, 'cbo_economic.csv'))
e['year'] = e['date'].astype(int)


def cbo(var):
    s = e[e.variable == var].set_index('year')['value'].astype(float)
    missing = [y for y in range(FIRST, L + 1) if y not in s.index or not np.isfinite(s.loc[y]) or s.loc[y] <= 0]
    assert not missing, f'cbo_economic.csv: {var} missing or not positive in {missing}'
    return s


gdp, bill = cbo('gdp'), cbo('treasury_bill_rate_3mo')
level = {y: float(gdp.loc[y]) * float(bill.loc[y]) for y in range(FIRST, L + 1)}

# ---- 2. Macro-Projections after cbo_last_year ------------------------------------------------------------------
h, p = pd.read_csv(os.path.join(MP, 'historical.csv')), pd.read_csv(os.path.join(MP, 'projections.csv'))
m = pd.concat([h, p[~p.year.isin(h.year)]]).set_index('year').sort_index()
for c in ('gdp', 'tsy_3m'):
    v = m.loc[L:LAST, c].astype(float)
    assert list(v.index) == list(range(L, LAST + 1)) and np.isfinite(v).all() and (v > 0).all(), \
        f'{MP}: {c} missing or not positive in {L}-{LAST}'
agg = lambda y: float(m.loc[y, 'gdp']) * float(m.loc[y, 'tsy_3m'])
# the two sources at cbo_last_year (the long-run file is built from the same CBO vintage; only rounding differs)
gap_gdp = float(m.loc[L, 'gdp']) / float(gdp.loc[L]) - 1
gap_bill = float(m.loc[L, 'tsy_3m']) - float(bill.loc[L])
assert abs(gap_gdp) < 1e-4 and abs(gap_bill) < 0.01, f'{L}: Macro-Projections and CBO disagree (GDP {gap_gdp:.2e}, bill {gap_bill:.4f})'

v = {y: level[y] / level[FIRST] for y in range(FIRST, L + 1)}
for y in range(L + 1, LAST + 1):
    v[y] = v[L] * (agg(y) / agg(L))     # as build_long_run.continued, kind total

# ---- 3. the column into the series file, every other byte kept -------------------------------------------------
with open(SERIES) as f:
    lines = f.read().splitlines()
head = lines[0].split(',')
if head[-1] == COL:                       # a second run: drop the column written before
    lines = [ln.rsplit(',', 1)[0] for ln in lines]
    head = head[:-1]
assert COL not in head, f'{SERIES}: {COL} is not the last column'
years = [int(ln.split(',', 1)[0]) for ln in lines[1:]]
assert years == list(range(FIRST, LAST + 1)), f'{SERIES}: years {years[0]}-{years[-1]}, not {FIRST}-{LAST}'
out = [lines[0] + ',' + COL] + [ln + ',' + repr(float(v[y])) for ln, y in zip(lines[1:], years)]
with open(SERIES, 'w', newline='') as f:
    f.write('\n'.join(out) + '\n')

show = [2023, 2024, 2025, 2026, 2030, 2035, 2036, 2040, 2050, 2075]
tab = pd.DataFrame({'GDP ($bn)': [float(gdp.loc[y]) if y <= L else float(m.loc[y, 'gdp']) for y in show],
                    '3-month bill (%)': [float(bill.loc[y]) if y <= L else float(m.loc[y, 'tsy_3m']) for y in show],
                    COL: [v[y] for y in show], 'growth from 2024': [v[y] / v[2024] for y in show]}, index=show)
print(f'{SERIES}: column {COL} written, {FIRST}-{LAST}; Macro-Projections {MP}')
print(f'{L}: Macro-Projections over CBO, GDP {gap_gdp:+.2e}, bill rate {gap_bill:+.4f} points')
print(tab.round(5).to_string())
