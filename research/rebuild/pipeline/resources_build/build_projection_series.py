"""Builds the series the years after the last SOI tables are grown and targeted by. Usage, through Slurm, a plain
script with every input passed as an argument:

    python -I research/rebuild/pipeline/resources_build/build_projection_series.py <Z.1 csv zip> <Macro-Projections folder> <CBO details csv> <out folder>

Writes, into <out folder> (research/rebuild/resources/growth/):
  z1_household_net_worth.csv   the Federal Reserve's Financial Accounts (Z.1, release of 11 September 2026, table
                               S1M.b, formerly B.101), households and nonprofit organizations, net worth (FL152090005),
                               the fourth-quarter level of each year in dollars, 2010-2025; from 2026 the 2025 level
                               grown with nominal GDP (Macro-Projections gdp), so the ratio of wealth to GDP stays at
                               its 2025 value (a note on each such row). Grows the balance sheet per person past the
                               SCF's last wave (build_table.scf_growth).
  ss_area_population.csv       the Social Security area population, every age, married and unmarried summed
                               (Macro-Projections historical.csv and projections.csv: CBO's January 2026 demographic
                               projections), 2010-2075.
  cbo_individual_income_tax.csv  CBO's February 2026 projections of individual income tax items (Revenue
                               Projections by Category, sheet 3, calendar years 2022-2036; billions of dollars, returns
                               in millions), one row per concept and year: year, concept, value.
"""
import csv
import io
import os
import sys
import zipfile

z1_zip, mp, cbo, out = sys.argv[1:5]


def macro(name):
    with open(os.path.join(mp, name)) as f:
        return list(csv.DictReader(f))


rows = macro('historical.csv') + macro('projections.csv')
gdp = {int(float(r['year'])): float(r['gdp']) for r in rows if r.get('gdp') not in ('', 'NA', None)}
pop = {}
for r in rows:
    y = int(float(r['year']))
    cols = [k for k in r if k.startswith('married_') or k.startswith('unmarried_')]
    vals = [r[k] for k in cols]
    if all(v not in ('', 'NA') for v in vals) and cols:
        pop[y] = sum(float(v) for v in vals)

with zipfile.ZipFile(z1_zip) as z:
    text = z.read('csv/S1M_b.csv').decode()
rd = list(csv.reader(io.StringIO(text)))
col = rd[0].index('FL152090005.Q')
nw = {}
for r in rd[1:]:
    if r[0].endswith(':Q4'):
        nw[int(r[0][:4])] = float(r[col]) * 1e6
last = max(nw)
with open(os.path.join(out, 'z1_household_net_worth.csv'), 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['year', 'value', 'note'])
    for y in range(2010, 2076):
        if y in nw:
            w.writerow([y, f'{nw[y]:.0f}', ''])
        else:
            assert y > last and y in gdp and last in gdp, y
            w.writerow([y, f'{nw[last] * gdp[y] / gdp[last]:.0f}',
                        f'not observed: the {last} level grown with Macro-Projections nominal GDP (wealth to GDP held at {last})'])
with open(os.path.join(out, 'ss_area_population.csv'), 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['year', 'value', 'note'])
    for y in range(2010, 2076):
        assert y in pop, f'no population for {y}'
        w.writerow([y, f'{pop[y]:.0f}', 'projection (CBO January 2026 demographic projections)' if y >= 2026 else ''])
with open(cbo) as f:
    d = list(csv.DictReader(f))
with open(os.path.join(out, 'cbo_individual_income_tax.csv'), 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['year', 'concept', 'value', 'row'])
    for r in d:
        concept = ' '.join(r['concept'].split())
        for k, v in r.items():
            if k.isdigit() and v not in ('', None):
                w.writerow([int(k), concept, v, r['row']])
print('Z.1 net worth', {y: round(nw[y] / 1e12, 2) for y in sorted(nw) if y >= 2015}, 'trillion; last observed', last)
print('population', {y: round(pop[y] / 1e6, 1) for y in (2015, 2019, 2023, 2025, 2030, 2035)}, 'million')
