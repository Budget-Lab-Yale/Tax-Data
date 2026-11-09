"""Builds three series in resources/growth/ for the years after the last SOI tables:
z1_household_net_worth.csv (Z.1 table S1M.b, release of 11 September 2026, households and nonprofits' net worth at
each fourth quarter; after the last observed year, held at that year's ratio to nominal GDP), ss_area_population.csv
(the Social Security area population from Macro-Projections, CBO's January 2026 demographic projections), and
cbo_individual_income_tax.csv (CBO's February 2026 individual income tax projections, one row per concept and year).

    python -I research/rebuild/pipeline/resources_build/build_projection_series.py <Z.1 csv zip> <Macro-Projections folder> <CBO details csv> <out folder>
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
