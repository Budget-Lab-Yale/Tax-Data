"""Builds resources/growth/nipa_pce_categories.csv: nominal PCE in the eight CEX categories, billions of
dollars, 2010-2025, from BEA NIPA Table 2.4.5U (release of 29 October 2026, as a CSV of series, period and millions).
The columns are those of production's resources/pce_historical.csv, which starts in 2017. Other services and health is
total PCE less goods less housing and utilities. Prints the largest difference from production's file. Usage:
    python -I research/rebuild/pipeline/resources_build/build_nipa_pce.py <nipa csv> <pce_historical.csv> <out csv>
"""
import csv
import sys

src, prod, out = sys.argv[1:4]
v = {}
for r in csv.DictReader(open(src)):
    if r['millions_of_dollars'].strip('. '):   # BEA marks a value it does not publish with dots
        v[(r['series'], int(r['period']))] = float(r['millions_of_dollars']) / 1000
COLS = {'c_clothing': ['DCLORC'], 'c_motor_vehicles': ['DMOTRC'], 'c_durables': ['DFDHRC', 'DREQRC', 'DODGRC'],
        'c_other_nondurables': ['DONGRC'], 'c_food_off_premises': ['DFXARC'], 'c_gasoline': ['DGOERC'],
        'c_housing_utilities': ['DHUTRC'], 'gdp_c': ['DPCERC']}
rows = []
for y in range(2010, 2026):
    r = {'year': y}
    for c, ss in COLS.items():
        r[c] = sum(v[(s, y)] for s in ss)
    r['c_other_services_health'] = v[('DPCERC', y)] - v[('DGDSRC', y)] - v[('DHUTRC', y)]
    rows.append(r)
cols = ['year', 'c_clothing', 'c_motor_vehicles', 'c_durables', 'c_other_nondurables', 'c_food_off_premises',
        'c_gasoline', 'c_housing_utilities', 'c_other_services_health', 'gdp_c']
with open(out, 'w', newline='') as f:
    w = csv.DictWriter(f, fieldnames=cols)
    w.writeheader()
    for r in rows:
        w.writerow({k: (round(x, 3) if k != 'year' else x) for k, x in r.items()})
p = {int(r['year']): r for r in csv.DictReader(open(prod))}
worst = max((abs(float(p[r['year']][c]) / r[c] - 1), r['year'], c) for r in rows if r['year'] in p for c in cols[1:]
            if p[r['year']].get(c) not in ('', None))
print('largest relative difference from production in common years:', worst)
