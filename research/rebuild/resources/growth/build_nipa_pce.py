"""Builds nipa_pce_categories.csv (30 October 2026, targets for every year): BEA NIPA Table 2.4.5U (nominal PCE by
product, annual, release of 29 October 2026, the underlying detail workbook Section2All_xls.xlsx as extracted to
codex_cbo/derived/nipa_2_4_5u_A.csv), billions of dollars, 2010-2025, the eight CEX categories in the columns of
production's resources/pce_historical.csv (which starts in 2017), so the CEX can be grown to 2015 and 2016:
  c_clothing DCLORC; c_motor_vehicles DMOTRC; c_durables DFDHRC + DREQRC + DODGRC; c_other_nondurables DONGRC;
  c_food_off_premises DFXARC; c_gasoline DGOERC; c_housing_utilities DHUTRC;
  c_other_services_health DPCERC - DGDSRC - DHUTRC; gdp_c DPCERC.
Usage: python -I build_nipa_pce.py <nipa_2_4_5u_A.csv> <production pce_historical.csv> <out csv>. Prints the largest
difference from production's file in the years both have."""
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
