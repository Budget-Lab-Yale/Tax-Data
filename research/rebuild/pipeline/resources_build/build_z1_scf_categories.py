"""Build SCF-category Z.1 proxies, Q4 levels; after 2025 hold each proxy's ratio to GDP fixed. A plain script:

    python -I research/rebuild/pipeline/resources_build/build_z1_scf_categories.py S1M_b.csv macro_directory output_directory
"""
import csv
from pathlib import Path
import sys

source, macro, output = map(Path, sys.argv[1:])
# S1M.b, households and nonprofits. Other real estate is total less owner occupied;
# other assets combine consumer durables, life insurance, loans and miscellaneous assets.
series = {
    'deposits_bonds': {'FL153020005.Q': 1, 'FL153030005.Q': 1, 'LM153030505.Q': 1, 'LM154022005.Q': 1},
    'stocks_funds': {'LM153064105.Q': 1, 'LM153064205.Q': 1},
    'retirement': {'FL153050005.Q': 1},
    'business': {'LM153081115.Q': 1},
    'primary_home': {'LM155035015.Q': 1},
    'other_real_estate': {'LM155035005.Q': 1, 'LM155035015.Q': -1},
    'other_assets': {'LM155111005.Q': 1, 'FL153040005.Q': 1, 'FL154035005.Q': 1, 'FL153090005.Q': 1},
    'mortgages': {'FL153165105.Q': 1},
    'other_debt': {'FL153168005.Q': 1, 'FL153166000.Q': 1, 'FL153169005.Q': 1},
}
with source.open() as f:
    rows = list(csv.DictReader(f))
observed = {}
for row in rows:
    if row['date'].endswith(':Q4') and 2010 <= int(row['date'][:4]) <= 2025:
        y = int(row['date'][:4])
        observed[y] = {name: sum(float(row[col]) * sign for col, sign in parts.items()) * 1e6
                       for name, parts in series.items()}
assert set(range(2010, 2026)) <= observed.keys()
assert all(v > 0 for d in observed.values() for v in d.values())
gdp = {}
for file in ('historical.csv', 'projections.csv'):
    with (macro / file).open() as f:
        for r in csv.DictReader(f):
            if r.get('gdp') not in ('', 'NA', None):
                gdp[int(float(r['year']))] = float(r['gdp'])
for name in series:
    with (output / f'z1_scf_{name}.csv').open('w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['year', 'value', 'note'])
        for y in range(2010, 2076):
            value = observed[y][name] if y <= 2025 else observed[2025][name] * gdp[y] / gdp[2025]
            w.writerow([y, f'{value:.0f}', '' if y <= 2025 else 'projection: 2025 ratio to nominal GDP held fixed'])
