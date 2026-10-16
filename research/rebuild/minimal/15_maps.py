"""
15_maps.py  (minimal example, v4 baseline: the rank-to-dollar maps from table E4)

What it does. Builds the step 4 maps (04_dollars.py) from table E4
(data/stacked_E4.parquet, data/band_edges_E4.json) instead of the v2 table,
and writes them where 04_dollars.puf_maps reads them when MINIMAL_MAP_DIR is
set:

  map/V4/map_puf_2015.parquet  incomes, from the PUF rows of table E4
  map/V4/map_puf_2019.parquet  the same grown to 2019 by step 4's factors
                               (map/growth_factors.csv, unchanged)
  map/V4/map_scf_2015.parquet  net worth, from the SCF rows of table E4
  map/V4/map_check.csv         per variable: rows, and whether the map equals
                               the v2 map in map/ (same bands, values, weights)

Only the map tables are built here; 04_dollars.py itself is not run.
Run through Slurm (15_run_cpu.sbatch maps).
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = f'{ROOT}/map/V4'
TABLE, EDGES = f'{ROOT}/data/stacked_E4.parquet', f'{ROOT}/data/band_edges_E4.json'


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


dollars = load('dollars', '04_dollars.py')


def main():
    os.makedirs(OUT, exist_ok=True)
    df = pd.read_parquet(TABLE)
    edges = json.load(open(EDGES))
    assert set(df['year']) == {2015}
    puf = df[df['source'] == 'puf'].reset_index(drop=True)
    scf = df[df['source'] == 'scf'].reset_index(drop=True)
    incomes = [v for v in dollars.AMOUNTS if v != 'net_worth']
    keep = dollars.AMOUNTS
    try:
        dollars.AMOUNTS = incomes
        mp = dollars.build_map(puf, edges, 'puf')
        dollars.AMOUNTS = ['net_worth']
        ms = dollars.build_map(scf, edges, 'scf')
    finally:
        dollars.AMOUNTS = keep
    factors = pd.read_csv(f'{ROOT}/map/growth_factors.csv')
    mp.to_parquet(f'{OUT}/map_puf_2015.parquet', index=False)
    dollars.grow_map(mp, factors).to_parquet(f'{OUT}/map_puf_2019.parquet', index=False)
    ms.to_parquet(f'{OUT}/map_scf_2015.parquet', index=False)
    rows = []
    old = {'puf': pd.read_parquet(f'{ROOT}/map/map_puf_2015.parquet'),
           'scf': pd.read_parquet(f'{ROOT}/map/map_scf_2015.parquet')}
    for src, new, vars_ in [('puf', mp, incomes), ('scf', ms, ['net_worth'])]:
        for v in vars_:
            a = new[new['variable'] == v].reset_index(drop=True)
            b = old[src][old[src]['variable'] == v].reset_index(drop=True)
            same = (len(a) == len(b) and np.array_equal(a['band'], b['band'])
                    and np.allclose(a['value'], b['value']) and np.allclose(a['weight'], b['weight']))
            rows.append({'source': src, 'variable': v, 'rows_E4': len(a), 'rows_v2': len(b),
                         'bands_E4': a['band'].nunique(), 'filled_rows_E4': int(a['filled'].sum()),
                         'same_as_v2_map': same})
    chk = pd.DataFrame(rows)
    chk.to_csv(f'{OUT}/map_check.csv', index=False)
    print(chk.to_string())


if __name__ == '__main__':
    main()
