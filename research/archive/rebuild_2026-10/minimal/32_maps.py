"""
32_maps.py  (minimal example, step 32: a 2019 starting file that mixes SCF
waves; 32_STATUS.md; 2026-10-18)

What it does. Builds the dollar maps step 32 needs in map/V32/, new files
only (map/V27/, step 31's folder, is read and never written):

  map_puf_2015.parquet, map_puf_2019.parquet, map_scf_2015.parquet
      byte-for-byte copies of map/V27/ (table E27's maps, step 31's).
  map_scf_nw_2015.parquet
      the net worth rows of map/V27/map_scf_2015.parquet, unchanged: the
      map step 31 used for every record. Records whose net worth was drawn
      under the 2016 wave's year token get exactly step 31's dollars, so the
      file that is 100% 2016 token reproduces step 31's tilt.
  map_scf_nw_2018.parquet
      net worth from table E27's 2019-wave rows (year token 2018), built as
      21_maps.py built it for table Y1: the wave's own band edges
      (scf/2018/net_worth), its own records and weights, values converted to
      the price basis of the wealth targets (2015 dollars: the wave's
      2019-dollar values times its asset factor 1.1592 over 1.2493059).

Checks (stop on failure) and diagnostics (printed and written to
map/V32/map_check_v32.csv):
  - the copies equal V27's files;
  - the 2016 wave's map rebuilt from table E27 equals V27's (the method is
    the one 27_tables.py used);
  - the 2019 wave's total net worth in 2015 dollars equals the wealth
    target's total ($89.14 trillion) to within 0.1%;
  - no SCF row of the two waves lacks net worth (count printed);
  - the price basis of step 31's 2016-wave map: its weighted total against
    the same wave in 2015 dollars (map/V5/map_scf_nw_2015.parquet, built by
    21_maps.py from table Y1, whose 2016-wave rows E27 copies). Printed, not
    asserted.

Run through Slurm: sbatch -J maps32 research/rebuild/minimal/32_cpu.sbatch maps
"""
import filecmp
import importlib.util
import json
import os
import shutil

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
V27, OUT, V5 = f'{ROOT}/map/V27', f'{ROOT}/map/V32', f'{ROOT}/map/V5'
TABLE, EDGES = f'{ROOT}/data/stacked_E27.parquet', f'{ROOT}/data/band_edges_E27.json'
TO_2015 = 1.249305937909091          # the 2016 wave's exact factor (21_maps.py)
TARGET_TOTAL_2019_WAVE = 89.14e12    # 2015 dollars (21_maps.py)


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


dollars = load('dollars', '04_dollars.py')


def asset_factor_2019():
    c = pd.read_csv(f'{ROOT}/data/checks_scf2019.csv')
    return float(c.loc[c['key'] == 'asset_factor_used_for_net_worth', 'value'].item())


def nw_map(scf, edges, year):
    """build_map for net worth only, on this wave's edges."""
    keep = dollars.AMOUNTS
    try:
        dollars.AMOUNTS = ['net_worth']
        return dollars.build_map(scf, {'scf/2015/net_worth': edges[f'scf/{year}/net_worth']}, 'scf')
    finally:
        dollars.AMOUNTS = keep


def wtotal(mp):
    real = ~mp['filled'].astype(bool)
    return float((mp['value'] * mp['weight'])[real].sum())


def main():
    os.makedirs(OUT, exist_ok=True)
    for f in ['map_puf_2015.parquet', 'map_puf_2019.parquet', 'map_scf_2015.parquet']:
        shutil.copyfile(f'{V27}/{f}', f'{OUT}/{f}')
        assert filecmp.cmp(f'{V27}/{f}', f'{OUT}/{f}', shallow=False), f
    old = pd.read_parquet(f'{V27}/map_scf_2015.parquet')
    old = old[old['variable'] == 'net_worth'].reset_index(drop=True)
    old.to_parquet(f'{OUT}/map_scf_nw_2015.parquet', index=False)

    df = pd.read_parquet(TABLE, columns=['source', 'year', 'net_worth', 'net_worth_band', 'weight'])
    edges = json.load(open(EDGES))
    rows = []
    for year in (2015, 2018):
        scf = df[(df['source'] == 'scf') & (df['year'] == year)].reset_index(drop=True)
        n_na = int(scf['net_worth'].isna().sum())
        print(f'wave with year token {year}: {len(scf)} SCF rows, {n_na} without net worth', flush=True)
        assert len(scf) > 0 and n_na == 0, (year, len(scf), n_na)
        mp = nw_map(scf, edges, year)
        if year == 2015:
            same = (len(mp) == len(old) and (mp['band'].to_numpy() == old['band'].to_numpy()).all()
                    and np.array_equal(mp['value'].to_numpy(), old['value'].to_numpy())
                    and np.array_equal(mp['weight'].to_numpy(), old['weight'].to_numpy()))
            print(f'2016 wave map rebuilt from E27 equals map/V27 net worth map: {same}', flush=True)
            assert same
            v5 = pd.read_parquet(f'{V5}/map_scf_nw_2015.parquet')
            ratio = wtotal(old) / wtotal(v5)
            print(f'price basis: step 31 2016-wave map total ${wtotal(old) / 1e12:.3f}T against the same '
                  f'wave in 2015 dollars (map/V5) ${wtotal(v5) / 1e12:.3f}T, ratio {ratio:.5f} '
                  f'(2016 dollars over 2015 dollars would be {TO_2015 / 1.2333709132:.5f})', flush=True)
            rows.append({'map': 'map_scf_nw_2015 (step 31 map, unchanged)', 'records': len(scf),
                         'units_millions': scf['weight'].sum() / 1e6, 'factor_applied': 1.0,
                         'total_net_worth_trillions_map_units': wtotal(old) / 1e12,
                         'ratio_to_2015_dollar_version': ratio, 'map_rows': len(old),
                         'filled_rows': int(old['filled'].sum())})
        else:
            f = asset_factor_2019() / TO_2015
            mp['value'] = mp['value'] * f
            mp.to_parquet(f'{OUT}/map_scf_nw_2018.parquet', index=False)
            total = float((scf['net_worth'] * f * scf['weight']).sum())
            gap = total / TARGET_TOTAL_2019_WAVE - 1
            print(f'2019 wave total in 2015 dollars ${total / 1e12:.3f}T against the target $89.14T '
                  f'({100 * gap:+.3f}%)', flush=True)
            assert abs(gap) < 0.001, gap
            v5 = pd.read_parquet(f'{V5}/map_scf_nw_2018.parquet')
            print(f'against step 21 map/V5/map_scf_nw_2018 (table Y1): rows {len(mp)} vs {len(v5)}, '
                  f'totals ${wtotal(mp) / 1e12:.3f}T vs ${wtotal(v5) / 1e12:.3f}T', flush=True)
            rows.append({'map': 'map_scf_nw_2018 (2019 wave, 2015 dollars)', 'records': len(scf),
                         'units_millions': scf['weight'].sum() / 1e6, 'factor_applied': f,
                         'total_net_worth_trillions_map_units': wtotal(mp) / 1e12,
                         'ratio_to_2015_dollar_version': 1.0, 'map_rows': len(mp),
                         'filled_rows': int(mp['filled'].sum())})
    out = pd.DataFrame(rows)
    out.to_csv(f'{OUT}/map_check_v32.csv', index=False)
    print(out.to_string())


if __name__ == '__main__':
    main()
