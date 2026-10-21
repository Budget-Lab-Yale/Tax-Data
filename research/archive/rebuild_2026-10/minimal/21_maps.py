"""
21_maps.py  (minimal example, step 21: priors for 2019 from several SCF
waves; 21_PRIORS.md; 2026-10-16)

What it does. Builds the dollar maps for the step 21 runs in map/V5/:

  map_puf_2015.parquet, map_puf_2019.parquet, map_scf_2015.parquet
      copied from map/V4/ unchanged (the PUF's bands are the same in tables
      E4 and Y1, so the income maps are v4c's).
  map_scf_nw_<t>.parquet, t = 2015, 2018, 2021
      net worth, from the SCF rows of table Y1 with year token t (the 2016,
      2019 and 2022 waves). Net worth is cut within each wave, so a record's
      net worth band means a position in its own wave, and its dollars come
      from that wave. All three maps are in 2015 dollars, the price basis of
      the wealth targets (06_wealth_tables.scf2019): table Y1 holds each
      wave's net worth in its own survey year's dollars (the SCF summary
      file's 2022-dollar value over the wave's asset factor), so the values
      are multiplied by the wave's asset factor over 1.2493059 (TO_2015).

Checks (stop on failure):
  - the 2015 map matches v4c's net worth map (map/V4/map_scf_2015.parquet):
    at most two rows in another band (one row sits on a cut point; see the
    code) and the same weighted total to 1e-6;
  - the 2018 map's wave, weighted, totals the 2019 wealth target's total
    net worth ($89.14 trillion in 2015 dollars) to within 0.1%.

Run through Slurm: sbatch research/rebuild/minimal/21_run_cpu.sbatch maps
"""
import importlib.util
import json
import os
import shutil

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = f'{ROOT}/map/V5'
V4 = f'{ROOT}/map/V4'
TABLE, EDGES = f'{ROOT}/data/stacked_Y1.parquet', f'{ROOT}/data/band_edges_Y1.json'
# The factor each wave's net worth was divided by in table Y1, from the
# wave builders' checks, and the 2015 price factor of the targets.
FACTOR_KEY = {2015: ('checks_scf2016.csv', 'factor_used_for_net_worth'),
              2018: ('checks_scf2019.csv', 'asset_factor_used_for_net_worth'),
              2021: ('checks_scf2022.csv', 'factor_used_for_net_worth')}
# The 2015-dollar factor: the 2016 wave's exact summary-file factor, the one
# table E4 divided net worth by (v4c's map). The wealth targets
# (06_wealth_tables.py) use it rounded, 1.2493; the two differ by 0.0005%.
TO_2015 = 1.249305937909091
TARGET_TOTAL_2019_WAVE = 89.14e12   # 15_BASELINE_V4.md, v4c targets, total net worth (rounded)


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


dollars = load('dollars', '04_dollars.py')


def wave_factor(year):
    file, key = FACTOR_KEY[year]
    c = pd.read_csv(f'{ROOT}/data/{file}')
    return float(c.loc[c['key'] == key, 'value'].item())


def main():
    os.makedirs(OUT, exist_ok=True)
    for f in ['map_puf_2015.parquet', 'map_puf_2019.parquet', 'map_scf_2015.parquet']:
        shutil.copyfile(f'{V4}/{f}', f'{OUT}/{f}')
    df = pd.read_parquet(TABLE)
    edges = json.load(open(EDGES))
    keep, rows = dollars.AMOUNTS, []
    for t in sorted(FACTOR_KEY):
        scf = df[(df['source'] == 'scf') & (df['year'] == t)].reset_index(drop=True)
        assert len(scf) > 0, t
        assert not scf['net_worth'].isna().any(), f'{t}: SCF rows without net worth'
        # build_map reads the edges under scf/2015/; hand it this wave's.
        e = {'scf/2015/net_worth': edges[f'scf/{t}/net_worth']}
        try:
            dollars.AMOUNTS = ['net_worth']
            mp = dollars.build_map(scf, e, 'scf')
        finally:
            dollars.AMOUNTS = keep
        f = wave_factor(t) / TO_2015
        mp['value'] = mp['value'] * f        # real and filled rows alike
        mp.to_parquet(f'{OUT}/map_scf_nw_{t}.parquet', index=False)
        total = float((scf['net_worth'] * f * scf['weight']).sum())
        rows.append({'year_token': t, 'records': len(scf), 'units_millions': scf['weight'].sum() / 1e6,
                     'factor_to_2015_dollars': f, 'total_net_worth_trillions_2015_dollars': total / 1e12,
                     'map_rows': len(mp), 'filled_rows': int(mp['filled'].sum())})
        if t == 2015:
            old = pd.read_parquet(f'{V4}/map_scf_2015.parquet')
            old = old[old['variable'] == 'net_worth'].reset_index(drop=True)
            new = mp.reset_index(drop=True)
            # Table Y1 holds this wave in 2016 dollars, E4 in 2015 dollars
            # (ratio 1.01292); one row of 34,377 sits on a cut point and lands
            # one band lower in Y1 through rounding. Allow that, nothing more.
            assert len(old) == len(new), (len(old), len(new))
            moved = int((old['band'].to_numpy() != new['band'].to_numpy()).sum())
            assert moved <= 2, moved
            tot_old = float((old['value'] * old['weight'])[old['filled'].astype(bool).eq(False).to_numpy()].sum())
            tot_new = float((new['value'] * new['weight'])[new['filled'].astype(bool).eq(False).to_numpy()].sum())
            assert abs(tot_new / tot_old - 1) < 1e-6, (tot_old, tot_new)
            print(f'2015 map against v4c net worth map: {moved} map rows in another band; '
                  f'weighted totals differ by {tot_new / tot_old - 1:.2e}', flush=True)
        if t == 2018:
            gap = total / TARGET_TOTAL_2019_WAVE - 1
            print(f'2019 wave total in 2015 dollars ${total/1e12:.2f}T against target $89.14T '
                  f'({100*gap:+.3f}%)', flush=True)
            assert abs(gap) < 0.001, gap   # the target is quoted to $0.01 trillion
    out = pd.DataFrame(rows)
    out.to_csv(f'{OUT}/map_check_v5.csv', index=False)
    print(out.to_string())


if __name__ == '__main__':
    main()
