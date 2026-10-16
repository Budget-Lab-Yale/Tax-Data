"""
20_tables.py  (minimal example, step 20: separate models, the two one-source tables)

What it does. Writes table E4 split by source, so that 02_model.py can train
a model on one source through its MINIMAL_TABLE setting without any change
to 02_model.py (20_SEPARATE_MODELS.md, arm B):

  data/stacked_E4puf.parquet   the PUF rows of data/stacked_E4.parquet, as they are
  data/stacked_E4scf.parquet   the SCF rows, as they are
  data/band_edges_E4puf.json   copies of data/band_edges_E4.json, so both
  data/band_edges_E4scf.json   one-source models have the same band layout,
                               vocabulary and slots as model V4

Nothing else changes: same columns, same weights, same split column. The 2%
held-back slice of MINIMAL_ALL_RECORDS is drawn per source with a seed that
depends only on the source (02_model.held_back), so it holds exactly the
same records in each one-source table as in E4 (checked below).
"""
import importlib.util
import os
import shutil

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))


def main():
    t = pd.read_parquet(f'{ROOT}/data/stacked_E4.parquet')
    for s in ['puf', 'scf']:
        part = t[t['source'] == s].reset_index(drop=True)
        part.to_parquet(f'{ROOT}/data/stacked_E4{s}.parquet', index=False)
        shutil.copyfile(f'{ROOT}/data/band_edges_E4.json', f'{ROOT}/data/band_edges_E4{s}.json')
        print(s, len(part), 'rows written', flush=True)

    # Check: the held-back slice is the same records in E4 and in each part.
    os.environ['MINIMAL_TABLE'] = 'E4'
    spec = importlib.util.spec_from_file_location('m', os.path.join(HERE, '02_model.py'))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    full = m.held_back(t)
    for s in ['puf', 'scf']:
        part = t[t['source'] == s].reset_index(drop=True)
        same = np.array_equal(m.held_back(part), full[(t['source'] == s).to_numpy()])
        print(s, 'held-back slice identical to E4:', same, int(m.held_back(part).sum()), 'records')
        assert same


if __name__ == '__main__':
    main()
