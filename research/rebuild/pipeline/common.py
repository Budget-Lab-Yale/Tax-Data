"""Paths, settings and small helpers shared by every stage.

Every choice of a run is in one settings file: REBUILD_SETTINGS if set (run.sbatch sets it; every stage, R and
Python, reads it), else the pipeline's own settings.json. A variant run is a copy of that file. Outputs go under OUT
(scratch), one folder per stage:
  data/     the training table, its band edges, the dollar maps   (build_table.py)
  targets/  the published SOI numbers                              (targets.py)
  model/    the trained network                                    (train.py)
  gen/      the generated records                                  (generate.py)
  tilt/     the target year's weights and the final file           (tilt.py)
Environment: REBUILD_OUT overrides OUT; REBUILD_SEED the training seed; REBUILD_RUN names the model/gen/tilt
subfolder (default seed<n>); REBUILD_MODEL_RUN generates from another run's model; REBUILD_PASSES the passes
(for a quick test).
"""
import json
import os

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
SETTINGS = json.load(open(os.environ.get('REBUILD_SETTINGS', os.path.join(HERE, 'settings.json'))))
OUT = os.environ.get('REBUILD_OUT', SETTINGS['paths']['out'])
RAW = SETTINGS['paths']['raw']
SEED = int(os.environ.get('REBUILD_SEED', SETTINGS['training']['seed']))
PASSES = int(os.environ.get('REBUILD_PASSES', SETTINGS['training']['passes']))   # fewer for a quick test
PUF_YEAR = int(SETTINGS['years']['puf'])          # the PUF's tax year: its records, cut points and dollar maps
TARGET_YEAR = int(SETTINGS['years']['target'])    # the year of the file the tilt makes, and of its targets

DATA = f'{OUT}/data'
TABLE = f'{DATA}/table.parquet'
EDGES = f'{DATA}/band_edges.json'
MAPS = f'{DATA}/maps'
TARGETS = f'{OUT}/targets'
RUN = os.environ.get('REBUILD_RUN', f'seed{SEED}')
MODEL = f'{OUT}/model/{os.environ.get("REBUILD_MODEL_RUN", RUN)}'
GEN = f'{OUT}/gen/{RUN}'
TILT = f'{OUT}/tilt/{RUN}'

# The primary filer's age range (ages.py): six adult ranges, then three for dependent filers.
AGE_LABELS = ['under 26', '26-34', '35-44', '45-54', '55-64', '65+', 'dep <18', 'dep 18-25', 'dep 26+']
ADULT_RANGES = 6
N_RANGES = len(AGE_LABELS)
DEPENDENT_RANGES = list(range(ADULT_RANGES, N_RANGES))

# Wage groups for reports, lower edge inclusive: $25,000 is in $25k-50k.
WAGE_GROUPS = [(-np.inf, 1, 'none'), (1, 25e3, '$1-25k'), (25e3, 50e3, '$25k-50k'), (50e3, 100e3, '$50k-100k'),
               (100e3, 200e3, '$100k-200k'), (200e3, np.inf, '$200k+')]


def groups_by(x, lines):
    """Labels of dollar ranges: lines = [(lo, hi, label)], lo inclusive."""
    out = np.full(len(x), '', dtype=object)
    for lo, hi, lab in lines:
        out[(x >= lo) & (x < hi)] = lab
    return out


def wage_group(wages):
    return groups_by(wages, WAGE_GROUPS)


_EDGES = None


def edges():
    """The band edges written by build_table.py (read once)."""
    global _EDGES
    if _EDGES is None:
        _EDGES = json.load(open(EDGES))
    return _EDGES


def meta():
    """The band edges' meta: items, computed totals, the surveys' slots, AGI signs (build_table.py)."""
    return edges()['meta']


def wquantile(x, w, probs):
    """Smallest x whose cumulative weight share reaches each probability."""
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)]


def say(*parts):
    print(*parts, flush=True)
