"""Paths and settings shared by every stage.

Every choice of the current run is in settings.json. Outputs go under OUT
(scratch), one folder per stage:
  data/     the training table, its band edges, the dollar maps   (build_table.py)
  targets/  the published IRS and SCF numbers the tilt uses       (targets.py)
  model/    the trained network                                    (train.py)
  gen/      the generated records                                  (generate.py)
  tilt/     the 2019 weights and the final file                    (tilt.py)
REBUILD_OUT overrides OUT; REBUILD_SEED the training seed; REBUILD_PASSES the passes (for a quick test).
"""
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))
SETTINGS = json.load(open(os.path.join(HERE, 'settings.json')))
OUT = os.environ.get('REBUILD_OUT', SETTINGS['paths']['out'])
RAW = SETTINGS['paths']['raw']
SEED = int(os.environ.get('REBUILD_SEED', SETTINGS['training']['seed']))
PASSES = int(os.environ.get('REBUILD_PASSES', SETTINGS['training']['passes']))   # fewer for a quick test

DATA = f'{OUT}/data'
TABLE = f'{DATA}/table.parquet'
EDGES = f'{DATA}/band_edges.json'
MAPS = f'{DATA}/maps'
TARGETS = f'{OUT}/targets'
MODEL = f'{OUT}/model/seed{SEED}'
GEN = f'{OUT}/gen/seed{SEED}'
TILT = f'{OUT}/tilt/seed{SEED}'


def say(*parts):
    print(*parts, flush=True)
