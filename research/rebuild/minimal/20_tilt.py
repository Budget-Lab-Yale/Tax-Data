"""
20_tilt.py  (minimal example, step 20: the tilt of v4b applied to another stack)

Runs 07_baseline.main() unchanged on a stack that is not in gen/: the same
136 IRS targets for 2019 (exact), the same eight 2019 SCF wealth targets
from all units as dollar lines (each soft at 2%), the same start (importance
weights scaled to SOI's return count), the same V4 maps (map/V4/, which
15_maps.py showed equal to the v2 maps), the same redraw. As for v4b:
WEALTH_UNITS=all.

07_baseline.py reads its stack from gen/gen_puf_stack_<v>.parquet and
writes to tilt/<v>/; here its module-level paths are pointed elsewhere
before main() runs.

Usage: python 20_tilt.py <stack parquet> <meta json> <out dir> <tag>
"""
import importlib.util
import os
import sys

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))


def main():
    stack, meta, out, tag = sys.argv[1:5]
    os.environ['STACK_VERSION'] = tag
    os.environ['WEALTH_UNITS'] = 'all'
    os.environ['MINIMAL_MAP_DIR'] = f'{ROOT}/map/V4'
    spec = importlib.util.spec_from_file_location('base', os.path.join(HERE, '07_baseline.py'))
    base = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base)
    base.V, base.STACK, base.META, base.OUT = tag, stack, meta, out
    base.main()


if __name__ == '__main__':
    sys.exit(main())
