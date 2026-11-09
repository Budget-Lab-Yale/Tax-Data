"""The top stage: the part of the work on the top of the file that needs the networks, run on a GPU.

Builds the year's start as the reweighting does (scale/start.py). Then top_records.network_part adds Part A, the
rebuilt aggregate records of the PUF, and draws again every generated balance sheet of $1 billion or more. The result
is written to the top cache, which reweight/tilt.py reads. FEATURES.md, "The top of the file", describes the method.
"""
import os
import pickle

from shared import common
from scale import start as START
from reweight import target_list as TLS
from generate import top_records as TR
from shared.common import TARGET_YEAR as Y


def main():
    # 1. Build the start and do the part of the top that needs the networks.
    os.makedirs(common.TILT, exist_ok=True)
    spec = TLS.start_spec(Y)
    g19, raw, imp, _ = START.year_start(spec)
    base = TR.network_part(g19, raw, imp, Y, spec['mode'])
    # 2. Write the top cache, first under a temporary name and then renamed.
    with open(common.TOP_CACHE + '.part', 'wb') as fh:
        pickle.dump(base, fh, protocol=4)
    os.replace(common.TOP_CACHE + '.part', common.TOP_CACHE)
    common.say(f'the top step written to {common.TOP_CACHE}')


if __name__ == '__main__':
    main()
