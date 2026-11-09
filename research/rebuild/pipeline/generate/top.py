"""The top stage: the part of the top of the file that needs the network, run on a GPU.

Builds the year's start as the reweighting does (scale/start.py), adds Part A's rebuilt hidden returns and redraws
balance sheets at $1 billion or more (top_records.network_part), and writes the result to the top cache, which
reweight/tilt.py reads.
"""
import os
import pickle

from shared import common
from scale import start as START
from reweight import target_list as TLS
from generate import top_records as TR
from shared.common import TARGET_YEAR as Y


def main():
    os.makedirs(common.TILT, exist_ok=True)
    spec = TLS.start_spec(Y)
    g19, raw, imp, _ = START.year_start(spec)
    base = TR.network_part(g19, raw, imp, Y, spec['mode'])
    with open(common.TOP_CACHE + '.part', 'wb') as fh:
        pickle.dump(base, fh, protocol=4)
    os.replace(common.TOP_CACHE + '.part', common.TOP_CACHE)
    common.say(f'the top step written to {common.TOP_CACHE}')


if __name__ == '__main__':
    main()
