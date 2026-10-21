"""Probe ST: probe Q's SCF set-up (q_common.py, four target families:
income group, age range, filing status, children under 17), pointed at
probe ST's scratch folder. Records are the SCF rows with wealth observed of
the version 2 data (both waves, as probe Q; the 2016 rows are the ones
evaluated). A "variant" changes the weights of the 2016 rows to make a
harder stretch with a known truth (see harden()).
"""
import os
import sys
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import q_common as QC          # noqa: E402

ST = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/st")
QC.Q = ST
QC.QA = ST / "a"
FULL = QC.BEST.parent                       # test 3's version 2 model folder
FULL_CKPT = {s: FULL / f"checkpoint_full_v2_w1_seed0{s}.pt" for s in (1, 2, 3)}
HELD_CKPT = {s: ST / "model" / f"checkpoint_no2016_seed0{s}.pt" for s in (1, 2, 3)}
Y16 = 2016


def setup(ckpt=None):
    return QC.setup(ckpt)


def harden(r, mult):
    """Harder stretch with a known truth: multiply the weight of every 2016
    row whose true net worth is in the top 1 percent of its income group
    (weighted, 2016 rows of the split) by `mult`. Rows of 2013 unchanged.
    The changed records are the truth of a 'future' year in which wealth is
    more concentrated within income groups than in 2016."""
    r = r.copy()
    w = r["weight"].to_numpy(float).copy()
    y = r["year"].to_numpy()
    gi = r["g_inc"].to_numpy()
    nw = r["nw"].to_numpy(float)
    for g in np.unique(gi):
        m = np.flatnonzero((y == Y16) & (gi == g))
        o = m[np.argsort(-nw[m])]
        cw = np.cumsum(w[o])
        top = o[cw <= 0.01 * cw[-1]]
        w[top] *= mult
    r["weight"] = w
    return r


def as_2016(r13):
    """2013 records relabelled as 2016 records (wave, year, target blocks),
    for the reweighted-2013 comparator. Call after setup()."""
    L = QC.A.layout()
    r = r13.copy()
    off = {}
    for b in L.blocks.itertuples():
        off.setdefault((b.wave_i, b.family), b.block - b.group)
    for f in QC.FAMILIES:
        r["blk_" + f] = r["blk_" + f] - off[(0, f)] + off[(1, f)]
    r["wave_i"] = 1
    r["year"] = 2016
    return r


def records(split, variant=None):
    r = QC.records(split)
    if variant is None:
        return r
    fn = QC.QA / f"records_{split}_{variant}.parquet"
    if fn.exists():
        return pd.read_parquet(fn)
    assert variant.startswith("top")
    r = harden(r, float(variant[3:]))
    tmp = fn.with_suffix(f".tmp{os.getpid()}")       # several jobs may build it at once
    r.to_parquet(tmp)
    os.replace(tmp, fn)
    return r


def targets(split, variant=None):
    if variant is None:
        return QC.targets(split)
    fn = QC.QA / f"targets_{split}_{variant}.parquet"
    if fn.exists():
        return pd.read_parquet(fn)
    t = QC.A.targets(records(split, variant))
    tmp = fn.with_suffix(f".tmp{os.getpid()}")
    t.to_parquet(tmp)
    os.replace(tmp, fn)
    return t
