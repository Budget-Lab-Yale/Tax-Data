"""Probe BL: the reference point of pure copying. Draws 664,624 PUF training
records with replacement, with probability proportional to weight, and
writes them in probe AS's generated-file format (equal weights). A generator
that only memorized the training records would score like this; the spread
over its two seeds is the sampling noise of a 664,624-record file.

    python bl_boot.py --seeds 1 2
"""
import argparse
import json
import os

import numpy as np
import pandas as pd

DATA = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s"
OUT = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/gen"
N = 664_624

p = argparse.ArgumentParser()
p.add_argument("--seeds", type=int, nargs="+", default=[1, 2])
a = p.parse_args()
meta = json.load(open(os.path.join(DATA, "meta.json")))
pv = [v for v in meta["variables"] if v["group"] in ("shared", "puf_only")]
rec = pd.read_parquet(os.path.join(DATA, "records_train.parquet"))
puf = (rec["source"].astype(str) == "PUF").to_numpy()
tr = rec[puf].reset_index(drop=True)
tok = np.load(os.path.join(DATA, "tokens_train.npy"))[puf]
w = tr["weight"].to_numpy(float)
for s in a.seeds:
    rng = np.random.default_rng(777 + s)
    idx = rng.choice(len(tr), size=N, p=w / w.sum())
    df = pd.DataFrame({"gen_id": np.arange(N), "source": "PUF", "year": 2015,
                       "weight": np.full(N, w.sum() / N), "draw": np.zeros(N, dtype=np.int64)})
    for v in pv:
        df[v["name"]] = tr[v["name"]].to_numpy(float)[idx]
        df["tok_" + v["name"]] = tok[idx, v["column"]].astype(np.int32)
    fn = os.path.join(OUT, f"bl_bootstrap_s{s}.parquet")
    df.to_parquet(fn, index=False)
    print("wrote", fn, "distinct training records", len(np.unique(idx)), flush=True)
