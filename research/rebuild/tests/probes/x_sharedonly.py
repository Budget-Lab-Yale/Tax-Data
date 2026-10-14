"""Probe X: datasets in which PUF records show only the variables the SCF
also measures (every PUF-only variable set to unobserved, token -1), for
completing PUF wealth from the shared variables alone. One for full_v2,
one for full_v3x; other files are symbolic links to the originals.

    python x_sharedonly.py
"""
import json
import os

import numpy as np
import pandas as pd

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x"
for tag, src in (("full_v2", f"{T2}/model/full_v2/data"), ("full_v3x", f"{Q}/model/full_v3x/data"), ("full_v3x2", f"{Q}/model/full_v3x2/data")):
    if os.path.exists(f"{Q}/data/{tag}_sharedonly/tokens_train.npy"):
        continue
    out = f"{Q}/data/{tag}_sharedonly"
    os.makedirs(out, exist_ok=True)
    meta = json.load(open(f"{src}/meta.json"))
    pufonly = [i for i, v in enumerate(meta["variables"]) if v["group"] == "puf_only"]
    for f in os.listdir(src):
        if f.startswith("tokens_train") or f.startswith("truth_train"):
            continue
        d = os.path.join(out, f)
        if not os.path.lexists(d):
            os.symlink(os.path.join(src, f), d)
    rec = pd.read_parquet(f"{src}/records_train.parquet", columns=["source"])
    puf = (rec.source == "PUF").to_numpy()
    for f in ("tokens_train.npy", "truth_train.npy"):
        t = np.load(f"{src}/{f}")
        t2 = t.copy()
        t2[np.ix_(puf, pufonly)] = -1
        np.save(f"{out}/{f}", t2)
    print(tag, "PUF-only variables hidden on", int(puf.sum()), "PUF rows:", [meta["variables"][i]["name"] for i in pufonly])
