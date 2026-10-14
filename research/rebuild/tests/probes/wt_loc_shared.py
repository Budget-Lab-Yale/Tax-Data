"""Probe WT (locate step): a copy of probe C's full_v3c in which PUF training
records show only variables the SCF also observes (every variable observed on
PUF rows only is set to unobserved, token -1, on PUF rows), for completing PUF
wealth from the shared variables alone (as probe X's x_sharedonly.py did).
Other files are symbolic links to the originals.

    python wt_loc_shared.py
"""
import json
import os

import numpy as np
import pandas as pd

SRC = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/full_v3c"
OUT = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt/locate/data/full_v3c_sharedonly"
os.makedirs(OUT, exist_ok=True)
meta = json.load(open(f"{SRC}/meta.json"))
pufonly = [v["column"] for v in meta["variables"] if v.get("observed_in") == ["PUF"]]
print("hidden on PUF rows:", [v["name"] for v in meta["variables"] if v.get("observed_in") == ["PUF"]])
for f in os.listdir(SRC):
    if f.startswith("tokens_train") or f.startswith("truth_train"):
        continue
    d = os.path.join(OUT, f)
    if not os.path.lexists(d):
        os.symlink(os.path.join(SRC, f), d)
rec = pd.read_parquet(f"{SRC}/records_train.parquet", columns=["source"])
puf = (rec.source == "PUF").to_numpy()
for kind in ("tokens", "truth"):
    a = np.load(f"{SRC}/{kind}_train.npy")
    for c in pufonly:
        a[puf, c] = -1
    np.save(f"{OUT}/{kind}_train.npy", a)
print("written", OUT)
