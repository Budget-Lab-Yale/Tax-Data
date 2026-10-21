"""Part C development check for c_evaluate_puf.py: a fabricated completion
of the expB PUF test records in which each record takes the wealth of a
random SCF 2016 test record (drawn by weight, 2 draws). No link to income,
so holding by income group should come out flat; the overall shares should
match SCF 2016. Written to $S/dev/c/ (scratch only).

Usage: python c_make_dev_puf.py
"""
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from c_common import S, WEALTH, Dataset  # noqa: E402

ds = Dataset("expB", "test")
rec = ds.records
src = rec["source"].astype(str).to_numpy()
puf = np.flatnonzero(src == "PUF")
scf = np.flatnonzero((src == "SCF") & (rec["year"].to_numpy() == 2016))
p = rec["weight"].to_numpy()[scf]
p = p / p.sum()
rng = np.random.default_rng(5)
parts = []
for d in range(2):
    donor = scf[rng.choice(len(scf), size=len(puf), p=p)]
    out = rec.iloc[puf][["source", "year", "hh_id", "implicate", "unit", "weight"]].reset_index(drop=True)
    out["row"] = puf
    out["draw"] = d
    for v in WEALTH:
        out[v] = rec[v].to_numpy()[donor]
        out["tok_" + v] = ds.col(v)[donor]
    parts.append(out)
os.makedirs(os.path.join(S, "dev", "c"), exist_ok=True)
f = os.path.join(S, "dev", "c", "completed_pufshuffle_seed01.parquet")
pd.concat(parts, ignore_index=True).to_parquet(f)
print("wrote", f, len(puf), "PUF records")
