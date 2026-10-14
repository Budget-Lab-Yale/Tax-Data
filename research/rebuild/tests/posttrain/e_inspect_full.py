"""Piece E development: print the layout of the full dataset (no record-level output)."""
import json, pandas as pd, numpy as np
D="/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full/"
m=json.load(open(D+"meta.json"))
print(m.keys())
print([(v["name"], v.get("group"), v["kind"]) for v in m["variables"]])
r=pd.read_parquet(D+"records_train.parquet")
print(r.columns.tolist())
print(r["source"].value_counts())
print(r.groupby("source")[["E00200","E00900","E25850","P23250","E00100"]].apply(lambda d: d.notna().mean()))
t=np.load(D+"truth_train.npy"); print(t.shape, t.dtype)
print(m.get("generation_order"))
