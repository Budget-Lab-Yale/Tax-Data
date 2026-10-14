"""Probe UW: table of the tokens each PUF variable takes within each group of
sampling weights, from the PUF training records of `full_v2w`.

Weight groups: the distinct PUF weights (one PUFWT token each) sorted by
value, adjacent values merged until each group has at least --min records.
support[g, j, t] is true when some PUF training record in group g has token
t for slot variable j. Variables the PUF does not observe are left
unrestricted. Used by uw_complete.py --support. Writes only the table
(token sets by group), no records.

Usage: python uw_support.py <out npz> [--min N]
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/full_v2w")
p = argparse.ArgumentParser()
p.add_argument("out")
p.add_argument("--min", type=int, default=300)
a = p.parse_args()
meta = json.load(open(DATA / "meta.json"))
vs = meta["variables"]
slots = [j for j, v in enumerate(vs) if v.get("group") != "context"]
svars = [vs[j] for j in slots]
names = [v["name"] for v in svars]
tok = np.load(DATA / "tokens_train.npy")[:, slots].astype(np.int64)
rec = pd.read_parquet(DATA / "records_train.parquet", columns=["source"])
puf = (rec["source"].astype(str) == "PUF").to_numpy()
tok = tok[puf]
jw = names.index("PUFWT")
K = svars[jw]["vocab_size"]
vals = np.array([t["value"] for t in svars[jw]["tokens"]])
cnt = np.bincount(tok[:, jw], minlength=K)
order = np.argsort(vals)
group = np.empty(K, dtype=np.int64)
g, n = 0, 0
for k in order:
    group[k] = g
    n += cnt[k]
    if n >= a.min:
        g, n = g + 1, 0
if n > 0 and g > 0:            # last small group joins the previous one
    group[group == g] = g - 1
G = int(group.max()) + 1
max_v = max(int(v["vocab_size"]) for v in svars)
V = len(svars)
sup = np.ones((G, V, max_v), dtype=bool)
gr = group[tok[:, jw]]
for j in range(V):
    if (tok[:, j] < 0).all():
        continue               # not a PUF variable: unrestricted
    sup[:, j, :] = False
    for gg in range(G):
        t = np.unique(tok[gr == gg, j])
        sup[gg, j, t[t >= 0]] = True
np.savez(a.out, group=group, support=sup)
print(f"{G} weight groups (min {a.min} records)")
for gg in range(G):
    ks = np.flatnonzero(group == gg)
    print(f"group {gg}: weights {vals[ks].min():.2f}-{vals[ks].max():.2f}, records "
          f"{cnt[ks].sum():,}; AGI tokens allowed {sup[gg, names.index('E00100')].sum()} of "
          f"{svars[names.index('E00100')]['vocab_size']}")
