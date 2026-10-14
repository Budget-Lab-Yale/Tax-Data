"""Probe AS: list the 40 PUF-side variables of full_v2 with descriptions and
weighted totals on PUF training records (aggregates only)."""
import json
import numpy as np
import pandas as pd
D = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data/"
m = json.load(open(D + "meta.json"))
r = pd.read_parquet(D + "records_train.parquet")
p = r[r.source.astype(str) == "PUF"]
s = r[r.source.astype(str) == "SCF"]
w = p.weight.to_numpy(float)
print("records cols:", list(r.columns))
print("generation_order:", m["generation_order"])
for v in m["variables"]:
    n = v["name"]
    if v["group"] in ("scf_only",):
        continue
    tot = neg = nz = None
    if n in p.columns and v["kind"] == "amount":
        x = p[n].to_numpy(float)
        tot = np.nansum(w * x) / 1e9; neg = np.nansum(w * np.minimum(x, 0)) / 1e9
        nz = np.sum(w * (x != 0)) / w.sum()
    scfobs = (s[n].notna().mean() if n in s.columns else None)
    print(f"{n:10s} {v['kind']:11s} {v['group']:9s} vocab={v['vocab_size']:3d} tot={tot} neg={neg} nz={nz} scf_obs={scfobs} | {v.get('description','')}")
print("other meta keys:", list(m.keys()))
print("E00100 meta:", {k: v for k, v in [x for x in m['variables'] if x['name']=='E00100'][0].items() if k != 'tokens'})
