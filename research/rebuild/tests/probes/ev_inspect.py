"""Probe EV: column and row checks on probe C's files (aggregates only)."""
import json
import numpy as np
import pandas as pd
C3 = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3"
D = f"{C3}/data/full_v3cz"
for sp in ("train", "test"):
    r = pd.read_parquet(f"{D}/records_{sp}.parquet")
    print(sp, len(r), list(r.columns))
    print(r.groupby([r.source.astype(str), "year"]).size())
    s = r[r.source.astype(str) == "SCF"]
    print("SCF wealth_observed", s.wealth_observed.value_counts(dropna=False).to_dict())
    for c in ["MARS", "AGERANGE", "age", "N24", "XTOT", "E00200", "t2_wages", "primary_home"]:
        if c in r:
            print(c, "SCF NA", int(s[c].isna().sum()), "describe", s[c].describe().round(1).to_dict())
f = pd.read_parquet(f"{C3}/model_cz/h_scf_test_seed01.parquet")
print("scf_test cols", list(f.columns), len(f), f.draw.nunique(), f.row.min(), f.row.max(), f.row.nunique())
rt = pd.read_parquet(f"{D}/records_test.parquet")
scf_rows = np.flatnonzero((rt.source.astype(str) == "SCF").to_numpy())
print("SCF rows in test", len(scf_rows), scf_rows.min(), scf_rows.max(), "file rows subset", np.isin(f.row.unique(), scf_rows).all())
sub = rt.iloc[f.row.unique()]
print("file rows wealth_observed", sub.wealth_observed.value_counts(dropna=False).to_dict())
for c in ["source", "year", "weight", "age", "MARS"]:
    if c in f:
        print(c, "in file; matches records", np.allclose(f.drop_duplicates("row")[c].astype(float).to_numpy() if c not in ("source",) else 0,
              rt.iloc[f.drop_duplicates("row").row][c].astype(float).to_numpy() if c not in ("source",) else 0, equal_nan=True))
g = pd.read_parquet(f"{C3}/model_cz/h_puf_wealth_seed01.parquet", columns=None)
print("puf_wealth cols", list(g.columns)[:60], len(g), g.draw.nunique(), g.row.min(), g.row.max())
h = pd.read_parquet(f"{C3}/gen/h_cz_random_s1_seed01.parquet")
print("gen cols", list(h.columns), len(h))
m = json.load(open(f"{D}/meta.json"))
print("meta keys", list(m.keys()))
print([v["name"] for v in m["variables"]])
