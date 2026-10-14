"""Probe C: compare full_v2, BR's full_v3br and X's full_v3x2 (layout, row alignment)."""
import json, numpy as np, pandas as pd
D = {"v2": "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2",
     "br": "/nfs/roberts/scratch/pi_nrs36/jar335/probes/br/data/full_v3br",
     "x": "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x/data/full_v3x2"}
M = {k: json.load(open(f"{d}/meta.json")) for k, d in D.items()}
for k in D:
    print(k, len(M[k]["variable_names"]), M[k]["variable_names"] == M["v2"]["variable_names"])
for k in ("br", "x"):
    for a, b in zip(M["v2"]["variables"], M[k]["variables"]):
        diff = [kk for kk in set(a) | set(b) if a.get(kk) != b.get(kk)]
        if diff:
            print(k, a["name"], diff, {kk: (str(a.get(kk))[:150], str(b.get(kk))[:150]) for kk in diff if kk not in ("tokens", "binning")})
for p in ("train", "val", "test"):
    R = {k: pd.read_parquet(f"{d}/records_{p}.parquet") for k, d in D.items()}
    T = {k: np.load(f"{d}/tokens_{p}.npy") for k, d in D.items()}
    print(p, {k: R[k].shape for k in R}, {k: T[k].shape for k in T})
    print("columns v2", list(R["v2"].columns)[:20], "...")
    for k in ("br", "x"):
        print(k, "extra cols", set(R[k].columns) - set(R["v2"].columns), "missing", set(R["v2"].columns) - set(R[k].columns))
        idc = [c for c in R["v2"].columns if c in ("id", "record_id", "RECID", "unit_id", "source", "year", "weight")]
        for c in idc:
            a, b = R["v2"][c].to_numpy(), R[k][c].to_numpy()
            print("  ", c, "equal" if (len(a) == len(b) and (pd.Series(a).astype(str).values == pd.Series(b).astype(str).values).all()) else "DIFFER")
        puf = (R["v2"].source.astype(str) == "PUF").to_numpy()
        same = (T["v2"] == T[k])
        print("  token columns differing on PUF rows:", [M["v2"]["variable_names"][j] for j in range(T["v2"].shape[1]) if not same[puf, j].all()])
        print("  token columns differing on SCF rows:", [M["v2"]["variable_names"][j] for j in range(T["v2"].shape[1]) if not same[~puf, j].all()])
    if p == "train":
        r = R["br"]; puf = (r.source.astype(str) == "PUF").to_numpy()
        tb = T["br"]; names = M["v2"]["variable_names"]
        f = r.loc[puf, "FDED"].to_numpy()
        for c in ["E17500", "E18400", "E18500", "E19200", "E19800", "E20100"]:
            j = names.index(c)
            print(c, "PUF itemizers unobserved", (tb[puf][f == 1, j] < 0).mean(), "non-itemizers unobserved", (tb[puf][f != 1, j] < 0).mean(),
                  "SCF observed", (tb[~puf, j] >= 0).mean(), "records NaN nonitem", r.loc[puf, c].isna().to_numpy()[f != 1].mean())
        print(r.columns.tolist())
        print(r.groupby([r.source.astype(str), r.year]).size())
