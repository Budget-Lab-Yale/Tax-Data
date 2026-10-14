"""Probe BR: how full_v2 codes the itemized deductions for PUF non-itemizers."""
import pandas as pd, numpy as np
D = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2/"
r = pd.concat([pd.read_parquet(D + f"records_{p}.parquet") for p in ("train", "val", "test")], ignore_index=True)
p = r[r.source == "PUF"]
w = p.weight
print("PUF records", len(p), "weighted returns (m)", w.sum() / 1e6)
for f in (1, 2, 3):
    m = p.FDED == f
    print(f"FDED {f}: records {m.sum()}, weighted share {w[m].sum() / w.sum():.3f}")
for v in ["E19200", "E18500", "E19800", "E20100", "E17500", "E18400"]:
    for f in (1, 2, 3):
        m = p.FDED == f
        x = p.loc[m, v]
        print(f"{v} FDED {f}: NaN {x.isna().sum()}, nonzero weighted share {np.sum(w[m] * (x != 0)) / w[m].sum():.3f}, "
              f"total $bn {np.sum(w[m] * x.fillna(0)) / 1e9:.1f}")
m = p.FDED == 1
for v in ["E19200", "E18500", "E19800", "E20100"]:
    x = p.loc[m, v]; ww = w[m]; nz = x > 0
    o = np.argsort(x[nz].to_numpy()); xs = x[nz].to_numpy()[o]; c = np.cumsum(ww[nz].to_numpy()[o]); c /= c[-1]
    print(v, "itemizers positive: min", xs[0], "weighted quantiles 10/50/90/99:", [round(xs[np.searchsorted(c, q)]) for q in (.1, .5, .9, .99)])
