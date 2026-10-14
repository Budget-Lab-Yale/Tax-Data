"""Undo the tilt of the damped-weight model (hypothesis 4, EXPERIMENTS.md).
A model trained with record i's loss scaled by w_i^p / mean(w^p) learns to
generate records in proportion to (sample share) x w^p / mean(w^p). Each
generated record's weight is therefore divided by its loss weight:
new weight = w x mean_train(w^p) / w^p. Prints aggregates only.
With a fourth argument "snap", each new weight is moved to the nearest (in
logs) of the weight values found in the training records, so that the
classifier test is not decided by off-grid weights alone.
Usage: python exp_reweight.py <in.parquet> <out.parquet> <p> [snap]"""
import numpy as np
import sys
import pandas as pd
S = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator"
src, dst, p = sys.argv[1], sys.argv[2], float(sys.argv[3])
tw = pd.read_parquet(f"{S}/data/values_train.parquet", columns=["weight"]).weight
m = (tw ** p).mean()
d = pd.read_parquet(src)
assert (d.weight > 0).all() and d.weight.notna().all()
d["weight"] = d.weight * m / d.weight ** p
if len(sys.argv) > 4 and sys.argv[4] == "snap":
    grid = np.log(np.sort(tw.unique()))
    lw = np.log(d.weight.to_numpy())
    i = np.clip(np.searchsorted(grid, lw), 1, len(grid) - 1)
    pick = np.where(np.abs(lw - grid[i - 1]) <= np.abs(lw - grid[i]), i - 1, i)
    d["weight"] = np.exp(grid[pick])
    d["weight"] = d.weight.map(dict(zip(np.exp(grid), np.sort(tw.unique()))))
    assert d.weight.notna().all()
d.to_parquet(dst, index=False)
print(f"mean training w^{p} = {m:.4f}; total weight before/after relative to training: "
      f"{pd.read_parquet(src, columns=['weight']).weight.sum() / tw.sum():.4f} / {d.weight.sum() / tw.sum():.4f}")
