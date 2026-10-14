"""Age range x AGI band for every generated file of the noise/order batch
(test 1, diagnostics 2). Non-dependent filers only, as in diag_age_agi.py.

For each file: the share of weight in the wrong age x AGI cell against the
training records (total variation distance, TVD = half the sum of absolute
differences in cell shares), the same for the age mix alone, the share of
the oldest age range with wages, and seed-to-seed TVDs between files from
the same model. Writes aggregate CSVs to $S/diag2/; prints aggregates only.
"""
import itertools
from pathlib import Path

import numpy as np
import pandas as pd

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
OUT = S / "diag2"
OUT.mkdir(exist_ok=True)
M = S / "model"
files = {"train": S / "data/values_train.parquet", "test": S / "data/values_test.parquet",
         "val": S / "data/values_val.parquet"}
for m, s09 in (("small", "synthetic_small"), ("base", "synthetic_base"), ("large", "synthetic")):
    files[f"{m}_s09"] = M / f"{s09}.parquet"
    for s in (10, 11, 12):
        files[f"{m}_s{s}"] = M / f"synthetic_{m}_s202610{s}.parquet"
files["base_demoglast"] = M / "synthetic_base_demoglast.parquet"
files["base_gibbs2"] = M / "synthetic_base_gibbs2.parquet"
import sys
for arg in sys.argv[1:]:          # extra files as label=path
    k, v = arg.split("=", 1)
    files[k] = Path(v)

edges = [-np.inf, 0, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, np.inf]
labels = ["<0", "0-10k", "10-25k", "25-50k", "50-75k", "75-100k", "100-200k",
          "200-500k", "500k-1m", "1m+"]
ages = [-1, 0, 1, 2, 3, 4, 5, 6]

tabs, rows = {}, []
for k, f in files.items():
    d = pd.read_parquet(f, columns=["weight", "DSI", "AGERANGE", "E00100", "E00200"])
    tot_all = d.weight.sum()
    d = d[d.DSI == 0]
    band = pd.cut(d.E00100, edges, labels=labels, right=False)
    t = pd.crosstab(d.AGERANGE, band, values=d.weight, aggfunc="sum", dropna=False) \
        .reindex(index=ages, columns=labels).fillna(0)
    tabs[k] = t / t.values.sum()
    old = d[d.AGERANGE == 6]
    rows.append(dict(file=k, records=len(d), nondep_weight_share=d.weight.sum() / tot_all,
                     oldest_with_wages=np.average(old.E00200 != 0, weights=old.weight)))

res = pd.DataFrame(rows).set_index("file")
for ref in ("train", "test"):
    res[f"tvd_agexagi_vs_{ref}"] = [0.5 * np.abs(tabs[k] - tabs[ref]).values.sum() for k in res.index]
    res[f"tvd_age_vs_{ref}"] = [0.5 * np.abs(tabs[k].sum(1) - tabs[ref].sum(1)).sum() for k in res.index]
res.to_csv(OUT / ("agexagi_by_file.csv" if len(sys.argv) == 1 else "agexagi_by_file_extra.csv"))

pairs = []
for m in ("small", "base", "large"):
    for a, b in itertools.combinations([f"{m}_s{s:02d}" for s in (9, 10, 11, 12)], 2):
        pairs.append(dict(model=m, a=a, b=b, tvd_agexagi=0.5 * np.abs(tabs[a] - tabs[b]).values.sum(),
                          tvd_age=0.5 * np.abs(tabs[a].sum(1) - tabs[b].sum(1)).sum()))
pairs.append(dict(model="real", a="train", b="test",
                  tvd_agexagi=0.5 * np.abs(tabs["train"] - tabs["test"]).values.sum(),
                  tvd_age=0.5 * np.abs(tabs["train"].sum(1) - tabs["test"].sum(1)).sum()))
pairs.append(dict(model="real", a="train", b="val",
                  tvd_agexagi=0.5 * np.abs(tabs["train"] - tabs["val"]).values.sum(),
                  tvd_age=0.5 * np.abs(tabs["train"].sum(1) - tabs["val"].sum(1)).sum()))
pairs = pd.DataFrame(pairs)
if len(sys.argv) == 1:
    pairs.to_csv(OUT / "agexagi_seed_pairs.csv", index=False)

# Cell differences, percentage points, against training: base seed 09 and
# the two order variants.
for k in ("base_s09", "base_demoglast", "base_gibbs2", "large_s09"):
    (100 * (tabs[k] - tabs["train"])).round(3).to_csv(OUT / f"agexagi_diff_vs_train_{k}.csv")

pd.set_option("display.width", 220)
print(res.round(4).to_string())
print(pairs.round(4).to_string())
for k in ["base_s09", "base_demoglast", "base_gibbs2"] + [a.split("=")[0] for a in sys.argv[1:]]:
    print(f"\n{k} minus train, percentage points")
    print((100 * (tabs[k] - tabs["train"])).round(2).to_string())
