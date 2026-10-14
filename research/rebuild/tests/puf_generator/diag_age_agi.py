"""Age range x AGI band, training records versus generated files (test 1).
Non-dependent filers only (age codes mean other ages for dependents).
Prints aggregate weighted shares only."""
import numpy as np
import pandas as pd

S = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator"
files = {"train": f"{S}/data/values_train.parquet",
         "small": f"{S}/model/synthetic_small.parquet",
         "medium": f"{S}/model/synthetic_base.parquet",
         "large": f"{S}/model/synthetic.parquet"}
edges = [-np.inf, 0, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, np.inf]
labels = ["<0", "0-10k", "10-25k", "25-50k", "50-75k", "75-100k", "100-200k",
          "200-500k", "500k-1m", "1m+"]
pd.set_option("display.width", 220, "display.max_columns", 30)

tabs = {}
for k, f in files.items():
    d = pd.read_parquet(f)
    d = d[d.DSI == 0]
    d["agi"] = pd.cut(d.E00100, edges, labels=labels, right=False)
    d["age"] = d.AGERANGE.map(lambda a: "none" if a == -1 else str(int(a)))
    t = d.pivot_table(index="age", columns="agi", values="weight", aggfunc="sum",
                      observed=False).fillna(0)
    tabs[k] = t / t.values.sum()

print("Non-dependent filers. Rows: age-range code (1 youngest .. 6 oldest; 0 'not present';"
      " none = not in demographic file). Columns: AGI band.\n")
print("== TRAINING: % of all weight in each cell")
print((100 * tabs["train"]).round(2).to_string())
for k in ["small", "medium", "large"]:
    diff = 100 * (tabs[k] - tabs["train"])
    tvd = 0.5 * (tabs[k] - tabs["train"]).abs().values.sum()
    print(f"\n== {k.upper()} minus TRAINING, percentage points (TVD {tvd:.4f})")
    print(diff.round(2).to_string())

print("\n== Age mix WITHIN each AGI band (% of the band), training vs large")
for k in ["train", "large"]:
    col = tabs[k] / tabs[k].sum(axis=0)
    print(f"\n-- {k}")
    print((100 * col).round(1).to_string())
