"""Probe Y, step 4: completed wealth for half 1 against the truth, for test
2's joint model trained on untranslated data and the same model retrained
on translated data (y_prepare.py, y_pipeline.sbatch).

    python y_eval_completion.py unrep
Writes $Q/y_completion_tables_<variant>.csv (same layout as y_run's tables).
"""
import glob
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import y_run as yr  # noqa: E402

T2M = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model"
Q = yr.Q
variant = sys.argv[1]
h1, _ = yr.load(f"expA_{variant}")
h1 = yr.add_columns(h1)
rows = yr.tables(h1, "truth")
runs = {}
for f in sorted(glob.glob(os.path.join(T2M, f"expA_{variant}", "completed_joint_w1_*plain_seed*.parquet"))):
    runs["untranslated_" + os.path.basename(f).replace("completed_joint_w1_", "").replace(".parquet", "")] = f
for folder, label in ((f"expA_{variant}tr", "translated_"), (f"expA_{variant}trrandom", "translated_random_")):
    for f in sorted(glob.glob(os.path.join(Q, "model", folder, "completed_joint_w1_*plain_seed*.parquet"))):
        runs[label + os.path.basename(f).replace("completed_joint_w1_", "").replace(".parquet", "")] = f
for name, f in runs.items():
    c = pd.read_parquet(f)
    nd = c.draw.nunique()
    parts = []
    for d, g in c.groupby("draw"):
        g = g.sort_values("row")
        assert len(g) == len(h1) and (g.row.to_numpy() == np.arange(len(h1))).all()
        x = h1.copy()
        for w in yr.WEALTH:
            x[w] = g[w].to_numpy(float)
        x["weight"] = x.weight / nd
        parts.append(x)
    x = pd.concat(parts, ignore_index=True)
    # net worth from completed values, compared only where the truth exists
    obs = x.wealth_observed.astype(bool)
    x["networth"] = x[yr.ASSETS].sum(axis=1) - x[yr.DEBTS].sum(axis=1)
    x.loc[~obs, "networth"] = np.nan
    rows += yr.tables(x, name)
    print(name, nd, "draws", flush=True)
tab = pd.DataFrame(rows, columns=["method", "table", "group", "stat", "value"])
tab.insert(0, "variant", variant)
tab.to_csv(os.path.join(Q, f"y_completion_tables_{variant}.csv"), index=False)
t = tab[(tab.table == "income_group") & (tab.stat.isin(["mean_nw", "median_nw"]))]
print(t.pivot_table(index=["stat", "group"], columns="method", values="value").round(-3).to_string())
t = tab[tab.table.str.startswith("status_int") | tab.table.str.startswith("status_cap")]
print(t[t.stat == "mean_nw"].pivot_table(index=["table", "group"], columns="method", values="value").round(-3).to_string())
print(tab[tab.table == "top1"].set_index("method").value.round(3).to_string())
