"""Probe Y, step 4: markdown from y_completion_tables_<variant>.csv: completed
half 1 wealth from test 2's model trained on untranslated data and on
translated data, against the truth (means over three training seeds, with
the range)."""
import os
import sys

import numpy as np
import pandas as pd

Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/y"
variant = sys.argv[1]
t = pd.read_csv(os.path.join(Q, f"y_completion_tables_{variant}.csv"))
t["run"] = t.method.str.replace(r"_seed\d\d$", "", regex=True)
GROUPS = ["Bottom 20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-99%", "Top 1%"]
RUNS = [r for r in ["untranslated_asSCF_plain", "translated_asSCF_plain", "translated_random_asSCF_plain",
                    "untranslated_plain", "translated_plain", "translated_random_plain"] if (t.run == r).any()]
out = []


def cell(table, group, stat, run, scale=1000, fmt="{:,.0f}"):
    v = t[(t.run == run) & (t.table == table) & (t.group == group) & (t.stat == stat)].value / scale
    if run == "truth":
        return fmt.format(v.iloc[0])
    return fmt.format(v.mean()) + " (" + fmt.format(v.min()) + "–" + fmt.format(v.max()) + ")"


for stat, lab in (("mean_nw", "Mean net worth"), ("median_nw", "Median net worth")):
    out.append(f"{lab}, thousands of dollars, half 1 by its own income groups: mean over three training seeds (range).")
    out.append("")
    out.append("| Income group | truth | " + " | ".join(RUNS) + " |")
    out.append("|---|---|" + "---|" * len(RUNS))
    for g in GROUPS:
        out.append(f"| {g} | {cell('income_group', g, stat, 'truth')} | " + " | ".join(cell('income_group', g, stat, r) for r in RUNS) + " |")
    out.append("")
out.append("Mean net worth (thousands) by whether the income is reported in half 1.")
out.append("")
out.append("| Income | Reported? | truth | " + " | ".join(RUNS) + " |")
out.append("|---|---|---|" + "---|" * len(RUNS))
for i in ("int_div", "capital_gains"):
    for g, gl in (("nonzero", "yes"), ("zero", "no")):
        out.append(f"| {i} | {gl} | {cell('status_' + i, g, 'mean_nw', 'truth')} | " + " | ".join(cell('status_' + i, g, 'mean_nw', r) for r in RUNS) + " |")
out.append("")
out.append("| | truth | " + " | ".join(RUNS) + " |")
out.append("|---|---|" + "---|" * len(RUNS))
out.append("| Top 1% by income: percent of net worth | " + cell("top1", "Top 1% by income", "share_of_networth", "truth", 0.01, "{:.1f}") + " | " +
           " | ".join(cell("top1", "Top 1% by income", "share_of_networth", r, 0.01, "{:.1f}") for r in RUNS) + " |")
out.append("")
# error summary per seed, then mean
out.append("Mean absolute error against the truth across the seven income groups (percent), mean over seeds (range).")
out.append("")
out.append("| Run | Mean net worth | Median net worth | Mean net worth by interest/dividend and capital gain reporting (4 cells) |")
out.append("|---|---|---|---|")


def truth(table, group, stat):
    return t[(t.run == "truth") & (t.table == table) & (t.group == group) & (t.stat == stat)].value.iloc[0]


for r in RUNS:
    es = []
    for m in sorted(t[t.run == r].method.unique()):
        def v(table, group, stat):
            return t[(t.method == m) & (t.table == table) & (t.group == group) & (t.stat == stat)].value.iloc[0]
        e1 = np.mean([abs(v("income_group", g, "mean_nw") / truth("income_group", g, "mean_nw") - 1) for g in GROUPS]) * 100
        e2 = np.mean([abs(v("income_group", g, "median_nw") / truth("income_group", g, "median_nw") - 1) for g in GROUPS]) * 100
        e3 = np.mean([abs(v("status_" + i, g, "mean_nw") / truth("status_" + i, g, "mean_nw") - 1)
                      for i in ("int_div", "capital_gains") for g in ("zero", "nonzero")]) * 100
        es.append((e1, e2, e3))
    es = np.array(es)
    out.append(f"| {r} | " + " | ".join(f"{es[:, k].mean():.0f} ({es[:, k].min():.0f}–{es[:, k].max():.0f})" for k in range(3)) + " |")
open(os.path.join(Q, f"y_completion_summary_{variant}.md"), "w").write("\n".join(out) + "\n")
print("\n".join(out))
