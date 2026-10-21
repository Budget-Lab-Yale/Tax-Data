"""Probe Y: turn y_tables_<variant>.csv and y_movement_<variant>.json into
markdown tables ($Q/y_summary.md). Weighted aggregates only."""
import json
import os
import sys

import numpy as np
import pandas as pd

Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/y"
BASE = ["clean", "noise", "recat", "unrep", "unrep_poor", "unrep_rich", "svy_unrep", "svy_unrep_poor", "svy_unrep_rich"]
VARIANTS = [v for v in [b + suf for suf in ("_self", "") for b in BASE]
            if os.path.exists(os.path.join(Q, f"y_tables_{v}.csv"))]
METHODS = ["truth", "none", "translated", "reweighted", "oracle"]
ALL_METHODS = ["none", "translated", "translated_rank", "translated_random", "translated_likely_on_random_off", "reweighted", "oracle"]
GROUPS = ["Bottom 20%", "20-40%", "40-60%", "60-80%", "80-90%", "90-99%", "Top 1%"]
INC = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens", "ui_other"]

out = []


def p(s=""):
    out.append(s)


def k(x):
    return f"{x / 1000:,.0f}"


def pct(x):
    return f"{100 * x:.1f}"


tabs = {v: pd.read_csv(os.path.join(Q, f"y_tables_{v}.csv")) for v in VARIANTS}


def get(t, method, table, group, stat):
    r = t[(t.method == method) & (t.table == table) & (t.group == group) & (t.stat == stat)]
    return float(r.value.iloc[0]) if len(r) else np.nan


# Error summary across variants
p("## Error against the truth, all variants")
p()
p("Mean absolute error across the seven income groups: mean and median net worth in percent of the truth; "
  "shares holding stocks, a business and bonds in percentage points. Status cells: mean net worth by "
  "zero or nonzero status of each of the seven incomes (14 cells), percent. Top 1%: share of net worth "
  "held by the top 1% by income, percentage points.")
p()
p("| Variant | Method | Mean net worth (%) | Median net worth (%) | Holding shares (points) | Status cells, mean net worth (%) | Top 1% share (points) |")
p("|---|---|---|---|---|---|---|")
for v in VARIANTS:
    t = tabs[v]
    for m in ALL_METHODS:
        if not (t.method == m).any() or (m == "oracle" and v.endswith("_self")):
            continue
        e_mean = np.mean([abs(get(t, m, "income_group", g, "mean_nw") / get(t, "truth", "income_group", g, "mean_nw") - 1) for g in GROUPS]) * 100
        e_med = np.mean([abs(get(t, m, "income_group", g, "median_nw") / get(t, "truth", "income_group", g, "median_nw") - 1) for g in GROUPS]) * 100
        e_sh = np.mean([abs(get(t, m, "income_group", g, s) - get(t, "truth", "income_group", g, s))
                        for g in GROUPS for s in ("share_equities", "share_business", "share_bonds")]) * 100
        e_st = np.mean([abs(get(t, m, "status_" + i, g, "mean_nw") / get(t, "truth", "status_" + i, g, "mean_nw") - 1)
                        for i in INC for g in ("zero", "nonzero")]) * 100
        e_top = abs(get(t, m, "top1", "Top 1% by income", "share_of_networth") - get(t, "truth", "top1", "Top 1% by income", "share_of_networth")) * 100
        p(f"| {v} | {m} | {e_mean:.1f} | {e_med:.1f} | {e_sh:.1f} | {e_st:.1f} | {e_top:.1f} |")
p()

for v in VARIANTS:
    t = tabs[v]
    METHODS = ["truth", "none", "translated", "translated_rank", "translated_random", "reweighted"] + ([] if v.endswith("_self") else ["oracle"])
    p(f"## Variant `{v}`")
    p()
    p("Mean net worth, thousands of dollars, by income group (groups by each table's own total of the seven incomes).")
    p()
    p("| Income group | " + " | ".join(METHODS) + " |")
    p("|---|" + "---|" * len(METHODS))
    for g in GROUPS:
        p(f"| {g} | " + " | ".join(k(get(t, m, "income_group", g, "mean_nw")) for m in METHODS) + " |")
    p()
    p("Median net worth, thousands of dollars.")
    p()
    p("| Income group | " + " | ".join(METHODS) + " |")
    p("|---|" + "---|" * len(METHODS))
    for g in GROUPS:
        p(f"| {g} | " + " | ".join(k(get(t, m, "income_group", g, "median_nw")) for m in METHODS) + " |")
    p()
    p("Percent holding directly held stocks / a business, by income group.")
    p()
    p("| Income group | " + " | ".join(METHODS) + " |")
    p("|---|" + "---|" * len(METHODS))
    for g in GROUPS:
        p(f"| {g} | " + " | ".join(pct(get(t, m, "income_group", g, "share_equities")) + " / " +
                                   pct(get(t, m, "income_group", g, "share_business")) for m in METHODS) + " |")
    p()
    p("By whether an income is reported: percent of units reporting it, and mean net worth (thousands) of those "
      "reporting and not reporting it.")
    p()
    p("| Income | Statistic | " + " | ".join(METHODS) + " |")
    p("|---|---|" + "---|" * len(METHODS))
    for i in ["wages", "business", "int_div", "capital_gains", "rent"]:
        p(f"| {i} | percent reporting | " + " | ".join(pct(get(t, m, "status_" + i, "nonzero", "pop_share")) for m in METHODS) + " |")
        p(f"| {i} | mean net worth, reporting | " + " | ".join(k(get(t, m, "status_" + i, "nonzero", "mean_nw")) for m in METHODS) + " |")
        p(f"| {i} | mean net worth, not reporting | " + " | ".join(k(get(t, m, "status_" + i, "zero", "mean_nw")) for m in METHODS) + " |")
    p()
    p("| | " + " | ".join(METHODS) + " |")
    p("|---|" + "---|" * len(METHODS))
    p("| Top 1% by income: percent of net worth | " + " | ".join(pct(get(t, m, "top1", "Top 1% by income", "share_of_networth")) for m in METHODS) + " |")
    p("| All units: mean net worth (thousands) | " + " | ".join(k(get(t, m, "all", "All", "mean_nw")) for m in METHODS) + " |")
    p()
    mv = json.load(open(os.path.join(Q, f"y_movement_{v}.json")))
    tr = mv["translated"]
    p("How far half 2's households moved under translation (likelihood-proportional switching; weighted, over the five draws). "
      "Rank move: change in percentile among holders, from the survey's own distribution to the reference's, for "
      "households holding the income before and after.")
    p()
    p("| Income | Percent holding, before | after | Percent switched on | switched off | Mean rank move (points) | Percent moved over 10 points | over 25 points |")
    p("|---|---|---|---|---|---|---|---|")
    for i in INC:
        d = tr[i]
        p(f"| {i} | {pct(d['share_holding_before'])} | {pct(d['share_holding_after'])} | {pct(d['share_switched_on'])} | "
          f"{pct(d['share_switched_off'])} | {d['rank_move_mean_points']:.1f} | {pct(d['rank_move_share_over_10'])} | {pct(d['rank_move_share_over_25'])} |")
    d = tr["total_income"]
    p(f"| total income | | | | | {d['rank_move_mean_points']:.1f} | {pct(d['rank_move_share_over_10'])} | {pct(d['rank_move_share_over_25'])} (over 40: {pct(d['rank_move_share_over_40'])}) |")
    p()
    rw = mv["reweighted"]
    p(f"Reweighting: new weight over old weight, 1st percentile {rw['weight_ratio_p1']:.2f}, median {rw['weight_ratio_p50']:.2f}, "
      f"99th percentile {rw['weight_ratio_p99']:.2f}, largest {rw['weight_ratio_max']:.1f}; effective sample size "
      f"{rw['ess_new']:.0f} against {rw['ess_old']:.0f} before.")
    p()
    if v == "noise":
        sc = mv["oracle_distortion_of_half2"]
        p("How much the noise scrambles order: applying the same noise to half 2, the correlation between each "
          "household's rank among holders before and after, and the mean rank move in percentile points.")
        p()
        p("| Income | Rank correlation | Mean rank move (points) |")
        p("|---|---|---|")
        for i in INC:
            p(f"| {i} | {sc[i]['rank_corr']:.3f} | {sc[i]['mean_abs_rank_move_points']:.1f} |")
        p()

open(os.path.join(Q, "y_summary.md"), "w").write("\n".join(out) + "\n")
print("\n".join(out))
