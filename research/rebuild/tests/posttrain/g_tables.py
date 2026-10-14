"""Test 3, piece G: markdown tables of the checks of a `full` dataset, for
G_DATA_REPORT.md (aggregates only).

    python g_tables.py [dataset_dir]   (default $T2/data/full)

Writes <dataset_dir>/g_tables.md.
"""

import json
import os
import sys

import numpy as np
import pandas as pd

D = sys.argv[1] if len(sys.argv) > 1 else "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full"
POPS = ["PUF 2015, all returns", "PUF 2015, no dependent filers", "SCF 2013 filers", "SCF 2016 filers"]
SHORT = {"PUF 2015, all returns": "PUF all", "PUF 2015, no dependent filers": "PUF no dep. filers",
         "SCF 2013 filers": "SCF 2013", "SCF 2016 filers": "SCF 2016"}


def pct(x):
    return "" if pd.isna(x) else f"{100 * x:.1f}%"


def main():
    meta = json.load(open(os.path.join(D, "meta.json")))
    desc = {v["name"]: v.get("description", "") for v in meta["variables"]}
    out = []
    amt = pd.read_csv(os.path.join(D, "checks_totals.csv"))
    cat = pd.read_csv(os.path.join(D, "checks_categories.csv"))
    shared = [v["name"] for v in meta["variables"] if v["group"] == "shared"]

    out.append("### Crosswalked amounts: weighted total (billions; SCF 2013 incomes in 2015 dollars by CPI-U) and share nonzero\n")
    out.append("| Variable | " + " | ".join(f"{SHORT[p]} total" for p in POPS) + " | " +
               " | ".join(f"{SHORT[p]} nonzero" for p in POPS) + " | " +
               " | ".join(f"{SHORT[p]} negative" for p in POPS) + " |")
    out.append("|" + "---|" * (1 + 3 * len(POPS)))
    for n in shared:
        a = amt.loc[amt.variable == n].set_index("population")
        if a.empty:
            continue
        row = [f"`{n}` {desc[n]}"]
        row += [f"{a.loc[p, 'total_billions_2015_dollars_incomes']:,.0f}" if p in a.index else "" for p in POPS]
        row += [pct(a.loc[p, "share_nonzero"]) if p in a.index else "" for p in POPS]
        row += [pct(a.loc[p, "share_negative"]) if p in a.index else "" for p in POPS]
        out.append("| " + " | ".join(row) + " |")

    out.append("\n### Crosswalked categoricals: weighted shares\n")
    out.append("| Variable | Value | " + " | ".join(SHORT[p] for p in POPS) + " |")
    out.append("|---|---|" + "---|" * len(POPS))
    for n in shared:
        c = cat.loc[cat.variable == n]
        if c.empty:
            continue
        for val in sorted(c.value.unique()):
            row = [f"`{n}`", f"{val:g}"]
            for p in POPS:
                s = c.loc[(c.population == p) & (c.value == val), "share"]
                row.append(pct(s.iloc[0]) if len(s) else "")
            out.append("| " + " | ".join(row) + " |")
    out.append("\nAge range shares are among units with an age range observed (PUF: records in the demographic "
               "file, AGERANGE 1-6; on 'PUF all' dependent filers' codes 1-3 mean other ranges).\n")

    out.append("\n### PUF-only amounts (unobserved in the SCF): PUF weighted total (billions) and share nonzero\n")
    out.append("| Variable | PUF all total | PUF all nonzero | PUF all negative |")
    out.append("|---|---|---|---|")
    for v in meta["variables"]:
        if v["group"] != "puf_only" or v["kind"] != "amount":
            continue
        a = amt.loc[(amt.variable == v["name"]) & (amt.population == POPS[0])]
        out.append(f"| `{v['name']}` {v['description']} | {a.total_billions.iloc[0]:,.0f} | "
                   f"{pct(a.share_nonzero.iloc[0])} | {pct(a.share_negative.iloc[0])} |")

    out.append("\n### Wealth (SCF filers with wealth observed; survey-year dollars): total (billions) and share nonzero\n")
    out.append("| Variable | SCF 2013 total | SCF 2016 total | SCF 2013 nonzero | SCF 2016 nonzero |")
    out.append("|---|---|---|---|---|")
    for v in meta["variables"]:
        if v["group"] != "scf_only":
            continue
        a = amt.loc[amt.variable == v["name"]].set_index("population")
        out.append(f"| `{v['name']}` | {a.loc['SCF 2013 filers', 'total_billions']:,.0f} | "
                   f"{a.loc['SCF 2016 filers', 'total_billions']:,.0f} | {pct(a.loc['SCF 2013 filers', 'share_nonzero'])} | "
                   f"{pct(a.loc['SCF 2016 filers', 'share_nonzero'])} |")

    oc = pd.read_csv(os.path.join(D, "checks_observed_counts.csv"))
    out.append("\n### Observed and unobserved rows per variable and source (all parts)\n")
    out.append("| Variable | Group | PUF observed | PUF unobserved | SCF 2013 observed | SCF 2013 unobserved | SCF 2016 observed | SCF 2016 unobserved |")
    out.append("|---|---|---|---|---|---|---|---|")
    g = oc.groupby(["variable", "group", "source", "year"])[["observed", "unobserved"]].sum().reset_index()
    for v in meta["variables"]:
        if v["group"] == "context":
            continue
        r = g.loc[g.variable == v["name"]].set_index(["source", "year"])
        row = [f"`{v['name']}`", v["group"]]
        for k in [("PUF", 2015), ("SCF", 2013), ("SCF", 2016)]:
            row += [f"{int(r.loc[k, 'observed']):,}", f"{int(r.loc[k, 'unobserved']):,}"]
        out.append("| " + " | ".join(row) + " |")

    rt = pd.read_csv(os.path.join(D, "roundtrip.csv"))
    out.append("\n### Round trip (training rows, true tokens decoded to dollars, five seeds)\n")
    out.append("| Source-year | Variables | Median abs. error of total (mean of 5 seeds) | Largest three (mean, sd across seeds) |")
    out.append("|---|---|---|---|")
    for (s, y), r in rt.groupby(["source", "year"]):
        e = r.total_rel_err_mean_5_seeds.abs()
        top = r.assign(a=e).sort_values("a", ascending=False).head(3)
        txt = ", ".join(f"{t.variable} {100 * t.total_rel_err_mean_5_seeds:+.1f}% ({100 * t.total_rel_err_sd_5_seeds:.1f}%)"
                        for t in top.itertuples())
        out.append(f"| {s} {y} | {len(r)} | {100 * e.median():.2f}% | {txt} |")
    zero_ok = bool((rt.share_zero_true - rt.share_zero_decoded).abs().max() < 1e-12)
    neg_ok = bool((rt.share_neg_true - rt.share_neg_decoded).abs().max() < 1e-12)
    out.append(f"\nShares at zero reproduced exactly: {zero_ok}; shares negative reproduced exactly: {neg_ok}.\n")
    ps = json.load(open(os.path.join(D, "prep_summary.json")))
    ag = ps["reencode_agreement"]
    low = sorted(((k, v) for k, v in ag.items()), key=lambda kv: kv[1])[:6]
    out.append("Re-encoding decoded values gives the same token for: " +
               ", ".join(f"{k} {100 * v:.1f}%" for k, v in low) + " (the six lowest; all others higher).\n")
    with open(os.path.join(D, "g_tables.md"), "w") as f:
        f.write("\n".join(out) + "\n")
    print("\n".join(out))


if __name__ == "__main__":
    main()
