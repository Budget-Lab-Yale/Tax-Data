"""Piece F: one table of every full-model calibration report
($P/<version>/f/<arm>/report_*.json)."""
import glob
import json
import os

import pandas as pd


def mean_pos_target(ver, s):
    t = []
    for nm in ("puf", "wcore" if s == "core" else "wfull"):
        x = pd.read_parquet(f"{P}/{ver}/a/targets_{nm}.parquet")["target"]
        t.append(x[x > 0])
    return float(pd.concat(t).mean())

P = "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain"
rows = []
for fn in sorted(glob.glob(f"{P}/full*/f/*/report_*.json")):
    r = json.load(open(fn))
    ver = fn.split("/")[-4]
    arm = fn.split("/")[-2]
    st = r.get("status_counts_targets") or r.get("status_counts", {})
    infeas = sum(v for k, v in st.items() if k in ("unreachable", "above_reach", "below_forced", "joint", "block", "missed"))
    if arm == "ps1s":
        kl, ess = r.get("kl_from_model_total"), r.get("ess_final")
        extra = f"between groups {r.get('kl_between_income_groups', float('nan')):.3f}"
        n = r.get("draws")
    elif arm == "ps1":
        kl, ess, n = r.get("kl_nats_per_unit_weight"), r.get("ess"), r.get("K")
        extra = ""
    else:
        kl, ess, n = r.get("kl_nats_per_unit_weight"), r.get("effective_draws_weighted_mean"), r.get("n_records", 0) * r.get("K", 0)
        extra = f"effective draws per record {ess:.1f} of {r.get('K')}" if ess else ""
    rel = r.get("all_cells_mean_abs_error", float("nan")) / mean_pos_target(ver, r.get("set"))
    rows.append(f"| {ver} | {arm} | {r.get('set')} | {rel:.1%} | {r.get('size', '')} | {n:,} | {r.get('converged')} | "
                f"{st.get('active', 0):,} | {infeas:,} | {st.get('zero', 0):,} | {kl:.3f} | "
                f"{(ess or 0):,.0f} | {r.get('feasible_max_rel_error', float('nan')):.1e} | {extra} |")
print("| Model | Arm | Targets | Mean miss per cell, share of the average positive target | Size | Draws | Converged | Cells hit | Cells infeasible | Zero cells | KL (nats) | Effective sample | Largest relative miss, hit cells | Note |")
print("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|")
print("\n".join(rows))
