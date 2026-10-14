"""Part C, experiment B: PUF records completed with wealth at 2015, against
the SCF 2013 and 2016 filers and their interpolation to 2015.

  python c_evaluate_puf.py --dataset expB --completed <file> --label <name>

There is no true wealth for PUF records, so the check is on aggregates:
holding and amounts of each component by income rank group and age range,
and net worth at the top. The SCF side uses every SCF filer record of the
dataset (train, val and test). Income groups are weighted ranks of the
seven-income total within each source and year, so "top 1%" means the top
1% of PUF filers on the PUF side and of SCF filers on the SCF side.

Interpolation to 2015 (one third 2013, two thirds 2016): shares holding and
shares of net worth linearly; dollar amounts in log dollars.

The SCF's own change from 2013 to 2016 is shown as a scale: a gap between
the PUF completion and the interpolation is small if it is small next to
how much the SCF itself moves in three years (which includes its sampling
noise).
"""
import argparse
import json
import os
import sys
from datetime import datetime

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from c_common import (S, WEALTH, ASSETS, DEBTS, INCOMES, INC_EDGES, INC_LABELS, Dataset,  # noqa: E402
                      truth_frame, completed_frame, frame_stats, wrank, income_groups, decode_wealth, jsonable, log)

A13, A16 = 1 / 3, 2 / 3
QSHOW = [0.25, 0.50, 0.75, 0.90]


def regroup(F):
    g = np.zeros(len(F), dtype=np.int8)
    for key, idx in F.groupby(["source", "year"]).indices.items():
        r = wrank(F["inc_total"].to_numpy()[idx], F["w"].to_numpy()[idx])
        g[idx] = np.searchsorted(np.array(INC_EDGES), r, side="right")
    F["inc_group"] = g
    return F


def interp_log(a, b):
    if a is None or b is None or a <= 0 or b <= 0:
        return np.nan if (a is None or b is None) else A13 * a + A16 * b
    return float(np.exp(A13 * np.log(a) + A16 * np.log(b)))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", default="expB")
    ap.add_argument("--completed", required=True)
    ap.add_argument("--label")
    ap.add_argument("--data-root", default=os.path.join(S, "data"))
    ap.add_argument("--out-root", default=os.path.join(S, "eval"))
    args = ap.parse_args()
    label = (args.label or os.path.basename(args.completed).replace("completed_", "")
             .replace(".parquet", "")) + "_puf"

    # SCF side: all splits
    # Income groups are ranks among ALL SCF filer units of the year (split-off
    # units included, as on the PUF side); the wealth statistics then use only
    # units whose wealth is observed (split-off units have no SCF wealth).
    scf, allrec = [], []
    n_unobs = 0
    for sp in ["train", "val", "test"]:
        ds = Dataset(args.dataset, sp, args.data_root)
        keep = (ds.records["source"].astype(str) == "SCF").to_numpy()
        F = truth_frame(ds, keep)
        n_unobs += F.attrs.get("n_excluded_wealth_unobserved", 0)
        F["_key"] = sp + ":" + F["row"].astype(str)
        scf.append(F)
        r = ds.records.loc[keep, ["source", "year", "weight"] + INCOMES].copy()
        r["_key"] = sp + ":" + pd.Series(np.flatnonzero(keep)).astype(str).to_numpy()
        allrec.append(r)
    tinfo = ds.tinfo
    allrec = pd.concat(allrec, ignore_index=True)
    allrec["inc_group"] = income_groups(allrec)
    scf = pd.concat(scf, ignore_index=True)
    scf["inc_group"] = scf["_key"].map(allrec.set_index("_key")["inc_group"]).to_numpy()
    log(f"SCF: {n_unobs:,} filer records with wealth unobserved (split-off units) are ranked for "
        "income groups but left out of the wealth statistics")
    log(f"SCF filers: " + ", ".join(f"{y}: {n:,} records" for y, n in scf["year"].value_counts().sort_index().items()))

    # PUF side: the completed PUF records
    dt = Dataset(args.dataset, "test", args.data_root)
    src = dt.records["source"].astype(str).to_numpy()
    puf_keep = src == "PUF"
    rec = dt.records
    P = pd.DataFrame({"row": np.arange(len(rec))})
    P["source"], P["year"] = src, rec["year"].to_numpy()
    P["hh"] = rec["hh_id"].astype(np.int64).to_numpy()
    P["w"] = rec["weight"].to_numpy().astype(float)
    P["inc_total"] = rec[INCOMES].to_numpy().sum(axis=1)
    for v in ["age_range", "married", "n_dep"] + INCOMES:
        P["t_" + v] = dt.col(v)
    for v in INCOMES:
        P["d_" + v] = rec[v].to_numpy()
    P = P.loc[puf_keep].reset_index(drop=True)
    C = pd.read_parquet(args.completed)
    C = C[C["source"].astype(str) == "PUF"].reset_index(drop=True)
    if len(C) == 0:
        raise SystemExit("no PUF rows in the completed file")
    pos = pd.Series(np.arange(len(P)), index=P["row"].to_numpy())
    if not np.isin(C["row"].to_numpy(), P["row"].to_numpy()).all():
        raise SystemExit("completed PUF rows not found among the PUF test records")
    F = P.iloc[pos.loc[C["row"].to_numpy()].to_numpy()].reset_index(drop=True).copy()
    F["draw"] = C["draw"].to_numpy()
    nd = F.groupby("row")["draw"].transform("nunique").to_numpy()
    F["ndraw"] = nd
    if any(C[v].isna().any() for v in WEALTH):
        log("  completed PUF dollars missing; decoding the completed tokens at 2015 with a_tokenizer")
        dec = decode_wealth(dt, C, seed=20261009, year=2015)
        for v in WEALTH:
            C[v] = dec[v].to_numpy()
    for v in WEALTH:
        d = C[v].to_numpy().astype(float)
        if np.isnan(d).any():
            raise SystemExit(f"completed PUF dollars missing for {v}: {int(np.isnan(d).sum())}")
        F["d_" + v] = d
        if "tok_" + v in C.columns:
            F["t_" + v] = C["tok_" + v].to_numpy()
        else:
            # No tokens (piece D's drf-puf files: no single SCF rank scale at
            # 2015). Placeholder tokens carry only zero / negative / positive,
            # enough for holding; the rank measures are not reported for PUF.
            kinds = np.array([tt.get("kind", "") for tt in dt.vmeta[v]["tokens"]])
            tz = int(np.flatnonzero(kinds == "zero")[0])
            tp = int(np.flatnonzero(kinds == "pos")[0])
            tn = int(np.flatnonzero(kinds == "neg")[0]) if (kinds == "neg").any() else tz
            F["t_" + v] = np.where(d > 0, tp, np.where(d < 0, tn, tz))
    F = regroup(F)                      # income groups among PUF records
    F["w"] = F["w"] / nd
    F["nw"] = F[["d_" + v for v in ASSETS]].sum(axis=1) - F[["d_" + v for v in DEBTS]].sum(axis=1)
    n_age_missing = int((F["t_age_range"] < 0).sum())
    log(f"PUF completed: {F['row'].nunique():,} records x {sorted(set(nd.tolist()))} draws; "
        f"{n_age_missing:,} rows with age range unobserved (shown as their own group)")

    age_keys = sorted(set(np.unique(scf["t_age_range"]).tolist()) | set(np.unique(F["t_age_range"]).tolist()))
    st = {"PUF 2015 (completed)": frame_stats(F, tinfo, age_keys)}
    for y in (2013, 2016):
        st[f"SCF {y}"] = frame_stats(scf[scf["year"] == y].reset_index(drop=True), tinfo, age_keys)
    a, b, p = st["SCF 2013"], st["SCF 2016"], st["PUF 2015 (completed)"]

    # holding shares
    rows = []
    for (g, k) in p["hold"]:
        for j, v in enumerate(WEALTH):
            h13, h16, hp = a["hold"][(g, k)][j], b["hold"][(g, k)][j], p["hold"][(g, k)][j]
            rows.append({"by": g, "group": k, "variable": v, "scf2013": h13, "scf2016": h16,
                         "interp2015": A13 * h13 + A16 * h16, "puf2015": hp})
    hold = pd.DataFrame(rows)
    hold["gap"] = hold["puf2015"] - hold["interp2015"]
    hold["wave_change"] = hold["scf2016"] - hold["scf2013"]
    # amounts among holders, dollars
    rows = []
    for key in p["qd"]:
        if key not in a["qd"] or key not in b["qd"]:
            continue
        g, k, v = key
        for i, q in enumerate([0.10, 0.25, 0.50, 0.75, 0.90]):
            if q not in QSHOW:
                continue
            q13, q16, qp = a["qd"][key][i], b["qd"][key][i], p["qd"][key][i]
            rows.append({"by": g, "group": k, "variable": v, "q": q, "scf2013": q13, "scf2016": q16,
                         "interp2015": interp_log(q13, q16), "puf2015": qp,
                         "n_puf": p["ncell"][key], "n_2013": a["ncell"][key], "n_2016": b["ncell"][key]})
    amt = pd.DataFrame(rows)
    amt["log_gap"] = np.log(amt["puf2015"] / amt["interp2015"])
    amt["log_wave_change"] = np.log(amt["scf2016"] / amt["scf2013"])
    ok = (amt[["n_puf", "n_2013", "n_2016"]].min(axis=1) >= 30)
    # net worth
    nwrows = []
    for q in [0.10, 0.25, 0.50, 0.75, 0.90, 0.99]:
        nwrows.append({"stat": f"p{int(round(q * 100))}", "scf2013": a["nw_q"][q], "scf2016": b["nw_q"][q],
                       "interp2015": interp_log(a["nw_q"][q], b["nw_q"][q]), "puf2015": p["nw_q"][q]})
    nwrows.append({"stat": "mean", "scf2013": a["nw_mean"], "scf2016": b["nw_mean"],
                   "interp2015": interp_log(a["nw_mean"], b["nw_mean"]), "puf2015": p["nw_mean"]})
    for t in [0.10, 0.01, 0.001]:
        nwrows.append({"stat": f"top{t * 100:g}%_share", "scf2013": a["nw_top"][t], "scf2016": b["nw_top"][t],
                       "interp2015": A13 * a["nw_top"][t] + A16 * b["nw_top"][t], "puf2015": p["nw_top"][t]})
    nw = pd.DataFrame(nwrows)
    nwi = pd.DataFrame([{"group": INC_LABELS[k], "scf2013": a["nw_inc"][k]["mean"], "scf2016": b["nw_inc"][k]["mean"],
                         "interp2015": interp_log(a["nw_inc"][k]["mean"], b["nw_inc"][k]["mean"]),
                         "puf2015": p["nw_inc"][k]["mean"]} for k in range(4)])

    head = {
        "hold_all_mae_pp": 100 * float(hold.loc[hold.by == "all", "gap"].abs().mean()),
        "hold_all_wave_change_pp": 100 * float(hold.loc[hold.by == "all", "wave_change"].abs().mean()),
        "hold_inc_mae_pp": 100 * float(hold.loc[hold.by == "inc", "gap"].abs().mean()),
        "hold_inc_wave_change_pp": 100 * float(hold.loc[hold.by == "inc", "wave_change"].abs().mean()),
        "hold_age_mae_pp": 100 * float(hold.loc[(hold.by == "age") & (hold.group >= 0), "gap"].abs().mean()),
        "hold_age_wave_change_pp": 100 * float(hold.loc[(hold.by == "age") & (hold.group >= 0), "wave_change"].abs().mean()),
        "amt_inc_dollar_mae_pct": 100 * float(np.exp(amt.loc[ok & (amt.by == "inc"), "log_gap"].abs().mean()) - 1),
        "amt_inc_wave_change_pct": 100 * float(np.exp(amt.loc[ok & (amt.by == "inc"), "log_wave_change"].abs().mean()) - 1),
    }
    for r in nw.itertuples():
        head[f"nw_{r.stat}"] = {"scf2013": r.scf2013, "scf2016": r.scf2016, "interp2015": r.interp2015,
                                "puf2015": r.puf2015}

    outdir = os.path.join(args.out_root, args.dataset, label)
    os.makedirs(outdir, exist_ok=True)
    hold.to_csv(os.path.join(outdir, "detail_hold.csv"), index=False)
    amt.to_csv(os.path.join(outdir, "detail_amounts.csv"), index=False)
    nw.to_csv(os.path.join(outdir, "detail_nw.csv"), index=False)
    nwi.to_csv(os.path.join(outdir, "detail_nw_inc.csv"), index=False)

    # readable report
    def t_hold(sub, keycols, names):
        lines = ["| " + " | ".join(names + ["SCF 2013", "SCF 2016", "Interpolated 2015", "PUF 2015 completed"]) + " |",
                 "|" + "---|" * (len(names) + 4)]
        for r in sub.itertuples():
            lines.append("| " + " | ".join([str(getattr(r, c)) for c in keycols] +
                                           [f"{100 * x:.1f}" for x in (r.scf2013, r.scf2016, r.interp2015, r.puf2015)]) + " |")
        return "\n".join(lines)

    md = [f"#### PUF 2015 completed with wealth (`{label}`)", "",
          f"{F['row'].nunique():,} PUF test records, {int(F['draw'].nunique())} draws each, "
          f"against {int((scf.year == 2013).sum()):,} SCF 2013 and {int((scf.year == 2016).sum()):,} "
          "SCF 2016 filer records with wealth observed (all implicates; "
          f"{n_unobs:,} split-off unit records without SCF wealth are left out of the SCF side, "
          "so the SCF side covers fewer low-wealth units than the PUF side). "
          "Interpolated 2015: one third 2013, two thirds 2016.", "",
          "| Measure | PUF against interpolated SCF | SCF 2013 against SCF 2016 (scale) |", "|---|---|---|",
          f"| Share holding, all records: average gap (points) | {head['hold_all_mae_pp']:.2f} | {head['hold_all_wave_change_pp']:.2f} |",
          f"| Share holding by income group: average gap (points) | {head['hold_inc_mae_pp']:.2f} | {head['hold_inc_wave_change_pp']:.2f} |",
          f"| Share holding by age range: average gap (points) | {head['hold_age_mae_pp']:.2f} | {head['hold_age_wave_change_pp']:.2f} |",
          f"| Amounts among holders by income group (25th-90th percentiles): average gap (%) | {head['amt_inc_dollar_mae_pct']:.1f} | {head['amt_inc_wave_change_pct']:.1f} |",
          "", "**Share holding each component (%)**", "",
          t_hold(hold[hold.by == "all"], ["variable"], ["Component"]), "",
          "**Share holding by income group, selected components (%)**", ""]
    sel = hold[(hold.by == "inc") & hold.variable.isin(["primary_home", "equities", "pass_throughs", "dc",
                                                         "primary_mortgage", "bonds", "trusts"])].copy()
    sel["_o"] = sel["variable"].map(WEALTH.index)
    sel = sel.sort_values(["_o", "group"])
    sel["group"] = sel["group"].map(lambda k: INC_LABELS[k])
    md += [t_hold(sel, ["variable", "group"], ["Component", "Income group"]), "",
           "**Net worth (dollars of each year; PUF decoded at 2015)**", "",
           "| Statistic | SCF 2013 | SCF 2016 | Interpolated 2015 | PUF 2015 completed |", "|---|---|---|---|---|"]
    for r in nw.itertuples():
        f = (lambda x: f"{100 * x:.1f}%") if "share" in r.stat else (lambda x: f"{x / 1000:,.0f}k")
        md.append(f"| {r.stat} | {f(r.scf2013)} | {f(r.scf2016)} | {f(r.interp2015)} | {f(r.puf2015)} |")
    md += ["", "**Mean net worth by income group**", "",
           "| Income group | SCF 2013 | SCF 2016 | Interpolated 2015 | PUF 2015 completed |", "|---|---|---|---|---|"]
    for r in nwi.itertuples():
        md.append(f"| {r.group} | {r.scf2013 / 1000:,.0f}k | {r.scf2016 / 1000:,.0f}k | "
                  f"{r.interp2015 / 1000:,.0f}k | {r.puf2015 / 1000:,.0f}k |")
    open(os.path.join(outdir, "report.md"), "w").write("\n".join(md) + "\n")
    summary = {"dataset": args.dataset, "label": label, "kind": "puf", "file": os.path.abspath(args.completed),
               "file_mtime": os.path.getmtime(args.completed),
               "n_records": int(F["row"].nunique()), "n_age_unobserved_rows": n_age_missing,
               "n_scf_excluded_wealth_unobserved": int(n_unobs),
               "headline": head, "time": datetime.now().isoformat(timespec="minutes")}
    json.dump(jsonable(summary), open(os.path.join(outdir, "summary.json"), "w"), indent=1)
    log(f"wrote {outdir}")


if __name__ == "__main__":
    main()
