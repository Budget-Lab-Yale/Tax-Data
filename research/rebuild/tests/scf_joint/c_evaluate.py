"""Part C: compare completed wealth with the true wealth of the same records.

  python c_evaluate.py --dataset expA_clean --yardstick
      Noise yardsticks for the dataset (no completed file needed):
      ref_sampling  one random half of the true records' households against
                    the other half, five different splits, gaps averaged
                    (how much the statistics move from sampling alone);
      ref_shuffled  every record given the whole wealth vector of a random
                    other record (each component's distribution and the
                    patterns across components kept, the link to income and
                    age broken: the "no information" end of the scale).

  python c_evaluate.py --dataset expA_clean --completed <file> --label <name>
      Completed records against the truth for the same records, and the
      completion noise (draw 0 against draw 1 of the same file).

Outputs in $S/eval/<dataset>/<label>/: summary.json (headline numbers),
detail_*.csv (every cell), report.md (readable tables).
"""
import argparse
import json
import os
import sys
from datetime import datetime

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from c_common import (S, WEALTH, INCOMES, INC_LABELS, COND_PAIRS, Dataset, truth_frame,  # noqa: E402
                      completed_frame, frame_stats, compare_stats, classifier_test,
                      household_halves, shuffled_frame, jsonable, log)

N_SPLITS = 5


def save(outdir, summary, det):
    os.makedirs(outdir, exist_ok=True)
    for k, df in det.items():
        df.to_csv(os.path.join(outdir, f"detail_{k}.csv"), index=False)
    json.dump(jsonable(summary), open(os.path.join(outdir, "summary.json"), "w"), indent=1)
    log(f"wrote {outdir}")


def run_yardsticks(ds, T, ST, age_keys, outroot, seed):
    tinfo = ds.tinfo
    # sampling noise: half against half, N_SPLITS splits
    heads, dets, clfs = [], [], []
    for s in range(N_SPLITS):
        A, B = household_halves(T, seed + s)
        h, d = compare_stats(frame_stats(A, tinfo, age_keys), frame_stats(B, tinfo, age_keys))
        heads.append(h)
        dets.append(d)
        if s < 2:
            c, imp = classifier_test(A, B, seed + 100 + s)
            clfs.append(c["auc"])
        log(f"  sampling split {s}: holding gap by income group {h['hold_inc_mae_pp']:.2f} pp")
    head = {}
    for k in heads[0]:
        vals = [h[k] for h in heads]
        if all(isinstance(v, (int, float, np.floating, np.integer)) for v in vals):
            head[k] = float(np.nanmean(np.abs(vals))) if k.endswith(("gap_pp",)) else float(np.nanmean(vals))
        else:
            head[k] = vals[0]
    # ratios: the yardstick is the typical distance of the ratio from 1
    for k in heads[0]:
        if "_ratio" in k:
            head[k + "_absdev"] = float(np.mean([abs(h[k] - 1) for h in heads]))
    head["classifier_auc"] = float(np.mean(clfs))
    # per-cell yardsticks: mean absolute gap across splits
    det = {}
    for key, col, keys in [("hold", "gap", ["by", "group", "variable"]),
                           ("amounts", "rank_gap", ["by", "group", "variable", "q"]),
                           ("cond", "gap", ["holds", "given"]),
                           ("income_corr", "gap", ["variable"])]:
        cat = pd.concat([d[key][keys + [col]].assign(split=i) for i, d in enumerate(dets)])
        cat[col] = cat[col].abs()
        agg = cat.groupby(keys, dropna=False)[col].mean().reset_index().rename(columns={col: "noise_abs_gap"})
        if key == "amounts":
            lg = pd.concat([d[key][keys + ["log_gap"]] for d in dets])
            lg["log_gap"] = lg["log_gap"].abs()
            agg = agg.merge(lg.groupby(keys)["log_gap"].mean().reset_index()
                            .rename(columns={"log_gap": "noise_abs_log_gap"}), on=keys)
        det[key] = agg
    nw = pd.concat([d["nw"].assign(rel=np.abs(d["nw"]["cand"] / d["nw"]["ref"] - 1),
                                   absdiff=np.abs(d["nw"]["cand"] - d["nw"]["ref"])) for d in dets])
    det["nw"] = nw.groupby("stat", sort=False)[["rel", "absdiff"]].mean().reset_index()
    nwi = pd.concat([d["nw_inc"].assign(rel_mean=np.abs(d["nw_inc"]["cand_mean"] / d["nw_inc"]["ref_mean"] - 1),
                                        rel_median=np.abs(d["nw_inc"]["cand_median"] / d["nw_inc"]["ref_median"] - 1))
                     for d in dets])
    det["nw_inc"] = nwi.groupby("group", sort=False)[["rel_mean", "rel_median"]].mean().reset_index()
    summary = {"dataset": ds.name, "label": "ref_sampling", "kind": "yardstick",
               "what": f"one random half of the households against the other, {N_SPLITS} splits, gaps averaged",
               "n_records": int(len(T)), "n_households": int(T["hh"].nunique()),
               "n_excluded_wealth_unobserved": T.attrs.get("n_excluded_wealth_unobserved", 0),
               "headline": head, "per_split": heads, "seed": seed,
               "time": datetime.now().isoformat(timespec="minutes")}
    save(os.path.join(outroot, "ref_sampling"), summary, det)

    # no-information reference: shuffled wealth vectors
    Sh = shuffled_frame(T, seed + 7)
    h, d = compare_stats(ST, frame_stats(Sh, tinfo, age_keys))
    c, imp = classifier_test(T, Sh[Sh["draw"] == 0].assign(w=lambda x: x["w"] * 5), seed + 200)
    h["classifier_auc"] = c["auc"]
    d["importance"] = imp
    summary = {"dataset": ds.name, "label": "ref_shuffled", "kind": "yardstick",
               "what": "each record takes the whole wealth vector of a random record (by weight)",
               "n_records": int(len(T)), "headline": h, "classifier": c, "seed": seed + 7,
               "time": datetime.now().isoformat(timespec="minutes")}
    save(os.path.join(outroot, "ref_shuffled"), summary, d)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True)
    ap.add_argument("--completed")
    ap.add_argument("--label")
    ap.add_argument("--split", default="test")
    ap.add_argument("--data-root", default=os.path.join(S, "data"))
    ap.add_argument("--out-root", default=os.path.join(S, "eval"))
    ap.add_argument("--yardstick", action="store_true")
    ap.add_argument("--seed", type=int, default=20261009)
    args = ap.parse_args()

    ds = Dataset(args.dataset, args.split, args.data_root)
    src = ds.records["source"].astype(str)
    log(f"dataset {args.dataset} split {args.split}: {len(ds.records):,} records; "
        + ", ".join(f"{k} {v:,}" for k, v in src.value_counts().items()))
    keep = (src != "PUF").to_numpy()
    if (~keep).any():
        log(f"  {(~keep).sum():,} PUF records are not part of this evaluation (no true wealth); "
            "c_evaluate_puf.py handles them")
    T = truth_frame(ds, keep)
    age_keys = sorted(np.unique(T["t_age_range"]).tolist())
    outroot = os.path.join(args.out_root, args.dataset)
    log(f"evaluation set: {len(T):,} records, {T['hh'].nunique():,} households, "
        f"years {sorted(T['year'].unique().tolist())}, total weight {T['w'].sum():,.0f}")
    ST = frame_stats(T, ds.tinfo, age_keys)
    if ST["hold_token_dollar_disagree"]:
        log(f"  WARNING: holding by token and by dollar disagree in the truth: {ST['hold_token_dollar_disagree']}")

    if args.yardstick:
        run_yardsticks(ds, T, ST, age_keys, outroot, args.seed)

    if args.completed:
        label = args.label or os.path.basename(args.completed).replace("completed_", "").replace(".parquet", "")
        src_c = pd.read_parquet(args.completed, columns=["source"])["source"].astype(str)
        if (src_c == "PUF").all():
            log(f"{label}: only PUF rows (no true wealth); nothing to grade here (see c_evaluate_puf.py)")
            return
        C, covered = completed_frame(ds, T, args.completed)
        Tc = T.loc[covered].reset_index(drop=True)
        STc = ST if covered.all() else frame_stats(Tc, ds.tinfo, age_keys)
        SC = frame_stats(C, ds.tinfo, age_keys)
        if SC["hold_token_dollar_disagree"]:
            log(f"  WARNING: completed holding by token and by dollar disagree: {SC['hold_token_dollar_disagree']}")
        h, d = compare_stats(STc, SC)
        C0 = C[C["draw"] == C["draw"].min()].copy()
        C0["w"] = C0["w"] * C0["ndraw"]
        c, imp = classifier_test(Tc, C0, args.seed + 300)
        h["classifier_auc"] = c["auc"]
        d["importance"] = imp
        # completion noise: one draw against another
        draws = sorted(C["draw"].unique().tolist())
        cn = None
        if len(draws) >= 2:
            D0 = C[C["draw"] == draws[0]].copy()
            D1 = C[C["draw"] == draws[1]].copy()
            D0["w"] *= D0["ndraw"]
            D1["w"] *= D1["ndraw"]
            cn, _ = compare_stats(frame_stats(D0, ds.tinfo, age_keys), frame_stats(D1, ds.tinfo, age_keys))
            cc, _ = classifier_test(D0, D1, args.seed + 400)
            cn["classifier_auc"] = cc["auc"]
        data_mtime = max(os.path.getmtime(os.path.join(ds.dir, f)) for f in
                         (f"records_{args.split}.parquet", "quantiles.parquet"))
        stale = os.path.getmtime(args.completed) < data_mtime
        if stale:
            log("  WARNING: the completed file is older than the dataset's records/quantiles; "
                "its dollars may be on the old scale (rank measures are unaffected)")
        summary = {"dataset": args.dataset, "label": label, "kind": "completed",
                   "stale_dollars": bool(stale),
                   "decoded_by_C": C.attrs.get("decoded_by_C"),
                   "file": os.path.abspath(args.completed),
                   "file_mtime": os.path.getmtime(args.completed),
                   "n_records": int(len(Tc)), "n_records_missing": int((~covered).sum()),
                   "n_excluded_wealth_unobserved": T.attrs.get("n_excluded_wealth_unobserved", 0),
                   "n_draws": len(draws), "headline": h, "completion_noise": cn,
                   "classifier": c,
                   "token_dollar_disagree": SC["hold_token_dollar_disagree"],
                   "time": datetime.now().isoformat(timespec="minutes")}
        save(os.path.join(outroot, label), summary, d)
        log(f"{label}: holding gap by income group {h['hold_inc_mae_pp']:.2f} pp, "
            f"top 1% NW share {h['nw_top1_cand']:.1f}% (truth {h['nw_top1_ref']:.1f}%), "
            f"classifier AUC {c['auc']:.3f}")


if __name__ == "__main__":
    main()
