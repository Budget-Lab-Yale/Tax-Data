"""Quick check of completed files against the truth (piece B; piece C does
the full evaluation).

For each completed file of a dataset, on target records whose true wealth
is known (`wealth_observed`), weighted by the records' population weights
and averaged over draws:
  - the share holding each wealth component (code other than exact zero),
    completed and true, and the mean absolute gap over the 24 components;
  - weighted mean and median net worth in dollars (14 assets minus 6 debts;
    the 4 unrealized gains are not part of net worth), completed and true.
Aggregates only. Writes $S/model/<dataset>/b_quick_check.csv.

Usage: python b_quick_check.py --dataset expA_clean
"""

import argparse
import glob
import json
from pathlib import Path

import numpy as np
import pandas as pd

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint")
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts',
          'other_fin', 'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards',
         'installment_debt', 'other_debt']


def wquant(x, w, q):
    o = np.argsort(x)
    c = np.cumsum(w[o]) / w.sum()
    return float(x[o][np.searchsorted(c, q)])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True)
    a = ap.parse_args()
    data, mdir = S / "data" / a.dataset, S / "model" / a.dataset
    meta = json.load(open(data / "meta.json"))
    vars_ = meta["variables"]
    names = [v["name"] for v in vars_]
    wealth = [v["name"] for v in vars_ if v["group"] == "scf_only" and v["kind"] == "amount"]
    zero_tok = {v["name"]: [t["token"] for t in v["tokens"] if t.get("kind") == "zero"][0]
                for v in vars_ if v["name"] in wealth}
    rec = pd.read_parquet(data / "records_test.parquet")
    truth = np.load(data / "truth_test.npy")
    rows = []
    for f in sorted(glob.glob(str(mdir / "completed_*_seed*.parquet"))):
        df = pd.read_parquet(f)
        if "wealth_observed" not in df.columns:
            continue
        df = df[df["wealth_observed"].astype(bool)]
        if df["source"].astype(str).eq("PUF").all() or len(df) == 0:
            continue
        w = df["weight"].to_numpy(float)
        r = dict(file=Path(f).name)
        tr = df["row"].to_numpy()
        tw = rec["weight"].to_numpy(float)[tr]
        gaps = []
        for n in wealth:
            j = names.index(n)
            c = np.average(df[f"tok_{n}"].to_numpy() != zero_tok[n], weights=w)
            t = np.average(truth[tr, j] != zero_tok[n], weights=tw)
            r[f"hold_{n}"] = c
            r[f"true_hold_{n}"] = t
            gaps.append(abs(c - t))
        r["mean_abs_holding_gap"] = float(np.mean(gaps))
        if all(n in df.columns for n in ASSETS + DEBTS) and df[ASSETS + DEBTS].notna().all().all():
            nw = df[ASSETS].sum(axis=1).to_numpy() - df[DEBTS].sum(axis=1).to_numpy()
            tnw = (rec.iloc[tr][ASSETS].sum(axis=1).to_numpy()
                   - rec.iloc[tr][DEBTS].sum(axis=1).to_numpy())
            r.update(nw_mean=float(np.average(nw, weights=w)),
                     true_nw_mean=float(np.average(tnw, weights=tw)),
                     nw_median=wquant(nw, w, 0.5), true_nw_median=wquant(tnw, tw, 0.5),
                     nw_p90=wquant(nw, w, 0.9), true_nw_p90=wquant(tnw, tw, 0.9))
            # Weight share above the true 99th and 99.9th percentiles (true
            # shares are 0.01 and 0.001 by construction), and the share of
            # completed mean net worth contributed by records above the true
            # 99.9th percentile.
            for q, lab in ((0.99, "p99"), (0.999, "p999")):
                thr = wquant(tnw, tw, q)
                r[f"share_above_true_{lab}"] = float(np.average(nw > thr, weights=w))
                # Unweighted (share of records): tells whether the plain
                # completion reproduces the over-sampled SCF sample.
                r[f"unw_share_above_true_{lab}"] = float(np.mean(nw > thr))
                r[f"true_unw_share_above_{lab}"] = float(np.mean(tnw > thr))
            thr = wquant(tnw, tw, 0.999)
            r["mean_share_from_above_true_p999"] = float(
                np.sum(w * nw * (nw > thr)) / np.sum(w * nw))
            r["true_mean_share_from_above_p999"] = float(
                np.sum(tw * tnw * (tnw > thr)) / np.sum(tw * tnw))
        rows.append(r)
    out = pd.DataFrame(rows)
    out.to_csv(mdir / "b_quick_check.csv", index=False)
    cols = ["file", "mean_abs_holding_gap", "hold_equities", "true_hold_equities",
            "hold_primary_home", "true_hold_primary_home", "hold_bonds", "true_hold_bonds",
            "nw_mean", "true_nw_mean", "nw_median", "true_nw_median", "nw_p90", "true_nw_p90",
            "share_above_true_p99", "share_above_true_p999", "mean_share_from_above_true_p999",
            "true_mean_share_from_above_p999", "unw_share_above_true_p99", "true_unw_share_above_p99",
            "unw_share_above_true_p999", "true_unw_share_above_p999"]
    pd.set_option("display.width", 250)
    pd.set_option("display.max_columns", 30)
    print(out[[c for c in cols if c in out.columns]].round(3).to_string(index=False))


if __name__ == "__main__":
    main()
