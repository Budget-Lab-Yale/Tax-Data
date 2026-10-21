"""Quick sanity check of D's completed files against the truth.

Not the evaluation (that is piece C). Prints, for each completed file of a
dataset: the number of records and draws, any missing values, and for each
wealth variable the weighted share holding (nonzero) and the weighted
median and 90th percentile among holders, completed against true, plus the
weighted mean of net worth. Aggregate figures only.

Usage (sbatch only): python d_check.py --dataset expA_clean
"""

import argparse
import glob
import os

import numpy as np
import pandas as pd

import d_common as dc


def wq(x, w, q):
    if len(x) == 0:
        return np.nan
    o = np.argsort(x)
    c = np.cumsum(w[o]) / w.sum()
    return float(x[o][min(np.searchsorted(c, q), len(x) - 1)])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True)
    args = ap.parse_args()
    _, _, rec = dc.load(args.dataset, "test")
    for path in sorted(glob.glob(os.path.join(dc.comp_dir(args.dataset), "completed_*.parquet"))):
        df = pd.read_parquet(path)
        print(f"\n== {os.path.basename(path)}: {df['row'].nunique()} records, "
              f"{df['draw'].nunique()} draws, {len(df)} rows")
        na = df[dc.WEALTH].isna().sum()
        if na.any():
            print("   missing completed values:", na[na > 0].to_dict())
        if "PUF" in set(df["source"].astype(str)):
            print("   (PUF targets: no truth; skipping comparison)")
            continue
        # Grade only records whose wealth is known (split-off SCF units are
        # completed but have no true wealth since A's 13:19 change).
        if "wealth_observed" in rec.columns:
            known = rec["wealth_observed"].astype(bool).values
            n_unk = int((~known[df.loc[df.draw == 0, "row"].values]).sum())
            print(f"   completed records without true wealth (not graded): {n_unk}")
            df = df[known[df["row"].values]]
        tr = rec.iloc[df.loc[df.draw == 0, "row"].values]
        wt = tr["weight"].values
        w = df["weight"].values / df["draw"].nunique()
        print(f"   {'variable':18s} {'hold_c':>7s} {'hold_t':>7s} {'p50_c':>10s} {'p50_t':>10s} {'p90_c':>11s} {'p90_t':>11s}")
        for v in dc.WEALTH:
            xc, xt = df[v].values, tr[v].values
            hc = np.sum(w * (xc != 0)) / w.sum()
            ht = np.sum(wt * (xt != 0)) / wt.sum()
            mc, mt = xc != 0, xt != 0
            print(f"   {v:18s} {hc:7.3f} {ht:7.3f} {wq(xc[mc], w[mc], .5):10.0f} {wq(xt[mt], wt[mt], .5):10.0f}"
                  f" {wq(xc[mc], w[mc], .9):11.0f} {wq(xt[mt], wt[mt], .9):11.0f}")
        nw_c = df[dc.ASSETS].sum(axis=1) - df[dc.DEBTS].sum(axis=1)
        nw_t = tr[dc.ASSETS].sum(axis=1) - tr[dc.DEBTS].sum(axis=1)
        print(f"   mean net worth: completed {np.sum(w * nw_c) / w.sum():,.0f}, "
              f"true {np.sum(wt * nw_t) / wt.sum():,.0f}")
        if "tok_cash" in df.columns:
            print("   token columns present; min token", int(df[[f'tok_{v}' for v in dc.WEALTH]].min().min()))


if __name__ == "__main__":
    main()
