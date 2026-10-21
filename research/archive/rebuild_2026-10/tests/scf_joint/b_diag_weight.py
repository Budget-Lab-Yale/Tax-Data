"""Diagnostic of the drawn SCF weight in completion (piece B).

For completed files of experiment A (half 1, true wealth and true weight
known), on records with wealth observed:
  1. the distribution of the drawn weight code (64 bins of the weight's rank
     among SCF records) against half 1's true weight codes, unweighted
     (the SCF sample's own distribution): share in the lowest 8 codes (the
     smallest-weight eighth of SCF records, mostly the over-sampled
     wealthy), mean code, and the total variation distance over the codes;
  2. the share of records above the true 99th percentile of net worth, by
     group of the drawn weight code (completed) and of the true weight code
     (truth): codes 0-7, 8-31, 32-63. A model that has learned the sample
     puts low drawn weights with high wealth, as the truth does.
Aggregates only. Usage: python b_diag_weight.py --dataset expA_clean --files a,b,...
"""

import argparse
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
    ap.add_argument("--dataset", default="expA_clean")
    ap.add_argument("--files", required=True)
    a = ap.parse_args()
    data, mdir = S / "data" / a.dataset, S / "model" / a.dataset
    meta = json.load(open(data / "meta.json"))
    names = [v["name"] for v in meta["variables"]]
    jw = names.index("scf_weight")
    rec = pd.read_parquet(data / "records_test.parquet")
    truth = np.load(data / "truth_test.npy")
    obs = rec["wealth_observed"].astype(bool).to_numpy()
    tw = rec["weight"].to_numpy(float)
    tnw = rec[ASSETS].sum(axis=1).to_numpy() - rec[DEBTS].sum(axis=1).to_numpy()
    p99 = wquant(tnw[obs], tw[obs], 0.99)
    tcode = truth[:, jw]
    groups = [(0, 7), (8, 31), (32, 63)]
    rows = []
    t_hist = np.bincount(tcode[obs], minlength=64) / obs.sum()
    r = dict(file="TRUTH (half 1)", low8_share=float(np.mean(tcode[obs] <= 7)),
             mean_code=float(np.mean(tcode[obs])), tvd_vs_truth=0.0)
    for lo, hi in groups:
        m = obs & (tcode >= lo) & (tcode <= hi)
        r[f"share_rows_codes_{lo}_{hi}"] = float(m.sum() / obs.sum())
        r[f"above_p99_codes_{lo}_{hi}"] = float(np.mean(tnw[m] > p99))
    rows.append(r)
    for f in a.files.split(","):
        df = pd.read_parquet(mdir / f)
        df = df[df["wealth_observed"].astype(bool)]
        code = df["tok_scf_weight"].to_numpy()
        nw = df[ASSETS].sum(axis=1).to_numpy() - df[DEBTS].sum(axis=1).to_numpy()
        h = np.bincount(code, minlength=64) / len(code)
        r = dict(file=f, low8_share=float(np.mean(code <= 7)), mean_code=float(code.mean()),
                 tvd_vs_truth=float(0.5 * np.abs(h - t_hist).sum()))
        for lo, hi in groups:
            m = (code >= lo) & (code <= hi)
            r[f"share_rows_codes_{lo}_{hi}"] = float(m.mean())
            r[f"above_p99_codes_{lo}_{hi}"] = float(np.mean(nw[m] > p99)) if m.any() else np.nan
        rows.append(r)
    out = pd.DataFrame(rows)
    pd.set_option("display.width", 250)
    pd.set_option("display.max_columns", 30)
    print(f"true weighted 99th percentile of net worth (half 1): {p99:,.0f}")
    print(out.round(4).T.to_string())
    out.to_csv(mdir / "b_diag_weight.csv", index=False)


if __name__ == "__main__":
    main()
