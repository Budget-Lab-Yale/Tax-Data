"""Key outcomes for the night-of-2026-10-08 training experiments
(EXPERIMENTS.md, hypotheses 1, 3, 4), adapted from diag_age_agi.py.

For each file, against the training records:
  - age range x AGI band among non-dependent filers: share of weight in the
    wrong cell (total variation distance between the two tables);
  - share of the oldest age range (code 6, non-dependent filers) with wages;
  - top 0.1% (by AGI) shares of long-term gains and of wages, as in
    evaluate.py (positive part, weighted);
  - total weight relative to the reference;
  - with --demog: everything restricted to records with demographics
    (AGERANGE present) in both the file and the reference, plus age x
    (has wages, has Social Security, has pension, filing status) tables.
Prints aggregate figures only and writes them to --out (csv).

Usage: python exp_diag.py [--demog] --out file.csv label=path [label=path ...]
The reference is always the training records (values_train.parquet).
"""
import argparse

import numpy as np
import pandas as pd

S = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator"
EDGES = [-np.inf, 0, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, np.inf]


def wrank(x, w):
    order = np.argsort(x, kind="mergesort")
    xs, ws = x[order], w[order] / w.sum()
    uniq, start = np.unique(xs, return_index=True)
    block_w = np.add.reduceat(ws, start)
    below = np.concatenate([[0.0], np.cumsum(block_w)[:-1]])
    mid = below + block_w / 2
    r = np.empty(len(x))
    r[order] = np.repeat(mid, np.diff(np.append(start, len(xs))))
    return r


def wtable(d, rows, cols):
    t = d.groupby([rows, cols], observed=False)["weight"].sum()
    return t / t.sum()


def tvd(a, b):
    idx = a.index.union(b.index)
    return 0.5 * (a.reindex(idx, fill_value=0) - b.reindex(idx, fill_value=0)).abs().sum()


def prep(d):
    d = d.copy()
    d["agi_band"] = pd.cut(d.E00100, EDGES, right=False, labels=False)
    d["has_wages"] = (d.E00200 != 0).astype(int)
    d["has_ss"] = (d.E02400 != 0).astype(int)
    d["has_pension"] = (d.E01500 != 0).astype(int)
    return d


def stats(d, ref_total):
    out = {}
    out["records"] = len(d)
    out["weight_ratio"] = d.weight.sum() / ref_total
    nd = d[d.DSI == 0]
    old = nd[nd.AGERANGE == 6]
    out["oldest_share_with_wages"] = np.average(old.E00200 != 0, weights=old.weight)
    w = d.weight.to_numpy()
    r = wrank(d.E00100.to_numpy(), w)
    for v, lab in [("P23250", "top01_ltcg"), ("E00200", "top01_wages")]:
        xp = np.maximum(d[v].to_numpy(), 0.0)
        out[lab] = (w * xp)[r >= 0.999].sum() / (w * xp).sum()
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--demog", action="store_true")
    ap.add_argument("--out", required=True)
    ap.add_argument("files", nargs="+")
    a = ap.parse_args()
    ref = prep(pd.read_parquet(f"{S}/data/values_train.parquet"))
    if a.demog:
        ref = ref[ref.AGERANGE != -1]
    ref_total = ref.weight.sum()
    ref_nd = ref[ref.DSI == 0]
    rows = []
    r0 = stats(ref, ref_total)
    r0["label"] = "train"
    rows.append(r0)
    for pair in a.files:
        lab, path = pair.split("=", 1)
        d = prep(pd.read_parquet(path))
        n_all = len(d)
        if a.demog:
            d = d[d.AGERANGE != -1]
        s = stats(d, ref_total)
        s["label"] = lab
        s["records_before_demog_filter"] = n_all
        nd = d[d.DSI == 0]
        s["age_x_agi_wrong_share"] = tvd(wtable(nd, "AGERANGE", "agi_band"),
                                         wtable(ref_nd, "AGERANGE", "agi_band"))
        s["age_marginal_tvd"] = tvd(nd.groupby("AGERANGE")["weight"].sum() / nd.weight.sum(),
                                    ref_nd.groupby("AGERANGE")["weight"].sum() / ref_nd.weight.sum())
        for c in ["has_wages", "has_ss", "has_pension", "MARS"]:
            s[f"age_x_{c}_tvd"] = tvd(wtable(nd, "AGERANGE", c), wtable(ref_nd, "AGERANGE", c))
        rows.append(s)
    t = pd.DataFrame(rows).set_index("label")
    pd.set_option("display.width", 250, "display.max_columns", 40)
    print(("Records with demographics only. " if a.demog else "") +
          "Reference: training records. Age tables: non-dependent filers.")
    print(t.round(4).T.to_string())
    t.to_csv(a.out)


if __name__ == "__main__":
    main()
