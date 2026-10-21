"""Probe TR: copying check by AGI group. A tree copies every value from some
real record, and tr_synth.py keeps which one (the donor). For each generated
record: the largest number of its non-zero tax amounts copied from one PUF
training record, and the largest number of its non-zero wealth amounts
copied from one SCF household (any of its five implicates). Reported as the
weighted share of records with 3, 5 or 8 or more, overall and by AGI group
(the real PUF training records' thresholds). For completions, the wealth
side by AGI group of the completed PUF records. Aggregates only.

    python tr_copies.py > $Q/tables/tr_copies.md
"""
import glob
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tr_synth as ts  # noqa: E402

Q = ts.Q
GROUPS = ["all", "bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]


def wq(x, w, q):
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o]) / w.sum()
    return float(x[o][min(np.searchsorted(cw, q), len(x) - 1)])


def by_group(best, w, agi, edges):
    g = np.searchsorted(edges, agi, side="right")
    out = {}
    for k, lab in enumerate(GROUPS):
        m = np.ones(len(w), bool) if k == 0 else (g == k - 1)
        out[lab] = {f"{t}+": float(np.average(best[m] >= t, weights=w[m])) * 100 for t in (3, 5, 8)}
        out[lab]["records"] = int(m.sum())
    return out


def show(title, res):
    print(f"\n{title}\n")
    print("| AGI group | records | 3 or more (%) | 5 or more (%) | 8 or more (%) |")
    print("|---|---:|---:|---:|---:|")
    for lab, r in res.items():
        print(f"| {lab} | {r['records']:,} | {r['3+']:.2f} | {r['5+']:.3f} | {r['8+']:.3f} |")


def main():
    D = ts.Data()
    rec, tok = D.split("train")
    if "E00100" not in rec.columns:
        import c3_identities as ci
        tmp = rec[rec.source.astype(str) == "PUF"].copy()
        ci.add_totals(tmp)
        rec["E00100"] = np.nan
        rec.loc[tmp.index, "E00100"] = tmp["E00100"].to_numpy(float)
    puf = rec[rec.source.astype(str) == "PUF"]
    agi_r = puf["E00100"].to_numpy(float)
    wr = puf["weight"].to_numpy(float)
    edges = np.array([wq(agi_r, wr, q) for q in (0.5, 0.9, 0.99, 0.999)])
    src = rec["source"].astype(str).to_numpy()
    wc = [D.col[n] for n in D.wealth]
    m = (src == "SCF") & (tok[:, wc] >= 0).all(axis=1)
    hh = pd.factorize((rec["year"].astype(int).astype(str) + "_" + rec["hh_id"].astype(str)).to_numpy()[m])[0]
    for f in sorted(glob.glob(f"{Q}/gen/tr_joint_*_s*.parquet")):
        dn = np.load(f.replace(".parquet", "_donors.npz"))
        df = pd.read_parquet(f)
        order = list(dn["tax_order"])
        amts = [n for n in order if n not in D.cats]
        A = np.column_stack([df[a].to_numpy(float) for a in amts])
        Ad = np.column_stack([dn["tax_donor_rows"][:, order.index(a)] for a in amts])
        w = df.weight.to_numpy(float)
        agi = df.E00100.to_numpy(float)
        show(f"{os.path.basename(f)}: non-zero tax amounts copied from one PUF training record (of {len(amts)} amounts)",
             by_group(ts.copy_counts(Ad, np.nan_to_num(A) != 0), w, agi, edges))
        W = np.column_stack([df[x].to_numpy(float) for x in D.wealth])
        show(f"{os.path.basename(f)}: non-zero wealth amounts copied from one SCF household (of 24)",
             by_group(ts.copy_counts(hh[dn["wealth_donor_rows"]], np.nan_to_num(W) != 0), w, agi, edges))
    prow = np.flatnonzero(src == "PUF")
    agi_by_row = pd.Series(rec["E00100"].to_numpy(float), index=np.arange(len(rec)))
    for f in sorted(glob.glob(f"{Q}/comp/tr_puf_wealth_*.parquet")):
        dn = np.load(f.replace(".parquet", "_donors.npz"))
        df = pd.read_parquet(f, columns=["row", "weight"] + D.wealth)
        W = df[D.wealth].to_numpy(float)
        best = ts.copy_counts(hh[dn["wealth_donor_rows"]], np.nan_to_num(W) != 0)
        show(f"{os.path.basename(f)}: non-zero wealth amounts copied from one SCF household (of 24), five draws",
             by_group(best, df.weight.to_numpy(float), agi_by_row.loc[df.row].to_numpy(), edges))
    assert len(prow) > 0


if __name__ == "__main__":
    main()
