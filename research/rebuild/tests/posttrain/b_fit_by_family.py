"""Piece B: target fit of a method-1 weighting, by family (aggregates only).

    python b_fit_by_family.py scf <weights.npz> [--draws ref_draws.npz]
    python b_fit_by_family.py P <weights.npz> --version full --set wcore

For every positive-target cell: relative error |achieved - target| / target,
and the error as a share of the cell's group total (the sum of the targets
over the bins of the same wave/table, family, group and variable). Prints
one Markdown table: per family, the share of cells (and of target weight)
within 1e-6, 1e-3 and 1e-2 of target, and the mean and largest error as a
share of the group total.
"""
import argparse
import os
import sys
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import a_common as A      # noqa: E402


def table(cells, t, ach, status):
    keys = [c for c in ("wave", "table", "family", "group", "variable") if c in cells.columns]
    gt = cells.assign(_t=np.nan_to_num(t)).groupby(keys)["_t"].transform("sum").to_numpy()
    pos = np.nan_to_num(t) > 0
    rel = np.where(pos, np.abs(ach - t) / np.where(pos, t, 1), np.nan)
    rg = np.where(gt > 0, np.abs(ach - np.nan_to_num(t)) / np.where(gt > 0, gt, 1), np.nan)
    rows = []
    for fam, idx in cells.groupby("family").indices.items():
        m = pos[idx]
        r, w, g = rel[idx][m], t[idx][m], rg[idx][m]
        row = {"family": fam, "positive cells": int(m.sum())}
        for tol in (1e-6, 1e-3, 1e-2):
            row[f"cells within {tol:g}"] = f"{(r <= tol).mean():.1%}"
        row["target weight within 1e-3"] = f"{w[r <= 1e-3].sum() / w.sum():.1%}"
        row["mean error, share of group total"] = f"{np.mean(g):.2e}"
        row["largest error, share of group total"] = f"{np.max(g):.2e}"
        rows.append(row)
    df = pd.DataFrame(rows)
    cols = list(df.columns)
    out = ["| " + " | ".join(cols) + " |", "|" + "---|" * len(cols)]
    for _, rr in df.iterrows():
        out.append("| " + " | ".join(str(rr[c]) for c in cols) + " |")
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("arm", choices=["scf", "P"])
    ap.add_argument("weights")
    ap.add_argument("--draws", default=None)
    ap.add_argument("--version", default="full")
    ap.add_argument("--set", default="wcore")
    a = ap.parse_args()
    z = np.load(a.weights, allow_pickle=False)
    w = z["w"]
    status = z["cell_status"]
    K = w.shape[1]
    if a.arm == "scf":
        recs, cells = A.load_records(), A.load_targets()
        tok = np.load(a.draws or str(A.PA / "ref_draws.npz"))["tokens"][:, :K]
        idx = A.contrib_index(tok, recs)
        ach = A.cell_totals(idx, w.reshape(-1), len(cells))
    else:
        import a_full as AF
        from b_full import build_index
        A.configure_full(a.version)
        recs = AF.load_puf_records()
        ts = AF.rebuild_set(a.set)
        cells = pd.read_parquet(A.PA / f"targets_{a.set}.parquet")
        G = np.stack([recs[f"g_{a.set}_{fam.name}"].to_numpy() for fam, _ in ts.tables], axis=1)
        tok = np.load(a.draws or str(A.PA / "ref_draws_P.npz"))["tokens"][:, :K]
        idx = build_index(ts, recs, G, tok, AF.Spec(False).wealth)
        ach = ts.totals(idx, w.reshape(-1))
    t = cells["target"].to_numpy(np.float64)
    print(f"weights {a.weights}, K = {K}")
    print()
    print(table(cells, t, ach, status))


if __name__ == "__main__":
    main()
