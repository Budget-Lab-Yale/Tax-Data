"""Probe WT (test 4): build `full_v3cwt` = probe C's `full_v3c` (unknown
coding of non-itemizers' deductions, the dataset C's wealth results come
from) plus one model variable, SCFWT: the SCF record's survey weight
(records column `weight`, the implicate weight), observed on SCF rows and
unobserved (-1) on PUF rows. Mirror image of probe UW's PUFWT.

SCFWT is categorical; its tokens are equal-count bands of the weight among
SCF training rows (both waves pooled), each decoded to the plain mean weight
of the SCF training rows in its band. K = 50 bands unless the round-trip
check below fails for every K tried (50, 100, 200).

Round-trip check (aggregates only, printed): SCF training records with
wealth observed, by wave and by the seven-income total's weighted rank
(probe X's groups), mean net worth with the true weights against the
band-mean weights.

SCFWT is placed first in generation_order, so a fill of SCFWT plus the
wealth variables draws the weight first. Its meta group is "scf_weight"
(not "scf_only"), so scripts that list wealth variables by group skip it.

Usage: python wt_prepare.py <source dir> <out dir>
"""
import json
import shutil
import sys
from pathlib import Path

import numpy as np
import pandas as pd

SRC = Path(sys.argv[1])
OUT = Path(sys.argv[2])
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
T2_INC = ["t2_wages", "t2_business", "t2_int_div", "t2_capital_gains", "t2_rent", "t2_ss_pens", "t2_ui_other"]
EDGES = [0.5, 0.9, 0.99, 0.999]
LABELS = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]


def wrank(x, w):
    o = np.argsort(x, kind="mergesort")
    cw = np.cumsum(w[o])
    r = np.empty(len(x))
    r[o] = (cw - w[o] / 2) / cw[-1]
    return r


def bands(w, K):
    qs = np.quantile(w, np.linspace(0, 1, K + 1))
    e = np.unique(qs)
    e[0], e[-1] = -np.inf, np.inf
    return e


def assign(w, e):
    return np.clip(np.searchsorted(e, w, side="right") - 1, 0, len(e) - 2)


def main():
    meta = json.load(open(SRC / "meta.json"))
    names = [v["name"] for v in meta["variables"]]
    if "SCFWT" in names:
        raise ValueError("SCFWT already in meta")
    recs = {s: pd.read_parquet(SRC / f"records_{s}.parquet") for s in ("train", "val", "test")}
    rt = recs["train"]
    scf = (rt["source"].astype(str) == "SCF").to_numpy()
    s = rt[scf].reset_index(drop=True)
    w = s["weight"].to_numpy(float)
    if not (np.isfinite(w).all() and (w > 0).all()):
        raise ValueError("SCF weights not finite and positive")
    print(f"SCF training rows {len(w):,}; total weight {w.sum():,.0f}; ESS "
          f"{w.sum() ** 2 / (w ** 2).sum():,.0f}; distinct {len(np.unique(w)):,}")
    print("weight quantiles (unweighted, 0,1,5,10,25,50,75,90,99,100%): " + ", ".join(
        f"{x:.1f}" for x in np.quantile(w, [0, .01, .05, .1, .25, .5, .75, .9, .99, 1])))
    for y in sorted(s.year.unique()):
        m = (s.year == y).to_numpy()
        print(f"  year {y}: rows {m.sum():,}, total weight {w[m].sum():,.0f}")
    # where the sample sits: SCF rows by income group, share of rows against share of weight
    nw = np.full(len(s), np.nan)
    obs = s["wealth_observed"].to_numpy().astype(bool)
    nw[obs] = s.loc[obs, ASSETS].to_numpy(float).sum(1) - s.loc[obs, DEBTS].to_numpy(float).sum(1)
    grp = np.zeros(len(s), int)
    for y in sorted(s.year.unique()):
        m = (s.year == y).to_numpy()
        grp[m] = np.searchsorted(EDGES, wrank(s.loc[m, T2_INC].to_numpy(float).sum(1), w[m]), side="right")
    for k, lab in enumerate(LABELS):
        m = grp == k
        print(f"income group {lab}: SCF rows {m.sum():,} ({m.mean():.2%} of rows), weight share "
              f"{w[m].sum() / w.sum():.4%}, median weight {np.median(w[m]):,.1f}")

    def nw_table(ww):
        out = {}
        for y in sorted(s.year.unique()):
            for k, lab in enumerate(LABELS):
                m = (s.year == y).to_numpy() & (grp == k) & obs
                out[f"{y}|{lab}"] = float((ww[m] * nw[m]).sum() / ww[m].sum())
        return out

    truth = nw_table(w)
    choice = None
    res = {}
    for K in (50, 100, 200):
        e = bands(w, K)
        b = assign(w, e)
        nb = len(e) - 1
        mean = np.array([w[b == j].mean() for j in range(nb)])
        dec = nw_table(mean[b])
        worst = max(abs(dec[k] / truth[k] - 1) for k in truth)
        res[K] = dict(n_tokens=nb, worst=worst, decoded=dec)
        print(f"\nK={K}: {nb} tokens; largest relative change in mean net worth by wave and income "
              f"group {worst:.3%}")
        for k in truth:
            print(f"  {k}: true weights ${truth[k] / 1e6:,.3f}m, band-mean weights ${dec[k] / 1e6:,.3f}m")
        if choice is None and worst <= 0.02:
            choice = K
    if choice is None:
        choice = 200
    print(f"\nCHOSEN K = {choice}")
    e = bands(w, choice)
    b = assign(w, e)
    nb = len(e) - 1
    mean = np.array([w[b == j].mean() for j in range(nb)])

    OUT.mkdir(parents=True, exist_ok=True)
    col = len(meta["variables"])
    toks = [{"token": j, "value": float(mean[j]), "lo": float(w[b == j].min()),
             "hi": float(w[b == j].max()), "n_train": int((b == j).sum())} for j in range(nb)]
    meta["variables"].append({
        "name": "SCFWT", "group": "scf_weight", "kind": "categorical", "vocab_size": nb,
        "tokens": toks, "column": col, "observed_in": ["SCF"],
        "description": "SCF survey weight (records column `weight`, implicate weight) as a model "
                       "variable; equal-count bands among SCF training rows, each decoded to the "
                       "plain mean weight of its band (probe WT)",
        "band_edges": [float(x) for x in e], "band_choice": str(choice)})
    if isinstance(meta.get("variable_names"), list):
        meta["variable_names"].append("SCFWT")
    meta["generation_order"] = ["SCFWT"] + [x for x in meta["generation_order"] if x != "SCFWT"]
    meta["dataset"] = OUT.name
    meta["description"] = meta["description"] + " Probe WT: plus SCFWT, the SCF weight as a model variable."
    for sp in ("train", "val", "test"):
        rs = recs[sp]
        ms = (rs["source"].astype(str) == "SCF").to_numpy()
        newcol = np.full(len(rs), -1, dtype=np.int64)
        newcol[ms] = assign(rs["weight"].to_numpy(float)[ms], e)
        for kind in ("tokens", "truth"):
            arr = np.load(SRC / f"{kind}_{sp}.npy")
            if arr.shape[1] != col:
                raise ValueError(f"{kind}_{sp}: {arr.shape[1]} columns, meta has {col}")
            np.save(OUT / f"{kind}_{sp}.npy",
                    np.concatenate([arr, newcol.astype(arr.dtype)[:, None]], axis=1))
        shutil.copy2(SRC / f"records_{sp}.parquet", OUT / f"records_{sp}.parquet")
        bc = np.bincount(newcol[ms], minlength=nb)
        print(f"{sp}: SCF rows {ms.sum():,}; SCFWT token counts min {bc.min()}, max {bc.max()}")
    shutil.copy2(SRC / "quantiles.parquet", OUT / "quantiles.parquet")
    json.dump(meta, open(OUT / "meta.json", "w"), indent=1)
    json.dump({"truth": truth, "results": {str(k): v for k, v in res.items()}, "choice": choice},
              open(OUT / "wt_roundtrip.json", "w"), indent=1)
    (OUT / "DONE").write_text(f"probe WT full_v3cwt built from {SRC}; SCFWT bands {choice}\n")
    print("written", OUT)


if __name__ == "__main__":
    main()
