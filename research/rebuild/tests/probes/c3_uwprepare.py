"""Probe C (test 4): unchanged copy of uw_prepare.py except that the source
dataset is the first argument and the output the second (used to build
full_v3cw = full_v3c plus PUFWT). Original notes:

Probe UW (test 4): build `full_v2w` = test 3's version 2 data plus the PUF
record's sampling weight (records column `weight`, S006/100) as a model
variable, PUFWT, observed on PUF rows only (-1, unobserved, on SCF rows).

PUFWT is coded as a categorical variable whose tokens are bands of the
weight. Each token decodes to the plain (unweighted) mean weight of the PUF
training records in its band, so the decoded weights of the training
records add up exactly to the PUF's total returns.

Band choice: if the PUF training weights take at most 200 distinct values,
each distinct value is its own token (exact). Otherwise bands are equal-count
(unweighted) quantile bands of the weight, with K the smallest of 50, 100,
150, 200 for which decoding every training record's weight by its band mean
moves AGI and the six main components in every AGI group by at most 0.5%
(else 200).

Round-trip check printed for the chosen K and the others: totals by AGI
group with true weights against band-mean weights. Aggregates only.

Usage: python uw_prepare.py <out dir>
"""
import json
import shutil
import sys
from pathlib import Path

import numpy as np
import pandas as pd

SRC = Path(sys.argv[1])
OUT = Path(sys.argv[2])
COMP = {"AGI": ["E00100"], "wages": ["E00200"], "cap_gains": ["P23250", "P22250"],
        "partnership_scorp": ["E26270"], "dividends": ["E00600"],
        "business_schC": ["E00900"], "interest": ["E00300"]}
GROUPS = [("bottom 50", 0, 0.5), ("50-90", 0.5, 0.9), ("90-99", 0.9, 0.99),
          ("99-99.9", 0.99, 0.999), ("top 0.1", 0.999, 1.01)]


def wq(x, w, q):
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o])
    return x[o][np.searchsorted(cw, q * cw[-1])]


def group_table(r, w, edges):
    agi = r["E00100"].to_numpy(float)
    out = {}
    for k, (lab, _, _) in enumerate(GROUPS):
        m = (agi >= edges[k]) & (agi < edges[k + 1])
        row = {"returns_m": w[m].sum() / 1e6}
        for c, cols in COMP.items():
            row[c] = sum((r[x].to_numpy(float)[m] * w[m]).sum() for x in cols) / 1e9
        out[lab] = row
    return out


def bands(w, K):
    """Equal-count band edges on the weight (ties kept in one band)."""
    qs = np.quantile(w, np.linspace(0, 1, K + 1))
    e = np.unique(qs)
    e[0], e[-1] = -np.inf, np.inf
    return e


def assign(w, e):
    return np.clip(np.searchsorted(e, w, side="right") - 1, 0, len(e) - 2)


def main():
    meta = json.load(open(SRC / "meta.json"))
    names = [v["name"] for v in meta["variables"]]
    if "PUFWT" in names:
        raise ValueError("PUFWT already in meta")
    recs = {s: pd.read_parquet(SRC / f"records_{s}.parquet") for s in ("train", "val", "test")}
    rt = recs["train"]
    puf = (rt["source"].astype(str) == "PUF").to_numpy()
    r = rt[puf].reset_index(drop=True)
    w = r["weight"].to_numpy(float)
    if not (np.isfinite(w).all() and (w > 0).all()):
        raise ValueError("PUF weights not finite and positive")
    print(f"PUF training records {len(w):,}; total weight {w.sum():,.0f}; ESS "
          f"{w.sum() ** 2 / (w ** 2).sum():,.0f}")
    print("weight quantiles (unweighted, 0,1,5,10,25,50,75,90,99,100%): " + ", ".join(
        f"{x:.2f}" for x in np.quantile(w, [0, .01, .05, .1, .25, .5, .75, .9, .99, 1])))
    nd = len(np.unique(w))
    print(f"distinct weight values among PUF training records: {nd:,}")
    for s in ("val", "test"):
        rs = recs[s]
        ws = rs.loc[rs["source"].astype(str) == "PUF", "weight"].to_numpy(float)
        print(f"{s}: PUF records {len(ws):,}, total weight {ws.sum():,.0f}, "
              f"distinct {len(np.unique(ws)):,}, share of values seen in train "
              f"{np.isin(ws, w).mean():.3f}")
    # scf side summary (for loss scaling bookkeeping)
    wscf = rt.loc[~puf, "weight"].to_numpy(float)
    print(f"SCF training rows {len(wscf):,}; total weight {wscf.sum():,.0f}; SCF share of "
          f"total record weight {wscf.sum() / (wscf.sum() + w.sum()):.4f}; SCF share of rows "
          f"{len(wscf) / len(rt):.4f}")

    agi = r["E00100"].to_numpy(float)
    edges_g = [-np.inf, wq(agi, w, 0.5), wq(agi, w, 0.9), wq(agi, w, 0.99), wq(agi, w, 0.999), np.inf]
    truth = group_table(r, w, edges_g)
    # weight share of the top groups among PUF records, sample counts
    for k, (lab, _, _) in enumerate(GROUPS):
        m = (agi >= edges_g[k]) & (agi < edges_g[k + 1])
        print(f"group {lab}: sample records {m.sum():,} ({m.mean():.2%} of the sample), "
              f"weight share {w[m].sum() / w.sum():.4%}")

    cands = {}
    if nd <= 200:
        vals = np.unique(w)
        e = np.concatenate([[-np.inf], (vals[1:] + vals[:-1]) / 2, [np.inf]])
        cands["exact"] = e
    for K in (50, 100, 150, 200):
        cands[K] = bands(w, K)
    choice, results = None, {}
    for key, e in cands.items():
        b = assign(w, e)
        nb = len(e) - 1
        mean = np.array([w[b == j].mean() if (b == j).any() else np.nan for j in range(nb)])
        if np.isnan(mean).any():
            raise ValueError(f"{key}: empty band")
        wd = mean[b]
        dec = group_table(r, wd, edges_g)
        worst = 0.0
        rel = {}
        for g in truth:
            rel[g] = {}
            for c in truth[g]:
                t, d = truth[g][c], dec[g][c]
                x = (d - t) / abs(t) if abs(t) > 1e-9 else 0.0
                rel[g][c] = x
                if c in COMP:
                    worst = max(worst, abs(x))
        a = np.clip(agi, 0, None)
        sh = lambda ww, q: (a * ww)[agi >= edges_g[q]].sum() / (a * ww).sum()
        results[str(key)] = dict(n_tokens=nb, worst_rel=worst, rel=rel, decoded=dec,
                                 top1=sh(wd, 3), top01=sh(wd, 4),
                                 total_agi_tn=(agi * wd).sum() / 1e12,
                                 ess=wd.sum() ** 2 / (wd ** 2).sum(),
                                 within_band_sd_share=float(np.sqrt(((w - wd) ** 2).mean()) / w.mean()))
        print(f"\nbands {key}: {nb} tokens; largest relative shift of AGI or a component in "
              f"any AGI group {worst:.4%}; total AGI ${results[str(key)]['total_agi_tn']:.4f}tn; "
              f"top 1% share {results[str(key)]['top1']:.4%}; top 0.1% share "
              f"{results[str(key)]['top01']:.4%}; ESS {results[str(key)]['ess']:,.0f}")
        if choice is None and (key == "exact" or worst <= 0.005):
            choice = key
    if choice is None:
        choice = 200
    a = np.clip(agi, 0, None)
    print(f"\ntrue weights: total AGI ${(agi * w).sum() / 1e12:.4f}tn; top 1% share "
          f"{(a * w)[agi >= edges_g[3]].sum() / (a * w).sum():.4%}; top 0.1% share "
          f"{(a * w)[agi >= edges_g[4]].sum() / (a * w).sum():.4%}")
    print(f"\nCHOSEN: {choice}")
    e = cands[choice]
    b = assign(w, e)
    nb = len(e) - 1
    mean = np.array([w[b == j].mean() for j in range(nb)])
    # round-trip table for the chosen bands
    dec = results[str(choice)]["decoded"]
    print("\nRound trip, chosen bands: $bn (returns in millions), true weights / band-mean weights")
    print("| AGI group | " + " | ".join(truth["top 0.1"].keys()) + " |")
    for g in truth:
        print(f"| {g} | " + " | ".join(f"{truth[g][c]:,.1f} / {dec[g][c]:,.1f}"
                                       for c in truth[g]) + " |")

    # ---- write the dataset ----
    OUT.mkdir(parents=True, exist_ok=True)
    col = len(meta["variables"])
    toks = []
    for j in range(nb):
        lo, hi = e[j], e[j + 1]
        toks.append({"token": j, "value": float(mean[j]),
                     "lo": float(w[b == j].min()), "hi": float(w[b == j].max()),
                     "n_train": int((b == j).sum())})
    var = {"name": "PUFWT", "group": "puf_only", "kind": "categorical", "vocab_size": nb,
           "tokens": toks, "column": col, "observed_in": ["PUF"],
           "description": "PUF sampling weight (records column `weight`, S006/100) as a model "
                          "variable; tokens are bands of the weight, each decoded to the plain "
                          "mean weight of the PUF training records in the band (probe UW)",
           "band_edges": [float(x) for x in e], "band_choice": str(choice)}
    meta["variables"].append(var)
    if "variable_names" in meta and isinstance(meta["variable_names"], list):
        meta["variable_names"].append("PUFWT")
    meta["generation_order"] = ["PUFWT"] + [x for x in meta["generation_order"] if x != "PUFWT"]
    meta["dataset"] = OUT.name
    meta["description"] = meta["description"] + " Probe UW: plus PUFWT, the PUF sampling weight as a model variable."
    meta["record_weight"] = ("records_*.parquet column `weight` (PUF: S006/100; SCF: implicate "
                             "weight); for PUF rows also the model variable PUFWT (banded)")
    for s in ("train", "val", "test"):
        rs = recs[s]
        ps = (rs["source"].astype(str) == "PUF").to_numpy()
        newcol = np.full(len(rs), -1, dtype=np.int64)
        newcol[ps] = assign(rs["weight"].to_numpy(float)[ps], e)
        for kind in ("tokens", "truth"):
            arr = np.load(SRC / f"{kind}_{s}.npy")
            if arr.shape[1] != col:
                raise ValueError(f"{kind}_{s}: {arr.shape[1]} columns, meta has {col}")
            out = np.concatenate([arr, newcol.astype(arr.dtype)[:, None]], axis=1)
            np.save(OUT / f"{kind}_{s}.npy", out)
        shutil.copy2(SRC / f"records_{s}.parquet", OUT / f"records_{s}.parquet")
        print(f"{s}: PUFWT token counts on PUF rows min {np.bincount(newcol[ps], minlength=nb).min()}"
              f", max {np.bincount(newcol[ps], minlength=nb).max()}")
    shutil.copy2(SRC / "quantiles.parquet", OUT / "quantiles.parquet")
    json.dump(meta, open(OUT / "meta.json", "w"), indent=1)
    json.dump({k: {kk: vv for kk, vv in v.items() if kk != "decoded"} | {"decoded": v["decoded"]}
               for k, v in results.items()} | {"truth": truth, "choice": str(choice)},
              open(OUT / "uw_roundtrip.json", "w"), indent=1, default=float)
    (OUT / "DONE").write_text(f"probe UW full_v2w built from {SRC}; PUFWT bands {choice}\n")
    print("written", OUT)


if __name__ == "__main__":
    main()
