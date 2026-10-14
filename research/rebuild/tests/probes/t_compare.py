"""Probe T: levels table, real PUF against generated or completed PUF files.

Usage: python t_compare.py --out <json> LABEL=PATH[:tilt=SPEC] ...
  PATH: a parquet from h_complete.py (generate or complete mode), or the
  word `roundtrip` (the PUF training records' own tokens decoded back to
  dollars with g_tokenizer.decode).
  tilt=SPEC (e.g. 0.9:3,0.99:10,0.999:30): the file was generated from a
  model trained with t_train.py --top-tilt SPEC; each record is weighted by
  1 / factor of its AGI token, rescaled to the PUF total weight.

Every statistic uses the real PUF training records' thresholds: weighted
percentiles of AGI (E00100) at 50, 90, 99 and 99.9. Records are grouped by
their own AGI against those thresholds. Aggregates only are printed.
"""
import argparse
import importlib.util
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
DATA = Path(f"{T2}/model/full_v2/data")
PT = Path(__file__).resolve().parent.parent / "posttrain"
COMP = {"wages": ["E00200"], "cap_gains": ["P23250", "P22250"],
        "partnership_scorp": ["E26270"], "dividends": ["E00600"],
        "business_schC": ["E00900"], "interest": ["E00300"]}
GROUPS = [("bottom 50", 0, 0.5), ("50-90", 0.5, 0.9), ("90-99", 0.9, 0.99),
          ("99-99.9", 0.99, 0.999), ("top 0.1", 0.999, 1.01)]


def wq(x, w, q):
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o])
    return x[o][np.searchsorted(cw, q * cw[-1])]


def tilt_factor(meta, tok, spec):
    edges = sorted((float(a.split(":")[0]), float(a.split(":")[1])) for a in spec.split(","))
    v = [x for x in meta["variables"] if x["name"] == "E00100"][0]
    f = np.ones(len(tok))
    for t in v["tokens"]:
        if t.get("kind") != "pos":
            continue
        fac = 1.0
        for e, c in edges:
            if float(t["lo"]) >= e - 1e-9:
                fac = c
        f[tok == int(t["token"])] = fac
    return f


def stats(df, w, thr, cuts):
    agi = df["E00100"].to_numpy(float)
    a = np.clip(agi, 0, None)
    A = (a * w).sum()
    out = dict(total_agi_tn=float((agi * w).sum() / 1e12),
               returns_m=float(w.sum() / 1e6),
               w_above_p99=float(w[agi >= thr[0.99]].sum() / w.sum()),
               w_above_p999=float(w[agi >= thr[0.999]].sum() / w.sum()),
               share_above_p99=float((a * w)[agi >= thr[0.99]].sum() / A),
               share_above_p999=float((a * w)[agi >= thr[0.999]].sum() / A))
    if "tok_E00100" in df:
        t = df["tok_E00100"].to_numpy()
        out["w_tok_rank_ge_0.99"] = float(w[t >= 68].sum() / w.sum())
        out["w_tok_rank_ge_0.999"] = float(w[t >= 77].sum() / w.sum())
    g = {}
    edges = [-np.inf, cuts[0.5], cuts[0.9], thr[0.99], thr[0.999], np.inf]
    for k, (lab, _, _) in enumerate(GROUPS):
        m = (agi >= edges[k]) & (agi < edges[k + 1])
        row = dict(weight_share=float(w[m].sum() / w.sum()),
                   agi_bn=float((agi[m] * w[m]).sum() / 1e9))
        for c, cols in COMP.items():
            row[c + "_bn"] = float(sum((df[x].to_numpy(float)[m] * w[m]).sum() for x in cols) / 1e9)
        g[lab] = row
    out["groups"] = g
    out["totals_bn"] = {c: float(sum((df[x].to_numpy(float) * w).sum() for x in cols) / 1e9)
                        for c, cols in COMP.items()}
    return out


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--out", required=True)
    p.add_argument("items", nargs="+")
    a = p.parse_args()
    meta = json.load(open(DATA / "meta.json"))
    rec = pd.read_parquet(DATA / "records_train.parquet")
    puf = (rec["source"].astype(str) == "PUF").to_numpy()
    r = rec[puf].reset_index(drop=True)
    w0 = r["weight"].to_numpy(float)
    agi0 = r["E00100"].to_numpy(float)
    thr = {q: wq(agi0, w0, q) for q in (0.99, 0.999)}
    cuts = {q: wq(agi0, w0, q) for q in (0.5, 0.9)}
    names = [v["name"] for v in meta["variables"]]
    tok_all = np.load(DATA / "tokens_train.npy")[puf]
    r = r.copy()
    r["tok_E00100"] = tok_all[:, names.index("E00100")]
    res = {"thresholds": {"p50": cuts[0.5], "p90": cuts[0.9], "p99": thr[0.99],
                          "p99.9": thr[0.999]},
           "PUF training records": stats(r, w0, thr, cuts)}
    for item in a.items:
        lab, rest = item.split("=", 1)
        path, _, opt = rest.partition(":tilt=")
        if path == "roundtrip":
            spec = importlib.util.spec_from_file_location("g_tok", PT / "g_tokenizer.py")
            g = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(g)
            q = pd.read_parquet(DATA / "quantiles.parquet")
            dec = g.decode(tok_all, meta, r[["source", "year"]], q, np.random.default_rng(7))
            dec["tok_E00100"] = r["tok_E00100"].to_numpy()
            df, w = dec, w0
        else:
            df = pd.read_parquet(path)
            if "draw" in df:
                df = df[df["draw"] == 0].reset_index(drop=True)
            w = df["weight"].to_numpy(float).copy()
            if opt:
                f = tilt_factor(meta, df["tok_E00100"].to_numpy(), opt)
                w = w / f
                w = w * w0.sum() / w.sum()
        res[lab] = stats(df, w, thr, cuts)
        if opt:
            res[lab]["effective_records_share"] = float(w.sum() ** 2 / (w ** 2).sum() / len(w))
        print(f"done {lab}", flush=True)
    json.dump(res, open(a.out, "w"), indent=2)
    labs = [k for k in res if k != "thresholds"]
    print("\n| | " + " | ".join(labs) + " |")
    print("|---" * (len(labs) + 1) + "|")
    for key, fmt in [("total_agi_tn", "${:.2f}tn"), ("w_above_p99", "{:.2%}"),
                     ("w_above_p999", "{:.3%}"), ("share_above_p99", "{:.1%}"),
                     ("share_above_p999", "{:.1%}"), ("w_tok_rank_ge_0.99", "{:.2%}"),
                     ("w_tok_rank_ge_0.999", "{:.3%}")]:
        print(f"| {key} | " + " | ".join(fmt.format(res[l][key]) if key in res[l] else "-"
                                        for l in labs) + " |")
    for c in ["agi_bn", "weight_share"] + [k + "_bn" for k in COMP]:
        print(f"\n{c} by AGI group")
        print("| group | " + " | ".join(labs) + " |")
        for gl, _, _ in GROUPS:
            print(f"| {gl} | " + " | ".join(
                (f"{res[l]['groups'][gl][c]:.4f}" if c == "weight_share"
                 else f"{res[l]['groups'][gl][c]:,.0f}") for l in labs) + " |")


if __name__ == "__main__":
    main()
