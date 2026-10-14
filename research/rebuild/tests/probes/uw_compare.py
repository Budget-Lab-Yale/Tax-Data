"""Probe UW: copy of t_compare.py (probe T). Changes: the data is `full_v2w`;
an item PATH:gen is a generated file whose records are weighted by their
drawn PUF weight (column PUFWT), rescaled so the weights add up to the PUF
training records' total returns; for such files the effective sample size
and the distribution of the drawn weight are also reported. Other items
use the file's `weight` column as before (completed PUF training records
carry their true weight).

Original usage of t_compare.py: python t_compare.py --out <json> LABEL=PATH[:tilt=SPEC] ...
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
DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/full_v2w")
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


WBANDS = [(0, 2), (2, 20), (20, 200), (200, 1000), (1000, 1e9)]


def wdist(wv, agi, thr, cuts, wpop=None):
    """Distribution of the weight variable across records (unweighted shares)
    and records per AGI group, for a file's weights before rescaling. The
    effective sample size is of the weights used for population statistics
    (wpop; default the weight variable itself)."""
    n = len(wv)
    we = wv if wpop is None else wpop
    out = dict(n_records=n, ess=float(we.sum() ** 2 / (we ** 2).sum()),
               ess_share=float(we.sum() ** 2 / (we ** 2).sum() / n),
               mean_weight=float(wv.mean()))
    out["records_share_by_weight_band"] = {f"{lo}-{hi}": float(((wv >= lo) & (wv < hi)).mean())
                                           for lo, hi in WBANDS}
    edges = [-np.inf, cuts[0.5], cuts[0.9], thr[0.99], thr[0.999], np.inf]
    out["records_share_by_agi_group"] = {lab: float(((agi >= edges[k]) & (agi < edges[k + 1])).mean())
                                         for k, (lab, _, _) in enumerate(GROUPS)}
    out["mean_weight_by_agi_group"] = {
        lab: float(wv[(agi >= edges[k]) & (agi < edges[k + 1])].mean())
        if ((agi >= edges[k]) & (agi < edges[k + 1])).any() else float("nan")
        for k, (lab, _, _) in enumerate(GROUPS)}
    vals, cnt = np.unique(wv, return_counts=True)
    out["values"] = {f"{v:.6g}": int(c) for v, c in zip(vals, cnt)}
    return out


def leak(wv, agi, wpop, thr):
    """By band of the drawn (or true) sampling weight: share of records with
    AGI above the true 99th and 99.9th percentiles and below -$100,000, and
    the population-weighted AGI ($bn) of records with negative AGI."""
    out = {}
    for lo, hi in WBANDS:
        m = (wv >= lo) & (wv < hi)
        if not m.any():
            continue
        out[f"{lo}-{hi}"] = dict(
            records_share=float(m.mean()),
            share_agi_above_p99=float((agi[m] >= thr[0.99]).mean()),
            share_agi_above_p999=float((agi[m] >= thr[0.999]).mean()),
            share_agi_below_minus100k=float((agi[m] < -1e5).mean()),
            negative_agi_bn=float((np.minimum(agi[m], 0) * wpop[m]).sum() / 1e9),
            agi_above_p999_bn=float((agi[m] * wpop[m])[agi[m] >= thr[0.999]].sum() / 1e9))
    return out


def tv_distance(a, b):
    keys = set(a) | set(b)
    na, nb = sum(a.values()), sum(b.values())
    return 0.5 * sum(abs(a.get(k, 0) / na - b.get(k, 0) / nb) for k in keys)


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
    res["PUF training records"]["weight_dist"] = wdist(w0, agi0, thr, cuts)
    res["PUF training records"]["leak"] = leak(w0, agi0, w0, thr)
    for item in a.items:
        lab, rest = item.split("=", 1)
        gen = rest.endswith(":gen") or rest.endswith(":gensqrt")
        gsqrt = rest.endswith(":gensqrt")
        if gen:
            rest = rest[:rest.rindex(":")]
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
            if gen:
                wv = df["PUFWT"].to_numpy(float)
                if not (np.isfinite(wv).all() and (wv > 0).all()):
                    raise ValueError(f"{lab}: {int((~np.isfinite(wv)).sum())} missing, "
                                     f"{int((wv <= 0).sum())} non-positive drawn weights")
                if gsqrt:
                    # model trained with PUF loss weight sqrt(weight): the population
                    # is the generated records weighted by sqrt(drawn weight)
                    wv = np.sqrt(wv)
                w = wv * w0.sum() / wv.sum()
            if opt:
                f = tilt_factor(meta, df["tok_E00100"].to_numpy(), opt)
                w = w / f
                w = w * w0.sum() / w.sum()
        res[lab] = stats(df, w, thr, cuts)
        if gen:
            res[lab]["leak"] = leak(df["PUFWT"].to_numpy(float), df["E00100"].to_numpy(float), w, thr)
            res[lab]["weight_dist"] = wdist(df["PUFWT"].to_numpy(float),
                                            df["E00100"].to_numpy(float), thr, cuts, wpop=w)
            ref = res["PUF training records"]["weight_dist"]["values"]
            if gsqrt:
                # the model learns the sample tilted by sqrt(weight)
                ref = {k: c * float(k) ** 0.5 for k, c in ref.items()}
            res[lab]["weight_dist"]["tv_distance_to_puf_sample"] = tv_distance(
                res[lab]["weight_dist"]["values"], ref)
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
    six = ["wages", "cap_gains", "partnership_scorp", "dividends", "interest", "business_schC"]
    print("| six components, top 0.1, $bn | " + " | ".join(
        f"{sum(res[l]['groups']['top 0.1'][c + '_bn'] for c in six):,.0f}" for l in labs) + " |")
    print("| six components, 99-99.9, $bn | " + " | ".join(
        f"{sum(res[l]['groups']['99-99.9'][c + '_bn'] for c in six):,.0f}" for l in labs) + " |")
    wl = [l for l in labs if "weight_dist" in res[l]]
    print("\nweight variable: records, effective sample size, share of records by weight band and AGI group")
    print("| | " + " | ".join(wl) + " |")
    for key in ["n_records", "ess", "ess_share", "mean_weight", "tv_distance_to_puf_sample"]:
        print(f"| {key} | " + " | ".join(
            (f"{res[l]['weight_dist'][key]:,.3f}" if key in res[l]["weight_dist"] else "-") for l in wl) + " |")
    for sub in ["records_share_by_weight_band", "records_share_by_agi_group", "mean_weight_by_agi_group"]:
        for k in res["PUF training records"]["weight_dist"][sub]:
            print(f"| {sub} {k} | " + " | ".join(f"{res[l]['weight_dist'][sub][k]:.4f}" for l in wl) + " |")
    for c in ["agi_bn", "weight_share"] + [k + "_bn" for k in COMP]:
        print(f"\n{c} by AGI group")
        print("| group | " + " | ".join(labs) + " |")
        for gl, _, _ in GROUPS:
            print(f"| {gl} | " + " | ".join(
                (f"{res[l]['groups'][gl][c]:.4f}" if c == "weight_share"
                 else f"{res[l]['groups'][gl][c]:,.0f}") for l in labs) + " |")


if __name__ == "__main__":
    main()
