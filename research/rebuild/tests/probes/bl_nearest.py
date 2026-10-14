"""Probe BL: partial copying at the top. For generated records with AGI at or
above the PUF's 99th (and 99.9th) percentile, find the single PUF training
record that shares the most of the generated record's non-zero amounts
exactly (same variable, same dollar value), among the 31 amounts. A tree
synthesizer copies each value from a real record, so a generated top record
can carry several of one real taxpayer's exact amounts even when the whole
record is not a copy. The same count on tokens (same variable, same rank
band) is reported for every file, because the transformer draws a dollar
amount at random inside a band and so almost never repeats a dollar value.
Real PUF validation records, which no model saw, give the rate expected by
coincidence.

Records examined per group: up to --max records, drawn with probability
proportional to the file's weight (so the unweighted tree arm, whose top
records carry small weights, is examined as a population). Aggregates only.

    python bl_nearest.py --out <json> LABEL=PATH ...   (PATH or `validation`)
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import as_agi  # noqa: E402

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s")


def wq(x, w, q):
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o])
    return x[o][min(np.searchsorted(cw, q * cw[-1]), len(x) - 1)]


def best_overlap(G, Gmask, T, chunk=40):
    """For each row of G: the largest number of masked positions equal to the
    same row of T, and the number of masked positions."""
    best = np.zeros(len(G), dtype=np.int64)
    for s in range(0, len(G), chunk):
        g, m = G[s:s + chunk], Gmask[s:s + chunk]
        eq = (g[:, None, :] == T[None, :, :]) & m[:, None, :]
        best[s:s + chunk] = eq.sum(axis=2).max(axis=1)
    return best, Gmask.sum(axis=1)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--out", required=True)
    p.add_argument("--max", type=int, default=1500)
    p.add_argument("items", nargs="+")
    a = p.parse_args()
    meta = json.load(open(DATA / "meta.json"))
    pv = [v for v in meta["variables"] if v["group"] in ("shared", "puf_only")]
    amts = [v["name"] for v in pv if v["kind"] == "amount"]
    aidx = [v["column"] for v in pv if v["kind"] == "amount"]
    rec = pd.read_parquet(DATA / "records_train.parquet")
    puf = (rec["source"].astype(str) == "PUF").to_numpy()
    tr = rec[puf].reset_index(drop=True)
    w0, agi0 = tr["weight"].to_numpy(float), tr["E00100"].to_numpy(float)
    thr = {"top 1": wq(agi0, w0, 0.99), "top 0.1": wq(agi0, w0, 0.999)}
    # Candidate donors: training records with AGI at or above the 90th percentile
    # (a top generated record's amounts come from high-income records).
    cand = agi0 >= wq(agi0, w0, 0.90)
    Td = tr.loc[cand, amts].to_numpy(np.float32)
    Tt = np.load(DATA / "tokens_train.npy")[puf][cand][:, aidx].astype(np.int16)
    print(f"candidate donors: {int(cand.sum())} training records with AGI >= 90th percentile", flush=True)
    res = {"thresholds": thr, "candidate_donors": int(cand.sum())}
    rng = np.random.default_rng(5)
    for item in a.items:
        lab, path = item.split("=", 1)
        if path == "validation":
            r = pd.read_parquet(DATA / "records_val.parquet")
            vm = (r["source"].astype(str) == "PUF").to_numpy()
            df = r[vm].reset_index(drop=True)
            tok = np.load(DATA / "tokens_val.npy")[vm][:, aidx].astype(np.int16)
            agi = df["E00100"].to_numpy(float)
        else:
            df = pd.read_parquet(path)
            df = df[df["draw"] == 0].reset_index(drop=True) if "draw" in df else df
            tok = df[[f"tok_{n}" for n in amts]].to_numpy(np.int16)
            agi = as_agi.agi_from_parts(df)
        w = df["weight"].to_numpy(float)
        D = df[amts].to_numpy(np.float32)
        out = {}
        for gl in ("top 1", "top 0.1"):
            idx = np.flatnonzero(agi >= thr[gl])
            if len(idx) > a.max:
                idx = rng.choice(idx, size=a.max, replace=False, p=w[idx] / w[idx].sum())
            mask = D[idx] != 0
            bd, nz = best_overlap(D[idx], mask, Td)
            bt, _ = best_overlap(tok[idx], mask, Tt)
            g = dict(records_examined=int(len(idx)), mean_nonzero_amounts=float(nz.mean()))
            for nm, b in (("dollars", bd), ("tokens", bt)):
                g[f"{nm}_mean_most_shared"] = float(b.mean())
                g[f"{nm}_share_3plus"] = float((b >= 3).mean())
                g[f"{nm}_share_5plus"] = float((b >= 5).mean())
                g[f"{nm}_share_all_nonzero"] = float(((b == nz) & (nz >= 3)).mean())
            out[gl] = g
        res[lab] = out
        print(f"done {lab}", flush=True)
    json.dump(res, open(a.out, "w"), indent=2)
    print("\nPartial copying at the top: for each generated record, the one training record that shares")
    print("the most of its non-zero amounts exactly. Shares are % of records examined.\n")
    print("| File | Group | Records examined | Non-zero amounts per record | Most shared, dollars (mean) | 3 or more shared, dollars | 5 or more, dollars | All (3 or more) shared, dollars | Most shared, tokens (mean) | 3 or more, tokens | 5 or more, tokens | All (3 or more), tokens |")
    print("|---" * 12 + "|")
    for l, o in res.items():
        if l == "thresholds" or not isinstance(o, dict) or "top 1" not in o:
            continue
        for gl in ("top 1", "top 0.1"):
            g = o[gl]
            print(f"| {l} | {gl} | {g['records_examined']:,} | {g['mean_nonzero_amounts']:.1f} | "
                  f"{g['dollars_mean_most_shared']:.2f} | {100 * g['dollars_share_3plus']:.1f} | "
                  f"{100 * g['dollars_share_5plus']:.1f} | {100 * g['dollars_share_all_nonzero']:.1f} | "
                  f"{g['tokens_mean_most_shared']:.2f} | {100 * g['tokens_share_3plus']:.1f} | "
                  f"{100 * g['tokens_share_5plus']:.1f} | {100 * g['tokens_share_all_nonzero']:.1f} |")


if __name__ == "__main__":
    main()
