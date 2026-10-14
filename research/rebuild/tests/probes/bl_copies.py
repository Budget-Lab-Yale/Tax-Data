"""Probe BL: how often generated PUF records are exact copies of a PUF
training record.

For each file (LABEL=PATH, a generated parquet in probe AS's format, or the
word `validation` for the PUF validation records, which are real records
the models never saw and so show how often a fresh draw from the same
population repeats a training record by coincidence), reports the share of
records (weighted by the file's weight; equal weights in every generated
file except the unweighted tree arm) that equal some training record:

  all 40     every one of the 40 PUF variables equal, in dollars
  amounts    the 31 amounts equal (categoricals free)
  main six   wages, long-term gains, short-term gains, partnership and S
             corporation, ordinary dividends, interest and Schedule C equal,
             counted only when at least one of them is non-zero
  tokens     every one of the 40 tokens equal (the transformer draws tokens
             and then a dollar amount inside the token's rank band, so this
             is its fairer copy measure)

overall, for records with AGI at or above the PUF's 99th percentile, and at
or above its 99.9th percentile (AGI as items less adjustments plus the
remainder). Also the number of distinct training records copied in the top
0.1%. Aggregates only.

    python bl_copies.py --out <json> LABEL=PATH ...
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
MAIN = ["E00200", "P23250", "P22250", "E26270", "E00600", "E00300", "E00900"]


def wq(x, w, q):
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o])
    return x[o][min(np.searchsorted(cw, q * cw[-1]), len(x) - 1)]


def row_hash(a):
    return pd.util.hash_pandas_object(pd.DataFrame(a), index=False).to_numpy()


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--out", required=True)
    p.add_argument("items", nargs="+")
    a = p.parse_args()
    meta = json.load(open(DATA / "meta.json"))
    pv = [v for v in meta["variables"] if v["group"] in ("shared", "puf_only")]
    names = [v["name"] for v in pv]
    amts = [v["name"] for v in pv if v["kind"] == "amount"]
    cols_idx = [meta["variable_names"].index(n) for n in names]
    rec = pd.read_parquet(DATA / "records_train.parquet")
    puf = (rec["source"].astype(str) == "PUF").to_numpy()
    tr = rec[puf].reset_index(drop=True)
    w0 = tr["weight"].to_numpy(float)
    agi0 = tr["E00100"].to_numpy(float)
    thr = {"top 1": wq(agi0, w0, 0.99), "top 0.1": wq(agi0, w0, 0.999)}
    ttok = np.load(DATA / "tokens_train.npy")[puf][:, cols_idx].astype(np.int64)

    def keys(df_vals, df_amts, df_main, tok):
        k = dict(all40=row_hash(df_vals), amounts=row_hash(df_amts), main=row_hash(df_main))
        if tok is not None:
            k["tokens"] = row_hash(tok)
        return k

    tk_ = keys(tr[names].to_numpy(float), tr[amts].to_numpy(float), tr[MAIN].to_numpy(float), ttok)
    tfirst = {m: pd.Series(np.arange(len(v)), index=v).groupby(level=0).first() for m, v in tk_.items()}
    res = {"thresholds": thr, "n_training_records": int(len(tr))}
    for item in a.items:
        lab, path = item.split("=", 1)
        if path == "validation":
            r = pd.read_parquet(DATA / "records_val.parquet")
            vm = (r["source"].astype(str) == "PUF").to_numpy()
            df = r[vm].reset_index(drop=True)
            tok = np.load(DATA / "tokens_val.npy")[vm][:, cols_idx].astype(np.int64)
            agi = df["E00100"].to_numpy(float)
        else:
            df = pd.read_parquet(path)
            if "draw" in df:
                df = df[df["draw"] == 0].reset_index(drop=True)
            tc = [f"tok_{n}" for n in names]
            tok = df[tc].to_numpy(np.int64) if all(c in df for c in tc) else None
            if tok is not None and (tok < 0).all(axis=0).any():   # tokens not written
                tok = None
            agi = as_agi.agi_from_parts(df)
        w = df["weight"].to_numpy(float)
        k = keys(df[names].to_numpy(float), df[amts].to_numpy(float), df[MAIN].to_numpy(float), tok)
        main_nonzero = (df[MAIN].to_numpy(float) != 0).any(axis=1)
        out = {"n_records": int(len(df))}
        for gl, m in [("all", np.ones(len(df), bool)), ("top 1", agi >= thr["top 1"]),
                      ("top 0.1", agi >= thr["top 0.1"])]:
            g = {"records": int(m.sum()), "weight_share_of_file": float(w[m].sum() / w.sum())}
            for meas, h in k.items():
                hit = np.isin(h, tk_[meas])
                if meas == "main":
                    hit &= main_nonzero
                g[f"copy_{meas}_weighted"] = float(w[m & hit].sum() / max(w[m].sum(), 1e-12))
                g[f"copy_{meas}_records"] = float((m & hit).sum() / max(m.sum(), 1))
                if gl == "top 0.1" and meas == "all40":
                    src = tfirst[meas].reindex(h[m & hit]).to_numpy()
                    g["distinct_training_records_copied"] = int(len(np.unique(src)))
            out[gl] = g
        res[lab] = out
        print(f"done {lab}", flush=True)
    json.dump(res, open(a.out, "w"), indent=2)
    labs = [k for k in res if isinstance(res[k], dict) and "all" in res[k]]
    print("\nShare of records that exactly copy a PUF training record (weighted, %)")
    print("| File | Group | Records | All 40 variables | 31 amounts | Main income items | 40 tokens | Distinct records copied |")
    print("|---|---|---|---|---|---|---|---|")
    for l in labs:
        for gl in ("all", "top 1", "top 0.1"):
            g = res[l][gl]
            tokc = f"{100 * g['copy_tokens_weighted']:.2f}" if "copy_tokens_weighted" in g else "n/a"
            print(f"| {l} | {gl} | {g['records']:,} | {100 * g['copy_all40_weighted']:.2f} | "
                  f"{100 * g['copy_amounts_weighted']:.2f} | {100 * g['copy_main_weighted']:.2f} | {tokc} | "
                  f"{g.get('distinct_training_records_copied', '')} |")


if __name__ == "__main__":
    main()
