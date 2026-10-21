"""Hypothesis 2, third pass (test 1): income by age range, teacher-forced
versus observed versus free-running. Aggregates only.

Non-dependent filers. For each age range: share with wages, with Social
Security, with pensions, and with AGI under $25,000, and the age range's
share of weight —
  observed: validation records;
  teacher-forced: weighted mean over validation records of the model's
    predicted probability, given the true values of the variables before
    it in the default order (so the true age range is visible to the
    income variables, which come after it);
  free-running: generated files (mean over seeds).
Writes $S/diag2/income_by_age.csv.
"""
from pathlib import Path

import numpy as np
import pandas as pd
import torch

from diag2_calib import teacher_forced
from model import generation_order, load_checkpoint, load_meta

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
OUT = S / "diag2"
GEN = {"base": ["synthetic_base", "synthetic_base_s20261010", "synthetic_base_s20261011",
                "synthetic_base_s20261012"],
       "large": ["synthetic", "synthetic_large_s20261010", "synthetic_large_s20261011",
                 "synthetic_large_s20261012"],
       "base_demoglast": ["synthetic_base_demoglast"]}


def main():
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    meta = load_meta(S / "data" / "meta.json")
    vars_ = meta["variables"]
    names = [v["name"] for v in vars_]
    ix = {n: i for i, n in enumerate(names)}
    tv = np.load(S / "data" / "tokens_val.npy").astype(np.int64)
    w = pd.read_parquet(S / "data" / "values_val.parquet", columns=["weight"])["weight"].to_numpy()
    ja, jd = ix["AGERANGE"], ix["DSI"]
    dsi0 = [t["token"] for t in vars_[jd]["tokens"] if t["value"] == 0][0]
    agevals = [t["value"] for t in vars_[ja]["tokens"]]

    def zero_tok(j):
        return [t["token"] for t in vars_[j]["tokens"] if t["lo"] == 0 and t["hi"] == 0][0]

    def low_agi_toks():
        return np.array([t["token"] for t in vars_[ix["E00100"]]["tokens"] if t["hi"] <= 25000])

    meas = {"wages": ("E00200", "nonzero"), "Social Security": ("E02400", "nonzero"),
            "pensions": ("E01500", "nonzero"), "AGI under 25k": ("E00100", "low")}

    def indicator(tok, m):
        v, kind = meas[m]
        j = ix[v]
        if kind == "nonzero":
            return (tok[:, j] != zero_tok(j)).astype(float)
        return np.isin(tok[:, j], low_agi_toks()).astype(float)

    def prob(P, m):
        v, kind = meas[m]
        j = ix[v]
        if kind == "nonzero":
            return 1 - P[j][:, zero_tok(j)]
        return P[j][:, low_agi_toks()].sum(1)

    rows = []

    def add(source, tok, wts, vals):
        sel = tok[:, jd] == dsi0
        for k, a in enumerate(agevals):
            s = sel & (tok[:, ja] == k)
            r = dict(source=source, age=a, weight_share=wts[s].sum() / wts[sel].sum())
            for m in meas:
                r[m] = np.average(vals[m][s], weights=wts[s]) if s.any() else np.nan
            rows.append(r)

    add("observed (validation)", tv, w, {m: indicator(tv, m) for m in meas})
    for lab, stems in GEN.items():
        for st in stems:
            g = np.load(S / "model" / f"{st}_tokens.npy").astype(np.int64)
            gw = pd.read_parquet(S / "model" / f"{st}.parquet", columns=["weight"])["weight"].to_numpy()
            add(f"free:{lab}", g, gw, {m: indicator(g, m) for m in meas})
    order = generation_order(meta)
    for m_ in ("base", "large"):
        model, _ = load_checkpoint(S / "model" / f"checkpoint_{m_}.pt", meta, device)
        P = teacher_forced(model, tv, order, device)
        add(f"teacher_forced:{m_}", tv, w, {m: prob(P, m) for m in meas})
        del P, model
        torch.cuda.empty_cache()
    res = pd.DataFrame(rows).groupby(["age", "source"], sort=False).mean().reset_index()
    res.to_csv(OUT / "income_by_age.csv", index=False)
    pd.set_option("display.width", 250, "display.max_rows", 200)
    print((res.set_index(["age", "source"]) * 100).round(1).to_string())
    print("done", flush=True)


if __name__ == "__main__":
    main()
