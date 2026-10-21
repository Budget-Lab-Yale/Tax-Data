"""Fabricate a small dataset in the SPEC.md layout, for developing piece D.

Nothing here is real data. It writes $S/dev/d/fake_expA/ with the same files
piece A writes for the real datasets (meta.json, tokens_*, truth_*,
records_*, quantiles.parquet), so the comparison methods can be written and
tested before A's files exist. The token layout follows SPEC.md "Units:
ranks"; where SPEC.md leaves a detail open, this file makes a guess, and the
D code reads meta.json rather than relying on the guess.

Run through sbatch only (d_dev.sbatch).
"""

import json
import os
import sys

import numpy as np
import pandas as pd

S = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(S, "dev/d/fake_expA")

INCOMES = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens",
           "ui_other"]
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities',
          'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
          're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards',
         'installment_debt', 'other_debt']
KG = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
WEALTH = ASSETS + DEBTS + KG

GRID = np.unique(np.round(np.concatenate([
    np.arange(0.02, 0.9001, 0.02), np.arange(0.91, 0.9901, 0.01),
    np.arange(0.991, 0.99901, 0.001), [0.9999, 1.0]]), 6))
NEG_GRID = np.round(np.arange(0.1, 1.0001, 0.1), 6)


def wrank(x, w):
    """Weighted mid-rank in (0, 1] of each element of x among x."""
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o]) / w.sum()
    r = np.empty_like(cw)
    r[o] = cw
    return r


def amount_tokens(var_name, has_neg):
    toks = []
    if has_neg:
        lo = 0.0
        for hi in NEG_GRID:   # negative bins: rank of |x| among negatives
            toks.append({"kind": "negative", "lo": float(lo), "hi": float(hi)})
            lo = hi
    toks.append({"kind": "zero"})
    lo = 0.0
    for hi in GRID:
        toks.append({"kind": "positive", "lo": float(lo), "hi": float(hi)})
        lo = hi
    return toks


def encode_amount(x, w, toks, group_keys):
    """Token per value; ranks within each group (source-year)."""
    out = np.full(len(x), -1, dtype=np.int32)
    zero_tok = [i for i, t in enumerate(toks) if t["kind"] == "zero"][0]
    pos_idx = [i for i, t in enumerate(toks) if t["kind"] == "positive"]
    neg_idx = [i for i, t in enumerate(toks) if t["kind"] == "negative"]
    pos_hi = np.array([toks[i]["hi"] for i in pos_idx])
    neg_hi = np.array([toks[i]["hi"] for i in neg_idx]) if neg_idx else None
    for g in np.unique(group_keys):
        m = group_keys == g
        xs, ws = x[m], w[m]
        o = np.full(m.sum(), -1, dtype=np.int32)
        o[xs == 0] = zero_tok
        p = xs > 0
        if p.any():
            r = wrank(xs[p], ws[p])
            o[p] = np.array(pos_idx)[np.minimum(np.searchsorted(pos_hi, r - 1e-12), len(pos_idx) - 1)]
        n = xs < 0
        if n.any():
            r = wrank(-xs[n], ws[n])
            o[n] = np.array(neg_idx)[np.minimum(np.searchsorted(neg_hi, r - 1e-12), len(neg_idx) - 1)]
        out[m] = o
    return out


def main():
    rng = np.random.default_rng(1)
    n_hh = 3000
    hh = np.arange(1, n_hh + 1)
    # Latent permanent income and wealth propensity per household
    li = rng.normal(0, 1, n_hh)
    lw = 0.8 * li + rng.normal(0, 0.8, n_hh)
    age = rng.integers(25, 85, n_hh)
    married = rng.random(n_hh) < 0.5
    ndep = rng.integers(0, 4, n_hh)
    hw = np.exp(rng.normal(8, 1.0, n_hh) - 1.2 * np.clip(lw, 0, None))  # oversampled rich
    rows = []
    for i in range(n_hh):
        n_units = 1 + (rng.random() < 0.15)
        for u in range(1, n_units + 1):
            for imp in range(1, 6):
                e = rng.normal(0, 0.2)
                r = {"hh_id": int(hh[i]), "unit": u, "implicate": imp,
                     "weight": hw[i] / 5,
                     "age": int(age[i] if u == 1 else rng.integers(18, 30)),
                     "married": int(married[i] and u == 1),
                     "n_dep": int(min(ndep[i], 3) if u == 1 else 0)}
                inc = np.exp(10.5 + li[i] + e)
                r["wages"] = inc * (rng.random() < 0.8) * (age[i] < 70)
                r["business"] = inc * 0.3 * rng.normal(1, 1) * (rng.random() < 0.15)
                r["int_div"] = np.exp(lw[i] * 1.5 + 6 + e) * (rng.random() < 0.4 + 0.2 * (lw[i] > 0))
                r["capital_gains"] = np.exp(lw[i] * 2 + 7) * rng.normal(1, 0.7) * (rng.random() < 0.15)
                r["rent"] = 5000 * rng.normal(1, 1.5) * (rng.random() < 0.08)
                r["ss_pens"] = 20000 * np.exp(e) * (age[i] >= 65)
                r["ui_other"] = 3000 * (rng.random() < 0.1)
                wl = np.exp(11 + 1.5 * lw[i] + e)
                for k, v in enumerate(WEALTH):
                    hold = rng.random() < (0.2 + 0.5 / (1 + np.exp(-lw[i] + 0.1 * k)))
                    r[v] = float(round(wl * np.exp(rng.normal(-k * 0.05, 0.5)) * hold))
                r["kg_pass_throughs"] *= rng.choice([1, -1], p=[0.8, 0.2])
                r["kg_primary_home"] *= rng.choice([1, -1], p=[0.9, 0.1])
                rows.append(r)
    df = pd.DataFrame(rows)
    for v in INCOMES:
        df[v] = np.round(df[v])
    df["source"] = "SCF"
    df["year"] = 2016
    # Split households into halves (seed 20261009), half 1 = pretend PUF.
    srng = np.random.default_rng(20261009)
    h1 = set(srng.choice(hh, n_hh // 2, replace=False).tolist())
    df["half"] = np.where(df.hh_id.isin(h1), 1, 2)
    df.loc[df.half == 1, "source"] = "SCF_H1"
    val_hh = set(srng.choice(hh, n_hh // 10, replace=False).tolist())
    df["split"] = np.where(df.hh_id.isin(val_hh), "val", "train")
    age_bins = [0, 26, 35, 45, 55, 65, 200]
    df["age_range"] = np.digitize(df.age, age_bins[1:-1]) + 1
    # SCF weight variable: 64 quantile bins of log weight
    lwt = np.log(df.weight.values)
    wq = np.quantile(lwt[df.half == 2], np.linspace(0, 1, 65))
    wtok = np.clip(np.searchsorted(wq, lwt, side="right") - 1, 0, 63)

    groups = (df.source.map({"SCF": "SCF", "SCF_H1": "SCF_H1"}) + "_" + df.year.astype(str)).values
    variables = [
        {"name": "age_range", "group": "shared", "kind": "categorical",
         "vocab_size": 6, "tokens": [{"value": k} for k in range(1, 7)]},
        {"name": "married", "group": "shared", "kind": "categorical",
         "vocab_size": 2, "tokens": [{"value": 0}, {"value": 1}]},
        {"name": "n_dep", "group": "shared", "kind": "categorical",
         "vocab_size": 4, "tokens": [{"value": k} for k in range(4)]},
    ]
    cols = {"age_range": df.age_range.values - 1, "married": df.married.values,
            "n_dep": df.n_dep.values}
    for v in INCOMES:
        has_neg = bool((df[v] < 0).any())
        toks = amount_tokens(v, has_neg)
        variables.append({"name": v, "group": "shared", "kind": "amount",
                          "vocab_size": len(toks), "tokens": toks})
        cols[v] = encode_amount(df[v].values, df.weight.values, toks, groups)
    variables.append({"name": "scf_weight", "group": "scf_only", "kind": "weight",
                      "vocab_size": 64,
                      "tokens": [{"lo": float(np.exp(wq[k])), "hi": float(np.exp(wq[k + 1]))}
                                 for k in range(64)]})
    cols["scf_weight"] = wtok
    for v in WEALTH:
        has_neg = bool((df[v] < 0).any())
        toks = amount_tokens(v, has_neg)
        variables.append({"name": v, "group": "scf_only", "kind": "amount",
                          "vocab_size": len(toks), "tokens": toks})
        cols[v] = encode_amount(df[v].values, df.weight.values, toks, groups)
    names = [v["name"] for v in variables]
    truth = np.stack([cols[n] for n in names], axis=1).astype(np.int16)
    tokens = truth.copy()
    scf_only = [i for i, v in enumerate(variables) if v["group"] == "scf_only"]
    h1m = (df.half == 1).values
    tokens[np.ix_(h1m, scf_only)] = -1
    holding = [(v, float((df.loc[df.half == 2, v] != 0).mean())) for v in WEALTH]
    order = ["scf_weight"] + [v for v, _ in sorted(holding, key=lambda t: -t[1])]
    meta = {"variables": variables, "sources": ["SCF", "SCF_H1"], "years": [2016],
            "seeds": {"split": 20261009}, "generation_order": order,
            "note": "FABRICATED development data (d_devdata.py), not real"}
    os.makedirs(OUT, exist_ok=True)
    with open(os.path.join(OUT, "meta.json"), "w") as f:
        json.dump(meta, f, indent=1)
    rec_cols = ["source", "year", "hh_id", "implicate", "unit", "weight", "half"] + INCOMES + WEALTH
    for split in ["train", "val"]:
        m = (df.split == split).values
        np.save(os.path.join(OUT, f"tokens_{split}.npy"), tokens[m])
        np.save(os.path.join(OUT, f"truth_{split}.npy"), truth[m])
        df.loc[m, rec_cols].reset_index(drop=True).to_parquet(os.path.join(OUT, f"records_{split}.parquet"))
    m = h1m
    np.save(os.path.join(OUT, "tokens_test.npy"), tokens[m])
    np.save(os.path.join(OUT, "truth_test.npy"), truth[m])
    df.loc[m, rec_cols].reset_index(drop=True).to_parquet(os.path.join(OUT, "records_test.parquet"))
    # Quantile functions per source-year-variable, positives and negatives
    qrows = []
    probs = np.unique(np.concatenate([np.linspace(0, 1, 201), GRID]))
    for g in np.unique(groups):
        src, yr = g.split("_")[0] if not g.startswith("SCF_H1") else "SCF_H1", int(g[-4:])
        mg = groups == g
        for v in INCOMES + WEALTH:
            for sign in ["positive", "negative"]:
                x = df.loc[mg, v].values
                w = df.loc[mg, "weight"].values
                s = x > 0 if sign == "positive" else x < 0
                if s.sum() < 2:
                    continue
                ax = np.abs(x[s])
                o = np.argsort(ax)
                cw = np.cumsum(w[s][o]) / w[s].sum()
                qv = np.interp(probs, cw, ax[o])
                for p, q in zip(probs, qv):
                    qrows.append({"source": src, "year": yr, "variable": v, "sign": sign,
                                  "p": float(p), "value": float(q if sign == "positive" else -q)})
    pd.DataFrame(qrows).to_parquet(os.path.join(OUT, "quantiles.parquet"))
    open(os.path.join(OUT, "DONE"), "w").close()
    print("wrote", OUT, "records:", len(df), "test:", int(m.sum()))


if __name__ == "__main__":
    main()
