"""Fabricated dataset for checking piece B's code (nothing here is real data).

Layout as in SPEC.md section A (meta.json, tokens/truth/records parquet,
quantiles.parquet), shaped like experiment B: a "PUF" source at year 2015
whose wealth and weight are unobserved, and an "SCF" source at 2013 and
2016 that observes everything.

The known model, per year, for a population of tax units:
  - a latent wealth propensity z ~ N(0, 1) and income propensity y ~ N(0, 1);
  - age range (0..5), married, dependents (0..3);
  - seven incomes; interest and dividends (`int_div`) depend strongly on z:
    held with probability logistic(-0.3 + 1.5 z), amount exp(1.5 z + 0.5 y);
    this is the planted link between a shared variable and the wealth that
    only the SCF observes;
  - six wealth amounts: equities (held with probability logistic(-0.8 + 2 z
    + 0.2 (year - 2013)), so holding rises over time), primary home,
    primary mortgage (only homeowners), cash, a retirement account, and an
    unrealized gain that can be negative.
The SCF over-samples high z: inclusion probability proportional to
1 + 9 logistic(4 (z - 1.2)) times a little noise; the weight is the
inverse. So among SCF records with the same incomes, wealth is higher than
in the population, and the weight-corrected completion should undo it.
The PUF is a simple random sample of the 2015 population (equal weights).

Ranks are weighted ranks within source and year, as in SPEC.md, so tokens
mean population rank bands in both sources. For the checks the PUF's true
wealth tokens are written to truth_*.npy (the real PUF has none).

Usage: python b_make_dev.py --out $S/dev/b_expB
"""

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd

GRID = np.round(np.concatenate([np.arange(0.02, 0.9001, 0.02), np.arange(0.91, 0.9901, 0.01),
                                np.arange(0.991, 0.99901, 0.001), [0.9999, 1.0]]), 6)
NEG_GRID = np.round(np.arange(0.1, 1.0001, 0.1), 6)
INCOMES = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens", "ui_other"]
WEALTH = ["equities", "primary_home", "primary_mortgage", "cash", "dc", "kg_other"]
CAN_NEG = {"business", "capital_gains", "rent", "kg_other"}
SEED = 20261009


def sig(x):
    return 1 / (1 + np.exp(-x))


def wrank(x, w):
    order = np.argsort(x, kind="mergesort")
    xs, ws = x[order], w[order] / w.sum()
    uniq, start = np.unique(xs, return_index=True)
    block_w = np.add.reduceat(ws, start)
    below = np.concatenate([[0.0], np.cumsum(block_w)[:-1]])
    mid = below + block_w / 2
    r = np.empty(len(x))
    r[order] = np.repeat(mid, np.diff(np.append(start, len(xs))))
    return r


def token_meta(has_neg):
    toks = [{"token": 0, "zero": True, "negative": False, "lo": None, "hi": None}]
    if has_neg:
        lo = 0.0
        for hi in NEG_GRID:
            toks.append({"token": len(toks), "zero": False, "negative": True,
                         "lo": float(lo), "hi": float(hi)})
            lo = hi
    lo = 0.0
    for hi in GRID:
        toks.append({"token": len(toks), "zero": False, "negative": False,
                     "lo": float(lo), "hi": float(hi)})
        lo = hi
    return toks


def encode(x, w, toks):
    out = np.zeros(len(x), dtype=np.int32)
    neg = [t for t in toks if t["negative"]]
    pos = [t for t in toks if not t["negative"] and not t["zero"]]
    for sel, grp in ((x > 0, pos), (x < 0, neg)):
        if sel.any():
            assert grp, "negative value without negative tokens"
            r = wrank(x[sel], w[sel])
            his = np.array([t["hi"] for t in grp])
            out[sel] = grp[0]["token"] + np.minimum(np.searchsorted(his, r), len(grp) - 1)
    return out


def population(n, year, rng):
    z = rng.standard_normal(n)
    y = rng.standard_normal(n)
    age = np.clip(np.round(2.5 + 1.2 * rng.standard_normal(n) + 0.3 * z), 0, 5).astype(int)
    married = (rng.random(n) < sig(-0.2 + 0.5 * y + 0.2 * (age - 2))).astype(int)
    n_dep = np.minimum(rng.poisson(np.where(age < 3, 1.0, 0.3)), 3)
    d = dict(age_range=age, married=married, n_dep=n_dep)
    e = lambda s=0.5: np.exp(s * rng.standard_normal(n))
    d["wages"] = np.where(rng.random(n) < np.where(age >= 4, 0.35, 0.9), 40000 * np.exp(0.8 * y) * e(), 0)
    bus = rng.random(n) < 0.15
    d["business"] = np.where(bus, 20000 * np.exp(0.5 * y + 0.5 * z) * e() * np.where(rng.random(n) < 0.2, -1, 1), 0)
    d["int_div"] = np.where(rng.random(n) < sig(-0.3 + 1.5 * z), 500 * np.exp(1.5 * z + 0.5 * y) * e(), 0)
    d["capital_gains"] = np.where(rng.random(n) < sig(-2 + z), 3000 * np.exp(z) * e(1) * np.where(rng.random(n) < 0.25, -1, 1), 0)
    d["rent"] = np.where(rng.random(n) < 0.08, 8000 * e() * np.where(rng.random(n) < 0.4, -1, 1), 0)
    d["ss_pens"] = np.where(age >= 4, 20000 * e(0.3), np.where(rng.random(n) < 0.05, 10000 * e(), 0))
    d["ui_other"] = np.where(rng.random(n) < 0.05, 3000 * e(), 0)
    eq_hold = rng.random(n) < sig(-0.8 + 2 * z + 0.2 * (year - 2013))
    d["equities"] = np.where(eq_hold, 30000 * np.exp(2 * z + 0.3 * y) * e(), 0)
    home = rng.random(n) < sig(-0.5 + 0.5 * age + 0.5 * y)
    d["primary_home"] = np.where(home, 200000 * np.exp(0.5 * z + 0.4 * y) * e(0.3), 0)
    mort = home & (rng.random(n) < sig(1.5 - 0.5 * age))
    d["primary_mortgage"] = np.where(mort, d["primary_home"] * rng.uniform(0.2, 0.9, n), 0)
    d["cash"] = 5000 * np.exp(z + 0.5 * y) * e()
    d["dc"] = np.where((d["wages"] > 0) & (rng.random(n) < sig(0.5 * y + 0.3 * z)), 50000 * np.exp(0.6 * y + 0.4 * z) * e(), 0)
    d["kg_other"] = np.where(eq_hold, d["equities"] * rng.normal(0.3, 0.4, n), 0)
    d["_z"] = z
    return pd.DataFrame(d)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--pop", type=int, default=400000)
    ap.add_argument("--scf-n", type=int, default=20000)
    ap.add_argument("--puf-n", type=int, default=60000)
    a = ap.parse_args()
    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    rng = np.random.default_rng(SEED)

    parts = []
    for year in (2013, 2016):
        pop = population(a.pop, year, rng)
        pi = (1 + 9 * sig(4 * (pop["_z"].to_numpy() - 1.2))) * np.exp(0.2 * rng.standard_normal(a.pop))
        pi = pi / pi.sum() * a.scf_n
        take = rng.random(a.pop) < pi
        s = pop[take].copy()
        s["weight"] = 1 / pi[take]
        s["source"], s["year"] = "SCF", year
        parts.append(s)
    pop15 = population(a.pop, 2015, rng)
    p = pop15.sample(a.puf_n, random_state=SEED).copy()
    p["weight"] = a.pop / a.puf_n
    p["source"], p["year"] = "PUF", 2015
    parts.append(p)
    df = pd.concat(parts, ignore_index=True)
    df["hh_id"] = np.arange(len(df)) + 1
    df["implicate"] = np.where(df["source"] == "SCF", 1, 0)
    df["unit"] = 1
    df["half"] = 0
    df["scf_weight"] = np.where(df["source"] == "SCF", df["weight"], np.nan)
    # Population truth for the checks, not in the layout: z, and the 2015
    # population's equity holding rate.
    print("population 2015 share holding equities:", float((pop15["equities"] > 0).mean()))

    variables = [
        {"name": "source", "group": "context", "kind": "categorical", "vocab_size": 2,
         "tokens": [{"token": 0, "value": "PUF"}, {"token": 1, "value": "SCF"}]},
        {"name": "age_range", "group": "shared", "kind": "categorical", "vocab_size": 6,
         "tokens": [{"token": i, "value": i} for i in range(6)]},
        {"name": "married", "group": "shared", "kind": "categorical", "vocab_size": 2,
         "tokens": [{"token": i, "value": i} for i in range(2)]},
        {"name": "n_dep", "group": "shared", "kind": "categorical", "vocab_size": 4,
         "tokens": [{"token": i, "value": i} for i in range(4)]},
    ]
    for n in INCOMES:
        t = token_meta(n in CAN_NEG)
        variables.append({"name": n, "group": "shared", "kind": "amount", "vocab_size": len(t), "tokens": t})
    scf = df["source"] == "SCF"
    lw = np.log(df.loc[scf, "weight"].to_numpy())
    edges = np.quantile(lw, np.linspace(0, 1, 33))
    edges[0], edges[-1] = -np.inf, np.inf
    wtok_scf = np.clip(np.searchsorted(edges, lw, side="right") - 1, 0, 31)
    wv = [float(np.exp(lw[wtok_scf == k]).mean()) for k in range(32)]
    variables.append({"name": "scf_weight", "group": "scf_only", "kind": "weight", "vocab_size": 32,
                      "tokens": [{"token": k, "lo": float(edges[k]), "hi": float(edges[k + 1]),
                                  "value": wv[k]} for k in range(32)]})
    for n in WEALTH:
        t = token_meta(n in CAN_NEG)
        variables.append({"name": n, "group": "scf_only", "kind": "amount", "vocab_size": len(t), "tokens": t})
    names = [v["name"] for v in variables]

    T = np.zeros((len(df), len(variables)), dtype=np.int32)
    for j, v in enumerate(variables):
        n = v["name"]
        if n == "source":
            T[:, j] = (df["source"] == "SCF").astype(int)
        elif v["kind"] == "categorical":
            T[:, j] = df[n].to_numpy()
        elif n == "scf_weight":
            T[scf.to_numpy(), j] = wtok_scf
        else:
            for (s, yr), g in df.groupby(["source", "year"]):
                idx = g.index.to_numpy()
                T[idx, j] = encode(g[n].to_numpy(float), g["weight"].to_numpy(float), v["tokens"])
    truth = T.copy()
    tok = T.copy()
    puf = (df["source"] == "PUF").to_numpy()
    for j, v in enumerate(variables):
        if v["group"] == "scf_only":
            tok[puf, j] = -1
    truth[puf, names.index("scf_weight")] = -1

    split = np.empty(len(df), dtype=object)
    u = rng.random(len(df))
    split[:] = np.where(u < 0.7, "train", np.where(u < 0.8, "val", "test"))
    keep_cols = ["source", "year", "hh_id", "implicate", "unit", "weight", "half"] + \
        INCOMES + WEALTH + ["age_range", "married", "n_dep"]
    for sp in ("train", "val", "test"):
        m = split == sp
        np.save(out / f"tokens_{sp}.npy", tok[m])
        np.save(out / f"truth_{sp}.npy", truth[m])
        r = df.loc[m, keep_cols].reset_index(drop=True)
        r.to_parquet(out / f"records_{sp}.parquet", index=False)
        df.loc[m, ["_z"]].reset_index(drop=True).to_parquet(out / f"latent_{sp}.parquet", index=False)
    pd.DataFrame({"note": ["fabricated; decode not used"]}).to_parquet(out / "quantiles.parquet")
    meta = dict(description="fabricated dataset for piece B checks (b_make_dev.py)",
                variables=variables, sources=["PUF", "SCF"], years=[2013, 2015, 2016],
                seeds={"data": SEED}, order=["scf_weight", "cash", "primary_home", "equities",
                                             "dc", "primary_mortgage", "kg_other"],
                population_2015_equity_share=float((pop15["equities"] > 0).mean()),
                counts={sp: int((split == sp).sum()) for sp in ("train", "val", "test")})
    with open(out / "meta.json", "w") as f:
        json.dump(meta, f, indent=1)
    print(df.groupby(["source", "year"]).size())
    print("SCF unweighted vs weighted equity holding:",
          float((df.loc[scf, "equities"] > 0).mean()),
          float(np.average(df.loc[scf, "equities"] > 0, weights=df.loc[scf, "weight"])))
    (out / "DONE").touch()


if __name__ == "__main__":
    main()
