"""Fabricated dataset in the `full` layout, for checking piece H's code.

Nothing here is real data. Shape as SPEC.md's 20:45 entry describes `full`:
a context variable `source`; 39 PUF-side variables (14 "shared", observed
on both sources, and 25 "puf_only", unobserved on SCF records); 24 wealth
variables ("scf_only", unobserved on PUF records and on 10% of SCF records,
standing in for split-off units); and one weight-kind variable
(`scf_weight`) that training drops. PUF at 2015 with unequal weights; SCF at
2013 and 2016. Age range is unobserved on 40% of PUF records. Tokens follow a
latent factor so the model has something to learn. No quantiles file: run
h_complete.py with --no-decode on it.

Usage: python h_make_fake.py --out $P/dev/h_full_fake
"""

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd


def amount_tokens(n_pos, has_neg):
    toks = []
    if has_neg:
        for i in range(8):
            toks.append(dict(token=len(toks), kind="neg", lo=i / 8, hi=(i + 1) / 8,
                             zero=False, negative=True))
    toks.append(dict(token=len(toks), kind="zero", lo=0.0, hi=0.0, zero=True, negative=False))
    for i in range(n_pos):
        toks.append(dict(token=len(toks), kind="pos", lo=i / n_pos, hi=(i + 1) / n_pos,
                         zero=False, negative=False))
    return toks


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--puf-n", type=int, default=30000)
    ap.add_argument("--scf-n", type=int, default=6000, help="per wave")
    a = ap.parse_args()
    rng = np.random.default_rng(20261009)
    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)

    variables = [dict(name="source", group="context", kind="categorical", vocab_size=2,
                      tokens=[dict(token=0, value="PUF"), dict(token=1, value="SCF")])]
    cats = [("age_range", 6), ("married", 2), ("n_dep", 4), ("MARS", 4), ("XTOT", 6)]
    for nm, k in cats:
        variables.append(dict(name=nm, group="shared", kind="categorical", vocab_size=k,
                              tokens=[dict(token=i, value=float(i)) for i in range(k)]))
    for i in range(9):
        t = amount_tokens(66, i % 3 == 0)
        variables.append(dict(name=f"S{i:02d}", group="shared", kind="amount",
                              vocab_size=len(t), tokens=t))
    for i in range(25):
        t = amount_tokens(66, i % 4 == 0)
        variables.append(dict(name=f"P{i:02d}", group="puf_only", kind="amount",
                              vocab_size=len(t), tokens=t))
    variables.append(dict(name="scf_weight", group="scf_only", kind="weight", vocab_size=64,
                          tokens=[dict(token=i, kind="weight", lo=i / 64, hi=(i + 1) / 64)
                                  for i in range(64)]))
    wealth = []
    for i in range(24):
        t = amount_tokens(66, i % 5 == 0)
        nm = f"W{i:02d}"
        wealth.append(nm)
        variables.append(dict(name=nm, group="scf_only", kind="amount",
                              vocab_size=len(t), tokens=t))
    names = [v["name"] for v in variables]

    def make(n, source, year):
        z = rng.normal(size=n)
        u = rng.normal(size=n)
        tok = np.full((n, len(variables)), -1, dtype=np.int64)
        tok[:, 0] = 0 if source == "PUF" else 1
        for j, v in enumerate(variables[1:], start=1):
            k = v["vocab_size"]
            if v["kind"] == "categorical":
                tok[:, j] = np.clip(np.floor((0.5 + 0.2 * (z + u)) * k), 0, k - 1)
            elif v["kind"] == "weight":
                tok[:, j] = rng.integers(0, 64, n)
            else:
                kinds = np.array([t["kind"] for t in v["tokens"]])
                zero = int(np.flatnonzero(kinds == "zero")[0])
                npos = int((kinds == "pos").sum())
                held = rng.random(n) < 1 / (1 + np.exp(-(z + 0.3 * j % 3)))
                r = 1 / (1 + np.exp(-(z + 0.5 * u + rng.normal(scale=0.5, size=n))))
                t = zero + 1 + np.clip((r * npos).astype(int), 0, npos - 1)
                t = np.where(held, t, zero)
                if zero > 0:
                    neg = rng.random(n) < 0.05
                    t = np.where(neg, rng.integers(0, zero, n), t)
                tok[:, j] = t
        grp = np.array([v["group"] for v in variables])
        if source == "PUF":
            tok[:, grp == "scf_only"] = -1
            miss_age = rng.random(n) < 0.4
            tok[miss_age, names.index("age_range")] = -1
            w = np.exp(rng.normal(5.5, 1.0, n))
        else:
            tok[:, grp == "puf_only"] = -1
            split_off = rng.random(n) < 0.1
            tok[np.ix_(split_off, np.flatnonzero(grp == "scf_only"))] = -1
            w = np.exp(rng.normal(3.0, 0.8, n))
        rec = pd.DataFrame(dict(source=source, year=year, weight=w))
        return tok, rec

    parts = {}
    for src, yr, n in (("PUF", 2015, a.puf_n), ("SCF", 2013, a.scf_n), ("SCF", 2016, a.scf_n)):
        tok, rec = make(n, src, yr)
        split = rng.choice(["train", "val", "test"], size=n, p=[0.7, 0.15, 0.15])
        for s in ("train", "val", "test"):
            m = split == s
            parts.setdefault(s, []).append((tok[m], rec[m]))
    for s, lst in parts.items():
        tok = np.concatenate([x[0] for x in lst]).astype(np.int16)
        rec = pd.concat([x[1] for x in lst], ignore_index=True)
        np.save(out / f"tokens_{s}.npy", tok)
        np.save(out / f"truth_{s}.npy", tok)
        rec.to_parquet(out / f"records_{s}.parquet", index=False)
    meta = dict(dataset="h_full_fake", sources=["PUF", "SCF"], variables=variables,
                variable_names=names, generation_order=["scf_weight"] + wealth,
                puf_generation_order=["MARS", "XTOT", "age_range", "married", "n_dep"]
                + [f"S{i:02d}" for i in range(9)] + [f"P{i:02d}" for i in range(25)])
    with open(out / "meta.json", "w") as f:
        json.dump(meta, f, indent=1)
    (out / "DONE").write_text("fabricated\n")
    print(f"wrote {out}: {len(variables)} variables "
          f"({sum(v['group'] != 'context' for v in variables)} slots)")


if __name__ == "__main__":
    main()
