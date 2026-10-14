"""Part C development data: a fabricated dataset in the SPEC.md layout, and
fabricated completed files, to check that c_evaluate.py reacts correctly.

Nothing here is real data. The fabricated records follow a known model in
which wealth depends on income and age, so we can also draw an "oracle"
completion: fresh wealth drawn from the true model given each record's
shared variables. That is what a perfect imputation model would produce,
and it tells us how large the gaps of a perfect imputation are compared with
the half-against-half sampling yardstick.

Completed files written (5 draws per record, B's layout):
  perfect   every draw equals the truth (all gaps must be zero, AUC 0.5)
  oracle    wealth redrawn from the true model given shared variables
            (seeds 1 and 2, to exercise the training-noise path)
  shuffled  each record's whole wealth vector taken from a random other
            record (keeps the joint pattern of wealth, loses the link to
            income and age)
  noincome  wealth redrawn from the true model with income replaced by a
            random other record's income (a partly broken link)

Usage: python c_make_dev.py --out $S/dev/c
"""
import argparse
import json
import os

import numpy as np
import pandas as pd

ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities',
          'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
          're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards',
         'installment_debt', 'other_debt']
KG = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
WEALTH = ASSETS + DEBTS + KG
INCOMES = ['wages', 'business', 'int_div', 'capital_gains', 'rent', 'ss_pens',
           'ui_other']
CAN_BE_NEG = {'business', 'capital_gains', 'rent', 'pass_throughs'} | set(KG)

GRID = np.concatenate([np.arange(0.02, 0.9001, 0.02), np.arange(0.91, 0.9901, 0.01),
                       np.arange(0.991, 0.99901, 0.001), [0.9999, 1.0]])
GRID = np.round(GRID, 6)
NEG_GRID = np.round(np.arange(0.1, 1.0001, 0.1), 6)


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


def token_meta(name, has_neg):
    """Token 0 = zero; then negative bins (rank among negatives, ascending
    in value), then positive bins on the shared grid."""
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
    p = x > 0
    if p.any():
        r = wrank(x[p], w[p])
        his = np.array([t["hi"] for t in pos])
        out[p] = pos[0]["token"] + np.minimum(np.searchsorted(his, r, side="left"), len(pos) - 1)
    n = x < 0
    if n.any():
        assert neg, "negative value for a variable without negative tokens"
        r = wrank(x[n], w[n])
        his = np.array([t["hi"] for t in neg])
        out[n] = neg[0]["token"] + np.minimum(np.searchsorted(his, r, side="left"), len(neg) - 1)
    return out


def draw_wealth(inc_total, age, married, rng):
    """The known model: holding and amount depend on log income and age,
    plus one latent wealth factor shared by all components."""
    n = len(inc_total)
    li = np.log1p(np.maximum(inc_total, 0)) - 10.5
    a = (age - 3.5) / 2.0
    lat = rng.normal(size=n)
    out = {}
    for j, v in enumerate(WEALTH):
        base = -1.5 + 0.15 * j % 2.5
        slope_i = 0.6 + 0.05 * (j % 5)
        slope_a = (0.4 if v in ('primary_home', 'dc', 'db', 'annuities') else
                   -0.4 if v in DEBTS else 0.1)
        logit = base + slope_i * li + slope_a * a + 0.8 * lat + 0.3 * married
        hold = rng.random(n) < 1 / (1 + np.exp(-logit))
        mu = 9.0 + 0.9 * li + 0.3 * a + 0.7 * lat + 0.2 * (j % 4)
        amt = np.exp(mu + rng.normal(scale=1.1, size=n))
        if v in CAN_BE_NEG:
            sign = np.where(rng.random(n) < 0.15, -0.3, 1.0)
            amt = amt * sign
        out[v] = np.where(hold, np.round(amt, -1), 0.0)
    if True:  # gains only where the asset is held
        out['kg_primary_home'] *= out['primary_home'] > 0
        out['kg_pass_throughs'] *= out['pass_throughs'] != 0
        out['primary_mortgage'] *= out['primary_home'] > 0
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--households", type=int, default=3000)
    ap.add_argument("--seed", type=int, default=7)
    args = ap.parse_args()
    rng = np.random.default_rng(args.seed)
    dd = os.path.join(args.out, "fake")
    cd = os.path.join(args.out, "fake_completed")
    os.makedirs(dd, exist_ok=True)
    os.makedirs(cd, exist_ok=True)

    H = args.households
    units = rng.choice([1, 2], size=H, p=[0.8, 0.2])
    hh = np.repeat(np.arange(1, H + 1), units)
    unit = np.concatenate([np.arange(1, u + 1) for u in units])
    hw = np.exp(rng.normal(8.0, 1.0, size=H))
    nunit = len(hh)
    age = rng.integers(1, 8, size=nunit)        # AGERANGE 1..7
    married = (rng.random(nunit) < 0.5).astype(int)
    ndep = rng.choice(4, size=nunit, p=[0.6, 0.15, 0.15, 0.1])
    lat_inc = rng.normal(size=nunit)
    inc = {}
    for j, v in enumerate(INCOMES):
        p = [0.8, 0.15, 0.6, 0.2, 0.1, 0.3, 0.1][j]
        held = rng.random(nunit) < p
        amt = np.exp(9.5 + 0.8 * lat_inc + rng.normal(scale=1.0, size=nunit))
        if v in CAN_BE_NEG:
            amt *= np.where(rng.random(nunit) < 0.1, -0.2, 1.0)
        inc[v] = np.where(held, np.round(amt, -1), 0.0)
    total = sum(inc[v] for v in INCOMES)

    # Five implicates: same unit, wealth drawn with a shared latent so that
    # implicates resemble each other.
    rows = []
    base_rng = np.random.default_rng(args.seed + 1)
    common = draw_wealth(total, age, married, base_rng)
    for imp in range(1, 6):
        r = np.random.default_rng(args.seed + 10 + imp)
        fresh = draw_wealth(total, age, married, r)
        keep = r.random(nunit) < 0.8      # 80% of values identical across implicates
        w = {v: np.where(keep, common[v], fresh[v]) for v in WEALTH}
        df = pd.DataFrame({"source": "SCF_H1", "year": 2016, "hh_id": hh,
                           "implicate": imp, "unit": unit,
                           "weight": hw[hh - 1] / units[hh - 1] / 5, "half": 1,
                           "_age": age, "_married": married, "_ndep": ndep})
        for v in INCOMES:
            df[v] = inc[v]
        for v in WEALTH:
            df[v] = w[v]
        rows.append(df)
    rec = pd.concat(rows, ignore_index=True)
    rec = rec.sort_values(["hh_id", "unit", "implicate"]).reset_index(drop=True)
    w = rec["weight"].to_numpy()

    variables = []
    variables.append({"name": "age_range", "group": "shared", "kind": "categorical",
                      "vocab_size": 7, "tokens": [{"token": i, "value": i + 1} for i in range(7)]})
    variables.append({"name": "married", "group": "shared", "kind": "categorical",
                      "vocab_size": 2, "tokens": [{"token": i, "value": i} for i in range(2)]})
    variables.append({"name": "n_dep", "group": "shared", "kind": "categorical",
                      "vocab_size": 4, "tokens": [{"token": i, "value": i} for i in range(4)]})
    for v in INCOMES + WEALTH:
        toks = token_meta(v, v in CAN_BE_NEG)
        variables.append({"name": v, "group": "shared" if v in INCOMES else "scf_only",
                          "kind": "amount", "vocab_size": len(toks), "tokens": toks})
    variables.append({"name": "scf_weight", "group": "scf_only", "kind": "weight",
                      "vocab_size": 64, "tokens": [{"token": i} for i in range(64)]})
    meta = {"dataset": "fake", "fabricated": True, "variables": variables,
            "sources": ["SCF_H1", "SCF"], "years": [2016], "seeds": {"fake": args.seed},
            "generation_order": ["scf_weight"] + WEALTH}
    json.dump(meta, open(os.path.join(dd, "meta.json"), "w"), indent=1)

    T = np.zeros((len(rec), len(variables)), dtype=np.int32)
    for j, vm in enumerate(variables):
        n = vm["name"]
        if n == "age_range":
            T[:, j] = rec["_age"] - 1
        elif n == "married":
            T[:, j] = rec["_married"]
        elif n == "n_dep":
            T[:, j] = rec["_ndep"]
        elif n == "scf_weight":
            T[:, j] = np.minimum((wrank(np.log(w), np.ones(len(w))) * 64).astype(int), 63)
        else:
            T[:, j] = encode(rec[n].to_numpy(), w, vm["tokens"])
    rec = rec.drop(columns=["_age", "_married", "_ndep"])
    np.save(os.path.join(dd, "truth_test.npy"), T)
    Tobs = T.copy()
    for j, vm in enumerate(variables):
        if vm["group"] == "scf_only":
            Tobs[:, j] = -1
    np.save(os.path.join(dd, "tokens_test.npy"), Tobs)
    rec.to_parquet(os.path.join(dd, "records_test.parquet"))
    # quantiles: not used by c_evaluate.py; write a minimal file for layout.
    pd.DataFrame({"source": [], "year": [], "variable": [], "sign": [], "p": [],
                  "value": []}).to_parquet(os.path.join(dd, "quantiles.parquet"))
    open(os.path.join(dd, "DONE"), "w").write("fabricated\n")

    # ---------------- completed files ----------------
    names = [vm["name"] for vm in variables]
    widx = [names.index(v) for v in WEALTH]

    def write(tag, seed, wealth_dollars_by_draw, wealth_tokens_by_draw):
        parts = []
        for d in range(5):
            df = rec.copy()
            df["row"] = np.arange(len(rec))
            df["draw"] = d
            for k, v in enumerate(WEALTH):
                df[v] = wealth_dollars_by_draw[d][:, k]
                df["tok_" + v] = wealth_tokens_by_draw[d][:, k]
            parts.append(df)
        out = pd.concat(parts, ignore_index=True)
        out.to_parquet(os.path.join(cd, f"completed_{tag}_seed{seed:02d}.parquet"))

    Wd = rec[WEALTH].to_numpy()
    Wt = T[:, widx]
    write("perfect", 1, [Wd] * 5, [Wt] * 5)

    # unit-level shared vars, one per record
    tot_rec = rec[INCOMES].sum(axis=1).to_numpy()
    age_rec = T[:, names.index("age_range")] + 1
    mar_rec = T[:, names.index("married")]

    def tokens_of(d):
        # encode with the TRUE records' quantile function, as decoding would
        out = np.zeros(d.shape, dtype=np.int32)
        for k, v in enumerate(WEALTH):
            vm = variables[names.index(v)]
            x = d[:, k]
            ref = rec[v].to_numpy()
            # rank of each completed value within the true distribution
            out[:, k] = encode_against(x, ref, w, vm["tokens"])
        return out

    for seed in (1, 2):
        Dd, Dt = [], []
        for d in range(5):
            r = np.random.default_rng(1000 * seed + d)
            o = draw_wealth(tot_rec, age_rec, mar_rec, r)
            m = np.column_stack([o[v] for v in WEALTH])
            Dd.append(m)
            Dt.append(tokens_of(m))
        write("oracle", seed, Dd, Dt)

    Dd, Dt = [], []
    for d in range(5):
        r = np.random.default_rng(2000 + d)
        donor = r.choice(len(rec), size=len(rec), p=w / w.sum())
        Dd.append(Wd[donor])
        Dt.append(Wt[donor])
    write("shuffled", 1, Dd, Dt)

    Dd, Dt = [], []
    for d in range(5):
        r = np.random.default_rng(3000 + d)
        other = r.permutation(len(rec))
        o = draw_wealth(tot_rec[other], age_rec, mar_rec, r)
        m = np.column_stack([o[v] for v in WEALTH])
        Dd.append(m)
        Dt.append(tokens_of(m))
    write("noincome", 1, Dd, Dt)
    print("wrote", dd, "and", cd, "records:", len(rec))


def encode_against(x, ref, w, toks):
    """Token of each value x by its weighted rank within the reference
    values ref (positives among positives, negatives among negatives)."""
    out = np.zeros(len(x), dtype=np.int32)
    neg = [t for t in toks if t["negative"]]
    pos = [t for t in toks if not t["negative"] and not t["zero"]]
    for mask, rmask, group in ((x > 0, ref > 0, pos), (x < 0, ref < 0, neg)):
        if not mask.any():
            continue
        rv, rw = ref[rmask], w[rmask]
        if len(rv) == 0:
            rv, rw = np.abs(x[mask]), np.ones(mask.sum())
        o = np.argsort(rv)
        cw = np.cumsum(rw[o]) / rw.sum()
        r = np.interp(x[mask], rv[o], cw)
        his = np.array([t["hi"] for t in group])
        out[mask] = group[0]["token"] + np.minimum(np.searchsorted(his, r, side="left"),
                                                   len(group) - 1)
    return out


if __name__ == "__main__":
    main()
