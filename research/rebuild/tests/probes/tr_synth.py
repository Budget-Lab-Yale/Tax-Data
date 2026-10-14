"""Probe TR (test 4, round 4): sequential trees on the multi-survey job.

Data: probe C's dataset full_v3cz (PUF 2015 plus the SCF 2013 and 2016 waves
translated into PUF terms; totals replaced by parts plus remainders; bridge
variables mortgage interest and real estate tax on SCF rows; the six itemized
deductions zero for PUF non-itemizers).

Tax variables (generation only): probe BL's method (bl_synth.py) on the PUF
training rows: unweighted, the PUF weight drawn first and used as a predictor
of every later variable, minimum leaf 20, values copied in dollars from a
training record in the leaf (uniformly), in probe AS's size-first order with
the three totals replaced by the pieces that complete them. Totals are
computed afterwards (c3_totals.py).

Wealth variables: for each of the 24 wealth variables in the dataset's order
(most to least commonly held), a regression tree fitted on the SCF training
rows with wealth observed, survey-weighted (the SCF implicate weight is the
sample weight and donors in a leaf are drawn in proportion to it). The tree's
target is the variable's rank token (weighted rank within the SCF wave) and
its predictors are the shared tax variables' tokens (the PUF-side variables
the SCF measures, translated by probe X and ranked as the transformer saw
them) plus the wealth tokens already drawn. A target record's drawn token is
decoded to dollars with the dataset's quantile functions exactly as the
transformer's draws are (g_tokenizer.decode: the record's own SCF wave for
SCF records, 2013 and 2016 interpolated to 2015 for PUF records). So shared
variables meet in rank space, as in the transformer.

Missing age and sex on PUF records (about 42% are not in the demographic
file): before wealth is drawn, each is drawn from an SCF tree on the other
shared variables (survey-weighted, leaf draw). They are used only to condition
wealth; the output keeps them missing, as probe C's files do.

Itemizers. Two sets of wealth trees: with the bridge variables among the
predictors, and without them. Arm `split` (main): PUF itemizers use the first
set, PUF non-itemizers (whose zero mortgage interest and real estate tax mean
"not itemized", not "none paid") the second. Arm `zero`: every record uses the
first set, reading the zeros as the data codes them (what the transformer
trained on full_v3cz saw).

Modes (one job each):
  generate     whole joint records, --n per sampling seed, arm split
  puf_wealth   PUF training records, wealth completed, five draws
  scf_test     SCF test records, wealth completed given shared variables and
               the bridge variables (five draws)
  scf_test_nobr  as scf_test with the bridge variables hidden: wealth from the
               no-bridge trees, then mortgage interest and real estate tax
               drawn from SCF trees given everything (probe C's nobr order)

Outputs in probe C's formats (h_complete.py's docstring), plus per file a
`_donors.npz` (record-level donor row indices, scratch only) and `_info.json`.

    python tr_synth.py --mode puf_wealth --arm split --wealth-leaf 50
"""
import argparse
import json
import os
import sys
import time

import numpy as np
import pandas as pd
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "posttrain"))
sys.path.insert(0, HERE)
import g_tokenizer as tk  # noqa: E402

DATA = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/full_v3cz"
AS_SIZEFIRST_INFO = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/gen/h_mix25_sizefirst_s1_seed01_info.json"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr"
N_GEN = 664_624
BRIDGE = ["E19200", "E18500"]
DEMO = ["AGERANGE", "GENDER"]
TOTAL_TO_PIECE = {"E00600": "DIV_NONQUAL", "E02000": "SCHE_REMAINDER", "E01500": "PENS_NONTAX"}


def leaf_sampler(train_leaf, w):
    """Draw one training row per target record within the leaf it lands in,
    with probability proportional to w (bl_synth.py's sampler)."""
    order = np.argsort(train_leaf, kind="stable")
    sl = train_leaf[order]
    cw = np.cumsum(w[order])

    def draw(gen_leaf, rng):
        lo = np.searchsorted(sl, gen_leaf, side="left")
        hi = np.searchsorted(sl, gen_leaf, side="right")
        assert (hi > lo).all(), "a target record reached a leaf with no training record"
        base = np.where(lo > 0, cw[np.maximum(lo - 1, 0)], 0.0)
        tot = cw[hi - 1] - base
        u = base + rng.random(len(gen_leaf)) * tot
        idx = np.clip(np.searchsorted(cw, u, side="right"), lo, hi - 1)
        return order[idx]
    return draw


class FallbackCache(dict):
    """The tokenizer's quantile-function table, with one fallback: a tree can
    hand a record a donor's token from the other SCF wave, and that token can
    be a negative band of a variable that has no negative values in the
    record's own wave (or in one of the two waves 2015 interpolates). Such a
    band is decoded with the nearest wave that has it. Membership tests are
    unchanged, so every other decoding is exactly the tokenizer's."""
    fallbacks = 0

    def __missing__(self, key):
        s, y, n, side = key
        alts = [k for k in self.keys() if k[0] == s and k[2] == n and k[3] == side]
        if not alts:
            raise KeyError(key)
        FallbackCache.fallbacks += 1
        return dict.__getitem__(self, min(alts, key=lambda k: abs(k[1] - y)))


class Data:
    def __init__(self):
        self.meta = json.load(open(os.path.join(DATA, "meta.json")))
        self.vars = self.meta["variables"]
        self.names = [v["name"] for v in self.vars]
        self.col = {v["name"]: k for k, v in enumerate(self.vars)}
        assert all(v["column"] == k for k, v in enumerate(self.vars) if "column" in v)
        self.q = pd.read_parquet(os.path.join(DATA, "quantiles.parquet"))
        self.cache = FallbackCache(tk._qf_cache(self.q))
        self.wealth = [n for n in self.meta["generation_order"]
                       if self.vars[self.col[n]]["group"] == "scf_only"]
        assert len(self.wealth) == 24
        self.shared = [v["name"] for v in self.vars if v["group"] == "shared"]
        self.puf = [v["name"] for v in self.vars if v["group"] in ("shared", "puf_only")]
        self.cats = {v["name"] for v in self.vars if v["kind"] == "categorical"}

    def split(self, part):
        rec = pd.read_parquet(os.path.join(DATA, f"records_{part}.parquet"))
        tok = np.load(os.path.join(DATA, f"tokens_{part}.npy")).astype(np.int64)
        assert tok.shape == (len(rec), len(self.vars)), (tok.shape, len(rec))
        return rec, tok


class WealthModel:
    """Survey-weighted trees on SCF training rows with wealth observed."""

    def __init__(self, D, rec, tok, leaf, log, weight_variable=False, unweighted_fit=False):
        """weight_variable: the variant of probe UW applied to the SCF. Wealth
        trees are fitted unweighted (the sample as drawn) with the log SCF
        weight as a predictor; before wealth, each target record's SCF weight
        is drawn from a tree on its shared variables with donors in proportion
        to weight (the weight's population distribution given the shared
        variables). The bridge-variable trees for scf_test_nobr follow the
        same rule; age and sex stay weighted.
        unweighted_fit: wealth trees fitted on the sample as drawn, without the
        weight as a predictor.
        In every arm a donor in a leaf is drawn in proportion to its SCF
        weight, so a draw follows the population distribution within the leaf
        (a first version of weight_variable drew uniformly; that over-weights
        the oversampled wealthy wherever a leaf mixes weights)."""
        unw = weight_variable or unweighted_fit
        self.D, self.leaf, self.wv = D, leaf, weight_variable
        self.V = len(D.vars)
        self.col = dict(D.col, __w=self.V)
        src = rec["source"].astype(str).to_numpy()
        wc = [D.col[n] for n in D.wealth]
        m = (src == "SCF") & (tok[:, wc] >= 0).all(axis=1)
        self.w = rec["weight"].to_numpy(float)[m]
        self.tok = np.column_stack([tok[m], np.round(np.log(self.w) * 100).astype(np.int64)])
        assert "hh_id" in rec.columns
        self.hh = (rec["year"].astype(int).astype(str) + "_" + rec["hh_id"].astype(str)).to_numpy()[m]
        assert np.isfinite(self.w).all() and (self.w > 0).all()
        log(f"SCF training rows with wealth observed: {m.sum()} ({len(np.unique(self.hh))} households); "
            f"weight total {self.w.sum():,.0f}")
        for n in D.shared:
            k = (self.tok[:, D.col[n]] < 0).sum()
            if k:
                log(f"  shared variable {n} unobserved on {k} SCF training rows (kept as its own lowest value)")
        self.fit_info = []
        self.demo = {}
        base = [n for n in D.shared if n not in DEMO and n not in BRIDGE]
        self.demo["AGERANGE"] = self._fit("AGERANGE", base, cls=True)
        self.demo["GENDER"] = self._fit("GENDER", base + ["AGERANGE"], cls=True)
        self.sets, self.wtrees = {}, {}
        wx = ["__w"] if weight_variable else []
        for setname, pred in (("bridge", [n for n in D.shared]),
                              ("nobridge", [n for n in D.shared if n not in BRIDGE])):
            if weight_variable:
                self.wtrees[setname] = self._fit("__w", pred, cls=False)
            trees = []
            for k, n in enumerate(D.wealth):
                trees.append(self._fit(n, pred + wx + D.wealth[:k], cls=False, unweighted=unw))
            self.sets[setname] = trees
        # bridge variables given everything else (scf_test_nobr), C's order: wealth then E19200, E18500
        nb = [n for n in D.shared if n not in BRIDGE] + wx + D.wealth
        self.bridge_trees = [self._fit("E19200", nb, cls=False, unweighted=unw),
                             self._fit("E18500", nb + ["E19200"], cls=False, unweighted=unw)]

    def _fit(self, target, pred, cls, unweighted=False):
        X = self.tok[:, [self.col[n] for n in pred]]
        y = self.tok[:, self.col[target]]
        sw = np.ones(len(y)) if unweighted else self.w
        ok = (y >= 0) | (target == "__w")
        assert ok.all(), f"{target}: {int((~ok).sum())} unobserved on SCF training rows"
        Mdl = DecisionTreeClassifier if cls else DecisionTreeRegressor
        t0 = time.time()
        m = Mdl(min_samples_leaf=self.leaf, random_state=20261014)
        m.fit(X, y, sample_weight=sw)
        leaf = m.apply(X)
        cnt = np.bincount(leaf)
        cnt = cnt[cnt > 0]
        hh_per_leaf = pd.Series(self.hh).groupby(leaf).nunique().to_numpy()
        self.fit_info.append(dict(target=target, predictors=len(pred), unweighted=unweighted, leaves=int(m.get_n_leaves()),
                                  median_leaf_rows=float(np.median(cnt)),
                                  min_leaf_households=int(hh_per_leaf.min()),
                                  median_leaf_households=float(np.median(hh_per_leaf)),
                                  seconds=time.time() - t0))
        return dict(model=m, pred=pred, target=target, sampler=leaf_sampler(leaf, self.w))

    def _draw(self, tr, T, rng):
        X = T[:, [self.col[n] for n in tr["pred"]]]
        idx = tr["sampler"](tr["model"].apply(X), rng)
        return self.tok[idx, self.col[tr["target"]]], idx

    def fill(self, T, use_bridge, rng, fill_bridge=False):
        """T: token matrix [n, V] of the target records (shared variables
        observed; wealth -1). Returns the token matrix with wealth (and, if
        fill_bridge, the bridge variables) drawn, the demographic tokens used,
        and the donor row index (into the SCF training rows) per wealth variable."""
        D = self.D
        assert T.shape[1] == self.V
        n = len(T)
        T = np.column_stack([T, np.full(n, -1, dtype=np.int64)])
        demo_used = {}
        for d in DEMO:
            c = D.col[d]
            miss = T[:, c] < 0
            if miss.any():
                v, _ = self._draw(self.demo[d], T[miss], rng)
                T[miss, c] = v
            demo_used[d] = miss
        if self.wv:
            for setname, sel in (("bridge", use_bridge), ("nobridge", ~use_bridge)):
                if sel.any():
                    T[sel, self.V], _ = self._draw(self.wtrees[setname], T[sel], rng)
        donors = np.full((n, len(D.wealth)), -1, dtype=np.int64)
        for k, name in enumerate(D.wealth):
            c = D.col[name]
            vals = np.empty(n, dtype=np.int64)
            for setname, sel in (("bridge", use_bridge), ("nobridge", ~use_bridge)):
                if sel.any():
                    v, idx = self._draw(self.sets[setname][k], T[sel], rng)
                    vals[sel], donors[sel, k] = v, idx
            T[:, c] = vals
        if fill_bridge:
            for tr in self.bridge_trees:
                v, _ = self._draw(tr, T, rng)
                T[:, D.col[tr["target"]]] = v
        return T[:, :self.V], demo_used, donors


def decode_cols(D, T, records, cols, rng, year):
    full = np.full_like(T, -1)
    for n in cols:
        full[:, D.col[n]] = T[:, D.col[n]]
    dec = tk.decode(full, D.meta, records, D.q, rng, year=year, cache=D.cache)
    return {n: dec[n].to_numpy() for n in cols}


def copy_counts(donor_ids, nonzero):
    """Per record: the largest number of non-zero values taken from one donor."""
    d = np.where(nonzero, donor_ids, -1 - np.arange(donor_ids.shape[1])[None, :] - 10**9)
    d = np.sort(d, axis=1)
    run = np.ones(len(d), dtype=np.int64)
    best = np.where(nonzero.any(axis=1), 1, 0)
    for j in range(1, d.shape[1]):
        same = (d[:, j] == d[:, j - 1]) & (d[:, j] >= 0)
        run = np.where(same, run + 1, 1)
        best = np.maximum(best, np.where(d[:, j] >= 0, run, 0))
    return best


def completion(D, WM, mode, arm, draws, seed, tag, log):
    part = "train" if mode == "puf_wealth" else "test"
    rec, tok = D.split(part)
    src = rec["source"].astype(str).to_numpy()
    rows = np.flatnonzero(src == ("PUF" if mode == "puf_wealth" else "SCF"))
    rec_t = rec.iloc[rows].reset_index(drop=True)
    tok_obs = tok[rows].copy()
    fill_names = list(D.wealth) + (BRIDGE if mode == "scf_test_nobr" else [])
    tok_in = tok_obs.copy()
    for n in fill_names:
        tok_in[:, D.col[n]] = -1
    if mode == "puf_wealth":
        fded = rec_t["FDED"].to_numpy(float)
        use_bridge = np.ones(len(rows), bool) if arm == "zero" else (fded == 1)
        records = pd.DataFrame({"source": "PUF", "year": np.full(len(rows), 2015)})
        dyear = 2015
    elif mode == "scf_test":
        use_bridge = np.ones(len(rows), bool)
        records, dyear = rec_t[["source", "year"]], None
    else:
        use_bridge = np.zeros(len(rows), bool)
        records, dyear = rec_t[["source", "year"]], None
    log(f"{mode}: {len(rows)} records; bridge trees used for {use_bridge.mean():.3f} of them")
    rng = np.random.default_rng(seed)
    slot = [n for n in D.names if D.vars[D.col[n]]["group"] != "context"]
    rec_cols = [c for c in rec_t.columns if c not in set(D.names)]
    frames, donors_all, demo_all = [], [], []
    for d in range(draws):
        T, demo_used, donors = WM.fill(tok_in, use_bridge, rng, fill_bridge=(mode == "scf_test_nobr"))
        dec = decode_cols(D, T, records, fill_names, rng, dyear)
        cols = {"row": rows.astype(np.int64)}
        for c in rec_cols:
            cols[c] = rec_t[c].to_numpy()
        cols["draw"] = np.full(len(rows), d, dtype=np.int64)
        for n in slot:
            c = D.col[n]
            obs = tok_in[:, c] >= 0
            if n in fill_names:
                val = dec[n]
                tk_ = T[:, c]
            else:
                val = np.where(obs, rec_t[n].to_numpy(dtype=float), np.nan)
                tk_ = tok_in[:, c]
            cols[n] = val
            cols[f"tok_{n}"] = tk_.astype(np.int32)
            cols[f"obs_{n}"] = obs
        frames.append(pd.DataFrame(cols))
        donors_all.append(donors)
        demo_all.append(np.column_stack([demo_used[x] for x in DEMO]))
        log(f"draw {d + 1}/{draws} done")
    df = pd.concat(frames, ignore_index=True)
    os.makedirs(os.path.join(Q, "comp"), exist_ok=True)
    base = os.path.join(Q, "comp", f"tr_{tag}")
    df.to_parquet(base + ".parquet", index=False)
    don = np.concatenate(donors_all)
    np.savez_compressed(base + "_donors.npz", wealth_donor_rows=don, demo_drawn=np.concatenate(demo_all))
    # copying: non-zero wealth amounts from one SCF household
    W = np.column_stack([df[n].to_numpy() for n in D.wealth])
    hh_codes = pd.factorize(WM.hh)[0]
    best = copy_counts(hh_codes[don], np.nan_to_num(W) != 0)
    w = df["weight"].to_numpy(float)
    copy = {f"share_{k}plus_nonzero_wealth_from_one_scf_household": float(np.average(best >= k, weights=w))
            for k in (3, 5, 8)}
    log(f"copying: {copy}")
    return base, df, copy


def tax_order(D):
    size = json.load(open(AS_SIZEFIRST_INFO))["fill"]
    order = [TOTAL_TO_PIECE.get(n, n) for n in size]
    missing = [n for n in D.puf if n not in order]
    extra = [n for n in order if n not in D.puf]
    assert not extra, extra
    return order + missing


def generate(D, WM, seeds, n_gen, tag, log):
    rec, tok = D.split("train")
    m = rec["source"].astype(str).to_numpy() == "PUF"
    tr, ttok = rec[m].reset_index(drop=True), tok[m]
    w = tr["weight"].to_numpy(float)
    order = tax_order(D)
    cols = ["weight"] + order
    na = {c: int(tr[c].isna().sum()) for c in order if tr[c].isna().any()}
    bad = {c: k for c, k in na.items() if c not in ("AGERANGE", "GENDER", "EARNSPLIT")}
    if bad:
        raise ValueError(f"unexpected missing values in PUF training records: {bad}")
    log(f"{len(tr)} PUF training records; missing (kept as a value): {na}; order {cols}")
    X = tr[cols].to_numpy(float)
    TT = ttok[:, [D.col[n] for n in order]]
    ones = np.ones(len(tr))
    trees, samplers, fit_info = [None], [None], []
    t0 = time.time()
    for j in range(1, len(cols)):
        y = X[:, j]
        Mdl = DecisionTreeClassifier if cols[j] in D.cats else DecisionTreeRegressor
        mdl = Mdl(min_samples_leaf=20, random_state=20261011 + j)
        tj = time.time()
        mdl.fit(X[:, :j], np.where(np.isnan(y), -1.0, y))
        leaf = mdl.apply(X[:, :j])
        trees.append(mdl)
        samplers.append(leaf_sampler(leaf, ones))
        fit_info.append(dict(variable=cols[j], leaves=int(mdl.get_n_leaves()), seconds=time.time() - tj))
    log(f"tax trees fitted in {time.time() - t0:.0f}s")
    names_out = order
    out = {}
    for seed in seeds:
        rng = np.random.default_rng(20261014 * 100 + seed)
        ts = time.time()
        n = n_gen
        G = np.empty((n, len(cols)))
        DON = np.empty((n, len(cols)), dtype=np.int64)
        DON[:, 0] = rng.integers(0, len(tr), size=n)
        G[:, 0] = X[DON[:, 0], 0]
        for j in range(1, len(cols)):
            idx = samplers[j](trees[j].apply(G[:, :j]), rng)
            DON[:, j] = idx
            G[:, j] = X[idx, j]
        # tokens: each generated value carries its donor's token
        T = np.full((n, len(D.vars)), -1, dtype=np.int64)
        T[:, 0] = 0                                   # source PUF
        for j, nm in enumerate(order):
            T[:, D.col[nm]] = TT[DON[:, j + 1], j]
        fded = G[:, 1 + order.index("FDED")]
        T2, demo_used, wdon = WM.fill(T, fded == 1, rng)
        records = pd.DataFrame({"source": "PUF", "year": np.full(n, 2015)})
        dec = decode_cols(D, T2, records, D.wealth, rng, 2015)
        df = pd.DataFrame({"gen_id": np.arange(n), "source": "PUF", "year": 2015})
        wg = G[:, 0] * (w.sum() / G[:, 0].sum())
        df["weight"] = wg
        df["draw"] = np.zeros(n, dtype=np.int64)
        for nm in D.names:
            if nm in names_out:                       # tax variables: the donor's value and token
                df[nm] = G[:, 1 + order.index(nm)]    # (missing age and sex stay missing)
                df[f"tok_{nm}"] = T[:, D.col[nm]].astype(np.int32)
            elif nm in D.wealth:
                df[nm] = dec[nm]
                df[f"tok_{nm}"] = T2[:, D.col[nm]].astype(np.int32)
        os.makedirs(os.path.join(Q, "gen"), exist_ok=True)
        base = os.path.join(Q, "gen", f"tr_{tag}_s{seed}")
        df.to_parquet(base + ".parquet", index=False)
        np.savez_compressed(base + "_donors.npz", tax_donor_rows=DON[:, 1:], weight_donor_row=DON[:, 0],
                            tax_order=np.array(order), wealth_donor_rows=wdon,
                            demo_drawn=np.column_stack([demo_used[x] for x in DEMO]))
        # copying, both sides
        amts = [nm for nm in order if nm not in D.cats]
        A = np.column_stack([df[a].to_numpy(float) for a in amts])
        Ad = np.column_stack([DON[:, 1 + order.index(a)] for a in amts])
        best_tax = copy_counts(Ad, np.nan_to_num(A) != 0)
        W = np.column_stack([df[x].to_numpy() for x in D.wealth])
        hh_codes = pd.factorize(WM.hh)[0]
        best_w = copy_counts(hh_codes[wdon], np.nan_to_num(W) != 0)
        info = dict(probe="TR", tag=tag, seed=seed, n_records=n, order=cols, tax_min_leaf=20,
                    wealth_min_leaf=WM.leaf, generation_seconds=time.time() - ts, tax_trees=fit_info,
                    file=base + ".parquet", slurm_job_id=os.environ.get("SLURM_JOB_ID"),
                    share_5plus_nonzero_tax_amounts_from_one_puf_record=float(np.average(best_tax >= 5, weights=wg)),
                    share_5plus_nonzero_wealth_from_one_scf_household=float(np.average(best_w >= 5, weights=wg)),
                    itemizer_share=float(np.average(fded == 1, weights=wg)))
        json.dump(info, open(base + "_info.json", "w"), indent=1)
        log(f"seed {seed}: wrote {base}.parquet in {info['generation_seconds']:.0f}s; "
            f"itemizers {info['itemizer_share']:.3f}; copies tax {info['share_5plus_nonzero_tax_amounts_from_one_puf_record']:.4f} "
            f"wealth {info['share_5plus_nonzero_wealth_from_one_scf_household']:.4f}")
        out[seed] = base
    return out


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--mode", required=True, choices=["generate", "puf_wealth", "scf_test", "scf_test_nobr"])
    p.add_argument("--arm", default="split", choices=["split", "zero"])
    p.add_argument("--wealth-leaf", type=int, default=50)
    p.add_argument("--draws", type=int, default=5)
    p.add_argument("--seeds", type=int, nargs="+", default=[1, 2])
    p.add_argument("--n", type=int, default=N_GEN)
    p.add_argument("--tag", default=None)
    p.add_argument("--scf-weight-variable", action="store_true")
    p.add_argument("--scf-unweighted-fit", action="store_true")
    a = p.parse_args()
    t0 = time.time()

    def log(s):
        print(f"[{time.time() - t0:7.1f}s] {s}", flush=True)

    D = Data()
    rec, tok = D.split("train")
    WM = WealthModel(D, rec, tok, a.wealth_leaf, log, weight_variable=a.scf_weight_variable,
                     unweighted_fit=a.scf_unweighted_fit)
    wtag = ("_wv" if a.scf_weight_variable else "") + ("_uf" if a.scf_unweighted_fit else "")
    for fi in WM.fit_info:
        log(f"  tree {fi}")
    if a.mode == "generate":
        tag = a.tag or f"joint_leaf{a.wealth_leaf}{wtag}"
        generate(D, WM, a.seeds, a.n, tag, log)
    else:
        tag = a.tag or f"{a.mode}_{a.arm}_leaf{a.wealth_leaf}{wtag}"
        base, df, copy = completion(D, WM, a.mode, a.arm, a.draws, 20261014 + {"puf_wealth": 1, "scf_test": 2, "scf_test_nobr": 3}[a.mode] * 100 + a.wealth_leaf, tag, log)
        info = dict(probe="TR", mode=a.mode, arm=a.arm, scf_weight_variable=a.scf_weight_variable, scf_unweighted_fit=a.scf_unweighted_fit, wealth_min_leaf=a.wealth_leaf, draws=a.draws,
                    n_rows=len(df), file=base + ".parquet", copying=copy, wealth_trees=WM.fit_info,
                    slurm_job_id=os.environ.get("SLURM_JOB_ID"), seconds=time.time() - t0)
        json.dump(info, open(base + "_info.json", "w"), indent=1)
    log("done")


if __name__ == "__main__":
    main()
