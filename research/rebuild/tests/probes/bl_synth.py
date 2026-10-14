"""Probe BL (test 4): a standard baseline for whole-record PUF generation, the
sequential tree synthesizer used for synthetic tax and census files (the
method of the R package synthpop with its default `cart` method).

Data: the PUF training records of probe AS's dataset full_v2s (166,156
records), with its 40 PUF variables: 31 amounts (the income items, the
adjustments, the itemized deductions and AGI_REMAINDER, which is AGI less
the items plus the adjustments) and 9 categoricals. AGI is not synthesized;
it is computed afterwards as items less adjustments plus the remainder
(as_agi.py).

Method, for one order of the 40 variables:
  1. The first variable is drawn from its weighted distribution: each
     generated record copies the first variable's value from a training
     record chosen with probability proportional to its weight.
  2. Each later variable j gets a classification tree (categoricals) or a
     regression tree (amounts), fitted on the training records, with the
     variables before j as predictors and the population weight as the
     sample weight. Trees grow until a leaf has fewer than 2 x min_leaf
     records or is pure (scikit-learn's min_samples_leaf = min_leaf; no
     depth limit, as synthpop's rpart settings with a tiny complexity
     penalty).
  3. A generated record is dropped down the tree with its own generated
     values of the earlier variables; its value of j is a real value copied
     from a training record in the leaf it lands in, chosen with probability
     proportional to weight.
Every generated record then carries weight = total training weight / N, as
in probe AS's generated files.

--unweighted: the variant of probe UW. The training records are treated as
unweighted; the weight is the first variable (drawn uniformly from the
training records), a predictor for every later tree; trees use no sample
weights and donors are drawn uniformly in a leaf. Generated records carry
their drawn weight, scaled so the file has the training population total.

Output, per sampling seed, in --out-dir: bl_<tag>_s<seed>.parquet with
gen_id, source (PUF), year (2015), weight, draw (0), each of the 40
variables in dollars (or category value) and tok_<name> (its token under
full_v2s's tokenizer, so the file reads like probe AS's generated files);
and bl_<tag>_s<seed>_info.json with the settings and timings.

    python bl_synth.py --tag sizefirst --order sizefirst --seeds 1 2
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
import as_agi  # noqa: E402

DATA = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s"
AS_SIZEFIRST_INFO = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/gen/h_mix25_sizefirst_s1_seed01_info.json"
N_GEN = 664_624


def puf_variables(meta):
    return [v for v in meta["variables"] if v["group"] in ("shared", "puf_only")]


def make_order(kind, names, cats):
    size = json.load(open(AS_SIZEFIRST_INFO))["fill"]   # probe AS's size-first order
    assert sorted(size) == sorted(names), "size-first order does not cover the 40 variables"
    if kind == "sizefirst":
        return size
    if kind == "catfirst":                               # categoricals first, then amounts by size
        return [c for c in size if c in cats] + [c for c in size if c not in cats]
    raise ValueError(kind)


def leaf_sampler(train_leaf, w):
    """Weighted draw of one training record per generated record, within the
    leaf each generated record lands in."""
    order = np.argsort(train_leaf, kind="stable")
    sl = train_leaf[order]
    cw = np.cumsum(w[order])

    def draw(gen_leaf, rng):
        lo = np.searchsorted(sl, gen_leaf, side="left")
        hi = np.searchsorted(sl, gen_leaf, side="right")
        assert (hi > lo).all(), "a generated record reached a leaf with no training record"
        base = np.where(lo > 0, cw[np.maximum(lo - 1, 0)], 0.0)
        tot = cw[hi - 1] - base
        u = base + rng.random(len(gen_leaf)) * tot
        idx = np.clip(np.searchsorted(cw, u, side="right"), lo, hi - 1)
        return order[idx]
    return draw


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--tag", required=True)
    p.add_argument("--order", default="sizefirst", choices=["sizefirst", "catfirst"])
    p.add_argument("--unweighted", action="store_true")
    p.add_argument("--min-leaf", type=int, default=5)
    p.add_argument("--seeds", type=int, nargs="+", default=[1, 2])
    p.add_argument("--n", type=int, default=N_GEN)
    p.add_argument("--out-dir", default="/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/gen")
    a = p.parse_args()
    t_start = time.time()

    meta = json.load(open(os.path.join(DATA, "meta.json")))
    pvars = puf_variables(meta)
    names = [v["name"] for v in pvars]
    cats = {v["name"] for v in pvars if v["kind"] == "categorical"}
    assert len(names) == 40, len(names)
    rec = pd.read_parquet(os.path.join(DATA, "records_train.parquet"))
    tr = rec[rec["source"].astype(str) == "PUF"].reset_index(drop=True)
    w = tr["weight"].to_numpy(float)
    assert np.isfinite(w).all() and (w > 0).all()
    na = {c: int(tr[c].isna().sum()) for c in names if tr[c].isna().any()}
    print(f"{len(tr)} PUF training records, {w.sum():,.0f} returns; missing values: {na or 'none'}", flush=True)
    # The three demographics from the PUF's demographic file (age range,
    # gender, earnings split) are missing on about 42% of training records.
    # The synthesizer treats "missing" as a value of its own: it is copied
    # from donors like any other value, so generated files carry the same
    # pattern of missing demographics. Missing values anywhere else stop the run.
    bad = {c: n for c, n in na.items() if c not in ("AGERANGE", "GENDER", "EARNSPLIT")}
    if bad:
        raise ValueError(f"unexpected missing values in PUF training records: {bad}")

    order = make_order(a.order, names, cats)
    cols = (["weight"] if a.unweighted else []) + order
    X = tr[cols].to_numpy(float)
    tw = np.ones(len(tr)) if a.unweighted else w          # sample weight in trees and donor draws
    print("order:", cols, flush=True)

    # Fit every tree once; the sampling seeds share them.
    trees, samplers, fit_info = [None], [None], []
    t0 = time.time()
    for j in range(1, len(cols)):
        y = X[:, j]
        Xp = X[:, :j]
        if cols[j] in cats:
            m = DecisionTreeClassifier(min_samples_leaf=a.min_leaf, random_state=20261011 + j)
        else:
            m = DecisionTreeRegressor(min_samples_leaf=a.min_leaf, random_state=20261011 + j)
        tj = time.time()
        m.fit(Xp, np.where(np.isnan(y), -1.0, y), sample_weight=tw)   # missing -> its own class (categoricals only)
        leaf = m.apply(Xp)
        trees.append(m)
        samplers.append(leaf_sampler(leaf, tw))
        nl = int(m.get_n_leaves())
        # Leaf sizes in records (how many real values a generated record can get).
        cnt = np.bincount(leaf)
        cnt = cnt[cnt > 0]
        fit_info.append(dict(variable=cols[j], leaves=nl, depth=int(m.get_depth()),
                             median_leaf_records=float(np.median(cnt)),
                             share_records_in_leaves_with_one_distinct_value=None,
                             seconds=time.time() - tj))
        # Share of training records in leaves where every record has the same value of j.
        df_l = pd.DataFrame({"leaf": leaf, "y": y})
        nun = df_l.groupby("leaf")["y"].nunique(dropna=False)
        fit_info[-1]["share_records_in_leaves_with_one_distinct_value"] = float(
            (df_l["leaf"].map(nun) == 1).mean())
        print(f"tree {j:2d} {cols[j]:14s} leaves {nl:7d} depth {m.get_depth():3d} "
              f"median leaf {np.median(cnt):5.0f} one-value leaves "
              f"{fit_info[-1]['share_records_in_leaves_with_one_distinct_value']:.3f} "
              f"{time.time() - tj:6.1f}s", flush=True)
    fit_seconds = time.time() - t0

    q = pd.read_parquet(os.path.join(DATA, "quantiles.parquet"))
    cache = tk._qf_cache(q)
    mini = {"variables": pvars}
    os.makedirs(a.out_dir, exist_ok=True)
    for seed in a.seeds:
        rng = np.random.default_rng(20261011 * 100 + seed)
        ts = time.time()
        G = np.empty((a.n, len(cols)))
        p0 = tw / tw.sum()
        G[:, 0] = X[rng.choice(len(tr), size=a.n, p=p0), 0]
        for j in range(1, len(cols)):
            gl = trees[j].apply(G[:, :j])
            G[:, j] = X[samplers[j](gl, rng), j]
        gen_seconds = time.time() - ts
        df = pd.DataFrame({"gen_id": np.arange(a.n), "source": "PUF", "year": 2015})
        if a.unweighted:
            wg = G[:, 0] * (w.sum() / G[:, 0].sum())
        else:
            wg = np.full(a.n, w.sum() / a.n)
        df["weight"] = wg
        df["draw"] = np.zeros(a.n, dtype=np.int64)
        gd = pd.DataFrame(G[:, (1 if a.unweighted else 0):], columns=order)
        tok_err = None
        try:
            enc_in = pd.concat([df[["source", "year"]], gd[names]], axis=1)
            tok = tk.encode(enc_in, mini, q, np.random.default_rng(seed + 5), cache=cache)
        except Exception as e:                     # keep the dollars; report
            tok, tok_err = None, repr(e)
            print("token encoding failed:", tok_err, flush=True)
        for k, n in enumerate(names):              # meta order, as in probe AS's files
            df[n] = gd[n].to_numpy()
            df[f"tok_{n}"] = (tok[:, k].astype(np.int32) if tok is not None
                              else np.full(a.n, -1, dtype=np.int32))
        base = f"bl_{a.tag}_s{seed}"
        fn = os.path.join(a.out_dir, base + ".parquet")
        df.to_parquet(fn, index=False)
        agi = as_agi.agi_from_parts(df)
        info = dict(probe="BL", tag=a.tag, method="sequential CART synthesis (synthpop-style), weighted donors in leaves",
                    unweighted=a.unweighted, min_leaf=a.min_leaf, order=cols, seed=seed, n_records=a.n,
                    n_training_records=int(len(tr)), training_weight_total=float(w.sum()),
                    fit_seconds=fit_seconds, generation_seconds=gen_seconds, trees=fit_info,
                    token_encoding_error=tok_err, total_agi_tn=float((agi * wg).sum() / 1e12),
                    file=fn, slurm_job_id=os.environ.get("SLURM_JOB_ID"))
        json.dump(info, open(os.path.join(a.out_dir, base + "_info.json"), "w"), indent=1)
        print(f"seed {seed}: wrote {fn}; generation {gen_seconds:.0f}s; total AGI "
              f"${info['total_agi_tn']:.2f}tn", flush=True)
    print(f"done in {time.time() - t_start:.0f}s", flush=True)


if __name__ == "__main__":
    main()
