"""Sequential gradient-boosted tree imputation of the 24 wealth variables.

Piece D, method 2 (SPEC.md section D). Works in rank tokens:

1. Features are the shared variables (age range, married, number of
   dependents, the seven income amounts as rank tokens) and, when the
   dataset has more than one year, the year as a number. Rank tokens are
   turned into ordered numbers (zero -> 0, a positive rank band -> 1 + its
   midpoint rank, a negative band -> minus that), so a tree can split on
   "rank above x". A shared variable that is unobserved for a record (for
   example a PUF record absent from the demographic file) is passed as a
   missing value, which the trees handle directly.
2. For each wealth variable in the dataset's completion order, a
   gradient-boosted tree classifier (scikit-learn's
   HistGradientBoostingClassifier: an ensemble of shallow decision trees
   built one after another, each correcting the errors of the ones before)
   predicts that variable's token from the features plus the wealth
   variables earlier in the order. It is trained on the records whose
   wealth is observed (the training split), weighted by the population
   weight, using each record's true earlier wealth tokens.
3. Completion: for each target record, the first wealth variable's token
   is drawn from the classifier's predicted probabilities; then the second,
   given the first draw; and so on. Five independent chains per record.
4. Tokens are decoded to dollars with a_tokenizer.decode (or, on the
   fabricated development data, a simple decoder here).

Usage (through sbatch only):
    python d_seqtrees.py --dataset expA_clean --seed 1 [--draws 5]
                         [--include-puf] [--max-iter 300]
"""

import argparse
import copy
import json
import os
import sys
import time

import numpy as np
import pandas as pd
from sklearn.ensemble import HistGradientBoostingClassifier

import d_common as dc

SHARED_DEFAULT = ["age_range", "married", "n_dep"] + dc.INCOMES


def feature_matrix(meta, tok, rec, names, use_year):
    vi = dc.var_index(meta)
    cols = []
    for n in names:
        var = dc.var_meta(meta, n)
        key = dc.token_sort_key(var)
        t = tok[:, vi[n]]
        x = np.where(t >= 0, key[np.clip(t, 0, None)], np.nan)
        cols.append(x)
    if use_year:
        cols.append(rec["year"].astype(float).values)
    return np.stack(cols, axis=1)


def weighted_log_loss(clf, X, y, w):
    """Weighted mean of -log p(true class); classes unseen in training get
    probability 1e-12 (counted, not dropped)."""
    p = clf.predict_proba(X)
    idx = np.searchsorted(clf.classes_, y)
    idx_c = np.minimum(idx, len(clf.classes_) - 1)
    seen = clf.classes_[idx_c] == y
    pr = np.where(seen, p[np.arange(len(y)), idx_c], 1e-12)
    return float(np.sum(w * -np.log(np.maximum(pr, 1e-12))) / w.sum())


def fit_early_stopped(X, y, w, X_va, y_va, w_va, args, random_state):
    """Boost in steps of 10 trees per class, scoring the weighted log loss on
    the dataset's own validation households after each step; keep the model
    at the best step and stop after 3 steps without improvement.

    scikit-learn 1.6's built-in early stopping can only hold out a random
    10% of rows, which would put implicates of one household on both sides,
    so it is not used. A ridge penalty on leaf values (l2_regularization)
    is needed: without it, rare classes with tiny weighted second
    derivatives produce exploding leaf values and the loss diverges (seen
    on the development data).
    """
    clf = HistGradientBoostingClassifier(
        learning_rate=args.learning_rate, max_iter=10, warm_start=True,
        max_leaf_nodes=args.max_leaf_nodes,
        min_samples_leaf=args.min_samples_leaf,
        l2_regularization=args.l2, early_stopping=False,
        random_state=random_state)
    best, best_it, best_loss, since = None, 0, np.inf, 0
    for it in range(10, args.max_iter + 1, 10):
        clf.set_params(max_iter=it)
        clf.fit(X, y, sample_weight=w)
        loss = weighted_log_loss(clf, X_va, y_va, w_va)
        if loss < best_loss - 1e-4:
            best, best_it, best_loss, since = copy.deepcopy(clf), it, loss, 0
        else:
            since += 1
            if since >= 3:
                break
    return best, best_it, best_loss


_QUANT = None   # quantiles.parquet of the dataset (None on fabricated data)


def restrict_to_decodable(p, classes, var, name, groups):
    """Zero the probability of tokens the target's year cannot decode.

    In experiment B the classifiers are trained on both SCF waves, so a
    negative token can be predicted for a 2016 record although only the
    2013 wave has negative values of that variable (and a_tokenizer has no
    2016 negative quantile function to decode it). Such tokens get
    probability zero and the rest is renormalised; the mass removed is
    returned so it can be reported.
    """
    if _QUANT is None:
        return p, 0.0
    p = p.copy()
    removed = 0.0
    for rows, years in groups:
        ok = dc.allowed_tokens(var, dc.sides_present(_QUANT, name, list(years)))[classes]
        if ok.all():
            continue
        sub = p[np.ix_(rows, np.flatnonzero(~ok))]
        removed += float(sub.sum())
        p[np.ix_(rows, np.flatnonzero(~ok))] = 0.0
    s = p.sum(axis=1, keepdims=True)
    if (s <= 0).any():
        raise ValueError(f"{name}: a record has no decodable token with positive probability")
    return p / s, removed


def decode_dollars(meta, toks_by_var, rec_rows, year, rng, dataset):
    """Dollars from wealth tokens, via a_tokenizer if it exists."""
    try:
        import a_tokenizer as at
    except ImportError:
        at = None
    d = dc.data_dir(dataset)
    if at is not None and "fake" not in dataset:
        quantiles = pd.read_parquet(os.path.join(d, "quantiles.parquet"))
        vi = dc.var_index(meta)
        full = np.full((len(rec_rows), len(meta["variables"])), -1, np.int32)
        for v, t in toks_by_var.items():
            full[:, vi[v]] = t
        out = at.decode(full, meta, rec_rows, quantiles, rng, year=year)
        return {v: np.asarray(out[v], dtype=float) for v in toks_by_var}
    return simple_decode(meta, toks_by_var, rec_rows, rng, dataset)


def simple_decode(meta, toks_by_var, rec_rows, rng, dataset):
    """Development-only decoder for the fabricated data's quantile layout.

    Draws a rank uniformly inside the token's band and reads the SCF
    source-year's quantile function (linear interpolation).
    """
    q = pd.read_parquet(os.path.join(dc.data_dir(dataset), "quantiles.parquet"))
    vi = dc.var_index(meta)
    out = {}
    for v, t in toks_by_var.items():
        var = dc.var_meta(meta, v)
        x = np.zeros(len(t))
        for sign in ("positive", "negative"):
            qq = q[(q.variable == v) & (q.sign == sign) & (q.source == "SCF")]
            idx = [k for k, tk in enumerate(var["tokens"]) if tk["kind"] == sign]
            m = np.isin(t, idx)
            if not m.any():
                continue
            lo = np.array([var["tokens"][k]["lo"] for k in t[m]])
            hi = np.array([var["tokens"][k]["hi"] for k in t[m]])
            r = lo + (hi - lo) * rng.random(m.sum())
            x[m] = np.interp(r, qq.p.values, qq.value.values)
        out[v] = x
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True)
    ap.add_argument("--seed", type=int, required=True)
    ap.add_argument("--draws", type=int, default=5)
    ap.add_argument("--include-puf", action="store_true")
    ap.add_argument("--max-iter", type=int, default=300)
    ap.add_argument("--learning-rate", type=float, default=0.1)
    ap.add_argument("--max-leaf-nodes", type=int, default=7)
    ap.add_argument("--min-samples-leaf", type=int, default=300)
    ap.add_argument("--l2", type=float, default=1.0)
    ap.add_argument("--name", default="seqtrees",
                    help="method name in output files (for setting variants)")
    args = ap.parse_args()
    t_start = time.time()

    meta, tok_tr, rec_tr = dc.load(args.dataset, "train")
    _, tok_va, rec_va = dc.load(args.dataset, "val")
    _, tok_te, rec_te = dc.load(args.dataset, "test")
    order = dc.generation_order(meta)
    vi = dc.var_index(meta)
    shared = [n for n in SHARED_DEFAULT if n in vi]
    missing_shared = sorted(set(SHARED_DEFAULT) - set(shared))
    if missing_shared:
        raise ValueError(f"shared variables absent from meta: {missing_shared}")
    years = sorted(set(rec_tr["year"].tolist()) | set(rec_te["year"].tolist()))
    use_year = len(years) > 1

    tr = dc.training_mask(meta, tok_tr, rec_tr)
    va = dc.training_mask(meta, tok_va, rec_va)
    print(f"training rows with wealth observed: {tr.sum()} of {len(tr)} "
          f"(the rest have wealth hidden by design and are not used)")
    print(f"validation rows with wealth observed: {va.sum()} of {len(va)}")
    tok_tr, rec_tr = tok_tr[tr], rec_tr[tr].reset_index(drop=True)
    tok_va, rec_va = tok_va[va], rec_va[va].reset_index(drop=True)

    w_tr = rec_tr["weight"].values.astype(float)
    w_va = rec_va["weight"].values.astype(float)
    if not (np.isfinite(w_tr).all() and (w_tr > 0).all()):
        raise ValueError("non-positive or missing training weights")
    w_tr = w_tr / w_tr.mean()
    w_va = w_va / w_va.mean()

    targets = {"": dc.target_rows(args.dataset, meta, rec_te, tok_te, False)}
    if args.include_puf:
        src = rec_te["source"].astype(str).values
        targets["-puf"] = np.flatnonzero(src == "PUF")
    for tag, rows in targets.items():
        print(f"target set '{tag or 'main'}': {len(rows)} records")

    X_tr = feature_matrix(meta, tok_tr, rec_tr, shared, use_year)
    X_va = feature_matrix(meta, tok_va, rec_va, shared, use_year)
    X_te = {tag: feature_matrix(meta, tok_te[rows], rec_te.iloc[rows], shared,
                                use_year) for tag, rows in targets.items()}
    # State per target set and draw: feature matrix grows with each filled var.
    state = {(tag, d): X_te[tag].copy() for tag in targets for d in range(args.draws)}
    drawn = {(tag, d): {} for tag in targets for d in range(args.draws)}

    fit_log = []
    global _QUANT
    removed_mass = {}
    year_groups = {tag: [] for tag in targets}
    if "fake" not in args.dataset:
        _QUANT = pd.read_parquet(os.path.join(dc.data_dir(args.dataset), "quantiles.parquet"))
        scf_years = sorted(int(y) for y in _QUANT.loc[_QUANT["source"] == "SCF", "year"].unique())
        for tag, rows in targets.items():
            yrs = (np.full(len(rows), 2015) if tag == "-puf"
                   else rec_te.iloc[rows]["year"].astype(int).values)
            year_groups[tag] = [(np.flatnonzero(yrs == y), tuple(dc.scf_years_for(int(y), scf_years)))
                                for y in np.unique(yrs)]
    # Shared variables unobserved for some targets (PUF records outside the
    # demographic file have no age range) are drawn first, in the same
    # sequential way, from a classifier on the other shared variables
    # trained on the training records; the model completes them too.
    for k, v in enumerate(shared):
        n_miss = {tag: int(np.isnan(X[:, k]).sum()) for tag, X in X_te.items()}
        if sum(n_miss.values()) == 0:
            continue
        if np.isnan(X_tr[:, k]).any() or np.isnan(X_va[:, k]).any():
            raise ValueError(f"{v}: unobserved in training or validation records")
        t0 = time.time()
        var = dc.var_meta(meta, v)
        key = dc.token_sort_key(var)
        others = [c for c in range(X_tr.shape[1]) if c != k]
        y_tr_k = tok_tr[:, vi[v]]
        y_va_k = tok_va[:, vi[v]]
        clf, best_it, val_loss = fit_early_stopped(
            X_tr[:, others], y_tr_k, w_tr, X_va[:, others], y_va_k, w_va, args,
            random_state=args.seed * 1000 + 900 + k)
        rng = np.random.default_rng([args.seed, 900 + k, 7])
        for (tag, d), X in state.items():
            m = np.isnan(X[:, k])
            if not m.any():
                continue
            p = clf.predict_proba(X[m][:, others])
            u = rng.random(m.sum())[:, None]
            c = np.minimum((np.cumsum(p, axis=1) < u).sum(axis=1), p.shape[1] - 1)
            X[m, k] = key[clf.classes_[c]]
        fit_log.append({"variable": v, "imputed_shared": n_miss,
                        "iterations": best_it, "val_log_loss": round(val_loss, 4),
                        "seconds": round(time.time() - t0, 1)})
        print(f"shared {v}: unobserved for {n_miss} target records; drawn from a "
              f"classifier on the other shared variables ({best_it} iterations)",
              flush=True)

    for j, v in enumerate(order):
        t0 = time.time()
        var = dc.var_meta(meta, v)
        key = dc.token_sort_key(var)
        y_tr = tok_tr[:, vi[v]]
        y_va = tok_va[:, vi[v]]
        n_cls = len(np.unique(y_tr))
        if n_cls == 1:
            clf, best_it, val_loss = None, 0, float("nan")
            only = int(y_tr[0])
        else:
            clf, best_it, val_loss = fit_early_stopped(
                X_tr, y_tr, w_tr, X_va, y_va, w_va, args,
                random_state=args.seed * 1000 + j)
        rng = np.random.default_rng([args.seed, j, 7])
        for (tag, d), X in state.items():
            if clf is None:
                t = np.full(X.shape[0], only)
            else:
                p = clf.predict_proba(X)
                p, removed = restrict_to_decodable(p, clf.classes_, var, v, year_groups[tag])
                removed_mass[v] = removed_mass.get(v, 0.0) + removed
                u = rng.random(X.shape[0])[:, None]
                c = (np.cumsum(p, axis=1) < u).sum(axis=1)
                c = np.minimum(c, p.shape[1] - 1)
                t = clf.classes_[c]
            drawn[(tag, d)][v] = t.astype(np.int32)
            state[(tag, d)] = np.column_stack([X, key[t]])
        X_tr = np.column_stack([X_tr, key[y_tr]])
        X_va = np.column_stack([X_va, key[np.clip(y_va, 0, None)]])
        fit_log.append({"variable": v, "classes": n_cls, "iterations": best_it,
                        "val_log_loss": round(val_loss, 4),
                        "seconds": round(time.time() - t0, 1)})
        print(f"[{j+1:2d}/{len(order)}] {v:18s} classes={n_cls:3d} iters={best_it:4d} "
              f"val loss={val_loss:.3f} {time.time()-t0:6.1f}s", flush=True)

    for tag, rows in targets.items():
        rec_target = rec_te.iloc[rows]
        k = args.draws
        draws = np.repeat(np.arange(k), len(rows))
        toks = {v: np.concatenate([drawn[(tag, d)][v] for d in range(k)]) for v in dc.WEALTH}
        rec_stacked = pd.concat([rec_target.reset_index(drop=True)] * k, ignore_index=True)
        year = 2015 if tag == "-puf" else None
        rng = np.random.default_rng([args.seed, 99])
        dollars = decode_dollars(meta, toks, rec_stacked, year, rng, args.dataset)
        path = dc.write_completed(args.dataset, args.name + tag, args.seed,
                                  rec_target, rows, draws, dollars, toks)
        print("wrote", path)

    run = {"dataset": args.dataset, "seed": args.seed, "draws": args.draws,
           "features": shared + (["year"] if use_year else []),
           "order": order, "fits": fit_log,
           "settings": {k: getattr(args, k) for k in
                        ("max_iter", "learning_rate", "max_leaf_nodes",
                         "min_samples_leaf", "l2")},
           "n_train": int(len(rec_tr)), "n_val": int(len(rec_va)),
           "undecodable_probability_removed_per_record_draw": {
               v: m / sum(len(r) for r in targets.values()) / args.draws
               for v, m in removed_mass.items() if m > 0},
           "seconds": round(time.time() - t_start, 1)}
    with open(os.path.join(dc.comp_dir(args.dataset),
                           f"run_{args.name}_seed{args.seed:02d}.json"), "w") as f:
        json.dump(run, f, indent=1)
    print(f"done in {time.time()-t_start:.0f}s")


if __name__ == "__main__":
    main()
