"""Piece E of test 3: statistics of one set of completed wealth draws, with
optional per-draw weights, on the SCF training records of SPEC.md.

  python e_evaluate.py --label m1 --draws $P/a/ref_draws.npz \
        --weights $P/b/weights_ref.npz --dollars $P/a/ref_dollars.parquet
  python e_evaluate.py --truth             # the true records (and decoded truth)
  python e_evaluate.py --yardstick --draws $P/a/ref_draws.npz --dollars ...
        # Monte Carlo yardstick: reference draws against each other
  python e_evaluate.py --auto              # every input that is ready and new

Each call writes $P/e/<label>/stats.json: every statistic as a level (not a
gap), plus how far the draws moved from the reference (KL and effective
draws) and the target fit. `e_compare.py` sets the levels side by side and
takes the gaps against the truth.

Weights: a draws file alone means each of a record's K draws carries 1/K of
the record's population weight. With a weights file, draw j of record i
carries w_ij (the w of each record sums to its population weight).

Every statistic is weighted by population weight. Amount ranks use the
rank band of each token (the band of the weighted rank among the wave's
positive holders that the token stands for), so they do not depend on
where within a band a decoded dollar amount falls.
"""
import argparse
import glob
import importlib
import json
import os
import sys
import time
from datetime import datetime

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
T2CODE = os.path.join(HERE, "..", "scf_joint")
sys.path.insert(0, HERE)
sys.path.insert(1, T2CODE)
from c_common import (WEALTH, ASSETS, DEBTS, INCOMES, SHARED_CAT, Dataset,  # noqa: E402
                      wquantile, wrank, wcorr, top_shares, jsonable)

P = os.environ.get("POSTTRAIN_P", "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain")
T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
OUT = os.environ.get("E_OUT", os.path.join(P, "e"))

INC5_EDGES = [0.5, 0.9, 0.99, 0.999]
INC5_LABELS = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
SEED = 20261009
IA = [WEALTH.index(v) for v in ASSETS]
ID = [WEALTH.index(v) for v in DEBTS]

# Conditional holding rates shown individually: (holds X, given Y, words).
# Y is a wealth component, a tuple of components (holds any), or
# "inc:<income>" (that income nonzero). "Holds" means a nonzero amount.
COND = [
    ("primary_mortgage", "primary_home", "Mortgage, among homeowners"),
    ("other_mortgage", "other_home", "Mortgage on other property, among owners of other homes"),
    ("equities", "pass_throughs", "Stocks, among owners of a business (pass-through)"),
    ("equities", "inc:business", "Stocks, among units with business income"),
    ("pass_throughs", "inc:business", "Business (pass-through) asset, among units with business income"),
    ("kg_primary_home", "primary_home", "Unrealized gain or loss on the home, among homeowners"),
    ("kg_other_re", ("other_home", "re_fund"), "Unrealized gain or loss on other real estate, among its owners"),
    ("kg_pass_throughs", "pass_throughs", "Unrealized gain or loss on the business, among business owners"),
    ("kg_other", "equities", "Unrealized gain or loss on stocks and funds, among stockholders"),
    ("other_home", "inc:rent", "Other homes, among units with rental income"),
    ("primary_home", "equities", "Home, among stockholders"),
    ("credit_cards", "primary_home", "Credit-card debt, among homeowners"),
]

T0 = time.time()


def log(msg):
    print(f"[{time.time() - T0:7.1f}s] {msg}", flush=True)


def try_import(name):
    try:
        return importlib.import_module(name)
    except Exception as e:                       # noqa: BLE001
        log(f"  {name} not importable ({type(e).__name__}: {e})")
        return None


# --------------------------------------------------------------------------
# The records
# --------------------------------------------------------------------------

class Base:
    """The evaluation records, in the record order of $P/a/records.parquet
    (or, before piece A's file exists, SCF training rows with wealth
    observed, in expB order)."""

    def __init__(self):
        ds = Dataset("expB", "train", os.path.join(T2, "data"))
        self.ds = ds
        self.tinfo = ds.tinfo
        rec = ds.records
        src = rec["source"].astype(str).to_numpy()
        scf = src == "SCF"
        wo = rec["wealth_observed"].fillna(False).to_numpy().astype(bool)
        apath = os.path.join(P, "a", "records.parquet")
        self.from_a = os.path.exists(apath)
        if self.from_a:
            A = pd.read_parquet(apath)
            rcol = next((c for c in ("row", "row_id", "train_row", "expb_row") if c in A.columns), None)
            if rcol is None:
                raise SystemExit(f"records.parquet has no row-id column: {list(A.columns)[:20]}")
            rows = A[rcol].to_numpy().astype(np.int64)
            self.A = A
        else:
            rows = np.flatnonzero(scf & wo)
            self.A = None
        if not (scf[rows].all() and wo[rows].all()):
            raise SystemExit("evaluation rows include PUF rows or rows with wealth unobserved")
        self.rows = rows
        self.N = len(rows)
        r = rec.iloc[rows].reset_index(drop=True)
        self.year = r["year"].to_numpy().astype(int)
        self.hh = self.year.astype(np.int64) * 10**9 + r["hh_id"].astype(np.int64).to_numpy()
        self.W = r["weight"].to_numpy().astype(float)
        if np.isnan(self.W).any() or (self.W <= 0).any():
            raise SystemExit("record weights missing or not positive")
        # income groups and income rank: ranked over all SCF training units of
        # the wave, split-offs included (SPEC.md, family inc)
        tot_all = rec[INCOMES].to_numpy().astype(float).sum(axis=1)
        if np.isnan(tot_all[scf]).any():
            raise SystemExit(f"{int(np.isnan(tot_all[scf]).sum())} SCF training rows lack an income")
        rank_all = np.full(len(rec), np.nan)
        for y in np.unique(rec["year"].to_numpy()[scf]):
            idx = np.flatnonzero(scf & (rec["year"].to_numpy() == y))
            rank_all[idx] = wrank(tot_all[idx], rec["weight"].to_numpy()[idx].astype(float))
        self.inc_rank = rank_all[rows]
        self.inc5 = np.searchsorted(np.array(INC5_EDGES), self.inc_rank, side="right").astype(np.int8)
        if self.A is not None:
            gcol = next((c for c in ("inc", "g_inc", "grp_inc", "inc_group") if c in self.A.columns), None)
            if gcol is not None:
                ga = self.A[gcol].to_numpy()
                try:
                    agree = float((ga.astype(int) == self.inc5).mean())
                    log(f"  income groups agree with piece A's column {gcol} on {100 * agree:.2f}% of records")
                    if agree < 1:
                        log("  WARNING: income groups differ from piece A's; using piece A's")
                        self.inc5 = ga.astype(np.int8)
                except (TypeError, ValueError):
                    log(f"  piece A's {gcol} is not an integer group code; using E's own")
        self.inc_total = tot_all[rows]
        self.tokv = {v: ds.col(v)[rows].astype(np.int64) for v in SHARED_CAT + INCOMES}
        self.incd = {v: r[v].to_numpy().astype(float) for v in INCOMES}
        self.ttok = np.column_stack([ds.col(v)[rows] for v in WEALTH]).astype(np.int64)
        self.tdol = np.column_stack([r[v].to_numpy().astype(float) for v in WEALTH])
        nan = {v: int(np.isnan(self.tdol[:, j]).sum()) for j, v in enumerate(WEALTH)
               if np.isnan(self.tdol[:, j]).any()}
        if nan or (self.ttok < 0).any():
            raise SystemExit(f"true wealth missing: dollars {nan}, tokens {int((self.ttok < 0).sum())}")
        self.age = self.tokv["age_range"]
        self.ages = sorted(np.unique(self.age).tolist())
        am = self.ds.vmeta["age_range"]["tokens"]
        self.age_labels = {int(t.get("token", i)): str(t.get("meaning", t.get("value", i)))
                           for i, t in enumerate(am)}
        log(f"records: {self.N:,} ({'piece A order' if self.from_a else 'expB order, piece A file absent'}), "
            f"years {sorted(set(self.year.tolist()))}, total weight {self.W.sum():,.0f}")


# --------------------------------------------------------------------------
# Inputs
# --------------------------------------------------------------------------

def load_draws(path, base):
    z = np.load(path)
    tok = z["tokens"].astype(np.int64)
    if tok.ndim != 3 or tok.shape[0] != base.N or tok.shape[2] != len(WEALTH):
        raise SystemExit(f"{path}: tokens shape {tok.shape}, expected [{base.N}, K, {len(WEALTH)}]")
    if (tok < 0).any():
        raise SystemExit(f"{path}: {int((tok < 0).sum())} missing (negative) tokens")
    out = {"tokens": tok}
    for k in ("logp_ref", "logp_new"):
        if k in z.files:
            out[k] = z[k].astype(np.float64)
            if np.isnan(out[k]).any():
                raise SystemExit(f"{path}: {k} has {int(np.isnan(out[k]).sum())} missing values")
    return out


def load_weights(path, base, K):
    z = np.load(path)
    w = z["w"].astype(np.float64)
    if w.shape != (base.N, K):
        raise SystemExit(f"{path}: w shape {w.shape}, expected {(base.N, K)}")
    if np.isnan(w).any() or (w < 0).any():
        raise SystemExit(f"{path}: weights missing ({int(np.isnan(w).sum())}) or negative")
    rel = np.abs(w.sum(1) / base.W - 1).max()
    if rel > 1e-6:
        log(f"  WARNING: {path}: a record's draw weights differ from its population weight "
            f"by up to {100 * rel:.4f}%")
    return w


def load_dollars(path, base, K):
    D = pd.read_parquet(path)
    rc = next(c for c in ("record", "rec", "i", "row_index") if c in D.columns)
    dc = next(c for c in ("draw", "j", "k") if c in D.columns)
    i = D[rc].to_numpy().astype(np.int64)
    j = D[dc].to_numpy().astype(np.int64)
    if i.max() >= base.N:
        # records may be identified by expB row id rather than position
        pos = pd.Series(np.arange(base.N), index=base.rows)
        i = pos.loc[i].to_numpy()
    out = np.full((base.N, K, len(WEALTH)), np.nan)
    for c, v in enumerate(WEALTH):
        out[i, j, c] = D[v].to_numpy().astype(float)
    if np.isnan(out).any():
        raise SystemExit(f"{path}: {int(np.isnan(out).any(axis=2).sum())} draws lack dollars")
    return out


def decode(base, tok, seed=SEED):
    """Dollars for tokens [N, K, 24], using piece A's decoder when present,
    else test 2's decoder (same tokenizer, the wave's own quantiles)."""
    N, K, _ = tok.shape
    a = try_import("a_common")
    if a is not None and hasattr(a, "decode_dollars"):
        try:
            recs = pd.read_parquet(os.path.join(P, "a", "records.parquet"))
            if not (recs["row"].to_numpy() == base.rows).all():
                raise ValueError("records order differs from the evaluation order")
            d = a.decode_dollars(tok, recs, seed)
            d = np.asarray(d, dtype=float).reshape(N, K, len(WEALTH))
            log("  decoded with a_common.decode_dollars")
            return d
        except Exception as e:                   # noqa: BLE001
            log(f"  a_common.decode_dollars failed ({type(e).__name__}: {e}); using test 2's decoder")
    from c_common import decode_wealth
    C = pd.DataFrame({"source": "SCF", "year": np.repeat(base.year, K)})
    flat = tok.reshape(N * K, len(WEALTH))
    for c, v in enumerate(WEALTH):
        C["tok_" + v] = flat[:, c]
    dec = decode_wealth(base.ds, C, seed=seed)
    return np.column_stack([dec[v].to_numpy() for v in WEALTH]).reshape(N, K, len(WEALTH))


# --------------------------------------------------------------------------
# How far the draws moved from the reference
# --------------------------------------------------------------------------

def move_size(base, w, logp_new=None, logp_ref=None):
    """KL from the reference per unit of population weight (nats), split into
    the part from the draw weights and the part from a changed model, and
    effective draws per record."""
    N, K = w.shape
    W = base.W[:, None]
    q = w / W
    with np.errstate(divide="ignore", invalid="ignore"):
        lr = np.where(q > 0, np.log(K * q), 0.0)
    kl_w = float((w * lr).sum() / base.W.sum())
    out = {"K": int(K), "kl_weights": kl_w}
    if logp_new is not None and logp_ref is not None:
        dl = logp_new - logp_ref
        out["kl_model"] = float((w * dl).sum() / base.W.sum())
        out["kl_model_unweighted"] = float((base.W * dl.mean(1)).sum() / base.W.sum())
        out["kl_total"] = kl_w + out["kl_model"]
    else:
        out["kl_total"] = kl_w
    eff = 1.0 / (q ** 2).sum(1)
    out["eff_draws_mean"] = float((base.W * eff).sum() / base.W.sum())
    out["eff_draws_lt4_share"] = float((base.W * (eff < 4)).sum() / base.W.sum())
    out["eff_draws_p10"] = float(wquantile(eff, base.W, [0.1])[0])
    out["draws_zero_weight_share"] = float((w == 0).mean())
    out["max_draw_weight_over_record"] = float(q.max())
    return out


# --------------------------------------------------------------------------
# Target fit
# --------------------------------------------------------------------------

_TARGETS = {}


def targets_table():
    if "t" in _TARGETS:
        return _TARGETS["t"]
    path = os.path.join(P, "a", "targets.parquet")
    if not os.path.exists(path):
        return None
    t = pd.read_parquet(path)
    _TARGETS["t"] = t
    return t


def cell_fit(base, tok, w):
    """Total weight in each target cell (A^T w) with piece A's builder."""
    a = try_import("a_common")
    if a is None or not hasattr(a, "contrib_index") or not base.from_a:
        return None
    recs = pd.read_parquet(os.path.join(P, "a", "records.parquet"))
    if not (recs["row"].to_numpy() == base.rows).all():
        raise SystemExit("records order differs from the evaluation order")
    N, K = w.shape
    fit = np.zeros(a.n_cells())
    step = max(1, 4000 // K) * 50          # records per chunk (memory)
    for s0 in range(0, N, step):
        sl = slice(s0, min(N, s0 + step))
        idx = a.contrib_index(tok[sl], recs.iloc[sl])
        fit += a.cell_totals(idx, w[sl].reshape(-1), a.n_cells())
    return fit


def target_fit(base, tok, w):
    t = targets_table()
    if t is None:
        return None
    fit = cell_fit(base, tok, w)
    if fit is None:
        return None
    tcol = next(c for c in ("target", "target_weight", "value") if c in t.columns)
    if len(fit) != len(t) or not (t["cell"].to_numpy() == np.arange(len(t))).all():
        raise SystemExit(f"cell totals ({len(fit)}) and targets table ({len(t)} rows) do not line up")
    d = t[["wave" if "wave" in t.columns else "year", "family", "group", "variable"]].copy()
    d.columns = ["wave", "family", "group", "variable"]
    d["target"] = t[tcol].to_numpy().astype(float)
    d["fit"] = fit
    gt = d.groupby(["wave", "family", "group", "variable"])["target"].transform("sum")
    d["group_total"] = gt
    d["err_share"] = np.abs(d["fit"] - d["target"]) / gt
    d["zero_cell"] = d["target"] == 0
    rows = []
    for fam, s in d.groupby("family", sort=False):
        rows.append({"family": fam, "cells": int(len(s)),
                     "mean_err_pct": 100 * float(s["err_share"].mean()),
                     "max_err_pct": 100 * float(s["err_share"].max()),
                     "cells_off_1pt": int((s["err_share"] > 0.01).sum()),
                     "zero_cells": int(s["zero_cell"].sum()),
                     "weight_in_zero_cells_pct": 100 * float(s.loc[s["zero_cell"], "fit"].sum()
                                                            / s["target"].sum()),
                     "misplaced_pct": 100 * float(0.5 * np.abs(s["fit"] - s["target"]).sum()
                                                  / s["target"].sum())})
    fam = pd.DataFrame(rows)
    unreached = d[(d["target"] > 0) & (d["fit"] == 0)]
    allrow = {"family": "all", "cells": int(len(d)),
              "mean_err_pct": 100 * float(d["err_share"].mean()),
              "max_err_pct": 100 * float(d["err_share"].max()),
              "cells_off_1pt": int((d["err_share"] > 0.01).sum()),
              "zero_cells": int(d["zero_cell"].sum()),
              "weight_in_zero_cells_pct": 100 * float(d.loc[d["zero_cell"], "fit"].sum() / d["target"].sum()),
              "misplaced_pct": 100 * float(0.5 * np.abs(d["fit"] - d["target"]).sum() / d["target"].sum())}
    return {"_fit": fit, "by_family": fam.to_dict("records"), "all": allrow,
            "unreached_positive_cells": int(len(unreached)),
            "unreached_by_family": unreached.groupby("family").size().to_dict(),
            "worst": d.sort_values("err_share", ascending=False).head(10)
                      [["wave", "family", "group", "variable", "target", "fit", "err_share"]]
                      .to_dict("records")}


# --------------------------------------------------------------------------
# Statistics of one weighted set of records
# --------------------------------------------------------------------------

def frame_stats(base, ridx, tok, dol, w, with_patterns=True):
    """Levels of every untargeted statistic. ridx: record of each row; tok,
    dol: [M, 24]; w: [M] population weight of each row."""
    tinfo = base.tinfo
    w = np.asarray(w, float)
    wt = w / w.sum()
    zero = np.column_stack([tinfo[v]["is_zero"][tok[:, j]] for j, v in enumerate(WEALTH)])
    negt = np.column_stack([tinfo[v]["is_neg"][tok[:, j]] for j, v in enumerate(WEALTH)])
    H = ~zero
    POS = H & ~negt
    SC = np.column_stack([tinfo[v]["score"][tok[:, j]] for j, v in enumerate(WEALTH)])
    st = {"rows": int(len(ridx))}
    dis = ((dol != 0) != H).mean(0)
    st["hold_token_dollar_disagree"] = {v: float(x) for v, x in zip(WEALTH, dis) if x > 0}
    inc = base.inc5[ridx]
    age = base.age[ridx]
    nw = dol[:, IA].sum(1) - dol[:, ID].sum(1)

    # net worth levels
    tot = float((w * nw).sum())
    g = []
    for k in range(len(INC5_LABELS)):
        m = inc == k
        g.append({"group": INC5_LABELS[k], "pop_share": float(wt[m].sum()),
                  "mean": float((w[m] * nw[m]).sum() / w[m].sum()),
                  "median": float(wquantile(nw[m], w[m], [0.5])[0]),
                  "nw_share": float((w[m] * nw[m]).sum() / tot)})
    st["nw_inc"] = g
    a = []
    for k in base.ages:
        m = age == k
        a.append({"group": base.age_labels.get(k, str(k)), "mean": float((w[m] * nw[m]).sum() / w[m].sum()),
                  "median": float(wquantile(nw[m], w[m], [0.5])[0]),
                  "nw_share": float((w[m] * nw[m]).sum() / tot)})
    st["nw_age"] = a
    qs = [0.10, 0.25, 0.50, 0.75, 0.90, 0.99]
    st["nw_q"] = {f"p{int(round(100 * q))}": float(x) for q, x in zip(qs, wquantile(nw, w, qs))}
    st["nw_mean"] = float(tot / w.sum())
    st["nw_top"] = {f"top{100 * t:g}%": v for t, v in top_shares(nw, w).items()}
    st["share_negative_nw"] = float(wt[nw < 0].sum())

    # holding: by income group and by age are targeted (the zero bin); the
    # cross of the two is not
    hold_ia, n_ia, cellw, medr, nmed = {}, {}, {}, {}, {}
    for gi in range(len(INC5_LABELS)):
        for ak in base.ages:
            m = (inc == gi) & (age == ak)
            key = f"{gi}|{ak}"
            cellw[key] = float(wt[m].sum())
            n_ia[key] = int(m.sum())
            if not m.any():
                continue
            hold_ia[key] = ((wt[m] @ H[m]) / wt[m].sum()).tolist()
            mr, nn = [], []
            for j in range(len(WEALTH)):
                mm = m & POS[:, j]
                nn.append(int(mm.sum()))
                mr.append(float(wquantile(SC[mm, j], w[mm], [0.5])[0]) if mm.any() else None)
            medr[key], nmed[key] = mr, nn
    st["inc_age"] = {"hold": hold_ia, "rows": n_ia, "cell_share": cellw, "median_rank": medr,
                     "median_rank_rows": nmed}
    st["hold_all"] = (wt @ H).tolist()

    # joint holding
    p1 = wt @ H
    M11 = (H * wt[:, None]).T @ H
    st["p1"] = p1.tolist()
    st["M11"] = M11.tolist()
    if with_patterns:
        bits = (H.astype(np.int64) << np.arange(len(WEALTH), dtype=np.int64)).sum(axis=1)
        pat = pd.Series(wt).groupby(bits).sum()
        st["patterns"] = {str(int(k)): float(v) for k, v in pat.items()}
        st["n_patterns"] = int(len(pat))
    cond = []
    for x, y, words in COND:
        hx = H[:, WEALTH.index(x)]
        if isinstance(y, tuple):
            hy = np.any(np.column_stack([H[:, WEALTH.index(v)] for v in y]), axis=1)
        elif y.startswith("inc:"):
            if base.incd.get(y[4:]) is None:      # income not observed in this dataset
                cond.append({"holds": x, "given": y, "words": words, "rate": None, "given_share": None})
                continue
            hy = base.incd[y[4:]][ridx] != 0
        else:
            hy = H[:, WEALTH.index(y)]
        cond.append({"holds": x, "given": y if isinstance(y, str) else "+".join(y), "words": words,
                     "rate": float((wt * hx * hy).sum() / (wt * hy).sum()) if (wt * hy).sum() > 0 else None,
                     "given_share": float((wt * hy).sum())})
    st["cond"] = cond

    # rank correlations (rank bands), among positive holders
    ir = base.inc_rank[ridx]
    icorr, icn = {}, {}
    for j, v in enumerate(WEALTH):
        m = POS[:, j]
        icn[v] = int(m.sum())
        icorr[v] = wcorr(SC[m, j], ir[m], w[m]) if m.sum() > 2 else None
    st["income_corr"], st["income_corr_rows"] = icorr, icn
    pc, pn = {}, {}
    for a_ in range(len(WEALTH)):
        for b_ in range(a_ + 1, len(WEALTH)):
            m = POS[:, a_] & POS[:, b_]
            key = f"{WEALTH[a_]}|{WEALTH[b_]}"
            pn[key] = int(m.sum())
            pc[key] = wcorr(SC[m, a_], SC[m, b_], w[m]) if m.sum() > 2 else None
    st["pair_corr"], st["pair_corr_rows"] = pc, pn
    return st


def flat_rows(base, tok, dol, w):
    N, K, V = tok.shape
    return (np.repeat(np.arange(N), K), tok.reshape(N * K, V), dol.reshape(N * K, V), w.reshape(N * K))


# --------------------------------------------------------------------------
# Classifier test (from test 2): completed against true records
# --------------------------------------------------------------------------

def classifier_test(A, B, seed):
    """Test 2's classifier test (c_common.classifier_test), on whatever
    t_<variable> columns the two frames carry."""
    from sklearn.ensemble import HistGradientBoostingClassifier
    from sklearn.inspection import permutation_importance
    from sklearn.metrics import roc_auc_score
    feats = [c for c in A.columns if c.startswith("t_")]
    rng = np.random.default_rng(seed)
    X = np.vstack([A[feats].to_numpy(), B[feats].to_numpy()]).astype(float)
    y = np.concatenate([np.zeros(len(A)), np.ones(len(B))])
    w = np.concatenate([A["w"].to_numpy() / A["w"].sum(), B["w"].to_numpy() / B["w"].sum()])
    hh = np.concatenate([A["hh"].to_numpy(), B["hh"].to_numpy()])
    uh = np.unique(hh)
    test_h = rng.choice(uh, size=int(0.3 * len(uh)), replace=False)
    te = np.isin(hh, test_h)
    clf = HistGradientBoostingClassifier(max_iter=300, learning_rate=0.1, early_stopping=True,
                                         validation_fraction=0.15, n_iter_no_change=20, random_state=seed)
    clf.fit(X[~te], y[~te], sample_weight=w[~te] / w[~te].mean())
    p = clf.predict_proba(X[te])[:, 1]
    auc = roc_auc_score(y[te], p, sample_weight=w[te])
    pi = permutation_importance(clf, X[te], y[te], scoring="roc_auc", n_repeats=3,
                                random_state=seed, sample_weight=w[te], n_jobs=1)
    imp = pd.DataFrame({"variable": [f[2:] for f in feats], "auc_drop": pi.importances_mean}
                       ).sort_values("auc_drop", ascending=False)
    return {"auc": float(auc), "n_fit": int((~te).sum()), "n_score": int(te.sum()),
            "iterations": int(clf.n_iter_)}, imp


def class_frame(base, tok):
    F = pd.DataFrame({"hh": base.hh, "w": base.W})
    for v in getattr(base, "cls_vars", SHARED_CAT + INCOMES):
        F["t_" + v] = base.tokv[v]
    for j, v in enumerate(WEALTH):
        F["t_" + v] = tok[:, j]
    return F


def classifier(base, tok, w, seed):
    """One draw per record, picked with probability w_ij / W_i (so weighted
    draws are represented in proportion), against the true records."""
    N, K, _ = tok.shape
    rng = np.random.default_rng(seed)
    q = w / w.sum(1, keepdims=True)
    cq = np.cumsum(q, 1)
    u = rng.random(N)[:, None]
    pick = np.minimum((cq < u).sum(1), K - 1)
    B = class_frame(base, tok[np.arange(N), pick])
    A = class_frame(base, base.ttok)
    c, imp = classifier_test(A, B, seed)
    c["top_variables"] = imp.head(6).to_dict("records")
    return c


# --------------------------------------------------------------------------
# Running one label
# --------------------------------------------------------------------------

def save_fit(d, tf, name="cell_fit.npy"):
    """Cell totals (aggregates over records, no record-level data)."""
    if tf is not None and "_fit" in tf:
        np.save(os.path.join(d, name), tf.pop("_fit"))


def write(label, out):
    d = os.path.join(OUT, label)
    os.makedirs(d, exist_ok=True)
    save_fit(d, out.get("target_fit"))
    for name, s in out.get("sets", {}).items():
        save_fit(d, s.get("target_fit"), f"cell_fit_{name}.npy")
    if CURRENT_INPUTS:
        out["inputs"] = list(CURRENT_INPUTS)
    out["time"] = datetime.now().isoformat(timespec="minutes")
    tmp = os.path.join(d, "stats.json.tmp")
    json.dump(jsonable(out), open(tmp, "w"))
    os.replace(tmp, os.path.join(d, "stats.json"))
    log(f"wrote {d}/stats.json")


def run_truth(base):
    ridx = np.arange(base.N)
    out = {"label": "truth", "kind": "truth", "n_records": base.N,
           "note": "the training records' own wealth, in survey dollars",
           "stats": frame_stats(base, ridx, base.ttok, base.tdol, base.W)}
    write("truth", out)
    # the true tokens decoded the way draws are decoded: how much of a gap in
    # dollars comes from decoding alone
    dd = decode(base, base.ttok[:, None, :])[:, 0, :]
    out = {"label": "truth_decoded", "kind": "truth_decoded", "n_records": base.N,
           "note": "the true wealth tokens put through the same decoder as the draws (decoding noise only)",
           "stats": frame_stats(base, ridx, base.ttok, dd, base.W)}
    write("truth_decoded", out)


def run_label(base, label, draws, weights=None, dollars=None, kind="draws", note="",
              calibrate=False, classify=True):
    log(f"=== {label}: {draws}" + (f" + {weights}" if weights else ""))
    D = load_draws(draws, base)
    tok = D["tokens"]
    N, K, _ = tok.shape
    wnote = None
    cell_status = None
    if weights:
        w = load_weights(weights, base, K)
        z = np.load(weights)
        if "cell_status" in z.files:
            cell_status = z["cell_status"].astype(str)
    elif calibrate:
        w, wnote = residual_calibration(base, tok, draws)
        if w is None:
            log(f"  {label}: no residual weights available; skipped")
            return
    else:
        w = np.repeat(base.W[:, None] / K, K, axis=1)
    dol = load_dollars(dollars, base, K) if dollars else decode(base, tok)
    out = {"label": label, "kind": kind, "note": note, "draws_file": os.path.abspath(draws),
           "draws_mtime": os.path.getmtime(draws),
           "weights_file": os.path.abspath(weights) if weights else None,
           "weights_note": wnote, "n_records": N, "K": K}
    out["move"] = move_size(base, w, D.get("logp_new"), D.get("logp_ref"))
    out["target_fit"] = target_fit(base, tok, w)
    if cell_status is not None and targets_table() is not None:
        t = targets_table()
        bad = ~np.isin(cell_status, ["active", "zero"])
        out["calibration_cells"] = {
            "status_counts": pd.Series(cell_status).value_counts().to_dict(),
            "infeasible": int(bad.sum()),
            "infeasible_by_family": pd.Series(t["family"].to_numpy()[bad]).value_counts().to_dict(),
            "infeasible_target_weight_share": float(t["target"].to_numpy()[bad].sum() / t["target"].sum())}
    out["stats"] = frame_stats(base, *flat_rows(base, tok, dol, w))
    if classify:
        out["classifier"] = classifier(base, tok, w, SEED + 300)
    write(label, out)
    return out


def residual_calibration(base, tok, draws):
    """Weights from B's calibrate() on these draws (used only when the method
    did not save its own residual-calibration weights)."""
    b = try_import("b_calibrate")
    if b is None or not hasattr(b, "calibrate"):
        return None, None
    recs = pd.read_parquet(os.path.join(P, "a", "records.parquet"))
    w, lam, rep = b.calibrate(tok, recs, targets_table())
    w = np.asarray(w.cpu() if hasattr(w, "cpu") else w, dtype=float)
    return w, "computed by E with b_calibrate.calibrate"


def run_yardstick(base, draws, dollars=None, draws_b=None, dollars_b=None):
    """Monte Carlo yardstick: reference draws against each other.
    draw0 / draw1: one draw per record from each of two independent sets
    (or draws 0 and 1 of one set), each a complete file at full weight;
    all_a / all_b: every draw of set a against every draw of set b (two
    independent K-draw columns); without a second set, half_a / half_b split
    the draws of one set in two."""
    D = load_draws(draws, base)
    tok = D["tokens"]
    N, K, _ = tok.shape
    dol = load_dollars(dollars, base, K) if dollars else decode(base, tok)
    if draws_b:
        tok_b = load_draws(draws_b, base)["tokens"]
        Kb = tok_b.shape[1]
        dol_b = load_dollars(dollars_b, base, Kb) if dollars_b else decode(base, tok_b, seed=SEED + 1)
        sets = {"draw0": (tok[:, [0]], dol[:, [0]]), "draw1": (tok_b[:, [0]], dol_b[:, [0]]),
                "all_a": (tok, dol), "all_b": (tok_b, dol_b)}
    else:
        h = K // 2
        sets = {"draw0": (tok[:, [0]], dol[:, [0]]), "draw1": (tok[:, [1]], dol[:, [1]])}
        if K >= 4:
            sets["half_a"] = (tok[:, :h], dol[:, :h])
            sets["half_b"] = (tok[:, h:], dol[:, h:])
    out = {"label": "yardstick", "kind": "yardstick", "draws_file": os.path.abspath(draws),
           "draws_file_b": os.path.abspath(draws_b) if draws_b else None,
           "draws_mtime": os.path.getmtime(draws), "K": K, "sets": {}}
    for name, (st_, sd_) in sets.items():
        k = st_.shape[1]
        w = np.repeat(base.W[:, None] / k, k, axis=1)
        s = {"n_draws": int(k), "stats": frame_stats(base, *flat_rows(base, st_, sd_, w)),
             "target_fit": target_fit(base, st_, w)}
        out["sets"][name] = s
        log(f"  yardstick set {name} ({k} draws) done")
    # classifier: one draw of the reference against another draw of it
    c, imp = classifier_test(class_frame(base, sets["draw0"][0][:, 0]),
                             class_frame(base, sets["draw1"][0][:, 0]), SEED + 400)
    out["classifier_draw0_vs_draw1"] = c
    write("yardstick", out)


# --------------------------------------------------------------------------
# Discovering inputs (for --auto)
# --------------------------------------------------------------------------

def plan():
    """Every evaluation whose inputs exist: (label, kwargs, input files)."""
    a, b, c, d = (os.path.join(P, x) for x in "abcd")
    jobs = []
    ref = os.path.join(a, "ref_draws.npz")
    refd = os.path.join(a, "ref_dollars.parquet")
    ref_ready = os.path.exists(ref) and glob.glob(os.path.join(a, "DONE_*ref*"))
    dev = os.path.join(a, "dev_draws.npz")
    if not ref_ready and os.path.exists(dev):
        jobs.append(("dev_ref", dict(draws=dev, kind="reference", note="piece A's development draws (K = 4)"),
                     [dev]))
        jobs.append(("yardstick", dict(yardstick=True, draws=dev), [dev]))
        for wf in sorted(glob.glob(os.path.join(b, "weights_dev*.npz"))):
            jobs.append(("dev_m1", dict(draws=dev, weights=wf, kind="method1",
                                        note="method 1 on the development draws"), [dev, wf]))
    if ref_ready:
        rd = refd if os.path.exists(refd) else None
        jobs.append(("ref", dict(draws=ref, dollars=rd, kind="reference",
                                 note="reference model, unweighted"), [ref]))
        refb = os.path.join(a, "ref_draws_b.npz")
        if os.path.exists(refb) and os.path.exists(os.path.join(a, "DONE_ref_b")):
            rdb = os.path.join(a, "ref_dollars_b.parquet")
            rdb = rdb if os.path.exists(rdb) else None
            jobs.append(("yardstick", dict(yardstick=True, draws=ref, dollars=rd, draws_b=refb,
                                           dollars_b=rdb), [ref, refb]))
        else:
            jobs.append(("yardstick", dict(yardstick=True, draws=ref, dollars=rd), [ref]))
        wf = os.path.join(b, "weights_ref.npz")
        if os.path.exists(wf) and os.path.exists(os.path.join(b, "DONE_ref")):
            jobs.append(("m1", dict(draws=ref, weights=wf, dollars=rd, kind="method1",
                                    note="method 1: reference draws reweighted"), [ref, wf]))
        k128 = os.path.join(a, "ref_draws_k128.npz")
        wf = os.path.join(b, "weights_ref_k128.npz")
        if os.path.exists(k128) and os.path.exists(wf):
            jobs.append(("m1_k128", dict(draws=k128, weights=wf, kind="method1",
                                         note="method 1 on 128 reference draws per record"), [k128, wf]))
    cd = os.path.join(c, "draws.npz")
    def finished(path, folder):
        """A DONE marker, or a report.json beside the draws and 5 quiet minutes."""
        return os.path.exists(path) and (os.path.exists(os.path.join(folder, "DONE")) or (
            os.path.exists(os.path.join(folder, "report.json")) and time.time() - os.path.getmtime(path) > 300))
    if finished(cd, c):
        jobs.append(("m2", dict(draws=cd, kind="method2", note="method 2: tilted model, before residual calibration"),
                     [cd]))
        wf = os.path.join(c, "weights_resid.npz")
        if os.path.exists(wf):
            jobs.append(("m2_cal", dict(draws=cd, weights=wf, kind="method2_cal",
                                        note="method 2 after residual calibration"), [cd, wf]))

    for vd in sorted(glob.glob(os.path.join(d, "*", ""))):
        v = os.path.basename(os.path.dirname(vd))
        dd = os.path.join(vd, "draws.npz")
        if not (os.path.exists(dd) and (glob.glob(os.path.join(d, f"DONE_{v}*")) or finished(dd, vd))):
            continue
        jobs.append((f"m3_{v}", dict(draws=dd, kind="method3",
                                     note=f"method 3 ({v}): tuned network, before residual calibration"), [dd]))
        wf = os.path.join(vd, "weights_resid.npz")
        if os.path.exists(wf):
            jobs.append((f"m3_{v}_cal", dict(draws=dd, weights=wf, kind="method3_cal",
                                             note=f"method 3 ({v}) after residual calibration"), [dd, wf]))

    return jobs


CURRENT_INPUTS = []


def stale(label, inputs):
    f = os.path.join(OUT, label, "stats.json")
    if os.path.exists(f):
        try:
            got = json.load(open(f)).get("inputs")
        except (json.JSONDecodeError, OSError):
            got = None
        if got is not None and sorted(got) != sorted(os.path.abspath(x) for x in inputs):
            return True
    if not os.path.exists(f):
        return True
    newest = max(os.path.getmtime(x) for x in inputs)
    return os.path.getmtime(f) < newest


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--label")
    ap.add_argument("--draws")
    ap.add_argument("--weights")
    ap.add_argument("--dollars")
    ap.add_argument("--draws-b")
    ap.add_argument("--dollars-b")
    ap.add_argument("--kind", default="draws")
    ap.add_argument("--truth", action="store_true")
    ap.add_argument("--yardstick", action="store_true")
    ap.add_argument("--calibrate", action="store_true")
    ap.add_argument("--auto", action="store_true")
    ap.add_argument("--no-classifier", action="store_true")
    args = ap.parse_args()
    os.makedirs(OUT, exist_ok=True)
    base = Base()
    if args.truth or (args.auto and (stale("truth", [os.path.join(T2, "data", "expB", "truth_train.npy")])
                                     or (base.from_a and stale("truth", [os.path.join(P, "a", "records.parquet")])))):
        run_truth(base)
    if args.yardstick and args.draws:
        run_yardstick(base, args.draws, args.dollars, args.draws_b, args.dollars_b)
    elif args.draws:
        run_label(base, args.label or "custom", args.draws, args.weights, args.dollars, args.kind,
                  calibrate=args.calibrate, classify=not args.no_classifier)
    if args.auto:
        attempted = os.path.join(OUT, ".attempted")
        done = set(open(attempted).read().split("\n")) if os.path.exists(attempted) else set()
        for label, kw, inputs in plan():
            if not stale(label, inputs):
                continue
            # an input still being written: wait for the next round
            if time.time() - max(os.path.getmtime(x) for x in inputs) < 120:
                continue
            key = f"{label}:{max(os.path.getmtime(x) for x in inputs):.0f}"
            if key in done:
                continue
            with open(attempted, "a") as fh:
                fh.write(key + "\n")
            CURRENT_INPUTS[:] = [os.path.abspath(x) for x in inputs]
            try:
                if kw.pop("yardstick", False):
                    run_yardstick(base, kw["draws"], kw.get("dollars"), kw.get("draws_b"), kw.get("dollars_b"))
                else:
                    run_label(base, label, **kw)
            except SystemExit as e:
                log(f"  {label} failed: {e}")
            except Exception as e:               # noqa: BLE001
                import traceback
                traceback.print_exc()
                log(f"  {label} failed: {type(e).__name__}: {e}")


if __name__ == "__main__":
    main()
