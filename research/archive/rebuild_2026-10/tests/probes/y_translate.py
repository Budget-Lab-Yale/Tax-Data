"""Translate a survey's shared variables into a reference file's concepts (D24).

Probe Y of test 4. A general, reusable function: probe X can call it for the
real SCF-to-PUF case.

What it does, per draw:
  1. Categorical variables: by a crosswalk the caller supplies (a function
     that returns, for each survey row, the reference-concept value, drawing
     where the crosswalk is probabilistic). Variables without a crosswalk are
     left as they are (taken to mean the same thing in both tables).
  2. Amounts, one at a time in the caller's order, each within cells built
     from the categorical cell variables and from a group of the running
     total of amounts already translated (so later items are translated
     within cells of earlier ones):
     a. Zero against nonzero. If the survey cell has a smaller weighted share
        nonzero than the reference cell, survey zeros are switched to
        nonzero; if larger, survey holders are switched to zero. Which
        households are switched is decided by a model of the probability of
        holding the item given the whole survey row (other shared variables
        and the caller's extra columns, normally wealth), fit on the survey
        with the survey weights and predicted out of fold (by household), so
        a household's own answer does not set its own score.
          flip_rule="proportional"  (default) draw without replacement with
              probability proportional to the score (score = probability of
              holding for zeros switched on, probability of not holding for
              holders switched off); differs across draws.
          flip_rule="rank"  take the highest scores first (the same in every
              draw apart from ties).
          flip_rule="random"  every candidate equally likely (a comparator;
              this is what D23 and D24 rule out).
          flip_rule="likely_on_random_off"  zeros switched on most likely
              first, holders switched off at random. Probe Y's finding: when
              reporting does not depend on wealth given the true amount, a
              survey zero is a hidden holder with a probability that rises
              with the probability of holding, but which holders a file
              misses is unrelated to the row (Y_REPORT.md). Recommended.
        A household switched on needs an amount: it is placed among the
        holders at the amount a second model (fit on survey holders, same
        columns) predicts for it.
     b. Amounts among nonzero values: the survey household's weighted rank
        among the cell's nonzero values (signed, so losses and gains are one
        ordered scale) is mapped to the same weighted rank of the reference
        cell's nonzero values.
  3. Weights and all unshared columns are kept unchanged.

Cells too thin in either table (fewer than `min_cell_households` distinct
households in the survey or `min_cell_ref` in the reference) are merged into
the next coarser cell, by dropping cell variables from the end of the list.

Usage:

    from y_translate import translate, TranslateSpec
    spec = TranslateSpec(
        amounts=["wages", "business", "int_div", ...],      # order of the chain
        cell_vars=["married", "age3"],                       # cells, finest last
        categorical_crosswalks={"married": my_fn},           # optional
        flip_features=["networth_asinh", "equities_asinh"],  # survey-only columns
        weight="weight", ref_weight="weight", household="hh_id",
        n_draws=5, flip_rule="proportional", total_groups=3,
        min_cell_households=40, min_cell_ref=40, seed=1)
    out, diag = translate(survey_df, reference_df, spec)

`out` has one row per survey row per draw (column `draw`), the weight
divided by n_draws (so the stacked draws sum to the survey's weight), the
translated values in the shared columns, the survey's own values in
`src_<name>`, and a column `flip_<name>` (+1 switched on, -1 switched off, 0
not switched). `diag` is a dict of weighted shares switched and rank moves
per amount, and the cell sizes used.
A crosswalk function has the signature fn(survey_df, rng) -> array of
reference-concept values for each row.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable

import numpy as np
import pandas as pd


@dataclass
class TranslateSpec:
    amounts: list
    cell_vars: list
    categorical_crosswalks: dict = field(default_factory=dict)
    flip_features: list = field(default_factory=list)
    weight: str = "weight"
    ref_weight: str = "weight"
    household: str = "hh_id"
    n_draws: int = 5
    flip_rule: str = "proportional"
    total_groups: int = 3
    min_cell_households: int = 40
    min_cell_ref: int = 40
    seed: int = 1
    model_folds: int = 5


# ---------------------------------------------------------------------------
# Weighted ranks and quantiles
# ---------------------------------------------------------------------------

def weighted_rank(x, w, rng):
    """Mid-point weighted rank in (0, 1); ties broken at random."""
    x = np.asarray(x, float)
    w = np.asarray(w, float)
    if len(x) == 0:
        return np.zeros(0)
    order = np.lexsort((rng.random(len(x)), x))
    cw = np.cumsum(w[order])
    r = np.empty(len(x))
    r[order] = (cw - w[order] / 2) / cw[-1]
    return r


def weighted_quantile_map(x_ref, w_ref, r):
    """Read ranks r off the reference's weighted quantile function."""
    order = np.argsort(x_ref, kind="stable")
    xs = np.asarray(x_ref, float)[order]
    ws = np.asarray(w_ref, float)[order]
    cw = np.cumsum(ws)
    cdf = (cw - ws / 2) / cw[-1]
    return np.interp(r, cdf, xs)


def weighted_cdf_at(x_ref, w_ref, x):
    """Weighted share of the reference at or below x (mid-point at ties)."""
    order = np.argsort(x_ref, kind="stable")
    xs = np.asarray(x_ref, float)[order]
    ws = np.asarray(w_ref, float)[order]
    cw = np.concatenate([[0.0], np.cumsum(ws)]) / ws.sum()
    lo = np.searchsorted(xs, x, side="left")
    hi = np.searchsorted(xs, x, side="right")
    return (cw[lo] + cw[hi]) / 2


def weighted_cutpoints(x, w, k):
    """Inner cut points splitting x into k weighted groups."""
    if k <= 1:
        return np.array([])
    qs = np.arange(1, k) / k
    return weighted_quantile_map(x, w, qs)


# ---------------------------------------------------------------------------
# Models for which households to switch between zero and nonzero
# ---------------------------------------------------------------------------

def _design(df, spec, exclude):
    cols = [c for c in spec.cell_vars + list(spec.categorical_crosswalks) + spec.amounts + spec.flip_features
            if c != exclude]
    cols = list(dict.fromkeys(cols))
    X = df[cols].astype(float).to_numpy()
    for j, c in enumerate(cols):
        if c in spec.amounts:
            X[:, j] = np.arcsinh(X[:, j] / 1000.0)
    return X


def fit_flip_models(survey, spec, log=print):
    """Out-of-fold probability of holding each amount, and predicted amounts."""
    from sklearn.ensemble import HistGradientBoostingClassifier, HistGradientBoostingRegressor
    rng = np.random.default_rng(spec.seed + 7)
    hh = survey[spec.household].to_numpy()
    uh = np.unique(hh)
    fold_of = dict(zip(uh, rng.integers(0, spec.model_folds, len(uh))))
    fold = np.array([fold_of[h] for h in hh])
    w = survey[spec.weight].to_numpy(float)
    prob, pred = {}, {}
    for v in spec.amounts:
        X = _design(survey, spec, v)
        y = (survey[v].to_numpy(float) != 0).astype(int)
        p = np.full(len(y), y.mean())
        a = np.zeros(len(y))
        for f in range(spec.model_folds):
            tr, te = fold != f, fold == f
            if y[tr].min() == y[tr].max():
                p[te] = y[tr].mean()
            else:
                clf = HistGradientBoostingClassifier(max_iter=200, learning_rate=0.05, max_leaf_nodes=15,
                                                     min_samples_leaf=40, random_state=spec.seed)
                clf.fit(X[tr], y[tr], sample_weight=w[tr])
                p[te] = clf.predict_proba(X[te])[:, 1]
            hold = tr & (y == 1)
            if hold.sum() > 50:
                reg = HistGradientBoostingRegressor(max_iter=200, learning_rate=0.05, max_leaf_nodes=15,
                                                    min_samples_leaf=40, random_state=spec.seed)
                reg.fit(X[hold], np.arcsinh(survey[v].to_numpy(float)[hold] / 1000.0), sample_weight=w[hold])
                a[te] = np.sinh(reg.predict(X[te])) * 1000.0
        prob[v] = np.clip(p, 1e-4, 1 - 1e-4)
        pred[v] = a
        log(f"  flip model {v}: weighted share holding {np.average(y, weights=w):.3f}, "
            f"mean predicted {np.average(p, weights=w):.3f}")
    return prob, pred


def _choose(cand_idx, score, w, mass, rule, rng):
    """Pick candidates until their weight reaches `mass`."""
    if mass <= 0 or len(cand_idx) == 0:
        return cand_idx[:0]
    if rule == "rank":
        key = score + 1e-9 * rng.random(len(score))
    elif rule == "proportional":
        key = np.log(rng.random(len(score))) / np.maximum(score, 1e-6)
    elif rule == "random":
        key = rng.random(len(score))
    else:
        raise ValueError(rule)
    order = np.argsort(-key, kind="stable")
    cw = np.cumsum(w[order])
    k = int(np.searchsorted(cw, mass, side="left"))
    # include the unit that crosses `mass` if that leaves us closer to it
    if k < len(order):
        before = cw[k - 1] if k > 0 else 0.0
        if abs(cw[k] - mass) < abs(before - mass):
            k += 1
    return cand_idx[order[:k]]


# ---------------------------------------------------------------------------
# Cells
# ---------------------------------------------------------------------------

def _assign_cells(s_keys, r_keys, s_hh, r_hh, min_s, min_r):
    """Finest level at which both tables have enough households.

    s_keys / r_keys: lists of arrays, one per cell variable (finest last).
    Returns string cell labels for survey and reference rows.
    """
    L = len(s_keys)
    s_lab = np.array(["all"] * len(s_hh), dtype=object)
    r_lab = np.array(["all"] * len(r_hh), dtype=object)
    s_done = np.zeros(len(s_hh), bool)
    r_done = np.zeros(len(r_hh), bool)
    for k in range(L, 0, -1):
        sk = pd.Series(["|".join(map(str, t)) for t in zip(*[a for a in s_keys[:k]])])
        rk = pd.Series(["|".join(map(str, t)) for t in zip(*[a for a in r_keys[:k]])])
        s_n = pd.DataFrame({"k": sk[~s_done].to_numpy(), "h": s_hh[~s_done]}).drop_duplicates().k.value_counts()
        r_n = pd.DataFrame({"k": rk[~r_done].to_numpy(), "h": r_hh[~r_done]}).drop_duplicates().k.value_counts()
        ok = set(s_n.index[s_n >= min_s]) & set(r_n.index[r_n >= min_r])
        sm = (~s_done) & sk.isin(ok).to_numpy()
        rm = (~r_done) & rk.isin(ok).to_numpy()
        s_lab[sm] = f"L{k}:" + sk[sm]
        r_lab[rm] = f"L{k}:" + rk[rm]
        s_done |= sm
        r_done |= rm
    return s_lab, r_lab


# ---------------------------------------------------------------------------
# The translation
# ---------------------------------------------------------------------------

def _translate_once(survey, reference, spec, prob, pred, rng):
    s = survey.copy()
    w_s = s[spec.weight].to_numpy(float)
    w_r = reference[spec.ref_weight].to_numpy(float)
    s_hh = s[spec.household].to_numpy()
    r_hh = reference[spec.household].to_numpy()
    for c, fn in spec.categorical_crosswalks.items():
        s[c] = fn(s, rng)
    run_s = np.zeros(len(s))
    run_r = np.zeros(len(reference))
    cells_used = {}
    for i, v in enumerate(spec.amounts):
        s_keys = [s[c].to_numpy() for c in spec.cell_vars]
        r_keys = [reference[c].to_numpy() for c in spec.cell_vars]
        if i > 0 and spec.total_groups > 1:
            cuts = weighted_cutpoints(run_r, w_r, spec.total_groups)
            s_keys.append(np.searchsorted(cuts, run_s, side="right"))
            r_keys.append(np.searchsorted(cuts, run_r, side="right"))
        s_lab, r_lab = _assign_cells(s_keys, r_keys, s_hh, r_hh, spec.min_cell_households, spec.min_cell_ref)
        cells_used[v] = int(len(np.unique(s_lab)))
        x = s[v].to_numpy(float).copy()
        xr = reference[v].to_numpy(float)
        out = x.copy()
        flip = np.zeros(len(s), int)
        for lab in np.unique(s_lab):
            si = np.flatnonzero(s_lab == lab)
            ri = np.flatnonzero(r_lab == lab)
            ws, wr = w_s[si], w_r[ri]
            nz_s = x[si] != 0
            p_ref = wr[xr[ri] != 0].sum() / wr.sum()
            target = p_ref * ws.sum()
            have = ws[nz_s].sum()
            hold = nz_s.copy()
            if target > have:
                cand = np.flatnonzero(~nz_s)
                rule_on = "rank" if spec.flip_rule == "likely_on_random_off" else spec.flip_rule
                pick = _choose(cand, prob[v][si[cand]], ws[cand], target - have, rule_on, rng)
                hold[pick] = True
                flip[si[pick]] = 1
            elif target < have:
                cand = np.flatnonzero(nz_s)
                rule_off = "random" if spec.flip_rule == "likely_on_random_off" else spec.flip_rule
                pick = _choose(cand, 1 - prob[v][si[cand]], ws[cand], have - target, rule_off, rng)
                hold[pick] = False
                flip[si[pick]] = -1
            vals = np.where(nz_s, x[si], pred[v][si])
            out[si[~hold]] = 0.0
            r_nz = xr[ri] != 0
            if hold.any():
                if r_nz.sum() == 0:
                    raise ValueError(f"{v}: reference cell {lab} has no nonzero values but survey holders remain")
                rk = weighted_rank(vals[hold], ws[hold], rng)
                out[si[hold]] = weighted_quantile_map(xr[ri][r_nz], wr[r_nz], rk)
        s["src_" + v] = survey[v].to_numpy(float)
        s["flip_" + v] = flip
        s[v] = out
        run_s = run_s + out
        run_r = run_r + xr
    return s, cells_used


def translate(survey: pd.DataFrame, reference: pd.DataFrame, spec: TranslateSpec, log=print):
    for c in spec.amounts + spec.cell_vars:
        for name, d in (("survey", survey), ("reference", reference)):
            if c not in d.columns:
                raise KeyError(f"{c} missing from {name}")
            if d[c].isna().any():
                raise ValueError(f"{c} has {int(d[c].isna().sum())} missing values in {name}")
    survey = survey.reset_index(drop=True)
    reference = reference.reset_index(drop=True)
    prob, pred = fit_flip_models(survey, spec, log=log)
    rng = np.random.default_rng(spec.seed)
    draws, cells = [], None
    for d in range(spec.n_draws):
        s, cells = _translate_once(survey, reference, spec, prob, pred, rng)
        s["draw"] = d
        s[spec.weight] = s[spec.weight] / spec.n_draws
        draws.append(s)
    out = pd.concat(draws, ignore_index=True)
    diag = movement(out, reference, spec)
    diag["cells_per_amount"] = cells
    return out, diag


def movement(out, reference, spec):
    """How far households moved: shares switched, and rank moves."""
    w = out[spec.weight].to_numpy(float)
    wr = reference[spec.ref_weight].to_numpy(float)
    rng = np.random.default_rng(0)
    res = {}
    for v in spec.amounts:
        f = out["flip_" + v].to_numpy()
        src = out["src_" + v].to_numpy(float)
        new = out[v].to_numpy(float)
        both = (src != 0) & (new != 0)
        r_before = np.full(len(out), np.nan)
        r_after = np.full(len(out), np.nan)
        nz = src != 0
        # survey rank among its own nonzero values; translated value's rank
        # among the reference's nonzero values (whole table, not within cell)
        r_before[nz] = weighted_rank(src[nz], w[nz], rng)
        rnz = reference[v].to_numpy(float) != 0
        r_after[both] = weighted_cdf_at(reference[v].to_numpy(float)[rnz], wr[rnz], new[both])
        d = np.abs(r_after[both] - r_before[both]) * 100
        res[v] = {
            "share_switched_on": float(w[f == 1].sum() / w.sum()),
            "share_switched_off": float(w[f == -1].sum() / w.sum()),
            "share_holding_before": float(w[src != 0].sum() / w.sum()),
            "share_holding_after": float(w[new != 0].sum() / w.sum()),
            "rank_move_mean_points": float(np.average(d, weights=w[both])) if both.any() else None,
            "rank_move_share_over_10": float(np.average(d > 10, weights=w[both])) if both.any() else None,
            "rank_move_share_over_25": float(np.average(d > 25, weights=w[both])) if both.any() else None,
        }
    tot_src = out[["src_" + v for v in spec.amounts]].sum(axis=1).to_numpy()
    tot_new = out[spec.amounts].sum(axis=1).to_numpy()
    tot_ref = reference[spec.amounts].sum(axis=1).to_numpy()
    rb = weighted_rank(tot_src, w, rng)
    ra = weighted_cdf_at(tot_ref, wr, tot_new)
    d = np.abs(ra - rb) * 100
    res["total_income"] = {
        "rank_move_mean_points": float(np.average(d, weights=w)),
        "rank_move_share_over_10": float(np.average(d > 10, weights=w)),
        "rank_move_share_over_25": float(np.average(d > 25, weights=w)),
        "rank_move_share_over_40": float(np.average(d > 40, weights=w)),
    }
    return res


# ---------------------------------------------------------------------------
# The rejected alternative: reweight the survey to the reference (raking)
# ---------------------------------------------------------------------------

def rake(survey, reference, margins, weight="weight", ref_weight="weight", iters=500, tol=1e-6, min_rows=30, log=print):
    """Entropy raking (iterative proportional fitting) of survey weights.

    margins: list of functions f(df) -> array of category labels; the survey's
    weighted totals in each category are set to the reference's (scaled to
    the survey's total weight). Returns the new weights.
    """
    w0 = survey[weight].to_numpy(float)
    wr = reference[ref_weight].to_numpy(float)
    scale = w0.sum() / wr.sum()
    labs = []
    for f in margins:
        ls, lr = np.asarray(f(survey)).astype(object).astype(str), np.asarray(f(reference)).astype(object).astype(str)
        # Categories with fewer than min_rows rows in either table are pooled
        # into one category per margin, so that thin cells do not make the
        # margins impossible to meet together.
        cs, cr = pd.Series(ls).value_counts(), pd.Series(lr).value_counts()
        ok = set(cs.index[cs >= min_rows]) & set(cr.index[cr >= min_rows])
        ls = np.where(pd.Series(ls).isin(ok), ls, "_pooled")
        lr = np.where(pd.Series(lr).isin(ok), lr, "_pooled")
        tgt = pd.Series(wr * scale).groupby(lr).sum()
        missing = set(np.unique(ls)) - set(tgt.index)
        if missing:
            # A survey category the reference lacks is left as it is (its
            # survey total becomes its target), and the count is reported.
            log(f"  raking: {len(missing)} survey categories absent from the reference, left unadjusted")
            extra = pd.Series(w0).groupby(ls).sum().reindex(sorted(missing))
            tgt = pd.concat([tgt, extra])
        labs.append((ls, tgt))
    w = w0.copy()
    for it in range(iters):
        worst = 0.0
        for ls, tgt in labs:
            cur = pd.Series(w).groupby(ls).sum()
            ratio = (tgt.reindex(cur.index).fillna(0) / cur).to_numpy()
            lookup = dict(zip(cur.index, ratio))
            w = w * pd.Series(ls).map(lookup).to_numpy()
            worst = max(worst, float(np.max(np.abs(ratio - 1))))
        if worst < tol:
            break
    gap = max(float(np.max(np.abs(pd.Series(w).groupby(ls).sum().reindex(tgt.index).fillna(0).to_numpy()
                                  - tgt.to_numpy()))) / w0.sum() for ls, tgt in labs)
    log(f"  raking: largest remaining gap in any category, as a share of total weight: {gap:.2e}")
    ess = w.sum() ** 2 / (w ** 2).sum()
    ess0 = w0.sum() ** 2 / (w0 ** 2).sum()
    log(f"  raking: {it + 1} passes, last largest adjustment {worst:.2e}, "
        f"weight ratio new/old p1 {np.percentile(w / w0, 1):.3f} p99 {np.percentile(w / w0, 99):.3f} "
        f"max {np.max(w / w0):.2f}; effective sample size {ess:.0f} against {ess0:.0f}")
    return w
