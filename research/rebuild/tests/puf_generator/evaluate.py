#!/usr/bin/env python
"""Evaluation for test 1 (SPEC.md, section C).

Compares candidate files of tax records with the held-out test records
(`values_test.parquet`). Every measure is computed for:

  floor        the training records themselves (real records; the gap to the
               test records is sampling noise only);
  independent  each variable drawn on its own from its weighted distribution in
               the training records (every relationship between variables lost);
  <label>      the candidate passed with --synthetic, if any.

Floor and independent are computed once per data vintage and cached under
$S/eval/_cache/ (the cache key includes this file's source, so editing the
measures invalidates it).

Usage (always through Slurm, see evaluate.sbatch):
  python evaluate.py --label baseline                     # floor + independent only
  python evaluate.py --synthetic $S/model/synthetic.parquet --label model
  python evaluate.py --make-dev-split $S/eval/dev/data    # temporary split from raw PUF
  ... --data-dir <dir>          read values_*.parquet from another folder
  ... --update-report <md>      rewrite the generated blocks in EVAL_REPORT.md

Outputs in $S/eval/<label>/: summary.json, CSV tables, PNG plots and
report_tables.md (the generated tables, aggregate statistics only).

Weighting: every distribution comparison uses each file's own `weight`,
normalised to sum to one. The classifier and copying tests use records as
sampled, with the weight as one of the variables.
"""

import argparse
import hashlib
import json
import os
import pickle
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
CODE_DIR = Path(__file__).resolve().parent
SEED = 20261008

WEIGHT = "weight"
CATEGORICAL = ["MARS", "DSI", "XTOT", "EIC", "N24", "FDED",
               "AGERANGE", "GENDER", "EARNSPLIT"]
AMOUNTS = ["E00100", "E00200", "E00300", "E00400", "E00600", "E00650", "E00900",
           "E01100", "P22250", "P23250", "E01400", "E01500", "E01700", "E02100",
           "E02300", "E02400", "E25850", "E25860", "E26270", "E26390", "E26400",
           "E03150", "E03210", "E03270", "E17500", "E18400", "E18500", "E19200",
           "E19800", "E20100"]
VARIABLES = [WEIGHT] + CATEGORICAL + AMOUNTS
DEMOGRAPHIC = ["AGERANGE", "GENDER", "EARNSPLIT"]

# Income variables eligible for the nonzero-pattern measure (the 15 with the
# largest weighted nonzero share in the test records are used).
INCOME_VARS = ["E00200", "E00300", "E00400", "E00600", "E00650", "E00900",
               "E01100", "P22250", "P23250", "E01400", "E01500", "E01700",
               "E02100", "E02300", "E02400", "E25850", "E25860", "E26270",
               "E26390", "E26400"]

LABELS = {
    "E00100": "AGI", "E00200": "wages", "E00300": "taxable interest",
    "E00400": "tax-exempt interest", "E00600": "ordinary dividends",
    "E00650": "qualified dividends", "E00900": "Schedule C net",
    "E01100": "cap. gain distributions", "P22250": "short-term gain (net)",
    "P23250": "long-term gain (net)", "E01400": "taxable IRA dist.",
    "E01500": "total pensions", "E01700": "taxable pensions",
    "E02100": "Schedule F net", "E02300": "unemployment comp.",
    "E02400": "gross Social Security", "E25850": "rent/royalty income",
    "E25860": "rent/royalty loss", "E26270": "partnership/S corp net",
    "E26390": "estate/trust income", "E26400": "estate/trust loss",
    "E03150": "IRA deduction", "E03210": "student loan interest",
    "E03270": "SE health insurance", "E17500": "medical expenses",
    "E18400": "state/local income or sales tax", "E18500": "real estate tax",
    "E19200": "interest paid", "E19800": "cash charity",
    "E20100": "noncash charity", "weight": "weight", "MARS": "filing status",
    "DSI": "dependent filer", "XTOT": "exemptions", "EIC": "EITC children",
    "N24": "CTC children", "FDED": "deduction type", "AGERANGE": "age range",
    "GENDER": "gender", "EARNSPLIT": "earnings split",
}

QUANTILES = [0.10, 0.25, 0.50, 0.75, 0.90, 0.99, 0.999]
GROUP_EDGES = [0, .1, .2, .3, .4, .5, .6, .7, .8, .9, .99, .999, 1.0]
GROUP_NAMES = ["P0-10", "P10-20", "P20-30", "P30-40", "P40-50", "P50-60",
               "P60-70", "P70-80", "P80-90", "P90-99", "P99-99.9", "P99.9-100"]

# Three-way tables. Each entry: name, list of three "columns", where a column
# is either a variable (binned as in the pair tables) or a derived indicator.
TRIPLES = [
    ("AGI band x filing status x net capital gain sign",
     ["E00100", "MARS", "cg_sign"]),
    ("AGI band x deduction type x age range", ["E00100", "FDED", "AGERANGE"]),
    ("wage decile x dividend decile x filing status", ["E00200", "E00600", "MARS"]),
    ("AGI band x Schedule C sign x partnership/S corp sign",
     ["E00100", "E00900_sign", "E26270_sign"]),
    ("AGI band x has Social Security x age range",
     ["E00100", "E02400_pos", "AGERANGE"]),
    ("AGI band x EITC children x filing status", ["E00100", "EIC", "MARS"]),
    ("AGI band x deduction type x state/local tax band",
     ["E00100", "FDED", "E18400"]),
    ("AGI band x CTC children x exemptions", ["E00100", "N24", "XTOT"]),
    ("wage band x taxable pension band x age range",
     ["E00200", "E01700", "AGERANGE"]),
]

# Plot colours (reference palette, light mode): test is neutral ink, then
# categorical slots 1-3 in fixed order.
COLORS = {"test": "#0b0b0b", "floor": "#2a78d6", "independent": "#eb6834",
          "floor_binned": "#4a3aa7"}
MODEL_COLOR = "#1baf7a"

# Approximate-AGI parameters for tax year 2015.
SS_WAGE_BASE_2015 = 118_500
# Copying: thresholds at these percentiles of the test records' distances.
COPY_Q = [5, 10]
GAP_EDGES = [-np.inf, -0.5, -0.1, -0.02, -0.002, 0.002, 0.02, 0.1, 0.5, np.inf]
GAP_NAMES = ["< -50%", "-50% to -10%", "-10% to -2%", "-2% to -0.2%",
             "within 0.2%", "0.2% to 2%", "2% to 10%", "10% to 50%", "> 50%"]


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


# --------------------------------------------------------------------------
# Loading
# --------------------------------------------------------------------------

def load_values(path, name):
    df = pd.read_parquet(path)
    missing = [v for v in VARIABLES if v not in df.columns]
    if missing:
        raise SystemExit(f"{name}: missing columns {missing} in {path}")
    out = pd.DataFrame(index=np.arange(len(df)))
    for v in VARIABLES:
        col = pd.to_numeric(df[v], errors="coerce").to_numpy()
        n_na = int(np.isnan(col.astype(float)).sum())
        if n_na:
            raise SystemExit(f"{name}: {n_na} missing or non-numeric values in "
                             f"{v}; the evaluation does not drop values silently.")
        out[v] = col.astype(np.int64) if v in CATEGORICAL else col.astype(np.float64)
    n_nonpos = int((out[WEIGHT] <= 0).sum())
    if n_nonpos:
        raise SystemExit(f"{name}: {n_nonpos} records with weight <= 0.")
    log(f"loaded {name}: {len(out):,} records from {path}")
    return out


def norm_w(df):
    w = df[WEIGHT].to_numpy()
    return w / w.sum()


# --------------------------------------------------------------------------
# Weighted statistics
# --------------------------------------------------------------------------

def wquantile(x, w, qs):
    """Weighted quantile: the smallest value whose weighted cumulative share is
    at least q (the inverse of the weighted distribution function)."""
    order = np.argsort(x, kind="mergesort")
    xs, cw = x[order], np.cumsum(w[order])
    cw = cw / cw[-1]
    idx = np.searchsorted(cw, np.asarray(qs), side="left")
    return xs[np.minimum(idx, len(xs) - 1)]


def wrank(x, w):
    """Weighted mid-rank in [0, 1]: share of weight below the value plus half
    the weight at the value (ties share one rank)."""
    order = np.argsort(x, kind="mergesort")
    xs, ws = x[order], w[order] / w.sum()
    uniq, start = np.unique(xs, return_index=True)
    block_w = np.add.reduceat(ws, start)
    below = np.concatenate([[0.0], np.cumsum(block_w)[:-1]])
    mid = below + block_w / 2
    r = np.empty(len(x))
    r[order] = np.repeat(mid, np.diff(np.append(start, len(xs))))
    return r


def agi_groups(df, w):
    r = wrank(df["E00100"].to_numpy(), w)
    return np.searchsorted(np.array(GROUP_EDGES[1:-1]), r, side="right")


# --------------------------------------------------------------------------
# Binning shared across candidates (set from the test records)
# --------------------------------------------------------------------------

class Binner:
    """Amounts: negative, zero, then deciles of positive values among test
    records (weighted). Weight: weighted deciles of test weights.
    Categoricals: their own values; a value not seen in train or test goes to
    an 'other' code."""

    def __init__(self, test, train):
        wt = norm_w(test)
        self.edges, self.cats, self.K = {}, {}, {}
        for v in AMOUNTS:
            x = test[v].to_numpy()
            pos = x > 0
            e = wquantile(x[pos], wt[pos], np.arange(1, 10) / 10) if pos.any() else np.array([])
            self.edges[v] = np.unique(e)
            self.K[v] = 2 + len(self.edges[v]) + 1
        e = wquantile(test[WEIGHT].to_numpy(), wt, np.arange(1, 10) / 10)
        self.edges[WEIGHT] = np.unique(e)
        self.K[WEIGHT] = len(self.edges[WEIGHT]) + 1
        for v in CATEGORICAL:
            cats = np.unique(np.concatenate([test[v].to_numpy(), train[v].to_numpy()]))
            self.cats[v] = cats
            self.K[v] = len(cats) + 1
        # derived indicators for the triples
        self.K.update({"cg_sign": 3, "E00900_sign": 3, "E26270_sign": 3,
                       "E02400_pos": 2})

    def code(self, df, v):
        if v in AMOUNTS:
            x = df[v].to_numpy()
            c = 2 + np.searchsorted(self.edges[v], x, side="right")
            c[x == 0] = 1
            c[x < 0] = 0
            return c
        if v == WEIGHT:
            return np.searchsorted(self.edges[WEIGHT], df[WEIGHT].to_numpy(), side="right")
        if v in CATEGORICAL:
            cats = self.cats[v]
            x = df[v].to_numpy()
            i = np.searchsorted(cats, x)
            i = np.minimum(i, len(cats) - 1)
            return np.where(cats[i] == x, i, len(cats))
        if v == "cg_sign":
            return np.sign(df["P22250"].to_numpy() + df["P23250"].to_numpy()).astype(int) + 1
        if v.endswith("_sign"):
            return np.sign(df[v[:-5]].to_numpy()).astype(int) + 1
        if v == "E02400_pos":
            return (df["E02400"].to_numpy() > 0).astype(int)
        raise KeyError(v)

    def codes(self, df, cols):
        return {v: self.code(df, v) for v in cols}


def table(codes, cols, K, w):
    idx = np.ravel_multi_index([codes[c] for c in cols], [K[c] for c in cols])
    return np.bincount(idx, weights=w, minlength=int(np.prod([K[c] for c in cols])))


def tvd(p, q):
    return 0.5 * np.abs(p / p.sum() - q / q.sum()).sum()


# --------------------------------------------------------------------------
# Approximate AGI (2015 Form 1040, lines 7-37, from the variables available)
# --------------------------------------------------------------------------

def approx_agi(df):
    g = {v: df[v].to_numpy() for v in AMOUNTS}
    mars = df["MARS"].to_numpy()
    mfj, mfs = mars == 2, mars == 3
    # Schedule D: net short- plus long-term gain, losses limited to $3,000
    # ($1,500 married filing separately).
    capg = np.maximum(g["P22250"] + g["P23250"], np.where(mfs, -1500.0, -3000.0))
    # Schedule E: rents and royalties, partnerships and S corporations,
    # estates and trusts (losses stored as positive amounts).
    sched_e = (g["E25850"] - g["E25860"] + g["E26270"] + g["E26390"] - g["E26400"])
    # Deduction for half of self-employment tax on Schedule C and F profit.
    se_earn = 0.9235 * np.maximum(g["E00900"] + g["E02100"], 0.0)
    se_earn = np.where(se_earn >= 400, se_earn, 0.0)
    se_tax = (0.124 * np.minimum(se_earn, np.maximum(SS_WAGE_BASE_2015 - g["E00200"], 0.0))
              + 0.029 * se_earn)
    other = (g["E00200"] + g["E00300"] + g["E00600"] + g["E00900"] + capg + g["E01100"]
             + g["E01400"] + g["E01700"] + g["E02100"] + g["E02300"] + sched_e
             - g["E03150"] - g["E03210"] - g["E03270"] - 0.5 * se_tax)
    # Taxable Social Security (Form 1040 worksheet).
    ss = g["E02400"]
    b1 = np.where(mfj, 32000.0, np.where(mfs, 0.0, 25000.0))
    b2 = np.where(mfj, 44000.0, np.where(mfs, 0.0, 34000.0))
    prov = other + g["E00400"] + 0.5 * ss
    tier1 = np.minimum(0.5 * (prov - b1), 0.5 * ss)
    tier2 = np.minimum(0.85 * (prov - b2) + np.minimum(0.5 * (b2 - b1), 0.5 * ss), 0.85 * ss)
    taxable_ss = np.where(prov <= b1, 0.0, np.where(prov <= b2, tier1, tier2))
    return other + np.maximum(taxable_ss, 0.0)


def agi_gap_bins(df):
    agi = df["E00100"].to_numpy()
    rel = (agi - approx_agi(df)) / np.maximum(np.abs(agi), 1000.0)
    return np.searchsorted(np.array(GAP_EDGES[1:-1]), rel, side="right"), rel


# --------------------------------------------------------------------------
# Copying: rank transform and nearest-neighbour distances
# --------------------------------------------------------------------------

class RankTransform:
    """Maps each variable to its mid-rank among the training records (0-1)."""

    def __init__(self, train):
        self.sorted = {v: np.sort(train[v].to_numpy().astype(float)) for v in VARIABLES}

    def __call__(self, df):
        cols = []
        for v in VARIABLES:
            s, x = self.sorted[v], df[v].to_numpy().astype(float)
            lo = np.searchsorted(s, x, side="left")
            hi = np.searchsorted(s, x, side="right")
            cols.append((lo + hi) / (2.0 * len(s)))
        return np.column_stack(cols).astype(np.float64)


def nearest_distance(query, ref, chunk=1024):
    """Euclidean distance from each query row to its nearest reference row
    (exact, brute force in chunks)."""
    import torch
    torch.set_num_threads(max(1, int(os.environ.get("SLURM_CPUS_PER_TASK", "4"))))
    R = torch.from_numpy(ref)
    rn = (R * R).sum(1)
    out = np.empty(len(query), dtype=np.float64)
    for s in range(0, len(query), chunk):
        Q = torch.from_numpy(query[s:s + chunk])
        d2 = (Q * Q).sum(1)[:, None] + rn[None, :] - 2.0 * Q @ R.T
        nn = d2.argmin(1)
        # recompute the distance to the chosen record directly, so identical
        # records come out at exactly zero (the expansion above loses that)
        out[s:s + chunk] = torch.sqrt(((Q - R[nn]) ** 2).sum(1)).numpy()
    return out


def row_hashes(df):
    return pd.util.hash_pandas_object(df[VARIABLES].round(6), index=False).to_numpy()


def token_hashes(df, meta):
    sys.path.insert(0, str(CODE_DIR))
    import tokenizer  # part A's tokenizer
    tok = tokenizer.encode(df, meta)
    return pd.util.hash_pandas_object(pd.DataFrame(tok), index=False).to_numpy()


# --------------------------------------------------------------------------
# The measures, for one candidate
# --------------------------------------------------------------------------

def evaluate_candidate(name, cand, ctx):
    t0 = time.time()
    test, train, B = ctx["test"], ctx["train"], ctx["binner"]
    wc, wt = norm_w(cand), ctx["wt"]
    res = {"name": name, "n_records": len(cand),
           "total_weight": float(cand[WEIGHT].sum())}

    # 1. single variables -------------------------------------------------
    rows = []
    for v in AMOUNTS + [WEIGHT]:
        x = cand[v].to_numpy()
        rows.append((v, "share_zero", float(wc[x == 0].sum())))
        rows.append((v, "share_negative", float(wc[x < 0].sum())))
        for q, val in zip(QUANTILES, wquantile(x, wc, QUANTILES)):
            rows.append((v, f"q{q * 100:g}", float(val)))
        rows.append((v, "mean", float((wc * x).sum())))
    for v in CATEGORICAL:
        x = cand[v].to_numpy()
        for c in B.cats[v]:
            rows.append((v, f"share_{c}", float(wc[x == c].sum())))
        rows.append((v, "share_other", float(wc[~np.isin(x, B.cats[v])].sum())))
    res["single"] = pd.DataFrame(rows, columns=["variable", "statistic", "value"])

    codes = B.codes(cand, VARIABLES + ["cg_sign", "E00900_sign", "E26270_sign", "E02400_pos"])
    tc = ctx["test_codes"]
    res["marginal_tvd"] = pd.Series(
        {v: tvd(table(codes, [v], B.K, wc), table(tc, [v], B.K, wt)) for v in VARIABLES})

    # 2. two- and three-variable tables ------------------------------------
    pairs = []
    for i, a in enumerate(VARIABLES):
        for b in VARIABLES[i + 1:]:
            pairs.append((a, b, tvd(table(codes, [a, b], B.K, wc),
                                    ctx["test_pair_tables"][(a, b)])))
    res["pairs"] = pd.DataFrame(pairs, columns=["var_a", "var_b", "tvd"])
    res["triples"] = pd.Series(
        {nm: tvd(table(codes, cols, B.K, wc), table(tc, cols, B.K, wt))
         for nm, cols in TRIPLES})

    # 3. nonzero patterns ---------------------------------------------------
    pv = ctx["pattern_vars"]
    pat = np.zeros(len(cand), dtype=np.int64)
    for k, v in enumerate(pv):
        pat |= (cand[v].to_numpy() != 0).astype(np.int64) << k
    pc = pd.Series(wc).groupby(pat).sum()
    pt = ctx["test_patterns"]
    allp = pc.index.union(pt.index)
    pcv, ptv = pc.reindex(allp, fill_value=0), pt.reindex(allp, fill_value=0)
    res["patterns"] = {
        "tvd": float(0.5 * np.abs(pcv - ptv).sum()),
        "n_patterns": int((pc > 0).sum()),
        "share_in_patterns_absent_from_test": float(pc[~pc.index.isin(pt.index)].sum()),
        "shares": pc,
    }

    # 4. conditional profiles by AGI percentile group ------------------------
    grp = agi_groups(cand, wc)
    gw = np.bincount(grp, weights=wc, minlength=len(GROUP_NAMES))
    prof = []
    for v in AMOUNTS:
        x = cand[v].to_numpy()
        nz = np.bincount(grp, weights=wc * (x != 0), minlength=len(GROUP_NAMES))
        sm = np.bincount(grp, weights=wc * x, minlength=len(GROUP_NAMES))
        with np.errstate(invalid="ignore", divide="ignore"):
            for g in range(len(GROUP_NAMES)):
                prof.append((v, GROUP_NAMES[g], "share_nonzero", nz[g] / gw[g]))
                prof.append((v, GROUP_NAMES[g], "mean", sm[g] / gw[g]))
    res["profiles"] = pd.DataFrame(prof, columns=["variable", "group", "statistic", "value"])
    res["group_weight_share"] = gw

    # 5. the top --------------------------------------------------------------
    r = wrank(cand["E00100"].to_numpy(), wc)
    top = []
    for v in AMOUNTS:
        xp = np.maximum(cand[v].to_numpy(), 0.0)
        tot = (wc * xp).sum()
        for cut, lab in [(0.99, "top1"), (0.999, "top0.1")]:
            top.append((v, lab, float((wc * xp)[r >= cut].sum() / tot) if tot > 0 else np.nan))
    a = cand["E00100"].to_numpy()
    for cut, lab in [(0.99, "top1"), (0.999, "top0.1")]:
        top.append(("E00100_net", lab, float((wc * a)[r >= cut].sum() / (wc * a).sum())))
    res["top"] = pd.DataFrame(top, columns=["variable", "group", "share"])

    # 6. classifier test ------------------------------------------------------
    res["classifier"] = classifier_test(cand, test, B, ctx["seed"])

    # 7. AGI consistency -------------------------------------------------------
    gb, rel = agi_gap_bins(cand)
    res["agi_gap_bins"] = np.bincount(gb, weights=wc, minlength=len(GAP_NAMES))
    res["agi_gap_quantiles"] = wquantile(rel, wc, [0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99])
    res["agi_gap_tvd"] = float(tvd(res["agi_gap_bins"], ctx["test_gap_bins"]))

    # 8. copying ----------------------------------------------------------------
    if name == "floor":
        res["copy"] = ctx["test_copy"]   # floor of the copying test: test records
    else:
        res["copy"] = copying_test(cand, ctx)

    res["seconds"] = time.time() - t0
    log(f"{name}: done in {res['seconds']:.0f}s")
    return res


def classifier_test(cand, test, B, seed):
    from sklearn.ensemble import HistGradientBoostingClassifier
    from sklearn.inspection import permutation_importance
    from sklearn.metrics import roc_auc_score
    from sklearn.model_selection import train_test_split

    rng = np.random.default_rng(seed + 11)
    n = min(len(cand), len(test))
    ci = rng.choice(len(cand), n, replace=False)
    ti = rng.choice(len(test), n, replace=False)

    def feats(df):
        X = np.column_stack([B.code(df, v) if v in CATEGORICAL else df[v].to_numpy()
                             for v in VARIABLES]).astype(float)
        return X
    X = np.vstack([feats(cand.iloc[ci]), feats(test.iloc[ti])])
    y = np.concatenate([np.ones(n), np.zeros(n)])
    Xtr, Xho, ytr, yho = train_test_split(X, y, test_size=0.3, stratify=y,
                                          random_state=seed + 12)
    clf = HistGradientBoostingClassifier(
        max_iter=500, learning_rate=0.1, early_stopping=True,
        validation_fraction=0.15, n_iter_no_change=20, random_state=seed + 13,
        categorical_features=[v in CATEGORICAL for v in VARIABLES])
    clf.fit(Xtr, ytr)
    p = clf.predict_proba(Xho)[:, 1]
    auc = roc_auc_score(yho, p)
    acc = float(((p > 0.5) == yho).mean())
    pi = permutation_importance(clf, Xho, yho, scoring="roc_auc", n_repeats=5,
                                random_state=seed + 14, n_jobs=1)
    imp = pd.DataFrame({"variable": VARIABLES, "auc_drop": pi.importances_mean,
                        "auc_drop_sd": pi.importances_std}).sort_values("auc_drop", ascending=False)
    return {"n_per_class": int(n), "n_holdout": int(len(yho)), "auc": float(auc),
            "accuracy": acc, "iterations": int(clf.n_iter_), "importance": imp}


def copying_test(cand, ctx):
    q = ctx["rank"](cand)
    d = nearest_distance(q, ctx["train_ranks"])
    td = ctx["test_copy"]["distances"]
    out = {"distances": d,
           "share_identical_values": float(np.isin(row_hashes(cand), ctx["train_hashes"]).mean())}
    for q in COPY_Q:
        out[f"share_within_test_p{q}"] = float((d <= np.quantile(td, q / 100)).mean())
    out["median_ratio_to_test"] = float(np.median(d) / np.median(td))
    if ctx.get("train_token_hashes") is not None:
        try:
            out["share_identical_tokens"] = float(
                np.isin(token_hashes(cand, ctx["meta"]), ctx["train_token_hashes"]).mean())
        except Exception as e:  # noqa: BLE001
            out["token_error"] = repr(e)
    return out


# --------------------------------------------------------------------------
# Baselines and context
# --------------------------------------------------------------------------

def make_independent(train, seed):
    """Each variable drawn independently. The weight is drawn from the
    training records' weights as sampled; every other variable is drawn with
    probability proportional to the training weight, so each weighted
    marginal matches train in expectation and every relationship is lost."""
    rng = np.random.default_rng(seed)
    n = len(train)
    p = norm_w(train)
    out = pd.DataFrame(index=np.arange(n))
    out[WEIGHT] = train[WEIGHT].to_numpy()[rng.integers(0, n, n)]
    for v in CATEGORICAL + AMOUNTS:
        out[v] = train[v].to_numpy()[rng.choice(n, n, replace=True, p=p)]
    return out[VARIABLES]


def binned_round_trip(train, meta, seed):
    """The training records passed through part A's tokenizer: encoded to
    tokens and decoded back (draws within each bin). Shows how much of any gap
    comes from binning alone, before any model is involved."""
    sys.path.insert(0, str(CODE_DIR))
    import tokenizer
    dec = tokenizer.decode(tokenizer.encode(train, meta), meta, np.random.default_rng(seed))
    out = pd.DataFrame({v: dec[v].to_numpy() for v in VARIABLES})
    for v in CATEGORICAL:
        out[v] = out[v].astype(np.int64)
    return out


def build_context(test, train, seed, meta):
    log("building context (bins, test tables, rank transform)")
    B = Binner(test, train)
    wt = norm_w(test)
    tc = B.codes(test, VARIABLES + ["cg_sign", "E00900_sign", "E26270_sign", "E02400_pos"])
    ctx = {"test": test, "train": train, "binner": B, "wt": wt, "seed": seed,
           "test_codes": tc, "meta": meta}
    ctx["test_pair_tables"] = {(a, b): table(tc, [a, b], B.K, wt)
                               for i, a in enumerate(VARIABLES) for b in VARIABLES[i + 1:]}
    nzs = {v: wt[test[v].to_numpy() != 0].sum() for v in INCOME_VARS}
    ctx["pattern_vars"] = sorted(INCOME_VARS, key=lambda v: -nzs[v])[:15]
    pat = np.zeros(len(test), dtype=np.int64)
    for k, v in enumerate(ctx["pattern_vars"]):
        pat |= (test[v].to_numpy() != 0).astype(np.int64) << k
    ctx["test_patterns"] = pd.Series(wt).groupby(pat).sum()
    ctx["test_gap_bins"] = np.bincount(agi_gap_bins(test)[0], weights=wt, minlength=len(GAP_NAMES))
    ctx["rank"] = RankTransform(train)
    ctx["train_ranks"] = ctx["rank"](train)
    ctx["train_hashes"] = row_hashes(train)
    ctx["train_token_hashes"] = None
    if meta is not None:
        try:
            ctx["train_token_hashes"] = token_hashes(train, meta)
        except Exception as e:  # noqa: BLE001
            log(f"token-level copy check unavailable: {e!r}")
    log("nearest-training-record distances for test records (copying floor)")
    td = nearest_distance(ctx["rank"](test), ctx["train_ranks"])
    ctx["test_copy"] = {"distances": td,
                        "share_identical_values": float(np.isin(row_hashes(test), ctx["train_hashes"]).mean()),
                        **{f"share_within_test_p{q}": float((td <= np.quantile(td, q / 100)).mean())
                           for q in COPY_Q},
                        "median_ratio_to_test": 1.0}
    if ctx["train_token_hashes"] is not None:
        ctx["test_copy"]["share_identical_tokens"] = float(
            np.isin(token_hashes(test, meta), ctx["train_token_hashes"]).mean())
    log("context ready")
    return ctx


def test_reference(ctx):
    """Statistics of the test records themselves, for the tables."""
    test, wt = ctx["test"], ctx["wt"]
    ref = evaluate_reference_stats(test, wt, ctx["binner"])
    ref["agi_gap_bins"] = ctx["test_gap_bins"]
    ref["agi_gap_quantiles"] = wquantile(agi_gap_bins(test)[1], wt,
                                         [0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99])
    ref["patterns"] = {"shares": ctx["test_patterns"],
                       "n_patterns": int((ctx["test_patterns"] > 0).sum())}
    ref["copy"] = ctx["test_copy"]
    ref["n_records"] = len(test)
    ref["total_weight"] = float(test[WEIGHT].sum())
    return ref


def evaluate_reference_stats(df, w, B):
    """Single-variable, profile and top statistics only (no comparisons)."""
    fake_ctx_rows = {}
    rows = []
    for v in AMOUNTS + [WEIGHT]:
        x = df[v].to_numpy()
        rows.append((v, "share_zero", float(w[x == 0].sum())))
        rows.append((v, "share_negative", float(w[x < 0].sum())))
        for q, val in zip(QUANTILES, wquantile(x, w, QUANTILES)):
            rows.append((v, f"q{q * 100:g}", float(val)))
        rows.append((v, "mean", float((w * x).sum())))
    for v in CATEGORICAL:
        x = df[v].to_numpy()
        for c in B.cats[v]:
            rows.append((v, f"share_{c}", float(w[x == c].sum())))
        rows.append((v, "share_other", float(w[~np.isin(x, B.cats[v])].sum())))
    fake_ctx_rows["single"] = pd.DataFrame(rows, columns=["variable", "statistic", "value"])
    grp = agi_groups(df, w)
    gw = np.bincount(grp, weights=w, minlength=len(GROUP_NAMES))
    prof = []
    for v in AMOUNTS:
        x = df[v].to_numpy()
        nz = np.bincount(grp, weights=w * (x != 0), minlength=len(GROUP_NAMES))
        sm = np.bincount(grp, weights=w * x, minlength=len(GROUP_NAMES))
        for g in range(len(GROUP_NAMES)):
            prof.append((v, GROUP_NAMES[g], "share_nonzero", nz[g] / gw[g]))
            prof.append((v, GROUP_NAMES[g], "mean", sm[g] / gw[g]))
    fake_ctx_rows["profiles"] = pd.DataFrame(prof, columns=["variable", "group", "statistic", "value"])
    r = wrank(df["E00100"].to_numpy(), w)
    top = []
    for v in AMOUNTS:
        xp = np.maximum(df[v].to_numpy(), 0.0)
        tot = (w * xp).sum()
        for cut, lab in [(0.99, "top1"), (0.999, "top0.1")]:
            top.append((v, lab, float((w * xp)[r >= cut].sum() / tot) if tot > 0 else np.nan))
    a = df["E00100"].to_numpy()
    for cut, lab in [(0.99, "top1"), (0.999, "top0.1")]:
        top.append(("E00100_net", lab, float((w * a)[r >= cut].sum() / (w * a).sum())))
    fake_ctx_rows["top"] = pd.DataFrame(top, columns=["variable", "group", "share"])
    return fake_ctx_rows


def file_key(paths, extra=""):
    h = hashlib.sha256(Path(__file__).read_bytes())
    for p in paths:
        st = os.stat(p)
        h.update(f"{p}:{st.st_size}:{st.st_mtime_ns}".encode())
    h.update(extra.encode())
    return h.hexdigest()[:16]


# --------------------------------------------------------------------------
# Output: tables, plots, summary, markdown
# --------------------------------------------------------------------------

def assemble(results, ref, train_total, outdir, label):
    names = [r["name"] for r in results]
    outdir.mkdir(parents=True, exist_ok=True)

    tot = pd.DataFrame([{"file": "test", "n_records": ref["n_records"],
                         "total_weight": ref["total_weight"],
                         "ratio_to_train": ref["total_weight"] / train_total}] +
                       [{"file": r["name"], "n_records": r["n_records"],
                         "total_weight": r["total_weight"],
                         "ratio_to_train": r["total_weight"] / train_total} for r in results])
    tot.to_csv(outdir / "totals.csv", index=False)

    single = ref["single"].rename(columns={"value": "test"})
    for r in results:
        single[r["name"]] = r["single"]["value"].to_numpy()
    single.to_csv(outdir / "single_variables.csv", index=False)

    mt = pd.DataFrame({r["name"]: r["marginal_tvd"] for r in results})
    mt.index.name = "variable"
    mt.to_csv(outdir / "marginal_tvd.csv")

    pairs = results[0]["pairs"][["var_a", "var_b"]].copy()
    for r in results:
        pairs[r["name"]] = r["pairs"]["tvd"].to_numpy()
    for n in names:
        if n != "floor":
            pairs[f"{n}_minus_floor"] = pairs[n] - pairs["floor"]
    pairs.to_csv(outdir / "pair_tvd.csv", index=False)
    pq = pd.DataFrame({n: pairs[n].quantile([0.1, 0.25, 0.5, 0.75, 0.9, 1.0]).to_numpy()
                       for n in names},
                      index=["p10", "p25", "median", "p75", "p90", "max"])
    for n in names:
        pq.loc["mean", n] = pairs[n].mean()
    pq.index.name = "across_pairs"
    pq.to_csv(outdir / "pair_tvd_summary.csv")

    tri = pd.DataFrame({r["name"]: r["triples"] for r in results})
    tri.index.name = "triple"
    tri.to_csv(outdir / "triple_tvd.csv")

    # patterns
    allp = ref["patterns"]["shares"].index
    for r in results:
        allp = allp.union(r["patterns"]["shares"].index)
    pat = pd.DataFrame({"test": ref["patterns"]["shares"].reindex(allp, fill_value=0)})
    for r in results:
        pat[r["name"]] = r["patterns"]["shares"].reindex(allp, fill_value=0)
    pv = results[0]["pattern_vars"]
    pat.insert(0, "nonzero_variables", [
        "+".join(v for k, v in enumerate(pv) if (p >> k) & 1) or "(none)" for p in pat.index])
    pat = pat.sort_values("test", ascending=False)
    pat.head(40).to_csv(outdir / "nonzero_patterns_top40.csv", index=False)

    prof = ref["profiles"].rename(columns={"value": "test"})
    for r in results:
        prof[r["name"]] = r["profiles"]["value"].to_numpy()
    prof.to_csv(outdir / "profiles.csv", index=False)

    top = ref["top"].rename(columns={"share": "test"})
    for r in results:
        top[r["name"]] = r["top"]["share"].to_numpy()
    top.to_csv(outdir / "top_shares.csv", index=False)

    cls = pd.DataFrame([{"file": r["name"], "auc": r["classifier"]["auc"],
                         "accuracy": r["classifier"]["accuracy"],
                         "n_per_class": r["classifier"]["n_per_class"],
                         "n_holdout": r["classifier"]["n_holdout"],
                         "boosting_iterations": r["classifier"]["iterations"]} for r in results])
    cls.to_csv(outdir / "classifier.csv", index=False)
    imp = pd.concat([r["classifier"]["importance"].assign(file=r["name"]) for r in results])
    imp.to_csv(outdir / "classifier_importance.csv", index=False)

    gap = pd.DataFrame({"gap_bin": GAP_NAMES, "test": ref["agi_gap_bins"]})
    for r in results:
        gap[r["name"]] = r["agi_gap_bins"]
    gap.to_csv(outdir / "agi_gap_bins.csv", index=False)
    gq = pd.DataFrame({"quantile": [0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99],
                       "test": ref["agi_gap_quantiles"]})
    for r in results:
        gq[r["name"]] = r["agi_gap_quantiles"]
    gq.to_csv(outdir / "agi_gap_quantiles.csv", index=False)

    cq = [0.001, 0.01, 0.05, 0.25, 0.5, 0.75]
    cp = pd.DataFrame({"quantile": cq, "test": np.quantile(ref["copy"]["distances"], cq)})
    for r in results:
        if r["name"] != "floor":
            cp[r["name"]] = np.quantile(r["copy"]["distances"], cq)
    cp.to_csv(outdir / "copying_distance_quantiles.csv", index=False)

    # profile summary: mean absolute gap from test in share nonzero
    prof_sum = {}
    sn = prof[prof.statistic == "share_nonzero"]
    for n in names:
        prof_sum[n] = float(np.nanmean(np.abs(sn[n] - sn["test"])))

    summary = {
        "label": label,
        "files": {"test": int(ref["n_records"]), **{r["name"]: int(r["n_records"]) for r in results}},
        "total_weight_ratio_to_train": dict(zip(tot.file, tot.ratio_to_train)),
        "pattern_variables": pv,
        "seed": SEED,
    }
    for r in results:
        n = r["name"]
        c = r["copy"]
        summary[n] = {
            "marginal_tvd_median": float(r["marginal_tvd"].median()),
            "marginal_tvd_max": float(r["marginal_tvd"].max()),
            "marginal_tvd_worst": r["marginal_tvd"].idxmax(),
            "pair_tvd_median": float(pairs[n].median()),
            "pair_tvd_p90": float(pairs[n].quantile(0.9)),
            "pair_tvd_max": float(pairs[n].max()),
            "pair_worst": " x ".join(pairs.loc[pairs[n].idxmax(), ["var_a", "var_b"]]),
            "triple_tvd": {k: float(v) for k, v in r["triples"].items()},
            "triple_tvd_mean": float(r["triples"].mean()),
            "pattern_tvd": r["patterns"]["tvd"],
            "pattern_count": r["patterns"]["n_patterns"],
            "pattern_share_absent_from_test": r["patterns"]["share_in_patterns_absent_from_test"],
            "profile_share_nonzero_mean_abs_gap": prof_sum[n],
            "top1_agi_share": float(top.query("variable=='E00100' and group=='top1'")[n].iloc[0]),
            "top01_agi_share": float(top.query("variable=='E00100' and group=='top0.1'")[n].iloc[0]),
            "classifier_auc": r["classifier"]["auc"],
            "classifier_accuracy": r["classifier"]["accuracy"],
            "classifier_top_variables": r["classifier"]["importance"].head(5)["variable"].tolist(),
            "agi_gap_tvd": r["agi_gap_tvd"],
            "agi_gap_share_within_0.2pct": float(r["agi_gap_bins"][GAP_NAMES.index("within 0.2%")]),
            "agi_gap_share_within_2pct": float(r["agi_gap_bins"][3:6].sum()),
            "copy_median_distance_ratio_to_test": c["median_ratio_to_test"],
            "copy_share_within_test_p5": c["share_within_test_p5"],
            "copy_share_within_test_p10": c["share_within_test_p10"],
            "copy_share_identical_values": c["share_identical_values"],
            "copy_share_identical_tokens": c.get("share_identical_tokens"),
            "seconds": r["seconds"],
        }
    summary["test"] = {
        "top1_agi_share": float(top.query("variable=='E00100' and group=='top1'")["test"].iloc[0]),
        "top01_agi_share": float(top.query("variable=='E00100' and group=='top0.1'")["test"].iloc[0]),
        "agi_gap_share_within_0.2pct": float(ref["agi_gap_bins"][GAP_NAMES.index("within 0.2%")]),
        "agi_gap_share_within_2pct": float(ref["agi_gap_bins"][3:6].sum()),
        "copy_median_distance": float(np.median(ref["copy"]["distances"])),
        "copy_share_identical_values": ref["copy"]["share_identical_values"],
        "copy_share_within_test_p5": ref["copy"]["share_within_test_p5"],
        "copy_share_within_test_p10": ref["copy"]["share_within_test_p10"],
        "copy_share_identical_tokens": ref["copy"].get("share_identical_tokens"),
    }
    with open(outdir / "summary.json", "w") as f:
        json.dump(summary, f, indent=2, default=float)

    make_plots(results, ref, prof, pairs, gap, outdir)
    md = markdown_tables(summary, names, tot, mt, single, pairs, tri, pat, top, cls, imp, gap, cp, ref)
    (outdir / "report_tables.md").write_text(md["headline"] + "\n\n" + md["detail"])
    return summary, md


def make_plots(results, ref, prof, pairs, gap, outdir):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    plt.rcParams.update({"font.size": 8, "axes.spines.top": False,
                         "axes.spines.right": False, "axes.grid": True,
                         "grid.color": "#e4e3df", "grid.linewidth": 0.6,
                         "axes.edgecolor": "#8a8984", "axes.labelcolor": "#52514e",
                         "xtick.color": "#52514e", "ytick.color": "#52514e"})
    names = [r["name"] for r in results]
    col = {n: COLORS.get(n, MODEL_COLOR) for n in names}
    col["test"] = COLORS["test"]
    series = ["test"] + names
    x = np.arange(len(GROUP_NAMES))

    for stat, fname, ylab in [("share_nonzero", "profiles_share_nonzero.png", "share nonzero"),
                              ("mean", "profiles_mean.png", "weighted mean ($)")]:
        fig, axes = plt.subplots(6, 5, figsize=(16, 17))
        sub = prof[prof.statistic == stat]
        for ax, v in zip(axes.flat, AMOUNTS):
            d = sub[sub.variable == v]
            for s in series:
                ax.plot(x, d[s].to_numpy(), color=col[s], lw=2 if s == "test" else 1.6,
                        ls="-" if s != "test" else "--", marker="o", ms=2.5, label=s)
            ax.set_title(f"{v} {LABELS[v]}", fontsize=8)
            ax.set_xticks(x[::3])
            ax.set_xticklabels([GROUP_NAMES[i] for i in x[::3]], fontsize=6)
            if stat == "mean":
                ax.set_yscale("symlog", linthresh=100)
        fig.legend(*axes.flat[0].get_legend_handles_labels(), loc="upper center",
                   ncol=len(series), frameon=False, fontsize=10)
        fig.suptitle(f"{ylab} by AGI percentile group (each file ranked on its own AGI)",
                     y=0.985, fontsize=11)
        fig.tight_layout(rect=[0, 0, 1, 0.965])
        fig.savefig(outdir / fname, dpi=110)
        plt.close(fig)

    fig, ax = plt.subplots(figsize=(7, 4))
    for n in names:
        v = np.sort(pairs[n].to_numpy())
        ax.plot(v, np.arange(1, len(v) + 1) / len(v), color=col[n], lw=2, label=n)
    ax.set_xlabel("total variation distance of the two-way table (vs test)")
    ax.set_ylabel("share of variable pairs at or below")
    ax.set_title("Two-way tables: distribution across all variable pairs")
    ax.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(outdir / "pair_tvd_cdf.png", dpi=130)
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(8, 4))
    k = len(series)
    wbar = 0.8 / k
    for i, s in enumerate(series):
        ax.bar(np.arange(len(GAP_NAMES)) + (i - (k - 1) / 2) * wbar, gap[s], width=wbar * 0.92,
               color=col[s], label=s)
    ax.set_xticks(np.arange(len(GAP_NAMES)))
    ax.set_xticklabels(GAP_NAMES, rotation=30, ha="right")
    ax.set_ylabel("weighted share of records")
    ax.set_title("Reported AGI minus approximate AGI, relative to max(|AGI|, $1,000)")
    ax.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(outdir / "agi_gap.png", dpi=130)
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(7, 4))
    floor_d = 1e-4  # identical records (distance 0) are drawn at this value
    td = np.sort(np.maximum(ref["copy"]["distances"], floor_d))
    ax.plot(td, np.arange(1, len(td) + 1) / len(td), color=col["test"], lw=2, ls="--",
            label="test (floor)")
    for r in results:
        if r["name"] == "floor":
            continue
        d = np.sort(np.maximum(r["copy"]["distances"], floor_d))
        ax.plot(d, np.arange(1, len(d) + 1) / len(d), color=col[r["name"]], lw=2, label=r["name"])
    ax.set_xscale("log")
    ax.set_xlabel("distance to nearest training record (rank scale, 40 variables; "
                  "identical records shown at 0.0001)")
    ax.set_ylabel("share of records at or below")
    ax.set_title("Nearest-training-record distances")
    ax.legend(frameon=False)
    fig.tight_layout()
    fig.savefig(outdir / "copying_distance_cdf.png", dpi=130)
    plt.close(fig)

    fig, axes = plt.subplots(1, len(results), figsize=(4.2 * len(results), 5), sharex=False)
    for ax, r in zip(np.atleast_1d(axes), results):
        imp = r["classifier"]["importance"].head(15)[::-1]
        ax.barh(imp["variable"], imp["auc_drop"], color=col[r["name"]], height=0.7)
        ax.set_title(f"{r['name']} (AUC {r['classifier']['auc']:.3f})")
        ax.set_xlabel("fall in AUC when the variable is shuffled")
    fig.suptitle("Classifier test: variables the classifier relies on (top 15)")
    fig.tight_layout()
    fig.savefig(outdir / "classifier_importance.png", dpi=130)
    plt.close(fig)


def fmt(x, nd=3):
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return "n/a"
    return f"{x:.{nd}f}"


def pct(x, nd=1):
    if x is None or (isinstance(x, float) and np.isnan(x)):
        return "n/a"
    return f"{100 * x:.{nd}f}%"


def mdtable(header, rows):
    out = ["| " + " | ".join(header) + " |", "|" + "|".join(["---"] * len(header)) + "|"]
    out += ["| " + " | ".join(str(c) for c in r) + " |" for r in rows]
    return "\n".join(out)


def markdown_tables(summary, names, tot, mt, single, pairs, tri, pat, top, cls, imp, gap, cp, ref):
    S_ = summary
    h = []
    h.append(mdtable(
        ["Measure", "Test records", *names],
        [
            ["Total weight relative to train", pct(S_["total_weight_ratio_to_train"]["test"]),
             *[pct(S_["total_weight_ratio_to_train"][n]) for n in names]],
            ["Single variables: median TVD across 40 variables", "",
             *[fmt(S_[n]["marginal_tvd_median"]) for n in names]],
            ["Single variables: largest TVD", "",
             *[f"{fmt(S_[n]['marginal_tvd_max'])} ({S_[n]['marginal_tvd_worst']})" for n in names]],
            ["Two-way tables: median TVD across 780 pairs", "",
             *[fmt(S_[n]["pair_tvd_median"]) for n in names]],
            ["Two-way tables: 90th percentile across pairs", "",
             *[fmt(S_[n]["pair_tvd_p90"]) for n in names]],
            ["Two-way tables: largest", "",
             *[f"{fmt(S_[n]['pair_tvd_max'])} ({S_[n]['pair_worst']})" for n in names]],
            ["Three-way tables: mean TVD across 9 tables", "",
             *[fmt(S_[n]["triple_tvd_mean"]) for n in names]],
            ["Nonzero patterns: TVD", "", *[fmt(S_[n]["pattern_tvd"]) for n in names]],
            ["Profiles: mean absolute gap in share nonzero (points)", "",
             *[fmt(100 * S_[n]["profile_share_nonzero_mean_abs_gap"], 2) for n in names]],
            ["Top 1% share of AGI (positive part)", pct(S_["test"]["top1_agi_share"]),
             *[pct(S_[n]["top1_agi_share"]) for n in names]],
            ["Top 0.1% share of AGI (positive part)", pct(S_["test"]["top01_agi_share"]),
             *[pct(S_[n]["top01_agi_share"]) for n in names]],
            ["Classifier AUC (0.5 = indistinguishable)", "",
             *[fmt(S_[n]["classifier_auc"]) for n in names]],
            ["AGI consistency: share within 0.2% of approximate AGI",
             pct(S_["test"]["agi_gap_share_within_0.2pct"]),
             *[pct(S_[n]["agi_gap_share_within_0.2pct"]) for n in names]],
            ["AGI consistency: share within 2% of approximate AGI",
             pct(S_["test"]["agi_gap_share_within_2pct"]),
             *[pct(S_[n]["agi_gap_share_within_2pct"]) for n in names]],
            ["AGI consistency: TVD of gap distribution", "",
             *[fmt(S_[n]["agi_gap_tvd"]) for n in names]],
            ["Copying: median nearest-record distance, relative to test", "1.00",
             *[("(= test)" if n == "floor" else fmt(S_[n]["copy_median_distance_ratio_to_test"], 2))
               for n in names]],
            ["Copying: share at or below the test records' 10th-percentile distance",
             pct(S_["test"]["copy_share_within_test_p10"]),
             *[("(= test)" if n == "floor" else pct(S_[n]["copy_share_within_test_p10"]))
               for n in names]],
            ["Copying: share identical to a training record (values)",
             pct(S_["test"]["copy_share_identical_values"], 2),
             *[("(= test)" if n == "floor" else pct(S_[n]["copy_share_identical_values"], 2))
               for n in names]],
            ["Copying: share identical to a training record (tokens)",
             pct(S_["test"]["copy_share_identical_tokens"], 2),
             *[("(= test)" if n == "floor" else pct(S_[n]["copy_share_identical_tokens"], 2))
               for n in names]],
        ]))
    headline = "\n".join(h)

    d = []
    # single variables: amounts
    rows = []
    sv = single.set_index(["variable", "statistic"])
    for v in AMOUNTS:
        tz = 1 - sv.loc[(v, "share_zero"), "test"]
        tm = sv.loc[(v, "mean"), "test"]
        row = [f"{v} {LABELS[v]}", pct(tz), f"{tm:,.0f}"]
        for n in names:
            row.append(f"{100 * ((1 - sv.loc[(v, 'share_zero'), n]) - tz):+.2f}")
            row.append(fmt(sv.loc[(v, "mean"), n] / tm, 2) if tm != 0 else "n/a")
            row.append(fmt(mt.loc[v, n]))
        rows.append(row)
    hdr = ["Variable", "Test: share nonzero", "Test: mean ($)"]
    for n in names:
        hdr += [f"{n}: nonzero gap (pts)", f"{n}: mean / test", f"{n}: TVD"]
    d.append("#### Single variables (amounts)\n\n" + mdtable(hdr, rows))
    rows = [[f"{v} {LABELS[v]}", *[fmt(mt.loc[v, n]) for n in names]] for v in [WEIGHT] + CATEGORICAL]
    d.append("#### Single variables (weight and categorical variables): TVD\n\n"
             + mdtable(["Variable", *names], rows))

    pq = pd.DataFrame({n: pairs[n].quantile([0.1, 0.25, 0.5, 0.75, 0.9, 1.0]).to_numpy()
                       for n in names}, index=["10th pct", "25th pct", "median", "75th pct",
                                               "90th pct", "largest"])
    d.append("#### Two-way tables: TVD across the 780 pairs\n\n" + mdtable(
        ["Across pairs", *names], [[i, *[fmt(pq.loc[i, n]) for n in names]] for i in pq.index]))
    for n in names:
        if n == "floor":
            continue
        w = pairs.sort_values(f"{n}_minus_floor", ascending=False).head(10)
        d.append(f"#### Two-way tables: the 10 pairs furthest above the floor ({n})\n\n" + mdtable(
            ["Pair", f"{n} TVD", "floor TVD", "difference"],
            [[f"{a} {LABELS[a]} x {b} {LABELS[b]}", fmt(x), fmt(f), fmt(x - f)]
             for a, b, x, f in zip(w.var_a, w.var_b, w[n], w.floor)]))
    d.append("#### Three-way tables: TVD\n\n" + mdtable(
        ["Table", *names], [[i, *[fmt(tri.loc[i, n]) for n in names]] for i in tri.index]))

    pv = summary["pattern_variables"]
    rows = [[r["nonzero_variables"], pct(r["test"], 2), *[pct(r[n], 2) for n in names]]
            for _, r in pat.head(12).iterrows()]
    d.append("#### Nonzero patterns: the 12 most common in the test records\n\n"
             f"Variables used: {', '.join(pv)}.\n\n" + mdtable(["Nonzero incomes", "test", *names], rows)
             + "\n\n" + mdtable(["", *names], [
                 ["TVD across all patterns", *[fmt(summary[n]["pattern_tvd"]) for n in names]],
                 ["Number of distinct patterns (test: %d)" % ref["patterns"]["n_patterns"],
                  *[str(summary[n]["pattern_count"]) for n in names]],
                 ["Weight in patterns that never occur in test",
                  *[pct(summary[n]["pattern_share_absent_from_test"], 2) for n in names]]]))

    tv = top.set_index(["variable", "group"])
    big = ["E00100", "E00200", "E00300", "E00600", "E00900", "P22250", "P23250", "E01400",
           "E01700", "E02400", "E25850", "E26270"]
    rows = []
    for v in big:
        row = [f"{v} {LABELS[v]}"]
        for g in ["top1", "top0.1"]:
            row += [pct(tv.loc[(v, g), s]) for s in ["test", *names]]
        rows.append(row)
    d.append("#### The top: share of each income's positive amounts held above the 99th and "
             "99.9th AGI percentiles (each file ranked on its own AGI)\n\n" + mdtable(
                 ["Variable", *[f"top 1%: {s}" for s in ["test", *names]],
                  *[f"top 0.1%: {s}" for s in ["test", *names]]], rows))

    rows = []
    for _, r in cls.iterrows():
        top5 = imp[imp.file == r.file].head(5)
        rows.append([r.file, fmt(r.auc), pct(r.accuracy), f"{r.n_per_class:,}",
                     ", ".join(f"{v} ({x:.3f})" for v, x in zip(top5.variable, top5.auc_drop))])
    d.append("#### Classifier test\n\n" + mdtable(
        ["Candidate", "AUC", "Accuracy", "Records per class", "Top 5 variables (fall in AUC)"], rows))

    rows = [[g, pct(t), *[pct(gap.loc[i, n]) for n in names]]
            for i, (g, t) in enumerate(zip(gap.gap_bin, gap.test))]
    rows.append(["TVD vs test", "", *[fmt(summary[n]["agi_gap_tvd"]) for n in names]])
    d.append("#### AGI consistency: reported AGI minus approximate AGI, relative to "
             "max(|AGI|, $1,000)\n\n" + mdtable(["Gap", "test", *names], rows))

    rows = [[f"P{q * 100:g}", fmt(cp.loc[i, "test"])] + [fmt(cp.loc[i, n]) if n in cp else "(= test)"
                                                        for n in names]
            for i, q in enumerate(cp["quantile"])]
    d.append("#### Copying: distance to the nearest training record (rank scale)\n\n"
             + mdtable(["Quantile", "test", *names], rows))
    return {"headline": headline, "detail": "\n\n".join(d)}


def update_report(path, md, label):
    path = Path(path)
    text = path.read_text()
    for block in ["headline", "detail"]:
        b, e = f"<!-- BEGIN GENERATED {block.upper()} -->", f"<!-- END GENERATED {block.upper()} -->"
        if b not in text or e not in text:
            log(f"report {path} has no {block} markers; not updated")
            continue
        pre, rest = text.split(b, 1)
        _, post = rest.split(e, 1)
        stamp = f"<!-- generated by evaluate.py, label '{label}', {time.strftime('%Y-%m-%d %H:%M')} -->"
        text = pre + b + "\n" + stamp + "\n\n" + md[block] + "\n\n" + e + post
    path.write_text(text)
    log(f"updated {path}")


# --------------------------------------------------------------------------
# Temporary development split from the raw PUF (before part A's files exist)
# --------------------------------------------------------------------------

def make_dev_split(outdir, seed):
    raw = Path("/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical")
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    puf_cols = ["RECID", "S006"] + [c for c in CATEGORICAL if c not in DEMOGRAPHIC] + AMOUNTS
    puf = pd.read_csv(raw / "puf_2015.csv", usecols=puf_cols)
    dem = pd.read_csv(raw / "demographics_2015.csv", usecols=["RECID"] + DEMOGRAPHIC)
    n0 = len(puf)
    agg = puf.RECID >= 999996
    log(f"raw PUF {n0:,} records; {int(agg.sum())} aggregate top records set aside")
    puf = puf[~agg]
    df = puf.merge(dem, on="RECID", how="left", validate="one_to_one")
    for c in DEMOGRAPHIC:
        n_m = int(df[c].isna().sum())
        log(f"{c}: {n_m:,} records absent from the demographic file -> -1")
        df[c] = df[c].fillna(-1).astype(int)
    df["weight"] = df.S006 / 100.0
    df = df[["RECID", "weight"] + CATEGORICAL + AMOUNTS]
    rng = np.random.default_rng(seed)
    wband = pd.qcut(np.log(df.weight), 5, labels=False, duplicates="drop")
    agi = df.E00100.to_numpy()
    aband = np.where(agi <= 0, 0, 1 + pd.qcut(pd.Series(agi).rank(method="first"), 10,
                                              labels=False).to_numpy())
    strata = wband.to_numpy() * 100 + aband
    u = rng.random(len(df))
    part = np.empty(len(df), dtype=object)
    for s in np.unique(strata):
        idx = np.where(strata == s)[0]
        idx = idx[np.argsort(u[idx])]
        n = len(idx)
        n_tr, n_va = int(round(0.8 * n)), int(round(0.1 * n))
        part[idx[:n_tr]] = "train"
        part[idx[n_tr:n_tr + n_va]] = "val"
        part[idx[n_tr + n_va:]] = "test"
    for p in ["train", "val", "test"]:
        d = df[part == p].reset_index(drop=True)
        d.to_parquet(outdir / f"values_{p}.parquet", index=False)
        log(f"dev {p}: {len(d):,} records, total weight {d.weight.sum():,.0f}")
    # A copy-of-train file, to check the copying measure flags copies.
    tr = df[part == "train"].drop(columns="RECID")
    tr.sample(20000, random_state=seed).to_parquet(outdir / "dev_copy_of_train.parquet", index=False)
    # Formula check on the full PUF (aggregate statistics only).
    full = pd.read_csv(raw / "puf_2015.csv", usecols=lambda c: c not in ("RECID",))
    full = full[~agg.to_numpy()].reset_index(drop=True)
    w = full.S006.to_numpy() / 100
    w = w / w.sum()
    chk = pd.DataFrame(index=full.index)
    for c in CATEGORICAL:
        chk[c] = full[c] if c in full else 0
    for c in AMOUNTS:
        chk[c] = full[c]
    gap = full.E00100 - approx_agi(chk)
    rel = gap / np.maximum(full.E00100.abs(), 1000)
    rows = [("share within 0.2%", float(w[np.abs(rel) <= 0.002].sum())),
            ("share within 2%", float(w[np.abs(rel) <= 0.02].sum()))]
    omitted = (full.E00700 + full.E00800 + full.E01200 + full.E27200
               - full.E03220 - full.E03230 - full.E03240 - full.E03290 - full.E03300
               - full.E03400 - full.E03500)
    rows.append(("share of records with omitted components nonzero", float(w[omitted != 0].sum())))
    rel2 = (gap - omitted) / np.maximum(full.E00100.abs(), 1000)
    rows.append(("share within 0.2% after adding omitted components", float(w[np.abs(rel2) <= 0.002].sum())))
    se = full.E02000 - (full.E25850 - full.E25860 + full.E26270 + full.E26390 - full.E26400 + full.E27200)
    rows.append(("share with E02000 = Schedule E components (within $1)", float(w[np.abs(se) <= 1].sum())))
    cg = full.E01000 - np.maximum(full.P22250 + full.P23250, np.where(full.MARS == 3, -1500, -3000))
    rows.append(("share with E01000 = limited P22250+P23250 (within $1)", float(w[np.abs(cg) <= 1].sum())))
    rows.append(("min of E25860 (sign check)", float(full.E25860.min())))
    rows.append(("min of E26400 (sign check)", float(full.E26400.min())))
    pd.DataFrame(rows, columns=["check", "value"]).to_csv(outdir / "formula_check.csv", index=False)
    log("formula check:\n" + pd.DataFrame(rows, columns=["check", "value"]).to_string(index=False))


# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--synthetic", help="candidate file (values format, parquet)")
    ap.add_argument("--label", default=None)
    ap.add_argument("--data-dir", default=str(S / "data"))
    ap.add_argument("--eval-dir", default=str(S / "eval"))
    ap.add_argument("--seed", type=int, default=SEED)
    ap.add_argument("--update-report", default=None,
                    help="EVAL_REPORT.md to rewrite between the generated-block markers")
    ap.add_argument("--make-dev-split", default=None, metavar="OUTDIR")
    ap.add_argument("--reference", default=None,
                    help="file to compare against in place of values_test.parquet "
                         "(for example values_train.parquet, or another generated file "
                         "to measure seed-to-seed noise). Tables still call it 'test'; "
                         "the copying measures are only meaningful for the default.")
    args = ap.parse_args()

    if args.make_dev_split:
        make_dev_split(args.make_dev_split, args.seed)
        return

    t_start = time.time()
    data = Path(args.data_dir)
    label = args.label or (Path(args.synthetic).stem if args.synthetic else "baseline")
    outdir = Path(args.eval_dir) / label
    log(f"label {label}; data {data}; output {outdir}; seed {args.seed}")
    train = load_values(data / "values_train.parquet", "train")
    ref_path = Path(args.reference) if args.reference else data / "values_test.parquet"
    test = load_values(ref_path, "test")
    log(f"reference file: {ref_path}")
    meta = None
    if (data / "meta.json").exists():
        meta = json.loads((data / "meta.json").read_text())
        if "variables" in meta:
            mk = {v["name"]: v["kind"] for v in meta["variables"]}
            bad = [v for v in CATEGORICAL + AMOUNTS if v not in mk]
            if bad:
                log(f"note: meta.json lacks {bad}")
        else:
            meta = None

    ctx = build_context(test, train, args.seed, meta)
    ref = test_reference(ctx)

    cache_dir = Path(args.eval_dir) / "_cache"
    cache_dir.mkdir(parents=True, exist_ok=True)
    key = file_key([data / "values_train.parquet", ref_path],
                   extra=f"seed={args.seed}")
    results = []
    baselines = ["floor", "independent"]
    if meta is not None:
        baselines.append("floor_binned")
    for name in baselines:
        cpath = cache_dir / f"{name}_{key}.pkl"
        if cpath.exists():
            with open(cpath, "rb") as f:
                r = pickle.load(f)
            log(f"{name}: loaded from cache {cpath.name}")
        else:
            if name == "floor":
                cand = train
            elif name == "independent":
                cand = make_independent(train, args.seed + 1)
            else:
                try:
                    cand = binned_round_trip(train, meta, args.seed + 2)
                except Exception as e:  # noqa: BLE001
                    log(f"floor_binned unavailable: {e!r}")
                    continue
            log(f"{name}: evaluating {len(cand):,} records")
            r = evaluate_candidate(name, cand, ctx)
            with open(cpath, "wb") as f:
                pickle.dump(r, f)
        r["pattern_vars"] = ctx["pattern_vars"]
        results.append(r)

    if args.synthetic:
        if label in ("floor", "independent", "floor_binned", "test"):
            raise SystemExit("label must differ from floor / independent / test")
        cand = load_values(args.synthetic, label)
        r = evaluate_candidate(label, cand, ctx)
        r["pattern_vars"] = ctx["pattern_vars"]
        results.append(r)

    summary, md = assemble(results, ref, float(train[WEIGHT].sum()), outdir, label)
    summary["run_seconds"] = time.time() - t_start
    summary["data_dir"] = str(data)
    summary["synthetic"] = args.synthetic
    summary["reference"] = str(ref_path)
    with open(outdir / "summary.json", "w") as f:
        json.dump(summary, f, indent=2, default=float)
    if args.update_report:
        update_report(args.update_report, md, label)
    log(f"finished in {time.time() - t_start:.0f}s; outputs in {outdir}")


if __name__ == "__main__":
    main()
