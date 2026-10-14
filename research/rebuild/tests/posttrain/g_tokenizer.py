"""Rank tokens for test 3 dataset `full` (piece G). A copy of test 2's
../scf_joint/a_tokenizer.py with one change: `_rank_basis` falls back to the
PUF's quantile function for a PUF variable the SCF does not measure, so SCF
records' completed PUF variables can be decoded to dollars. Everything else is
unchanged. load_meta("full") reads $T2/data/full/meta.json.

Original docstring (test 2, piece A):

Every amount is turned into a token from its weighted rank within its own
source and year, and back into dollars through that source-year's weighted
quantile function. See SPEC.md, "Units: ranks", and DATA_REPORT.md.

    meta = load_meta(dataset)                      # $S/data/<dataset>/meta.json
    q = load_quantiles(dataset)                    # $S/data/<dataset>/quantiles.parquet
    tokens = encode(df, meta, q, rng)              # DataFrame -> int array [records, variables]
    dollars = decode(tokens, meta, records, q, rng, year=None)

`df` (for encode) has columns `source`, `year` and one column per variable
name in meta (categoricals as their values, amounts in dollars, `scf_weight`
as the population weight). NaN means unobserved and gives token -1.

`decode` returns a DataFrame with one column per non-context variable:
categoricals as values, amounts and `scf_weight` in dollars (weight units).
Token -1 decodes to NaN. `records` gives each row's `source` and `year`.
`year`, if given, overrides the records' year for variables the record's
source does not observe (for example PUF wealth decoded at 2015).

Token layout of an amount variable (sorted by value):
    negative bins (if the variable has negatives anywhere): weighted rank
        among negative values, most negative first, edges 0, 0.001, 0.01,
        0.05, 0.1, 0.2, ..., 0.9, 1 (finer at the large-loss end);
    one token for exact zero;
    positive bins: weighted rank among positive values on the shared grid
        (0.02 steps to 0.90, 0.01 to 0.99, 0.001 to 0.999, then 0.9999, 0.99999, 1).
`scf_weight` has 64 bins of the unweighted rank of the weight within the SCF
source-year (so each bin holds about 1/64 of the SCF records).

Ranks. A value's weighted rank is drawn uniformly between the weight share
strictly below it and the share at or below it (so ties are spread over the
bins they span; a value not among the quantile function's points gets the
share below it). This uses the rng; pass a seeded generator.

Quantile function (quantiles.parquet). One row per distinct value per
(source, year, variable, side), side 'pos', 'neg' or 'all' (the weight):
`value`, `share` (weight share of the value within its side), `cum`
(cumulative share, inclusive). Decoding draws a rank u uniformly inside the
token's band and reads the value off a piecewise log-linear interpolation of
|value| through knots at each value's mid-rank; values holding at least
MASS_POINT of their side get two knots (flat over the middle 80% of their
band) so that common round amounts come back exactly. Below the first knot
and above the last the end values are used.

Variables a source does not observe (wealth, `scf_weight` for the PUF and
for the pretend PUF half) are decoded with the SCF quantile functions: the
SCF wave for that year if there is one, otherwise the two SCF waves around
the year interpolated in log dollars (2015 between 2013 and 2016: weight 1/3
on 2013 and 2/3 on 2016), outside the SCF years the nearest wave.
"""

import json
import os

import numpy as np
import pandas as pd

S = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
DATA = os.path.join(S, "data")

POS_EDGES = np.round(np.concatenate([
    np.arange(0, 46) * 0.02,                 # 0 .. 0.90
    0.90 + np.arange(1, 10) * 0.01,          # 0.91 .. 0.99
    0.99 + np.arange(1, 10) * 0.001,         # 0.991 .. 0.999
    [0.9999, 0.99999, 1.0],
]), 6)
NEG_EDGES = np.round(np.concatenate([[0, 0.001, 0.01, 0.05], np.arange(1, 11) * 0.1]), 6)
WEIGHT_EDGES = np.round(np.arange(0, 65) / 64, 9)
MASS_POINT = 0.002
SCF_SOURCE = "SCF"


def load_meta(dataset):
    with open(os.path.join(DATA, dataset, "meta.json")) as f:
        return json.load(f)


def load_quantiles(dataset):
    return pd.read_parquet(os.path.join(DATA, dataset, "quantiles.parquet"))


# ---------------------------------------------------------------------------
# Quantile functions
# ---------------------------------------------------------------------------

def quantile_table(x, w, side):
    """Distinct values of x (already restricted to one side) with weight
    shares and cumulative shares. For side 'neg' values are sorted ascending
    (most negative first); for 'pos' and 'all' ascending too."""
    x = np.asarray(x, dtype=np.float64)
    w = np.asarray(w, dtype=np.float64)
    if len(x) == 0:
        return pd.DataFrame({"value": [], "share": [], "cum": [], "n": []})
    df = pd.DataFrame({"value": x, "w": w}).groupby("value", sort=True).agg(
        w=("w", "sum"), n=("w", "size")).reset_index()
    tot = df.w.sum()
    df["share"] = df.w / tot
    df["cum"] = np.cumsum(df.w) / tot
    df.loc[df.index[-1], "cum"] = 1.0
    return df[["value", "share", "cum", "n"]]


class _QF:
    """Quantile function of one (source, year, variable, side)."""

    def __init__(self, tab):
        self.v = tab["value"].to_numpy(np.float64)
        self.share = tab["share"].to_numpy(np.float64)
        self.cum = tab["cum"].to_numpy(np.float64)
        self.lo = self.cum - self.share
        # knots for decoding
        kx, ky = [], []
        for v, s, lo, hi in zip(self.v, self.share, self.lo, self.cum):
            if s >= MASS_POINT:
                kx += [lo + 0.1 * s, hi - 0.1 * s]
                ky += [v, v]
            else:
                kx.append(0.5 * (lo + hi))
                ky.append(v)
        self.kx = np.asarray(kx)
        mag = np.abs(np.asarray(ky))
        self.sign = -1.0 if len(ky) and ky[0] < 0 else 1.0
        self.klog = np.log(np.maximum(mag, 1e-9))

    def rank(self, x, rng):
        i = np.searchsorted(self.v, x, side="left")
        n = len(self.v)
        ic = np.minimum(i, n - 1)
        exact = (i < n) & (self.v[ic] == x)
        below = np.where(i > 0, self.cum[np.maximum(i - 1, 0)], 0.0)
        above = np.where(exact, self.cum[ic], below)
        return below + rng.random(len(x)) * (above - below)

    def log_mag(self, u):
        return np.interp(u, self.kx, self.klog)


def _bin(r, edges):
    return np.clip(np.searchsorted(edges, r, side="right") - 1, 0, len(edges) - 2)


# ---------------------------------------------------------------------------
# Encoding
# ---------------------------------------------------------------------------

def _qf_cache(quantiles):
    cache = {}
    for key, g in quantiles.groupby(["source", "year", "variable", "side"], sort=False):
        cache[(key[0], int(key[1]), key[2], key[3])] = _QF(g.sort_values("value"))
    return cache


def _rank_basis(var, source, year, cache):
    """(source, year) whose quantile function ranks/decodes `var` for a
    record of (source, year): its own if it exists, else the SCF's, else
    (piece G change, test 3) the PUF's. The last case covers a PUF variable
    that the SCF does not measure, decoded for an SCF record: it is read off
    the PUF 2015 function (the only PUF year)."""
    side = "all" if var["kind"] == "weight" else "pos"
    if (source, year, var["name"], side) in cache or (source, year, var["name"], "neg") in cache:
        return [(source, year, 1.0)]
    others = [SCF_SOURCE] + sorted({k[0] for k in cache if k[2] == var["name"] and k[0] != SCF_SOURCE})
    for other in others:
        years = sorted({k[1] for k in cache if k[0] == other and k[2] == var["name"]})
        if not years:
            continue
        if year in years:
            return [(other, year, 1.0)]
        if year < years[0]:
            return [(other, years[0], 1.0)]
        if year > years[-1]:
            return [(other, years[-1], 1.0)]
        hi = min(y for y in years if y > year)
        lo = max(y for y in years if y < year)
        a = (year - lo) / (hi - lo)
        return [(other, lo, 1.0 - a), (other, hi, a)]
    raise ValueError(f"no quantile function for {var['name']}")


def encode(df, meta, quantiles, rng, cache=None):
    """Encode a DataFrame to tokens. NaN -> -1. Amounts of a record whose
    source has no quantile function for the variable are ranked with the
    SCF function of the same year (used for the truth tokens of hidden
    wealth in experiment A); there must then be an SCF wave for that year."""
    cache = cache or _qf_cache(quantiles)
    n = len(df)
    out = np.full((n, len(meta["variables"])), -1, dtype=np.int64)
    src = df["source"].to_numpy()
    yr = df["year"].to_numpy().astype(int)
    # One random stream per variable, so the tie-breaking draws of a variable
    # do not depend on how many draws earlier variables used (an altered
    # income in one experiment A variant leaves the others' tokens unchanged).
    child = rng.integers(0, 2**62, size=len(meta["variables"]))
    for k, var in enumerate(meta["variables"]):
        name = var["name"]
        vrng = np.random.default_rng(int(child[k]))
        if var["kind"] == "categorical":
            vals = [t["value"] for t in var["tokens"]]
            col = df[name].to_numpy()
            for t, v in enumerate(vals):
                out[np.asarray(col == v, dtype=bool), k] = t
            na = pd.isna(df[name]).to_numpy()
            bad = (out[:, k] < 0) & ~na
            if bad.any():
                raise ValueError(f"{name}: values not in vocabulary: {pd.unique(df[name][bad])[:10]}")
            continue
        x = df[name].to_numpy(np.float64)
        for (s, y) in sorted(set(zip(src, yr))):
            m = (src == s) & (yr == y) & ~np.isnan(x)
            if not m.any():
                continue
            basis = _rank_basis(var, s, int(y), cache)
            if len(basis) != 1:
                raise ValueError(f"{name}: no single quantile function for {s} {y}")
            bs, by, _ = basis[0]
            xm = x[m]
            tok = np.full(len(xm), -1, dtype=np.int64)
            if var["kind"] == "weight":
                qf = cache[(bs, by, name, "all")]
                tok = _bin(qf.rank(xm, vrng), WEIGHT_EDGES)
            else:
                b = var["binning"]
                tok[xm == 0] = b["zero_token"]
                pos = xm > 0
                if pos.any():
                    qf = cache[(bs, by, name, "pos")]
                    tok[pos] = b["pos_token0"] + _bin(qf.rank(xm[pos], vrng), POS_EDGES)
                neg = xm < 0
                if neg.any():
                    if b["neg_token0"] is None:
                        raise ValueError(f"{name}: negative values but no negative tokens")
                    key = (bs, by, name, "neg")
                    if key not in cache:
                        raise ValueError(f"{name}: negative values in {s} {y} but no negative quantile function in {bs} {by}")
                    tok[neg] = b["neg_token0"] + _bin(cache[key].rank(xm[neg], vrng), NEG_EDGES)
            out[np.flatnonzero(m), k] = tok
    dt = np.int16 if max(v["vocab_size"] for v in meta["variables"]) < 32000 else np.int32
    return out.astype(dt)


# ---------------------------------------------------------------------------
# Decoding
# ---------------------------------------------------------------------------

def decode(tokens, meta, records, quantiles, rng, year=None, cache=None):
    cache = cache or _qf_cache(quantiles)
    tokens = np.asarray(tokens).astype(np.int64)
    src = records["source"].to_numpy()
    yr = records["year"].to_numpy().astype(int)
    data = {}
    for k, var in enumerate(meta["variables"]):
        name = var["name"]
        if var["group"] == "context":
            continue
        tok = tokens[:, k]
        if var["kind"] == "categorical":
            vals = np.array([t["value"] for t in var["tokens"]], dtype=np.float64)
            data[name] = np.where(tok >= 0, vals[np.maximum(tok, 0)], np.nan)
            continue
        res = np.full(len(tok), np.nan)
        lo = np.array([t["lo"] for t in var["tokens"]])
        hi = np.array([t["hi"] for t in var["tokens"]])
        kind = np.array([t["kind"] for t in var["tokens"]])
        for (s, y) in sorted(set(zip(src, yr))):
            m = (src == s) & (yr == y) & (tok >= 0)
            if not m.any():
                continue
            own = _rank_basis(var, s, int(y), cache)
            if own[0][0] == s:
                basis = own
            else:
                basis = _rank_basis(var, s, int(year) if year is not None else int(y), cache)
            idx = np.flatnonzero(m)
            t = tok[idx]
            u = lo[t] + rng.random(len(t)) * (hi[t] - lo[t])
            val = np.zeros(len(t))
            for side, sel in (("all", kind[t] == "weight"), ("pos", kind[t] == "pos"), ("neg", kind[t] == "neg")):
                if not sel.any():
                    continue
                lm = np.zeros(sel.sum())
                for bs, by, a in basis:
                    lm += a * cache[(bs, by, name, side)].log_mag(u[sel])
                val[sel] = (-1.0 if side == "neg" else 1.0) * np.exp(lm)
            # 'zero' tokens stay 0
            res[idx] = val
        data[name] = res
    return pd.DataFrame(data)
