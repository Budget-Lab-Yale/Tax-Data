"""Turn PUF records into integer tokens and back.

Used by all three parts of the PUF generator test (see SPEC.md):

    meta = load_meta()                       # $S/data/meta.json
    tokens = encode(df, meta)                # DataFrame -> int array [records, variables]
    df2 = decode(tokens, meta, rng)          # int array -> DataFrame

`df` is in the `values_*.parquet` format: a `weight` column (S006 / 100) and
one column per variable under its PUF name; categoricals hold their original
codes, with -1 for "absent from the demographic file". A `RECID` column, if
present, is ignored by `encode`. `decode` returns `weight` followed by the
variables in `meta["variables"]` order, without `RECID`.

Token conventions (all recorded in meta.json, per variable):
- tokens are integers 0 .. vocab_size-1, local to each variable;
- for amounts and the weight, tokens are sorted by value, so token index is
  the position in the variable's order (negative bins, zero, positive bins,
  with exact-value "point" tokens placed at their value);
- each amount token carries a `decode` rule: `exact` (zero and point
  tokens); `log_uniform` (uniform in log|value| between the bin's bounds);
  `uniform` (uniform in dollars, for narrow bins); `pareto` (only the
  outermost bin on each side: a truncated Pareto between the bounds with the
  token's fitted `alpha`). Bin draws are rounded down in magnitude to whole
  dollars.

Bin membership. For positive values, bin j covers [lo, hi), except the top
positive bin, which also includes hi. Negative bins mirror this on |value|:
a negative bin with bounds lo < hi < 0 covers (lo, hi], except the most
negative bin, which also includes lo. Values outside the training range are
put in the end bin on their side.
"""

import json
import os

import numpy as np
import pandas as pd

DATA_DIR = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data"

# A bin is decoded on the dollar scale, not the log scale, when its upper
# bound is less than this multiple of its lower bound.
NARROW_RATIO = 1.1


def load_meta(path=None):
    """Read meta.json (default: $S/data/meta.json)."""
    path = path or os.path.join(DATA_DIR, "meta.json")
    with open(path) as f:
        return json.load(f)


def _token_dtype(meta):
    biggest = max(v["vocab_size"] for v in meta["variables"])
    return np.int16 if biggest < 32767 else np.int32


# ---------------------------------------------------------------------------
# Encoding
# ---------------------------------------------------------------------------

def _encode_categorical(x, var):
    values = np.array([t["value"] for t in var["tokens"]], dtype=np.int64)
    if pd.isna(x).any():
        raise ValueError(f"{var['name']}: {int(pd.isna(x).sum())} NA values; "
                         "absent demographics must be coded -1 before encoding")
    x = np.asarray(x, dtype=np.int64)
    order = np.argsort(values)
    pos = np.searchsorted(values[order], x)
    pos = np.clip(pos, 0, len(values) - 1)
    ok = values[order][pos] == x
    if not ok.all():
        bad = np.unique(x[~ok])
        raise ValueError(f"{var['name']}: codes not in vocabulary: {bad.tolist()}")
    return order[pos]


def _encode_weight(w, var):
    # Weight tokens are exact S006 values (S006 is an integer; weight = S006/100).
    s006 = np.rint(np.asarray(w, dtype=np.float64) * 100).astype(np.int64)
    values = np.array([t["S006"] for t in var["tokens"]], dtype=np.int64)
    pos = np.clip(np.searchsorted(values, s006), 0, len(values) - 1)
    ok = values[pos] == s006
    if not ok.all():
        bad = np.unique(s006[~ok])
        raise ValueError(f"weight: S006 values not in vocabulary: {bad[:10].tolist()}")
    return pos


def _bin_index(mag, edges):
    """Bin index of each magnitude in contiguous bins defined by `edges`
    (bin j = [edges[j], edges[j+1]); the last bin also includes its top).
    Magnitudes outside the range go to the end bins."""
    edges = np.asarray(edges, dtype=np.float64)
    j = np.searchsorted(edges, mag, side="right") - 1
    return np.clip(j, 0, len(edges) - 2)


def _encode_amount(x, var):
    x = np.asarray(x, dtype=np.float64)
    if np.isnan(x).any():
        raise ValueError(f"{var['name']}: {int(np.isnan(x).sum())} NA values")
    b = var["binning"]
    out = np.full(len(x), -1, dtype=np.int64)

    out[x == 0] = b["zero_token"]

    pts = np.array(b["point_values"], dtype=np.float64)
    pt_tok = np.array(b["point_tokens"], dtype=np.int64)
    if len(pts):
        order = np.argsort(pts)
        pos = np.clip(np.searchsorted(pts[order], x), 0, len(pts) - 1)
        hit = (pts[order][pos] == x) & (x != 0)
        out[hit] = pt_tok[order][pos[hit]]

    rest = out < 0
    pos_mask = rest & (x > 0)
    neg_mask = rest & (x < 0)
    if pos_mask.any():
        if not b["pos_edges"]:
            raise ValueError(f"{var['name']}: positive value but no positive bins")
        j = _bin_index(x[pos_mask], b["pos_edges"])
        out[pos_mask] = np.asarray(b["pos_bin_tokens"])[j]
    if neg_mask.any():
        if not b["neg_edges"]:
            raise ValueError(f"{var['name']}: negative value but no negative bins")
        j = _bin_index(-x[neg_mask], b["neg_edges"])
        out[neg_mask] = np.asarray(b["neg_bin_tokens"])[j]
    assert (out >= 0).all()
    return out


def encode(df, meta):
    """Encode a values-format DataFrame to an int array [records, variables]."""
    cols = []
    for var in meta["variables"]:
        if var["kind"] == "weight":
            cols.append(_encode_weight(df["weight"].to_numpy(), var))
        elif var["kind"] == "categorical":
            cols.append(_encode_categorical(df[var["name"]].to_numpy(), var))
        else:
            cols.append(_encode_amount(df[var["name"]].to_numpy(), var))
    return np.stack(cols, axis=1).astype(_token_dtype(meta))


# ---------------------------------------------------------------------------
# Decoding
# ---------------------------------------------------------------------------

def _token_decode_rule(t):
    """Decode rule of one amount token ('exact', 'uniform', 'log_uniform', 'pareto')."""
    if "decode" in t:
        return t["decode"]
    if t["point"]:
        return "exact"
    m_lo, m_hi = sorted((abs(t["lo"]), abs(t["hi"])))
    return "uniform" if m_hi < NARROW_RATIO * m_lo else "log_uniform"


def _amount_tables(var):
    """Per-token arrays for decoding one amount variable."""
    toks = var["tokens"]
    lo = np.array([t["lo"] for t in toks], dtype=np.float64)
    hi = np.array([t["hi"] for t in toks], dtype=np.float64)
    neg = lo < 0
    return {
        "neg": neg,
        # Magnitude bounds: a negative bin (lo, hi] has magnitudes [|hi|, |lo|).
        "m_lo": np.where(neg, -hi, lo),
        "m_hi": np.where(neg, -lo, hi),
        "rule": np.array([_token_decode_rule(t) for t in toks]),
        "alpha": np.array([t.get("alpha", 0.0) for t in toks], dtype=np.float64),
    }


def _decode_amount(tok, var, rng):
    tb = _amount_tables(var)
    rule = tb["rule"][tok]
    m_lo, m_hi = tb["m_lo"][tok], tb["m_hi"][tok]
    alpha = tb["alpha"][tok]
    u = rng.random(len(tok))
    mag = m_lo.copy()                                  # 'exact' tokens: lo == hi

    r = rule == "uniform"
    mag[r] = m_lo[r] + u[r] * (m_hi[r] - m_lo[r])

    r = (rule == "log_uniform") | ((rule == "pareto") & (np.abs(alpha) < 1e-9))
    mag[r] = np.exp(np.log(m_lo[r]) + u[r] * (np.log(m_hi[r]) - np.log(m_lo[r])))

    r = (rule == "pareto") & (np.abs(alpha) >= 1e-9)
    if r.any():
        # Inverse of the truncated-Pareto distribution function on [lo, hi]:
        # F(x) = (lo^-a - x^-a) / (lo^-a - hi^-a). Computed relative to lo for stability.
        a, ratio = alpha[r], m_hi[r] / m_lo[r]
        mag[r] = m_lo[r] * (1.0 - u[r] * (1.0 - ratio ** (-a))) ** (-1.0 / a)

    inexact = rule != "exact"
    # Whole dollars, rounded down in magnitude; never below the bin floor.
    mag[inexact] = np.maximum(np.floor(mag[inexact]), m_lo[inexact])
    return np.where(tb["neg"][tok], -mag, mag)


def decode(tokens, meta, rng):
    """Decode an int array [records, variables] to a values-format DataFrame
    (columns `weight` then each variable by PUF name; no RECID)."""
    tokens = np.asarray(tokens)
    data = {}
    for k, var in enumerate(meta["variables"]):
        tok = tokens[:, k].astype(np.int64)
        if (tok < 0).any() or (tok >= var["vocab_size"]).any():
            raise ValueError(f"{var['name']}: token out of range")
        if var["kind"] == "weight":
            s006 = np.array([t["S006"] for t in var["tokens"]], dtype=np.int64)[tok]
            data["weight"] = s006 / 100.0
        elif var["kind"] == "categorical":
            vals = np.array([t["value"] for t in var["tokens"]], dtype=np.int64)
            data[var["name"]] = vals[tok]
        else:
            data[var["name"]] = _decode_amount(tok, var, rng)
    # The weight variable comes first in meta; keep `weight` first regardless.
    cols = ["weight"] + [v["name"] for v in meta["variables"] if v["kind"] != "weight"]
    return pd.DataFrame(data)[cols]
