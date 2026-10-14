"""Part A of the PUF generator test: data preparation (see SPEC.md).

Reads the 2015 PUF and its demographic file, sets aside the four aggregate
records, splits the rest 80/10/10 within strata of weight band x AGI band,
builds the token vocabulary for each variable from the training records,
writes tokens and values for each part, and runs the round-trip check
(decode(encode(train)) versus train).

Outputs, all under $S/data/:
    meta.json, tokens_{train,val,test}.npy, values_{train,val,test}.parquet,
    bins_summary.csv, roundtrip.csv, roundtrip_quantiles.csv, split_strata.csv,
    prep_summary.json
This script deletes any existing DONE marker when it starts. The marker is
created by hand (touch $S/data/DONE) after the outputs and the round-trip
results have been reviewed; other parts of the test wait on it.

Run through Slurm: sbatch prepare_data.sbatch
"""

import json
import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tokenizer  # noqa: E402

RAW = "/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/"
OUT = tokenizer.DATA_DIR

SPLIT_SEED = 20261008
ROUNDTRIP_SEED = 20261009

MIN_PER_BIN = 20        # at least about this many training records per bin
MAX_POS_BINS = 256
MAX_NEG_BINS = 64
MAX_POINTS_PER_SIDE = 64
POINT_MIN_SHARE = 0.01  # a point value must also hold >= 1% of the side's nonzero records
TAIL_GROWTH = 1.5       # top-tail bins hold 20, 30, 45, ... records (see make_edges)
MISSING_CODE = -1

# ---------------------------------------------------------------------------
# Variables, in generation order. The token arrays use this column order.
# ---------------------------------------------------------------------------

VARIABLES = [
    ("S006", "weight", "Record weight x 100 (record weight = S006 / 100)"),
    ("MARS", "categorical", "Filing status (1 single, 2 joint, 3 separate, 4 head of household)"),
    ("DSI", "categorical", "Dependent of another taxpayer (1 yes)"),
    ("XTOT", "categorical", "Total exemptions"),
    ("N24", "categorical", "Children eligible for the child tax credit"),
    ("EIC", "categorical", "Children counted for the earned income credit"),
    ("FDED", "categorical", "Form of deduction (1 itemized, 2 standard, 3 none)"),
    ("AGERANGE", "categorical", "Age range of primary filer (demographic file; -1 = not in file; codes depend on DSI)"),
    ("GENDER", "categorical", "Gender of primary filer (demographic file; -1 = not in file)"),
    ("EARNSPLIT", "categorical", "Earnings split on joint returns (demographic file; -1 = not in file)"),
    ("E00100", "amount", "Adjusted gross income"),
    ("E00200", "amount", "Wages and salaries"),
    ("E00900", "amount", "Schedule C business net income or loss"),
    ("E26270", "amount", "Partnership and S corporation net income or loss"),
    ("P23250", "amount", "Long-term capital gain or loss"),
    ("P22250", "amount", "Short-term capital gain or loss"),
    ("E00600", "amount", "Ordinary dividends"),
    ("E00650", "amount", "Qualified dividends"),
    ("E00300", "amount", "Taxable interest"),
    ("E01500", "amount", "Total pensions and annuities"),
    ("E01700", "amount", "Taxable pensions and annuities"),
    ("E01400", "amount", "Taxable IRA distributions"),
    ("E02400", "amount", "Gross Social Security benefits"),
    ("E25850", "amount", "Rent and royalty net income"),
    ("E25860", "amount", "Rent and royalty net loss (positive number)"),
    ("E02100", "amount", "Schedule F farm net income or loss"),
    ("E00400", "amount", "Tax-exempt interest"),
    ("E26390", "amount", "Estate and trust net income"),
    ("E26400", "amount", "Estate and trust net loss (positive number)"),
    ("E01100", "amount", "Capital gain distributions"),
    ("E02300", "amount", "Unemployment compensation"),
    ("E03150", "amount", "Deductible IRA contributions"),
    ("E03210", "amount", "Student loan interest deduction"),
    ("E03270", "amount", "Self-employed health insurance deduction"),
    ("E17500", "amount", "Medical and dental expenses (itemizers)"),
    ("E18400", "amount", "State and local income or sales taxes (itemizers)"),
    ("E18500", "amount", "Real estate taxes (itemizers)"),
    ("E19200", "amount", "Interest paid deduction (itemizers)"),
    ("E19800", "amount", "Cash charitable contributions (itemizers)"),
    ("E20100", "amount", "Non-cash charitable contributions (itemizers)"),
]
DEMOG = ["AGERANGE", "GENDER", "EARNSPLIT"]
CATEGORICALS = [n for n, k, _ in VARIABLES if k == "categorical"]
AMOUNTS = [n for n, k, _ in VARIABLES if k == "amount"]

# Split strata: weight band x AGI band.
WEIGHT_BANDS = [0, 2, 20, 200, 1000, np.inf]          # record weight S006/100
WEIGHT_LABELS = ["w<2", "2<=w<20", "20<=w<200", "200<=w<1000", "w>=1000"]
AGI_BANDS = [-np.inf, 0, 50_000, 200_000, 1_000_000, 10_000_000, np.inf]
AGI_LABELS = ["AGI<0", "0<=AGI<50k", "50k<=AGI<200k", "200k<=AGI<1m", "1m<=AGI<10m", "AGI>=10m"]


def log(*a):
    print(*a, flush=True)


# ---------------------------------------------------------------------------
# Reading
# ---------------------------------------------------------------------------

def read_puf():
    puf = pd.read_csv(RAW + "puf_2015.csv")
    demo = pd.read_csv(RAW + "demographics_2015.csv")
    n_raw = len(puf)
    assert puf.RECID.is_unique and demo.RECID.is_unique
    assert demo.RECID.isin(puf.RECID).all(), "demographic RECIDs missing from PUF"

    agg = puf.RECID >= 999996
    n_agg = int(agg.sum())
    puf = puf.loc[~agg].copy()

    cols = ["RECID", "S006"] + [c for c in CATEGORICALS if c not in DEMOG] + AMOUNTS
    na = {c: int(puf[c].isna().sum()) for c in cols}
    bad = {c: n for c, n in na.items() if n}
    if bad:
        raise ValueError(f"unexpected NAs in PUF columns used: {bad}")
    df = puf[cols].merge(demo[["RECID"] + DEMOG], on="RECID", how="left")
    in_demo = df["GENDER"].notna()
    for c in DEMOG:
        # Only absence from the demographic file may produce NA here.
        assert (df[c].notna() == in_demo).all(), c
        df[c] = df[c].fillna(MISSING_CODE).astype(np.int64)
    for c in CATEGORICALS:
        df[c] = df[c].astype(np.int64)
    for c in AMOUNTS:
        df[c] = df[c].astype(np.float64)
    assert (df.S006 > 0).all() and (df.S006 == df.S006.round()).all()
    df["S006"] = df["S006"].astype(np.int64)
    df.insert(1, "weight", df["S006"] / 100.0)

    info = {
        "raw_records": n_raw,
        "aggregate_records_set_aside": n_agg,
        "aggregate_records_rule": "RECID >= 999996 (the PUF's four aggregate records for extreme incomes)",
        "records_kept": len(df),
        "records_absent_from_demographic_file": int((~in_demo).sum()),
        "demographic_file_records": len(demo),
        "na_counts_in_used_puf_columns": na,
        "puf_columns_with_any_na": [c for c in puf.columns if puf[c].isna().any()],
    }
    return df, info


# ---------------------------------------------------------------------------
# Split
# ---------------------------------------------------------------------------

def split(df):
    wb = pd.cut(df.weight, WEIGHT_BANDS, right=False, labels=WEIGHT_LABELS)
    ab = pd.cut(df.E00100, AGI_BANDS, right=False, labels=AGI_LABELS)
    assert wb.notna().all() and ab.notna().all()
    stratum = wb.astype(str) + " | " + ab.astype(str)

    rng = np.random.default_rng(SPLIT_SEED)
    part = np.empty(len(df), dtype=object)
    rows = []
    for s in sorted(stratum.unique()):
        idx = np.flatnonzero((stratum == s).to_numpy())
        idx = idx[rng.permutation(len(idx))]
        n = len(idx)
        n_val = int(round(0.1 * n))
        n_test = int(round(0.1 * n))
        part[idx[:n_val]] = "val"
        part[idx[n_val:n_val + n_test]] = "test"
        part[idx[n_val + n_test:]] = "train"
        rows.append({"stratum": s, "records": n, "train": n - n_val - n_test,
                     "val": n_val, "test": n_test})
    strata = pd.DataFrame(rows)
    return part, strata


# ---------------------------------------------------------------------------
# Binning
# ---------------------------------------------------------------------------

def choose_points(mag, k_max):
    """Values holding more than one bin's worth of records, and at least
    POINT_MIN_SHARE of the nonzero records on their side, become point tokens.
    Repeated until no remaining value qualifies (removing a spike changes the
    number of bins and so the threshold). The share floor matters for rare
    variables, where one bin's worth is only about 20 records and the PUF's
    rounding grid (multiples of 10 or 100) would otherwise make dozens of
    ordinary values into points."""
    vc = pd.Series(mag).value_counts()
    n_side = int(vc.sum())
    points = []
    while True:
        rest = vc.drop(points)
        n = int(rest.sum())
        k = min(k_max, n // MIN_PER_BIN)
        if k < 1:
            break
        new = rest[(rest > n / k) & (rest >= POINT_MIN_SHARE * n_side)].index.tolist()
        new = new[: MAX_POINTS_PER_SIDE - len(points)]
        if not new:
            break
        points += new
    return sorted(float(p) for p in points)


def make_edges(mag, k_max):
    """Contiguous bin edges on magnitudes (bin j = [e_j, e_{j+1}); top bin closed).

    Quantile bins of equal count in the body. At both ends the bins shrink
    geometrically (20, 30, 45, ... records counting in from each end): at the
    top so the largest amounts, which carry much of each total, sit in narrow
    bins; at the bottom so low weighted quantiles, which often fall among the
    smallest nonzero amounts, are resolved. Bins with fewer than MIN_PER_BIN
    records (caused by repeated values) are then merged into their smaller
    neighbour.
    """
    mag = np.sort(mag)
    n = len(mag)
    if n == 0:
        return []
    k = max(1, min(k_max, n // MIN_PER_BIN))
    # End bin sizes (used at both ends), each smaller than the body bin size.
    tail = []
    if k >= 16:
        size = MIN_PER_BIN
        while True:
            body_size = (n - 2 * (sum(tail) + size)) / max(1, k - 2 * (len(tail) + 1))
            if size >= body_size:
                break
            tail.append(int(round(size)))
            size *= TAIL_GROWTH
    k_body = k - 2 * len(tail)
    n_body = n - 2 * sum(tail)
    pos = [0]
    for s_ in tail:
        pos.append(pos[-1] + s_)
    start = pos[-1]
    pos += list(start + np.round(np.linspace(0, n_body, k_body + 1)).astype(int)[1:])
    for s_ in reversed(tail):
        pos.append(pos[-1] + s_)
    assert pos[-1] == n, (pos[-1], n)
    edges = [mag[min(p, n - 1)] for p in pos[:-1]] + [mag[-1]]
    edges = np.unique(edges)
    if len(edges) == 1:
        return [float(edges[0]), float(edges[0])]

    counts = np.bincount(tokenizer._bin_index(mag, edges), minlength=len(edges) - 1)
    edges = list(edges)
    counts = list(counts)
    while len(counts) > 1 and min(counts) < MIN_PER_BIN:
        i = int(np.argmin(counts))
        if i == 0:
            j = 1
        elif i == len(counts) - 1:
            j = i - 1
        else:
            j = i - 1 if counts[i - 1] <= counts[i + 1] else i + 1
        a, b = min(i, j), max(i, j)
        counts[a] += counts[b]
        del counts[b]
        del edges[b]          # the edge between bins a and b
    return [float(e) for e in edges]


def fit_tail_alpha(mag, lo, hi):
    """Maximum-likelihood exponent of a truncated Pareto on [lo, hi]
    (density proportional to x^-(alpha+1)), from unweighted amounts `mag`.

    In log terms the density of y = ln x is proportional to exp(-alpha y),
    a truncated exponential; its likelihood depends on the data only through
    the mean of y, so the fit matches the mean log amount in the bin. alpha = 0
    is log-uniform; alpha may be negative (density rising toward hi).
    """
    la, L = np.log(lo), np.log(hi) - np.log(lo)
    target = np.mean(np.log(mag)) - la            # mean of y - ln lo, in (0, L)

    def mean_y(a):
        if abs(a * L) < 1e-8:
            return L / 2
        return 1 / a - L / np.expm1(a * L)        # decreasing in a

    lo_a, hi_a = -60 / L, 60 / L
    for _ in range(200):
        mid = (lo_a + hi_a) / 2
        if mean_y(mid) > target:
            lo_a = mid
        else:
            hi_a = mid
    return float((lo_a + hi_a) / 2)


def bin_amount(name, x_train):
    """Build the token vocabulary for one amount variable from training values."""
    entries = []   # (sort key, dict)
    entries.append(((0.0, 0.0), {"kind": "zero", "lo": 0.0, "hi": 0.0, "point": True, "decode": "exact"}))
    binning = {"point_values": [], "point_tokens": [], "pos_edges": [], "pos_bin_tokens": [],
               "neg_edges": [], "neg_bin_tokens": []}
    side_info = {}
    for sign, k_max in ((1, MAX_POS_BINS), (-1, MAX_NEG_BINS)):
        mag = np.abs(x_train[np.sign(x_train) == sign])
        pts = choose_points(mag, k_max) if len(mag) else []
        rest = mag[~np.isin(mag, pts)]
        edges = make_edges(rest, k_max) if len(rest) else []
        side_info[sign] = (len(mag), pts, edges)
        for p in pts:
            v = sign * p
            entries.append(((v, v), {"kind": "point", "lo": v, "hi": v, "point": True, "decode": "exact"}))
        n_bins = len(edges) - 1
        for j in range(n_bins):
            lo_m, hi_m = edges[j], edges[j + 1]
            if hi_m < tokenizer.NARROW_RATIO * lo_m:
                dec = {"decode": "uniform"}
            elif j == n_bins - 1 and n_bins > 1:
                # Outermost bin on this side: a truncated Pareto fitted to the
                # training amounts in the bin (see fit_tail_alpha).
                in_bin = rest[tokenizer._bin_index(rest, edges) == j]
                dec = {"decode": "pareto", "alpha": fit_tail_alpha(in_bin, lo_m, hi_m)}
            else:
                dec = {"decode": "log_uniform"}
            if sign > 0:
                d = {"kind": "bin", "lo": lo_m, "hi": hi_m, "point": False, **dec, "_side": 1, "_j": j}
            else:
                d = {"kind": "bin", "lo": -hi_m, "hi": -lo_m, "point": False, **dec, "_side": -1, "_j": j}
            entries.append(((d["lo"], d["hi"]), d))
    entries.sort(key=lambda e: e[0])

    tokens = []
    pos_tok = [None] * max(0, len(side_info[1][2]) - 1)
    neg_tok = [None] * max(0, len(side_info[-1][2]) - 1)
    for t, (_, d) in enumerate(entries):
        d = dict(d)
        side, j = d.pop("_side", None), d.pop("_j", None)
        d = {"token": t, **d}
        tokens.append(d)
        if d["kind"] == "zero":
            binning["zero_token"] = t
        elif d["kind"] == "point":
            binning["point_values"].append(d["lo"])
            binning["point_tokens"].append(t)
        elif side == 1:
            pos_tok[j] = t
        else:
            neg_tok[j] = t
    binning["pos_edges"] = side_info[1][2]
    binning["neg_edges"] = side_info[-1][2]
    binning["pos_bin_tokens"] = pos_tok
    binning["neg_bin_tokens"] = neg_tok
    binning["decode_rule"] = (
        "per token, field 'decode': 'exact' (zero and point tokens); 'log_uniform' (uniform in "
        "log|value| between the bounds); 'uniform' (uniform in dollars, for bins with hi < "
        f"{tokenizer.NARROW_RATIO} x lo); 'pareto' (outermost bin on each side only: density "
        "proportional to |value|^-(alpha+1) between the bounds, alpha fitted by maximum likelihood "
        "to the training amounts in the bin, unweighted). Magnitudes are rounded down to whole dollars.")
    var = {"name": name, "kind": "amount", "vocab_size": len(tokens), "tokens": tokens,
           "binning": binning}
    stats = {
        "variable": name,
        "train_nonzero_pos": side_info[1][0],
        "train_nonzero_neg": side_info[-1][0],
        "pos_bins": len(pos_tok), "neg_bins": len(neg_tok),
        "pos_points": len(side_info[1][1]), "neg_points": len(side_info[-1][1]),
        "vocab_size": len(tokens),
    }
    return var, stats


def build_meta(train, info, strata, n_parts):
    variables = []
    stats = []
    for k, (name, kind, desc) in enumerate(VARIABLES):
        if kind == "weight":
            # Vocabulary from all kept records: S006 takes a small closed set of
            # values (one per sampling stratum), so every value gets a token.
            vals = np.sort(info["_all_s006"])
            cnt = train["S006"].value_counts()
            toks = [{"token": t, "S006": int(v), "lo": v / 100.0, "hi": v / 100.0, "point": True,
                     "train_count": int(cnt.get(v, 0))} for t, v in enumerate(vals)]
            var = {"name": name, "kind": "weight", "values_column": "weight", "scale": 0.01,
                   "vocab_size": len(toks), "tokens": toks,
                   "note": "every distinct S006 value is a point token; decoded weight = S006/100"}
        elif kind == "categorical":
            vals = np.sort(info["_all_cat_values"][name])
            cnt = train[name].value_counts()
            toks = [{"token": t, "value": int(v), "train_count": int(cnt.get(v, 0))}
                    for t, v in enumerate(vals)]
            var = {"name": name, "kind": "categorical", "vocab_size": len(toks), "tokens": toks}
            if name in DEMOG:
                var["missing_value"] = MISSING_CODE
        else:
            var, st = bin_amount(name, train[name].to_numpy())
            # Training counts per token.
            tk = tokenizer._encode_amount(train[name].to_numpy(), var)
            cnt = np.bincount(tk, minlength=var["vocab_size"])
            for t in var["tokens"]:
                t["train_count"] = int(cnt[t["token"]])
            st["min_train_records_in_a_bin"] = int(min(
                [cnt[t["token"]] for t in var["tokens"] if t["kind"] == "bin"], default=0))
            st["train_zero"] = int(cnt[var["binning"]["zero_token"]])
            pts = [(t["lo"], t["train_count"]) for t in var["tokens"] if t["kind"] == "point"]
            st["point_values"] = ";".join(f"{v:.0f}({c})" for v, c in pts)
            stats.append(st)
        var = {"column": k, "description": desc, **var}
        variables.append(var)

    names = [v["name"] for v in variables]
    meta = {
        "description": "PUF generator test, part A: token vocabularies for the 2015 PUF (see SPEC.md)",
        "created": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "source": {"puf": RAW + "puf_2015.csv", "demographics": RAW + "demographics_2015.csv"},
        "seeds": {"split": SPLIT_SEED, "roundtrip_decode": ROUNDTRIP_SEED},
        "counts": {k: v for k, v in info.items() if not k.startswith("_")},
        "split": {
            "fractions": {"train": 0.8, "val": 0.1, "test": 0.1},
            "sizes": n_parts,
            "method": "within each stratum, records shuffled with the split seed; "
                      "round(10%) to val, round(10%) to test, the rest to train",
            "weight_bands_record_weight": WEIGHT_LABELS,
            "agi_bands": AGI_LABELS,
            "strata": strata.to_dict(orient="records"),
        },
        "binning": {
            "min_train_records_per_bin": MIN_PER_BIN,
            "max_positive_bins": MAX_POS_BINS,
            "max_negative_bins": MAX_NEG_BINS,
            "max_point_tokens_per_side": MAX_POINTS_PER_SIDE,
            "point_min_share_of_side": POINT_MIN_SHARE,
            "tail_growth": TAIL_GROWTH,
            "rule": "zero token; point token for any value held by more than one bin's worth of "
                    "nonzero training records on its side and by at least 1% of them; remaining nonzero values in quantile bins "
                    "(unweighted, training records), equal-count in the body and shrinking to about "
                    "20 records at both ends of |value|; bins under 20 records merged",
            "token_order": "tokens of amounts and the weight are sorted by value; token index is the "
                           "position in the variable's order",
        },
        "missing_code": MISSING_CODE,
        "values_columns": ["RECID", "weight"] + [n for n in names if n != "S006"],
        "variables": variables,
        "order": names,
        "order_index": list(range(len(names))),
        "order_note": "generation order: weight, categoricals, AGI, larger incomes, smaller incomes, "
                      "adjustments, deductions. Token array columns use the same order.",
    }
    return meta, pd.DataFrame(stats)


# ---------------------------------------------------------------------------
# Round-trip check
# ---------------------------------------------------------------------------

QS = [0.10, 0.25, 0.50, 0.75, 0.90, 0.99, 0.999]


def wquantile(x, w, qs):
    o = np.argsort(x, kind="stable")
    x, w = x[o], w[o]
    c = np.cumsum(w) / w.sum()
    return np.array([x[min(np.searchsorted(c, q), len(x) - 1)] for q in qs])


def roundtrip(orig, dec):
    w = orig["weight"].to_numpy()
    assert np.array_equal(w, dec["weight"].to_numpy()), "weight did not round-trip exactly"
    rows, qrows = [], []
    for c in CATEGORICALS:
        assert np.array_equal(orig[c].to_numpy(), dec[c].to_numpy()), f"{c} did not round-trip"
    for c in AMOUNTS:
        a, b = orig[c].to_numpy(), dec[c].to_numpy()
        qa, qb = wquantile(a, w, QS), wquantile(b, w, QS)
        rel = np.where(qa != 0, (qb - qa) / np.abs(np.where(qa == 0, 1, qa)), np.where(qb == 0, 0.0, np.inf))
        ta, tb = np.sum(w * a), np.sum(w * b)
        tpa, tpb = np.sum(w * np.maximum(a, 0)), np.sum(w * np.maximum(b, 0))
        tna, tnb = np.sum(w * np.minimum(a, 0)), np.sum(w * np.minimum(b, 0))
        top = a >= qa[-1]
        rows.append({
            "variable": c,
            "share_zero_orig": np.sum(w * (a == 0)) / w.sum(),
            "share_zero_decoded": np.sum(w * (b == 0)) / w.sum(),
            "share_neg_orig": np.sum(w * (a < 0)) / w.sum(),
            "share_neg_decoded": np.sum(w * (b < 0)) / w.sum(),
            "total_rel_err": (tb - ta) / abs(ta) if ta != 0 else np.nan,
            "pos_total_rel_err": (tpb - tpa) / tpa if tpa else np.nan,
            "neg_total_rel_err": (tnb - tna) / abs(tna) if tna else np.nan,
            "max_abs_quantile_rel_err": float(np.max(np.abs(rel))),
            "worst_quantile": QS[int(np.argmax(np.abs(rel)))],
            "records_equal_after_roundtrip": float(np.mean(a == b)),
            "unweighted_median_abs_rel_err_nonzero": float(np.median(np.abs(b[a != 0] - a[a != 0]) / np.abs(a[a != 0]))) if (a != 0).any() else 0.0,
        })
        for q, x, y, r in zip(QS, qa, qb, rel):
            qrows.append({"variable": c, "q": q, "rel_err": r, "orig_is_zero": x == 0})
    return pd.DataFrame(rows), pd.DataFrame(qrows)


# ---------------------------------------------------------------------------

def main():
    os.makedirs(OUT, exist_ok=True)
    marker = os.path.join(OUT, "DONE")
    if os.path.exists(marker):
        os.remove(marker)

    df, info = read_puf()
    log("records kept", len(df), "aggregate set aside", info["aggregate_records_set_aside"])
    info["_all_s006"] = np.unique(df.S006.to_numpy())
    info["_all_cat_values"] = {c: np.unique(df[c].to_numpy()) for c in CATEGORICALS}

    # Demographic-file coverage by weight band (for the report).
    wb = pd.cut(df.weight, WEIGHT_BANDS, right=False, labels=WEIGHT_LABELS)
    cov = df.assign(wb=wb, absent=df.GENDER == MISSING_CODE).groupby("wb", observed=True).agg(
        records=("absent", "size"), absent_from_demographic_file=("absent", "sum"))
    log("demographic coverage by weight band\n", cov)
    info["demographic_absence_by_weight_band"] = {
        k: {"records": int(r.records), "absent": int(r.absent_from_demographic_file)}
        for k, r in cov.iterrows()}
    info["distinct_S006_values"] = int(len(info["_all_s006"]))

    part, strata = split(df)
    n_parts = {p: int((part == p).sum()) for p in ("train", "val", "test")}
    log("split sizes", n_parts)
    strata.to_csv(os.path.join(OUT, "split_strata.csv"), index=False)

    parts = {p: df.loc[part == p].reset_index(drop=True) for p in ("train", "val", "test")}
    train = parts["train"]

    meta, bstats = build_meta(train, info, strata, n_parts)
    bstats.to_csv(os.path.join(OUT, "bins_summary.csv"), index=False)
    with open(os.path.join(OUT, "meta.json"), "w") as f:
        json.dump(meta, f, indent=1)
    meta = tokenizer.load_meta(os.path.join(OUT, "meta.json"))   # use exactly what was written
    log("vocab sizes", {v["name"]: v["vocab_size"] for v in meta["variables"]})

    values_cols = meta["values_columns"]
    clip_report = {}
    for p, d in parts.items():
        vals = d[values_cols]
        tok = tokenizer.encode(vals, meta)
        assert tok.shape == (len(d), len(meta["variables"]))
        np.save(os.path.join(OUT, f"tokens_{p}.npy"), tok)
        vals.to_parquet(os.path.join(OUT, f"values_{p}.parquet"), index=False)
        log(p, "tokens", tok.shape, tok.dtype)
        # Values outside the training range (put in the end bins).
        if p != "train":
            out = {}
            for v in meta["variables"]:
                if v["kind"] != "amount":
                    continue
                b = v["binning"]
                x = d[v["name"]].to_numpy()
                pts = np.array(b["point_values"], dtype=np.float64)
                binned = (x != 0) & ~np.isin(x, pts)
                n_out = 0
                if b["pos_edges"]:
                    e = b["pos_edges"]
                    n_out += int((binned & (x > 0) & ((x < e[0]) | (x > e[-1]))).sum())
                if b["neg_edges"]:
                    e = b["neg_edges"]
                    n_out += int((binned & (x < 0) & ((-x < e[0]) | (-x > e[-1]))).sum())
                if n_out:
                    out[v["name"]] = n_out
            clip_report[p] = out
    log("values outside training bin range (put in end bins):", clip_report)

    # Round-trip check on train.
    tok = np.load(os.path.join(OUT, "tokens_train.npy"))
    rng = np.random.default_rng(ROUNDTRIP_SEED)
    dec = tokenizer.decode(tok, meta, rng)
    vals = pd.read_parquet(os.path.join(OUT, "values_train.parquet"))
    assert list(dec.columns) == [c for c in values_cols if c != "RECID"]
    rt, rtq = roundtrip(vals, dec)
    # Decoding is random; repeat the totals with further seeds to separate the
    # systematic part of the error from draw-to-draw noise.
    extra = []
    for i in range(1, 5):
        d_i = tokenizer.decode(tok, meta, np.random.default_rng(ROUNDTRIP_SEED + i))
        extra.append(roundtrip(vals, d_i)[0].set_index("variable").total_rel_err)
    all_tot = pd.concat([rt.set_index("variable").total_rel_err] + extra, axis=1)
    rt["total_rel_err_mean_5_seeds"] = all_tot.mean(axis=1).to_numpy()
    rt["total_rel_err_sd_5_seeds"] = all_tot.std(axis=1, ddof=1).to_numpy()
    rt.to_csv(os.path.join(OUT, "roundtrip.csv"), index=False)
    rtq.to_csv(os.path.join(OUT, "roundtrip_quantiles.csv"), index=False)
    # Re-encoding the decoded records must give the same tokens.
    re_tok = tokenizer.encode(dec, meta)
    same = (re_tok == tok).mean(axis=0)
    rt_reencode = {v["name"]: float(s) for v, s in zip(meta["variables"], same)}
    pd.set_option("display.width", 200)
    log(rt.to_string())
    log("re-encode agreement (share of records whose token is unchanged):", rt_reencode)

    summary = {
        "counts": {k: v for k, v in info.items() if not k.startswith("_")},
        "split_sizes": n_parts,
        "values_outside_training_range": clip_report,
        "reencode_agreement": rt_reencode,
        "roundtrip_max_abs_total_rel_err": float(rt.total_rel_err.abs().max()),
        "roundtrip_max_abs_quantile_rel_err": float(rt.max_abs_quantile_rel_err.max()),
    }
    with open(os.path.join(OUT, "prep_summary.json"), "w") as f:
        json.dump(summary, f, indent=1, default=float)
    log("done")


if __name__ == "__main__":
    main()
