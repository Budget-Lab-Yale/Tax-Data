"""Turn d_drf.R's donor draws into a completed file in B's layout.

Reads $S/comp/<dataset>/drf_raw_seed<NN>.parquet (one row per target record
and draw, the donor's 24 wealth amounts in the target year's dollars),
attaches the target records from records_test.parquet, encodes the dollars
to rank tokens with a_tokenizer.encode (on the fabricated development data,
a simple encoder here), and writes completed_drf_seed<NN>.parquet (SCF
targets) and, if present, completed_drf-puf_seed<NN>.parquet (PUF targets).

Usage (sbatch only): python d_drf_finish.py --dataset expA_clean --seed 1
"""

import argparse
import os

import numpy as np
import pandas as pd

import d_common as dc


def encode_wealth(dataset, meta, rec_stacked, dollars, seed):
    """Rank tokens of the completed wealth dollars, with a_tokenizer.encode.

    Each value is ranked within the SCF quantile function of the target's
    own year (a_tokenizer ranks a source's unobserved variables with the
    SCF function of the same year). Ties are spread with a seeded generator.
    """
    import a_tokenizer as at
    df = pd.DataFrame({"source": rec_stacked["source"].values,
                       "year": rec_stacked["year"].values})
    for var in meta["variables"]:
        if var["name"] not in df.columns:
            df[var["name"]] = np.nan
    quantiles = pd.read_parquet(os.path.join(dc.data_dir(dataset), "quantiles.parquet"))
    # A donor from one SCF wave can carry a negative value of a variable
    # that is never negative in the target's wave (experiment B pools 2013
    # and 2016), and that wave has no negative rank scale to encode it.
    # Such values keep their dollars but get the zero token; counted here.
    n_unencodable = {}
    for v in dc.WEALTH:
        x = np.asarray(dollars[v], dtype=float).copy()
        for y in np.unique(df["year"].values):
            m = df["year"].values == y
            sides = dc.sides_present(quantiles, v, [int(y)])
            for side, sel in (("neg", x < 0), ("pos", x > 0)):
                bad = m & sel
                if not sides[side] and bad.any():
                    n_unencodable[f"{v} {int(y)} {side}"] = int(bad.sum())
                    x[bad] = 0.0
        df[v] = x
    if n_unencodable:
        print("values with no rank scale in the target's wave (zero token, dollars kept):",
              n_unencodable)
    toks = np.asarray(at.encode(df, meta, quantiles, np.random.default_rng([seed, 4242])))
    vi = dc.var_index(meta)
    out = {v: toks[:, vi[v]] for v in dc.WEALTH}
    for v, t in out.items():
        if (t < 0).any():
            raise ValueError(f"{v}: {int((t < 0).sum())} completed values not encoded")
    return out


def simple_encode(dataset, meta, dollars):
    """Development-only encoder for the fabricated data (SCF quantiles)."""
    q = pd.read_parquet(os.path.join(dc.data_dir(dataset), "quantiles.parquet"))
    vi = dc.var_index(meta)
    out = {}
    for v in dc.WEALTH:
        var = dc.var_meta(meta, v)
        x = np.asarray(dollars[v], dtype=float)
        t = np.full(len(x), -1, np.int32)
        zero = [k for k, tk in enumerate(var["tokens"]) if tk["kind"] == "zero"][0]
        t[x == 0] = zero
        for sign, m in (("positive", x > 0), ("negative", x < 0)):
            if not m.any():
                continue
            qq = q[(q.variable == v) & (q.sign == sign) & (q.source == "SCF")]
            idx = [k for k, tk in enumerate(var["tokens"]) if tk["kind"] == sign]
            hi = np.array([var["tokens"][k]["hi"] for k in idx])
            vals, ps = np.abs(qq.value.values), qq.p.values
            r = np.interp(np.abs(x[m]), vals, ps)
            t[m] = np.array(idx)[np.minimum(np.searchsorted(hi, r - 1e-12), len(idx) - 1)]
        if (t < 0).any():
            raise ValueError(f"{v}: {int((t < 0).sum())} values not encoded")
        out[v] = t
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True)
    ap.add_argument("--seed", type=int, required=True)
    ap.add_argument("--method", default="drf", help="drf, or drfsw for the survey-weighted forest")
    args = ap.parse_args()
    meta, tok_te, rec_te = dc.load(args.dataset, "test")
    raw = pd.read_parquet(os.path.join(dc.comp_dir(args.dataset),
                                       f"{args.method}_raw_seed{args.seed:02d}.parquet"))
    if raw[dc.WEALTH].isna().any().any():
        raise ValueError("missing wealth values in drf_raw")
    src = rec_te["source"].astype(str).values
    for tag, keep_src in (("", "SCF_like"), ("-puf", "PUF")):
        if keep_src == "PUF":
            sub = raw[src[raw["row"].values] == "PUF"]
        else:
            sub = raw[src[raw["row"].values] != "PUF"]
        if len(sub) == 0:
            continue
        sub = sub.sort_values(["draw", "row"]).reset_index(drop=True)
        k = sub["draw"].nunique()
        rows = sub.loc[sub.draw == 0, "row"].values
        for d in range(k):
            if not np.array_equal(sub.loc[sub.draw == d, "row"].values, rows):
                raise ValueError("draws do not cover the same target rows")
        rec_target = rec_te.iloc[rows]
        rec_stacked = pd.concat([rec_target.reset_index(drop=True)] * k, ignore_index=True)
        dollars = {v: sub[v].values for v in dc.WEALTH}
        if "fake" in args.dataset:
            toks = simple_encode(args.dataset, meta, dollars)
        elif tag == "-puf":
            # a_tokenizer.encode has no single SCF quantile function for
            # 2015 (it lies between the 2013 and 2016 waves), so PUF
            # completions are written in dollars only; C reads dollars.
            toks = None
        else:
            toks = encode_wealth(args.dataset, meta, rec_stacked, dollars, args.seed)
        extra = {c: sub[c].values for c in ("donor_row", "donor_hh", "donor_year",
                                            "leaf_size", "cell")}
        path = dc.write_completed(args.dataset, args.method + tag, args.seed, rec_target,
                                  rows, sub["draw"].values, dollars, toks, extra)
        print("wrote", path, "records", len(rows), "draws", k)


if __name__ == "__main__":
    main()
