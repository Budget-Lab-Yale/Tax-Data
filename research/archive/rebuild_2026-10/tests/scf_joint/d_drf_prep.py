"""Inputs for the production-method refit (d_drf.R), from a SPEC dataset.

Writes $S/comp/<dataset>/drf_input/{train,targets}.parquet with, per record:
`row` (0-based row in its records file), `source`, `year`, `weight`, the
three shared categoricals as their category values (`age_range`, `married`,
`n_dep`; age_range missing where unobserved), the seven shared incomes in
dollars, and for training records the 24 wealth amounts in dollars.

Income dollars come from the records file. Because the variants of
experiment A alter half 1's incomes before ranking, the script checks that
the records' dollars agree with the tokens the model sees (same zero
pattern, and token order never reverses dollar order within a source-year),
and stops if not, so the production method never sees different incomes
from the model.

Usage (sbatch only): python d_drf_prep.py --dataset expA_clean [--include-puf]
"""

import argparse
import os

import numpy as np
import pandas as pd

import d_common as dc


def categorical_values(meta, tok, name):
    vi = dc.var_index(meta)
    var = dc.var_meta(meta, name)
    key = dc.token_sort_key(var)
    t = tok[:, vi[name]]
    return np.where(t >= 0, key[np.clip(t, 0, None)], np.nan)


def check_income_dollars(meta, tok, rec, label):
    """Stop if the records' income dollars disagree with the income tokens."""
    vi = dc.var_index(meta)
    groups = rec["source"].astype(str) + "_" + rec["year"].astype(str)
    for v in dc.INCOMES:
        var = dc.var_meta(meta, v)
        key = dc.token_sort_key(var)
        t = tok[:, vi[v]]
        x = rec[v].values.astype(float)
        obs = t >= 0
        if np.isnan(x[obs]).any():
            raise ValueError(f"{label} {v}: {int(np.isnan(x[obs]).sum())} observed "
                             "tokens with missing dollars")
        k = key[np.clip(t, 0, None)]
        zero_mismatch = obs & ((k == 0) != (x == 0))
        if zero_mismatch.any():
            raise ValueError(f"{label} {v}: {int(zero_mismatch.sum())} records where "
                             "the dollar value and the token disagree about zero")
        bad = 0
        for g in groups.unique():
            m = (groups == g).values & obs & (x != 0)
            o = np.argsort(x[m], kind="stable")
            ks = k[m][o]
            xs = x[m][o]
            # Order reversal: a larger dollar amount with a lower rank token,
            # excluding exact ties in dollars.
            rev = (np.diff(ks) < 0) & (np.diff(xs) > 0)
            bad += int(rev.sum())
        if bad:
            raise ValueError(f"{label} {v}: {bad} adjacent pairs where tokens reverse "
                             "dollar order; records dollars are not what was ranked")
    print(f"{label}: income dollars agree with the income tokens")


def build(meta, tok, rec, rows, with_wealth):
    out = pd.DataFrame({"row": rows})
    sub = rec.iloc[rows].reset_index(drop=True)
    for c in ["source", "year", "hh_id", "implicate", "unit", "weight"]:
        out[c] = sub[c].values
    for c in ["age_range", "married", "n_dep"]:
        out[c] = categorical_values(meta, tok[rows], c)
    for v in dc.INCOMES:
        out[v] = sub[v].values.astype(float)
    if with_wealth is not None:
        # with_wealth: boolean per row, True for training records (wealth
        # observed); the others are in the file only for ranking.
        out["in_training"] = with_wealth
        for v in dc.WEALTH:
            x = sub[v].values.astype(float)
            if np.isnan(x[with_wealth]).any():
                raise ValueError(f"training wealth {v}: {int(np.isnan(x[with_wealth]).sum())} missing")
            out[v] = x
    for c in ["married", "n_dep"]:
        if out[c].isna().any():
            raise ValueError(f"{c}: {int(out[c].isna().sum())} unobserved; the "
                             "production features need it")
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True)
    ap.add_argument("--include-puf", action="store_true")
    args = ap.parse_args()

    meta, tok_tr, rec_tr = dc.load(args.dataset, "train")
    _, tok_te, rec_te = dc.load(args.dataset, "test")
    # Rank base: every SCF unit of the training split, split-off units
    # included (their incomes are observed), so that training records are
    # ranked among all filers, as the targets are. Only units with wealth
    # observed are training records (in_training).
    train_mask = dc.training_mask(meta, tok_tr, rec_tr)
    tr_rows = np.flatnonzero(rec_tr["source"].astype(str).values == "SCF")
    if not set(np.flatnonzero(train_mask)) <= set(tr_rows):
        raise ValueError("training records outside the SCF rows of the training split")
    print(f"training rows with wealth observed: {int(train_mask.sum())}; SCF rows in the "
          f"rank base (adds split-off units): {len(tr_rows)} of {len(rec_tr)}")
    te_rows = dc.target_rows(args.dataset, meta, rec_te, tok_te, args.include_puf)
    print(f"target rows: {len(te_rows)} of {len(rec_te)}; by source:",
          rec_te.iloc[te_rows]["source"].value_counts().to_dict())

    check_income_dollars(meta, tok_tr[tr_rows], rec_tr.iloc[tr_rows].reset_index(drop=True), "train")
    check_income_dollars(meta, tok_te[te_rows], rec_te.iloc[te_rows].reset_index(drop=True), "targets")

    out_dir = os.path.join(dc.comp_dir(args.dataset), "drf_input")
    os.makedirs(out_dir, exist_ok=True)
    tr = build(meta, tok_tr, rec_tr, tr_rows, train_mask[tr_rows])
    te = build(meta, tok_te, rec_te, te_rows, None)
    n_age_na = int(te["age_range"].isna().sum())
    print(f"targets with age range unobserved: {n_age_na} (d_drf.R draws one "
          "from training records in the same income cell and marital status)")
    if tr["age_range"].isna().any():
        raise ValueError("training rows with unobserved age range")
    tr.to_parquet(os.path.join(out_dir, "train.parquet"), index=False)
    te.to_parquet(os.path.join(out_dir, "targets.parquet"), index=False)
    print("wrote", out_dir)


if __name__ == "__main__":
    main()
