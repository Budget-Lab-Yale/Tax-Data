"""Shared input and output helpers for piece D (the comparison methods).

Reads a dataset folder written by piece A (SPEC.md, section A, item 5) and
writes completed files in the layout of SPEC.md section B as amended in
"Contract changes" (C, 12:55): the record columns of `records_test.parquet`,
a `row` column (0-based row in `records_test.parquet`), a `draw` column
(0 .. K-1), the completed dollar value of each wealth variable in a column
named as the variable, and the completed token in `tok_<variable>`.

Target and training records, for every dataset:
- training records: rows of `records_train.parquet` whose wealth variables
  are all observed in `tokens_train.npy` (experiment A: half 2's training
  rows; experiment B: the SCF training rows);
- validation records (used only by the sequential trees, for early
  stopping): the same rule applied to the `val` split;
- target records: rows of `records_test.parquet` whose wealth is hidden
  (experiment A: half 1) or, in experiment B, the SCF test rows (wealth
  observed in the token file but it is the truth being predicted) and,
  optionally, the PUF test rows.
"""

import json
import os

import numpy as np
import pandas as pd

S = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"

ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities',
          'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
          're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards',
         'installment_debt', 'other_debt']
KG = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
WEALTH = ASSETS + DEBTS + KG
INCOMES = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens",
           "ui_other"]
RECORD_KEYS = ["source", "year", "hh_id", "implicate", "unit"]


def data_dir(dataset):
    """Real datasets live in $S/data/<name>; fabricated ones in $S/dev/d/."""
    for base in (os.path.join(S, "data"), os.path.join(S, "dev", "d")):
        p = os.path.join(base, dataset)
        if os.path.exists(os.path.join(p, "meta.json")):
            return p
    raise FileNotFoundError(f"no meta.json for dataset {dataset}")


def comp_dir(dataset):
    # D_COMP_ROOT (optional) sends trial runs elsewhere, so piece C's watcher
    # does not evaluate them.
    p = os.path.join(os.environ.get("D_COMP_ROOT", os.path.join(S, "comp")), dataset)
    os.makedirs(p, exist_ok=True)
    return p


def load(dataset, split):
    d = data_dir(dataset)
    with open(os.path.join(d, "meta.json")) as f:
        meta = json.load(f)
    tok = np.load(os.path.join(d, f"tokens_{split}.npy")).astype(np.int32)
    rec = pd.read_parquet(os.path.join(d, f"records_{split}.parquet"))
    if len(rec) != tok.shape[0]:
        raise ValueError(f"{dataset}/{split}: {len(rec)} records but "
                         f"{tok.shape[0]} token rows")
    return meta, tok, rec


def var_index(meta):
    """Variable name -> column in the token arrays (meta's `column` field
    where present, else position in meta['variables'])."""
    return {v["name"]: int(v.get("column", i)) for i, v in enumerate(meta["variables"])}


def var_meta(meta, name):
    for v in meta["variables"]:
        if v["name"] == name:
            return v
    raise KeyError(name)


def generation_order(meta):
    """Wealth variables in the dataset's default completion order."""
    for key in ("generation_order", "completion_order", "order"):
        if key in meta:
            order = [v for v in meta[key] if v in WEALTH]
            missing = set(WEALTH) - set(order)
            if missing:
                raise ValueError(f"generation order lacks {sorted(missing)}")
            return order
    raise KeyError("meta.json has no generation order")


def wealth_observed(meta, tok):
    """Boolean per row: all 24 wealth variables observed (no -1)."""
    vi = var_index(meta)
    cols = [vi[v] for v in WEALTH]
    obs = tok[:, cols] >= 0
    partial = obs.any(axis=1) & ~obs.all(axis=1)
    if partial.any():
        raise ValueError(f"{int(partial.sum())} rows have some but not all "
                         "wealth variables observed; D does not handle this")
    return obs.all(axis=1)


def training_mask(meta, tok, rec):
    """Rows usable for training: all 24 wealth tokens observed.

    Since A's 13:19 change, split-off SCF units have wealth unobserved
    (-1), so they are not training records; the records file's
    `wealth_observed` must be True on every row used (half 1 in experiment
    A has it True but its tokens hidden, so it is not used either).
    """
    m = wealth_observed(meta, tok)
    if "wealth_observed" in rec.columns:
        bad = m & ~rec["wealth_observed"].astype(bool).values
        if bad.any():
            raise ValueError(f"{int(bad.sum())} rows have wealth tokens but "
                             "wealth_observed False")
        n_split = int((~rec["wealth_observed"].astype(bool).values
                       & (rec["source"].astype(str).values != "PUF")).sum())
        print(f"  SCF rows with wealth unobserved (split-off units), not used "
              f"for training: {n_split}")
    return m


def target_rows(dataset, meta, rec, tok, include_puf=False):
    """0-based rows of records_test.parquet that D completes.

    Experiment A (sources SCF and SCF_H1): rows with wealth hidden, which
    is half 1. Experiment B: SCF rows, plus PUF rows if include_puf.
    """
    src = rec["source"].astype(str).values
    if "PUF" in set(src):
        keep = src == "SCF"
        if include_puf:
            keep = keep | (src == "PUF")
    else:
        keep = ~wealth_observed(meta, tok)
    rows = np.flatnonzero(keep)
    return rows


def token_sort_key(var):
    """A number per token of an amount variable that orders tokens by value.

    Used to turn rank tokens into ordered numeric features for trees: zero
    maps to 0, a positive rank band to 1 + its midpoint rank, a negative
    band to -(1 + its midpoint rank among negatives). Categorical tokens map
    to their category value. Reads whatever token descriptors meta.json
    carries; raises if it cannot tell.
    """
    if var["kind"] == "amount" and "binning" in var:
        # Piece A's layout: amount tokens are sorted by value (negative
        # bins, zero, positive bins), so the token index less the zero
        # token's index is already an ordered number with zero at 0.
        return (np.arange(len(var["tokens"]), dtype=float)
                - float(var["binning"]["zero_token"]))
    keys = []
    for k, t in enumerate(var["tokens"]):
        if var["kind"] == "categorical":
            keys.append(float(t.get("value", k)))
            continue
        kind = t.get("kind")
        if kind is None:
            if t.get("zero"):
                kind = "zero"
            elif t.get("negative") or t.get("neg"):
                kind = "negative"
            elif "lo" in t:
                kind = "positive"
            else:
                kind = "other"
        if kind == "zero":
            keys.append(0.0)
        elif kind == "positive":
            keys.append(1.0 + 0.5 * (float(t["lo"]) + float(t["hi"])))
        elif kind == "negative":
            keys.append(-(1.0 + 0.5 * (float(t["lo"]) + float(t["hi"]))))
        else:
            keys.append(float(k))
    return np.array(keys)


def scf_years_for(year, scf_years):
    """SCF waves whose quantile functions decode wealth at `year`
    (a_tokenizer: the wave itself, else the two around it)."""
    if year in scf_years:
        return [year]
    lo = [y for y in scf_years if y < year]
    hi = [y for y in scf_years if y > year]
    if lo and hi:
        return [max(lo), min(hi)]
    return [min(scf_years)] if not lo else [max(scf_years)]


def sides_present(quantiles, var_name, years):
    """{'pos': bool, 'neg': bool}: the side has an SCF quantile function in
    every one of `years`."""
    q = quantiles[(quantiles["source"] == "SCF") & (quantiles["variable"] == var_name)]
    out = {}
    for side in ("pos", "neg"):
        out[side] = all(((q["year"] == y) & (q["side"] == side)).any() for y in years)
    return out


def allowed_tokens(var, sides):
    """Boolean per token of an amount variable: decodable given `sides`."""
    kinds = np.array([t.get("kind") for t in var["tokens"]])
    ok = kinds == "zero"
    if sides["pos"]:
        ok |= kinds == "pos"
    if sides["neg"]:
        ok |= kinds == "neg"
    return ok


def write_completed(dataset, method, seed, rec_target, rows, draws,
                    dollars, tokens, extra=None):
    """Write $S/comp/<dataset>/completed_<method>_seed<NN>.parquet.

    rec_target: records_test rows being completed (DataFrame, in `rows`
    order); draws: int array; dollars/tokens: dicts var -> array, aligned
    with the stacked (draw-major) rows.
    """
    k = len(np.unique(draws))
    out = pd.concat([rec_target.reset_index(drop=True)] * k, ignore_index=True)
    out["row"] = np.tile(rows, k)
    out["draw"] = draws
    for v in WEALTH:
        out[v] = dollars[v] if dollars is not None else np.nan
    if tokens is not None:
        for v in WEALTH:
            out[f"tok_{v}"] = tokens[v].astype(np.int32)
    out["method"] = method
    out["seed"] = seed
    if extra:
        for c, x in extra.items():
            out[c] = x
    path = os.path.join(comp_dir(dataset),
                        f"completed_{method}_seed{seed:02d}.parquet")
    out.to_parquet(path, index=False)
    return path
