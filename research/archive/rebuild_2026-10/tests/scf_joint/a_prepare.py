"""Test 2, piece A: build the token datasets (see SPEC.md section A).

    python a_prepare.py expA_clean [expA_noise expA_recat expA_unrep expB crosswalk]

Inputs: $S/data/scf_units/scf_units_<wave>.csv.gz (from a_build_units.R),
the raw 2015 PUF and its demographic file, and test 1's split
(/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data/values_*.parquet).

Outputs per dataset in $S/data/<dataset>/: meta.json, tokens_{train,val,test}.npy,
truth_{train,val,test}.npy, records_{train,val,test}.parquet, quantiles.parquet,
roundtrip.csv, prep_summary.json. `crosswalk` writes $S/data/crosswalk_table.csv.
The DONE marker is written by hand after review (this script removes it).
"""

import json
import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import a_tokenizer as tk  # noqa: E402

S = tk.S
UNITS = os.path.join(S, "data", "scf_units")
PUF_RAW = "/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/"
T1 = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data/"

SEED_SPLIT = 20261009
SEED_ALTER = 20261010
SEED_ENCODE = 20261011
SEED_ROUNDTRIP = 20261012

INC = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens", "ui_other"]
WEALTH = ["cash", "equities", "bonds", "dc", "db", "life_ins", "annuities", "trusts",
          "other_fin", "pass_throughs", "primary_home", "other_home", "re_fund",
          "other_nonfin", "primary_mortgage", "other_mortgage", "credit_lines",
          "credit_cards", "installment_debt", "other_debt", "kg_primary_home",
          "kg_other_re", "kg_pass_throughs", "kg_other"]
DEBTS = ["primary_mortgage", "other_mortgage", "credit_lines", "credit_cards",
         "installment_debt", "other_debt"]
KG = ["kg_primary_home", "kg_other_re", "kg_pass_throughs", "kg_other"]
# The SCFP summary files on the cluster are all in 2022 dollars. Wealth comes
# from them (through the builder), so it is divided by these factors to put
# each wave in its own survey-year dollars. Measured record by record by
# a_scfp_dollars.R (SCFP HOUSES / raw X716; identical for every record):
# $S/data/scf_units/scfp_dollar_factors.csv. Incomes come from the raw
# interview items and are already in tax-year (wave - 1) dollars.
WEALTH_FACTOR = {2013: 1.272833, 2016: 1.233371, 2019: 1.159205, 2022: 1.0}
AGE_LABELS = {1: "under 26", 2: "26 to 34", 3: "35 to 44", 4: "45 to 54", 5: "55 to 64", 6: "65 and over"}

DESC = {
    "source": "Source of the record (context, always observed)",
    "age_range": "Age range of the primary filer (PUF AGERANGE codes 1-6; SCF: age of the tax unit's head)",
    "married": "Married filing jointly (1) or not (0)",
    "n_dep": "Number of dependents, top-coded at 3",
    "wages": "Wages and salaries",
    "business": "Schedule C and F business and farm net income",
    "int_div": "Taxable and tax-exempt interest plus dividends",
    "capital_gains": "Net capital gains or losses (Schedule D before the loss limit, plus other gains)",
    "rent": "Schedule E: rent, royalties, partnerships, S corporations, estates and trusts",
    "ss_pens": "Social Security (gross) plus pensions and annuities (PUF: taxable pensions)",
    "ui_other": "Unemployment compensation and alimony (SCF also: workers' compensation, child support)",
    "scf_weight": "SCF survey weight (population weight, implicate weight = household weight / 5)",
}


def log(*a):
    print(*a, flush=True)


def age_range(age):
    return pd.cut(age, [-np.inf, 26, 35, 45, 55, 65, np.inf], right=False, labels=[1, 2, 3, 4, 5, 6]).astype(float)


# ---------------------------------------------------------------------------
# Loading
# ---------------------------------------------------------------------------

def load_scf(wave, info):
    u = pd.read_csv(os.path.join(UNITS, f"scf_units_{wave}.csv.gz"))
    n_all = len(u)
    filer = u["filer"].astype(str).str.upper().isin(["TRUE", "1"])
    drop = u.loc[~filer]
    info[f"scf{wave}_unit_rows"] = n_all
    info[f"scf{wave}_nonfiler_rows_dropped"] = int(len(drop))
    info[f"scf{wave}_nonfiler_rows_dropped_by_unit_code"] = {
        str(k): int(v) for k, v in drop.taxunit_code.clip(upper=3).value_counts().sort_index().items()}
    info[f"scf{wave}_nonfiler_weight_dropped_millions"] = float(drop.weight.sum() / 1e6)
    u = u.loc[filer].reset_index(drop=True)
    na = {c: int(u[c].isna().sum()) for c in ["age1", "n_dep", "filestat", "weight"] + INC + WEALTH}
    if any(na.values()):
        raise ValueError(f"NA in SCF {wave}: {na}")
    df = pd.DataFrame({
        "source": "SCF", "year": wave, "hh_id": u.scf_hh_id.astype(np.int64),
        "implicate": u.implicate.astype(np.int64), "unit": u.taxunit_code.astype(np.int64),
        "weight": u.weight.astype(float), "half": 0, "age": u.age1.astype(float),
        "age_range": age_range(u.age1), "married": (u.filestat == 2).astype(float),
        "n_dep": u.n_dep.clip(upper=3).astype(float),
    })
    for c in INC:
        df[c] = u[c].astype(float)
    # John, 9 October: split-off units (taxunit_code >= 3) have zero wealth by
    # construction (the SCF does not ask about their assets), so their wealth
    # is unobserved (NaN here, token -1), not zero.
    split_off = (u.taxunit_code >= 3).to_numpy()
    df["wealth_observed"] = ~split_off
    for c in WEALTH:
        if (u.loc[split_off, c] != 0).any():
            raise ValueError(f"split-off units with nonzero {c} in SCF {wave}")
        df[c] = np.where(split_off, np.nan, u[c].astype(float) / WEALTH_FACTOR[wave])
    info[f"scf{wave}_split_off_filer_rows_wealth_unobserved"] = int(split_off.sum())
    info[f"scf{wave}_split_off_filer_weight_millions"] = float(u.weight[split_off].sum() / 1e6)
    info[f"scf{wave}_wealth_divided_by_scfp_dollar_factor"] = WEALTH_FACTOR[wave]
    # SCF 2022: X5722 (Social Security and pensions) is -1, the codebook's
    # "nothing" code, for one household's five implicates, and its
    # defined-benefit pension value is -1 too. Neither can be negative, so -1
    # is set to 0 (counted). No other wave has negative values in these.
    for c in ["ss_pens", "db"]:
        neg = (df[c] < 0).to_numpy()
        if neg.any():
            if not np.all(df.loc[neg, c] == -1):
                raise ValueError(f"unexpected negative {c} in SCF {wave}")
            df.loc[neg, c] = 0.0
        info[f"scf{wave}_{c}_minus1_set_to_0"] = int(neg.sum())
    df["scf_weight"] = df["weight"]
    for c in ["hh_X5704", "hh_X5714"]:
        info[f"scf{wave}_filer_rows_{c}_eq_-9"] = int((u[c] == -9).sum())
        info[f"scf{wave}_filer_rows_{c}_eq_-2"] = int((u[c] == -2).sum())
    return df


PUF_COLS = ["RECID", "S006", "MARS", "DSI", "XTOT", "E00200", "E00900", "E02100", "E00300",
            "E00400", "E00600", "E01000", "E01100", "E01200", "P22250", "P23250", "E02000",
            "E25850", "E25860", "E26270", "E26390", "E26400", "E01500", "E01700", "E02400",
            "E02300", "E00800", "E00700", "E01400"]


def load_puf_raw():
    puf = pd.read_csv(PUF_RAW + "puf_2015.csv", usecols=PUF_COLS)
    demo = pd.read_csv(PUF_RAW + "demographics_2015.csv", usecols=["RECID", "AGERANGE"])
    puf = puf.loc[puf.RECID < 999996].merge(demo, on="RECID", how="left")
    return puf


def puf_crosswalk(p):
    """The proposed crosswalk (a_crosswalk.md)."""
    return {
        "wages": p.E00200,
        "business": p.E00900 + p.E02100,
        "int_div": p.E00300 + p.E00400 + p.E00600,
        "capital_gains": p.P22250 + p.P23250 + p.E01100 + p.E01200,
        "rent": p.E02000,
        "ss_pens": p.E01700 + p.E02400,
        "ui_other": p.E02300 + p.E00800,
    }


def load_puf(info):
    puf = load_puf_raw()
    parts = []
    for part in ["train", "val", "test"]:
        r = pd.read_parquet(T1 + f"values_{part}.parquet", columns=["RECID"])
        r["part"] = part
        parts.append(r)
    parts = pd.concat(parts)
    assert parts.RECID.is_unique
    puf = puf.merge(parts, on="RECID", how="inner", validate="one_to_one")
    info["puf_records_nonaggregate"] = int(len(puf))
    dep = puf.DSI == 1
    info["puf_dependent_filers_dropped"] = int(dep.sum())
    info["puf_dependent_filers_dropped_weight_millions"] = float((puf.S006[dep] / 100).sum() / 1e6)
    info["puf_dependent_filers_dropped_by_part"] = {k: int(v) for k, v in puf.part[dep].value_counts().items()}
    puf = puf.loc[~dep].reset_index(drop=True)
    na = {c: int(puf[c].isna().sum()) for c in PUF_COLS}
    if any(na.values()):
        raise ValueError(f"NA in PUF columns: {na}")
    ag = puf.AGERANGE
    info["puf_agerange_absent_from_demographic_file"] = int(ag.isna().sum())
    info["puf_agerange_code_0_not_present"] = int((ag == 0).sum())
    df = pd.DataFrame({
        "source": "PUF", "year": 2015, "hh_id": puf.RECID.astype(np.int64), "implicate": 0, "unit": 0,
        "weight": puf.S006 / 100.0, "half": 0, "age": np.nan,
        "age_range": ag.where(ag.isin([1, 2, 3, 4, 5, 6])).astype(float),
        "married": (puf.MARS == 2).astype(float),
        "n_dep": (puf.XTOT - 1 - (puf.MARS == 2)).clip(lower=0, upper=3).astype(float),
    })
    info["puf_n_dep_raw_negative_before_clip"] = int(((puf.XTOT - 1 - (puf.MARS == 2)) < 0).sum())
    for k, v in puf_crosswalk(puf).items():
        df[k] = v.astype(float)
    for c in WEALTH + ["scf_weight"]:
        df[c] = np.nan
    df["wealth_observed"] = False
    return df, puf.part.to_numpy()


# ---------------------------------------------------------------------------
# Experiment A alterations (half 1 only, dollars, before ranking)
# ---------------------------------------------------------------------------

def alter(df, variant, info):
    h1 = df.half == 1
    for c in INC:
        df["orig_" + c] = df[c]
    if variant == "clean":
        return df
    rng = np.random.default_rng(SEED_ALTER)
    # Draws are per tax unit (household x unit), shared by its five implicates:
    # a reporting difference is a property of the respondent, not of the
    # imputation. Units are taken in sorted (hh_id, unit) order.
    key = df.loc[h1, ["hh_id", "unit"]].drop_duplicates().sort_values(["hh_id", "unit"]).reset_index(drop=True)
    if variant == "noise":
        for c in INC:
            key["z_" + c] = rng.normal(0, 0.3, len(key))
        m = df.loc[h1, ["hh_id", "unit"]].merge(key, on=["hh_id", "unit"], how="left")
        for c in INC:
            z = m["z_" + c].to_numpy()
            x = df.loc[h1, c].to_numpy()
            df.loc[h1, c] = np.where(x != 0, x * np.exp(z), 0.0)
        info["alteration"] = "half 1: each nonzero income amount multiplied by exp(N(0, 0.3^2)), one draw per tax unit and income, seed 20261010"
    elif variant == "recat":
        sel = h1 & (df.business > 0)
        move = 0.3 * df.loc[sel, "business"]
        df.loc[sel, "wages"] = df.loc[sel, "wages"] + move
        df.loc[sel, "business"] = df.loc[sel, "business"] - move
        info["alteration"] = ("half 1: for records with positive business income, 30% of it moved to wages "
                              "(deterministic; losses untouched)")
        info["alteration_records"] = int(sel.sum())
    elif variant == "unrep":
        key["u_id"] = rng.random(len(key))
        key["u_kg"] = rng.random(len(key))
        m = df.loc[h1, ["hh_id", "unit"]].merge(key, on=["hh_id", "unit"], how="left")
        x = df.loc[h1, "int_div"].to_numpy()
        z1 = (x != 0) & (m.u_id.to_numpy() < 0.25)
        df.loc[h1, "int_div"] = np.where(z1, 0.0, x)
        x = df.loc[h1, "capital_gains"].to_numpy()
        z2 = (x != 0) & (m.u_kg.to_numpy() < 0.25)
        df.loc[h1, "capital_gains"] = np.where(z2, 0.0, x)
        info["alteration"] = ("half 1: interest and dividends set to zero for units whose uniform draw is "
                              "below 0.25 among records holding them; capital gains likewise with a second, "
                              "independent draw; one draw per tax unit, seed 20261010")
        info["alteration_records_int_div_zeroed"] = int(z1.sum())
        info["alteration_records_capital_gains_zeroed"] = int(z2.sum())
    return df


# ---------------------------------------------------------------------------
# Splits
# ---------------------------------------------------------------------------

def household_split(hh, fracs, rng):
    """Assign each distinct household id to a part by fractions (dict)."""
    ids = np.sort(pd.unique(hh))
    perm = ids[rng.permutation(len(ids))]
    out, start = {}, 0
    names = list(fracs)
    for i, n in enumerate(names):
        k = len(ids) - start if i == len(names) - 1 else int(round(fracs[n] * len(ids)))
        for h in perm[start:start + k]:
            out[h] = n
        start += k
    return pd.Series(hh).map(out).to_numpy()


# ---------------------------------------------------------------------------
# Meta
# ---------------------------------------------------------------------------

def amount_var(name, group, has_neg):
    toks = []
    b = {}
    if has_neg:
        b["neg_token0"] = 0
        for j in range(len(tk.NEG_EDGES) - 1):
            toks.append({"token": j, "kind": "neg", "lo": float(tk.NEG_EDGES[j]), "hi": float(tk.NEG_EDGES[j + 1]),
                         "zero": False, "negative": True,
                         "meaning": f"negative value, weighted rank among negatives {tk.NEG_EDGES[j]:g}-{tk.NEG_EDGES[j+1]:g} (0 = most negative)"})
    else:
        b["neg_token0"] = None
    b["zero_token"] = len(toks)
    toks.append({"token": len(toks), "kind": "zero", "lo": 0.0, "hi": 0.0, "zero": True, "negative": False,
                 "meaning": "exactly zero"})
    b["pos_token0"] = len(toks)
    for j in range(len(tk.POS_EDGES) - 1):
        lo, hi = float(tk.POS_EDGES[j]), float(tk.POS_EDGES[j + 1])
        toks.append({"token": len(toks), "kind": "pos", "lo": lo, "hi": hi, "zero": False, "negative": False,
                     "meaning": f"positive value, weighted rank among positives {lo:g}-{hi:g}"})
    return {"name": name, "group": group, "kind": "amount", "vocab_size": len(toks), "binning": b,
            "tokens": toks, "description": DESC.get(name, f"SCF wealth component {name} (wealth_schema.R)")}


def build_meta(dataset, df, train_mask, sources, observe, info):
    variables = []
    variables.append({"name": "source", "group": "context", "kind": "categorical", "vocab_size": len(sources),
                      "tokens": [{"token": i, "value": s} for i, s in enumerate(sources)],
                      "description": DESC["source"]})
    variables.append({"name": "age_range", "group": "shared", "kind": "categorical", "vocab_size": 6,
                      "tokens": [{"token": i, "value": float(c), "meaning": AGE_LABELS[c]} for i, c in enumerate(range(1, 7))],
                      "description": DESC["age_range"]})
    variables.append({"name": "married", "group": "shared", "kind": "categorical", "vocab_size": 2,
                      "tokens": [{"token": 0, "value": 0.0, "meaning": "not married filing jointly"},
                                 {"token": 1, "value": 1.0, "meaning": "married filing jointly"}],
                      "description": DESC["married"]})
    variables.append({"name": "n_dep", "group": "shared", "kind": "categorical", "vocab_size": 4,
                      "tokens": [{"token": i, "value": float(i), "meaning": f"{i}" + (" or more" if i == 3 else "")} for i in range(4)],
                      "description": DESC["n_dep"]})
    for c in INC:
        variables.append(amount_var(c, "shared", bool((df[c] < 0).any())))
    variables.append({"name": "scf_weight", "group": "scf_only", "kind": "weight", "vocab_size": 64,
                      "tokens": [{"token": j, "kind": "weight", "lo": float(tk.WEIGHT_EDGES[j]), "hi": float(tk.WEIGHT_EDGES[j + 1]),
                                  "zero": False, "negative": False,
                                  "meaning": f"unweighted rank of the weight among the source-year's SCF records {tk.WEIGHT_EDGES[j]:.4f}-{tk.WEIGHT_EDGES[j+1]:.4f}"}
                                 for j in range(64)],
                      "description": DESC["scf_weight"]})
    # wealth in order of holding share among SCF training rows (most common first)
    scf_tr = df.loc[train_mask & (df.source == "SCF") & df.wealth_observed]
    hold = {c: float(np.sum(scf_tr.weight * (scf_tr[c] != 0)) / scf_tr.weight.sum()) for c in WEALTH}
    wealth_order = sorted(WEALTH, key=lambda c: -hold[c])
    if dataset == "expB_multi":
        # keep expB's column order so the two datasets' token files line up
        wealth_order = [n for n in tk.load_meta("expB")["generation_order"] if n != "scf_weight"]
    for c in wealth_order:
        v = amount_var(c, "scf_only", bool((df[c] < 0).any()))
        v["holding_share_scf_train"] = hold[c]
        variables.append(v)
    for i, v in enumerate(variables):
        v["column"] = i
        v["observed_in"] = [s for s in sources if v["group"] != "scf_only" or observe[s]]
    names = [v["name"] for v in variables]
    meta = {
        "dataset": dataset,
        "description": "Test 2 (one model across the PUF and the SCF), piece A: rank-token vocabularies. See SPEC.md and DATA_REPORT.md.",
        "created": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "sources": sources,
        "source_years": sorted({(s, int(y)) for s, y in zip(df.source, df.year)}),
        "observes_wealth": observe,
        "variables": variables,
        "variable_names": names,
        "generation_order": ["scf_weight"] + wealth_order,
        "generation_order_note": "default completion order: the weight, then wealth from most to least commonly held among SCF training records",
        "context_not_tokens": ["year (records file, a number)"],
        "missing_token": -1,
        "rank_grid": {"positive_edges": tk.POS_EDGES.tolist(), "negative_edges_most_negative_first": tk.NEG_EDGES.tolist(),
                      "weight_edges": "64 equal steps of the unweighted rank"},
        "rank_rules": ("Ranks are weighted (records' population weight) within source and year, among positive "
                       "values and among negative values separately, from each source-year's training records. "
                       "Ties are spread uniformly over the bins they span (seeded). Wealth and scf_weight of a "
                       "record whose source does not observe them are ranked and decoded with the SCF function of "
                       "the same year, or, for other years, two SCF waves interpolated in log dollars."),
        "seeds": {"split": SEED_SPLIT, "alteration": SEED_ALTER, "encode_ties": SEED_ENCODE, "roundtrip_decode": SEED_ROUNDTRIP},
        "counts": info,
    }
    return meta


def build_quantiles(df, train_mask, meta, observe, info):
    rows = []
    fall = []
    for var in meta["variables"]:
        name, kind = var["name"], var["kind"]
        if kind == "categorical":
            continue
        for (s, y), g in df.groupby(["source", "year"]):
            if var["group"] == "scf_only" and not observe[s]:
                continue
            gt = g.loc[train_mask[g.index]]
            sides = [("all", lambda x: np.ones(len(x), bool))] if kind == "weight" else \
                    [("pos", lambda x: x > 0), ("neg", lambda x: x < 0)]
            for side, f in sides:
                x, w = gt[name].to_numpy(), gt.weight.to_numpy()
                m = f(x)
                if kind == "weight":
                    w = np.ones(len(x))           # unweighted rank of the weight
                if not m.any():
                    xa = g[name].to_numpy()
                    ma = f(xa)
                    if not ma.any():
                        continue
                    fall.append(f"{s} {y} {name} {side}: no training values, built from all {int(ma.sum())} records")
                    x, w, m = xa, (np.ones(len(xa)) if kind == "weight" else g.weight.to_numpy()), ma
                t = tk.quantile_table(x[m], w[m], side)
                t.insert(0, "side", side)
                t.insert(0, "variable", name)
                t.insert(0, "year", int(y))
                t.insert(0, "source", s)
                t["weighted"] = kind != "weight"
                rows.append(t)
    info["quantile_fallbacks"] = fall
    return pd.concat(rows, ignore_index=True)


# ---------------------------------------------------------------------------
# Round trip
# ---------------------------------------------------------------------------

QS = [0.10, 0.25, 0.50, 0.75, 0.90, 0.99, 0.999]


def wq(x, w, qs):
    o = np.argsort(x, kind="stable")
    x, w = x[o], w[o]
    c = np.cumsum(w) / w.sum()
    return np.array([x[min(np.searchsorted(c, q), len(x) - 1)] for q in qs])


def roundtrip(df, truth, meta, q, mask):
    rec = df.loc[mask].reset_index(drop=True)
    dec = tk.decode(truth[mask], meta, rec, q, np.random.default_rng(SEED_ROUNDTRIP))
    rows = []
    for v in meta["variables"]:
        if v["kind"] == "categorical":
            continue
        n = v["name"]
        for (s, y), g in rec.groupby(["source", "year"]):
            a = g[n].to_numpy()
            b = dec.loc[g.index, n].to_numpy()
            ok = ~np.isnan(a)
            if not ok.any():
                continue
            a, b, w = a[ok], b[ok], g.weight.to_numpy()[ok]
            assert not np.isnan(b).any()
            qa, qb = wq(a, w, QS), wq(b, w, QS)
            r = {"variable": n, "source": s, "year": int(y), "records": int(ok.sum()),
                 "share_zero_true": np.sum(w * (a == 0)) / w.sum(), "share_zero_decoded": np.sum(w * (b == 0)) / w.sum(),
                 "share_neg_true": np.sum(w * (a < 0)) / w.sum(), "share_neg_decoded": np.sum(w * (b < 0)) / w.sum(),
                 "total_true": np.sum(w * a), "total_rel_err": (np.sum(w * b) - np.sum(w * a)) / abs(np.sum(w * a)) if np.sum(w * a) else np.nan,
                 "pos_total_rel_err": (np.sum(w * np.maximum(b, 0)) / np.sum(w * np.maximum(a, 0)) - 1) if np.sum(w * np.maximum(a, 0)) else np.nan,
                 "neg_total_rel_err": (np.sum(w * np.minimum(b, 0)) / np.sum(w * np.minimum(a, 0)) - 1) if np.sum(w * np.minimum(a, 0)) else np.nan}
            nz = a != 0
            # quantiles among nonzero holders (weighted), where most of the action is
            if nz.sum() > 10:
                qa2, qb2 = wq(a[nz], w[nz], QS), wq(b[nz], w[nz], QS)
                for qq, x1, x2 in zip(QS, qa2, qb2):
                    r[f"nonzero_q{qq}_rel_err"] = (x2 - x1) / abs(x1) if x1 != 0 else np.nan
            rows.append(r)
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Datasets
# ---------------------------------------------------------------------------

def build_dataset(dataset):
    out = os.path.join(S, "data", dataset)
    os.makedirs(out, exist_ok=True)
    marker = os.path.join(out, "DONE")
    if os.path.exists(marker):
        os.remove(marker)
    info = {}
    if dataset.startswith("expA"):
        variant = dataset.split("_")[1]
        df = load_scf(2016, info)
        rng = np.random.default_rng(SEED_SPLIT)
        df["half"] = np.where(household_split(df.hh_id, {"h1": 0.5, "h2": 0.5}, rng) == "h1", 1, 2)
        part = np.empty(len(df), dtype=object)
        for h in (1, 2):
            m = (df.half == h).to_numpy()
            part[m] = household_split(df.hh_id[m], {"train": 0.9, "val": 0.1}, rng)
        df.loc[df.half == 1, "source"] = "SCF_H1"
        df = alter(df, variant, info)
        sources = ["SCF_H1", "SCF"]
        observe = {"SCF_H1": False, "SCF": True}
        info["households_half1"] = int(df.hh_id[df.half == 1].nunique())
        info["households_half2"] = int(df.hh_id[df.half == 2].nunique())
    else:
        # expB_multi adds the 2019 and 2022 waves. The split generator runs
        # through the waves in this order, so 2013 and 2016 households get
        # exactly the same parts as in expB.
        waves = (2013, 2016, 2019, 2022) if dataset == "expB_multi" else (2013, 2016)
        scf = [load_scf(w, info) for w in waves]
        puf, ppart = load_puf(info)
        rng = np.random.default_rng(SEED_SPLIT)
        parts = [ppart]
        for d in scf:
            parts.append(household_split(d.hh_id, {"train": 0.7, "val": 0.1, "test": 0.2}, rng))
        df = pd.concat([puf] + scf, ignore_index=True)
        for c in INC:
            df["orig_" + c] = df[c]
        part = np.concatenate(parts)
        sources = ["PUF", "SCF"]
        observe = {"PUF": False, "SCF": True}
    df = df.reset_index(drop=True)
    train_mask = pd.Series(part == "train", index=df.index)
    info["records_by_part_and_source"] = {f"{p}|{s}|{y}": int(n) for (p, s, y), n in
                                          pd.DataFrame({"p": part, "s": df.source, "y": df.year}).value_counts().sort_index().items()}
    # households in more than one part (must be none)
    if dataset.startswith("expA") or True:
        sc = df.source != "PUF"
        hp = pd.DataFrame({"h": df.hh_id[sc].astype(str) + "_" + df.year[sc].astype(str), "p": part[sc.to_numpy()]}).drop_duplicates()
        assert hp.h.is_unique, "a household is in more than one part"

    meta = build_meta(dataset, df, train_mask, sources, observe, info)
    q = build_quantiles(df, train_mask, meta, observe, info)
    q.to_parquet(os.path.join(out, "quantiles.parquet"), index=False)
    q = tk.load_quantiles(dataset)
    cache = tk._qf_cache(q)

    names = meta["variable_names"]
    truth = tk.encode(df[["source", "year"] + names[1:]].assign(source=df.source), meta, q,
                      np.random.default_rng(SEED_ENCODE), cache=cache)
    # observation mask: scf_only variables hidden for sources that do not observe wealth
    tokens = truth.copy()
    for k, v in enumerate(meta["variables"]):
        if v["group"] == "scf_only":
            for s, ob in observe.items():
                if not ob:
                    tokens[(df.source == s).to_numpy(), k] = -1
    # unobserved shared variables (PUF age range outside the demographic file) are -1 in both
    assert ((truth == -1) == df[names[1:]].isna().reindex(columns=names[1:]).pipe(
        lambda x: np.column_stack([np.zeros(len(df), bool)] + [x[c].to_numpy() for c in names[1:]]))).all()

    rec_cols = ["source", "year", "hh_id", "implicate", "unit", "weight", "half", "age", "wealth_observed"] + INC + WEALTH + \
               [c for c in df.columns if c.startswith("orig_")]
    split_rows = {}
    if dataset.startswith("expA"):
        split_rows = {"train": np.flatnonzero(part == "train"), "val": np.flatnonzero(part == "val"),
                      "test": np.flatnonzero((df.half == 1).to_numpy())}
    else:
        split_rows = {p: np.flatnonzero(part == p) for p in ("train", "val", "test")}
    for p, idx in split_rows.items():
        np.save(os.path.join(out, f"tokens_{p}.npy"), tokens[idx])
        np.save(os.path.join(out, f"truth_{p}.npy"), truth[idx])
        df.loc[idx, rec_cols].reset_index(drop=True).to_parquet(os.path.join(out, f"records_{p}.parquet"), index=False)
        log(p, tokens[idx].shape, {s: int((df.source.iloc[idx] == s).sum()) for s in sources})
    meta["graded_rows"] = {p: {"rows": int(len(i)), "wealth_observed": int(df.wealth_observed.iloc[i].sum()),
                               "wealth_observed_scf_or_h1": int((df.wealth_observed.iloc[i] & (df.source.iloc[i] != "PUF")).sum()),
                               "split_off_no_truth": int(((df.source.iloc[i] != "PUF") & ~df.wealth_observed.iloc[i]).sum())}
                           for p, i in split_rows.items()}
    log("graded rows", meta["graded_rows"])
    meta["wealth_observed_rule"] = ("wealth is unobserved (token -1 in tokens and truth, NaN in records) for every PUF record "
                                    "and for SCF split-off units (unit >= 3), whose wealth the SCF does not measure; "
                                    "wealth ranks and quantile functions use units with wealth observed only")
    meta["split"] = {"sizes": {p: int(len(i)) for p, i in split_rows.items()},
                     "rule": ("experiment A: SCF 2016 filer households split 50/50 into halves; within each half "
                              "10% of households to val, the rest to train; test = all of half 1's rows (a copy)"
                              if dataset.startswith("expA") else
                              "SCF: per wave, households 70/10/20 to train/val/test, all implicates and units together; "
                              "PUF: test 1's split (seed 20261008), dependent filers removed")}
    with open(os.path.join(out, "meta.json"), "w") as f:
        json.dump(meta, f, indent=1, default=lambda o: o.item() if hasattr(o, "item") else str(o))

    # round trip on training rows (true tokens, including hidden wealth of half 1)
    rt = roundtrip(df, truth, meta, q, part == "train")
    # Decoding is random: repeat the totals with four more seeds to separate
    # systematic error from draw-to-draw noise.
    global SEED_ROUNDTRIP
    base = SEED_ROUNDTRIP
    tots = [rt.total_rel_err.to_numpy()]
    for i in range(1, 5):
        SEED_ROUNDTRIP = base + i
        tots.append(roundtrip(df, truth, meta, q, part == "train").total_rel_err.to_numpy())
    SEED_ROUNDTRIP = base
    tots = np.vstack(tots)
    rt["total_rel_err_mean_5_seeds"] = tots.mean(axis=0)
    rt["total_rel_err_sd_5_seeds"] = tots.std(axis=0, ddof=1)
    rt.to_csv(os.path.join(out, "roundtrip.csv"), index=False)
    pd.set_option("display.width", 250)
    pd.set_option("display.max_columns", 30)
    log(rt[["variable", "source", "year", "share_zero_true", "share_zero_decoded", "total_rel_err",
            "nonzero_q0.5_rel_err", "nonzero_q0.99_rel_err", "nonzero_q0.999_rel_err"]].to_string())
    # re-encode agreement
    rec = df.loc[part == "train"].reset_index(drop=True)
    dec = tk.decode(truth[part == "train"], meta, rec, q, np.random.default_rng(SEED_ROUNDTRIP), cache=cache)
    dec2 = rec[["source", "year"]].join(dec)
    re = tk.encode(dec2, meta, q, np.random.default_rng(SEED_ENCODE + 1), cache=cache)
    t0 = truth[part == "train"]
    agree = {v["name"]: float(np.mean(re[t0[:, k] >= 0, k] == t0[t0[:, k] >= 0, k])) for k, v in enumerate(meta["variables"])}
    log("re-encode agreement", agree)
    with open(os.path.join(out, "prep_summary.json"), "w") as f:
        json.dump({"counts": info, "split_sizes": meta["split"]["sizes"], "reencode_agreement": agree,
                   "roundtrip_max_abs_total_rel_err": float(rt.total_rel_err.abs().max())}, f, indent=1,
                  default=lambda o: o.item() if hasattr(o, "item") else str(o))
    log(dataset, "done", json.dumps(info, default=str)[:3000])


# ---------------------------------------------------------------------------
# Crosswalk table
# ---------------------------------------------------------------------------

CPI_U = {2012: 229.594, 2015: 237.017, 2018: 251.107}   # annual averages, BLS CPI-U


def crosswalk():
    info = {}
    puf = load_puf_raw()
    dep = puf.DSI == 1
    w = puf.S006 / 100
    rows = []
    cw = puf_crosswalk(puf)
    alt = {
        "ss_pens [alt: total pensions E01500 (includes rollovers) instead of taxable E01700]": puf.E01500 + puf.E02400,
        "ss_pens [alt: plus taxable IRA distributions E01400]": puf.E01500 + puf.E02400 + puf.E01400,
        "capital_gains [alt: 1040 line 13 E01000 + line 14 E01200, after the loss limit]": puf.E01000 + puf.E01200,
        "rent [check: sum of E25850 - E25860 + E26270 + E26390 - E26400]": puf.E25850 - puf.E25860 + puf.E26270 + puf.E26390 - puf.E26400,
        "ui_other [alt: plus taxable refunds E00700]": puf.E02300 + puf.E00800 + puf.E00700,
        "IRA distributions E01400 (in neither category)": puf.E01400,
    }
    for label, sel in (("PUF 2015, all non-aggregate returns", np.ones(len(puf), bool)),
                       ("PUF 2015, excluding dependent filers", ~dep.to_numpy())):
        for k, v in list(cw.items()) + list(alt.items()):
            x = v.to_numpy()[sel]
            ww = w.to_numpy()[sel]
            rows.append({"source": label, "category": k, "total_billions_2015usd": np.sum(ww * x) / 1e9,
                         "share_nonzero": np.sum(ww * (x != 0)) / ww.sum(), "share_negative": np.sum(ww * (x < 0)) / ww.sum(),
                         "units_millions": ww.sum() / 1e6})
    for wave in (2013, 2016, 2019):
        p = os.path.join(UNITS, f"scf_units_{wave}.csv.gz")
        if not os.path.exists(p):
            continue
        u = pd.read_csv(p)
        filer = u["filer"].astype(str).str.upper().isin(["TRUE", "1"]).to_numpy()
        f = CPI_U[2015] / CPI_U[wave - 1]
        for label, sel in ((f"SCF {wave} (tax year {wave-1}), all tax units", np.ones(len(u), bool)),
                           (f"SCF {wave} (tax year {wave-1}), filers", filer)):
            for k in INC + ["ui_other_broad_raw"]:
                x = u[k].to_numpy()[sel] * f
                ww = u.weight.to_numpy()[sel]
                rows.append({"source": label, "category": k, "total_billions_2015usd": np.sum(ww * x) / 1e9,
                             "share_nonzero": np.sum(ww * (x != 0)) / ww.sum(), "share_negative": np.sum(ww * (x < 0)) / ww.sum(),
                             "units_millions": ww.sum() / 1e6})
    t = pd.DataFrame(rows)
    t.to_csv(os.path.join(S, "data", "crosswalk_table.csv"), index=False)
    pd.set_option("display.width", 250)
    pd.set_option("display.max_colwidth", 90)
    log(t.to_string())
    # dependents and age distributions, PUF (non-dependent filers) vs SCF filers
    nd = (puf.XTOT - 1 - (puf.MARS == 2)).clip(lower=0, upper=3)
    sel = ~dep
    dist = []
    for k in range(4):
        dist.append({"what": f"n_dep={k}", "PUF": float(w[sel & (nd == k)].sum() / w[sel].sum())})
    for c in range(0, 7):
        dist.append({"what": f"AGERANGE={c}", "PUF": float(w[sel & (puf.AGERANGE == c)].sum() / w[sel & puf.AGERANGE.notna()].sum())})
    dist.append({"what": "married (MARS=2)", "PUF": float(w[sel & (puf.MARS == 2)].sum() / w[sel].sum())})
    dist.append({"what": "returns, millions", "PUF": float(w[sel].sum() / 1e6)})
    for wave in (2013, 2016):
        u = pd.read_csv(os.path.join(UNITS, f"scf_units_{wave}.csv.gz"))
        u = u.loc[u["filer"].astype(str).str.upper().isin(["TRUE", "1"])]
        uw = u.weight
        ar = age_range(u.age1)
        for d in dist:
            wh = d["what"]
            if wh.startswith("n_dep="):
                d[f"SCF{wave}"] = float(uw[u.n_dep.clip(upper=3) == int(wh[-1])].sum() / uw.sum())
            elif wh.startswith("AGERANGE="):
                d[f"SCF{wave}"] = float(uw[ar == int(wh[-1])].sum() / uw.sum())
            elif wh.startswith("married"):
                d[f"SCF{wave}"] = float(uw[u.filestat == 2].sum() / uw.sum())
            else:
                d[f"SCF{wave}"] = float(uw.sum() / 1e6)
    dd = pd.DataFrame(dist)
    dd.to_csv(os.path.join(S, "data", "crosswalk_demographics.csv"), index=False)
    log(dd.to_string())


if __name__ == "__main__":
    for ds in sys.argv[1:]:
        log("=====", ds)
        if ds == "crosswalk":
            crosswalk()
        else:
            build_dataset(ds)
