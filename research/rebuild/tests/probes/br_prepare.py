"""Probe BR (test 4): build `full_v3br`, a copy of test 3's `full_v2` in which
the bridge variables are observed in both sources:

  E19200 interest paid deduction and E18500 real estate taxes (and, with
  --charity, E19800 cash charity), observed on SCF filer units from the
  household items in $Q/scf_items/bridge_hh_<wave>.csv (br_bridge.R), and on
  PUF records only for itemizers (FDED = 1). A PUF non-itemizer's zero is not
  a measured zero, so for FDED 2 and 3 these variables are unobserved.
  SCF amounts are put on the PUF 2015 dollar scale: SCF 2013 amounts are
  raised by the CPI-U (2012 to 2015), and SCF records are ranked with the
  PUF 2015 quantile function (the SCF's own quantile tables for these
  variables are dropped), so a token means the same dollar amount in both
  sources. Everything else is g_prepare.py unchanged (--units g).

Original header (test 3, piece G): build the dataset `full` (see SPEC.md, contract entry
"Main session, 9 October 20:45", item 1, and g_crosswalk.md).

    python g_prepare.py [--units t2|g]

One token file over the 2015 PUF (test 1's 39 non-weight variables, test 1's
records and split) and the SCF 2013 and 2016 filer tax units (test 2's
builder and filing rule, all five implicates, test 2's household split).
The SCF observes the 24 wealth variables and the PUF variables of the
crosswalk; every other PUF variable is unobserved (token -1, NaN in the
records file) for SCF records, never zero. Amounts are rank tokens within
source and year (test 2's a_tokenizer grid, copied into g_tokenizer.py).

`--units t2` (default) reads test 2's unit files ($T2/data/scf_units/);
`--units g` reads piece G's rebuilt unit files ($P/g/scf_units/, from
g_build_units.R), which carry extra columns that let more PUF variables be
observed in the SCF (see g_crosswalk.md). The crosswalk used is recorded in
meta.json ("crosswalk").

Outputs in $T2/data/full/: meta.json, tokens_{train,val,test}.npy,
truth_{train,val,test}.npy (identical to tokens: nothing is hidden on
purpose), records_{train,val,test}.parquet, quantiles.parquet,
roundtrip.csv, checks_*.csv, prep_summary.json. The DONE marker is written
by hand after the log has been read (this script removes it).
"""

import argparse
import json
import os
import sys
from datetime import datetime, timezone

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "posttrain"))  # BR: g_tokenizer from test 3
import g_tokenizer as tk  # noqa: E402

T2 = tk.S
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/br"
OUT = os.path.join(Q, "data", "full_v3br")
BRIDGE_DIR = os.path.join(Q, "scf_items")
BRIDGE = ["E19200", "E18500"]      # --charity adds E19800
UNITS = {"t2": os.path.join(T2, "data", "scf_units"),
         "g": "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/g/scf_units"}
T1 = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data/"
EXPB = os.path.join(T2, "data", "expB")
PUF_RAW = "/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/"

DATASET_NAME = "full"     # set from --out in main()
SEED_ENCODE = 20261011
SEED_ROUNDTRIP = 20261012

WEALTH = ["cash", "equities", "bonds", "dc", "db", "life_ins", "annuities", "trusts",
          "other_fin", "pass_throughs", "primary_home", "other_home", "re_fund",
          "other_nonfin", "primary_mortgage", "other_mortgage", "credit_lines",
          "credit_cards", "installment_debt", "other_debt", "kg_primary_home",
          "kg_other_re", "kg_pass_throughs", "kg_other"]
WEALTH_FACTOR = {2013: 1.272833, 2016: 1.233371}      # SCFP files are in 2022 dollars (test 2)
INC7 = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens", "ui_other"]
CPI_U = {2012: 229.594, 2015: 237.017}
AGE_LABELS = {1: "under 26", 2: "26 to 34", 3: "35 to 44", 4: "45 to 54", 5: "55 to 64", 6: "65 and over"}
# PUF demographic-file codes that mean "not known" and become unobserved.
PUF_UNOBSERVED_CODES = {"AGERANGE": [-1, 0], "GENDER": [-1], "EARNSPLIT": [-1]}


def log(*a):
    print(*a, flush=True)


def age_range(age):
    return pd.cut(age, [-np.inf, 26, 35, 45, 55, 65, np.inf], right=False,
                  labels=[1, 2, 3, 4, 5, 6]).astype(float)


# ---------------------------------------------------------------------------
# PUF
# ---------------------------------------------------------------------------

def puf_variables():
    with open(T1 + "meta.json") as f:
        m = json.load(f)
    vs = [v for v in m["variables"] if v["kind"] != "weight"]
    assert len(vs) == 39
    return [(v["name"], v["kind"], v["description"]) for v in vs]


def load_puf(names, info):
    parts = []
    for part in ["train", "val", "test"]:
        r = pd.read_parquet(T1 + f"values_{part}.parquet")
        r["part"] = part
        parts.append(r)
    p = pd.concat(parts, ignore_index=True)
    assert p.RECID.is_unique
    info["puf_records"] = int(len(p))
    info["puf_records_by_part"] = {k: int(v) for k, v in p.part.value_counts().items()}
    na = {c: int(p[c].isna().sum()) for c in ["weight"] + [n for n in names if n != "E02000"]}
    if any(na.values()):
        raise ValueError(f"NA in test 1 PUF values: { {k: v for k, v in na.items() if v} }")
    df = pd.DataFrame({"source": "PUF", "year": 2015, "hh_id": p.RECID.astype(np.int64),
                       "implicate": 0, "unit": 0, "weight": p.weight.astype(float), "half": 0,
                       "age": np.nan, "wealth_observed": False})
    for c in names:
        if c == "E02000":
            continue                       # not in test 1's files; set from the raw PUF below
        x = p[c].astype(float)
        if c in PUF_UNOBSERVED_CODES:
            bad = x.isin(PUF_UNOBSERVED_CODES[c])
            info[f"puf_{c}_unobserved_codes_{PUF_UNOBSERVED_CODES[c]}"] = int(bad.sum())
            x = x.where(~bad)
        df[c] = x.to_numpy()
    for c in WEALTH:
        df[c] = np.nan
    # Test 2's seven income categories (a_crosswalk.md), for comparisons only.
    raw = pd.read_csv(PUF_RAW + "puf_2015.csv", usecols=["RECID", "E02000", "E01200", "E00800"])
    raw = p[["RECID"]].merge(raw, on="RECID", how="left", validate="one_to_one")
    assert not raw.isna().any().any()
    df["t2_wages"] = p.E00200.to_numpy()
    df["t2_business"] = (p.E00900 + p.E02100).to_numpy()
    df["t2_int_div"] = (p.E00300 + p.E00400 + p.E00600).to_numpy()
    df["t2_capital_gains"] = (p.P22250 + p.P23250 + p.E01100).to_numpy() + raw.E01200.to_numpy()
    df["t2_rent"] = raw.E02000.to_numpy()
    if "E02000" in names:
        df["E02000"] = raw.E02000.astype(float).to_numpy()
        info["puf_E02000_from_raw_puf"] = True
    # BR-CHANGE: bridge variables observed only for itemizers.
    nonitem = (p.FDED.to_numpy() != 1)
    for c in BRIDGE:
        info[f"puf_{c}_nonitemizer_rows_set_unobserved"] = int(nonitem.sum())
        info[f"puf_{c}_nonitemizer_rows_nonzero_before"] = int((nonitem & (df[c].to_numpy() != 0)).sum())
        df.loc[nonitem, c] = np.nan
    df["t2_ss_pens"] = (p.E01700 + p.E02400).to_numpy()
    df["t2_ui_other"] = p.E02300.to_numpy() + raw.E00800.to_numpy()
    return df, p.part.to_numpy()


# ---------------------------------------------------------------------------
# SCF
# ---------------------------------------------------------------------------

def scf_crosswalk(u, wave, info, units_kind):
    """PUF variables measured in the SCF, per filer tax unit (g_crosswalk.md).
    Returns a dict name -> float array (NaN = unobserved for that unit) and a
    dict name -> one-line definition."""
    n = len(u)
    peu = (u.taxunit_code <= 2).to_numpy()
    npeu = ~peu
    se = u.share_earn.to_numpy()
    out, how = {}, {}

    def item(c):
        x = u[c].to_numpy(np.float64).copy()
        neg = x < 0
        if neg.any():
            if not np.all(x[neg] == -1):
                raise ValueError(f"SCF {wave} {c}: negative values other than -1: {np.unique(x[neg])[:10]}")
            info[f"scf{wave}_{c}_minus1_set_to_0_rows"] = int(neg.sum())
            x[neg] = 0.0
        return x

    fs = u.filestat.to_numpy(np.float64)
    if np.isnan(fs).any() or not np.isin(fs, [1, 2, 3, 4]).all():
        raise ValueError(f"SCF {wave}: filestat values {np.unique(fs)}")
    out["MARS"] = fs
    how["MARS"] = "builder filing status (1 single, 2 joint, 3 separate, 4 head of household)"
    out["DSI"] = np.zeros(n)
    how["DSI"] = "0 for every unit: the builder never makes a financially dependent person a tax unit"
    nd = u.n_dep.to_numpy(np.float64)
    out["XTOT"] = np.minimum(1 + (fs == 2) + nd, 5)
    how["XTOT"] = "1 + 1 if joint + builder dependents (children under 19 and support of a child elsewhere), top-coded at 5 as in the PUF"
    out["AGERANGE"] = age_range(u.age1).to_numpy()
    how["AGERANGE"] = "age of the unit's head at the interview, in the PUF's six ranges (codes 1-6)"
    out["E00200"] = u.wages.to_numpy(np.float64)
    how["E00200"] = "X5702 wages, split by own wages; plus split-off unit's wages"
    out["E00900"] = u.business.to_numpy(np.float64)
    how["E00900"] = "X5704 Schedule C and F combined (the SCF does not separate farm), split by own business income; plus split-off unit's business income"
    out["P23250"] = u.capital_gains.to_numpy(np.float64)
    how["P23250"] = "X5712 net capital gain or loss (all holding periods; capital gain distributions included), split by earnings share; plus split-off unit's"

    x5706, x5708, x5710, x5716 = item("hh_X5706"), item("hh_X5708"), item("hh_X5710"), item("hh_X5716")
    e300 = np.where(peu, x5708 * se, np.nan)
    e400 = np.where(peu, x5706 * se, np.nan)
    e600 = np.where(peu, x5710 * se, np.nan)
    e2300 = np.where(peu, x5716 * se, np.nan)
    # Units of the main household: the three items must add up to the unit's int_div.
    d = np.abs((x5706 + x5708 + x5710) * se - u.int_div.to_numpy())[peu]
    if d.max() > 0.01:
        raise ValueError(f"SCF {wave}: interest and dividend items do not add to int_div (max diff {d.max()})")
    if units_kind == "g":
        e300[npeu] = u.npeu_intinc.to_numpy(np.float64)[npeu]
        e400[npeu] = 0.0
        e600[npeu] = u.npeu_divinc.to_numpy(np.float64)[npeu]
        e2300[npeu] = u.npeu_unempinc.to_numpy(np.float64)[npeu]
        how["E00300"] = "X5708 taxable interest, split by earnings share; split-off unit: its interest"
        how["E00400"] = "X5706 tax-exempt interest, split by earnings share; split-off unit: 0 (not asked)"
        how["E00600"] = "X5710 dividends, split by earnings share; split-off unit: its dividends"
    else:
        # Split-off units report interest and dividends as one flagged type in
        # test 2's unit file; where they have any, which item is unknown, so
        # all three are unobserved for those units (counted).
        idv = u.int_div.to_numpy(np.float64)
        known = npeu & (idv == 0)
        unknown = npeu & (idv != 0)
        for a in (e300, e400, e600):
            a[known] = 0.0
        info[f"scf{wave}_split_off_rows_int_div_nonzero_items_unobserved"] = int(unknown.sum())
        how["E00300"] = "X5708 taxable interest, split by earnings share; split-off unit: 0 if it has no interest or dividends, else unobserved"
        how["E00400"] = "X5706 tax-exempt interest, split by earnings share; split-off unit: as E00300"
        how["E00600"] = "X5710 dividends, split by earnings share; split-off unit: as E00300"
        # Split-off units' ui_other is their unemployment compensation only.
        e2300[npeu] = u.ui_other.to_numpy(np.float64)[npeu]
    how["E02300"] = "X5716 unemployment and workers' compensation, split by earnings share; plus split-off unit's unemployment"
    out.update({"E00300": e300, "E00400": e400, "E00600": e600, "E02300": e2300})
    if units_kind == "g":
        # John, 9 October (option b): total Schedule E as a 40th PUF-side
        # variable observed in both sources, test 2's `rent`.
        out["E02000"] = u.rent.to_numpy(np.float64)
        how["E02000"] = ("X5714 net rent, royalties, partnerships, S corporations, estates and trusts (Schedule E, "
                         "line 17), split by earnings share, -9 masked negatives kept as small losses (test 2's "
                         "`rent`); plus split-off unit's rent income")

    if units_kind == "g":
        n17 = u.n_u17.to_numpy(np.float64)
        info[f"scf{wave}_n_u17_above_n_dep_rows"] = int((n17 > nd).sum())
        out["N24"] = np.minimum(np.minimum(n17, nd), 3)
        how["N24"] = ("children under 17 in the unit (builder KIDSU17, at most the unit's dependents), "
                      "top-coded at 3 as in the PUF; split-off units 0")
        g = u.male1.to_numpy(np.float64)
        info[f"scf{wave}_gender_unobserved_rows"] = int(np.isnan(g).sum())
        out["GENDER"] = np.where(np.isnan(g), np.nan, np.where(g == 1, 1.0, 2.0))
        how["GENDER"] = ("sex of the unit's head (1 male, 2 female; X8021, or X103 for the spouse's separate unit); "
                         "split-off units unobserved")
        out["E02400"] = u.ss_gross.to_numpy(np.float64)
        how["E02400"] = ("current Social Security benefit annualized (X5306/X5307 respondent, X5311/X5312 "
                         "spouse), each person's to their own unit; split-off units: their Social Security")
        out["E01700"] = u.pens_total.to_numpy(np.float64)
        how["E01700"] = ("X5722 (Social Security, pensions, annuities) less the household's annualized current "
                         "Social Security, floored at 0, split by earnings share; split-off units: their pension "
                         "and disability income")
        # Schedule C or F: X5747 / X5749 (household filed Schedule C / F; 1 yes,
        # 5 no, 0 not asked). Farm-only households' business amount is farm
        # income; households filing both keep it in E00900 with farm
        # unobserved; split-off units: farm 0 without business income, else
        # unobserved.
        bus = u.business.to_numpy(np.float64)
        schc, schf = u.g_schc.to_numpy(np.float64), u.g_schf.to_numpy(np.float64)
        farm_only = peu & (schf == 1) & (schc != 1)
        both = peu & (schf == 1) & (schc == 1)
        e900 = np.where(farm_only, 0.0, bus)
        e2100 = np.where(farm_only, bus, 0.0)
        e2100[both] = np.nan
        e2100[npeu & (bus != 0)] = np.nan
        out["E00900"], out["E02100"] = e900, e2100
        info[f"scf{wave}_farm_only_rows"] = int(farm_only.sum())
        info[f"scf{wave}_farm_only_rows_business_nonzero"] = int((farm_only & (bus != 0)).sum())
        info[f"scf{wave}_both_C_and_F_rows_farm_unobserved"] = int(both.sum())
        info[f"scf{wave}_split_off_business_nonzero_farm_unobserved"] = int((npeu & (bus != 0)).sum())
        how["E00900"] = ("X5704 Schedule C and F, split by own business income; 0 for households that filed "
                         "Schedule F but not C (X5749, X5747); plus split-off unit's business income")
        how["E02100"] = ("X5704 for households that filed Schedule F but not C, else 0; unobserved for households "
                         "filing both, and for split-off units with business income")
        # BR-CHANGE: bridge variables from the household items (br_bridge.R).
        b = pd.read_csv(os.path.join(BRIDGE_DIR, f"bridge_hh_{wave}.csv"))
        y1 = (u.taxunitid // 100).astype(np.int64).to_numpy()
        bb = pd.DataFrame({"y1": y1}).merge(b, on="y1", how="left", validate="many_to_one")
        if bb[["mortint_ded", "retax"]].isna().any().any():
            raise ValueError(f"SCF {wave}: units without household bridge items")
        f = CPI_U[2015] / CPI_U[2012] if wave == 2013 else 1.0
        sh = u.share_housing.to_numpy(np.float64)
        out["E19200"] = np.where(peu, bb.mortint_ded.to_numpy() * sh * f, 0.0)
        out["E18500"] = np.where(peu, bb.retax.to_numpy() * sh * f, 0.0)
        how["E19200"] = ("mortgage interest on the home and on vacation homes, balance times the reported annual "
                         "rate summed over loans (first, second, third mortgage X805/X816, X905/X916, X1005/X1016; "
                         "other purchase loans; home-equity lines in use; vacation-home loans), limited as 2015 law "
                         "limited it (interest on at most $1 million of purchase debt plus $100,000 of home-equity "
                         "debt), split as housing (half each to a split couple); split-off units 0; SCF 2013 "
                         "raised to 2015 dollars; ranked on the PUF 2015 itemizers' scale")
        how["E18500"] = ("X721/X722 real estate taxes on the home, annualized, split as housing; split-off units 0; "
                         "taxes on other real estate are not asked; ranked on the PUF 2015 itemizers' scale")
        if "E19800" in BRIDGE:
            ch = bb.charity.to_numpy() * se * f
            out["E19800"] = np.where(peu, ch, np.nan)
            how["E19800"] = ("X5822/X5823 charitable contributions of the family (money, property or assets), split "
                             "by earnings share; split-off units unobserved; ranked on the PUF 2015 itemizers' scale "
                             "for cash contributions")
        for c in BRIDGE:
            info[f"scf{wave}_{c}_weighted_nonzero_share"] = float(np.sum(u.weight * (out[c] > 0)) / u.weight.sum())
            info[f"scf{wave}_{c}_weighted_total_bn"] = float(np.nansum(u.weight * out[c]) / 1e9)
        for c in ("N24", "E02400", "E01700"):
            if np.isnan(out[c]).any():
                raise ValueError(f"SCF {wave} {c}: {int(np.isnan(out[c]).sum())} NA")
    return out, how


def load_scf(wave, names, info, units_kind):
    u = pd.read_csv(os.path.join(UNITS[units_kind], f"scf_units_{wave}.csv.gz"))
    filer = u["filer"].astype(str).str.upper().isin(["TRUE", "1"])
    info[f"scf{wave}_unit_rows"] = int(len(u))
    info[f"scf{wave}_nonfiler_rows_dropped"] = int((~filer).sum())
    u = u.loc[filer].reset_index(drop=True)
    used = ["age1", "n_dep", "filestat", "weight", "share_earn", "taxunit_code"] + INC7 + WEALTH + \
           ["hh_X5706", "hh_X5708", "hh_X5710", "hh_X5716", "hh_X5718", "share_housing", "taxunitid"]
    if units_kind == "g":
        used += ["n_u17", "ss_gross", "pens_total", "npeu_intinc", "npeu_divinc", "npeu_unempinc"]
        bad_sex = u.male1.isna() & (u.taxunit_code <= 2)
        if bad_sex.any():
            raise ValueError(f"SCF {wave}: {int(bad_sex.sum())} main-household units without sex")
    na = {c: int(u[c].isna().sum()) for c in used}
    if any(na.values()):
        raise ValueError(f"NA in SCF {wave}: { {k: v for k, v in na.items() if v} }")
    df = pd.DataFrame({"source": "SCF", "year": wave, "hh_id": u.scf_hh_id.astype(np.int64),
                       "implicate": u.implicate.astype(np.int64), "unit": u.taxunit_code.astype(np.int64),
                       "weight": u.weight.astype(float), "half": 0, "age": u.age1.astype(float)})
    split_off = (u.taxunit_code >= 3).to_numpy()
    df["wealth_observed"] = ~split_off
    cw, how = scf_crosswalk(u, wave, info, units_kind)
    for c in names:
        df[c] = cw[c] if c in cw else np.nan
    for c in WEALTH:
        if (u.loc[split_off, c] != 0).any():
            raise ValueError(f"split-off units with nonzero {c} in SCF {wave}")
        df[c] = np.where(split_off, np.nan, u[c].astype(float) / WEALTH_FACTOR[wave])
    neg = (df["db"] < 0).to_numpy()
    if neg.any():
        raise ValueError(f"SCF {wave}: negative db values")
    for c in INC7:
        df["t2_" + c] = u[c].astype(float).to_numpy()
    info[f"scf{wave}_filer_rows"] = int(len(df))
    info[f"scf{wave}_split_off_filer_rows_wealth_unobserved"] = int(split_off.sum())
    return df, how


# ---------------------------------------------------------------------------
# Meta
# ---------------------------------------------------------------------------

def amount_var(name, group, has_neg, desc):
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
            "tokens": toks, "description": desc}


def build_meta(df, train_mask, pufvars, scf_obs, how, info):
    sources = ["PUF", "SCF"]
    variables = [{"name": "source", "group": "context", "kind": "categorical", "vocab_size": 2,
                  "tokens": [{"token": i, "value": s} for i, s in enumerate(sources)],
                  "description": "Source of the record (context, always observed)"}]
    for name, kind, desc in pufvars:
        group = "shared" if name in scf_obs else "puf_only"
        if kind == "categorical":
            vals = sorted(pd.unique(df[name].dropna()))
            toks = []
            for i, v in enumerate(vals):
                t = {"token": i, "value": float(v)}
                if name == "AGERANGE":
                    t["meaning"] = AGE_LABELS.get(int(v), str(v)) + " (codes 1-3 mean other ranges for dependent filers, DSI = 1)"
                toks.append(t)
            v = {"name": name, "group": group, "kind": "categorical", "vocab_size": len(toks), "tokens": toks,
                 "description": desc}
        else:
            v = amount_var(name, group, bool((df[name] < 0).any()), desc)
        v["scf_definition"] = how.get(name, "not measured by the SCF: unobserved (token -1) for SCF records")
        variables.append(v)
    with open(os.path.join(EXPB, "meta.json")) as f:
        wealth_order = [n for n in json.load(f)["generation_order"] if n != "scf_weight"]
    assert sorted(wealth_order) == sorted(WEALTH)
    scf_tr = df.loc[train_mask & (df.source == "SCF") & df.wealth_observed]
    for c in wealth_order:
        v = amount_var(c, "scf_only", bool((df[c] < 0).any()), f"SCF wealth component {c} (wealth_schema.R), survey-year dollars")
        v["holding_share_scf_train"] = float(np.sum(scf_tr.weight * (scf_tr[c] != 0)) / scf_tr.weight.sum())
        variables.append(v)
    for i, v in enumerate(variables):
        v["column"] = i
        v["observed_in"] = ["PUF"] * (v["group"] != "scf_only") + ["SCF"] * (v["group"] != "puf_only")
    names = [v["name"] for v in variables]
    meta = {
        "dataset": DATASET_NAME,
        "description": ("Test 3 (post-training), piece G: test 1's 39 PUF variables"
                        + (" plus total Schedule E (E02000)" if any(n == "E02000" for n, _, _ in pufvars) else "")
                        + " plus the 24 SCF wealth "
                        "variables, PUF 2015 and SCF 2013/2016 filer tax units, rank tokens within source and year. "
                        "See research/rebuild/tests/posttrain/G_DATA_REPORT.md and g_crosswalk.md."),
        "created": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "sources": sources,
        "source_years": sorted({(s, int(y)) for s, y in zip(df.source, df.year)}),
        "observes_wealth": {"PUF": False, "SCF": True},
        "variables": variables,
        "variable_names": names,
        "generation_order": [n for n, _, _ in pufvars] + wealth_order,
        "generation_order_note": ("test 1's PUF order (categoricals, AGI, incomes, adjustments, itemized deductions), "
                                  "then the 24 wealth variables in test 2's expB order (most to least commonly held)"),
        "groups_note": ("shared: a PUF variable the SCF also measures (g_crosswalk.md); puf_only: a PUF variable "
                        "the SCF does not measure (always -1 for SCF records); scf_only: SCF wealth (always -1 for "
                        "PUF records and for SCF split-off units)"),
        "context_not_tokens": ["year (records file, a number)"],
        "record_weight": "records_*.parquet column `weight` (PUF: S006/100; SCF: implicate weight); not a model variable",
        "missing_token": -1,
        "rank_grid": {"positive_edges": tk.POS_EDGES.tolist(),
                      "negative_edges_most_negative_first": tk.NEG_EDGES.tolist()},
        "rank_rules": ("Ranks are weighted (records' population weight) within source and year, among positive "
                       "values and among negative values separately, from each source-year's training records. "
                       "Ties are spread uniformly over the bins they span (seeded). Decoding: g_tokenizer.decode; a "
                       "variable a record's source does not observe is decoded with the SCF function (wealth; 2015 "
                       "interpolates 2013 and 2016 in log dollars) or the PUF 2015 function (PUF-only variables)."),
        "seeds": {"split": "test 1 (PUF, 20261008) and test 2 expB (SCF, 20261009), reused",
                  "encode_ties": SEED_ENCODE, "roundtrip_decode": SEED_ROUNDTRIP},
        "counts": info,
    }
    return meta


def build_quantiles(df, train_mask, meta, info):
    rows, fall = [], []
    for var in meta["variables"]:
        name, kind = var["name"], var["kind"]
        if kind != "amount":
            continue
        for (s, y), g in df.groupby(["source", "year"]):
            gt = g.loc[train_mask[g.index]]
            for side, f in (("pos", lambda x: x > 0), ("neg", lambda x: x < 0)):
                x, w = gt[name].to_numpy(), gt.weight.to_numpy()
                m = f(x)                                      # NaN compares False
                if not m.any():
                    xa = g[name].to_numpy()
                    ma = f(xa)
                    if not ma.any():
                        continue
                    fall.append(f"{s} {y} {name} {side}: no training values, built from all {int(ma.sum())} records")
                    x, w, m = xa, g.weight.to_numpy(), ma
                t = tk.quantile_table(x[m], w[m], side)
                t.insert(0, "side", side)
                t.insert(0, "variable", name)
                t.insert(0, "year", int(y))
                t.insert(0, "source", s)
                t["weighted"] = True
                rows.append(t)
    info["quantile_fallbacks"] = fall
    q = pd.concat(rows, ignore_index=True)
    # BR-CHANGE: SCF records' bridge variables are ranked on the PUF 2015 scale.
    drop = (q.source == "SCF") & q.variable.isin(BRIDGE)
    info["bridge_scf_quantile_rows_dropped"] = int(drop.sum())
    return q.loc[~drop].reset_index(drop=True)


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

QS = [0.10, 0.25, 0.50, 0.75, 0.90, 0.99, 0.999]


def wq(x, w, qs):
    o = np.argsort(x, kind="stable")
    x, w = x[o], w[o]
    c = np.cumsum(w) / w.sum()
    return np.array([x[min(np.searchsorted(c, q), len(x) - 1)] for q in qs])


def roundtrip(df, truth, meta, q, mask, seed, cache):
    rec = df.loc[mask].reset_index(drop=True)
    dec = tk.decode(truth[mask], meta, rec, q, np.random.default_rng(seed), cache=cache)
    rows = []
    for v in meta["variables"]:
        if v["kind"] != "amount":
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
            r = {"variable": n, "source": s, "year": int(y), "records": int(ok.sum()),
                 "share_zero_true": np.sum(w * (a == 0)) / w.sum(), "share_zero_decoded": np.sum(w * (b == 0)) / w.sum(),
                 "share_neg_true": np.sum(w * (a < 0)) / w.sum(), "share_neg_decoded": np.sum(w * (b < 0)) / w.sum(),
                 "total_true": np.sum(w * a),
                 "total_rel_err": (np.sum(w * b) - np.sum(w * a)) / abs(np.sum(w * a)) if np.sum(w * a) else np.nan}
            nz = a != 0
            if nz.sum() > 10:
                qa2, qb2 = wq(a[nz], w[nz], QS), wq(b[nz], w[nz], QS)
                for qq, x1, x2 in zip(QS, qa2, qb2):
                    r[f"nonzero_q{qq}_rel_err"] = (x2 - x1) / abs(x1) if x1 != 0 else np.nan
            rows.append(r)
    return pd.DataFrame(rows)


def population_checks(df, meta):
    """Weighted totals, nonzero and negative shares (amounts) and category
    shares (categoricals), all parts together: PUF all returns, PUF without
    dependent filers, SCF filer units by wave (all filers; wealth-observed
    units only for wealth). Dollars in each source-year's own dollars, plus
    SCF 2013 incomes in 2015 dollars by CPI-U."""
    pops = [("PUF 2015, all returns", (df.source == "PUF")),
            ("PUF 2015, no dependent filers", (df.source == "PUF") & (df.DSI != 1)),
            ("SCF 2013 filers", (df.source == "SCF") & (df.year == 2013)),
            ("SCF 2016 filers", (df.source == "SCF") & (df.year == 2016))]
    amt, cat = [], []
    for v in meta["variables"]:
        n = v["name"]
        if v["group"] == "context":
            continue
        for label, m in pops:
            g = df.loc[m]
            x = g[n].to_numpy(np.float64)
            ok = ~np.isnan(x)
            if not ok.any():
                continue
            w = g.weight.to_numpy()[ok]
            x = x[ok]
            if v["kind"] == "amount":
                f = 1.0
                if label.startswith("SCF 2013") and v["group"] != "scf_only":
                    f = CPI_U[2015] / CPI_U[2012]
                amt.append({"variable": n, "group": v["group"], "population": label,
                            "units_observed_millions": w.sum() / 1e6,
                            "total_billions": np.sum(w * x) / 1e9,
                            "total_billions_2015_dollars_incomes": np.sum(w * x) * f / 1e9,
                            "share_nonzero": np.sum(w * (x != 0)) / w.sum(),
                            "share_negative": np.sum(w * (x < 0)) / w.sum(),
                            "mean_among_nonzero": np.sum(w * x) / max(np.sum(w * (x != 0)), 1e-9)})
            else:
                for val in sorted(np.unique(x)):
                    cat.append({"variable": n, "group": v["group"], "population": label, "value": val,
                                "share": np.sum(w * (x == val)) / w.sum(), "units_observed_millions": w.sum() / 1e6})
    return pd.DataFrame(amt), pd.DataFrame(cat)


def observed_counts(df, tokens, meta, part):
    rows = []
    for k, v in enumerate(meta["variables"]):
        if v["group"] == "context":
            continue
        for (s, y), idx in df.groupby(["source", "year"]).groups.items():
            ii = np.asarray(idx)
            t = tokens[ii, k]
            w = df.weight.to_numpy()[ii]
            for p in ("train", "val", "test"):
                pm = part[ii] == p
                rows.append({"variable": v["name"], "group": v["group"], "source": s, "year": int(y), "part": p,
                             "rows": int(pm.sum()), "observed": int((t[pm] >= 0).sum()),
                             "unobserved": int((t[pm] < 0).sum()),
                             "weight_share_observed": float(np.sum(w[pm] * (t[pm] >= 0)) / max(np.sum(w[pm]), 1e-9))})
    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--units", choices=["t2", "g"], default="t2")
    ap.add_argument("--out", default=OUT)
    ap.add_argument("--charity", action="store_true", help="also bridge E19800 (cash charity)")
    a = ap.parse_args()
    if a.charity:
        BRIDGE.append("E19800")
    if a.units != "g":
        raise ValueError("probe BR builds on --units g only")
    out = a.out
    global DATASET_NAME
    DATASET_NAME = os.path.basename(os.path.normpath(out))
    os.makedirs(out, exist_ok=True)
    marker = os.path.join(out, "DONE")
    if os.path.exists(marker):
        os.remove(marker)
    info = {"units_files": UNITS[a.units], "bridge_variables": list(BRIDGE)}
    pufvars = puf_variables()
    if a.units == "g":
        # John, 9 October: total Schedule E (E02000, test 2's `rent`) added as a
        # 40th PUF-side variable, observed in both sources; placed after
        # Schedule C, before its five separate pieces (which stay PUF-only).
        k = [n for n, _, _ in pufvars].index("E00900") + 1
        pufvars.insert(k, ("E02000", "amount", "Schedule E net income or loss (rent, royalties, partnerships, "
                                               "S corporations, estates, trusts; 1040 line 17)"))
    names = [n for n, _, _ in pufvars]

    puf, ppart = load_puf(names, info)
    scf, hows = [], {}
    for wave in (2013, 2016):
        d, how = load_scf(wave, names, info, a.units)
        scf.append(d)
        hows.update(how)
    scf_obs = sorted(hows)
    info["scf_observed_puf_variables"] = scf_obs

    # SCF split: test 2's expB household split, reused.
    m = []
    for p in ("train", "val", "test"):
        r = pd.read_parquet(os.path.join(EXPB, f"records_{p}.parquet"), columns=["source", "year", "hh_id"])
        r = r.loc[r.source == "SCF", ["year", "hh_id"]].drop_duplicates()
        r["part"] = p
        m.append(r)
    m = pd.concat(m)
    assert not m.duplicated(["year", "hh_id"]).any()
    sp = pd.concat(scf, ignore_index=True)[["year", "hh_id"]].merge(m, on=["year", "hh_id"], how="left")
    if sp.part.isna().any():
        raise ValueError(f"{int(sp.part.isna().sum())} SCF filer rows whose household is not in expB")
    spart = sp.part.to_numpy()

    df = pd.concat([puf] + scf, ignore_index=True)
    part = np.concatenate([ppart, spart])
    train_mask = pd.Series(part == "train", index=df.index)
    info["records_by_part_and_source"] = {f"{p}|{s}|{y}": int(n) for (p, s, y), n in
                                          pd.DataFrame({"p": part, "s": df.source, "y": df.year}).value_counts().sort_index().items()}
    # SCF rows per part must equal expB's.
    for p in ("train", "val", "test"):
        r = pd.read_parquet(os.path.join(EXPB, f"records_{p}.parquet"), columns=["source"])
        n_expb = int((r.source == "SCF").sum())
        n_here = int(((df.source == "SCF").to_numpy() & (part == p)).sum())
        info[f"scf_rows_{p}_expB_vs_full"] = [n_expb, n_here]
        if n_expb != n_here:
            raise ValueError(f"SCF rows in {p}: expB {n_expb}, full {n_here}")

    meta = build_meta(df, train_mask, pufvars, scf_obs, hows, info)
    meta["crosswalk"] = {n: hows[n] for n in scf_obs}
    q = build_quantiles(df, train_mask, meta, info)
    q.to_parquet(os.path.join(out, "quantiles.parquet"), index=False)
    q = pd.read_parquet(os.path.join(out, "quantiles.parquet"))
    cache = tk._qf_cache(q)

    vnames = meta["variable_names"]
    truth = tk.encode(df[["source", "year"] + vnames[1:]], meta, q, np.random.default_rng(SEED_ENCODE), cache=cache)
    nan = np.column_stack([np.zeros(len(df), bool)] + [df[c].isna().to_numpy() for c in vnames[1:]])
    assert ((truth == -1) == nan).all(), "token -1 does not coincide with NaN"
    tokens = truth.copy()

    # Unobserved by design, per source: everything else must be observed.
    na_rows = []
    for k, v in enumerate(meta["variables"][1:], start=1):
        for s in ("PUF", "SCF"):
            sm = (df.source == s).to_numpy()
            n_na = int(nan[sm, k].sum())
            if s not in v["observed_in"]:
                expected = int(sm.sum())
            elif v["group"] == "scf_only":
                expected = int((sm & ~df.wealth_observed.to_numpy()).sum())
            else:
                expected = None                               # listed below
            na_rows.append({"variable": v["name"], "source": s, "rows": int(sm.sum()), "unobserved": n_na,
                            "unobserved_by_design_rule": expected})
    na_tab = pd.DataFrame(na_rows)
    na_tab.to_csv(os.path.join(out, "checks_unobserved_by_source.csv"), index=False)
    bad = na_tab.loc[na_tab.unobserved_by_design_rule.notna() &
                     (na_tab.unobserved != na_tab.unobserved_by_design_rule)]
    if len(bad):
        raise ValueError(f"unobserved counts differ from the design rule:\n{bad}")
    pd.set_option("display.width", 250)
    pd.set_option("display.max_rows", 500)
    pd.set_option("display.max_columns", 40)
    log("unobserved counts (shared variables, by source):")
    log(na_tab.loc[na_tab.unobserved_by_design_rule.isna() & (na_tab.unobserved > 0)].to_string())

    rec_cols = ["source", "year", "hh_id", "implicate", "unit", "weight", "half", "age", "wealth_observed"] + \
               names + WEALTH + ["t2_" + c for c in INC7]
    split_rows = {p: np.flatnonzero(part == p) for p in ("train", "val", "test")}
    for p, idx in split_rows.items():
        np.save(os.path.join(out, f"tokens_{p}.npy"), tokens[idx])
        np.save(os.path.join(out, f"truth_{p}.npy"), truth[idx])
        df.loc[idx, rec_cols].reset_index(drop=True).to_parquet(os.path.join(out, f"records_{p}.parquet"), index=False)
        log(p, tokens[idx].shape, {s: int((df.source.iloc[idx] == s).sum()) for s in ("PUF", "SCF")})
    meta["graded_rows"] = {p: {"rows": int(len(i)),
                               "scf_rows": int((df.source.iloc[i] == "SCF").sum()),
                               "scf_wealth_observed": int((df.wealth_observed.iloc[i] & (df.source.iloc[i] == "SCF")).sum()),
                               "scf_split_off_no_wealth": int(((df.source.iloc[i] == "SCF") & ~df.wealth_observed.iloc[i]).sum())}
                           for p, i in split_rows.items()}
    meta["split"] = {"sizes": {p: int(len(i)) for p, i in split_rows.items()},
                     "rule": ("PUF: test 1's split (80/10/10 within weight x AGI strata, seed 20261008), all of "
                              "test 1's 207,692 records including dependent filers; SCF: test 2 expB's household "
                              "split (per wave, households 70/10/20, seed 20261009), reused by household id")}
    meta["wealth_observed_rule"] = ("wealth is unobserved (token -1, NaN in records) for every PUF record and for SCF "
                                    "split-off units (unit >= 3); wealth ranks and quantile functions use units with "
                                    "wealth observed only")
    with open(os.path.join(out, "meta.json"), "w") as f:
        json.dump(meta, f, indent=1, default=lambda o: o.item() if hasattr(o, "item") else str(o))
    log("meta written; variables", len(vnames) - 1, "generation order", meta["generation_order"])

    # Observed / unobserved counts per variable, source-year, part.
    oc = observed_counts(df, tokens, meta, part)
    oc.to_csv(os.path.join(out, "checks_observed_counts.csv"), index=False)

    # Population checks (crosswalk: PUF against SCF filers).
    amt, cat = population_checks(df, meta)
    amt.to_csv(os.path.join(out, "checks_totals.csv"), index=False)
    cat.to_csv(os.path.join(out, "checks_categories.csv"), index=False)
    log(amt.loc[amt.group == "shared"].to_string())
    log(cat.loc[cat.group == "shared"].to_string())

    # Round trip on training rows, five decoding seeds.
    tm = part == "train"
    rt = roundtrip(df, truth, meta, q, tm, SEED_ROUNDTRIP, cache)
    tots = [rt.total_rel_err.to_numpy()]
    for i in range(1, 5):
        tots.append(roundtrip(df, truth, meta, q, tm, SEED_ROUNDTRIP + i, cache).total_rel_err.to_numpy())
    tots = np.vstack(tots)
    rt["total_rel_err_mean_5_seeds"] = tots.mean(axis=0)
    rt["total_rel_err_sd_5_seeds"] = tots.std(axis=0, ddof=1)
    rt.to_csv(os.path.join(out, "roundtrip.csv"), index=False)
    log(rt[["variable", "source", "year", "share_zero_true", "share_zero_decoded", "total_rel_err_mean_5_seeds",
            "total_rel_err_sd_5_seeds", "nonzero_q0.5_rel_err", "nonzero_q0.99_rel_err"]].to_string())
    # Re-encode agreement.
    rec = df.loc[tm].reset_index(drop=True)
    dec = tk.decode(truth[tm], meta, rec, q, np.random.default_rng(SEED_ROUNDTRIP), cache=cache)
    dec2 = rec[["source", "year"]].join(dec)
    re = tk.encode(dec2, meta, q, np.random.default_rng(SEED_ENCODE + 1), cache=cache)
    t0 = truth[tm]
    agree = {v["name"]: float(np.mean(re[t0[:, k] >= 0, k] == t0[t0[:, k] >= 0, k]))
             for k, v in enumerate(meta["variables"]) if (t0[:, k] >= 0).any()}
    log("re-encode agreement", agree)
    with open(os.path.join(out, "prep_summary.json"), "w") as f:
        json.dump({"counts": info, "split_sizes": meta["split"]["sizes"], "reencode_agreement": agree,
                   "roundtrip_max_abs_total_rel_err_mean5": float(np.nanmax(np.abs(rt.total_rel_err_mean_5_seeds))),
                   "crosswalk": meta["crosswalk"]}, f, indent=1,
                  default=lambda o: o.item() if hasattr(o, "item") else str(o))
    log("full done", json.dumps(info, default=str)[:4000])


if __name__ == "__main__":
    main()
