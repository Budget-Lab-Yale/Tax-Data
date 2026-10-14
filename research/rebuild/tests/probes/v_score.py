"""Probe V (test 4): a broad scorecard for a file of generated PUF records.

Usage:
  python v_score.py --gen <parquet | 'validation'> --label <name> --out <json>
                    [--tokens <npz>] [--meta-dir <dir with meta.json, tokens_train.npy>]

<parquet> is any file of generated PUF records with the model's PUF variables
as columns and a `weight` column, in the format of probe AS's generated files
(o_complete.py, generate mode). AGI is taken from `E00100` if the file has it
(test 3's model draws AGI) and otherwise computed from its parts plus
`AGI_REMAINDER` (probe AS, as_agi.py). 'validation' scores the real PUF
validation records (weights rescaled to the training total) as if they were a
generated file: the reference for sampling noise and for copying.

Reference: the real PUF training records of full_v2s (probe AS's data; the
PUF side is identical to test 3's full_v2 apart from AGI being replaced by
the remainder, and the records file carries both).

Every statistic is weighted. AGI groups: bottom 50, 50-90, 90-99, 99-99.9,
top 0.1, on each file's own AGI, with the thresholds from the real PUF
training records. Only weighted aggregates are written; no record-level rows.

Sections (one key each in the json):
  variables   share nonzero and total dollars by AGI group, every variable
  joint       filing status, itemizers, co-holding of 8 main items, EITC and
              child-credit counts, exemptions, age range (observed only)
  rules       rules that hold on 100% of real training records, checked on the
              file (weighted share breaking each, and any)
  tax         simplified 2015 federal income tax (see tax2015), by AGI group
  classifier  gradient-boosted trees separating the file from real records:
              cross-validated area under the ROC curve, overall and by group
  copying     nearest real training record on rank-scaled variables, against
              the same distance for real validation records; exact copies
"""
import argparse
import json
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import as_agi  # noqa: E402

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s")
EDGES_FOR_COPY = None  # set in main: the real AGI group edges
GROUPS = ["bottom 50", "50-90", "90-99", "99-99.9", "top 0.1"]
CATS = ["MARS", "DSI", "XTOT", "N24", "EIC", "FDED", "AGERANGE", "GENDER", "EARNSPLIT"]
DEMO = ["AGERANGE", "GENDER", "EARNSPLIT"]  # unobserved on ~42% of real records
DOLLARS = ["E00200", "E00900", "E02000", "E26270", "P23250", "P22250", "E00600", "E00650",
           "E00300", "E01500", "E01700", "E01400", "E02400", "E25850", "E25860", "E02100",
           "E00400", "E26390", "E26400", "E01100", "E02300", "E03150", "E03210", "E03270",
           "E17500", "E18400", "E18500", "E19200", "E19800", "E20100"]
ITEMIZED = ["E17500", "E18400", "E18500", "E19200", "E19800", "E20100"]
ALLVARS = DOLLARS + [as_agi.REM, "E00100"]
MAIN_ITEMS = {"wages": ["E00200"], "interest": ["E00300"], "dividends": ["E00600"],
              "capital gains": ["P23250", "P22250", "E01100"], "Schedule C": ["E00900"],
              "partnership and S corp": ["E26270"], "pensions": ["E01500"],
              "Social Security": ["E02400"]}


def wq(x, w, q):
    o = np.argsort(x, kind="stable")
    cw = np.cumsum(w[o])
    return x[o][min(np.searchsorted(cw, q * cw[-1]), len(x) - 1)]


def group_index(agi, edges):
    return np.clip(np.searchsorted(edges, agi, side="right") - 1, 0, 4)


def prepare(df):
    df = df.copy()
    if "draw" in df:
        df = df[df["draw"] == 0].reset_index(drop=True)
    for c in DOLLARS + CATS:
        if c not in df:
            raise SystemExit(f"column {c} missing")
    if "E00100" in df and df["E00100"].notna().all():
        if as_agi.REM not in df or df[as_agi.REM].isna().any():
            df[as_agi.REM] = df["E00100"].to_numpy(float) - as_agi.items_less_adjustments(df)
        df["_agi_source"] = "drawn"
    else:
        df["E00100"] = as_agi.agi_from_parts(df)
        df["_agi_source"] = "sum of parts"
    return df


# ----------------------------------------------------------------------------- 1. variables
def variables(df, w, g):
    out = {}
    for v in ALLVARS:
        x = df[v].to_numpy(float)
        row = {}
        for k, gl in enumerate(GROUPS + ["all"]):
            m = (g == k) if gl != "all" else np.ones(len(x), bool)
            ww = w[m]
            row[gl] = dict(nonzero=float(ww[x[m] != 0].sum() / ww.sum()) if ww.sum() > 0 else float("nan"),
                           total_bn=float((x[m] * ww).sum() / 1e9),
                           negative=float(ww[x[m] < 0].sum() / ww.sum()) if ww.sum() > 0 else float("nan"))
        out[v] = row
    out["_group_weight_share"] = {gl: float(w[g == k].sum() / w.sum()) for k, gl in enumerate(GROUPS)}
    out["_returns_m"] = float(w.sum() / 1e6)
    return out


# ----------------------------------------------------------------------------- 2. joint
def nz(df, cols):
    s = np.zeros(len(df))
    for c in cols:
        s = s + df[c].to_numpy(float)
    return s != 0


def joint(df, w, g):
    out = {}
    held = {k: nz(df, cols) for k, cols in MAIN_ITEMS.items()}
    names = list(MAIN_ITEMS)
    for k, gl in enumerate(GROUPS + ["all"]):
        m = (g == k) if gl != "all" else np.ones(len(df), bool)
        ww = w[m]
        W = ww.sum()
        r = {}
        for c in ["MARS", "FDED", "EIC", "N24", "XTOT", "DSI"]:
            x = np.round(df[c].to_numpy(float)[m])
            if c in ("N24", "XTOT"):
                x = np.minimum(x, 5 if c == "XTOT" else 3)
            vals = np.unique(x[~np.isnan(x)])
            r[c] = {str(int(v)): float(ww[x == v].sum() / W) for v in vals}
        a = df["AGERANGE"].to_numpy(float)[m]
        obs = a >= 1
        r["AGERANGE_observed_share"] = float(ww[obs].sum() / W)
        r["AGERANGE"] = {str(int(v)): float(ww[obs & (a == v)].sum() / max(ww[obs].sum(), 1e-12))
                         for v in range(1, 7)}
        r["held"] = {n: float(ww[held[n][m]].sum() / W) for n in names}
        co = {}
        for i in range(len(names)):
            for j in range(i + 1, len(names)):
                co[f"{names[i]} & {names[j]}"] = float(ww[(held[names[i]] & held[names[j]])[m]].sum() / W)
        r["coholding"] = co
        r["itemizer_share"] = float(ww[df["FDED"].to_numpy(float)[m] == 1].sum() / W)
        r["eitc_children_any"] = float(ww[df["EIC"].to_numpy(float)[m] > 0].sum() / W)
        r["ctc_children_mean"] = float((df["N24"].to_numpy(float)[m] * ww).sum() / W)
        r["exemptions_mean"] = float((df["XTOT"].to_numpy(float)[m] * ww).sum() / W)
        out[gl] = r
    return out


# ----------------------------------------------------------------------------- 3. rules
def discover_rules(r, w):
    """Rules that hold on every real training record."""
    rules = []
    X = {v: r[v].to_numpy(float) for v in DOLLARS + [as_agi.REM, "E00100"]}
    C = {c: r[c].to_numpy(float) for c in CATS}
    n = len(r)
    # signs
    for v in DOLLARS:
        if (X[v] >= 0).all():
            rules.append(dict(kind="sign", text=f"{v} >= 0", a=v))
    # orderings among nonnegative dollar variables and count variables
    nonneg = [v for v in DOLLARS if (X[v] >= 0).all()]
    for a in nonneg:
        if w[X[a] > 0].sum() / w.sum() < 0.001:
            continue
        for b in nonneg:
            if a != b and (X[a] <= X[b] + 0.5).all():
                rules.append(dict(kind="le", text=f"{a} <= {b}", a=a, b=b))
    for a in ["N24", "EIC", "XTOT"]:
        for b in ["N24", "EIC", "XTOT"]:
            if a != b and (C[a] <= C[b]).all():
                rules.append(dict(kind="le", text=f"{a} <= {b}", a=a, b=b))
    # a nonzero implies b nonzero
    for a in DOLLARS:
        if w[X[a] != 0].sum() / w.sum() < 0.001:
            continue
        for b in DOLLARS:
            if a != b and ((X[a] == 0) | (X[b] != 0)).all():
                # skip implications already implied by an ordering a <= b on nonneg vars
                rules.append(dict(kind="implies_nonzero", text=f"{a} nonzero => {b} nonzero", a=a, b=b))
    # categorical value implies a dollar variable is zero
    for c in ["MARS", "DSI", "FDED", "EIC"]:
        for v in np.unique(C[c][~np.isnan(C[c])]):
            m = C[c] == v
            if m.sum() < 200:
                continue
            for x in DOLLARS:
                if (X[x][m] == 0).all() and (X[x] != 0).any():
                    rules.append(dict(kind="cat_zero", text=f"{c} = {int(v)} => {x} = 0", c=c, v=float(v), a=x))
    # combinations of the six tax categoricals never seen on a real record
    rules.append(dict(kind="combo", text="combination of MARS, DSI, XTOT (capped 6), N24 (capped 4), "
                      "EIC, FDED never seen on a real training record"))
    # income ceilings by EITC children (highest AGI on any real record with EIC > 0)
    m = C["EIC"] > 0
    rules.append(dict(kind="eic_agi", text=f"EIC > 0 => AGI <= {X['E00100'][m].max():,.0f} "
                      "(highest AGI with EITC children on a real record)", cap=float(X["E00100"][m].max())))
    # itemizer with no itemized amounts; non-itemizer with amounts
    it = (C["FDED"] == 1)
    anyit = np.zeros(n, bool)
    for x in ITEMIZED:
        anyit |= X[x] != 0
    rules.append(dict(kind="info", text="FDED = 1 and no itemized amount (real share shown, not a rule)",
                      real_share=float(w[it & ~anyit].sum() / w.sum())))
    return rules


def combo_key(df):
    c = [np.nan_to_num(np.round(df["MARS"].to_numpy(float))), np.nan_to_num(np.round(df["DSI"].to_numpy(float))),
         np.minimum(np.nan_to_num(np.round(df["XTOT"].to_numpy(float))), 6),
         np.minimum(np.nan_to_num(np.round(df["N24"].to_numpy(float))), 4),
         np.nan_to_num(np.round(df["EIC"].to_numpy(float))), np.nan_to_num(np.round(df["FDED"].to_numpy(float)))]
    return (c[0] * 1e5 + c[1] * 1e4 + c[2] * 1e3 + c[3] * 1e2 + c[4] * 10 + c[5]).astype(np.int64)


def check_rules(df, w, g, rules, real_combos):
    X = {v: df[v].to_numpy(float) for v in DOLLARS + [as_agi.REM, "E00100"]}
    C = {c: df[c].to_numpy(float) for c in CATS}
    anyb = np.zeros(len(df), bool)
    res = []
    for ru in rules:
        k = ru["kind"]
        if k == "sign":
            bad = X[ru["a"]] < 0
        elif k == "le":
            A = X.get(ru["a"], C.get(ru["a"]))
            B = X.get(ru["b"], C.get(ru["b"]))
            bad = A > B + 0.5
        elif k == "implies_nonzero":
            bad = (X[ru["a"]] != 0) & (X[ru["b"]] == 0)
        elif k == "cat_zero":
            bad = (C[ru["c"]] == ru["v"]) & (X[ru["a"]] != 0)
        elif k == "combo":
            bad = ~np.isin(combo_key(df), real_combos)
        elif k == "eic_agi":
            bad = (C["EIC"] > 0) & (X["E00100"] > ru["cap"])
        elif k == "info":
            it = C["FDED"] == 1
            anyit = np.zeros(len(df), bool)
            for x in ITEMIZED:
                anyit |= X[x] != 0
            res.append(dict(text=ru["text"], share=float(w[it & ~anyit].sum() / w.sum()), real_share=ru["real_share"]))
            continue
        extra = {}
        if k == "le":
            bad5 = A > 1.05 * B + 1.0
            extra = dict(share_by_more_than_5pct=float(w[bad5].sum() / w.sum()))
        anyb |= bad
        res.append(dict(text=ru["text"], kind=k, share=float(w[bad].sum() / w.sum()), **extra,
                        by_group={gl: float(w[bad & (g == j)].sum() / max(w[g == j].sum(), 1e-12))
                                  for j, gl in enumerate(GROUPS)}))
    return dict(rules=res, any_share=float(w[anyb].sum() / w.sum()),
                any_by_group={gl: float(w[anyb & (g == j)].sum() / max(w[g == j].sum(), 1e-12))
                              for j, gl in enumerate(GROUPS)})


# ----------------------------------------------------------------------------- 4. tax
BR = {  # 2015 ordinary brackets: upper edges of 10, 15, 25, 28, 33, 35 percent
    1: [9225, 37450, 90750, 189300, 411500, 413200],
    2: [18450, 74900, 151200, 230450, 411500, 464850],
    3: [9225, 37450, 75600, 115225, 205750, 232425],
    4: [13150, 50200, 129600, 209850, 411500, 439000]}
RATES = [0.10, 0.15, 0.25, 0.28, 0.33, 0.35, 0.396]
STD = {1: 6300, 2: 12600, 3: 6300, 4: 9250}
PHASE = {1: 258250, 2: 309900, 3: 154950, 4: 284050}   # exemption phase-out and Pease
CG0 = {1: 37450, 2: 74900, 3: 37450, 4: 50200}         # top of the 0% capital gains band
CG15 = {1: 413200, 2: 464850, 3: 232425, 4: 439000}    # top of the 15% band
CTC_PS = {1: 75000, 2: 110000, 3: 55000, 4: 75000}


def by_status(mars, table):
    out = np.zeros(len(mars))
    for k, v in table.items():
        out[mars == k] = v
    return out


def bracket_tax(ti, mars):
    t = np.zeros(len(ti))
    for k, edges in BR.items():
        m = mars == k
        x = ti[m]
        lo = 0.0
        tk = np.zeros(m.sum())
        for e, r in zip(edges + [np.inf], RATES):
            tk += r * np.clip(x - lo, 0, e - lo)
            lo = e
        t[m] = tk
    return t


def tax2015(df, agi):
    """Simplified 2015 federal income tax. Inputs: the model's variables only.
    Included: brackets by filing status; standard deduction (dependent filers
    limited to max($1,050, earned income + $350)); $4,000 exemptions with the
    phase-out; itemized deductions from the six itemized amounts (medical above
    10% of AGI, state and local income and real estate taxes, interest paid,
    charity limited to 50% of AGI) with the Pease limitation, taken when the
    record itemizes (FDED 1) and they exceed the standard deduction; 0/15/20%
    rates on qualified dividends, net long-term gains and capital gain
    distributions; child tax credit with its phase-out and the refundable
    additional credit; earned income credit (2015 schedule, investment income
    limit). Not included: additional standard deduction for age or
    blindness, AMT, net investment income tax, other credits."""
    mars = np.clip(np.round(np.nan_to_num(df["MARS"].to_numpy(float), nan=1)), 1, 4).astype(int)
    dsi = np.nan_to_num(df["DSI"].to_numpy(float)) == 1
    xtot = np.clip(np.nan_to_num(df["XTOT"].to_numpy(float)), 0, None)
    n24 = np.clip(np.nan_to_num(df["N24"].to_numpy(float)), 0, None)
    eic = np.clip(np.nan_to_num(df["EIC"].to_numpy(float)), 0, 3)
    fded = np.nan_to_num(df["FDED"].to_numpy(float))
    get = lambda c: np.nan_to_num(df[c].to_numpy(float))  # noqa: E731
    earned = get("E00200") + get("E00900") + get("E02100")
    std = by_status(mars, STD)
    std = np.where(dsi, np.minimum(std, np.maximum(1050, earned + 350)), std)
    ps = by_status(mars, PHASE)
    step = np.where(mars == 3, 1250, 2500)
    frac = np.clip(np.ceil(np.clip(agi - ps, 0, None) / step) * 0.02, 0, 1)
    ex = np.where(dsi, 0, 4000 * xtot * (1 - frac))
    med = np.clip(get("E17500") - 0.10 * np.clip(agi, 0, None), 0, None)
    other = (np.clip(get("E18400"), 0, None) + np.clip(get("E18500"), 0, None) + np.clip(get("E19200"), 0, None)
             + np.minimum(np.clip(get("E19800") + get("E20100"), 0, None), 0.5 * np.clip(agi, 0, None)))
    pease = np.minimum(0.03 * np.clip(agi - ps, 0, None), 0.8 * other)
    item = med + other - pease
    ded = np.where(fded == 1, np.maximum(item, std), std)
    ti = np.clip(agi - ded - ex, 0, None)
    lt, st = get("P23250"), get("P22250")
    netcg = np.clip(np.minimum(lt, lt + st), 0, None) + np.clip(get("E01100"), 0, None)
    pref = np.minimum(np.clip(get("E00650"), 0, None) + netcg, ti)
    ordi = ti - pref
    z0, z15 = by_status(mars, CG0), by_status(mars, CG15)
    at0 = np.clip(np.minimum(ti, z0) - ordi, 0, None)
    at15 = np.clip(np.minimum(ti, z15) - np.maximum(ordi, z0), 0, None)
    at20 = np.clip(pref - at0 - at15, 0, None)
    tax = np.minimum(bracket_tax(ordi, mars) + 0.15 * at15 + 0.20 * at20, bracket_tax(ti, mars))
    # child tax credit
    ctc = 1000 * n24
    ctc = np.clip(ctc - 50 * np.ceil(np.clip(agi - by_status(mars, CTC_PS), 0, None) / 1000), 0, None)
    ctc_nr = np.minimum(ctc, tax)
    actc = np.minimum(ctc - ctc_nr, 0.15 * np.clip(earned - 3000, 0, None))
    # earned income credit
    k = eic.astype(int)
    rate = np.array([0.0765, 0.34, 0.40, 0.45])[k]
    eamt = np.array([6580, 9880, 13870, 13870])[k]
    start = np.array([8240, 18110, 18110, 18110])[k] + np.where(mars == 2, 5520, 0)
    prate = np.array([0.0765, 0.1598, 0.2106, 0.2106])[k]
    e = np.clip(earned, 0, None)
    eitc = np.clip(rate * np.minimum(e, eamt) - prate * np.clip(np.maximum(agi, e) - start, 0, None), 0, None)
    inv = get("E00300") + get("E00400") + get("E00600") + np.clip(lt + st, 0, None) + get("E01100")
    eitc = np.where((inv > 3400) | (mars == 3) | dsi, 0, eitc)
    net = tax - ctc_nr - actc - eitc
    return dict(taxable_income=ti, tax_before_credits=tax, ctc=ctc_nr + actc, eitc=eitc, net_tax=net,
                itemizing=(fded == 1) & (item > std))


def tax_tables(df, w, g, agi):
    t = tax2015(df, agi)
    out = {}
    for k, gl in enumerate(GROUPS + ["all"]):
        m = (g == k) if gl != "all" else np.ones(len(df), bool)
        ww = w[m]
        A = (agi[m] * ww).sum()
        r = {c + "_bn": float((t[c][m] * ww).sum() / 1e9) for c in
             ["taxable_income", "tax_before_credits", "ctc", "eitc", "net_tax"]}
        r["agi_bn"] = float(A / 1e9)
        r["avg_rate_net"] = float((t["net_tax"][m] * ww).sum() / A) if A != 0 else float("nan")
        r["share_positive_tax"] = float(ww[t["tax_before_credits"][m] > 0].sum() / ww.sum())
        r["share_eitc"] = float(ww[t["eitc"][m] > 0].sum() / ww.sum())
        r["returns_m"] = float(ww.sum() / 1e6)
        out[gl] = r
    # share of total net tax and of tax before credits by group
    tot = out["all"]["net_tax_bn"]
    for gl in GROUPS:
        out[gl]["share_of_net_tax"] = out[gl]["net_tax_bn"] / tot
    return out


# ----------------------------------------------------------------------------- 5. classifier
def wauc(y, s, w):
    from sklearn.metrics import roc_auc_score
    if len(np.unique(y)) < 2:
        return float("nan")
    return float(roc_auc_score(y, s, sample_weight=w))


def classifier(real, gen, wr, wg, gr, gg, rng, feats=None):
    from sklearn.ensemble import HistGradientBoostingClassifier
    from sklearn.model_selection import StratifiedKFold
    feats = feats or ([c for c in CATS if c not in DEMO] + DOLLARS + [as_agi.REM, "E00100"])
    ng = min(len(gen), len(real))
    pick = rng.choice(len(gen), ng, replace=False) if len(gen) > ng else np.arange(len(gen))
    X = np.vstack([real[feats].to_numpy(float), gen[feats].to_numpy(float)[pick]])
    y = np.r_[np.zeros(len(real)), np.ones(len(pick))]
    w = np.r_[wr / wr.sum(), wg[pick] / wg[pick].sum()] * len(y) / 2   # balanced classes
    grp = np.r_[gr, gg[pick]]
    out = {"features": feats, "n_real": int(len(real)), "n_generated": int(len(pick))}

    def cv(Xs, ys, ws, seed):
        oof = np.zeros(len(ys))
        models = []
        for tr, te in StratifiedKFold(5, shuffle=True, random_state=seed).split(Xs, ys):
            clf = HistGradientBoostingClassifier(max_iter=300, learning_rate=0.08, max_leaf_nodes=31,
                                                 min_samples_leaf=40, l2_regularization=1.0,
                                                 categorical_features=None, random_state=seed)
            clf.fit(Xs[tr], ys[tr], sample_weight=ws[tr])
            oof[te] = clf.predict_proba(Xs[te])[:, 1]
            models.append((clf, te))
        return oof, models

    t0 = time.time()
    oof, models = cv(X, y, w, 1)
    out["auc_overall"] = wauc(y, oof, w)
    out["auc_overall_unweighted"] = wauc(y, oof, np.ones(len(y)))
    out["auc_by_group_from_overall_model"] = {gl: wauc(y[grp == k], oof[grp == k], w[grp == k])
                                              for k, gl in enumerate(GROUPS)}
    # permutation importance on one fold, weighted AUC drop
    clf, te = models[0]
    base = wauc(y[te], clf.predict_proba(X[te])[:, 1], w[te])
    imp = {}
    for j, f in enumerate(feats):
        Xp = X[te].copy()
        Xp[:, j] = Xp[rng.permutation(len(te)), j]
        imp[f] = base - wauc(y[te], clf.predict_proba(Xp)[:, 1], w[te])
    out["importance_auc_drop"] = dict(sorted(imp.items(), key=lambda kv: -kv[1]))
    # a separate classifier within each AGI group (weighted, balanced within group)
    pg = {}
    for k, gl in enumerate(GROUPS):
        m = grp == k
        if (y[m] == 0).sum() < 50 or (y[m] == 1).sum() < 50:
            pg[gl] = dict(auc=float("nan"), n_real=int((y[m] == 0).sum()), n_generated=int((y[m] == 1).sum()))
            continue
        ym, wm = y[m], w[m].copy()
        wm[ym == 0] *= 1 / wm[ym == 0].sum()
        wm[ym == 1] *= 1 / wm[ym == 1].sum()
        wm *= len(ym) / 2
        o, mods = cv(X[m], ym, wm, 2)
        c2, te2 = mods[0]
        b2 = wauc(ym[te2], c2.predict_proba(X[m][te2])[:, 1], wm[te2])
        imp2 = {}
        for j, f in enumerate(feats):
            Xp = X[m][te2].copy()
            Xp[:, j] = Xp[rng.permutation(len(te2)), j]
            imp2[f] = b2 - wauc(ym[te2], c2.predict_proba(Xp)[:, 1], wm[te2])
        top = sorted(imp2.items(), key=lambda kv: -kv[1])[:6]
        pg[gl] = dict(auc=wauc(ym, o, wm), auc_unweighted=wauc(ym, o, np.ones(len(ym))),
                      n_real=int((ym == 0).sum()), n_generated=int((ym == 1).sum()), top_features=top)
    out["auc_within_group_models"] = pg
    out["seconds"] = time.time() - t0
    return out


# ----------------------------------------------------------------------------- 6. copying
def rank_fn(x, w):
    o = np.argsort(x, kind="stable")
    xs, ws = x[o], w[o]
    u, idx = np.unique(xs, return_index=True)
    cw = np.r_[0.0, np.cumsum(ws)] / ws.sum()

    def f(z):
        lo = np.searchsorted(xs, z, side="left")
        hi = np.searchsorted(xs, z, side="right")
        return (cw[lo] + cw[hi]) / 2
    return f


def nn_dist(Q, T, chunk=4096):
    import torch
    torch.set_num_threads(max(1, int(__import__("os").environ.get("SLURM_CPUS_PER_TASK", "4"))))
    Tt = torch.from_numpy(T.astype(np.float32))
    tn = (Tt * Tt).sum(1)
    out = np.empty(len(Q), np.float32)
    for s in range(0, len(Q), chunk):
        q = torch.from_numpy(Q[s:s + chunk].astype(np.float32))
        d2 = (q * q).sum(1)[:, None] + tn[None, :] - 2 * q @ Tt.T
        out[s:s + chunk] = torch.clamp(d2.min(1).values, min=0).sqrt().numpy()
    return out


def copying(real, wr, val, wv, gen, wg, gg, rng, tok_gen=None, tok_train=None, tok_val=None):
    feats = [c for c in CATS if c not in DEMO] + DOLLARS + [as_agi.REM, "E00100"]
    fns = [rank_fn(real[f].to_numpy(float), wr) for f in feats]
    R = lambda d: np.column_stack([fn(d[f].to_numpy(float)) for fn, f in zip(fns, feats)])  # noqa: E731
    T = R(real)
    V = R(val)
    top = np.where(gg == 4)[0]
    rest = rng.choice(len(gen), min(60000, len(gen)), replace=False)
    qi = np.unique(np.r_[rest, top])
    G = R(gen.iloc[qi])
    dv = nn_dist(V, T)
    dg = nn_dist(G, T)
    gq = gg[qi]
    wq_ = wg[qi]
    in_rest = np.isin(qi, rest)
    agiv = val["E00100"].to_numpy(float)
    out = {"features": len(feats), "n_query_generated": int(len(qi)), "n_validation": int(len(V))}
    pv = {p: float(wq(dv.astype(float), wv, p / 100)) for p in (1, 5, 50)}
    out["validation_nn_distance_pct"] = pv
    out["generated_nn_distance_pct"] = {p: float(wq(dg[in_rest].astype(float), wq_[in_rest], p / 100))
                                        for p in (1, 5, 50)}
    out["share_below_validation_p1"] = float(wq_[in_rest][dg[in_rest] < pv[1]].sum() / wq_[in_rest].sum())
    out["share_below_validation_p5"] = float(wq_[in_rest][dg[in_rest] < pv[5]].sum() / wq_[in_rest].sum())
    out["share_distance_zero"] = float(wq_[in_rest][dg[in_rest] == 0].sum() / wq_[in_rest].sum())
    out["validation_share_distance_zero"] = float(wv[dv == 0].sum() / wv.sum())
    # top 0.1% (by own AGI): generated against real validation records in the same group
    edges_val_top = None
    mt = gq == 4
    out["top01_generated_n"] = int(mt.sum())
    if mt.any():
        out["top01_generated_nn_median"] = float(np.median(dg[mt]))
        out["top01_generated_share_distance_zero"] = float((mt & (dg == 0)).sum() / mt.sum())
    out["_edges_val_top"] = edges_val_top
    gvv = group_index(agiv, np.array([-np.inf, *EDGES_FOR_COPY[1:]])) if EDGES_FOR_COPY is not None else None
    out["by_group"] = {}
    for k, gl in enumerate(GROUPS):
        mg = (gq == k)
        row = dict(n_generated=int(mg.sum()),
                   gen_share_zero=float(wq_[mg][dg[mg] == 0].sum() / max(wq_[mg].sum(), 1e-12)) if mg.any() else float("nan"),
                   gen_median=float(np.median(dg[mg])) if mg.any() else float("nan"))
        if gvv is not None:
            mv = gvv == k
            row.update(n_validation=int(mv.sum()),
                       val_share_zero=float(wv[mv][dv[mv] == 0].sum() / max(wv[mv].sum(), 1e-12)) if mv.any() else float("nan"),
                       val_median=float(np.median(dv[mv])) if mv.any() else float("nan"))
        out["by_group"][gl] = row
    # exact copies in dollars on all features
    # (distance zero in ranks means every feature's rank equals a training record's)
    # token-level copies: identical token pattern on the PUF variables
    if tok_gen is not None and tok_train is not None:
        th = set(map(bytes, np.ascontiguousarray(tok_train)))
        hit_g = np.array([bytes(r) in th for r in np.ascontiguousarray(tok_gen)])
        hit_v = np.array([bytes(r) in th for r in np.ascontiguousarray(tok_val)])
        out["token_copy_share_generated"] = float(wg[hit_g].sum() / wg.sum())
        out["token_copy_share_validation"] = float(wv[hit_v].sum() / wv.sum())
        out["token_copy_share_generated_top01"] = float(wg[hit_g & (gg == 4)].sum() / max(wg[gg == 4].sum(), 1e-12))
        out["token_copy_share_generated_by_group"] = {gl: float(wg[hit_g & (gg == k)].sum() / max(wg[gg == k].sum(), 1e-12))
                                                      for k, gl in enumerate(GROUPS)}
        out["_val_groups_for_token"] = "see validation run"
    return out, dv, agiv


# ----------------------------------------------------------------------------- main
def main():
    p = argparse.ArgumentParser()
    p.add_argument("--gen", required=True)
    p.add_argument("--label", required=True)
    p.add_argument("--out", required=True)
    p.add_argument("--tokens", default=None)
    p.add_argument("--meta-dir", default=None)
    p.add_argument("--skip", default="", help="comma list of sections to skip")
    p.add_argument("--merge", action="store_true", help="keep sections of an existing --out json not recomputed")
    a = p.parse_args()
    skip = set(a.skip.split(",")) if a.skip else set()
    t0 = time.time()
    rng = np.random.default_rng(20261011)
    rec = pd.read_parquet(DATA / "records_train.parquet")
    real = prepare(rec[rec["source"].astype(str) == "PUF"].reset_index(drop=True))
    real["_agi_source"] = "real"
    wr = real["weight"].to_numpy(float)
    vrec = pd.read_parquet(DATA / "records_val.parquet")
    val = prepare(vrec[vrec["source"].astype(str) == "PUF"].reset_index(drop=True))
    wv = val["weight"].to_numpy(float) * wr.sum() / val["weight"].to_numpy(float).sum()
    agi_r = real["E00100"].to_numpy(float)
    edges = np.array([-np.inf, wq(agi_r, wr, 0.5), wq(agi_r, wr, 0.9), wq(agi_r, wr, 0.99), wq(agi_r, wr, 0.999)])
    global EDGES_FOR_COPY
    EDGES_FOR_COPY = edges
    gr = group_index(agi_r, edges)
    gv = group_index(val["E00100"].to_numpy(float), edges)

    if a.gen == "validation":
        gen, wg = val, wv
    else:
        gen = prepare(pd.read_parquet(a.gen))
        wg = gen["weight"].to_numpy(float)
    agi_g = gen["E00100"].to_numpy(float)
    gg = group_index(agi_g, edges)
    res = dict(label=a.label, file=a.gen, n_records=int(len(gen)), agi_source=str(gen["_agi_source"].iloc[0]),
               thresholds=dict(p50=edges[1], p90=edges[2], p99=edges[3], p999=edges[4]),
               total_weight_m=float(wg.sum() / 1e6), real_total_weight_m=float(wr.sum() / 1e6))
    print(f"{a.label}: {len(gen)} records, AGI {res['agi_source']}", flush=True)

    if a.merge and Path(a.out).exists():
        old = json.load(open(a.out))
        old.update(res)
        res = old
    if "descriptive" not in skip:
        res.update(_descriptive(real, wr, gr, val, wv, gv, gen, wg, gg, agi_r, agi_g))
    json.dump(res, open(a.out, "w"), indent=1, default=float)
    print(f"descriptive sections done {time.time() - t0:.0f}s", flush=True)
    if "classifier" not in skip:
        res["classifier"] = classifier(real, gen, wr, wg, gr, gg, rng)
        print(f"classifier done {time.time() - t0:.0f}s auc {res['classifier']['auc_overall']:.3f}", flush=True)
        json.dump(res, open(a.out, "w"), indent=1, default=float)
    _copy_and_finish(a, skip, res, real, wr, val, wv, gen, wg, gg, gv, rng, t0)


def _descriptive(real, wr, gr, val, wv, gv, gen, wg, gg, agi_r, agi_g):
    res = {}
    res["variables"] = dict(real=variables(real, wr, gr), gen=variables(gen, wg, gg))
    res["joint"] = dict(real=joint(real, wr, gr), gen=joint(gen, wg, gg))
    rules = discover_rules(real, wr)
    real_combos = np.unique(combo_key(real))
    res["rules"] = dict(n_rules=len(rules), real_validation=check_rules(val, wv, gv, rules, real_combos),
                        gen=check_rules(gen, wg, gg, rules, real_combos))
    res["tax"] = dict(real=tax_tables(real, wr, gr, agi_r), gen=tax_tables(gen, wg, gg, agi_g))
    return res


def _copy_and_finish(a, skip, res, real, wr, val, wv, gen, wg, gg, gv, rng, t0):
    if "copying" not in skip:
        tg = tt = tv = None
        if a.meta_dir:
            md = Path(a.meta_dir)
            meta = json.load(open(md / "meta.json"))
            names = meta["variable_names"]
            cols = [names.index(c) for c in names if c in (CATS + DOLLARS + [as_agi.REM, "E00100"]) and c not in DEMO]
            rtr = pd.read_parquet(md / "records_train.parquet")
            ttr = np.load(md / "tokens_train.npy")
            tvl = np.load(md / "tokens_val.npy")
            rv = pd.read_parquet(md / "records_val.parquet")
            ptr = (rtr["source"].astype(str) == "PUF").to_numpy()
            pvl = (rv["source"].astype(str) == "PUF").to_numpy()
            # token arrays may or may not carry the context slot ('source', column 0)
            sh = lambda arr: [c - (len(names) - arr.shape[1]) for c in cols]  # noqa: E731
            tt, tv = ttr[ptr][:, sh(ttr)].astype(np.int16), tvl[pvl][:, sh(tvl)].astype(np.int16)
            if a.gen == "validation":
                tg = tv
            elif a.tokens == "columns":
                # tokens carried as tok_<name> columns of the generated file (probe BL's format)
                gp = pd.read_parquet(a.gen, columns=[f"tok_{n}" for n in names
                                                     if f"tok_{n}" in pd.read_parquet(a.gen).columns])
                if "draw" in gp:
                    gp = gp[gp["draw"] == 0]
                z = np.full((len(gp), len(names)), -1, np.int64)
                for j, n in enumerate(names):
                    if f"tok_{n}" in gp:
                        z[:, j] = np.nan_to_num(gp[f"tok_{n}"].to_numpy(float), nan=-1).astype(np.int64)
                tg = z[:, sh(z)].astype(np.int16) if len(z) == len(gen) else None
                if tg is None:
                    tt = None
            elif a.tokens:
                z = np.load(a.tokens)["tokens"]
                z = z[0] if z.ndim == 3 else z
                if z.shape[1] in (len(names), len(names) - 1) and len(z) == len(gen):
                    tg = z[:, sh(z)].astype(np.int16)
                else:
                    print(f"token shape {z.shape} does not match {len(names)} variables; token check skipped")
                    tt = None
        if "classifier_tokens" not in skip and tg is not None and tt is not None:
            # the same classifier on the rank-band tokens instead of dollars: no decoding noise
            cn = [names[c] for c in cols]
            gr_ = group_index(real["E00100"].to_numpy(float), EDGES_FOR_COPY)
            res["classifier_tokens"] = classifier(pd.DataFrame(tt, columns=cn), pd.DataFrame(tg, columns=cn),
                                                  wr, wg, gr_, gg, rng, feats=cn)
            print(f"token classifier done {time.time() - t0:.0f}s auc {res['classifier_tokens']['auc_overall']:.3f}",
                  flush=True)
            json.dump(res, open(a.out, "w"), indent=1, default=float)
        cp, dv, agiv = copying(real, wr, val, wv, gen, wg, gg, rng, tg, tt, tv)
        cp["validation_top01_nn_median"] = float(np.median(dv[gv == 4])) if (gv == 4).any() else float("nan")
        cp["validation_top01_n"] = int((gv == 4).sum())
        res["copying"] = cp
        print(f"copying done {time.time() - t0:.0f}s", flush=True)
    res["seconds"] = time.time() - t0
    json.dump(res, open(a.out, "w"), indent=1, default=float)
    print("written", a.out, flush=True)


if __name__ == "__main__":
    main()
