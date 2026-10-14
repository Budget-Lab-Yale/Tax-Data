"""Probe X (test 4): translate the SCF filer tax units of `full_v2` into PUF
2015 concepts, following D24, and write the dataset `full_v3x`.

    python x_translate.py            (Slurm only: sbatch x_cpu.sbatch x_translate.py)

Rules (one draw; every rule is listed in X_REPORT.md):

Reference population: PUF 2015 returns that are not dependent filers
(DSI = 0), all parts (train, val, test), own weights. The SCF 2013 and
2016 rows are translated separately, each onto this one PUF 2015
distribution. Wealth, weights and every unshared column stay as they are.

1. Filing status. Cells: age group (under 35, 35-64, 65+; SCF head's age,
   PUF primary filer's AGERANGE where known). Dependents are not a cell
   (they are not yet translated, and the SCF counts them differently).
   Moves allowed only within the married block (joint <-> separate) and
   within the unmarried block (single <-> head of household), so that the
   SCF's married share is kept, in number such that the shares within each
   block match the PUF's in the cell. Married movers at random; single to
   head of household takes units with dependents first, head of household
   to single takes units without dependents first (at random within each). Exemptions follow the spouse (joint
   to separate: one fewer; separate to joint: one more, top-coded at 5).
2. Age range. Within filing status, the SCF age (head at interview) is
   ranked by weight and mapped onto the PUF's AGERANGE shares (known ages)
   for that filing status: this is the head versus primary filer and
   interview-year adjustment, done by rank.
3. Dependents (exemptions less filer and spouse). Within filing status x
   age range: weighted rank (ties broken by children under 17, then at
   random) mapped onto the PUF's dependents distribution. Children under 17
   (N24): within filing status x age group x dependents (0,1,2,3+), same
   rule, then capped at dependents. Sex of head: within filing status x age
   group, random flips to the PUF's male share.
4. Wages, within filing status (single and separate together; joint; head
   of household) x age group. PUF records without an age get an age group
   drawn from the SCF's age-group shares within their filing status and
   income band (only to place them in cells).
5. Other shared incomes, in the order of INCOMES, within filing status x
   age group x wage band (zero; positive below the PUF's median, median to
   90th percentile, above).
   For wages and every income: the share positive (and, where the variable
   has losses, the share negative) is set to the PUF's in the cell by
   flipping records. The records flipped in are the most likely holders
   given the whole row (a logistic regression of holding on demographics,
   the other incomes and all 24 wealth components, fitted on the SCF wave);
   those flipped out are chosen at random within the cell (full_v3x2, the
   default; probe Y found removal by least likelihood biased) or the least
   likely (full_v3x, the first run). Among holders, amounts are
   mapped by weighted rank within the cell onto the PUF's distribution in
   the cell; records flipped in are placed below the SCF's own holders
   (ordered by their likelihood), on the evidence that the SCF's missing
   holders are small ones (similar totals, fewer holders).

Encoding: the SCF rows' shared amounts are encoded with the PUF 2015
quantile functions (they are now PUF 2015 amounts), so a translated SCF row
and a PUF return with the same amount get the same token. Wealth tokens and
the PUF rows are copied unchanged.

Outputs: $Q/data/full_v3x/ (full_v2's layout), $Q/translate/ (movement
tables, JSON summary).
"""

import json
import os
import shutil
import sys
import time

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler

POST = "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain"
sys.path.insert(0, POST)
import g_tokenizer as tk  # noqa: E402

SRC = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x"
# XVER=full_v3x: first run, holders switched off chosen least likely first.
# XVER=full_v3x2 (default): switched off at random within the cell (probe Y's
# finding; switching on stays most likely first).
XVER = os.environ.get("XVER", "full_v3x2")
OFF_RULE = "least_likely" if XVER == "full_v3x" else "random"
OUT = os.path.join(Q, "data", XVER)
TR = os.path.join(Q, "translate" if XVER == "full_v3x" else "translate_" + XVER)
SEED = 20261031

WEALTH = ["cash", "equities", "bonds", "dc", "db", "life_ins", "annuities", "trusts",
          "other_fin", "pass_throughs", "primary_home", "other_home", "re_fund",
          "other_nonfin", "primary_mortgage", "other_mortgage", "credit_lines",
          "credit_cards", "installment_debt", "other_debt", "kg_primary_home",
          "kg_other_re", "kg_pass_throughs", "kg_other"]
INCOMES = ["E00900", "E02000", "P23250", "E00600", "E00300", "E00400", "E01700", "E02400",
           "E02300", "E02100"]
AMOUNTS = ["E00200"] + INCOMES
T2_INC = ["t2_wages", "t2_business", "t2_int_div", "t2_capital_gains", "t2_rent", "t2_ss_pens",
          "t2_ui_other"]
INC_EDGES = [0.5, 0.9, 0.99]
INC_LABELS = ["<50", "50-90", "90-99", "top 1"]
MIN_REF = 30          # fewest PUF records a cell needs before falling back to a coarser one

LOG = []


def log(*a):
    s = " ".join(str(x) for x in a)
    print(s, flush=True)
    LOG.append(s)


# ---------------------------------------------------------------------------
# Weighted rank helpers
# ---------------------------------------------------------------------------

def midrank(keys, w):
    """Weighted mid-rank in (0, 1). keys: list of arrays, first is primary."""
    order = np.lexsort(tuple(reversed(keys)))
    cw = np.cumsum(w[order])
    mid = (cw - w[order] / 2) / cw[-1]
    r = np.empty(len(w))
    r[order] = mid
    return r


def ref_qf(x, w):
    o = np.argsort(x, kind="stable")
    xs, ws = x[o], w[o]
    cw = np.cumsum(ws)
    return (cw - ws / 2) / cw[-1], xs


def cat_map(u, cats, shares):
    """Map ranks u in (0,1) onto ordered categories with the given shares."""
    c = np.cumsum(shares) / np.sum(shares)
    return np.asarray(cats)[np.minimum(np.searchsorted(c, u, side="right"), len(cats) - 1)]


def select_prefix(idx_sorted, w, need):
    """First records of idx_sorted whose cumulative weight reaches `need`
    (a record is taken when the midpoint of its weight is within it)."""
    if need <= 0 or len(idx_sorted) == 0:
        return idx_sorted[:0]
    cw = np.cumsum(w[idx_sorted])
    k = int(np.searchsorted(cw - w[idx_sorted] / 2, need, side="right"))
    return idx_sorted[:k]


def slog(x):
    x = np.nan_to_num(np.asarray(x, float))
    return np.sign(x) * np.log1p(np.abs(x))


def age3_from_range(ar):
    out = np.full(len(ar), -1)
    out[np.isin(ar, [1, 2])] = 0
    out[np.isin(ar, [3, 4, 5])] = 1
    out[ar == 6] = 2
    return out


def age3_from_age(a):
    return np.where(a < 35, 0, np.where(a < 65, 1, 2))


def mars3(m):
    return np.where(m == 2, 1, np.where(m == 4, 2, 0))       # 0 single or separate, 1 joint, 2 head


def age_range(a):
    return np.searchsorted(np.array([26, 35, 45, 55, 65]), a, side="right") + 1


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    t0 = time.time()
    rng = np.random.default_rng(SEED)
    os.makedirs(TR, exist_ok=True)
    if os.path.exists(os.path.join(OUT, "DONE")):
        os.remove(os.path.join(OUT, "DONE"))
    os.makedirs(OUT, exist_ok=True)
    meta = json.load(open(os.path.join(SRC, "meta.json")))
    vnames = meta["variable_names"]
    shared = [v["name"] for v in meta["variables"] if v["group"] == "shared"]
    log("shared variables:", shared)
    expect = {"MARS", "DSI", "XTOT", "N24", "AGERANGE", "GENDER"} | set(AMOUNTS)
    if set(shared) != expect:
        raise SystemExit(f"shared variables differ from expected: {sorted(set(shared) ^ expect)}")

    parts = []
    for p in ("train", "val", "test"):
        r = pd.read_parquet(os.path.join(SRC, f"records_{p}.parquet"))
        r["part"] = p
        r["prow"] = np.arange(len(r))
        parts.append(r)
    df = pd.concat(parts, ignore_index=True)
    for c in ["weight", "source", "year"]:
        if df[c].isna().any():
            raise SystemExit(f"NA in {c}")

    puf = df[(df.source == "PUF") & (df.DSI == 0)].reset_index(drop=True)
    log(f"PUF reference (not dependent filers): {len(puf):,} records, weight {puf.weight.sum()/1e6:.2f}m")
    pw = puf.weight.to_numpy()
    p_mars = puf.MARS.to_numpy()
    p_ar = puf.AGERANGE.to_numpy()
    p_age3 = age3_from_range(np.nan_to_num(p_ar, nan=-1))
    p_known = p_age3 >= 0
    log(f"PUF reference without an age: {int((~p_known).sum()):,} records, "
        f"{pw[~p_known].sum()/pw.sum():.2%} of weight")
    if set(np.unique(p_ar[p_known])) - {1, 2, 3, 4, 5, 6}:
        raise SystemExit(f"PUF AGERANGE values {np.unique(p_ar[p_known])}")
    p_dep = np.maximum(puf.XTOT.to_numpy() - 1 - (p_mars == 2), 0)
    summary = {"puf_reference_records": int(len(puf)), "puf_reference_weight": float(pw.sum()),
               "puf_reference_no_age_weight_share": float(pw[~p_known].sum() / pw.sum()),
               "years": {}}

    # PUF income band (sum of shared amounts) for imputing an age group to
    # PUF records without one.
    def inc_sum(frame):
        return np.nansum(np.column_stack([frame[c].to_numpy(float) for c in AMOUNTS]), axis=1)

    p_inc = inc_sum(puf)
    p_incband = np.searchsorted(np.array([0.5, 0.9, 0.99, 0.999]), midrank([p_inc], pw), side="right")

    scf_all = df[df.source == "SCF"]
    s_inc_all = np.zeros(len(scf_all))
    s_band_all = np.zeros(len(scf_all), int)
    for y in (2013, 2016):
        m = (scf_all.year == y).to_numpy()
        s_inc_all[m] = inc_sum(scf_all[m])
        s_band_all[m] = np.searchsorted(np.array([0.5, 0.9, 0.99, 0.999]),
                                        midrank([s_inc_all[m]], scf_all.weight.to_numpy()[m]), side="right")
    s_age3_all = age3_from_age(scf_all.age.to_numpy())
    s_m3_all = mars3(scf_all.MARS.to_numpy())
    sw_all = scf_all.weight.to_numpy()
    p_age3_cell = p_age3.copy()
    imp_tab = []
    for m3 in range(3):
        for b in range(5):
            tgt = (~p_known) & (mars3(p_mars) == m3) & (p_incband == b)
            if not tgt.any():
                continue
            src = (s_m3_all == m3) & (s_band_all == b)
            sh = np.array([sw_all[src & (s_age3_all == a)].sum() for a in range(3)])
            if sh.sum() == 0:
                src = s_band_all == b
                sh = np.array([sw_all[src & (s_age3_all == a)].sum() for a in range(3)])
            sh = sh / sh.sum()
            p_age3_cell[tgt] = rng.choice(3, size=int(tgt.sum()), p=sh)
            imp_tab.append({"mars3": m3, "income_band": b, "puf_records": int(tgt.sum()),
                            "puf_weight_m": float(pw[tgt].sum() / 1e6), "scf_age3_shares": sh.round(3).tolist()})
    summary["puf_no_age_imputed_age_group_cells"] = imp_tab

    p_m3 = mars3(p_mars)
    p_wage = puf.E00200.to_numpy(float)
    wq_u, wq_x = ref_qf(p_wage[p_wage > 0], pw[p_wage > 0])
    wage_cut = np.interp([0.5, 0.9], wq_u, wq_x)
    log(f"PUF wage band cut points (median, 90th percentile among wage earners): {wage_cut.round(0)}")

    def wage_band(x):
        return np.where(x > 0, 1 + np.searchsorted(wage_cut, x, side="right"), 0)

    p_wb = wage_band(p_wage)

    out_scf = []
    moves_rows = []
    for y in (2013, 2016):
        s = df[(df.source == "SCF") & (df.year == y)].copy()
        n = len(s)
        w = s.weight.to_numpy()
        W = w.sum()
        ys = {"rows": int(n), "weight_m": float(W / 1e6)}
        log(f"\n===== SCF {y}: {n:,} rows, weight {W/1e6:.2f}m =====")
        for c in shared:
            s["orig_" + c] = s[c].to_numpy()
        age = s.age.to_numpy(float)
        if np.isnan(age).any():
            raise SystemExit(f"SCF {y}: {int(np.isnan(age).sum())} rows without age")
        mars = s.MARS.to_numpy().copy()
        xtot = s.XTOT.to_numpy().copy()
        if np.isnan(mars).any() or np.isnan(xtot).any():
            raise SystemExit("NA MARS/XTOT in SCF")
        dep0 = np.maximum(xtot - 1 - (mars == 2), 0)
        # income group for reporting: test 2's seven-income total, ranked in the wave
        t2 = s[T2_INC].to_numpy(float).sum(axis=1)
        igrp = np.searchsorted(np.array(INC_EDGES), midrank([t2], w), side="right")

        # ---------- 1. filing status ----------
        s_age3 = age3_from_age(age)
        hasdep = dep0 > 0
        p_hasdep = p_dep > 0
        cw_rows = []
        for a in range(3):
            pc = p_known & (p_age3 == a)
            sc = s_age3 == a
            for block, (lo, hi) in {"married": (2, 3), "unmarried": (1, 4)}.items():
                pb = pc & np.isin(p_mars, [lo, hi])
                sb = sc & np.isin(mars, [lo, hi])
                if not sb.any() or not pb.any():
                    continue
                q = pw[pb & (p_mars == hi)].sum() / pw[pb].sum()      # PUF share of the second status
                cur = w[sb & (mars == hi)].sum() / w[sb].sum()
                Wb = w[sb].sum()
                # order of movers: random within the married block; in the
                # unmarried block, single -> head of household takes units
                # with dependents first, head of household -> single takes
                # units without dependents first (random within each)
                if cur < q:
                    cand = np.flatnonzero(sb & (mars == lo))
                    key = -hasdep[cand].astype(float) if block == "unmarried" else np.zeros(len(cand))
                    cand = cand[np.lexsort((rng.random(len(cand)), key))]
                    sel = select_prefix(cand, w, (q - cur) * Wb)
                    mars[sel] = hi
                    direction = f"{lo}->{hi}"
                else:
                    cand = np.flatnonzero(sb & (mars == hi))
                    key = hasdep[cand].astype(float) if block == "unmarried" else np.zeros(len(cand))
                    cand = cand[np.lexsort((rng.random(len(cand)), key))]
                    sel = select_prefix(cand, w, (cur - q) * Wb)
                    mars[sel] = lo
                    direction = f"{hi}->{lo}"
                cw_rows.append({"year": y, "age_group": ["under 35", "35-64", "65+"][a], "block": block,
                                "puf_share_second": round(float(q), 4),
                                "scf_share_second_before": round(float(cur), 4),
                                "direction": direction,
                                "moved_weight_share_of_block": round(float(w[sel].sum() / Wb), 4),
                                "movers_with_dependents_share": round(float(w[sel][hasdep[sel]].sum() / max(w[sel].sum(), 1)), 4)})
        chg = mars != s.MARS.to_numpy()
        # exemptions follow the spouse
        j2s = chg & (s.MARS.to_numpy() == 2) & (mars == 3)
        s2j = chg & (s.MARS.to_numpy() == 3) & (mars == 2)
        xtot = np.where(j2s, np.maximum(xtot - 1, 1), xtot)
        xtot = np.where(s2j, np.minimum(xtot + 1, 5), xtot)
        ys["filing_status_changed_weight_share"] = float(w[chg].sum() / W)
        ys["filing_status_transitions"] = {f"{int(a)}->{int(b)}": float(w[chg & (s.MARS.to_numpy() == a) & (mars == b)].sum() / W)
                                           for a in (1, 2, 3, 4) for b in (1, 2, 3, 4)
                                           if a != b and (chg & (s.MARS.to_numpy() == a) & (mars == b)).any()}
        ys["filing_status_shares"] = {
            "puf": {int(k): float(pw[p_mars == k].sum() / pw.sum()) for k in (1, 2, 3, 4)},
            "scf_before": {int(k): float(w[s.MARS.to_numpy() == k].sum() / W) for k in (1, 2, 3, 4)},
            "scf_after": {int(k): float(w[mars == k].sum() / W) for k in (1, 2, 3, 4)}}
        pd.DataFrame(cw_rows).to_csv(os.path.join(TR, f"crosswalk_mars_{y}.csv"), index=False)
        log("filing status:", json.dumps(ys["filing_status_shares"]), "changed", round(ys["filing_status_changed_weight_share"], 4))

        # ---------- 2. age range ----------
        ar_old = age_range(age).astype(float)
        ar = np.zeros(n)
        tb = rng.random(n)
        for k in (1, 2, 3, 4):
            sc = mars == k
            pc = p_known & (p_mars == k)
            sh = np.array([pw[pc & (p_ar == c)].sum() for c in range(1, 7)])
            u = midrank([age[sc], tb[sc]], w[sc])
            ar[sc] = cat_map(u, np.arange(1, 7), sh)
        dmov = np.abs(ar - ar_old)
        ys["age_range_changed_weight_share"] = float(w[dmov > 0].sum() / W)
        ys["age_range_moved_2plus_weight_share"] = float(w[dmov >= 2].sum() / W)
        ys["age_range_shares"] = {"puf_known": [float(pw[p_known & (p_ar == c)].sum() / pw[p_known].sum()) for c in range(1, 7)],
                                  "scf_before": [float(w[ar_old == c].sum() / W) for c in range(1, 7)],
                                  "scf_after": [float(w[ar == c].sum() / W) for c in range(1, 7)]}
        # direction of the age moves (mean range code change)
        ys["age_range_mean_change_by_filing_status"] = {int(k): float(np.sum(w[mars == k] * (ar - ar_old)[mars == k]) / w[mars == k].sum())
                                                        for k in (1, 2, 3, 4)}
        log("age range:", json.dumps(ys["age_range_shares"]))

        # ---------- 3. dependents, children under 17, sex ----------
        dep_b = np.maximum(xtot - 1 - (mars == 2), 0)
        n24_old = s.N24.to_numpy(float)
        dep = np.zeros(n)
        for k in (1, 2, 3, 4):
            for c in range(1, 7):
                sc = (mars == k) & (ar == c)
                if not sc.any():
                    continue
                pc = p_known & (p_mars == k) & (p_ar == c)
                if pw[pc].sum() == 0:
                    pc = p_known & (p_mars == k)
                cats = np.arange(0, 5)
                sh = np.array([pw[pc & (p_dep == d)].sum() for d in cats])
                u = midrank([dep_b[sc], n24_old[sc], rng.random(int(sc.sum()))], w[sc])
                dep[sc] = cat_map(u, cats, sh)
        xtot_new = np.minimum(1 + (mars == 2) + dep, 5)
        dep = xtot_new - 1 - (mars == 2)
        ys["dependents_changed_weight_share"] = float(w[dep != dep0].sum() / W)
        ys["dependents_shares"] = {"puf": [float(pw[np.minimum(p_dep, 3) == d].sum() / pw.sum()) for d in range(4)],
                                   "scf_before": [float(w[np.minimum(dep0, 3) == d].sum() / W) for d in range(4)],
                                   "scf_after": [float(w[np.minimum(dep, 3) == d].sum() / W) for d in range(4)]}
        n24 = np.zeros(n)
        a3 = age3_from_range(ar)
        p_n24 = puf.N24.to_numpy(float)
        for k in (1, 2, 3, 4):
            for a in range(3):
                for d in range(4):
                    sc = (mars == k) & (a3 == a) & (np.minimum(dep, 3) == d)
                    if not sc.any():
                        continue
                    pc = p_known & (p_mars == k) & (p_age3 == a) & (np.minimum(p_dep, 3) == d)
                    if pw[pc].sum() == 0:
                        pc = (p_mars == k) & (np.minimum(p_dep, 3) == d)
                    sh = np.array([pw[pc & (p_n24 == c)].sum() for c in range(4)])
                    if sh.sum() == 0:
                        n24[sc] = 0
                        continue
                    u = midrank([n24_old[sc], dep_b[sc], rng.random(int(sc.sum()))], w[sc])
                    n24[sc] = cat_map(u, np.arange(4), sh)
        n24 = np.minimum(n24, dep)
        ys["n24_changed_weight_share"] = float(w[n24 != n24_old].sum() / W)
        g_old = s.GENDER.to_numpy(float)
        g = g_old.copy()
        p_g = puf.GENDER.to_numpy(float)
        for k in (1, 2, 3, 4):
            for a in range(3):
                sc = (mars == k) & (a3 == a) & ~np.isnan(g)
                pc = p_known & (p_mars == k) & (p_age3 == a) & ~np.isnan(p_g)
                if not sc.any() or pw[pc].sum() == 0:
                    continue
                q = pw[pc & (p_g == 1)].sum() / pw[pc].sum()
                cur = w[sc & (g == 1)].sum() / w[sc].sum()
                if cur > q:
                    cand = np.flatnonzero(sc & (g == 1))
                    cand = cand[rng.permutation(len(cand))]
                    g[select_prefix(cand, w, (cur - q) * w[sc].sum())] = 2
                else:
                    cand = np.flatnonzero(sc & (g == 2))
                    cand = cand[rng.permutation(len(cand))]
                    g[select_prefix(cand, w, (q - cur) * w[sc].sum())] = 1
        okg = ~np.isnan(g_old)
        ys["gender_changed_weight_share"] = float(w[okg & (g != g_old)].sum() / W)
        ys["male_share"] = {"puf_known": float(pw[p_g == 1].sum() / pw[~np.isnan(p_g)].sum()),
                            "scf_before": float(w[okg & (g_old == 1)].sum() / w[okg].sum()),
                            "scf_after": float(w[okg & (g == 1)].sum() / w[okg].sum())}
        log("dependents:", json.dumps(ys["dependents_shares"]), "male:", json.dumps(ys["male_share"]))

        # ---------- 4-5. incomes ----------
        m3 = mars3(mars)
        # features for the holding models: demographics, all shared incomes as
        # reported, all wealth, wealth-observed flag
        wobs = s.wealth_observed.to_numpy().astype(bool)
        base_feats = [np.eye(4)[(mars - 1).astype(int)], age[:, None], dep[:, None], n24[:, None],
                      wobs[:, None].astype(float)]
        wealth_f = np.column_stack([slog(s[c].to_numpy(float)) for c in WEALTH])
        inc_f = {c: slog(s["orig_" + c].to_numpy(float)) for c in AMOUNTS}

        def scores(v, target):
            """Predicted probability of `target` (bool over observed rows) given
            the row; logistic regression fitted on this wave."""
            ok = ~np.isnan(s["orig_" + v].to_numpy(float))
            X = np.column_stack(base_feats + [wealth_f] + [inc_f[c][:, None] for c in AMOUNTS if c != v])
            sc = np.full(n, np.nan)
            yv = target[ok]
            if yv.sum() < 30 or (~yv).sum() < 30:
                sc[ok] = rng.random(int(ok.sum()))
                return sc, None
            Xs = StandardScaler().fit_transform(X[ok])
            lr = LogisticRegression(C=1.0, max_iter=2000)
            lr.fit(Xs, yv, sample_weight=w[ok] / w[ok].mean())
            sc[ok] = lr.predict_proba(Xs)[:, 1]
            # discrimination, weighted AUC approximation by ranks
            from sklearn.metrics import roc_auc_score
            auc = roc_auc_score(yv, sc[ok], sample_weight=w[ok])
            return sc, float(auc)

        new_vals = {}
        wage_new = None
        inc_rep = {}
        for v in AMOUNTS:
            x0 = s["orig_" + v].to_numpy(float)
            ok = ~np.isnan(x0)
            px = puf[v].to_numpy(float)
            pok = ~np.isnan(px)
            has_neg = bool((px[pok] < 0).any())
            sp, auc_p = scores(v, x0 > 0)
            sn, auc_n = scores(v, x0 < 0) if has_neg else (np.zeros(n), None)
            if v == "E00200":
                cell = m3 * 3 + a3
                pcell = p_m3 * 3 + p_age3_cell
                coarse, pcoarse = m3, p_m3
            else:
                wb = wage_band(wage_new)
                cell = (m3 * 3 + a3) * 4 + wb
                pcell = (p_m3 * 3 + p_age3_cell) * 4 + p_wb
                coarse, pcoarse = m3 * 4 + wb, p_m3 * 4 + p_wb
            xn = np.full(n, np.nan)
            fallback = 0
            for c in np.unique(cell[ok]):
                idx = np.flatnonzero(ok & (cell == c))
                pc = pok & (pcell == c)
                if pc.sum() < MIN_REF * 3:
                    pc = pok & (pcoarse == coarse[idx[0]])
                    fallback += len(idx)
                Wc = w[idx].sum()
                pW = pw[pc].sum()
                p_pos = pw[pc & (px > 0)].sum() / pW
                p_neg = pw[pc & (px < 0)].sum() / pW
                xi = x0[idx]
                pos = xi > 0
                # positives
                cur = w[idx][pos].sum()
                newpos = pos.copy()
                if cur < p_pos * Wc:
                    cand = np.flatnonzero(xi == 0)
                    cand = cand[np.argsort(-sp[idx][cand], kind="stable")]
                    newpos[select_prefix(cand, w[idx], p_pos * Wc - cur)] = True
                else:
                    cand = np.flatnonzero(pos)
                    cand = cand[np.argsort(sp[idx][cand], kind="stable")] if OFF_RULE == "least_likely" \
                        else cand[rng.permutation(len(cand))]
                    newpos[select_prefix(cand, w[idx], cur - p_pos * Wc)] = False
                newneg = (xi < 0) & ~newpos
                if has_neg:
                    curn = w[idx][newneg].sum()
                    if curn < p_neg * Wc:
                        cand = np.flatnonzero(~newpos & ~newneg)
                        cand = cand[np.argsort(-sn[idx][cand], kind="stable")]
                        newneg[select_prefix(cand, w[idx], p_neg * Wc - curn)] = True
                    else:
                        cand = np.flatnonzero(newneg)
                        cand = cand[np.argsort(sn[idx][cand], kind="stable")] if OFF_RULE == "least_likely" \
                            else cand[rng.permutation(len(cand))]
                        newneg[select_prefix(cand, w[idx], curn - p_neg * Wc)] = False
                out = np.zeros(len(idx))
                # amounts among holders, by rank within the cell
                for side, sel, scr in (("pos", newpos, sp), ("neg", newneg, sn)):
                    if not sel.any():
                        continue
                    pref = pc & ((px > 0) if side == "pos" else (px < 0))
                    if pref.sum() < MIN_REF:
                        pref = pok & (pcoarse == coarse[idx[0]]) & ((px > 0) if side == "pos" else (px < 0))
                    if pref.sum() < MIN_REF:
                        pref = pok & ((px > 0) if side == "pos" else (px < 0))
                    mag0 = np.abs(xi[sel])
                    own = ((xi[sel] > 0) if side == "pos" else (xi[sel] < 0)).astype(float)
                    u = midrank([own, np.where(own > 0, mag0, 0.0), scr[idx][sel]], w[idx][sel])
                    ru, rx = ref_qf(np.abs(px[pref]), pw[pref])
                    val = np.interp(u, ru, rx)
                    out[sel] = val if side == "pos" else -val
                xn[idx] = out
            if v == "E00200":
                wage_new = np.nan_to_num(xn)
            new_vals[v] = xn
            # movement
            r = {"variable": v, "year": y, "auc_holding_model": auc_p, "auc_loss_model": auc_n,
                 "rows_in_coarser_cell": int(fallback)}
            wo = w[ok]
            a, b = x0[ok], xn[ok]
            Wo = wo.sum()
            r["share_positive_puf"] = float(pw[pok & (px > 0)].sum() / pw[pok].sum())
            r["share_positive_before"] = float(wo[a > 0].sum() / Wo)
            r["share_positive_after"] = float(wo[b > 0].sum() / Wo)
            r["share_negative_puf"] = float(pw[pok & (px < 0)].sum() / pw[pok].sum())
            r["share_negative_before"] = float(wo[a < 0].sum() / Wo)
            r["share_negative_after"] = float(wo[b < 0].sum() / Wo)
            r["flip_zero_to_pos"] = float(wo[(a == 0) & (b > 0)].sum() / Wo)
            r["flip_pos_to_zero"] = float(wo[(a > 0) & (b == 0)].sum() / Wo)
            r["flip_to_neg"] = float(wo[(a >= 0) & (b < 0)].sum() / Wo)
            r["flip_neg_to_zero"] = float(wo[(a < 0) & (b >= 0)].sum() / Wo)
            r["total_bn_puf"] = float(np.sum(pw[pok] * px[pok]) / 1e9)
            r["total_bn_before_own_dollars"] = float(np.sum(wo * a) / 1e9)
            r["total_bn_after"] = float(np.sum(wo * b) / 1e9)
            r["dollars_of_flipped_out_share_of_positive_total"] = float(
                np.sum(wo * a * ((a > 0) & (b <= 0))) / max(np.sum(wo * a * (a > 0)), 1))
            # rank shift among records positive before and after: weighted rank
            # among the SCF wave's positives against rank among PUF positives
            both = (a > 0) & (b > 0)
            if both.any():
                ro = midrank([a[a > 0]], wo[a > 0])
                r_old = np.full(len(a), np.nan)
                r_old[a > 0] = ro
                pu, pxs = ref_qf(px[pok & (px > 0)], pw[pok & (px > 0)])
                r_new = np.interp(b, pxs, pu)
                sh = np.abs(r_new - r_old)[both]
                ig = igrp[ok][both]
                ww = wo[both]
                r["rank_shift_mean"] = float(np.sum(ww * sh) / ww.sum())
                r["rank_shift_over_25pts_share"] = float(ww[sh > 0.25].sum() / ww.sum())
                r["rank_shift_over_40pts_share"] = float(ww[sh > 0.40].sum() / ww.sum())
                for gi, gl in enumerate(INC_LABELS):
                    mg = ig == gi
                    r[f"rank_shift_mean_{gl}"] = float(np.sum(ww[mg] * sh[mg]) / ww[mg].sum()) if mg.any() else np.nan
                    r[f"rank_shift_over_25pts_{gl}"] = float(ww[mg & (sh > 0.25)].sum() / ww[mg].sum()) if mg.any() else np.nan
                r["rank_signed_shift_mean"] = float(np.sum(ww * (r_new - r_old)[both]) / ww.sum())
            moves_rows.append(r)
            log(f"{v}: pos {r['share_positive_before']:.3f}->{r['share_positive_after']:.3f} (PUF {r['share_positive_puf']:.3f}); "
                f"neg {r['share_negative_before']:.3f}->{r['share_negative_after']:.3f} (PUF {r['share_negative_puf']:.3f}); "
                f"total {r['total_bn_before_own_dollars']:.0f}->{r['total_bn_after']:.0f}bn (PUF {r['total_bn_puf']:.0f}); "
                f"rank shift {r.get('rank_shift_mean', np.nan):.3f}; AUC {auc_p}")
            if not (np.isnan(xn) == np.isnan(x0)).all():
                raise SystemExit(f"{v}: NaN pattern changed")

        # flip-ins by wealth: e.g. dividends flipped in, mean equities
        s["MARS"] = mars
        s["XTOT"] = xtot_new
        s["N24"] = n24
        s["AGERANGE"] = ar
        s["GENDER"] = g
        for v in AMOUNTS:
            s[v] = new_vals[v]
        # income-group view of the moves (seven-income total groups)
        for gi, gl in enumerate(INC_LABELS):
            mg = igrp == gi
            moves_rows.append({"variable": "_demographics", "year": y, "income_group": gl,
                               "filing_status_changed": float(w[mg & chg].sum() / w[mg].sum()),
                               "age_range_changed": float(w[mg & (dmov > 0)].sum() / w[mg].sum()),
                               "dependents_changed": float(w[mg & (dep != dep0)].sum() / w[mg].sum())})
        summary["years"][str(y)] = ys
        out_scf.append(s)

    moves = pd.DataFrame(moves_rows)
    moves.to_csv(os.path.join(TR, "moves.csv"), index=False)
    pd.set_option("display.width", 250)
    pd.set_option("display.max_columns", 60)
    log(moves.to_string())

    # ---------- write the dataset ----------
    scf_new = pd.concat(out_scf)
    df2 = df.copy()
    for c in shared:
        df2["orig_" + c] = np.nan
    df2.loc[scf_new.index, scf_new.columns] = scf_new
    is_scf = (df2.source == "SCF").to_numpy()
    q = pd.read_parquet(os.path.join(SRC, "quantiles.parquet"))
    cache = tk._qf_cache(q)
    enc_in = df2.loc[is_scf, ["source", "year"] + vnames[1:]].copy()
    enc_in["source"] = "PUF"
    enc_in["year"] = 2015
    sv = [v["name"] for v in meta["variables"] if v["group"] != "shared" and v["name"] != "source"]
    for c in sv:
        enc_in[c] = np.nan
    new_tok = tk.encode(enc_in, meta, q, np.random.default_rng(SEED + 1), cache=cache)
    shared_cols = [vnames.index(c) for c in shared]
    scf_idx = np.flatnonzero(is_scf)
    pos_in_scf = {r: i for i, r in enumerate(scf_idx)}
    changed = {}
    for p in ("train", "val", "test"):
        tok = np.load(os.path.join(SRC, f"tokens_{p}.npy"))
        tru = np.load(os.path.join(SRC, f"truth_{p}.npy"))
        assert (tok == tru).all()
        pm = (df2.part == p).to_numpy()
        rows_p = np.flatnonzero(pm)
        assert (df2.prow.to_numpy()[rows_p] == np.arange(len(rows_p))).all()
        sc_rows = rows_p[is_scf[rows_p]]
        local = df2.prow.to_numpy()[sc_rows]
        nt = tok.copy()
        rows_new = new_tok[[pos_in_scf[r] for r in sc_rows]]
        for j in shared_cols:
            before = nt[local, j]
            after = rows_new[:, j]
            if not ((before < 0) == (after < 0)).all():
                raise SystemExit(f"{vnames[j]} {p}: observed pattern changed")
            changed[f"{p}|{vnames[j]}"] = float(np.mean(before != after))
            nt[local, j] = after
        # nothing else changed
        other = [k for k in range(tok.shape[1]) if k not in shared_cols]
        assert (nt[:, other] == tok[:, other]).all()
        assert (nt[~is_scf[rows_p]] == tok[~is_scf[rows_p]]).all()
        np.save(os.path.join(OUT, f"tokens_{p}.npy"), nt)
        np.save(os.path.join(OUT, f"truth_{p}.npy"), nt)
        rec = df2.loc[pm].drop(columns=["part", "prow"]).reset_index(drop=True)
        rec.to_parquet(os.path.join(OUT, f"records_{p}.parquet"), index=False)
        log(f"{p}: {len(rec):,} rows written")
    shutil.copy(os.path.join(SRC, "quantiles.parquet"), os.path.join(OUT, "quantiles.parquet"))
    meta2 = dict(meta)
    meta2["dataset"] = XVER
    meta2["description"] = meta["description"] + (
        " Probe X (test 4): the SCF rows' shared variables translated into PUF 2015 concepts (D24) by "
        "research/rebuild/tests/probes/x_translate.py; SCF shared amounts encoded with the PUF 2015 "
        "quantile functions; wealth, weights and PUF rows unchanged.")
    meta2["translation"] = {"script": "research/rebuild/tests/probes/x_translate.py", "seed": SEED,
                            "rules": __doc__}
    with open(os.path.join(OUT, "meta.json"), "w") as f:
        json.dump(meta2, f, indent=1)
    summary["share_of_scf_tokens_changed"] = changed
    summary["log"] = LOG
    with open(os.path.join(TR, "summary.json"), "w") as f:
        json.dump(summary, f, indent=1, default=float)
    with open(os.path.join(OUT, "DONE"), "w") as f:
        f.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} job {os.environ.get('SLURM_JOB_ID')} x_translate.py\n")
    log(f"done in {time.time()-t0:.0f}s")


if __name__ == "__main__":
    main()
