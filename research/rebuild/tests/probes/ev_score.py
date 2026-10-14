"""Probe EV (test 4, round 4): one scorer for completed wealth, the same for
any method that produces draws (the transformer, sequential trees).

    python ev_score.py scf --label "<method> | <run>" --file <parquet> --out <json>
    python ev_score.py scf --truth-only --out <json>
    python ev_score.py puf --label "<method> | <run>" --file <parquet> --out <json>
    python ev_score.py puf --truth-only --out <json>

Input files are in probe C's format (h_complete.py): one row per record and
draw, columns `row` (the record's row in full_v3cz's records_<split>.parquet),
`draw`, and the 24 wealth variables in dollars. `scf` files hold SCF test
records (split test of full_v3cz), `puf` files PUF training records.

scf: SCF test records with wealth observed (split-off units have no wealth
and are left out), true wealth from records_test.parquet. Groups: income
(the seven-income total of test 2 within each wave, thresholds at weighted
ranks 0.5/0.9/0.99/0.999 among all SCF training rows of the wave, as probes
Q and X did), age of the head (six ranges) and filing status (MARS). Both
waves pooled with their own weights. Every statistic weights a draw by its
record's weight over the number of draws. Untargeted relationships follow
probe Q (q_eval.py). Per-record score: the energy score on rank-transformed
wealth vectors (see energy()). Sampling error: 100 bootstrap resamples of
SCF households (all implicates and units of a household together, same
resamples for every file because the generator is seeded identically); for
each resample the truth and the file's draws are both recomputed on the
resampled records, so the collector can pair files by resample.

puf: probe X/C's comparison (c3_wealth.py): completed wealth by AGI group
against the SCF 2013 and 2016 training records interpolated to 2015 (one
third 2013, two thirds 2016, dollar levels in logs). Split-off units: the
SCF benchmark covers units with wealth observed; the all-returns mean and
median are also given with split-off units counted at zero (as X did). The
SCF's sampling error: 100 bootstrap resamples of SCF households.

Only weighted aggregates are written.
"""
import argparse
import json
import sys
import time

import numpy as np
import pandas as pd

D3 = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/full_v3cz"
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
KG = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
AD = ASSETS + DEBTS
WEALTH = AD + KG
T2_INC = ["t2_wages", "t2_business", "t2_int_div", "t2_capital_gains", "t2_rent", "t2_ss_pens", "t2_ui_other"]
# the eleven incomes the SCF measures, in PUF terms (probe Q's list; full_v3cz keeps the totals in the records)
INCOMES = ["E00200", "E00900", "E02000", "P23250", "E00600", "E00300", "E01700", "E02400", "E02100", "E00400",
           "E02300"]
INC_NAMES = ["wages", "Schedule C", "Schedule E", "long-term gains", "dividends", "taxable interest",
             "taxable pensions", "Social Security", "farm", "tax-exempt interest", "unemployment"]
EDGES = [0.5, 0.9, 0.99, 0.999]
INC_LABELS = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
AGE_EDGES = [35, 45, 55, 65, 75]
AGE_LABELS = ["under 35", "35-44", "45-54", "55-64", "65-74", "75 and over"]
MAR_LABELS = ["single", "married filing jointly", "married filing separately", "head of household"]
HOLD = ["primary_home", "primary_mortgage", "equities", "bonds", "dc", "pass_throughs", "other_home", "cash",
        "credit_cards"]
MAIN = ["primary_home", "equities", "pass_throughs", "dc"]
NAMED = [("primary_mortgage", "primary_home", "mortgage among homeowners"),
         ("equities", "pass_throughs", "stocks among business owners"),
         ("kg_pass_throughs", "pass_throughs", "business gains among business owners"),
         ("other_mortgage", "other_home", "mortgage among owners of other homes")]
B = 100


def wrank(x, w):
    o = np.argsort(x, kind="mergesort")
    cw = np.cumsum(w[o])
    r = np.empty(len(x))
    r[o] = (cw - w[o] / 2) / cw[-1]
    return r


def wq(x, w, q):
    if w.sum() <= 0:
        return float("nan")
    o = np.argsort(x, kind="mergesort")
    cw = np.cumsum(w[o]) / w.sum()
    return float(x[o][min(np.searchsorted(cw, q), len(x) - 1)])


def topshare(x, w, p):
    if w.sum() <= 0:
        return float("nan")
    o = np.argsort(-x, kind="mergesort")
    cw = np.cumsum(w[o]) / w.sum()
    k = cw <= p
    return float(np.sum((w * x)[o][k]) / np.sum(w * x))


def wspearman(x, y, w):
    k = w > 0
    x, y, w = x[k], y[k], w[k]
    rx, ry = wrank(x, w), wrank(y, w)
    mx, my = (w * rx).sum() / w.sum(), (w * ry).sum() / w.sum()
    cov = (w * (rx - mx) * (ry - my)).sum()
    return float(cov / np.sqrt((w * (rx - mx) ** 2).sum() * (w * (ry - my) ** 2).sum()))


def nw_of(X):
    """X [..., 24] in WEALTH order."""
    return X[..., :len(ASSETS)].sum(-1) - X[..., len(ASSETS):len(AD)].sum(-1)


# ----------------------------------------------------------------------------- SCF test records
def scf_truth():
    rtr = pd.read_parquet(f"{D3}/records_train.parquet")
    rte = pd.read_parquet(f"{D3}/records_test.parquet")
    thr = {}
    for y in (2013, 2016):
        s = rtr[(rtr.source.astype(str) == "SCF") & (rtr.year == y)]
        t = s[T2_INC].to_numpy(float).sum(1)
        w = s.weight.to_numpy(float)
        r = wrank(t, w)
        o = np.argsort(r)
        thr[y] = [float(t[o][np.searchsorted(r[o], e, side="left")]) for e in EDGES]
    m = ((rte.source.astype(str) == "SCF") & (rte.wealth_observed.fillna(False).astype(bool))).to_numpy()
    rows = np.flatnonzero(m)
    s = rte.iloc[rows].reset_index(drop=True)
    yr = s.year.to_numpy().astype(int)
    t = s[T2_INC].to_numpy(float).sum(1)
    g_inc = np.zeros(len(s), np.int64)
    for y in (2013, 2016):
        k = yr == y
        g_inc[k] = np.searchsorted(np.array(thr[y]), t[k], side="right")
    g_age = np.searchsorted(AGE_EDGES, s.age.to_numpy(float), side="right")
    g_mar = s.MARS.to_numpy().astype(int) - 1
    inc = s[INCOMES].to_numpy(float)
    X = s[WEALTH].to_numpy(float)
    if np.isnan(X).any():
        raise SystemExit("true wealth has NA on SCF test records with wealth observed")
    hh = pd.factorize(pd.Series(yr).astype(str) + "_" + s.hh_id.astype(str))[0]
    return dict(rows=rows, w=s.weight.to_numpy(float), g_inc=g_inc, g_age=g_age, g_mar=g_mar, inc=inc, X=X,
                hh=hh, thr=thr, year=yr)


def scf_stats(X, w, T):
    """X [N, K, 24] dollars, w [N] record weights (bootstrap multiplier
    included). Returns statistics (arrays)."""
    N, K, _ = X.shape
    R = X.reshape(N * K, -1)
    ww = np.repeat(w / K, K)
    rep = lambda a: np.repeat(a, K, axis=0)      # noqa: E731
    gi, ga, gm = rep(T["g_inc"]), rep(T["g_age"]), rep(T["g_mar"])
    nw = nw_of(R)
    hold = R != 0
    s = {}
    for fam, g, labs in (("inc", gi, INC_LABELS), ("age", ga, AGE_LABELS), ("mar", gm, MAR_LABELS)):
        G = len(labs)
        den = np.bincount(g, weights=ww, minlength=G)
        dd = np.maximum(den, 1e-300)
        s[f"{fam}_w"] = den
        s[f"{fam}_nw_mean"] = np.bincount(g, weights=ww * nw, minlength=G) / dd
        s[f"{fam}_nw_median"] = np.array([wq(nw[g == k], ww[g == k], 0.5) for k in range(G)])
        s[f"{fam}_hold"] = np.stack([np.bincount(g, weights=ww * hold[:, WEALTH.index(h)], minlength=G)
                                     for h in HOLD], 1) / dd[:, None]
        s[f"{fam}_main_mean"] = np.stack([np.bincount(g, weights=ww * R[:, WEALTH.index(h)], minlength=G)
                                          for h in MAIN], 1) / dd[:, None]
    s["all_nw_mean"] = np.array([np.sum(ww * nw) / ww.sum()])
    s["all_nw_median"] = np.array([wq(nw, ww, 0.5)])
    s["all_hold"] = np.array([np.sum(ww * hold[:, WEALTH.index(h)]) / ww.sum() for h in HOLD])
    # untargeted (probe Q)
    na = len(AGE_LABELS)
    c = gi * na + ga
    den_c = np.bincount(c, weights=ww, minlength=5 * na)
    s["cell_w"] = den_c
    ia = [WEALTH.index(v) for v in AD]
    s["hold_inc_age"] = np.stack([np.bincount(c, weights=ww * hold[:, k], minlength=5 * na) for k in ia], 1) \
        / np.maximum(den_c, 1e-300)[:, None]
    s["nw_inc_age"] = np.bincount(c, weights=ww * nw, minlength=5 * na) / np.maximum(den_c, 1e-300)
    H = hold[:, ia].astype(float)
    J = (H * ww[:, None]).T @ H / ww.sum()
    iu = np.triu_indices(len(ia), 1)
    s["pair_joint"] = J[iu]
    s["pair_joint_by_inc"] = np.stack([((H[gi == k] * ww[gi == k, None]).T @ H[gi == k]
                                        / max(ww[gi == k].sum(), 1e-300))[iu] for k in range(5)])
    hv = lambda v: hold[:, WEALTH.index(v)]      # noqa: E731
    s["named"] = np.array([(ww * hv(a) * hv(b)).sum() / max((ww * hv(b)).sum(), 1e-300) for a, b, _ in NAMED])
    s["named_by_inc"] = np.array([[(ww * hv(a) * hv(b) * (gi == k)).sum() / max((ww * hv(b) * (gi == k)).sum(), 1e-300)
                                   for k in range(5)] for a, b, _ in NAMED[:1]])[0]
    inc = rep(T["inc"])
    cr = []
    for j in range(len(INCOMES)):
        y = inc[:, j]
        ok = ~np.isnan(y)
        cr.append(wspearman(nw[ok], y[ok], ww[ok]))
    s["corr"] = np.array(cr)
    s["top1_share_inc"] = np.array([topshare(nw[gi == k], ww[gi == k], 0.01) for k in range(5)])
    return s


def scf_compare(m, t):
    e = {}
    for fam in ("inc", "age", "mar"):
        cw = t[f"{fam}_w"] / t[f"{fam}_w"].sum()
        ok = t[f"{fam}_w"] > 0
        e[f"{fam}_nw_mean_pct"] = float((cw[ok] * 100 * np.abs(m[f"{fam}_nw_mean"][ok] / t[f"{fam}_nw_mean"][ok] - 1)).sum())
        e[f"{fam}_nw_median_pct"] = float((cw[ok] * 100 * np.abs(m[f"{fam}_nw_median"][ok] / t[f"{fam}_nw_median"][ok] - 1)).sum())
        # unweighted over groups too, so the top groups count
        e[f"{fam}_nw_mean_pct_groups"] = float(np.mean(100 * np.abs(m[f"{fam}_nw_mean"][ok] / t[f"{fam}_nw_mean"][ok] - 1)))
        e[f"{fam}_nw_median_pct_groups"] = float(np.mean(100 * np.abs(m[f"{fam}_nw_median"][ok] / t[f"{fam}_nw_median"][ok] - 1)))
        e[f"{fam}_hold_pp"] = float(100 * np.abs(m[f"{fam}_hold"][ok] - t[f"{fam}_hold"][ok]).mean())
        e[f"{fam}_main_mean_pct"] = float(np.mean(100 * np.abs(m[f"{fam}_main_mean"][ok] / np.maximum(t[f"{fam}_main_mean"][ok], 1) - 1)))
    cw = t["cell_w"] / t["cell_w"].sum()
    d = np.abs(m["hold_inc_age"] - t["hold_inc_age"]).mean(1)
    e["hold_inc_age_pp"] = float(100 * (cw * d).sum())
    na = len(AGE_LABELS)
    dn = np.abs(m["nw_inc_age"] - t["nw_inc_age"]).reshape(5, na)
    cwi = cw.reshape(5, na)
    by = np.array([(cwi[g] * dn[g]).sum() / max(cwi[g].sum(), 1e-300) for g in range(5)])
    iw = t["inc_w"] / t["inc_w"].sum()
    e["nw_inc_age_pct"] = float((iw * 100 * by / t["inc_nw_mean"]).sum())
    e["nw_inc_age_pct_groups"] = float(np.mean(100 * by / t["inc_nw_mean"]))
    e["pair_joint_pp"] = float(100 * np.abs(m["pair_joint"] - t["pair_joint"]).mean())
    e["pair_joint_inc_pp"] = float(100 * np.abs(m["pair_joint_by_inc"] - t["pair_joint_by_inc"]).mean())
    e["mortgage_homeowners_inc_pp"] = float(100 * np.abs(m["named_by_inc"] - t["named_by_inc"]).mean())
    e["corr_abs"] = float(np.abs(m["corr"] - t["corr"]).mean())
    e["top1_share_pp"] = float(100 * np.abs(m["top1_share_inc"] - t["top1_share_inc"]).mean())
    e["mortgage_homeowners_pp"] = float(100 * abs(m["named"][0] - t["named"][0]))
    e["named_pp"] = float(100 * np.abs(m["named"] - t["named"]).mean())
    return e


def rank_transform(X, Xt, w):
    """Map dollars to the truth's weighted mid-rank (0..1), per column.
    X [..., J], Xt [N, J] truth, w [N]."""
    out = np.empty(X.shape, float)
    for j in range(X.shape[-1]):
        o = np.argsort(Xt[:, j], kind="mergesort")
        v = Xt[o, j]
        cw = np.concatenate([[0.0], np.cumsum(w[o])]) / w.sum()
        x = X[..., j]
        lo = cw[np.searchsorted(v, x, side="left")]
        hi = cw[np.searchsorted(v, x, side="right")]
        out[..., j] = (lo + hi) / 2
    return out


def energy(U, u):
    """Energy score per record. U [N, K, J] draws, u [N, J] truth (rank scale).
    ES = mean_k |U_k - u| - 1/(2 K^2) sum_{k,l} |U_k - U_l| (Euclidean)."""
    d1 = np.sqrt(((U - u[:, None, :]) ** 2).sum(-1)).mean(1)
    K = U.shape[1]
    d2 = np.zeros(len(U))
    for a in range(K):
        for b in range(K):
            if a != b:
                d2 += np.sqrt(((U[:, a] - U[:, b]) ** 2).sum(-1))
    return d1 - d2 / (2 * K * K)


def load_draws(path, rows, kind):
    C = pd.read_parquet(path, columns=["row", "draw"] + WEALTH)
    C = C[C.row.isin(rows)]
    K = int(C.draw.nunique())
    if len(C) != len(rows) * K:
        raise SystemExit(f"{path}: {len(C)} rows for {len(rows)} records x {K} draws")
    if C[WEALTH].isna().any().any():
        raise SystemExit(f"{path}: NA wealth in draws: {C[WEALTH].isna().sum()[lambda x: x > 0].to_dict()}")
    pos = pd.Series(np.arange(len(rows)), index=rows)
    C = C.assign(_i=pos.loc[C.row].to_numpy()).sort_values(["_i", "draw"])
    X = C[WEALTH].to_numpy(float).reshape(len(rows), K, len(WEALTH))
    return X, K


def boot_mult(hh, b):
    rng = np.random.default_rng(1000 + b)
    nh = hh.max() + 1
    cnt = np.bincount(rng.integers(0, nh, nh), minlength=nh)
    return cnt[hh].astype(float)


def tolist(s):
    return {k: np.asarray(v).tolist() for k, v in s.items()}


def run_scf(a):
    T = scf_truth()
    w = T["w"]
    Xt = T["X"][:, None, :]
    ts = scf_stats(Xt, w, T)
    out = dict(kind="scf", n_records=int(len(w)), n_households=int(T["hh"].max() + 1),
               thresholds={str(k): v for k, v in T["thr"].items()}, truth=tolist(ts))
    ut = rank_transform(T["X"][:, :len(AD)], T["X"][:, :len(AD)], w)
    nwt = nw_of(T["X"])[:, None]
    unt = rank_transform(nwt, nwt, w)
    if a.truth_only:
        # truth's sampling error of each statistic, and a baseline for the per-record score:
        # draws of other households' whole wealth vectors at random (weighted), five per record
        boots = [scf_stats(Xt, w * boot_mult(T["hh"], b), T) for b in range(B)]
        out["truth_boot_sd"] = {k: np.std([bb[k] for bb in boots], axis=0).tolist() for k in ts}
        rng = np.random.default_rng(7)
        idx = rng.choice(len(w), size=(len(w), 5), p=w / w.sum())
        Xm = T["X"][idx]
        es = energy(rank_transform(Xm[..., :len(AD)], T["X"][:, :len(AD)], w), ut)
        crps = energy(rank_transform(nw_of(Xm)[..., None], nwt, w), unt)
        out["baseline_marginal"] = dict(energy=float(np.sum(w * es) / w.sum()), nw_score=float(np.sum(w * crps) / w.sum()),
                                        compare=scf_compare(scf_stats(Xm, w, T), ts))
        json.dump(out, open(a.out, "w"), indent=1, default=float)
        print("truth written", a.out, flush=True)
        return
    X, K = load_draws(a.file, T["rows"], "scf")
    ms = scf_stats(X, w, T)
    U = rank_transform(X[..., :len(AD)], T["X"][:, :len(AD)], w)
    es = energy(U, ut)
    crps = energy(rank_transform(nw_of(X)[..., None], nwt, w), unt)
    out.update(label=a.label, file=a.file, draws=K, levels=tolist(ms), compare=scf_compare(ms, ts),
               energy=float(np.sum(w * es) / w.sum()), nw_score=float(np.sum(w * crps) / w.sum()),
               energy_by_inc=[float(np.sum((w * es)[T["g_inc"] == k]) / np.sum(w[T["g_inc"] == k])) for k in range(5)])
    # bootstrap: same household resamples for every file
    bc, be, bn = [], [], []
    for b in range(B):
        mlt = boot_mult(T["hh"], b)
        wb = w * mlt
        bc.append(scf_compare(scf_stats(X, wb, T), scf_stats(Xt, wb, T)))
        be.append(float(np.sum(wb * es) / wb.sum()))
        bn.append(float(np.sum(wb * crps) / wb.sum()))
    out["boot"] = dict(compare={k: [d[k] for d in bc] for k in bc[0]}, energy=be, nw_score=bn)
    json.dump(out, open(a.out, "w"), indent=1, default=float)
    print(a.label, "energy", round(out["energy"], 4), "nw score", round(out["nw_score"], 4),
          json.dumps({k: round(v, 3) for k, v in out["compare"].items()}), flush=True)


# ----------------------------------------------------------------------------- PUF records
HOLD_P = ["primary_home", "equities", "pass_throughs", "bonds", "dc", "primary_mortgage"]


def puf_group_stats(nw, w, g, hold):
    out = {"mean": float(np.sum(w * nw) / w.sum()), "median": wq(nw, w, 0.5),
           "top1_share": topshare(nw, w, 0.01), "top01_share": topshare(nw, w, 0.001)}
    tot = np.sum(w * nw)
    labs = INC_LABELS + ["Top 1%"]
    for k, lab in enumerate(labs):
        m = (g >= 3) if lab == "Top 1%" else (g == k)
        out[f"{lab}|mean"] = float(np.sum(w[m] * nw[m]) / w[m].sum())
        out[f"{lab}|median"] = wq(nw[m], w[m], 0.5)
        out[f"{lab}|share"] = float(np.sum(w[m] * nw[m]) / tot)
        for h, x in hold.items():
            out[f"{lab}|hold_{h}"] = float(w[m][x[m] != 0].sum() / w[m].sum())
    for h, x in hold.items():
        out[f"hold_{h}"] = float(w[x != 0].sum() / w.sum())
    return out


def scf_wave_stats(rec, y, mult=None):
    s = rec[(rec.source.astype(str) == "SCF") & (rec.year == y)]
    t2 = s[T2_INC].to_numpy(float).sum(1)
    w_all = s.weight.to_numpy(float)
    if mult is not None:
        w_all = w_all * mult[y]
    g_all = np.searchsorted(EDGES, wrank(t2, np.maximum(w_all, 1e-12)), side="right")
    m = s.wealth_observed.fillna(False).to_numpy().astype(bool)
    X = s[WEALTH].to_numpy(float)[m]
    w = w_all[m]
    st = puf_group_stats(nw_of(X), w, g_all[m], {h: X[:, WEALTH.index(h)] for h in HOLD_P})
    nw0 = np.zeros(len(m))
    nw0[m] = nw_of(X)
    st["splitoff_zero|mean"] = float(np.sum(w_all * nw0) / w_all.sum())
    st["splitoff_zero|median"] = wq(nw0, w_all, 0.5)
    return st


def interp(a, b):
    out = {}
    for k in a:
        if ("share" in k or "hold" in k) or not (a[k] > 0 and b[k] > 0):
            out[k] = a[k] / 3 + 2 * b[k] / 3
        else:
            out[k] = float(np.exp(np.log(a[k]) / 3 + 2 * np.log(b[k]) / 3))
    return out


def run_puf(a):
    rec = pd.read_parquet(f"{D3}/records_train.parquet")
    if a.truth_only:
        res = dict(kind="puf_truth")
        res["scf"] = interp(scf_wave_stats(rec, 2013), scf_wave_stats(rec, 2016))
        hh = {}
        for y in (2013, 2016):
            s = rec[(rec.source.astype(str) == "SCF") & (rec.year == y)]
            hh[y] = pd.factorize(s.hh_id)[0]
        boots = []
        for b in range(B):
            mult = {y: boot_mult(hh[y], b * 2 + (y == 2016)) for y in (2013, 2016)}
            boots.append(interp(scf_wave_stats(rec, 2013, mult), scf_wave_stats(rec, 2016, mult)))
        res["scf_boot_sd"] = {k: float(np.std([bb[k] for bb in boots])) for k in res["scf"]}
        json.dump(res, open(a.out, "w"), indent=1, default=float)
        print("SCF 2015:", {k: round(v) for k, v in res["scf"].items() if "|" not in k}, flush=True)
        return
    p = rec[rec.source.astype(str) == "PUF"].copy()
    p["row"] = np.flatnonzero((rec.source.astype(str) == "PUF").to_numpy())
    C = pd.read_parquet(a.file, columns=["row", "draw"] + WEALTH)
    K = int(C.draw.nunique())
    if len(C) != len(p) * K or not C.row.isin(p.row).all():
        raise SystemExit(f"{a.file}: {len(C)} rows, expected {len(p)} x {K} PUF training records")
    if C[WEALTH].isna().any().any():
        raise SystemExit(f"{a.file}: NA wealth in draws")
    C = C.merge(p[["row", "weight", "E00100", "DSI"]], on="row", how="left", validate="many_to_one")
    w = C.weight.to_numpy(float) / K
    g_agi = np.searchsorted(EDGES, wrank(p.E00100.to_numpy(float), p.weight.to_numpy(float)), side="right")
    g = pd.Series(g_agi, index=p.row.to_numpy()).loc[C.row].to_numpy()
    X = C[WEALTH].to_numpy(float)
    nw = nw_of(X)
    res = dict(kind="puf", label=a.label, file=a.file, draws=K,
               stats=puf_group_stats(nw, w, g, {h: X[:, WEALTH.index(h)] for h in HOLD_P}))
    nd = C.DSI.to_numpy() == 0
    res["not_dependent_filers"] = dict(mean=float(np.sum(w[nd] * nw[nd]) / w[nd].sum()), median=wq(nw[nd], w[nd], 0.5),
                                       hold_primary_home=float(w[nd][X[nd, WEALTH.index("primary_home")] != 0].sum() / w[nd].sum()))
    json.dump(res, open(a.out, "w"), indent=1, default=float)
    print(a.label, "mean", round(res["stats"]["mean"]), "median", round(res["stats"]["median"]),
          "top 0.1% mean", round(res["stats"]["Top 0.1%|mean"]), flush=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("part", choices=["scf", "puf"])
    ap.add_argument("--label", default="truth")
    ap.add_argument("--file", default=None)
    ap.add_argument("--out", required=True)
    ap.add_argument("--truth-only", action="store_true")
    a = ap.parse_args()
    t0 = time.time()
    (run_scf if a.part == "scf" else run_puf)(a)
    print(f"done in {time.time() - t0:.0f}s", flush=True)


if __name__ == "__main__":
    main()
