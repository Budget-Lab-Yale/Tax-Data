"""Probe C: copy of x_compare.py with probe C's models added (and BR's), the
output written to probe C's folder. Original notes:

Probe X: completed PUF wealth from the untranslated model (full_v2) and the
translated one (full_v3x), against the SCF 2013 and 2016 training records
interpolated to 2015.

    python x_compare.py

Inputs: h_complete.py's puf_wealth files (PUF training records, wealth
drawn with the SCF tag at 2015, five draws), full_v2's records_train for
the PUF weights, AGI and the SCF truth. Every statistic weights a draw by
its record's weight over the number of draws. SCF statistics are computed
per wave and interpolated (one third 2013, two thirds 2016; dollar levels
in logs where both are positive, as test 3's evaluation does; shares
linearly). PUF groups: AGI (E00100) weighted rank, and test 2's
seven-income total (as in EVAL_REPORT.md); SCF groups: the seven-income
total within each wave (the SCF has no AGI).
"""

import glob
import json
import os
import sys

import numpy as np
import pandas as pd

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x"
BRQ = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/br"
C3 = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3"
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
KG = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
WEALTH = ASSETS + DEBTS + KG
T2_INC = ["t2_wages", "t2_business", "t2_int_div", "t2_capital_gains", "t2_rent", "t2_ss_pens", "t2_ui_other"]
EDGES = [0.5, 0.9, 0.99, 0.999]
LABELS = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
HOLD = ["cash", "equities", "bonds", "dc", "pass_throughs", "primary_home", "other_home", "primary_mortgage",
        "credit_cards"]


def wrank(x, w):
    o = np.argsort(x, kind="mergesort")
    cw = np.cumsum(w[o])
    r = np.empty(len(x))
    r[o] = (cw - w[o] / 2) / cw[-1]
    return r


def wq(x, w, q):
    o = np.argsort(x, kind="mergesort")
    cw = np.cumsum(w[o]) / w.sum()
    return float(x[o][min(np.searchsorted(cw, q), len(x) - 1)])


def topshare(x, w, p):
    o = np.argsort(-x, kind="mergesort")
    cw = np.cumsum(w[o]) / w.sum()
    k = cw <= p
    return float(np.sum((w * x)[o][k]) / np.sum(w * x))


def stats(nw, w, grp, hold):
    out = {"mean": float(np.sum(w * nw) / w.sum()), "median": wq(nw, w, 0.5), "p90": wq(nw, w, 0.9),
           "p99": wq(nw, w, 0.99), "top10_share": topshare(nw, w, 0.10), "top1_share": topshare(nw, w, 0.01),
           "top01_share": topshare(nw, w, 0.001), "neg_share": float(w[nw < 0].sum() / w.sum())}
    for name, g in grp.items():
        tot = np.sum(w * nw)
        for k, lab in enumerate(LABELS):
            m = g == k
            out[f"{name}|{lab}|mean"] = float(np.sum(w[m] * nw[m]) / w[m].sum())
            out[f"{name}|{lab}|median"] = wq(nw[m], w[m], 0.5)
            out[f"{name}|{lab}|share"] = float(np.sum(w[m] * nw[m]) / tot)
            for h, x in hold.items():
                out[f"{name}|{lab}|hold_{h}"] = float(w[m][x[m] != 0].sum() / w[m].sum())
    for h, x in hold.items():
        out[f"hold_{h}"] = float(w[x != 0].sum() / w.sum())
    return out


def nw_of(frame):
    return frame[ASSETS].to_numpy(float).sum(axis=1) - frame[DEBTS].to_numpy(float).sum(axis=1)


def main():
    rec = pd.read_parquet(f"{T2}/data/full_v2/records_train.parquet")
    res = {}
    # SCF waves
    for y in (2013, 2016):
        s = rec[(rec.source == "SCF") & (rec.year == y)]
        t2 = s[T2_INC].to_numpy(float).sum(axis=1)
        w_all = s.weight.to_numpy(float)
        g_all = np.searchsorted(EDGES, wrank(t2, w_all), side="right")
        m = s.wealth_observed.to_numpy().astype(bool)
        s = s[m]
        w = w_all[m]
        res[f"SCF {y}"] = stats(nw_of(s), w, {"inc7": g_all[m]}, {h: s[h].to_numpy(float) for h in HOLD})
        # all filer units, split-off units (wealth not asked) counted at zero net worth: a bound
        # for the SCF population that matches all filers
        nw0 = np.zeros(len(m))
        nw0[m] = nw_of(s)
        alt = {"mean": float(np.sum(w_all * nw0) / w_all.sum()), "median": wq(nw0, w_all, 0.5),
               "split_off_weight_share": float(w_all[~m].sum() / w_all.sum())}
        res.setdefault("scf_with_split_offs_at_zero", {})[str(y)] = alt
        print("SCF", y, "all filer units, split-offs at zero:", alt, flush=True)
    a, b = res["SCF 2013"], res["SCF 2016"]
    interp = {}
    for k in a:
        if ("share" in k or "hold" in k) or not (a[k] > 0 and b[k] > 0):
            interp[k] = a[k] / 3 + 2 * b[k] / 3
        else:
            interp[k] = float(np.exp(np.log(a[k]) / 3 + 2 * np.log(b[k]) / 3))
    res["SCF interpolated 2015"] = interp
    # PUF completions
    p = rec[rec.source == "PUF"].copy()
    p["row"] = np.flatnonzero((rec.source == "PUF").to_numpy())
    files = {}
    for sd in (1, 2, 3):
        f = f"{T2}/model/full_v2/h_puf_wealth_seed{sd:02d}.parquet"
        if os.path.exists(f):
            files[f"untranslated, seed {sd}"] = f
    for xv, xl in (("full_v3x2", "translated"),):
        for f in sorted(glob.glob(f"{Q}/model/{xv}/h_puf_wealth_seed*.parquet")):
            files[f"{xl}, seed {int(f.split('seed')[-1][:2])}"] = f
    for f in sorted(glob.glob(f"{BRQ}/model/full_v3br/h_puf_wealth_seed*.parquet")):
        files[f"bridge (BR), seed {int(f.split('seed')[-1][:2])}"] = f
    for d, lab in (("model_cz", "combined, deductions zero"), ("model", "combined, deductions unknown"),
                   ("model_czw", "combined, deductions zero, weight as variable"),
                   ("model_czs", "combined, deductions zero, SCF Schedule E unobserved")):
        for f in sorted(glob.glob(f"{C3}/{d}/h_puf_wealth_seed*.parquet")):
            files[f"{lab}, seed {int(f.split('seed')[-1][:2])}"] = f
    for lab, f in files.items():
        C = pd.read_parquet(f, columns=["row", "draw"] + WEALTH)
        K = int(C.draw.nunique())
        C = C.merge(p[["row", "weight", "E00100"] + T2_INC], on="row", how="left", validate="many_to_one")
        if C.weight.isna().any():
            raise SystemExit(f"{f}: rows without a PUF record")
        w = C.weight.to_numpy(float) / K
        # groups ranked on the PUF records (one weight per record)
        g_agi = np.searchsorted(EDGES, wrank(p.E00100.to_numpy(float), p.weight.to_numpy(float)), side="right")
        g_t2 = np.searchsorted(EDGES, wrank(p[T2_INC].to_numpy(float).sum(axis=1), p.weight.to_numpy(float)),
                               side="right")
        gm = pd.Series(g_agi, index=p.row.to_numpy())
        gm2 = pd.Series(g_t2, index=p.row.to_numpy())
        grp = {"agi": gm.loc[C.row].to_numpy(), "inc7": gm2.loc[C.row].to_numpy()}
        res[lab] = stats(nw_of(C), w, grp, {h: C[h].to_numpy(float) for h in HOLD})
        res[lab]["draws"] = K
        nd = (C.merge(p[["row", "DSI"]], on="row", how="left").DSI.to_numpy() == 0)
        res.setdefault("puf_not_dependent_filers", {})[lab] = {
            "mean": float(np.sum(w[nd] * nw_of(C)[nd]) / w[nd].sum()), "median": wq(nw_of(C)[nd], w[nd], 0.5),
            "hold_primary_home": float(w[nd][C.primary_home.to_numpy(float)[nd] != 0].sum() / w[nd].sum())}
        print("  not dependent filers:", res["puf_not_dependent_filers"][lab], flush=True)
        print(lab, f, "K", K, "mean", round(res[lab]["mean"]), "median", round(res[lab]["median"]), flush=True)
    # SCF training records: wealth redrawn by each model from the records'
    # own shared variables (h_complete scf_train), against their true wealth
    scfm = rec[(rec.source == "SCF") & rec.wealth_observed].copy()
    scfm["row"] = np.flatnonzero(((rec.source == "SCF") & rec.wealth_observed).to_numpy())
    res["scf_records_redrawn"] = {}
    for lab, f in (("untranslated, seed 2", f"{T2}/model/full_v2/h_scf_train_seed02.parquet"),
                   ("translated, seed 2", f"{Q}/model/full_v3x2/h_scf_train_seed02.parquet")):
        if not os.path.exists(f):
            continue
        C = pd.read_parquet(f, columns=["row", "draw"] + WEALTH)
        K = int(C.draw.nunique())
        C = C.merge(scfm[["row", "weight", "year"]], on="row", how="inner")
        for y in (2013, 2016):
            m = (C.year == y).to_numpy()
            w = C.weight.to_numpy(float)[m] / K
            st = stats(nw_of(C[m]), w, {}, {h: C[h].to_numpy(float)[m] for h in HOLD})
            res["scf_records_redrawn"][f"{lab}, SCF {y}"] = st
            print("SCF records redrawn", lab, y, "median", round(st["median"]), "mean", round(st["mean"]),
                  "home", round(st["hold_primary_home"], 3), "| truth median", round(res[f"SCF {y}"]["median"]),
                  "home", round(res[f"SCF {y}"]["hold_primary_home"], 3), flush=True)
    with open(f"{C3}/c3_wealth.json", "w") as fo:
        json.dump(res, fo, indent=1)
    # print a compact table
    keys = ["mean", "median", "p90", "p99", "top10_share", "top1_share", "top01_share", "neg_share"] + \
           [f"hold_{h}" for h in HOLD]
    for nm in ("agi", "inc7"):
        for st in ("mean", "median", "share"):
            keys += [f"{nm}|{lab}|{st}" for lab in LABELS]
    for nm in ("agi", "inc7"):
        for h in ("equities", "bonds", "pass_throughs", "primary_home"):
            keys += [f"{nm}|{lab}|hold_{h}" for lab in LABELS]
    skip = {"scf_records_redrawn", "scf_with_split_offs_at_zero", "puf_not_dependent_filers"}
    tab = pd.DataFrame({c: [res[c].get(k, np.nan) for k in keys] for c in res if c not in skip}, index=keys)
    pd.set_option("display.width", 300)
    pd.set_option("display.max_columns", 20)
    pd.set_option("display.max_rows", 300)
    print(tab.to_string(float_format=lambda x: f"{x:,.4g}"))
    tab.to_csv(f"{C3}/c3_wealth.csv")


if __name__ == "__main__":
    main()
