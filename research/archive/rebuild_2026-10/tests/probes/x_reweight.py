"""Probe X: a model-free check of what completed PUF wealth "should" be,
before and after translation.

If the model learns wealth given the shared variables from the SCF and
applies it to PUF records, the completed PUF wealth should look like the
SCF reweighted so that its shared variables (as the model sees them: the
tokens) are distributed like the PUF's. This script estimates that by
classification: a gradient-boosted classifier separates PUF records from
SCF units with wealth observed, using only the shared variables' tokens;
each SCF unit gets weight x p/(1-p) (out-of-fold, two folds). It reports
the classifier's area under the ROC curve (how distinguishable the two
files' shared variables are; 0.5 = not at all), the effective sample size
of the reweighted SCF, and the reweighted SCF's wealth, next to the model
completions on the same PUF records.

Population: PUF training returns that are not dependent filers and have a
known age (the SCF has neither dependent filers nor missing ages); SCF
training units with wealth observed, each wave separately, interpolated
to 2015 as in x_compare.py. Sex of head is left out (it is the one shared
variable whose meaning is known to differ by construction, head against
primary filer, and the translation only partly aligns it).

    python x_reweight.py
"""
import json
import os

import numpy as np
import pandas as pd
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.metrics import roc_auc_score

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x"
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
FEATS = ["MARS", "XTOT", "N24", "AGERANGE", "E00200", "E00900", "E02000", "P23250", "E00600", "E00300",
         "E01700", "E02400", "E02100", "E00400", "E02300"]
HOLD = ["equities", "bonds", "pass_throughs", "primary_home", "primary_mortgage", "dc"]


def wq(x, w, q):
    o = np.argsort(x, kind="mergesort")
    cw = np.cumsum(w[o]) / w.sum()
    return float(x[o][min(np.searchsorted(cw, q), len(x) - 1)])


def topshare(x, w, p):
    o = np.argsort(-x, kind="mergesort")
    cw = np.cumsum(w[o]) / w.sum()
    return float(np.sum((w * x)[o][cw <= p]) / np.sum(w * x))


def wstats(nw, w, hold):
    out = {"mean": float(np.sum(w * nw) / w.sum()), "median": wq(nw, w, 0.5), "p90": wq(nw, w, 0.9),
           "top1_share": topshare(nw, w, 0.01), "neg_share": float(w[nw < 0].sum() / w.sum())}
    for h, x in hold.items():
        out["hold_" + h] = float(w[x != 0].sum() / w.sum())
    return out


def nw_of(f):
    return f[ASSETS].to_numpy(float).sum(axis=1) - f[DEBTS].to_numpy(float).sum(axis=1)


def interp(a, b):
    out = {}
    for k in a:
        if ("share" in k or "hold" in k or "auc" in k or "ess" in k) or not (a[k] > 0 and b[k] > 0):
            out[k] = a[k] / 3 + 2 * b[k] / 3
        else:
            out[k] = float(np.exp(np.log(a[k]) / 3 + 2 * np.log(b[k]) / 3))
    return out


def main():
    res = {}
    v2rec = pd.read_parquet(f"{T2}/data/full_v2/records_train.parquet")
    VERS = [("untranslated (full_v2)", f"{T2}/data/full_v2"), ("translated, least likely off (full_v3x)", f"{Q}/data/full_v3x"),
            ("translated (full_v3x2)", f"{Q}/data/full_v3x2")]
    VERS = [(a, b) for a, b in VERS if os.path.exists(f"{b}/DONE")]
    for ver, path in VERS:
        meta = json.load(open(f"{path}/meta.json"))
        names = meta["variable_names"]
        tok = np.load(f"{path}/tokens_train.npy").astype(np.int64)
        rec = pd.read_parquet(f"{path}/records_train.parquet")
        X = tok[:, [names.index(c) for c in FEATS]]
        puf = ((rec.source == "PUF") & (rec.DSI == 0) & (X[:, FEATS.index("AGERANGE")] >= 0)).to_numpy()
        per = {}
        for y in (2013, 2016):
            scf = ((rec.source == "SCF") & (rec.year == y) & rec.wealth_observed).to_numpy()
            idx = np.flatnonzero(puf | scf)
            yv = puf[idx].astype(int)
            w = rec.weight.to_numpy(float)[idx]
            sw = np.where(yv == 1, w / w[yv == 1].sum(), w / w[yv == 0].sum())
            rng = np.random.default_rng(20261040 + y)
            fold = rng.integers(0, 2, len(idx))
            p = np.zeros(len(idx))
            for k in (0, 1):
                clf = HistGradientBoostingClassifier(max_iter=300, learning_rate=0.1, max_leaf_nodes=31,
                                                     min_samples_leaf=50, random_state=1,
                                                     categorical_features=[0])
                tr = fold != k
                clf.fit(X[idx][tr], yv[tr], sample_weight=sw[tr])
                p[~tr] = clf.predict_proba(X[idx][~tr])[:, 1]
            auc = roc_auc_score(yv, p, sample_weight=sw)
            s = yv == 0
            ps = np.clip(p[s], 1e-4, 1 - 1e-4)
            odds = ps / (1 - ps)
            rw = w[s] * odds
            cap = np.quantile(rw, 0.999)
            rw_c = np.minimum(rw, cap)
            ess = rw_c.sum() ** 2 / np.sum(rw_c ** 2) / (w[s].sum() ** 2 / np.sum(w[s] ** 2))
            sc = v2rec.iloc[idx[s]] if ver.startswith("untranslated") else rec.iloc[idx[s]]
            nw = nw_of(sc)
            hold = {h: sc[h].to_numpy(float) for h in HOLD}
            per[y] = {"auc": float(auc), "ess_share": float(ess),
                      "scf_own": wstats(nw, w[s], hold), "scf_reweighted": wstats(nw, rw_c, hold)}
            print(ver, y, "AUC", round(auc, 3), "ESS share", round(ess, 3),
                  "median own", round(per[y]["scf_own"]["median"]), "reweighted", round(per[y]["scf_reweighted"]["median"]),
                  "home own", round(per[y]["scf_own"]["hold_primary_home"], 3), "rew", round(per[y]["scf_reweighted"]["hold_primary_home"], 3),
                  flush=True)
        res[ver] = {"2013": per[2013], "2016": per[2016],
                    "interp": {"auc": per[2013]["auc"] / 3 + 2 * per[2016]["auc"] / 3,
                               "scf_own": interp(per[2013]["scf_own"], per[2016]["scf_own"]),
                               "scf_reweighted": interp(per[2013]["scf_reweighted"], per[2016]["scf_reweighted"])}}
        res[ver]["puf_records"] = int(puf.sum())
        res[ver]["puf_weight_m"] = float(rec.weight.to_numpy()[puf].sum() / 1e6)
    # model completions on the same PUF records (rows of records_train)
    prow = np.flatnonzero(((v2rec.source == "PUF") & (v2rec.DSI == 0) & v2rec.AGERANGE.notna()).to_numpy())
    pw = pd.Series(v2rec.weight.to_numpy(float)[prow], index=prow)
    import glob
    files = {f"untranslated, seed {s}": f"{T2}/model/full_v2/h_puf_wealth_seed{s:02d}.parquet" for s in (1, 2, 3)}
    for xv, xl in (("full_v3x", "translated, least likely off"), ("full_v3x2", "translated")):
        for f in sorted(glob.glob(f"{Q}/model/{xv}/h_puf_wealth_seed*.parquet")):
            files[f"{xl}, seed {int(f.split('seed')[-1][:2])}"] = f
    for f in sorted(glob.glob(f"{Q}/model/sharedonly/*/h_puf_wealth_seed*.parquet")):
        ver = {"full_v2": "untranslated", "full_v3x": "translated, least likely off", "full_v3x2": "translated"}[f.split("/")[-2]]
        files[f"{ver}, shared variables only, seed {int(f.split('seed')[-1][:2])}"] = f
    comp = {}
    for lab, f in files.items():
        C = pd.read_parquet(f, columns=["row", "draw"] + ASSETS + DEBTS)
        K = C.draw.nunique()
        C = C[C.row.isin(prow)]
        w = pw.loc[C.row].to_numpy() / K
        comp[lab] = wstats(nw_of(C), w, {h: C[h].to_numpy(float) for h in HOLD})
        print(lab, {k: round(v, 3) for k, v in comp[lab].items()}, flush=True)
    res["completions_same_puf_records"] = comp
    with open(f"{Q}/x_reweight.json", "w") as fo:
        json.dump(res, fo, indent=1)
    rows = ["mean", "median", "p90", "top1_share", "neg_share"] + ["hold_" + h for h in HOLD]
    tab = {}
    for ver, _ in VERS:
        tab[f"SCF own, {ver}"] = res[ver]["interp"]["scf_own"]
        tab[f"SCF reweighted to PUF shared variables, {ver}"] = res[ver]["interp"]["scf_reweighted"]
    tab.update(comp)
    df = pd.DataFrame({k: [v.get(r, np.nan) for r in rows] for k, v in tab.items()}, index=rows)
    pd.set_option("display.width", 300)
    pd.set_option("display.max_columns", 20)
    print(df.T.to_string(float_format=lambda x: f"{x:,.4g}"))
    print({ver: (round(res[ver]["interp"]["auc"], 3), round(res[ver]["2013"]["ess_share"], 3),
                 round(res[ver]["2016"]["ess_share"], 3)) for ver, _ in VERS})


if __name__ == "__main__":
    main()
