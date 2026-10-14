"""Probe WT: collector. Completed wealth from the WT models (SCF rows
unweighted in training, the SCF weight SCFWT a model variable) against
probe C's unknown-coded combined model and the SCF.

    python wt_wealth.py

Writes $Q/WT_TABLES.md and $Q/wt_wealth.json (weighted aggregates only).

Two ways of weighting a WT model's draws, both reported:
  - resampled: each draw of a record gets the record's weight times its drawn
    SCF weight over the sum of the drawn SCF weights of that record's draws.
    This is resampling the draws in proportion to the drawn weight, taken to
    the limit of many resamples (no extra resampling noise).
  - plain: each draw gets the record's weight over the number of draws (what
    a model trained on the SCF sample would give without the correction).

PUF table: PUF training records grouped by true AGI (weighted rank), as in
probe C. SCF benchmark and probe C's columns are read from probe C's
c3_wealth.json (the SCF 2013 and 2016 training records interpolated to 2015,
grouped by the seven-income total within each wave).

SCF test table: SCF test records with wealth observed, grouped by the
seven-income total's weighted rank within each wave among the test records
(true weights), both waves pooled. Truth: the records' own wealth. Model
draws are weighted by the record's true weight (over draws, or resampled as
above).
"""
import glob
import json
import os

import numpy as np
import pandas as pd

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
C3 = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt"
DATA = f"{Q}/data/full_v3cwt"
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
T2_INC = ["t2_wages", "t2_business", "t2_int_div", "t2_capital_gains", "t2_rent", "t2_ss_pens", "t2_ui_other"]
EDGES = [0.5, 0.9, 0.99, 0.999]
LABELS = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
SHOW = ["90-99%", "99-99.9%", "Top 0.1%"]


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


def nw_of(f):
    return f[ASSETS].to_numpy(float).sum(axis=1) - f[DEBTS].to_numpy(float).sum(axis=1)


def group_stats(nw, w, g, extra=None, hiw=None):
    out = {"All|mean": float((w * nw).sum() / w.sum()), "All|median": wq(nw, w, 0.5)}
    for k, lab in enumerate(LABELS):
        m = g == k
        if not m.any():
            continue
        out[f"{lab}|mean"] = float((w[m] * nw[m]).sum() / w[m].sum())
        out[f"{lab}|median"] = wq(nw[m], w[m], 0.5)
        out[f"{lab}|share above $10m"] = float(w[m][nw[m] > 1e7].sum() / w[m].sum())
        out[f"{lab}|mean below $10m"] = float((w[m] * np.minimum(nw[m], 1e7)).sum() / w[m].sum())
        if hiw is not None:
            out[f"{lab}|share of net worth from drawn weight above 1000"] = float(
                (w[m] * nw[m])[hiw[m]].sum() / (w[m] * nw[m]).sum())
        if extra is not None:
            for nm, x in extra.items():
                out[f"{lab}|{nm}"] = wq(x[m], w[m], 0.5)
    return out


def draw_weights(C, wrec, scfwt):
    """resampled and plain weights for each (record, draw) row of C."""
    K = int(C.draw.nunique())
    plain = wrec / K
    s = pd.Series(scfwt).groupby(C.row.to_numpy()).transform("sum").to_numpy()
    res = wrec * scfwt / s
    return res, plain, K


def scfwt_values(C, meta):
    v = [x for x in meta["variables"] if x["name"] == "SCFWT"][0]
    vals = np.array([t["value"] for t in v["tokens"]], float)
    tok = C["tok_SCFWT"].to_numpy().astype(int)
    if (tok < 0).any():
        raise SystemExit("SCFWT not drawn on some rows")
    return vals[tok]


def fmt(x):
    if x is None or (isinstance(x, float) and not np.isfinite(x)):
        return "pending"
    a = abs(x)
    if a >= 1e6:
        return f"${x / 1e6:,.1f}m"
    return f"${x / 1e3:,.0f}k"


def main():
    meta = json.load(open(f"{DATA}/meta.json"))
    res = {"puf": {}, "scf_test": {}, "notes": []}
    c3 = json.load(open(f"{C3}/c3_wealth.json"))
    scf = c3["SCF interpolated 2015"]
    res["puf"]["SCF 2015 (C's benchmark)"] = {f"{lab}|{s}": scf.get(f"inc7|{lab}|{s}") for lab in LABELS
                                              for s in ("mean", "median")} | {"All|mean": scf["mean"],
                                                                              "All|median": scf["median"]}
    for sd in (1, 2, 3):
        k = f"combined, deductions unknown, seed {sd}"
        if k in c3:
            res["puf"][f"C unknown coding, seed {sd}"] = {f"{lab}|{s}": c3[k].get(f"agi|{lab}|{s}")
                                                          for lab in LABELS for s in ("mean", "median")} | {
                "All|mean": c3[k]["mean"], "All|median": c3[k]["median"]}

    # ---- PUF records ----
    rec = pd.read_parquet(f"{T2}/data/full_v2/records_train.parquet",
                          columns=["source", "weight", "E00100"] + T2_INC)
    p = rec[rec.source == "PUF"].copy()
    p["row"] = np.flatnonzero((rec.source == "PUF").to_numpy())
    g_agi = np.searchsorted(EDGES, wrank(p.E00100.to_numpy(float), p.weight.to_numpy(float)), side="right")
    gmap = pd.Series(g_agi, index=p.row.to_numpy())
    pfiles = [("", f) for f in sorted(glob.glob(f"{Q}/model/h_puf_wealth_seed*.parquet"))] + \
             [(" restricted", f) for f in sorted(glob.glob(f"{Q}/model/h_puf_wealth_r_seed*.parquet"))] + \
             [(" square root", f) for f in sorted(glob.glob(f"{Q}/model/h_sq_puf_wealth_seed*.parquet"))] + \
             [(" square root restricted", f) for f in sorted(glob.glob(f"{Q}/model/h_sq_puf_wealth_r_seed*.parquet"))]
    for rtag, f in pfiles:
        sd = int(f.split("seed")[-1][:2])
        C = pd.read_parquet(f, columns=["row", "draw", "tok_SCFWT"] + ASSETS + DEBTS)
        C = C.merge(p[["row", "weight"]], on="row", how="left", validate="many_to_one")
        if C.weight.isna().any():
            raise SystemExit(f"{f}: rows without a PUF record")
        wv = scfwt_values(C, meta)
        # square-root arm: the model learned p_pop / sqrt(w), so resample by sqrt(w)
        wr, wp, K = draw_weights(C, C.weight.to_numpy(float), np.sqrt(wv) if "square" in rtag else wv)
        g = gmap.loc[C.row].to_numpy()
        nw = nw_of(C)
        res["puf"][f"WT{rtag} resampled, seed {sd}"] = group_stats(nw, wr, g, {"drawn SCF weight": wv}, wv > 1000)
        res["puf"][f"WT{rtag} plain, seed {sd}"] = group_stats(nw, wp, g, {"drawn SCF weight": wv}, wv > 1000)
        print(f, "draws", K, flush=True)

    # SCF training records' own weights by income group (for the drawn-weight comparison)
    rs = pd.read_parquet(f"{DATA}/records_train.parquet")
    rs = rs[(rs.source == "SCF")]
    for y in sorted(rs.year.unique()):
        s = rs[rs.year == y]
        w = s.weight.to_numpy(float)
        g = np.searchsorted(EDGES, wrank(s[T2_INC].to_numpy(float).sum(1), w), side="right")
        res.setdefault("scf_train_median_weight", {})[str(y)] = {
            LABELS[k]: {"median_weight_unweighted": float(np.median(w[g == k])),
                        "median_weight_popweighted": wq(w[g == k], w[g == k], 0.5),
                        "rows": int((g == k).sum())} for k in range(5)}

    sw = rs[rs.wealth_observed.astype(bool)].copy()
    gw = np.zeros(len(sw), int)
    for y in sorted(rs.year.unique()):
        s_all = rs[rs.year == y]
        gall = np.searchsorted(EDGES, wrank(s_all[T2_INC].to_numpy(float).sum(1), s_all.weight.to_numpy(float)),
                               side="right")
        gser = pd.Series(gall, index=s_all.index)
        mm = (sw.year == y).to_numpy()
        gw[mm] = gser.loc[sw.index[mm]].to_numpy()
    res["puf"]["SCF training records, 2013 and 2016 pooled"] = group_stats(
        nw_of(sw), sw.weight.to_numpy(float), gw, hiw=sw.weight.to_numpy(float) > 1000)

    # ---- SCF test records ----
    rt = pd.read_parquet(f"{DATA}/records_test.parquet")
    st = rt[(rt.source == "SCF")].copy()
    st["row"] = np.flatnonzero((rt.source == "SCF").to_numpy())
    st = st[st.wealth_observed.astype(bool)]
    gt = np.zeros(len(st), int)
    for y in sorted(st.year.unique()):
        m = (st.year == y).to_numpy()
        gt[m] = np.searchsorted(EDGES, wrank(st.loc[m, T2_INC].to_numpy(float).sum(1),
                                             st.weight.to_numpy(float)[m]), side="right")
    st["g"] = gt
    res["scf_test"]["Truth"] = group_stats(nw_of(st), st.weight.to_numpy(float), gt, hiw=st.weight.to_numpy(float) > 1000)
    res["scf_test_counts"] = {LABELS[k]: int((gt == k).sum()) for k in range(5)}
    files = []
    for f in sorted(glob.glob(f"{C3}/model/h_scf_test_seed*.parquet")):
        files.append((f"C unknown coding, seed {int(f.split('seed')[-1][:2])}", f, "c"))
    for f in sorted(glob.glob(f"{Q}/model/h_scf_test_seed*.parquet")):
        files.append((f"WT, weight drawn, seed {int(f.split('seed')[-1][:2])}", f, "wt"))
    for f in sorted(glob.glob(f"{Q}/model/h_scf_test_r_seed*.parquet")):
        files.append((f"WT restricted, weight drawn, seed {int(f.split('seed')[-1][:2])}", f, "wt"))
    for f in sorted(glob.glob(f"{Q}/model/h_sq_scf_test_seed*.parquet")):
        files.append((f"WT square root, weight drawn, seed {int(f.split('seed')[-1][:2])}", f, "sq"))
    for f in sorted(glob.glob(f"{Q}/model/h_sq_scf_test_r_seed*.parquet")):
        files.append((f"WT square root restricted, weight drawn, seed {int(f.split('seed')[-1][:2])}", f, "sq"))
    for f in sorted(glob.glob(f"{Q}/model/h_scf_test_wobs_seed*.parquet")):
        files.append((f"WT, own weight given, seed {int(f.split('seed')[-1][:2])}", f, "wobs"))
    for lab, f, kind in files:
        cols = ["row", "draw"] + ASSETS + DEBTS + (["tok_SCFWT"] if kind in ("wt", "sq") else [])
        C = pd.read_parquet(f, columns=cols)
        C = C.merge(st[["row", "weight", "g"]], on="row", how="inner")
        wrec = C.weight.to_numpy(float)
        if kind in ("wt", "sq"):
            wv = scfwt_values(C, meta)
            wr, wp, K = draw_weights(C, wrec, np.sqrt(wv) if kind == "sq" else wv)
            res["scf_test"][lab.replace("weight drawn", "weight drawn, resampled")] = group_stats(
                nw_of(C), wr, C.g.to_numpy(), hiw=wv > 1000)
            res["scf_test"][lab.replace("weight drawn", "weight drawn, plain")] = group_stats(
                nw_of(C), wp, C.g.to_numpy())
        else:
            K = int(C.draw.nunique())
            res["scf_test"][lab] = group_stats(nw_of(C), wrec / K, C.g.to_numpy())
        print(lab, f, flush=True)

    json.dump(res, open(f"{Q}/wt_wealth.json", "w"), indent=1)

    # ---- markdown ----
    L = ["# Probe WT tables (written by wt_wealth.py)", ""]
    for part, title in (("puf", "Completed PUF wealth by AGI group (PUF training records); SCF by seven-income total"),
                        ("scf_test", "SCF test records: true against completed net worth, by seven-income total")):
        L += [f"## {title}", ""]
        cols = list(res[part].keys())
        L += ["| | " + " | ".join(cols) + " |", "|---|" + "---|" * len(cols)]
        for lab in ["All"] + LABELS:
            for s in ("mean", "median", "mean below $10m", "share above $10m", "share of net worth from drawn weight above 1000"):
                if s.startswith("share"):
                    L.append(f"| {lab}, {s} | " + " | ".join(
                        "" if res[part][c].get(f"{lab}|{s}") is None else f"{100 * res[part][c][f'{lab}|{s}']:.2f}%"
                        for c in cols) + " |")
                    continue
                L.append(f"| {lab}, {s} | " + " | ".join(fmt(res[part][c].get(f"{lab}|{s}")) for c in cols) + " |")
        L.append("")
    if any("WT" in c for c in res["puf"]):
        L += ["## Median drawn SCF weight on PUF records, by AGI group", ""]
        cols = [c for c in res["puf"] if c.startswith("WT") and "plain" in c]
        L += ["| | " + " | ".join(cols) + " |", "|---|" + "---|" * len(cols)]
        for lab in LABELS:
            L.append(f"| {lab} | " + " | ".join(f"{res['puf'][c].get(f'{lab}|drawn SCF weight', float('nan')):,.0f}"
                                               for c in cols) + " |")
        L.append("")
    L += ["## SCF training rows' own weights by income group", "", "```",
          json.dumps(res["scf_train_median_weight"], indent=1), "```", "",
          f"SCF test records with wealth observed, rows by income group: {res['scf_test_counts']}", ""]
    # compact summary: mean over seeds [range] for each method
    L += ["## Summary: mean over seeds [range]", ""]
    keys = ["All|mean", "All|median"] + [f"{lab}|{st}" for lab in LABELS[2:] for st in ("mean", "median")] + \
           [f"{lab}|share above $10m" for lab in LABELS] + [f"{lab}|mean" for lab in LABELS[:2]]
    for part in ("puf", "scf_test"):
        meth = {}
        for c in res[part]:
            meth.setdefault(c.split(", seed")[0], []).append(c)
        L += [f"### {part}", "", "| | " + " | ".join(f"{m} ({len(v)})" for m, v in meth.items()) + " |",
              "|---|" + "---|" * len(meth)]
        for k in keys:
            row = []
            for m, cs in meth.items():
                xs = [res[part][c].get(k) for c in cs]
                xs = [x for x in xs if x is not None]
                if not xs:
                    row.append("")
                    continue
                f_ = (lambda x: f"{100 * x:.2f}%") if "share" in k else fmt
                row.append(f_(np.mean(xs)) + (f" [{f_(min(xs))}–{f_(max(xs))}]" if len(xs) > 1 else ""))
            L.append(f"| {k} | " + " | ".join(row) + " |")
        L.append("")
    open(f"{Q}/WT_TABLES.md", "w").write("\n".join(L))
    print("\n".join(L))


if __name__ == "__main__":
    main()
