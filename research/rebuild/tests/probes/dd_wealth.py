"""Probe DD: completed PUF home ownership, home value, mortgage debt and net
worth, by AGI group and by mortgage-interest band, against the SCF and
against probe C's zero-coded and unknown-coded models (and X's and BR's).
Writes markdown to stdout and $Q/dd_wealth.json.

PUF: training records, wealth drawn five times with the SCF tag at 2015
(h_complete.py puf_wealth files); each draw weighted by the record's weight
over the number of draws. AGI groups by true AGI (weighted rank). Mortgage-
interest bands by the interest deduction claimed on the return (E19200):
non-itemizers; itemizers with zero; itemizers with $1-5k, $5-10k, $10-20k,
above $20k.
SCF: units with wealth observed, 2013 and 2016 waves, statistics computed per
wave and interpolated to 2015 (one third 2013, two thirds 2016; dollar levels
in logs where both positive, shares linearly). Income groups: the seven-income
total within each wave (as probes X and C). Interest bands: the mortgage
interest paid (probe BR's bridge value), zero and the same dollar bands; the
SCF has no form of deduction, so its zero band mixes owners without a
mortgage and renters.

    python dd_wealth.py
"""
import glob
import json
import os
import re

import numpy as np
import pandas as pd

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
P = "/nfs/roberts/scratch/pi_nrs36/jar335/probes"
Q = f"{P}/dd"
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
T2_INC = ["t2_wages", "t2_business", "t2_int_div", "t2_capital_gains", "t2_rent", "t2_ss_pens", "t2_ui_other"]
EDGES = [0.5, 0.9, 0.99, 0.999]
GLAB = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
BLAB = ["Non-itemizer", "Zero interest", "$1-5k", "$5-10k", "$10-20k", "Above $20k"]
BEDGE = [0, 5000, 10000, 20000]
MODELS = [
    ("Translated SCF (X)", f"{P}/x/model/full_v3x2/h_puf_wealth_seed*.parquet"),
    ("Bridge (BR)", f"{P}/br/model/full_v3br/h_puf_wealth_seed*.parquet"),
    ("C, deductions zero", f"{P}/c3/model_cz/h_puf_wealth_seed*.parquet"),
    ("C, deductions unknown", f"{P}/c3/model/h_puf_wealth_seed*.parquet"),
    ("DD paid observed for itemizers; claimed shown", f"{Q}/model_dd/h_puf_wealth_seed*.parquet"),
    ("DD paid observed for itemizers; claimed hidden", f"{Q}/model_dd/h_puf_wealth_hide_seed*.parquet"),
    ("DD paid SCF only; claimed shown", f"{Q}/model_ddp/h_puf_wealth_seed*.parquet"),
    ("DD paid SCF only; claimed hidden", f"{Q}/model_ddp/h_puf_wealth_hide_seed*.parquet"),
]


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


def cells(home, mort, nw, w, grp, labels):
    out = {}
    for k, lab in enumerate(["All"] + labels):
        m = np.ones(len(w), bool) if k == 0 else (grp == k - 1)
        if not m.any():
            continue
        ww = w[m]
        out[lab] = dict(share=float(ww.sum() / w.sum()),
                        own_home=float(ww[home[m] > 0].sum() / ww.sum()),
                        home_value=float((ww * home[m]).sum() / ww.sum()),
                        has_mortgage=float(ww[mort[m] > 0].sum() / ww.sum()),
                        mortgage=float((ww * mort[m]).sum() / ww.sum()),
                        nw_mean=float((ww * nw[m]).sum() / ww.sum()),
                        nw_median=wq(nw[m], ww, 0.5))
    return out


def band(interest, itemizer):
    b = np.where(interest > 0, 2 + np.searchsorted(BEDGE[1:], interest, side="right"), 1)
    return np.where(itemizer, b, 0)


def main():
    rec = pd.read_parquet(f"{T2}/data/full_v2/records_train.parquet")
    br = pd.read_parquet(f"{P}/c3/data/full_v3cz/records_train.parquet", columns=["source", "year", "weight", "E19200"])
    assert (br.source.astype(str).to_numpy() == rec.source.astype(str).to_numpy()).all()
    res = {}
    for y in (2013, 2016):
        mm = ((rec.source == "SCF") & (rec.year == y)).to_numpy()
        s = rec[mm]
        t2 = s[T2_INC].to_numpy(float).sum(axis=1)
        w_all = s.weight.to_numpy(float)
        g = np.searchsorted(EDGES, wrank(t2, w_all), side="right")
        ob = s.wealth_observed.to_numpy().astype(bool)
        mi = br.E19200.to_numpy(float)[mm]
        s, w, g, mi = s[ob], w_all[ob], g[ob], mi[ob]
        assert not np.isnan(mi).any()
        home, mort = s.primary_home.to_numpy(float), s.primary_mortgage.to_numpy(float)
        nw = s[ASSETS].to_numpy(float).sum(1) - s[DEBTS].to_numpy(float).sum(1)
        res[f"SCF {y}"] = {"agi": cells(home, mort, nw, w, g, GLAB),
                           "band": cells(home, mort, nw, w, band(mi, np.ones(len(mi), bool)), BLAB)}
    a, b = res["SCF 2013"], res["SCF 2016"]
    interp = {}
    for kind in a:
        interp[kind] = {}
        for lab in a[kind]:
            interp[kind][lab] = {}
            for st, va in a[kind][lab].items():
                vb = b[kind][lab][st]
                if st in ("share", "own_home", "has_mortgage") or not (va > 0 and vb > 0):
                    interp[kind][lab][st] = va / 3 + 2 * vb / 3
                else:
                    interp[kind][lab][st] = float(np.exp(np.log(va) / 3 + 2 * np.log(vb) / 3))
    res["SCF 2015"] = interp

    p = rec[rec.source == "PUF"].copy()
    p["row"] = np.flatnonzero((rec.source == "PUF").to_numpy())
    pw = p.weight.to_numpy(float)
    gp = pd.Series(np.searchsorted(EDGES, wrank(p.E00100.to_numpy(float), pw), side="right"), index=p.row.to_numpy())
    item = p.FDED.to_numpy(float) == 1
    bp = pd.Series(band(p.E19200.to_numpy(float), item), index=p.row.to_numpy())
    res["PUF band shares"] = {lab: float(pw[bp.to_numpy() == k].sum() / pw.sum()) for k, lab in enumerate(BLAB)}
    for lab, pat in MODELS:
        for f in sorted(glob.glob(pat)):
            sd = int(re.search(r"seed(\d+)", os.path.basename(f)).group(1))
            C = pd.read_parquet(f, columns=["row", "draw"] + ASSETS + DEBTS)
            K = int(C.draw.nunique())
            C = C.merge(p[["row", "weight"]], on="row", how="left", validate="many_to_one")
            assert not C.weight.isna().any()
            w = C.weight.to_numpy(float) / K
            home, mort = C.primary_home.to_numpy(float), C.primary_mortgage.to_numpy(float)
            nw = C[ASSETS].to_numpy(float).sum(1) - C[DEBTS].to_numpy(float).sum(1)
            res.setdefault(lab, {})[sd] = {"agi": cells(home, mort, nw, w, gp.loc[C.row].to_numpy(), GLAB),
                                           "band": cells(home, mort, nw, w, bp.loc[C.row].to_numpy(), BLAB),
                                           "draws": K, "file": f}
            print(f"<!-- {lab} seed {sd}: {f}, {K} draws -->", flush=True)
    json.dump(res, open(f"{Q}/dd_wealth.json", "w"), indent=1)

    def fm(st, v):
        if st in ("own_home", "has_mortgage", "share"):
            return f"{100 * v:.0f}%"
        return f"${v / 1e6:.2f}m" if abs(v) >= 1e6 else f"${v / 1e3:,.0f}k"

    def cell(vals, st):
        if not vals:
            return "pending"
        if len(vals) == 1:
            return fm(st, vals[0])
        return f"{fm(st, np.mean(vals))} [{fm(st, min(vals))}–{fm(st, max(vals))}]"

    cols = ["SCF 2015"] + [lab for lab, _ in MODELS]
    for kind, labels, title in (("agi", ["All"] + GLAB, "AGI group (SCF: seven-income group)"),
                                ("band", ["All"] + BLAB, "mortgage-interest band (PUF: claimed; SCF: paid)")):
        for st, name in (("own_home", "Own a home"), ("home_value", "Mean home value"),
                         ("has_mortgage", "Have a mortgage"), ("mortgage", "Mean mortgage debt"),
                         ("nw_mean", "Mean net worth"), ("nw_median", "Median net worth")):
            print(f"\n### {name}, by {title}\n")
            print("Mean over training seeds, range in brackets.\n")
            print("| Group | " + " | ".join(cols) + " |")
            print("|---" * (len(cols) + 1) + "|")
            for gl in labels:
                row = [cell([res["SCF 2015"][kind][gl][st]] if gl in res["SCF 2015"][kind] else [], st)]
                for lab, _ in MODELS:
                    row.append(cell([v[kind][gl][st] for v in res.get(lab, {}).values() if gl in v[kind]], st))
                print(f"| {gl} | " + " | ".join(row) + " |")
    print("\nPUF share of returns by band (claimed interest): " +
          ", ".join(f"{k} {100 * v:.1f}%" for k, v in res["PUF band shares"].items()))
    print("SCF 2015 share of units by band (paid interest): " +
          ", ".join(f"{k} {100 * v['share']:.1f}%" for k, v in res["SCF 2015"]["band"].items() if k != "All"))


if __name__ == "__main__":
    main()
