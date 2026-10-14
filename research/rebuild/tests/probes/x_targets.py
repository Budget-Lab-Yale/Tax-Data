"""Probe X: rebuild test 3's arm P wealth targets (a_full.py's `wcore` and
`wfull`: SCF wealth shares within groups, 2013 and 2016 interpolated to
2015, times the PUF group weights) from the translated dataset full_v3x,
and measure how far the target families disagree, before and after
translation, with C's measure (c_fit.py: for every wealth variable and
bin, the spread across the families that cover every PUF record of the
total weight each implies, as a share of all PUF weight).

    python x_targets.py build      # targets from full_v3x into $Q/a_v3x
    python x_targets.py measure    # disagreement before (test 3's files) and after

The build calls a_full.build() unchanged (imported from ../posttrain), only
pointed at full_v3x and an output folder of this probe.
"""

import json
import os
import sys
from pathlib import Path

import numpy as np
import pandas as pd

POST = Path("/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain")
sys.path.insert(0, str(POST))
Q = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/x")
BEFORE = Path("/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/full_v2/a")
XVER = os.environ.get("XVER", "full_v3x2")
AFTERS = {"full_v3x": Q / "a_v3x", "full_v3x2": Q / "a_v3x2"}
AFTER = AFTERS[XVER]


def build():
    import a_common as A
    import a_full as AF
    A.configure(Q / "data" / XVER,
                Q / "model" / XVER / f"checkpoint_{XVER}_w1_seed01.pt",
                AFTER, POST / "g_tokenizer.py")
    AFTER.mkdir(parents=True, exist_ok=True)
    AF.build(False)


def disagreement(cells, W):
    tt = cells.assign(t=cells["target"].to_numpy(np.float64))
    fam_tot = tt.groupby(["family", "variable"])["t"].sum()
    full_fams = [f for f in tt["family"].unique()
                 if np.allclose(fam_tot.loc[f].to_numpy(), W, rtol=1e-6)]
    vb = tt[tt["family"].isin(full_fams)].groupby(["variable", "bin", "family"])["t"].sum().unstack("family")
    spread = (vb.max(axis=1) - vb.min(axis=1)) / W
    top = spread.sort_values(ascending=False).head(10)
    lab = cells.drop_duplicates(["variable", "bin"]).set_index(["variable", "bin"])["bin_label"]
    by_var = spread.groupby(level=0).max().sort_values(ascending=False)
    # which family is furthest from the mean of the families, per variable-bin
    dev = vb.sub(vb.mean(axis=1), axis=0).abs().div(W)
    worst_family = dev.max(axis=0).sort_values(ascending=False)
    return dict(families_covering_all_records=full_fams,
                max_spread_share_of_total_weight=float(spread.max()),
                mean_spread_share_of_total_weight=float(spread.mean()),
                median_spread=float(spread.median()),
                largest=[dict(variable=k[0], bin=int(k[1]), bin_label=str(lab.get(k, "")), spread=float(x))
                         for k, x in top.items()],
                max_spread_by_variable={k: float(x) for k, x in by_var.items()},
                largest_deviation_from_family_mean_by_family={k: float(x) for k, x in worst_family.items()})


def group_mix(cells):
    """Per family: total variation distance between the SCF's group shares
    (wealth-observed training rows, each wave) and the PUF's group shares."""
    out = {}
    g = cells.drop_duplicates(["family", "group"])[["family", "group", "group_label", "puf_group_weight",
                                                     "scf_group_weight_2013", "scf_group_weight_2016"]]
    for f, d in g.groupby("family"):
        p = d.puf_group_weight / d.puf_group_weight.sum()
        r = {}
        for y in (2013, 2016):
            s = d[f"scf_group_weight_{y}"].fillna(0)
            s = s / s.sum()
            r[f"tvd_{y}"] = float(0.5 * np.abs(p.to_numpy() - s.to_numpy()).sum())
        r["groups"] = {str(gl): [round(float(a), 4), round(float(b), 4), round(float(c), 4)]
                       for gl, a, b, c in zip(d.group_label, p,
                                              d.scf_group_weight_2013.fillna(0) / d.scf_group_weight_2013.fillna(0).sum(),
                                              d.scf_group_weight_2016.fillna(0) / d.scf_group_weight_2016.fillna(0).sum())}
        out[f] = r
    return out


def measure():
    res = {}
    afters = [(f"after ({v}, translated)", d) for v, d in AFTERS.items() if (d / "DONE_targets").exists()]
    for label, d in [("before (full_v2, test 3's files)", BEFORE)] + afters:
        recs = pd.read_parquet(d / "records_puf.parquet", columns=["weight"])
        W = float(recs["weight"].sum())
        r = {"puf_weight": W}
        for s in ("wcore", "wfull"):
            cells = pd.read_parquet(d / f"targets_{s}.parquet")
            r[s] = dict(n_cells=int(len(cells)), zero_cells=int((cells.target == 0).sum()),
                        nan_cells=int(cells.target.isna().sum()),
                        disagreement=disagreement(cells, W), group_mix=group_mix(cells))
        res[label] = r
    for label, r in res.items():
        for s in ("wcore", "wfull"):
            dd = r[s]["disagreement"]
            print(f"{label} {s}: cells {r[s]['n_cells']}, max spread {dd['max_spread_share_of_total_weight']:.4f}, "
                  f"mean {dd['mean_spread_share_of_total_weight']:.5f}; largest {dd['largest'][:4]}", flush=True)
            print("   group mix TVD:", {f: (round(v['tvd_2013'], 4), round(v['tvd_2016'], 4))
                                        for f, v in r[s]["group_mix"].items()}, flush=True)
    # Variant: PUF group weights from returns that are not dependent filers
    # only (the population the SCF's filer units describe).
    for label, d in [("before, PUF without dependent filers", BEFORE)] + [(f"after ({v}), PUF without dependent filers", d) for v, d in AFTERS.items() if (d / "DONE_targets").exists()]:
        recs = pd.read_parquet(d / "records_puf.parquet")
        nd = (recs["tok_DSI"].to_numpy() == 0)
        wv = recs["weight"].to_numpy(float) * nd
        W = float(wv.sum())
        r = {"puf_weight": W}
        for s in ("wcore", "wfull"):
            cells = pd.read_parquet(d / f"targets_{s}.parquet").copy()
            gw = np.zeros(len(cells))
            for fam in cells.family.unique():
                col = f"g_{s}_{fam}"
                g = recs[col].to_numpy()
                tot = np.bincount(np.maximum(g, 0), weights=wv * (g >= 0), minlength=int(g.max()) + 1)
                m = (cells.family == fam).to_numpy()
                gw[m] = tot[cells.group.to_numpy()[m]]
            cells["puf_group_weight"] = gw
            cells["target"] = cells["s_2015"] * gw
            r[s] = dict(disagreement=disagreement(cells, W))
            dd = r[s]["disagreement"]
            print(f"{label} {s}: max spread {dd['max_spread_share_of_total_weight']:.4f}, "
                  f"mean {dd['mean_spread_share_of_total_weight']:.5f}; largest {dd['largest'][:3]}", flush=True)
        res[label] = r
    # Who is in each population: married-joint share and dependents, SCF
    # all filer units against those with wealth observed (split-off units
    # excluded), PUF all returns against returns that are not dependent filers.
    pop = {}
    for tag, path in (("full_v2", Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2")),
                      ("full_v3x", Q / "data" / "full_v3x"), ("full_v3x2", Q / "data" / "full_v3x2")):
        r = pd.read_parquet(path / "records_train.parquet")
        for nm, m in (("PUF all", (r.source == "PUF")), ("PUF not dependent filers", (r.source == "PUF") & (r.DSI == 0)),
                      ("SCF 2013 all filer units", (r.source == "SCF") & (r.year == 2013)),
                      ("SCF 2013 wealth observed", (r.source == "SCF") & (r.year == 2013) & r.wealth_observed),
                      ("SCF 2016 all filer units", (r.source == "SCF") & (r.year == 2016)),
                      ("SCF 2016 wealth observed", (r.source == "SCF") & (r.year == 2016) & r.wealth_observed)):
            g = r[m]
            w = g.weight.to_numpy(float)
            dep = np.clip(g.XTOT.to_numpy(float) - 1 - (g.MARS.to_numpy(float) == 2), 0, 3)
            pop[f"{tag} | {nm}"] = {"weight_m": round(w.sum() / 1e6, 2),
                                    "joint_share": round(float(w[g.MARS.to_numpy() == 2].sum() / w.sum()), 4),
                                    "dependents_0_1_2_3plus": [round(float(w[dep == k].sum() / w.sum()), 4) for k in range(4)]}
    for k, v in pop.items():
        print(k, v, flush=True)
    res["populations"] = pop
    with open(Q / "x_targets_disagreement.json", "w") as f:
        json.dump(res, f, indent=1)


if __name__ == "__main__":
    {"build": build, "measure": measure}[sys.argv[1]]()
