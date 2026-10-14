"""Hypothesis 2 diagnostics, second pass (test 1). Aggregates only.

1. The weight (S006): the model is trained unweighted, so its first
   conditional (nothing visible) should reproduce the RECORD-COUNT
   distribution of the weight token; compare unweighted.
2. Joint with AGI band: for every variable after AGI in the default order,
   the table (variable, coarse groups) x (AGI band), built three ways on the
   validation records and compared with the observed table:
     teacher-forced: sum of weight x predicted distribution given the true
       earlier variables, placed in the record's true AGI band;
     noise: the same with one value drawn per record from that
       distribution (what a perfectly calibrated model would show);
     free-running: the generated files' own table.
   Also age range x AGI band for the generated files against validation, and
   noise draws for the teacher-forced age x AGI tables of diag2_calib.py.
Writes $S/diag2/joint_with_agi.csv and prints aggregates.
"""
from pathlib import Path

import numpy as np
import pandas as pd
import torch

from diag2_calib import AGI_EDGES, AGI_LABELS, coarse_map, teacher_forced, tvd, wdist
from model import generation_order, load_checkpoint, load_meta

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
OUT = S / "diag2"
GEN = {
    "base": ["synthetic_base", "synthetic_base_s20261010", "synthetic_base_s20261011",
             "synthetic_base_s20261012"],
    "large": ["synthetic", "synthetic_large_s20261010", "synthetic_large_s20261011",
              "synthetic_large_s20261012"],
    "base_demoglast": ["synthetic_base_demoglast"], "base_gibbs2": ["synthetic_base_gibbs2"],
}


def main():
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    meta = load_meta(S / "data" / "meta.json")
    vars_ = meta["variables"]
    names = [v["name"] for v in vars_]
    ix = {n: i for i, n in enumerate(names)}
    K = [int(v["vocab_size"]) for v in vars_]
    V = len(names)
    tv = np.load(S / "data" / "tokens_val.npy").astype(np.int64)
    tt = np.load(S / "data" / "tokens_train.npy").astype(np.int64)
    w = pd.read_parquet(S / "data" / "values_val.parquet", columns=["weight"])["weight"].to_numpy()
    wt = pd.read_parquet(S / "data" / "values_train.parquet", columns=["weight"])["weight"].to_numpy()
    cmap = [coarse_map(vars_[j], tt[:, j]) for j in range(V)]
    nG = [cmap[j].max() + 1 for j in range(V)]
    rng = np.random.default_rng(20261008 + 1)
    jg, ja = ix["E00100"], ix["AGERANGE"]
    agi_lo = np.array([float(t["lo"]) for t in vars_[jg]["tokens"]])
    agi_hi = np.array([float(t["hi"]) for t in vars_[jg]["tokens"]])
    agi_mid = np.where(agi_lo == agi_hi, agi_lo, 0.5 * (agi_lo + agi_hi))
    tok_band = pd.cut(pd.Series(agi_mid), AGI_EDGES, labels=AGI_LABELS,
                      right=False).cat.codes.to_numpy()
    nB = len(AGI_LABELS)
    # band from the TOKEN, so generated files and validation are treated
    # identically
    band_v = tok_band[tv[:, jg]]
    order = generation_order(meta)
    after = order[order.index(jg) + 1:]

    def joint(tokcol, band, wts, j):
        t = np.zeros((nG[j], nB))
        np.add.at(t, (cmap[j][tokcol], band), wts)
        return t / t.sum()

    def joint_probs(P, band, wts, j):
        # P: [N, K_j] -> coarse groups
        Pc = np.zeros((P.shape[0], nG[j]))
        for g in range(nG[j]):
            Pc[:, g] = P[:, cmap[j] == g].sum(1)
        t = np.zeros((nG[j], nB))
        for b in range(nB):
            s = band == b
            t[:, b] = (wts[s, None] * Pc[s]).sum(0)
        return t / t.sum()

    def draw(P):
        u = rng.random(P.shape[0])[:, None]
        return (P.cumsum(1) < u).sum(1).clip(max=P.shape[1] - 1)

    obsj = {j: joint(tv[:, j], band_v, w, j) for j in after}
    trj = {j: joint(tt[:, j], tok_band[tt[:, jg]], wt, j) for j in after}
    rows = []
    for j in after:
        rows.append(dict(source="train records", variable=names[j], tvd=tvd(trj[j], obsj[j])))
    gens = {}
    for lab, stems in GEN.items():
        for st in stems:
            g = np.load(S / "model" / f"{st}_tokens.npy").astype(np.int64)
            gw = pd.read_parquet(S / "model" / f"{st}.parquet", columns=["weight"])["weight"].to_numpy()
            gens[st] = (g, gw)
            for j in after:
                rows.append(dict(source=f"free:{lab}", file=st, variable=names[j],
                                 tvd=tvd(joint(g[:, j], tok_band[g[:, jg]], gw, j), obsj[j])))
    # age x AGI band (all records, bands from tokens) for generated files vs val
    def agetab(tok, wts):
        t = np.zeros((K[ja], nB))
        np.add.at(t, (tok[:, ja], tok_band[tok[:, jg]]), wts)
        return t / t.sum()
    age_obs = agetab(tv, w)
    age_rows = [dict(source="train records", tvd=tvd(agetab(tt, wt), age_obs))]
    for st, (g, gw) in gens.items():
        age_rows.append(dict(source=f"free:{st}", tvd=tvd(agetab(g, gw), age_obs)))

    s006 = []
    for m in ("base", "large"):
        model, _ = load_checkpoint(S / "model" / f"checkpoint_{m}.pt", meta, device)
        P = teacher_forced(model, tv, order, device)
        j0 = ix["S006"]
        p0 = P[j0].mean(0)
        obs_unw = np.bincount(tv[:, j0], minlength=K[j0]) / len(tv)
        tr_unw = np.bincount(tt[:, j0], minlength=K[j0]) / len(tt)
        s006.append(dict(model=m, tvd_unweighted_vs_val=tvd(p0, obs_unw),
                         noise=tvd(p0, np.bincount(draw(P[j0]), minlength=K[j0]) / len(tv)),
                         train_vs_val=tvd(tr_unw, obs_unw),
                         implied_total_weight_ratio=(p0 * np.array(
                             [np.average(wt[tt[:, j0] == k]) if (tt[:, j0] == k).any() else 0
                              for k in range(K[j0])])).sum() * len(tt) / wt.sum()))
        for j in after:
            rows.append(dict(source=f"teacher_forced:{m}", variable=names[j],
                             tvd=tvd(joint_probs(P[j], band_v, w, j), obsj[j])))
            rows.append(dict(source=f"noise:{m}", variable=names[j],
                             tvd=tvd(joint_probs(P[j], band_v, w, j),
                                     joint(draw(P[j]), band_v, w, j))))
        # noise for the age x AGI teacher-forced tables (all records here)
        tabA = np.zeros((K[ja], nB))
        Pg = P[jg]
        Pgb = np.stack([Pg[:, tok_band == b].sum(1) for b in range(nB)], 1)
        for k in range(K[ja]):
            s = tv[:, ja] == k
            tabA[k] = (w[s, None] * Pgb[s]).sum(0)
        tabA /= tabA.sum()
        d = draw(Pg)
        tabD = np.zeros((K[ja], nB))
        np.add.at(tabD, (tv[:, ja], tok_band[d]), w)
        tabD /= tabD.sum()
        age_rows.append(dict(source=f"teacher_forced:{m}: AGI given true age and earlier",
                             tvd=tvd(tabA, age_obs)))
        age_rows.append(dict(source=f"noise:{m}: one AGI draw from that", tvd=tvd(tabA, tabD)))
        del P, model
        torch.cuda.empty_cache()

    res = pd.DataFrame(rows)
    res.to_csv(OUT / "joint_with_agi.csv", index=False)
    pd.DataFrame(age_rows).to_csv(OUT / "agexagi_vs_val.csv", index=False)
    pd.DataFrame(s006).to_csv(OUT / "s006_unweighted.csv", index=False)
    pd.set_option("display.width", 250, "display.max_rows", 500)
    piv = res.groupby(["variable", "source"])["tvd"].mean().unstack()
    piv = piv.reindex([names[j] for j in after])
    print("== Joint (variable coarse group x AGI band) TVD vs validation; free = mean over seeds")
    print(piv.round(4).to_string())
    print("\nMean over variables")
    print(piv.mean().round(4).to_string())
    print("\n== age range x AGI band (all records) vs validation")
    print(pd.DataFrame(age_rows).round(4).to_string())
    print("\n== S006 unweighted")
    print(pd.DataFrame(s006).round(4).to_string())
    print("done", flush=True)


if __name__ == "__main__":
    main()
