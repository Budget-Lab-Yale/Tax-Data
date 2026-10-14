"""Probe TR: completed wealth on SCF test records (their shared tax variables
known) against the records' true wealth, trees against probe C's transformer
(full_v3cz, model seed 1). Per SCF wave; groups are the seven-income total's
weighted rank within the wave (x_compare.py's grouping); every statistic
weights a draw by its record's weight over the number of draws. Records with
wealth unobserved (split-off units) are left out. Aggregates only.

    python tr_scftest.py > $Q/tables/tr_scftest.md
"""
import glob
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from tr_wealth import ASSETS, DEBTS, EDGES, LABELS, T2_INC, WEALTH, nw_of, topshare, wq, wrank  # noqa: E402

DATA = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/full_v3cz"
C3 = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3"
TRQ = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr"


def main():
    rec = pd.read_parquet(f"{DATA}/records_test.parquet")
    rec["row"] = np.arange(len(rec))
    s = rec[rec.source.astype(str) == "SCF"].copy()
    have_t2 = all(c in s.columns for c in T2_INC)
    files = {"C transformer, bridge observed (seed 1)": f"{C3}/model_cz/h_scf_test_seed01.parquet",
             "C transformer, bridge hidden (seed 1)": f"{C3}/model_cz/h_scf_test_nobr_seed01.parquet"}
    for f in sorted(glob.glob(f"{TRQ}/comp/tr_scf_test*.parquet")):
        files["trees, " + os.path.basename(f)[3:-8]] = f
    rows = []
    for y in (2013, 2016):
        sy = s[(s.year.astype(int) == y)]
        obs = sy[WEALTH].notna().all(axis=1).to_numpy()
        inc = sy[T2_INC].to_numpy(float).sum(axis=1) if have_t2 else sy["E00200"].to_numpy(float)
        g = pd.Series(np.searchsorted(EDGES, wrank(inc, sy.weight.to_numpy(float)), side="right"),
                      index=sy.row.to_numpy())
        so = sy[obs]
        cand = {"truth": (so, so.weight.to_numpy(float), g.loc[so.row].to_numpy())}
        for lab, f in files.items():
            if not os.path.exists(f):
                continue
            C = pd.read_parquet(f, columns=["row", "draw"] + WEALTH)
            K = C.draw.nunique()
            C = C[C.row.isin(so.row)].merge(so[["row", "weight"]], on="row", how="left")
            cand[lab] = (C, C.weight.to_numpy(float) / K, g.loc[C.row].to_numpy())
        for lab, (F, w, gg) in cand.items():
            nw = nw_of(F)
            r = {"wave": y, "source": lab, "mean NW ($k)": np.sum(w * nw) / w.sum() / 1e3,
                 "median NW ($k)": wq(nw, w, 0.5) / 1e3, "top 1% share": topshare(nw, w, 0.01),
                 "top 0.1% share": topshare(nw, w, 0.001),
                 "owns home": w[F.primary_home.to_numpy(float) != 0].sum() / w.sum(),
                 "holds equities": w[F.equities.to_numpy(float) != 0].sum() / w.sum(),
                 "holds pass-throughs": w[F.pass_throughs.to_numpy(float) != 0].sum() / w.sum()}
            for k, gl in enumerate(LABELS):
                m = gg == k
                r[f"mean NW {gl} ($k)"] = np.sum(w[m] * nw[m]) / w[m].sum() / 1e3
            rows.append(r)
    out = pd.DataFrame(rows)
    print(f"Grouping by {'the seven-income total' if have_t2 else 'wages (seven-income total not in records)'}.\n")
    for y in (2013, 2016):
        print(f"### SCF {y} test records\n")
        t = out[out.wave == y].drop(columns="wave").set_index("source").T
        try:
            print(t.to_markdown(floatfmt=",.3g"))
        except ImportError:
            print(t.to_string(float_format=lambda x: f"{x:,.3g}"))
        print()


if __name__ == "__main__":
    main()
