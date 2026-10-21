"""Probe BR: completed net worth of PUF itemizers by cash charity band (E19800),
version 2 against the bridge models; SCF filer units in the same band (full_v3brc,
SCF amounts of $500 or more). Weighted aggregates only."""
import numpy as np, pandas as pd
import br_analyze as A
rv2 = pd.read_parquet(f"{A.V2}/data/records_train.parquet", columns=["source", "FDED", "E19800", "E00100"])
brc = pd.concat([pd.read_parquet(f"{A.Q}/data/full_v3brc/records_{s}.parquet") for s in ("train", "val", "test")])
sb = brc.loc[(brc.source == "SCF") & brc.wealth_observed & brc.E19800.notna()].copy(); sb["nw"] = A.nw(sb)
edges = [0, 1, 2e3, 5e3, 2e4, 1e5, np.inf]
bl = ["0", "under $2k", "$2k-5k", "$5k-20k", "$20k-100k", "$100k and over"]
comp = {}
for lab, D, s in [("Version 2, seed 1", A.V2, 1), ("Version 2, seed 2", A.V2, 2), ("Bridge, seed 1", A.BR, 1),
                  ("Bridge, seed 2", A.BR, 2), ("Bridge + charity, seed 1", A.BRC, 1), ("Bridge + charity, seed 2", A.BRC, 2)]:
    c = A.load_comp(D, "puf_wealth", s, ["row", "draw", "weight"] + A.ASSETS + A.DEBTS)
    c["FDED"] = rv2.FDED.to_numpy()[c.row]; c["E19800"] = rv2.E19800.to_numpy()[c.row]; c["nw"] = A.nw(c)
    comp[lab] = c.loc[c.FDED == 1]
band = lambda x: np.digitize(x, edges[1:-1])
for title, f in [("Mean net worth", lambda d: A.money(np.average(d.nw, weights=d.weight))),
                 ("Median net worth", lambda d: A.money(A.wmed(d.nw.to_numpy(), d.weight.to_numpy())))]:
    rows = []
    for b in range(len(bl)):
        c0 = comp["Version 2, seed 1"]; d0 = c0.loc[c0.draw == 0]
        r = [bl[b], f"{100 * d0.weight[band(d0.E19800.to_numpy()) == b].sum() / d0.weight.sum():.0f}"]
        for n, c in comp.items():
            r.append(f(c.loc[band(c.E19800.to_numpy()) == b]))
        d = sb.loc[band(sb.E19800.to_numpy()) == b]
        r.append(f(d) if len(d) else "")
        rows.append(r)
    A.table(f"Cash charity deducted (E19800), PUF itemizers: {title}", rows,
            ["Band", "Itemizers in band (%)"] + list(comp) + ["SCF units in band"])
open(f"{A.Q}/analysis/br_charity_bands.md", "w").write("\n".join(A.OUTL) + "\n")
