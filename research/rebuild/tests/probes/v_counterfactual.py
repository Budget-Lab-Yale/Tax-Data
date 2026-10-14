"""Probe V: how much of the gap in simplified 2015 income tax by AGI group
comes from the itemized deductions? For each generated file, scale each of
the six itemized amounts within each AGI group so that its weighted group
total equals the real PUF's, recompute the simplified tax (v_score.tax2015),
and compare. Aggregates only.
Usage: python v_counterfactual.py <out json> LABEL=PATH ..."""
import json
import sys

import numpy as np
import pandas as pd

import v_score as vs

rec = pd.read_parquet(vs.DATA / "records_train.parquet")
real = vs.prepare(rec[rec["source"].astype(str) == "PUF"].reset_index(drop=True))
wr = real["weight"].to_numpy(float)
agi_r = real["E00100"].to_numpy(float)
edges = np.array([-np.inf, vs.wq(agi_r, wr, 0.5), vs.wq(agi_r, wr, 0.9), vs.wq(agi_r, wr, 0.99), vs.wq(agi_r, wr, 0.999)])
gr = vs.group_index(agi_r, edges)
res = {"real": vs.tax_tables(real, wr, gr, agi_r)}
for item in sys.argv[2:]:
    lab, path = item.split("=", 1)
    df = vs.prepare(pd.read_parquet(path))
    w = df["weight"].to_numpy(float)
    agi = df["E00100"].to_numpy(float)
    g = vs.group_index(agi, edges)
    before = vs.tax_tables(df, w, g, agi)
    adj = df.copy()
    factors = {}
    for v in vs.ITEMIZED:
        x = adj[v].to_numpy(float).copy()
        for k, gl in enumerate(vs.GROUPS):
            tr = (real[v].to_numpy(float)[gr == k] * wr[gr == k]).sum()
            tg = (x[g == k] * w[g == k]).sum()
            f = tr / tg if tg > 0 else 1.0
            x[g == k] *= f
            factors[f"{v} {gl}"] = f
        adj[v] = x
    after = vs.tax_tables(adj, w, g, agi)
    res[lab] = dict(before=before, after=after, factors=factors)
    print(lab, {gl: (round(before[gl]["net_tax_bn"], 1), round(after[gl]["net_tax_bn"], 1),
                     round(res["real"][gl]["net_tax_bn"], 1)) for gl in vs.GROUPS + ["all"]}, flush=True)
json.dump(res, open(sys.argv[1], "w"), indent=1)
