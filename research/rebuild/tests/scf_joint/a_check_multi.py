"""Test 2 piece A: quick checks of expB_multi against expB (aggregates only)."""
import json, os, sys
import numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import a_tokenizer as tk
D = tk.DATA
mb, mm = tk.load_meta("expB"), tk.load_meta("expB_multi")
print("same variable order:", mb["variable_names"] == mm["variable_names"])
print("vocab sizes differ:", {v["name"]: (v["vocab_size"], w["vocab_size"]) for v, w in zip(mb["variables"], mm["variables"]) if v["vocab_size"] != w["vocab_size"]})
for p in ("train", "val", "test"):
    tb = np.load(os.path.join(D, "expB", f"tokens_{p}.npy")); tm = np.load(os.path.join(D, "expB_multi", f"tokens_{p}.npy"))
    ub = np.load(os.path.join(D, "expB", f"truth_{p}.npy")); um = np.load(os.path.join(D, "expB_multi", f"truth_{p}.npy"))
    rb = pd.read_parquet(os.path.join(D, "expB", f"records_{p}.parquet")); rm = pd.read_parquet(os.path.join(D, "expB_multi", f"records_{p}.parquet"))
    n = len(rb)
    key = ["source", "year", "hh_id", "implicate", "unit"]
    same_rows = rb[key].reset_index(drop=True).equals(rm[key].iloc[:n].reset_index(drop=True))
    print(p, "rows expB", n, "expB_multi", len(rm), "| expB rows are the prefix, same order:", same_rows,
          "| tokens identical:", bool((tb == tm[:n]).all()), "| truth identical:", bool((ub == um[:n]).all()))
    print("   columns whose tokens differ on expB rows:", [mb["variable_names"][k] for k in range(tb.shape[1]) if not (tb[:, k] == tm[:n, k]).all()])
    print("   rows by source-year:", rm.groupby(["source", "year"]).size().to_dict(),
          "| with wealth observed:", rm.loc[rm.source == "SCF"].groupby("year").wealth_observed.sum().to_dict())
r = pd.concat([pd.read_parquet(os.path.join(D, "expB_multi", f"records_{p}.parquet")) for p in ("train", "val", "test")])
hh = r.loc[r.source == "SCF"].groupby("year").hh_id.nunique().to_dict()
print("SCF households with filers, by wave:", hh)
s = r.loc[r.source == "SCF"]
W = ["cash","equities","bonds","dc","db","life_ins","annuities","trusts","other_fin","pass_throughs","primary_home","other_home","re_fund","other_nonfin"]
Dd = ["primary_mortgage","other_mortgage","credit_lines","credit_cards","installment_debt","other_debt"]
for y, g in s.groupby("year"):
    o = g.wealth_observed
    nw = g.loc[o, W].sum(axis=1) - g.loc[o, Dd].sum(axis=1)
    print(y, "filer units (millions) %.1f" % (g.weight.sum() / 1e6),
          "| filer net worth, own-year dollars ($tn) %.2f" % ((g.weight[o] * nw).sum() / 1e12),
          "| wages ($bn) %.0f" % ((g.weight * g.wages).sum() / 1e9),
          "| wealth-observed share of filer weight %.3f" % (g.weight[o].sum() / g.weight.sum()))
rt = pd.read_csv(os.path.join(D, "expB_multi", "roundtrip.csv"))
for (src, y), g in rt.groupby(["source", "year"]):
    a = g.total_rel_err_mean_5_seeds.abs()
    top = g.loc[a.sort_values(ascending=False).index[:3]]
    print("roundtrip", src, y, "median |total err| %.4f" % a.median(), "| largest:",
          [(v, round(m, 4), round(sd, 4)) for v, m, sd in zip(top.variable, top.total_rel_err_mean_5_seeds, top.total_rel_err_sd_5_seeds)],
          "| max share-zero diff", float((g.share_zero_true - g.share_zero_decoded).abs().max()))
