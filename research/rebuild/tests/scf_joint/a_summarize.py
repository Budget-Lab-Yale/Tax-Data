"""Test 2 piece A: tables for DATA_REPORT.md (aggregates only)."""
import os, sys
import numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import a_tokenizer as tk
D = tk.DATA
pd.set_option("display.width", 250); pd.set_option("display.max_rows", 500)
for ds in ["expB", "expA_clean"]:
    rt = pd.read_csv(os.path.join(D, ds, "roundtrip.csv"))
    qc = [c for c in rt.columns if c.startswith("nonzero_q")]
    rt["max_abs_quantile_err_q10_to_q99"] = rt[[c for c in qc if c != "nonzero_q0.999_rel_err"]].abs().max(axis=1)
    rt["abs_q999_err"] = rt["nonzero_q0.999_rel_err"].abs()
    print("####", ds)
    print(rt[["variable", "source", "year", "share_zero_true", "total_rel_err_mean_5_seeds", "total_rel_err_sd_5_seeds",
              "max_abs_quantile_err_q10_to_q99", "abs_q999_err"]].round(4).to_markdown(index=False))
# variants: how much half 1's income tokens move relative to the clean dataset
meta = tk.load_meta("expA_clean")
inc = [k for k, v in enumerate(meta["variables"]) if v["group"] == "shared" and v["kind"] == "amount"]
c = np.load(os.path.join(D, "expA_clean", "tokens_test.npy"))
rows = []
for ds in ["expA_noise", "expA_recat", "expA_unrep"]:
    t = np.load(os.path.join(D, ds, "tokens_test.npy"))
    m2 = tk.load_meta(ds)
    for k in inc:
        v = meta["variables"][k]; v2 = m2["variables"][k]
        # compare as rank midpoints (vocabularies are the same layout)
        lo = np.array([x["lo"] for x in v["tokens"]]); hi = np.array([x["hi"] for x in v["tokens"]])
        kind = np.array([x["kind"] for x in v["tokens"]])
        same = (t[:, k] == c[:, k]).mean()
        zc, zt = kind[c[:, k]] == "zero", kind[t[:, k]] == "zero"
        both = (kind[c[:, k]] == "pos") & (kind[t[:, k]] == "pos")
        mc, mt = (lo + hi)[c[:, k]] / 2, (lo + hi)[t[:, k]] / 2
        rho = np.corrcoef(mc[both], mt[both])[0, 1] if both.sum() > 2 else np.nan
        rows.append({"dataset": ds, "variable": v["name"], "share_tokens_unchanged": same,
                     "share_zero_clean": zc.mean(), "share_zero_variant": zt.mean(),
                     "rank_corr_positive_both": rho})
print(pd.DataFrame(rows).round(3).to_markdown(index=False))
