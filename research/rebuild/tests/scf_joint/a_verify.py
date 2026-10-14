"""Test 2 piece A: consistency checks of the prepared datasets (no record values printed)."""
import json, os, sys
import numpy as np, pandas as pd
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import a_tokenizer as tk

pd.set_option("display.width", 250); pd.set_option("display.max_columns", 40); pd.set_option("display.max_rows", 400)
for ds in sys.argv[1:]:
    print("=====", ds, flush=True)
    meta = tk.load_meta(ds); q = tk.load_quantiles(ds)
    names = meta["variable_names"]
    for p in ("train", "val", "test"):
        t = np.load(os.path.join(tk.DATA, ds, f"tokens_{p}.npy")); u = np.load(os.path.join(tk.DATA, ds, f"truth_{p}.npy"))
        r = pd.read_parquet(os.path.join(tk.DATA, ds, f"records_{p}.parquet"))
        assert len(t) == len(u) == len(r), p
        # tokens equal truth wherever tokens observed; vocab ranges
        obs = t >= 0
        assert (t[obs] == u[obs]).all()
        for k, v in enumerate(meta["variables"]):
            assert u[:, k].max() < v["vocab_size"]
        # hidden pattern by source
        rows = []
        for s in meta["sources"]:
            m = (r.source == s).to_numpy()
            if not m.any(): continue
            hid_t = (t[m] < 0).mean(axis=0); hid_u = (u[m] < 0).mean(axis=0)
            groups = {g: [k for k, v in enumerate(meta["variables"]) if v["group"] == g] for g in ("context", "shared", "scf_only")}
            rows.append({"part": p, "source": s, "records": int(m.sum()),
                         **{f"tokens_hidden_share_{g}": float(hid_t[ks].mean()) for g, ks in groups.items()},
                         **{f"truth_missing_share_{g}": float(hid_u[ks].mean()) for g, ks in groups.items()}})
        print(pd.DataFrame(rows).to_string(), flush=True)
        # households not shared across parts checked in prepare; source/year in records consistent with token col 0
        src_tok = np.array([tk_["value"] for tk_ in meta["variables"][0]["tokens"]])[t[:, 0]]
        assert (src_tok == r.source.to_numpy()).all()
    # round-trip summary
    rt = pd.read_csv(os.path.join(tk.DATA, ds, "roundtrip.csv"))
    qc = [c for c in rt.columns if c.startswith("nonzero_q")]
    rt["max_abs_q_err"] = rt[qc].abs().max(axis=1)
    print(rt.groupby(["source", "year"]).agg(vars=("variable", "size"), max_abs_share_zero_diff=("share_zero_true", lambda s: 0),
          max_abs_total_rel_err=("total_rel_err", lambda s: s.abs().max()), median_abs_total_rel_err=("total_rel_err", lambda s: s.abs().median()),
          max_abs_quantile_rel_err=("max_abs_q_err", "max")).to_string())
    print((rt.share_zero_true - rt.share_zero_decoded).abs().max(), "max abs share-zero difference")
    print(rt[["variable", "source", "year", "records", "total_rel_err"] + qc].round(4).to_string())
    if ds == "expB":
        # PUF-year decoding path: decode SCF 2016 test truth as if PUF records at 2015
        r = pd.read_parquet(os.path.join(tk.DATA, ds, "records_test.parquet")); u = np.load(os.path.join(tk.DATA, ds, "truth_test.npy"))
        m = ((r.source == "SCF") & (r.year == 2016)).to_numpy()
        rr = r.loc[m].reset_index(drop=True).assign(source="PUF", year=2015)
        uu = u[m].copy()
        shared = [k for k, v in enumerate(meta["variables"]) if v["group"] == "shared"]
        uu[:, shared] = -1   # only wealth decoded
        d15 = tk.decode(uu, meta, rr, q, np.random.default_rng(1), year=2015)
        d16 = tk.decode(u[m], meta, r.loc[m].reset_index(drop=True), q, np.random.default_rng(1))
        d13 = tk.decode(uu, meta, rr.assign(source="PUF"), q, np.random.default_rng(1), year=2013)
        w = rr.weight.to_numpy()
        out = []
        for v in ["cash", "equities", "dc", "primary_home", "pass_throughs", "primary_mortgage"]:
            out.append({"variable": v, "total_2013_fn": np.sum(w * d13[v]) / 1e9, "total_2015_interp": np.sum(w * d15[v]) / 1e9, "total_2016_fn": np.sum(w * d16[v]) / 1e9})
        print("same SCF 2016 test tokens decoded with the 2013, 2015 (interpolated) and 2016 quantile functions, $bn:")
        print(pd.DataFrame(out).round(1).to_string())
