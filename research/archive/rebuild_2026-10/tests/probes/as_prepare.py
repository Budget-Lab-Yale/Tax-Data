"""Probe AS: build dataset full_v2s from test 3's full_v2: AGI (E00100) is
removed as a model variable and replaced, in the same column, by
AGI_REMAINDER = AGI - income items + adjustments (as_agi.py), encoded with
the same rank-band tokenizer (negative bands, zero, positive bands). PUF rows
only; SCF rows unobserved (as AGI was). Other columns are copied unchanged.

    python as_prepare.py
"""
import json
import os
import shutil
import sys
import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "posttrain"))
sys.path.insert(0, HERE)
import g_tokenizer as tk  # noqa: E402
import as_agi  # noqa: E402

SRC = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2"
OUT = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s"
SEED = 20261011 + 777
PARTS = ("train", "val", "test")


def main():
    os.makedirs(OUT, exist_ok=True)
    if os.path.exists(os.path.join(OUT, "DONE")):
        os.remove(os.path.join(OUT, "DONE"))
    meta = json.load(open(os.path.join(SRC, "meta.json")))
    names = meta["variable_names"]
    k = names.index("E00100")
    v = meta["variables"][k]
    assert v["column"] == k and v["kind"] == "amount" and v["binning"]["neg_token0"] == 0
    rec = {p: pd.read_parquet(os.path.join(SRC, f"records_{p}.parquet")) for p in PARTS}
    for p in PARTS:
        r = rec[p]
        puf = (r.source.astype(str) == "PUF").to_numpy()
        assert r.loc[~puf, "E00100"].isna().all()
        for c in ["E00100", "MARS"] + as_agi.ITEMS + as_agi.CAPGAIN + as_agi.ADJ:
            assert not r.loc[puf, c].isna().any(), (p, c)
        rem = np.full(len(r), np.nan)
        rem[puf] = r.loc[puf, "E00100"].to_numpy(float) - as_agi.items_less_adjustments(r.loc[puf])
        rem[puf] = np.round(rem[puf], 2)
        r[as_agi.REM] = rem
    # meta: the new variable takes AGI's column and place in the order
    nv = {kk: vv for kk, vv in v.items() if kk not in ("name", "description")}
    nv["name"] = as_agi.REM
    nv["description"] = ("AGI less the income items in the model plus the adjustments in the model "
                         "(other income less other adjustments); see probes/as_agi.py")
    meta["variables"][k] = nv
    meta["variable_names"][k] = as_agi.REM
    meta["generation_order"] = [as_agi.REM if n == "E00100" else n for n in meta["generation_order"]]
    meta["dataset"] = "full_v2s"
    meta["description"] = "Probe AS (test 4): full_v2 with AGI replaced by AGI_REMAINDER. " + meta["description"]
    meta["as_agi_identity"] = {"items": as_agi.ITEMS, "capital_line13": "E01100 + max(P22250 + P23250, -3000; -1500 if MARS 3)",
                               "adjustments": as_agi.ADJ}
    # quantiles: drop AGI, add the remainder (PUF 2015 training records, by side)
    q = pd.read_parquet(os.path.join(SRC, "quantiles.parquet"))
    q = q[q.variable != "E00100"]
    tr = rec["train"]
    tr = tr[tr.source.astype(str) == "PUF"]
    x, w = tr[as_agi.REM].to_numpy(float), tr.weight.to_numpy(float)
    rows = []
    for side, m in (("pos", x > 0), ("neg", x < 0)):
        t = tk.quantile_table(x[m], w[m], side)
        t.insert(0, "side", side); t.insert(0, "variable", as_agi.REM)
        t.insert(0, "year", 2015); t.insert(0, "source", "PUF")
        t["weighted"] = True
        rows.append(t)
    q = pd.concat([q] + rows, ignore_index=True)
    q.to_parquet(os.path.join(OUT, "quantiles.parquet"), index=False)
    q = pd.read_parquet(os.path.join(OUT, "quantiles.parquet"))
    cache = tk._qf_cache(q)
    mini = {"variables": [nv]}
    for i, p in enumerate(PARTS):
        r = rec[p]
        tok = np.load(os.path.join(SRC, f"tokens_{p}.npy"))
        tru = np.load(os.path.join(SRC, f"truth_{p}.npy"))
        assert (tok == tru).all()
        col = tk.encode(r[["source", "year", as_agi.REM]], mini, q, np.random.default_rng(SEED + i), cache=cache)[:, 0]
        assert ((col < 0) == r[as_agi.REM].isna().to_numpy()).all()
        tok = tok.copy()
        tok[:, k] = col.astype(tok.dtype)
        np.save(os.path.join(OUT, f"tokens_{p}.npy"), tok)
        np.save(os.path.join(OUT, f"truth_{p}.npy"), tok)
        r.to_parquet(os.path.join(OUT, f"records_{p}.parquet"), index=False)
        print(p, tok.shape, "remainder tokens observed", int((col >= 0).sum()), flush=True)
    json.dump(meta, open(os.path.join(OUT, "meta.json"), "w"), indent=1)
    shutil.copy(os.path.join(SRC, "DONE"), os.path.join(OUT, "DONE.src"))
    with open(os.path.join(OUT, "DONE"), "w") as f:
        f.write("full_v2s built by probes/as_prepare.py from " + SRC + "\n")
    print("done", flush=True)


if __name__ == "__main__":
    main()
