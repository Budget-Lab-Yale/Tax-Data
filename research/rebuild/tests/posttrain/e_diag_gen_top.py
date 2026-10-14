"""Piece E diagnostic: is the missing top of generated PUF records in the
tokens (the model) or in the decoding to dollars? Aggregates only."""
import json, os, sys
import numpy as np, pandas as pd
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "scf_joint"))
from c_common import token_info
ver = sys.argv[1] if len(sys.argv) > 1 else "full"
T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
D = f"{T2}/data/{ver}/"
m = json.load(open(D + "meta.json")); vm = {v["name"]: v for v in m["variables"]}
names = [v["name"] for v in m["variables"]]
rec = pd.read_parquet(D + "records_train.parquet"); tr = np.load(D + "truth_train.npy")
puf = (rec["source"] == "PUF").to_numpy()
for seed in ("seed01", "seed03", "seed02"):
    f = f"{T2}/model/{ver}/h_gen_puf_{seed}.parquet"
    if not os.path.exists(f):
        continue
    G = pd.read_parquet(f)
    print("==", ver, seed, len(G))
    for v in ["E00100", "E00200", "E26270", "P23250"]:
        ti = token_info(vm[v]); sc = ti["score"]
        tt = tr[puf, names.index(v)]; wt = rec["weight"].to_numpy()[puf]
        tg = G["tok_" + v].to_numpy(); wg = G["weight"].to_numpy()
        def sh(t, w, lo):
            s = sc[np.maximum(t, 0)]; return float((w * ((t >= 0) & (s > lo))).sum() / w.sum())
        dt = rec[v].to_numpy()[puf]; dg = G[v].to_numpy()
        top_t = (tt >= 0) & (sc[np.maximum(tt, 0)] > 0.999); top_g = (tg >= 0) & (sc[np.maximum(tg, 0)] > 0.999)
        print(f"{v}: share in positive rank band >.99: truth {sh(tt, wt, .99):.5f} gen {sh(tg, wg, .99):.5f}; "
              f">.999: truth {sh(tt, wt, .999):.6f} gen {sh(tg, wg, .999):.6f}; "
              f"mean $ in top 0.1% band: truth {np.average(dt[top_t], weights=wt[top_t]) if top_t.any() else float('nan'):,.0f} "
              f"gen {np.average(dg[top_g], weights=wg[top_g]) if top_g.any() else float('nan'):,.0f}; "
              f"positive share truth {float((wt * (dt > 0)).sum() / wt.sum()):.4f} gen {float((wg * (dg > 0)).sum() / wg.sum()):.4f}")
