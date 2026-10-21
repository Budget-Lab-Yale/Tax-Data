"""Piece F: is the low-income skew of the S0/S1 draws real or a decoding
artefact? Decode the real records' own income tokens the way draws are
decoded and compare income groups; compare zero shares of each income."""
import os, sys
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import f_common as F
import f_run
from f_common import A, INCOMES
recs = A.load_records()
W = recs["weight"].to_numpy()
tok = np.stack([recs["tok_" + v].to_numpy() for v in INCOMES], 1)
dol = F.decode(tok, INCOMES, recs["wave_i"].to_numpy(), 5).to_numpy()
g = F.income_group(dol.sum(1), recs["wave_i"].to_numpy(), F.income_thresholds())
print("real records: income group from decoded tokens agrees with A's on", round(float((g == recs["g_inc"]).mean()), 4))
print("weight share by group, A's:", np.round(np.bincount(recs["g_inc"], weights=W) / W.sum(), 4))
print("weight share by group, decoded tokens:", np.round(np.bincount(g, weights=W) / W.sum(), 4))
z = np.load(os.path.join(F.PF, "s0", "draws.npz"))
names = list(z["vars"])
K = z["tokens"].shape[1]
zt = {v: A.vmeta(v)["tokens"] for v in INCOMES}
for j, v in enumerate(INCOMES):
    zero = [i for i, t in enumerate(zt[v]) if t["kind"] == "zero"][0]
    real = float(W[tok[:, j] == zero].sum() / W.sum())
    s0 = float((np.repeat(W / K, K)[z["tokens"][:, :, names.index(v)].reshape(-1) == zero]).sum() / W.sum())
    s1 = f_run.load_s1(8)
    s1z = float((s1["tokens"][:, 3 + j] == zero).mean())
    print(f"{v}: share zero, real {real:.3f}  S0 draws {s0:.3f}  S1 draws {s1z:.3f}")
