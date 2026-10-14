"""Piece E development only: fabricated files in piece H's layout under
$P/dev/e/full_model/ (true SCF wealth with some records' wealth swapped; PUF
records given random SCF wealth vectors; a resample of PUF training records
as 'generated' records). Not part of the evaluation."""
import json
import os
import numpy as np
import pandas as pd
from c_common import WEALTH  # noqa
T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
D = T2 + "/data/full/"
OUT = "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e/full_model"
os.makedirs(OUT, exist_ok=True)
m = json.load(open(D + "meta.json"))
names = [v["name"] for v in m["variables"]]
rng = np.random.default_rng(3)
rec = pd.read_parquet(D + "records_train.parquet")
tr = np.load(D + "truth_train.npy")
col = [names.index(v) for v in WEALTH]
scf = np.flatnonzero((rec["source"] == "SCF").to_numpy() & rec["wealth_observed"].fillna(False).to_numpy())
puf = np.flatnonzero((rec["source"] == "PUF").to_numpy())
def frame(rows, donors, K):
    parts = []
    for k in range(K):
        d = donors[:, k]
        F = pd.DataFrame({"row": rows, "draw": k})
        for j, v in enumerate(WEALTH):
            F["tok_" + v] = tr[d, col[j]].astype(np.int16)
            F[v] = rec[v].to_numpy()[d]
        parts.append(F)
    return pd.concat(parts, ignore_index=True)
K = 4
don = np.repeat(scf[:, None], K, 1)
sw = rng.random(don.shape) < 0.2
for k in range(K):
    don[sw[:, k], k] = rng.choice(scf, sw[:, k].sum())
for split in ["train"]:
    frame(scf, don, K).to_parquet(f"{OUT}/h_scf_train_seed01.parquet")
    json.dump({"fake": True}, open(f"{OUT}/h_scf_train_seed01_info.json", "w"))
p = rec["weight"].to_numpy()[scf]; p = p / p.sum()
donp = rng.choice(scf, (len(puf), K), p=p)
frame(puf, donp, K).to_parquet(f"{OUT}/h_puf_wealth_seed01.parquet")
json.dump({"fake": True}, open(f"{OUT}/h_puf_wealth_seed01_info.json", "w"))
V = pd.read_parquet("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data/values_train.parquet")
G = V.sample(len(V), replace=True, random_state=1).reset_index(drop=True)
G.to_parquet(f"{OUT}/h_gen_puf_seed01.parquet")
json.dump({"fake": True}, open(f"{OUT}/h_gen_puf_seed01_info.json", "w"))
print("fake H files written")
