#!/bin/bash
# Piece D: code test of the full-model layout (a_full target sets, PUF arm)
# on piece A's pilot ($P/full/a/pilot_expB, pilot model). Not a result.
set -euo pipefail
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
DV=$P/dev/d_full
mkdir -p $DV
C="--dataset expB --arm puf --a-dir $P/full/a/pilot_expB --target-set wcore"
python -u d_tune.py draws --out-dir $DV/ref --checkpoint /nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/expB/checkpoint_joint_w1_seed01.pt --k 4 $C
python -u - <<'PY'
import numpy as np, pandas as pd
P = "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain"
z = np.load(f"{P}/dev/d_full/ref/draws.npz")
W = pd.read_parquet(f"{P}/full/a/pilot_expB/records_puf.parquet")["weight"].to_numpy(float)
K = z["tokens"].shape[1]
rng = np.random.default_rng(0)
w = W[:, None] * rng.dirichlet(np.ones(K), size=len(W))
np.savez(f"{P}/dev/d_full/weights_random.npz", w=w)
print("fabricated weights", w.shape)
PY
bash d_train.sh $DV/distill --variant distill $C --draws $DV/ref/draws.npz --weights $DV/weights_random.npz \
  --minutes 2 --eval-min 1 --eval-k 1 --patience 10
DK=4 bash d_finish.sh $DV/distill $DV/DONE_distill $C
bash d_train.sh $DV/direct --variant direct $C --beta 0.1 --lr 1e-4 --mix 0 --rl-batch 1024 \
  --minutes 5 --eval-min 2 --eval-k 1 --patience 10
DK=4 bash d_finish.sh $DV/direct $DV/DONE_direct $C
