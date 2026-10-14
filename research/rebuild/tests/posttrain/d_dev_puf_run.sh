#!/bin/bash
# Piece D: code test of the PUF-arm path on fabricated development data
# (pilot model). Run through d_gpu_sh.sbatch.
set -euo pipefail
DV=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/d_puf
[ -f $DV/weights.npz ] || python -u d_dev_puf.py
C="--arm puf --records $DV/records.parquet --targets $DV/targets.parquet --sample-batch 8192"
bash d_train.sh $DV/distill --variant distill $C --draws $DV/draws.npz --weights $DV/weights.npz \
  --minutes 3 --eval-min 1 --eval-k 2 --patience 10
DK=8 BREP=$DV/calib.json bash d_finish.sh $DV/distill $DV/DONE_distill $C
bash d_train.sh $DV/direct --variant direct $C --beta 0.1 --lr 3e-5 --mix 0.2 --rl-batch 512 \
  --minutes 4 --eval-min 1 --eval-k 2 --patience 10
DK=8 bash d_finish.sh $DV/direct $DV/DONE_direct $C
