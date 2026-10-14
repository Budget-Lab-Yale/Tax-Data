#!/bin/bash
# Piece D development trial on $P/dev/d (run through d_gpu.sbatch: sbatch d_gpu.sbatch-like wrapper).
set -euo pipefail
DV=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/d
[ -f $DV/weights.npz ] || python -u d_dev.py --n 3000 --k 8
COMMON="--records $DV/records.parquet --targets $DV/targets.parquet --sample-batch 8192"
python -u d_tune.py train --variant distill --out-dir $DV/distill $COMMON \
  --draws $DV/draws.npz --weights $DV/weights.npz --minutes 2 --eval-min 1 --eval-k 2 --patience 10
python -u d_tune.py draws --out-dir $DV/distill $COMMON --k 8 --check-n 1000
python -u b_calibrate.py --draws $DV/distill/draws.npz --records $DV/records.parquet \
  --targets $DV/targets.parquet --tag resid --outdir $DV/distill
python -u d_report.py --out-dir $DV/distill --b-report $DV/calib.json
python -u d_tune.py train --variant direct --out-dir $DV/direct $COMMON --beta 1.0 \
  --minutes 4 --eval-min 1 --eval-k 2 --patience 10 --mix 0 --rl-batch 512 --refresh-steps 20
