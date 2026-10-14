#!/bin/bash
# Piece H: re-check h_complete.py after the parquet-assembly change (expB, real tokenizer).
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
E=$P/dev/h_expB
CK=$T2/model/expB/checkpoint_joint_w1_seed01.pt
python -u h_complete.py --data-dir $T2/data/expB --checkpoint $CK --out-dir $E --train-seed 1 \
  --tag scf_test --mode complete --fill wealth --split test --target-sources SCF \
  --draws 2 --max-records 3000
python -u h_complete.py --data-dir $T2/data/expB --checkpoint $CK --out-dir $E --train-seed 1 \
  --tag gen_puf --mode generate --fill puf --gen-source PUF --gen-year 2015 --draws 1 \
  --max-records 3000
python -u h_dev_check.py
python -u h_report.py --model-dir $P/dev/h_model_fake --tag fake_w1
echo "dev test 2 done"
