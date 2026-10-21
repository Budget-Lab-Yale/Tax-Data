#!/bin/bash
# Piece H code check (run inside a GPU job: sbatch h_gpu.sbatch is for Python,
# so this is run with: sbatch --wrap via h_dev_test.sbatch). Fabricated `full`
# layout: train 3 epochs, then all four completion modes without decoding.
# Then test 2's expB checkpoint with the real tokenizer: decoding and the
# allowed-code tables on a few thousand records.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
D=$P/dev/h_full_fake
M=$P/dev/h_model_fake
rm -rf "$D" "$M"
python -u h_make_fake.py --out "$D"
python -u h_train.py --data-dir "$D" --out-dir "$M" --tag fake_w1_seed01 --seed 1 \
   --drop-weight-vars --loss-weight survey --max-epochs 3
DATA=$D OUT=$M TAGBASE=fake_w1 EXTRA="--no-decode" bash h_complete.sbatch 1
ls -la "$M"
# expB: real tokenizer, decode and allowed codes.
E=$P/dev/h_expB
mkdir -p "$E"
CK=$T2/model/expB/checkpoint_joint_w1_seed01.pt
python -u h_complete.py --data-dir $T2/data/expB --checkpoint $CK --out-dir $E --train-seed 1 \
  --tag puf_wealth --mode complete --fill wealth --split train --target-sources PUF \
  --as-source SCF --decode-year 2015 --draws 2 --max-records 3000 --no-score
python -u h_complete.py --data-dir $T2/data/expB --checkpoint $CK --out-dir $E --train-seed 1 \
  --tag scf_test --mode complete --fill wealth --split test --target-sources SCF \
  --draws 2 --max-records 3000
python -u h_complete.py --data-dir $T2/data/expB --checkpoint $CK --out-dir $E --train-seed 1 \
  --tag gen_puf --mode generate --fill puf --gen-source PUF --gen-year 2015 --draws 1 \
  --max-records 3000
python -u h_dev_check.py
echo "dev test done"
