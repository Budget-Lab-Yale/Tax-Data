#!/bin/bash
# Probe O: generate whole PUF records from one of test 3's checkpoints in one order.
# Usage: sbatch o_gen.sbatch <model seed 1-3> <order name (o_orders.py)> <sampling seed> [n]
#SBATCH --job-name=o_gen
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/o_gen_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/o_gen_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data
MS=$1; NAME=$2; SS=$3; N=${4:-664624}
CK=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/checkpoint_full_v2_w1_seed0${MS}.pt
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/o/gen
mkdir -p "$OUT"
ORDER=$(python o_orders.py "$NAME")
MODE=fixed
case "$NAME" in random|entropy_low|entropy_high) MODE=$NAME;; esac
echo "model seed $MS order $NAME mode $MODE sampling seed $SS n $N"
python -u o_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" --tag "${NAME}_s${SS}" \
  --train-seed "$MS" --mode generate --fill puf --gen-source PUF --gen-year 2015 --draws 1 \
  --n "$N" --seed $((20261010 + 1000 * MS + SS)) --order "$ORDER" --order-mode "$MODE" \
  --tokenizer $PT/g_tokenizer.py
echo "$(date) done"
