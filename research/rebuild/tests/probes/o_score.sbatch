#!/bin/bash
# Probe O: chain-rule log-probability of held-out PUF records in each order.
# Usage: sbatch o_score.sbatch <model seed 1-3> [split, default val] [extra args]
#SBATCH --job-name=o_score
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/o_score_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/o_score_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data
MS=$1; SPLIT=${2:-val}
CK=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/checkpoint_full_v2_w1_seed0${MS}.pt
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/o/score
mkdir -p "$OUT"
SPEC=$OUT/spec.json
python o_orders.py --spec "$SPEC"
python -u o_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" --tag "score_$SPLIT" \
  --train-seed "$MS" --mode score --fill puf --split "$SPLIT" --target-sources PUF \
  --order "$(python o_orders.py test3)" --score-spec "$SPEC" --seed $((20261020 + MS)) "${@:3}"
echo "$(date) done"
