#!/bin/bash
# Probe AS: held-out log-probability of PUF validation records in several orders
# (as_complete.py --mode score, as probe O's o_score.sbatch).
# Usage: sbatch as_score.sbatch <model seed>
#SBATCH --job-name=as_score
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/as_score_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/as_score_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s
MS=$1
CK=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/model/checkpoint_full_v2s_seed0${MS}.pt
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/score
mkdir -p "$OUT"
SPEC=$OUT/spec.json
python as_orders.py --spec "$SPEC"
python -u as_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" --tag "score_val" \
  --train-seed "$MS" --mode score --fill puf --split val --target-sources PUF \
  --order "$(python as_orders.py sizefirst)" --score-spec "$SPEC" --seed $((20261020 + MS))
echo "$(date) done"
