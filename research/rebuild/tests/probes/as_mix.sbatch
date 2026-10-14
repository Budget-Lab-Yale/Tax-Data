#!/bin/bash
# Probe AS: S's recipe on full_v2s: 25% of each batch on the questions
# generation asks, in the fixed "sizefirst" order (as_orders.py), 60 epochs
# with decaying learning rate (s_train.sbatch settings; as_train_mix.py is an
# unchanged copy of s_train.py).
# Usage: sbatch as_mix.sbatch <seed>
#SBATCH --job-name=as_mix
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/as_mix_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/as_mix_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s
SEED=$1
NN=$(printf %02d "$SEED")
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/model_mix25
mkdir -p "$OUT"
# the size-first order without the demographics (they follow in s_train's own order)
FIRST=$(python as_orders.py sizefirst | tr ',' '\n' | grep -v -x -e AGERANGE -e GENDER -e EARNSPLIT | paste -sd,)
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) seed $SEED ordered-first $FIRST"
python -u as_train_mix.py --data-dir "$DATA" --out-dir "$OUT" --tag "mix25_seed$NN" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours 1.6 \
    --max-epochs 60 --lr-epochs 60 --mix-ordered 0.25 --mix-style ordered --ordered-first "$FIRST"
echo "$(date) done"
