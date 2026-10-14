#!/bin/bash
# Probe S: train one variant of the joint model on `full_v2` (s_train.py).
# Usage: sbatch s_train.sbatch <variant> <seed> [extra s_train.py args ...]
# Settings otherwise as t_train.sbatch (probe T) and ../posttrain/h_train.sbatch.
#SBATCH --job-name=s_train
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data
VAR=$1; SEED=$2
NN=$(printf %02d "$SEED")
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/s/$VAR
mkdir -p "$OUT"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) variant $VAR seed $SEED extra: ${*:3}"
python -u s_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "${VAR}_seed$NN" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours 1.6 \
    --max-epochs 60 --lr-epochs 60 "${@:3}"
echo "$(date) done"
