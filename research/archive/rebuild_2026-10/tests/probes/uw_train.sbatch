#!/bin/bash
# Probe UW: train one arm on `full_v2w` (uw_train.py): PUF records unweighted,
# the PUF sampling weight a model variable, SCF records survey-weighted with
# the SCF's share of training signal as in version 2.
# Usage: sbatch uw_train.sbatch <arm> <seed> [extra uw_train.py args ...]
#SBATCH --job-name=uw_train
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:40:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/full_v2w
test -f "$DATA/DONE"
VAR=$1; SEED=$2
NN=$(printf %02d "$SEED")
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/$VAR
mkdir -p "$OUT"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) arm $VAR seed $SEED extra: ${*:3}"
python -u uw_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "${VAR}_seed$NN" \
    --seed "$SEED" --loss-weight uw --max-hours 1.25 "${@:3}"
echo "$(date) done"
