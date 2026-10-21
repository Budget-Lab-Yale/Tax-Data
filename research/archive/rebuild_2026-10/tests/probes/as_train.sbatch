#!/bin/bash
# Probe AS: train the joint model on full_v2s (AGI replaced by its remainder),
# with test 3's version 2 recipe (h_train.sbatch settings, unchanged h_train.py).
# Usage: sbatch as_train.sbatch <seed>
#SBATCH --job-name=as_train
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/as_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/as_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
SEED=$1
NN=$(printf %02d "$SEED")
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/model
mkdir -p "$OUT"
test -f "$DATA/DONE"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) seed $SEED data $DATA out $OUT"
python -u h_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "full_v2s_seed$NN" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours 2.6
echo "$(date) job ${SLURM_JOB_ID:-}" > "$OUT/DONE_seed$NN"
echo "$(date) training done"
