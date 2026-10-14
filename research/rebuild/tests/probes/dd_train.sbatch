#!/bin/bash
# Probe DD: train the joint model with test 3's version 2 recipe (h_train.sbatch
# settings, unchanged h_train.py), as probe C's c3_train.sbatch.
# Usage: sbatch dd_train.sbatch <arm: dd | ddp> <seed>
#SBATCH --job-name=dd_train
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
ARM=$1; SEED=$2
NN=$(printf %02d "$SEED")
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd
DATA=$Q/data/full_v4$ARM
OUT=$Q/model_$ARM
mkdir -p "$OUT"
test -f "$DATA/DONE"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) arm $ARM seed $SEED data $DATA out $OUT"
python -u h_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "full_v4${ARM}_seed$NN" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours 2.6
echo "$(date) job ${SLURM_JOB_ID:-}" > "$OUT/DONE_seed$NN"
echo "$(date) training done"
