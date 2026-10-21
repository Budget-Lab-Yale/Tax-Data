#!/bin/bash
# Probe WT: train on full_v3cwt (probe C's full_v3c plus SCFWT, the SCF weight
# as a model variable) with wt_train.py --loss-weight wt: PUF records keep
# their population weight, SCF rows count equally, the SCF's share of the
# training signal as in version 2. Settings otherwise as probe C's
# c3_uwtrain.sbatch (uw_train.py defaults: test 3's recipe, early stopping).
# Usage: sbatch wt_train.sbatch <seed>   (env LW=wtsqrt TAGP=wtsq for the square-root arm)
#SBATCH --job-name=wt_train
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:40:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt
DATA=$Q/data/full_v3cwt
test -f "$DATA/DONE"
SEED=$1
NN=$(printf %02d "$SEED")
OUT=$Q/model
mkdir -p "$OUT"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) seed $SEED"
python -u wt_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "${TAGP:-wt}_seed$NN" \
    --seed "$SEED" --loss-weight ${LW:-wt} --max-hours 1.25
echo "$(date) job ${SLURM_JOB_ID:-}" > "$OUT/DONE_${TAGP:-wt}_seed$NN"
echo "$(date) done"
