#!/bin/bash
# Probe BR: train the joint model on full_v3br, one seed. Copy of
# ../posttrain/h_train.sbatch with probe BR's paths; runs ../posttrain/h_train.py
# unchanged, with test 3's settings. Usage: sbatch br_train.sbatch <seed>
# Env: DATA (default $Q/data/full_v3br), OUT (default $Q/model/full_v3br), TAGBASE (default full_v3br_w1)
#SBATCH --job-name=br_train
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/br_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/br_train_%j.err
set -euo pipefail
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/br
SEED=$1
NN=$(printf %02d "$SEED")
DATA=${DATA:-$Q/data/full_v3br}
OUT=${OUT:-$Q/model/full_v3br}
TAGBASE=${TAGBASE:-full_v3br_w1}
mkdir -p "$OUT"
test -f "$DATA/DONE"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) seed $SEED data $DATA out $OUT"
python -u h_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "${TAGBASE}_seed$NN" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours 2.5 "${@:2}"
echo "$(date) job ${SLURM_JOB_ID:-}" > "$OUT/DONE_seed$NN"
echo "$(date) training done"
