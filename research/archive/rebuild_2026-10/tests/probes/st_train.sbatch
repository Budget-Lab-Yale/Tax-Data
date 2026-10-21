#!/bin/bash
# Probe ST: train the joint model (test 3's recipe, h_train.sbatch) on the
# version 2 data without the SCF 2016 wave. Usage: sbatch st_train.sbatch <seed>
#SBATCH --job-name=st_train
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/st_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/st_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
ST=/nfs/roberts/scratch/pi_nrs36/jar335/probes/st
SEED=$1
NN=$(printf %02d "$SEED")
DATA=$ST/data_no2016
OUT=$ST/model
mkdir -p "$OUT"
test -f "$DATA/DONE"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
python -u h_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "no2016_seed$NN" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours 2.5
echo "$(date) job ${SLURM_JOB_ID:-}" > "$OUT/DONE_seed$NN"
