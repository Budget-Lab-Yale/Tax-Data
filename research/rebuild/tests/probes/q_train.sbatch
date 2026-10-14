#!/bin/bash
# Probe Q: retrain the version 2 model (full_v2, seed 1 settings) and save
# checkpoints at several epochs (q_train.py --save-epochs), for a ladder of
# pretrained models of different quality.
#SBATCH --job-name=q_train
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/q_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/q_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data
OUT=${OUT:-/nfs/roberts/scratch/pi_nrs36/jar335/probes/q/model}
mkdir -p "$OUT"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) data $DATA out $OUT"
python -u q_train.py --data-dir "$DATA" --out-dir "$OUT" --tag q_full_v2_seed01 \
    --seed 1 --drop-weight-vars --loss-weight survey --max-hours ${MAXH:-1.6} \
    --save-epochs ${SAVE:-1,2,4,8,12,20,30}
echo "$(date) done" > "$OUT/DONE_train"
