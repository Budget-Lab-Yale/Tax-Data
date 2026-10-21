#!/bin/bash
# Train the masked record transformer on a GPU.
# Usage: sbatch train.sbatch --tag base --d-model 256 --layers 6 --heads 8
# Any arguments are passed to train.py. DATA_DIR / OUT_DIR may be overridden
# in the environment (used for the toy check).
#SBATCH --job-name=pufgen_train
#SBATCH --partition=gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
S=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
DATA_DIR=${DATA_DIR:-$S/data}
OUT_DIR=${OUT_DIR:-$S/model}
nvidia-smi --query-gpu=name,memory.total --format=csv
cd "$CODE"
python -u train.py --data-dir "$DATA_DIR" --out-dir "$OUT_DIR" "$@"
