#!/bin/bash
# Generate extra files from one checkpoint with several seeds (noise check).
# Usage: sbatch gen_seeds.sbatch <tag> <checkpoint> <seed> [<seed> ...]
#SBATCH --job-name=pg_gen_seeds
#SBATCH --partition=gpu,gpu_rtx6000,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/genseeds_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/genseeds_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
S=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
tag=$1; ckpt=$2; shift 2
cd "$CODE"
nvidia-smi --query-gpu=name --format=csv,noheader
for seed in "$@"; do
  python -u generate.py --data-dir "$S/data" --out-dir "$S/model" \
      --checkpoint "$ckpt" --out-name "synthetic_${tag}_s${seed}" --seed "$seed"
done
