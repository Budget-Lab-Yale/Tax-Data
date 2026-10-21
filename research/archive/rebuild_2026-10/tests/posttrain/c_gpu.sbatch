#!/bin/bash
# Piece C: run one Python script from this folder on a GPU.
# Usage: sbatch c_gpu.sbatch <script.py> [args ...]
#SBATCH --job-name=c_gpu
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/c_gpu_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/c_gpu_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "args: $*"
python -u "$@"
