#!/bin/bash
#SBATCH --job-name=b_overfit
#SBATCH --partition=gpu_rtx6000,gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=00:15:00
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint/logs/b_overfit_%j.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint/logs/b_overfit_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
python -u b_overfit_probe.py \
  --data-dir /nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/expB \
  --out logs/b_overfit_result.json
