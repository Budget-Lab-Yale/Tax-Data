#!/bin/bash
#SBATCH --job-name=min16zb
#SBATCH --partition=day
#SBATCH --time=00:20:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/16_zero_band_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/16_zero_band_%j.err
# Step 16: the SCF zero band split by the E9 reporting curve (16_zero_band.py).
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
python research/rebuild/minimal/16_zero_band.py
