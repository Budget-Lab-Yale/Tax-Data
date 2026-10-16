#!/bin/bash
#SBATCH --job-name=min01crs
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/01_coarsen_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/01_coarsen_%j.err
# Coarsened zeros: dollar lines and <var>_coarse_hi columns (01_coarsen.py).
# Usage: sbatch research/rebuild/minimal/01_coarsen.sbatch
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/01_coarsen.py
