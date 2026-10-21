#!/bin/bash -l
#SBATCH --job-name=s36cmp
#SBATCH --partition=day,week
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/compare_shared/cpu_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/compare_shared/cpu_%x_%j.err
# Shared variables, SCF against PUF (36_compare_shared.py).
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export MINIMAL_KG_PIECES=1 MINIMAL_MAP_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/V27
python research/rebuild/minimal/36_compare_shared.py
echo "=== done $(date)"
