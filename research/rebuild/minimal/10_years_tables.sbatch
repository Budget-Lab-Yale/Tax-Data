#!/bin/bash
#SBATCH --job-name=min10tab
#SBATCH --partition=day
#SBATCH --time=00:45:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/10_tables_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/10_tables_%j.err
# Set B of 10_TRANSLATION_EXPERIMENTS.md: the four three-year tables Y0-Y3
# (10_years_tables.py).
# Usage: sbatch research/rebuild/minimal/10_years_tables.sbatch Y0 Y1 Y2 Y3
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/10_years_tables.py "$@"
