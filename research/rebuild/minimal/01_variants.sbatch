#!/bin/bash
#SBATCH --job-name=min01var
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/01_variants_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/01_variants_%j.err
# Variant tables of the bridge-variable grid (01_variants.py), E1-E6.
# Usage: sbatch research/rebuild/minimal/01_variants.sbatch E1 E2 E4
#        (E3_LINES='{"interest": 500}' sbatch --export=ALL ... E3 to set E3's lines by hand)
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/01_variants.py "$@"
