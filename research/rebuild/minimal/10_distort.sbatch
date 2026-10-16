#!/bin/bash
#SBATCH --job-name=min10dist
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/10_distort_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/10_distort_%j.err
# Set A tables of the translation experiments (10_distort.py): one distortion
# of the SCF's shared amounts, banded under R0, R1 and R2.
# Usage: sbatch research/rebuild/minimal/10_distort.sbatch D1 [D2 ...]
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/10_distort.py "$@"
