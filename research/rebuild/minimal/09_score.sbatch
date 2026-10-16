#!/bin/bash
#SBATCH --job-name=min09score
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/09_score_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/09_score_%x_%j.err
# Minimal example, step 9: the scoreboard for one run of the bridge grid.
# Usage: sbatch -J score_E0 research/rebuild/minimal/09_score.sbatch E0
#        sbatch -J score_E12 research/rebuild/minimal/09_score.sbatch E12 <run> <puf|scf>
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/09_score.py "$@"
