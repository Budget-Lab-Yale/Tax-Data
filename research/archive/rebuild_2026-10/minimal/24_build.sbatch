#!/bin/bash -l
#SBATCH --job-name=min24b
#SBATCH --partition=day,week
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=24G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/24_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/24_%x_%j.err
# Step 24 (overnight variants of the fake-survey test, 24_OVERNIGHT.md): build
# one variant's tables. The variant is set by the MINIMAL_F22_* variables
# passed with --export (see 22_fake_survey.py).
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
mkdir -p "$MINIMAL_F22_OUT"
python research/rebuild/minimal/22_fake_survey.py build
