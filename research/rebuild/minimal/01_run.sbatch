#!/bin/bash
#SBATCH --job-name=min01
#SBATCH --partition=day
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/01_run_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/01_run_%j.err
# Minimal example, step 1: build the stacked PUF + SCF table (R), then band
# and split it (Python). Usage: sbatch research/rebuild/minimal/01_run.sbatch
# Pass "band" as the first argument to rerun only the Python part.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
if [ "${1:-all}" != "band" ]; then
  module load R/4.4.1-foss-2022b
  Rscript research/rebuild/minimal/01_build_table.R
  module purge
fi
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/01_band.py
