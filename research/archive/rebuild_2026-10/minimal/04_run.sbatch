#!/bin/bash
#SBATCH --job-name=min04
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/04_run_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/04_run_%j.err
# Minimal example, step 4: runs research/rebuild/minimal/04_dollars.py.
# Usage: sbatch research/rebuild/minimal/04_run.sbatch
# Waits for step 1's marker so the numbers come from the finished table.
# Set GEN_FILE=<path> to choose the generated file when gen/ holds several.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
while [ ! -f /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/STEP1_DONE ]; do sleep 60; done
# Optionally also wait for another marker, e.g. WAIT_FOR=<gen dir>/GEN_DONE_v2.
if [ -n "${WAIT_FOR:-}" ]; then while [ ! -f "$WAIT_FOR" ]; do sleep 60; done; fi
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/04_dollars.py
