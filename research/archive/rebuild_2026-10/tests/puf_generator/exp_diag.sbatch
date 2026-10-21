#!/bin/bash
# Run exp_diag.py (night-of-2026-10-08 experiments). Arguments pass through.
#SBATCH --job-name=pg_expdiag
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=00:20:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/expdiag_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/expdiag_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
python -u exp_diag.py "$@"
