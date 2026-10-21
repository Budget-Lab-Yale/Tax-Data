#!/bin/bash
# Run one diag2_ CPU script.  Usage: sbatch diag2_cpu.sbatch <script.py> [args]
#SBATCH --job-name=pg_diag2c
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=00:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/diag2c_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/diag2c_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
python -u "$@"
