#!/bin/bash
#SBATCH --job-name=pufgen_prep
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=24G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/prepare_data_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/prepare_data_%j.err
# Part A of the PUF generator test (see SPEC.md). Submit from anywhere:
#   sbatch research/rebuild/tests/puf_generator/prepare_data.sbatch
set -euo pipefail
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator/prepare_data.py
