#!/bin/bash -l
#SBATCH --job-name=min28
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/28_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/28_%x_%j.err
# Step 28 (28_WAGES_CG.md): wages and capital gains, SCF 2016 wave against
# the 2015 PUF with and without the crude non-filers. Writes scratch
# checks/step28/wages_cg.md.
#   sbatch research/rebuild/minimal/28_run.sbatch
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/28_wages_cg.py
echo "=== done $(date)"
