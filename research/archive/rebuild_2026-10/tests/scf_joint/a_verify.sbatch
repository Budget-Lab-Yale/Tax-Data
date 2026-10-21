#!/bin/bash
#SBATCH --job-name=a_verify
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/a_verify_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/a_verify_%j.err
set -euo pipefail
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint/a_verify.py "$@"
