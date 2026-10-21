#!/bin/bash
# Probe Y: run one Python command from this folder on a CPU node.
# Usage: sbatch y_py.sbatch <script.py> [args ...]
#SBATCH --job-name=y_py
#SBATCH --partition=day,scavenge
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/y_py_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/y_py_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd /nfs/roberts/scratch/pi_nrs36/jar335/probes/y
script=$1; shift
python -u "$CODE/$script" "$@"
