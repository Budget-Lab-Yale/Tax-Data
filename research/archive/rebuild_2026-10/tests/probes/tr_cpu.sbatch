#!/bin/bash
# Probe TR: CPU step running a bash script of steps. Usage: sbatch tr_cpu.sbatch <script.sh> [args]
#SBATCH --job-name=tr_cpu
#SBATCH --partition=day,scavenge
#SBATCH --requeue
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/tr_cpu_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/tr_cpu_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
bash "$@"
echo "$(date) done"
