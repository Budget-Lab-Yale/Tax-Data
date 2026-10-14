#!/bin/bash
# Probe DD: CPU step run on a GPU node (the CPU partitions are full). Usage: sbatch dd_cpug.sbatch <python args...>
#SBATCH --job-name=dd_cpug
#SBATCH --partition=gpu,gpu_h100,gpu_h200,gpu_b200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_cpug_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_cpug_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
python -u "$@"
echo "$(date) done"
