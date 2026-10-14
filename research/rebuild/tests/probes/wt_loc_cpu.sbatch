#!/bin/bash
# Probe WT (locate step): CPU step. Usage: sbatch wt_loc_cpu.sbatch <python args...>
#SBATCH --job-name=wt_loc
#SBATCH --partition=devel,day
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=00:45:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_loc_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_loc_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
python -u "$@"
echo "$(date) done"
