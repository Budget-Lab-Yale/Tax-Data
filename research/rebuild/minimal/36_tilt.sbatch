#!/bin/bash -l
#SBATCH --job-name=s36tilt
#SBATCH --partition=day,week
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step36/tilt_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step36/tilt_%x_%j.err
# Step 36 (36_tilt.py): one step-34 stack tilted to 2019 with SOI business income
# targets added, at three levels of detail; needs 36_business_targets.py run first.
#   sbatch -J t36Ns1 research/rebuild/minimal/36_tilt.sbatch Ns1
# Output: tilt/step36/s34_<run>/.
set -euo pipefail
RUN=${1:?run name}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
source research/rebuild/minimal/34_env.sh
export MINIMAL_RUN=step34/$RUN STACK_VERSION=s34_$(echo $RUN | tr A-Z a-z)
export WEALTH_UNITS=all WEALTH_COUNT_BASE=scf_units MINIMAL_NONFILERS=1
export OMP_NUM_THREADS=4 OPENBLAS_NUM_THREADS=4 MKL_NUM_THREADS=4
python research/rebuild/minimal/36_tilt.py
echo "=== done $(date)"
