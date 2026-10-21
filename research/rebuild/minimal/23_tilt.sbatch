#!/bin/bash -l
#SBATCH --job-name=min23t
#SBATCH --partition=day,week
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/23_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/23_%x_%j.err
# Step 23: the tilt alone, on a CPU node (23_run.sbatch runs it after the stack).
#   sbatch --export=NONE research/rebuild/minimal/23_tilt.sbatch N1
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?N1 or N2}; V=$(echo $RUN | tr A-Z a-z)
# optional second argument: the wealth count base (default scf_units); the
# output folder is then tilt/<run>_<base>
BASE=${2:-scf_units}; TAG=$V; [ "$BASE" != scf_units ] && TAG=${V}_$BASE
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export MINIMAL_TABLE=E4N MINIMAL_RUN=$RUN MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4 STACK_VERSION=$V
# optional third argument: BLAS threads (default 2); the folder gets _t<n> when set
TH=${3:-2}; [ -n "${3:-}" ] && TAG=${TAG}_t$TH
export OMP_NUM_THREADS=$TH OPENBLAS_NUM_THREADS=$TH MKL_NUM_THREADS=$TH
export TILT_TAG=$TAG WEALTH_UNITS=all WEALTH_COUNT_BASE=$BASE MINIMAL_NONFILERS=1
python research/rebuild/minimal/07_baseline.py
echo "=== done $(date)"
