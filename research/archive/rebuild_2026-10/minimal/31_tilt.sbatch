#!/bin/bash -l
#SBATCH --job-name=s31tilt
#SBATCH --partition=day,week
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step31/tilt_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step31/tilt_%x_%j.err
# Step 31 (31_STATUS.md), as step 29: the tilt of one run's stack to 2019
# (07_baseline.py: 136 IRS targets, eight SCF wealth targets, non-filer count;
# capital gains pieces turned into the targets' definitions). Note, step 13:
# the wealth count targets on two bases.
#   sbatch -J tKs1 research/rebuild/minimal/31_tilt.sbatch step31 Ks1 scf_units
#   sbatch -J tKs1f research/rebuild/minimal/31_tilt.sbatch step31 Ks1 file_units
# The first argument is the step whose stack is tilted (step31, or step29 / step27
# for the comparison runs, whose stacks exist). Output: tilt/step31/s<step>_<run>
# (scf_units) or tilt/step31/s<step>_<run>_file (file_units).
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
STEP=${1:?step31, step29 or step27}; RUN=${2:?run name}; BASE=${3:?scf_units or file_units}
V=s${STEP: -2}_$(echo $RUN | tr A-Z a-z)
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export MINIMAL_TABLE=E27 MINIMAL_RUN=$STEP/$RUN MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V27 STACK_VERSION=$V
case "$BASE" in
  scf_units)  TAG=step31/$V ;;
  file_units) TAG=step31/${V}_file ;;
  *) echo "unknown base $BASE"; exit 1 ;;
esac
export TILT_TAG=$TAG WEALTH_UNITS=all WEALTH_COUNT_BASE=$BASE MINIMAL_NONFILERS=1 MINIMAL_KG_PIECES=1
export OMP_NUM_THREADS=2 OPENBLAS_NUM_THREADS=2 MKL_NUM_THREADS=2
# if the solver misses its tolerance by rounding (23_NONFILERS.md), retry with one thread
[ -f $R/tilt/$TAG/tables_$V.md ] || python research/rebuild/minimal/07_baseline.py || \
  OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 python research/rebuild/minimal/07_baseline.py
echo "=== done $(date)"
