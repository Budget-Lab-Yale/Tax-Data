#!/bin/bash -l
#SBATCH --job-name=s29tilt
#SBATCH --partition=day,week
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step29/tilt_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step29/tilt_%x_%j.err
# Step 29 (29_STATUS.md), as step 27: the tilt of one run's stack to 2019 (07_baseline.py: 136 IRS
# targets, eight SCF wealth targets on the SCF's units, non-filer count), with
# the capital gains pieces turned into the targets' definitions
# (04_dollars.py MINIMAL_KG_PIECES=1). Output: tilt/step29/s29_<run>/.
#   sbatch -J tC1s1 research/rebuild/minimal/29_tilt.sbatch C1s1
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run name}; V=s29_$(echo $RUN | tr A-Z a-z)
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export MINIMAL_TABLE=E27 MINIMAL_RUN=step29/$RUN MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V27 STACK_VERSION=$V
# optional second argument "dist": the capital gains targets restated for the
# pieces (soi_cells.py MINIMAL_KG_DIST_TARGETS=1: distributions as their own
# count and amount per bracket); output tilt/step29/s29_<run>_dist/
TAGX=""; if [ "${2:-}" = dist ]; then export MINIMAL_KG_DIST_TARGETS=1; TAGX=_dist; fi
export TILT_TAG=step29/$V$TAGX WEALTH_UNITS=all WEALTH_COUNT_BASE=scf_units MINIMAL_NONFILERS=1 MINIMAL_KG_PIECES=1
export OMP_NUM_THREADS=2 OPENBLAS_NUM_THREADS=2 MKL_NUM_THREADS=2
# if the solver misses its tolerance by rounding (23_NONFILERS.md), retry with one thread
[ -f $R/tilt/step29/$V$TAGX/tables_$V.md ] || python research/rebuild/minimal/07_baseline.py || \
  OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 python research/rebuild/minimal/07_baseline.py
echo "=== done $(date)"
