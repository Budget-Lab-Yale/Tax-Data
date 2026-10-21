#!/bin/bash -l
#SBATCH --job-name=s33tilt
#SBATCH --partition=day,week
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step33/tilt_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step33/tilt_%x_%j.err
# Step 33 (33_STATUS.md), as step 31: the tilt of one run's stack to 2019
# (07_baseline.py: 136 IRS targets, eight SCF wealth targets, non-filer
# count). Dollars from map/V33; the nine new AGI parts are folded back into
# other_agi before AGI is computed (04_dollars MINIMAL_AGI_FOLD).
#   sbatch -J tNs1 research/rebuild/minimal/33_tilt.sbatch Ns1 scf_units
#   sbatch -J tNs1f research/rebuild/minimal/33_tilt.sbatch Ns1 file_units
#   with tax-exempt interest targets as well: third argument tei (output .._tei)
# Output: tilt/step33/s33_<run> (scf_units) or tilt/step33/s33_<run>_file (file_units).
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run name}; BASE=${2:?scf_units or file_units}; EXTRA=${3:-}
V=s33_$(echo $RUN | tr A-Z a-z)
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
source research/rebuild/minimal/33_env.sh
export MINIMAL_RUN=step33/$RUN STACK_VERSION=$V
case "$BASE" in
  scf_units)  TAG=step33/$V ;;
  file_units) TAG=step33/${V}_file ;;
  *) echo "unknown base $BASE"; exit 1 ;;
esac
if [ "$EXTRA" = tei ]; then export MINIMAL_TEI_TARGETS=1; TAG=${TAG}_tei; fi
export TILT_TAG=$TAG WEALTH_UNITS=all WEALTH_COUNT_BASE=$BASE MINIMAL_NONFILERS=1
export OMP_NUM_THREADS=2 OPENBLAS_NUM_THREADS=2 MKL_NUM_THREADS=2
[ -f $R/tilt/$TAG/tables_$V.md ] || python research/rebuild/minimal/07_baseline.py || \
  OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 python research/rebuild/minimal/07_baseline.py
echo "=== done $(date)"
