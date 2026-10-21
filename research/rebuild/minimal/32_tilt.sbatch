#!/bin/bash -l
#SBATCH --job-name=s32tilt
#SBATCH --partition=day,week
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step32/tilt_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step32/tilt_%x_%j.err
# Step 32 (32_STATUS.md): the tilt of one mixed starting file (32_mix.py) to
# 2019, with step 31's settings (31_tilt.sbatch): 136 IRS targets, eight SCF
# 2019 wealth targets, the non-filer count; capital gains pieces put on the
# targets' definitions.
#   sbatch -J t32k1m80 research/rebuild/minimal/32_tilt.sbatch 1 m80 scf_units
#   sbatch -J t32k1m80f research/rebuild/minimal/32_tilt.sbatch 1 m80 file_units
#   sbatch -J t32k1m80d research/rebuild/minimal/32_tilt.sbatch 1 m80 scf_units t19
# Net worth dollars: by default each record's rank is turned into dollars
# with the map of the wave it was drawn under (map/V32/map_scf_nw_2015 is
# step 31's map unchanged; map_scf_nw_2018 is the 2019 wave's, in the 2015
# dollars of the targets). With a fourth argument t19, every rank is turned
# into dollars with the 2019 wave's map, whatever wave it was drawn under
# (plan decisions 3 and 10; MINIMAL_NW_TARGET_MAP_YEAR=2018).
# Output: tilt/step32/s32_ks<k>_<mix>[_t19][_file]/. Skips itself if done.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
K=${1:?seed number}; MIX=${2:?m16, m80 or m19}; BASE=${3:?scf_units or file_units}; DOLLARS=${4:-}
V=s32_ks${K}_${MIX}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export MINIMAL_TABLE=E27 MINIMAL_RUN=step31/Ks$K MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V32 STACK_VERSION=$V
export MINIMAL_NW_MAP_YEARS=2015,2018
TAG=step32/$V
if [ "$DOLLARS" = t19 ]; then export MINIMAL_NW_TARGET_MAP_YEAR=2018; TAG=${TAG}_t19; fi
case "$BASE" in
  scf_units)  ;;
  file_units) TAG=${TAG}_file ;;
  *) echo "unknown base $BASE"; exit 1 ;;
esac
export TILT_TAG=$TAG WEALTH_UNITS=all WEALTH_COUNT_BASE=$BASE MINIMAL_NONFILERS=1 MINIMAL_KG_PIECES=1
export OMP_NUM_THREADS=2 OPENBLAS_NUM_THREADS=2 MKL_NUM_THREADS=2
echo "=== tilt $V to tilt/$TAG $(date)"
# if the solver misses its tolerance by rounding (23_NONFILERS.md), retry with one thread (as 31_tilt.sbatch)
[ -f $R/tilt/$TAG/tables_$V.md ] || python research/rebuild/minimal/07_baseline.py || \
  OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 python research/rebuild/minimal/07_baseline.py
echo "=== done $(date)"
