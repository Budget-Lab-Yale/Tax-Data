#!/bin/bash
#SBATCH --job-name=min15
#SBATCH --partition=day
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/15_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/15_%x_%j.err
# Minimal example, v4 baseline (15_BASELINE_V4.md): the CPU steps.
#   sbatch -J maps     research/rebuild/minimal/15_run_cpu.sbatch maps
#   sbatch -J copy     research/rebuild/minimal/15_run_cpu.sbatch copy
#   sbatch -J stackchk research/rebuild/minimal/15_run_cpu.sbatch stack
#   sbatch -J tilt     research/rebuild/minimal/15_run_cpu.sbatch tilt
#   sbatch -J v4crep   research/rebuild/minimal/15_run_cpu.sbatch v4creport   (15_v4c_report.py)
# maps: 15_maps.py; copy, stack: 15_checks.py; tilt: 07_baseline.py with
# STACK_VERSION=v4, WEALTH_UNITS=filers and the V4 maps.
# Touches markers/STEP15_<what>_DONE, or writes markers/STEP15_<what>_FAILED.
# MARKER_PREFIX (added 2026-10-16) replaces STEP15_, so that a later run
# (v4c: STEP_V4C_) leaves the earlier runs' markers alone.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
WHAT=${1:?maps, copy, stack or tilt}
P=${MARKER_PREFIX:-STEP15_}
mkdir -p $R/markers
rm -f $R/markers/${P}${WHAT}_DONE $R/markers/${P}${WHAT}_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
export MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
export STACK_VERSION=${STACK_VERSION:-v4} WEALTH_UNITS=${WEALTH_UNITS:-filers}
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
D=research/rebuild/minimal
case $WHAT in
  maps)  python $D/15_maps.py ;;
  copy|stack|tiltmore) python $D/15_checks.py $WHAT ;;
  tilt)  python $D/07_baseline.py ;;
  v4creport) python $D/15_v4c_report.py ;;
  *) echo "unknown step $WHAT"; exit 2 ;;
esac
rc=$?
if [ $rc -ne 0 ]; then
  echo "15 $WHAT failed (exit $rc); log $R/logs/15_${SLURM_JOB_NAME}_${SLURM_JOB_ID}.out" > $R/markers/${P}${WHAT}_FAILED
  exit $rc
fi
touch $R/markers/${P}${WHAT}_DONE
