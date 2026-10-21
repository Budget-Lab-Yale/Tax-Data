#!/bin/bash
#SBATCH --job-name=min20c
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/20_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/20_%x_%j.err
# Step 20 (20_SEPARATE_MODELS.md), the CPU steps.
#   sbatch -J t20 research/rebuild/minimal/20_run_cpu.sbatch tables
#   sbatch -J ... research/rebuild/minimal/20_run_cpu.sbatch tilt <stack dir> <tag> <tilt dir>
#   sbatch -J ... research/rebuild/minimal/20_run_cpu.sbatch heldback
#   sbatch -J ... research/rebuild/minimal/20_run_cpu.sbatch report
#   sbatch -J ... research/rebuild/minimal/20_run_cpu.sbatch diag
# Touches markers/STEP20_<what>[_<tag>]_DONE or writes ..._FAILED.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
WHAT=${1:?tables, tilt, heldback, report or diag}
KEY=$WHAT${3:+_$3}
mkdir -p $R/markers
rm -f $R/markers/STEP20_${KEY}_DONE $R/markers/STEP20_${KEY}_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1 MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
D=research/rebuild/minimal
case $WHAT in
  tables) python $D/20_tables.py ;;
  tilt)   python $D/20_tilt.py $2/gen_puf_stack_$3.parquet $2/gen_meta_$3.json $4 $3 ;;
  heldback)
    mkdir -p $R/checks/S20
    python $D/20_heldback.py $R/checks/S20/heldback.csv V4=$R/model/V4 B1=$R/model/S20_PUF B2=$R/model/S20_SCF \
      V4_s2=$R/model/S20_V4_seed2 B1_s2=$R/model/S20_PUF_seed2 B2_s2=$R/model/S20_SCF_seed2 \
      B2_long=$R/model/S20_SCF_long B2_long_s2=$R/model/S20_SCF_long_seed2 ;;
  report) python $D/20_report.py ;;
  diag)   python $D/20_diag.py V4=$R/model/V4 B2=$R/model/S20_SCF V4_s2=$R/model/S20_V4_seed2 B2_s2=$R/model/S20_SCF_seed2 \
            B2_long=$R/model/S20_SCF_long B2_long_s2=$R/model/S20_SCF_long_seed2 ;;
  *) echo "unknown step $WHAT"; exit 2 ;;
esac
rc=$?
if [ $rc -ne 0 ]; then
  echo "20 $KEY failed (exit $rc); log $R/logs/20_${SLURM_JOB_NAME}_${SLURM_JOB_ID}.out" > $R/markers/STEP20_${KEY}_FAILED
  exit $rc
fi
touch $R/markers/STEP20_${KEY}_DONE
