#!/bin/bash
#SBATCH --job-name=s19
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/19_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/19_%x_%j.err
# Step 19 (19_DIRECT.md): score the adjusted networks' draws against reweighted v4b.
#   sbatch -J s19 research/rebuild/minimal/19_cpu.sbatch all last
# Touches markers/STEP19_score_DONE, or writes markers/STEP19_score_FAILED.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
mkdir -p $R/markers
rm -f $R/markers/STEP19_score_DONE $R/markers/STEP19_score_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
export MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/19_score.py "$@"
rc=$?
if [ $rc -ne 0 ]; then
  echo "19 score failed (exit $rc); log $R/logs/19_${SLURM_JOB_NAME}_${SLURM_JOB_ID}.out" > $R/markers/STEP19_score_FAILED
  exit $rc
fi
touch $R/markers/STEP19_score_DONE
