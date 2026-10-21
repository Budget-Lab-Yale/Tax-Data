#!/bin/bash
#SBATCH --job-name=min18sc
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/18_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/18_%x_%j.err
# Experiment 18, tilt-then-distill (18_DISTILL.md): score the fine-tuned files
# against the reweighted v4b file.
#   sbatch research/rebuild/minimal/18_score.sbatch A A20 B
# Touches markers/STEP18_score_DONE, or writes markers/STEP18_score_FAILED.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
mkdir -p $R/markers
rm -f $R/markers/STEP18_score_DONE $R/markers/STEP18_score_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
export MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
if ! python research/rebuild/minimal/18_score.py "$@"; then
  echo "score failed; log $R/logs/18_${SLURM_JOB_NAME}_${SLURM_JOB_ID}.out" > $R/markers/STEP18_score_FAILED
  exit 1
fi
touch $R/markers/STEP18_score_DONE
