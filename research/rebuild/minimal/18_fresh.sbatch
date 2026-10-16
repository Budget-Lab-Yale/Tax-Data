#!/bin/bash
#SBATCH --job-name=min18fr
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/18_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/18_%x_%j.err
# Experiment 18 (18_DISTILL.md): fresh tilted records for variant C (18_fresh.py gen, then weights).
#   sbatch research/rebuild/minimal/18_fresh.sbatch ["gen weights" or retilt]
# Touches markers/STEP18_fresh_DONE, or writes markers/STEP18_fresh_FAILED.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
V=fresh
STEPS=${1:-gen weights}
mkdir -p $R/markers
rm -f $R/markers/STEP18_${V}_DONE $R/markers/STEP18_${V}_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
export MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
for step in $STEPS; do
  echo "== $step $(date)"
  if ! python research/rebuild/minimal/18_fresh.py $step; then
    echo "$step failed" > $R/markers/STEP18_${V}_FAILED
    exit 1
  fi
done
touch $R/markers/STEP18_${V}_DONE
echo "done $(date)"
