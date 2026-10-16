#!/bin/bash
#SBATCH --job-name=min18
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/18_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/18_%x_%j.err
# Experiment 18, tilt-then-distill (18_DISTILL.md): the GPU steps for one variant.
#   sbatch -J d18A research/rebuild/minimal/18_distill.sbatch A [steps]
# steps default to "train generate logprob". Touches markers/STEP18_<variant>_DONE,
# or writes markers/STEP18_<variant>_FAILED naming the step that failed.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
V=${1:?variant required}
STEPS=${2:-train generate logprob}
mkdir -p $R/markers
rm -f $R/markers/STEP18_${V}_DONE $R/markers/STEP18_${V}_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
export MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
for step in $STEPS; do
  echo "== $step $V $(date)"
  if ! python research/rebuild/minimal/18_distill.py $step $V; then
    echo "$step failed; log $R/logs/18_${SLURM_JOB_NAME}_${SLURM_JOB_ID}.out" > $R/markers/STEP18_${V}_FAILED
    exit 1
  fi
done
touch $R/markers/STEP18_${V}_DONE
echo "done $(date)"
