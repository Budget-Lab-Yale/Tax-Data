#!/bin/bash
#SBATCH --job-name=min16
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/16_diag_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/16_diag_%j.err
# Step 16 diagnostics (16_diag.py) on already-trained runs; no training.
#   sbatch research/rebuild/minimal/16_diag.sbatch E0 E4 E7 E8 E9
# Each run is a separate python process with that run's training flags.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
for T in "$@"; do
  echo "===== $T $(date)"
  case $T in
    E0) env DIAG_RUN=E0 MINIMAL_MODEL_DIR=$R/model/v2 MINIMAL_GEN_DIR=$R/gen/v2 python research/rebuild/minimal/16_diag.py ;;
    E4) env DIAG_RUN=E4 MINIMAL_RUN=E4 MINIMAL_TABLE=E4 python research/rebuild/minimal/16_diag.py ;;
    E7) env DIAG_RUN=E7 MINIMAL_RUN=E7 MINIMAL_NO_LABEL_SHARED=1 python research/rebuild/minimal/16_diag.py ;;
    E8) env DIAG_RUN=E8 MINIMAL_RUN=E8 MINIMAL_NO_LABEL_SHARED=1 MINIMAL_COARSEN=1 python research/rebuild/minimal/16_diag.py ;;
    E9) env DIAG_RUN=E9 MINIMAL_RUN=E9 MINIMAL_TABLE=E4 MINIMAL_NO_LABEL_SHARED=1 MINIMAL_REPORT_CURVE=1 python research/rebuild/minimal/16_diag.py ;;
    *) echo "unknown run $T" ;;
  esac
  echo "exit $? for $T"
done
