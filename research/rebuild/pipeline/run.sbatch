#!/bin/bash -l
#SBATCH --job-name=rebuild
#SBATCH --partition=gpu_h100,gpu_h200,gpu_b200,gpu
#SBATCH --gres=gpu:1
#SBATCH --time=08:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/logs/%x_%j.out
# The whole pipeline, or some stages, from the repository root:
#   sbatch research/rebuild/pipeline/run.sbatch                  every stage
#   sbatch research/rebuild/pipeline/run.sbatch train generate   only those
# Stages, in order: scf (R: SCF tax units), table, targets, train, generate, tilt.
# The R stage and the table need no GPU; run them alone on a CPU partition if you like:
#   sbatch --partition=day --gres=gpu:0 research/rebuild/pipeline/run.sbatch scf table targets
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
P=research/rebuild/pipeline
STAGES=${*:-scf table targets train generate tilt}
PY_MODULES='PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a'
export PYTHONDONTWRITEBYTECODE=1
for s in $STAGES; do
  echo "=== $s $(date)"
  [ $s = scf ] || module load $PY_MODULES
  case $s in
    scf)      module purge; module load R/4.4.1-foss-2022b; Rscript $P/build_scf.R; module purge ;;
    table)    python $P/build_table.py ;;
    targets)  python $P/targets.py ;;
    train)    python $P/train.py ;;
    generate) python $P/generate.py ;;
    tilt)     python $P/tilt.py ;;
    *) echo "unknown stage $s"; exit 1 ;;
  esac
done
echo "=== done $(date)"
