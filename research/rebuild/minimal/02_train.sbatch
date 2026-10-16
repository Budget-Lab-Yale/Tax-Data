#!/bin/bash
#SBATCH --job-name=min02
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=04:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/02_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/02_train_%j.err
# Minimal example, step 2: train the model (02_model.py), then score it on
# the test split (02_evaluate.py).
#   sbatch research/rebuild/minimal/02_train.sbatch          the real run
#   sbatch research/rebuild/minimal/02_train.sbatch smoke    5 epochs into model_smoke/
# The real run waits until step 1 has written its marker file, because the
# stacked table is being rebuilt while this is written.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
MARKER=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/COARSEN_DONE   # the table with coarsened SCF zeros (v3 model)
if [ "${1:-real}" = "smoke" ]; then
  export MINIMAL_SMOKE=1 MINIMAL_MAX_EPOCHS=5
  export MINIMAL_MODEL_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/model_smoke
else
  until [ -f "$MARKER" ]; do echo "waiting for $MARKER ($(date))"; sleep 60; done
fi
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
python research/rebuild/minimal/02_model.py
python research/rebuild/minimal/02_evaluate.py
