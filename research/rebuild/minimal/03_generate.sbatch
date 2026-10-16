#!/bin/bash
#SBATCH --job-name=min03
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/03_generate_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/03_generate_%j.err
# Minimal example, step 3: generate records from the step 2 model
# (03_generate.py), then compare them with the held-out data (03_compare.py).
#   sbatch --dependency=afterok:<step 2 job> research/rebuild/minimal/03_generate.sbatch
#   sbatch research/rebuild/minimal/03_generate.sbatch smoke   uses model_smoke/, writes gen_smoke/
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
if [ "${1:-real}" = "smoke" ]; then
  export MINIMAL_SMOKE=1
  export MINIMAL_MODEL_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/model_smoke
  export MINIMAL_GEN_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/gen_smoke
fi
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/03_generate.py
python research/rebuild/minimal/03_compare.py
