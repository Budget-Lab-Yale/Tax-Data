#!/bin/bash
#SBATCH --job-name=min02sweep
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --array=0-4
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/02_sweep_%A_%a.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/02_sweep_%A_%a.err
# Minimal example, step 2: network size sweep (02_size_sweep.py). One array
# task per size (0 = smallest, 4 = largest); each trains and scores one
# network into model/sweep/<name>/. Afterwards, on any node:
#   python3 research/rebuild/minimal/02_size_sweep.py summarize
# which reads the five result.json files and writes sweep/summary.md.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
python research/rebuild/minimal/02_size_sweep.py "$SLURM_ARRAY_TASK_ID"
