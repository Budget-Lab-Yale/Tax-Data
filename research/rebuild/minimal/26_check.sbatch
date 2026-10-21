#!/bin/bash -l
#SBATCH --job-name=min26
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --exclude=a1125u14n01,a1125u05n01
#SBATCH --gres=gpu:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/26_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/26_%x_%j.err
# Step 26 (26_LABEL_ON.md): the PUF-pile check of every compared model, then
# the tables. Training is 22_run.sbatch with the label-on arms, e.g.
#   MINIMAL_F22_OUT=<scratch>/fake26 sbatch research/rebuild/minimal/22_run.sbatch D1 20261016 \
#     latent_fixed_label latent_learn_label latent_learn_label_drop
# Models already checked (fake26/pufcheck/*.json) are skipped.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
python research/rebuild/minimal/26_label_on.py pufcheck
python research/rebuild/minimal/26_label_on.py fills
python research/rebuild/minimal/26_label_on.py report
echo "=== done $(date)"
