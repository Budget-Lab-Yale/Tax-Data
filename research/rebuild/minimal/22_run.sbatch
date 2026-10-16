#!/bin/bash
#SBATCH --job-name=min22
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=04:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/22_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/22_%x_%j.err
# Step 22 (22_FAKE_SURVEY.md): train and score the fake-survey arms.
#   sbatch -J f22_D1_s1 research/rebuild/minimal/22_run.sbatch D1 20261016 rule rank prefill nofill latent_fixed latent_learn
#   sbatch -J f22_D0_s1 research/rebuild/minimal/22_run.sbatch D0 20261016 oracle
# Arms already scored (fake22/<arm>_<D>_s<seed>/score.json) are skipped.
# MINIMAL_MAX_EPOCHS and MINIMAL_F22_OUT (a smoke-test folder) pass through.
set -euo pipefail
D=${1:?distortion}; SEED=${2:?seed}; shift 2
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
for ARM in "$@"; do
  echo "=== $ARM $D $SEED $(date)"
  python research/rebuild/minimal/22_fake_survey.py run "$ARM" "$D" "$SEED"
done
echo "=== done $(date)"
