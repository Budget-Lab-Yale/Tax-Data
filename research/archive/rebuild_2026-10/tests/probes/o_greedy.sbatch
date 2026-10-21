#!/bin/bash
# Probe O: greedy order from model seed 1 on PUF training records.
#SBATCH --job-name=o_greedy
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=00:40:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/o_greedy_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/o_greedy_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
python -u o_greedy.py --checkpoint /nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/checkpoint_full_v2_w1_seed01.pt \
  --out /nfs/roberts/scratch/pi_nrs36/jar335/probes/o/greedy_seed01.json
echo "$(date) done"
