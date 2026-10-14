#!/bin/bash
# Run exp_seqnll.py for several (checkpoint label [options]) lines read from a file.
# Usage: sbatch exp_seqnll.sbatch <jobs file: one line per run, arguments to exp_seqnll.py>
#SBATCH --job-name=pg_seqnll
#SBATCH --partition=gpu_b200,gpu,gpu_rtx6000,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=00:20:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/seqnll_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/seqnll_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
while read -r line; do [ -n "$line" ] && eval python -u exp_seqnll.py $line; done < "$1"
