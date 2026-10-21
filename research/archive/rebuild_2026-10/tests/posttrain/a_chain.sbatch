#!/bin/bash
# Test 3, piece A: unattended chain for the full model.
# Usage: sbatch a_chain.sbatch <version>     (version: full or full_v2)
#  1. waits for $T2/data/<version>/DONE, then builds the target sets
#     (a_full.py build) unless $P/<version>/a/DONE_targets exists;
#  2. waits for $T2/model/<version>/DONE_seed01, then submits the GPU job
#     for arm P's reference draws (a_full.py draws --k 32), which writes
#     $P/<version>/a/DONE_ref_P.
#SBATCH --job-name=a_chain
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=20:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/a_chain_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/a_chain_%j.err
set -euo pipefail
V=$1
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
cd "$CODE"
echo "$(date) waiting for $T2/data/$V/DONE"
until [ -f "$T2/data/$V/DONE" ]; do sleep 60; done
if [ ! -f "$P/$V/a/DONE_targets" ]; then
  echo "$(date) building targets"
  python -u a_full.py build --dataset "$V"
fi
echo "$(date) waiting for $T2/model/$V/DONE_seed01"
until [ -f "$T2/model/$V/DONE_seed01" ]; do sleep 60; done
echo "$(date) submitting reference draws"
sbatch --time=08:00:00 -p gpu_b200,gpu_h100,gpu_h200 --job-name="a_P_$V" a_gpu.sbatch a_full.py draws --dataset "$V" --k 32
