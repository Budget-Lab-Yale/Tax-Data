#!/bin/bash
# Probe X: complete PUF training records' wealth with only the shared
# variables visible (PUF-only variables hidden), five draws, as puf_wealth.
# Usage: sbatch x_complete_shared.sbatch <full_v2|full_v3x> <seed>
#SBATCH --job-name=x_compsh
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/x_compsh_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/x_compsh_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
POST=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/x
VER=$1; SEED=$2; NN=$(printf %02d "$SEED")
if [ "$VER" = full_v2 ]; then CK=$T2/model/full_v2/checkpoint_full_v2_w1_seed$NN.pt; else CK=$Q/model/$VER/checkpoint_${VER}_w1_seed$NN.pt; fi
OUT=$Q/model/sharedonly/$VER; mkdir -p $OUT
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
python -u $POST/h_complete.py --data-dir $Q/data/${VER}_sharedonly --checkpoint $CK --out-dir $OUT \
    --train-seed $SEED --tag puf_wealth --mode complete --fill wealth --split train \
    --target-sources PUF --as-source SCF --decode-year 2015 --draws 5 --seed $((20261021 + 100 * SEED)) --no-score
