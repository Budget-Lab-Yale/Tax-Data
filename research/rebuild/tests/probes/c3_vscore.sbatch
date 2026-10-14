#!/bin/bash
# Probe C: probe V's scorecard (v_score.py, run unchanged) on one generated file.
# Usage: sbatch c3_vscore.sbatch <parquet> <label> <out json>
#SBATCH --job-name=c3_vscore
#SBATCH --partition=day,scavenge
#SBATCH --requeue
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_vscore_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_vscore_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
mkdir -p "$(dirname "$3")"
python -u v_score.py --gen "$1" --label "$2" --out "$3"
echo "$(date) done"
