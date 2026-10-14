#!/bin/bash
# evaluate.py for the night-of-2026-10-08 experiments; outputs in $S/exp/eval/<label>/
# (the reference baselines' cache is shared with $S/eval/_cache through a link).
# Usage: sbatch exp_eval.sbatch <reference file> label=file [label=file ...]
#SBATCH --job-name=pg_expeval
#SBATCH --partition=day
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/expeval_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/expeval_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-8}
S=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
cd $S
ref=$1; shift
for pair in "$@"; do
  label=${pair%%=*}; file=${pair#*=}
  echo "=== $label ($file) vs $ref"
  python -u "$CODE/evaluate.py" --synthetic "$file" --label "$label" --reference "$ref" \
      --eval-dir "$S/exp/eval" || echo "FAILED: $label"
done
echo "exp_eval finished"
