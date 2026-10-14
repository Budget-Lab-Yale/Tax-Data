#!/bin/bash
# Evaluate a list of files against one reference, one after another (they
# share the reference's cached baselines).
# Usage: sbatch eval_list.sbatch <reference file or 'test'> <suffix> label=file [label=file ...]
#SBATCH --job-name=pg_eval_list
#SBATCH --partition=day
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G
#SBATCH --time=04:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/evallist_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/evallist_%j.err
set -uo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-8}
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
cd /nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
ref=$1; suffix=$2; shift 2
refarg=(); [ "$ref" != "test" ] && refarg=(--reference "$ref")
for pair in "$@"; do
  label=${pair%%=*}; file=${pair#*=}
  echo "=== $label$suffix ($file) vs $ref"
  python -u "$CODE/evaluate.py" --synthetic "$file" --label "$label$suffix" "${refarg[@]}" \
    || echo "FAILED: $label$suffix"
done
echo "eval_list finished"
