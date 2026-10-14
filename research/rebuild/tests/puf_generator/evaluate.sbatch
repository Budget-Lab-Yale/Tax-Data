#!/bin/bash
# Evaluation for test 1 (SPEC.md section C). Arguments are passed to evaluate.py:
#   sbatch evaluate.sbatch --label baseline
#   sbatch evaluate.sbatch --synthetic $S/model/synthetic.parquet --label model \
#          --update-report <repo>/research/rebuild/tests/puf_generator/EVAL_REPORT.md
#SBATCH --job-name=pg_eval
#SBATCH --partition=day
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/eval_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/eval_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-8}
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
cd /nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
python -u "$CODE/evaluate.py" "$@"
