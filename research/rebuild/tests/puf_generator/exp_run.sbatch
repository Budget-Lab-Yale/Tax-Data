#!/bin/bash
# Night-of-2026-10-08 training experiments (EXPERIMENTS.md, hypotheses 1, 3, 4):
# train one model into $S/exp/<name>/, then generate from its best checkpoint
# with seed 20261009.
# Usage: sbatch exp_run.sbatch <name> <n records to generate> [--gen-order a,b,..] -- <train.py args>
#SBATCH --job-name=pg_exp
#SBATCH --partition=gpu,gpu_rtx6000,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=00:50:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/exp_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/logs/exp_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
S=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
name=$1; ngen=$2; shift 2
genorder=()
if [ "${1:-}" = "--gen-order" ]; then genorder=(--order "$2"); shift 2; fi
[ "${1:-}" = "--" ] && shift
OUT=$S/exp/$name
mkdir -p "$OUT"
nvidia-smi --query-gpu=name,memory.total --format=csv
cd "$CODE"
echo "=== train $name: $*"
python -u train.py --data-dir "$S/data" --out-dir "$OUT" --tag "$name" "$@"
echo "=== generate $name: $ngen records, seed 20261009 ${genorder[*]}"
python -u generate.py --data-dir "$S/data" --out-dir "$OUT" --checkpoint "$OUT/checkpoint_$name.pt" \
    --out-name synthetic --seed 20261009 --n "$ngen" "${genorder[@]}"
touch "$OUT/DONE"
