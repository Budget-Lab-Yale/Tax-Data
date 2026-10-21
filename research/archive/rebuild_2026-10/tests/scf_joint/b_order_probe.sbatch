#!/bin/bash
# Six completion-only runs: reverse wealth order, random/orderless, three seeds.
#SBATCH --job-name=b_order_probe
#SBATCH --array=0-5%2
#SBATCH --partition=gpu_rtx6000,gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=00:15:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_order_probe_%A_%a.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_order_probe_%A_%a.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
SEED=$((SLURM_ARRAY_TASK_ID % 3 + 1))
STYLE=random
if (( SLURM_ARRAY_TASK_ID >= 3 )); then STYLE=orderless; fi
NN=$(printf '%02d' "$SEED")
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
ROOT=$S/followup_20261009
MODEL=$ROOT/model/expB
CFG=joint_w1_nosource_$STYLE
python -u b_complete.py --data-dir "$S/data/expB" --out-dir "$MODEL" \
    --checkpoint "$MODEL/checkpoint_${CFG}_seed${NN}.pt" --train-seed "$SEED" \
    --seed "$((20261011 + SEED))" --k 1 --target-sources SCF \
    --tag "${CFG}_reverse_scf" --reverse-wealth-order
python -u c_evaluate.py --dataset expB --out-root "$ROOT/eval" \
    --completed "$MODEL/completed_${CFG}_reverse_scf_plain_seed${NN}.parquet" \
    --label "model_${CFG}_reverse_scf_plain_seed${NN}"
