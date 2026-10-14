#!/bin/bash
# Fifteen runs: three seeds for real-data no-source, and clean/real data
# with corrected orderless or fixed-order objectives. Outputs isolated.
#SBATCH --job-name=b_followup
#SBATCH --array=0-14%4
#SBATCH --partition=gpu_rtx6000,gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=00:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_followup_%A_%a.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_followup_%A_%a.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
python -u b_mask_check.py
TASK=${SLURM_ARRAY_TASK_ID}
SEED=$((TASK % 3 + 1))
case $((TASK / 3)) in
  0) DS=expB; STYLE=random ;;
  1) DS=expA_clean; STYLE=orderless ;;
  2) DS=expB; STYLE=orderless ;;
  3) DS=expA_clean; STYLE=ordered ;;
  4) DS=expB; STYLE=ordered ;;
esac
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
ROOT=$S/followup_20261009
DATA=$S/data/$DS
MODEL=$ROOT/model/$DS
CFG=joint_w1_nosource_$STYLE
NN=$(printf '%02d' "$SEED")
CSEED=$((20261011 + SEED))
python -u b_train.py --data-dir "$DATA" --out-dir "$MODEL" --tag "${CFG}_seed${NN}" \
    --seed "$SEED" --drop-vars scf_weight --loss-weight survey --no-source --mask-style "$STYLE"
CK=$MODEL/checkpoint_${CFG}_seed${NN}.pt
comp() { python -u b_complete.py --data-dir "$DATA" --out-dir "$MODEL" --checkpoint "$CK" \
    --train-seed "$SEED" --seed "$CSEED" --k 1 "$@"; }
if [[ $DS == expA_clean ]]; then
    comp --tag "$CFG"
    python -u c_evaluate.py --dataset "$DS" --out-root "$ROOT/eval" \
        --completed "$MODEL/completed_${CFG}_plain_seed${NN}.parquet" --label "model_${CFG}_plain_seed${NN}"
else
    comp --tag "${CFG}_scf" --target-sources SCF
    python -u c_evaluate.py --dataset "$DS" --out-root "$ROOT/eval" \
        --completed "$MODEL/completed_${CFG}_scf_plain_seed${NN}.parquet" --label "model_${CFG}_scf_plain_seed${NN}"
    comp --tag "${CFG}_puf" --target-sources PUF --decode-year 2015
    python -u c_evaluate_puf.py --dataset "$DS" --out-root "$ROOT/eval" \
        --completed "$MODEL/completed_${CFG}_puf_plain_seed${NN}.parquet" --label "model_${CFG}_puf_plain_seed${NN}"
fi
echo "FOLLOWUP DONE: $DS $STYLE seed $SEED"
