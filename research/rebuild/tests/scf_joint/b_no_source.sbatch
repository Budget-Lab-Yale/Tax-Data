#!/bin/bash
# Matched source-embedding ablation: weighted expA_clean, seeds 1-3.
# sbatch b_no_source.sbatch
#SBATCH --job-name=b_no_source
#SBATCH --array=1-3
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=00:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_no_source_%A_%a.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_no_source_%A_%a.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-4}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
python -u b_no_source_check.py
# Separate output tree avoids evaluation races with existing watchers.
export MODEL_ROOT=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/no_source/model
bash b_pipeline.sbatch expA_clean joint_w1_nosource "$SLURM_ARRAY_TASK_ID" --no-source
NN=$(printf '%02d' "$SLURM_ARRAY_TASK_ID")
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/no_source/eval
python -u c_evaluate.py --dataset expA_clean --out-root "$OUT" \
    --completed "$MODEL_ROOT/expA_clean/completed_joint_w1_nosource_plain_seed${NN}.parquet" \
    --label "model_joint_w1_nosource_plain_seed${NN}"
echo "source ablation and evaluation finished for seed $SLURM_ARRAY_TASK_ID"
