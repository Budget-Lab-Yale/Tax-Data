#!/bin/bash
#SBATCH --job-name=min20g
#SBATCH --partition=gpu,scavenge_gpu
#SBATCH --gres=gpu:a40:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/20_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/20_%x_%j.err
# Step 20 (20_SEPARATE_MODELS.md), the GPU steps. A40, the GPU type model V4
# was trained on, so that training differences are not GPU-type differences.
#   sbatch -J s20seed1 research/rebuild/minimal/20_run_gpu.sbatch seed1
#   sbatch -J s20seed2 research/rebuild/minimal/20_run_gpu.sbatch seed2
# seed1: train B1 (PUF records only, model/S20_PUF) and B2 (SCF records only,
#        model/S20_SCF) with 02_model.py's settings of V4; generate the
#        stacks of arm B (gen/S20/seed1) and arm C (V4, gen/C20/seed1).
# seed2: the same with training seed 2 for B1, B2 and V4 (model/S20_PUF_seed2,
#        model/S20_SCF_seed2, model/S20_V4_seed2), stacks gen/S20/seed2,
#        gen/C20/seed2, and arm A (v4b's rule) from V4 seed 2, gen/C20/A_seed2.
# long:  B2 trained for 516 passes (as many gradient steps as V4), both seeds,
#        model/S20_SCF_long[_seed2], stacks gen/S20/long_seed1, gen/S20/long_seed2.
# Touches markers/STEP20_<what>_DONE or writes markers/STEP20_<what>_FAILED.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
WHAT=${1:?seed1, seed2 or long}
mkdir -p $R/markers
rm -f $R/markers/STEP20_${WHAT}_DONE $R/markers/STEP20_${WHAT}_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1 MINIMAL_ALL_RECORDS=1 MINIMAL_RUN=V4
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
D=research/rebuild/minimal
fail() { echo "$1 failed; log $R/logs/20_${SLURM_JOB_NAME}_${SLURM_JOB_ID}.out" > $R/markers/STEP20_${WHAT}_FAILED; echo "FAILED at $1"; exit 1; }
train() {  # table, model dir, seed (empty = 02_model's own), passes (empty = 80)
  echo "== train $1 -> $2 seed ${3:-default} passes ${4:-80} $(date)"
  MINIMAL_TABLE=$1 MINIMAL_MODEL_DIR=$2 MINIMAL_GEN_DIR=$2/unused_gen MINIMAL20_TRAIN_SEED=${3:-} \
    MINIMAL_MAX_EPOCHS=${4:-80} \
    python $D/20_train.py || fail "train $2"
}
gen() {
  echo "== generate $* $(date)"
  MINIMAL_TABLE=E4 python $D/20_generate.py "$@" || fail "generate $*"
}
case $WHAT in
  seed1)
    train E4puf $R/model/S20_PUF
    train E4scf $R/model/S20_SCF
    gen two $R/model/S20_PUF $R/model/S20_SCF $R/gen/S20/seed1 s20
    gen two $R/model/V4 $R/model/V4 $R/gen/C20/seed1 c20 ;;
  seed2)
    train E4puf $R/model/S20_PUF_seed2 20261116
    train E4scf $R/model/S20_SCF_seed2 20261116
    train E4 $R/model/S20_V4_seed2 20261116
    gen two $R/model/S20_PUF_seed2 $R/model/S20_SCF_seed2 $R/gen/S20/seed2 s20s2
    gen two $R/model/S20_V4_seed2 $R/model/S20_V4_seed2 $R/gen/C20/seed2 c20s2
    gen A $R/model/S20_V4_seed2 $R/gen/C20/A_seed2 a20s2 ;;
  long)
    # B2 with as many gradient steps as V4: V4 takes 58 steps per pass x 80
    # passes = 4,640; the SCF records alone take 9 steps per pass, so 516
    # passes. Both seeds; arm B rebuilt with B1 and this B2.
    train E4scf $R/model/S20_SCF_long "" 516
    train E4scf $R/model/S20_SCF_long_seed2 20261116 516
    gen two $R/model/S20_PUF $R/model/S20_SCF_long $R/gen/S20/long_seed1 s20l
    gen two $R/model/S20_PUF_seed2 $R/model/S20_SCF_long_seed2 $R/gen/S20/long_seed2 s20ls2 ;;
  *) echo "unknown step $WHAT"; exit 2 ;;
esac
touch $R/markers/STEP20_${WHAT}_DONE
echo "done $(date)"
