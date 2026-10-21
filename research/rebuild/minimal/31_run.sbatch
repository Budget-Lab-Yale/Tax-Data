#!/bin/bash -l
#SBATCH --job-name=s31run
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --exclude=a1125u14n01,a1125u05n01
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step31/run_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step31/run_%x_%j.err
# Step 31 (31_STATUS.md): the cell method (design/cell_method.md), one run,
# GPU part: train (31_train.py), then the stack (03_stack.py), as step 29.
#   sbatch -J Ks1 research/rebuild/minimal/31_run.sbatch Ks1 20261016
#   sbatch -J F29Ks1 --export=ALL,MINIMAL_TABLE=F29 research/rebuild/minimal/31_run.sbatch F29Ks1 20261016
#   (the fake survey of step 29: train, then 29_fake.py check, output step29/fake/F29Ks1.*)
# Each step skips itself when its output exists, so a resubmission resumes.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run name}; SEED=${2:?seed}; V=s31_$(echo $RUN | tr A-Z a-z)
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
D=research/rebuild/minimal
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
export MINIMAL_TABLE=${MINIMAL_TABLE:-E27} MINIMAL_RUN=step31/$RUN MINIMAL_SEED=$SEED MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V27
echo "=== train $(date)"
[ -f $R/model/step31/$RUN/model.pt ] || python $D/31_train.py
if [ "${MINIMAL_MAX_EPOCHS:-80}" != 80 ]; then echo "=== smoke run: no stack"; exit 0; fi
if [ "$MINIMAL_TABLE" = F29 ]; then
  echo "=== fake check $(date)"   # step 29's check: hidden holders and dollars against the planted truth
  [ -f $R/step29/fake/$RUN.json ] || python $D/29_fake.py check
  echo "=== done $(date)"; exit 0
fi
echo "=== stack $(date)"
export STACK_VERSION=$V MINIMAL_STACK_LABEL=scf MINIMAL_ORDER=own_first MINIMAL_STACK_DIR=$R/gen \
       MINIMAL_STACK_FOREIGN=absent MINIMAL_COMPUTED_TOTAL=1 MINIMAL_STACK_CG_STRATIFIER=kg_lt
[ -f $R/gen/gen_meta_$V.json ] || python $D/03_stack.py
echo "=== done $(date)"
