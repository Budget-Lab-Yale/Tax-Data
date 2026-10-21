#!/bin/bash -l
#SBATCH --job-name=s34run
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --exclude=a1125u14n01,a1125u05n01
#SBATCH --time=06:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step34/run_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step34/run_%x_%j.err
# Step 34 (34_STATUS.md): one run, GPU part: train with the cell method
# (31_train.py, unchanged, reading 34_settings_scf.json through
# MINIMAL_SETTINGS) on table E34, then the stack (03_stack.py) with step 31's
# settings, except the third file is stratified on scorp_nonpass_inc (S corporation
# nonpassive income, the largest part; business_income is computed from its parts
# and so cannot be drawn first) and business_income is computed (MINIMAL_COMPUTED_EXTRA).
#   sbatch -J Ns1 research/rebuild/minimal/34_run.sbatch Ns1 20261016
#   bigger network: sbatch -J Bs1 --export=ALL,MINIMAL_LAYERS=6,MINIMAL_WIDTH=256,MINIMAL_HEADS=8,MINIMAL_FFN=512 ... Bs1 20261016
#   smoke test (4 passes, then the stack anyway): --export=ALL,MINIMAL_MAX_EPOCHS=4,SMOKE_STACK=1
#   19 October (34_hidecount.py): loss weighted by slots hidden, MINIMAL_HIDE_WEIGHT=1; longer
#   training, MINIMAL_MAX_EPOCHS=160 (a run with fewer than 80 passes stops before the stack)
#   dropout (34_dropcheck.py): MINIMAL_DROPOUT=0 trains without it; MINIMAL_DROPOUT_OFF_LAST=10 turns it
#   off for the last 10 passes; an existing model's stack with dropout on in generation:
#   --export=ALL,MINIMAL_GEN_DROPOUT=1,STACK_SUFFIX=do ... Ns1 20261016 (stack version s34_ns1do)
# Each step skips itself when its output exists, so a resubmission resumes.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run name}; SEED=${2:?seed}; V=s34_$(echo $RUN | tr A-Z a-z)${STACK_SUFFIX:-}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
D=research/rebuild/minimal
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
source $D/34_env.sh
export MINIMAL_RUN=step34/$RUN MINIMAL_SEED=$SEED
echo "=== train $(date)"
[ -f $R/model/step34/$RUN/model.pt ] || python $D/31_train.py
if [ "${MINIMAL_MAX_EPOCHS:-80}" -lt 80 ] && [ "${SMOKE_STACK:-0}" != 1 ]; then echo "=== smoke run: no stack"; exit 0; fi
echo "=== stack $(date)"
export STACK_VERSION=$V MINIMAL_STACK_LABEL=scf MINIMAL_ORDER=own_first MINIMAL_STACK_DIR=$R/gen \
       MINIMAL_STACK_FOREIGN=absent MINIMAL_COMPUTED_TOTAL=1 MINIMAL_STACK_CG_STRATIFIER=kg_lt \
       MINIMAL_STACK_OTHER_STRATIFIER=scorp_nonpass_inc MINIMAL_COMPUTED_EXTRA=1 \
       MINIMAL_STACK_PLAIN=$R/gen/step34/$RUN/gen_puf_with_net_worth_ownerlabel_absent_ownfirst.parquet
[ -f $R/gen/gen_meta_$V.json ] || python $D/03_stack.py
echo "=== done $(date)"
