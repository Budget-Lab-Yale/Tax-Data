#!/bin/bash -l
#SBATCH --job-name=s27run
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --exclude=a1125u14n01,a1125u05n01
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step27/run_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step27/run_%x_%j.err
# Step 27 (27_MAPPING_SPEC.md, 27_STATUS.md): one real run on table E27, GPU
# part: train, score (27_score.py, latent runs only), stack (03_stack.py).
#   sbatch -J R2s1 research/rebuild/minimal/27_run.sbatch R2s1 20261016
# Run names: R0* = today's rule, label on (02_model.py); R1* = the new method,
# 2016 wave only; R2* = the new method, all waves (27_train.py). Second
# argument: the training seed. Each step skips itself when its output exists,
# so a resubmission resumes.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run name}; SEED=${2:?seed}; V=s27_$(echo $RUN | tr A-Z a-z)
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
D=research/rebuild/minimal
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
export MINIMAL_TABLE=${MINIMAL_TABLE:-E27} MINIMAL_RUN=step27/$RUN MINIMAL_SEED=$SEED MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V27
[[ "$RUN" == R1* ]] && export MINIMAL_SCF_YEARS=2015
# runs named *AB*: both families in training (27_train.py MINIMAL_FAMILY_B=1)
[[ "$RUN" == *AB* ]] && export MINIMAL_FAMILY_B=1
# runs named R2W*: the dollar factor searched over 0.2 to 5 instead of 0.5 to 2
[[ "$RUN" == R2W* ]] && export MINIMAL_FACTOR_GRID=0.2,5.0
# runs named R2P*: the fill-ins drawn from the PUF-labelled belief (diagnostic)
[[ "$RUN" == R2P* ]] && export MINIMAL_FILL_FROM=puf
# runs with J in the name: the settings fit on the chance of exactly what was reported
[[ "$RUN" == *J* ]] && export MINIMAL_SETTINGS_FIT=joint
echo "=== train $(date)"
if [ ! -f $R/model/step27/$RUN/model.pt ]; then
  if [[ "$RUN" == R0* ]]; then python $D/02_model.py; else python $D/27_train.py; fi
fi
if [[ "$RUN" != R0* ]] && [ ! -f $R/model/step27/$RUN/score.json ]; then
  echo "=== score $(date)"
  python $D/27_score.py || echo "SCORE FAILED (the stack and tilt go on)"
fi
if [ "${MINIMAL_TABLE}" != E27 ]; then echo "=== not E27: no stack"; exit 0; fi
echo "=== stack $(date)"
export STACK_VERSION=$V MINIMAL_STACK_LABEL=scf MINIMAL_ORDER=own_first MINIMAL_STACK_DIR=$R/gen \
       MINIMAL_STACK_FOREIGN=absent MINIMAL_COMPUTED_TOTAL=1 MINIMAL_STACK_CG_STRATIFIER=kg_lt
[ -f $R/gen/gen_meta_$V.json ] || python $D/03_stack.py
echo "=== done $(date)"
