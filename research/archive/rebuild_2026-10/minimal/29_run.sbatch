#!/bin/bash -l
#SBATCH --job-name=s29run
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --exclude=a1125u14n01,a1125u05n01
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step29/run_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step29/run_%x_%j.err
# Step 29 (29_STATUS.md): one run, GPU part: train, then on table E27 the
# stack (03_stack.py), on the fake table F29 the fake check (29_fake.py check).
#   sbatch -J C1s1 research/rebuild/minimal/29_run.sbatch C1s1 20261016
#   sbatch -J F29C3s1 --export=ALL,MINIMAL_TABLE=F29 research/rebuild/minimal/29_run.sbatch F29C3s1 20261016
# Run names: *C1* rule 1 (one S-curve), *C2* rule 2 (S-curve middle per cell),
# *C3* rule 3 (per-cell counts and slices), *C4* rule 3 applied wave by wave
# (the 2016 cell shares of hidden holders pinned in every wave), *C5* as C4 and
# *C6* as C3 with hidden holders spread over slices smallest first, all with the dollar factor fixed
# at 1 (29_train.py); *R0* today's rule (02_model.py); *R2J* step 27's R2J
# method, learned dollar factor (27_train.py, joint fit). Each step skips
# itself when its output exists, so a resubmission resumes.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run name}; SEED=${2:?seed}; V=s29_$(echo $RUN | tr A-Z a-z)
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
D=research/rebuild/minimal
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
export MINIMAL_TABLE=${MINIMAL_TABLE:-E27} MINIMAL_RUN=step29/$RUN MINIMAL_SEED=$SEED MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V27
TRAIN=""
case "$RUN" in
  *C1*) TRAIN=29_train.py; export MINIMAL_RULE=curve MINIMAL_LATENT_FACTOR_FIXED=1 ;;
  *C2*) TRAIN=29_train.py; export MINIMAL_RULE=cellcurve MINIMAL_LATENT_FACTOR_FIXED=1 ;;
  *C3*) TRAIN=29_train.py; export MINIMAL_RULE=cellcount MINIMAL_LATENT_FACTOR_FIXED=1 ;;
  *C4*) TRAIN=29_train.py; export MINIMAL_RULE=cellcount_wave MINIMAL_LATENT_FACTOR_FIXED=1 ;;
  *C5*) TRAIN=29_train.py; export MINIMAL_RULE=cellcount_wave MINIMAL_RULE3_ALLOC=smallest_first MINIMAL_LATENT_FACTOR_FIXED=1 ;;
  *C6*) TRAIN=29_train.py; export MINIMAL_RULE=cellcount MINIMAL_RULE3_ALLOC=smallest_first MINIMAL_LATENT_FACTOR_FIXED=1 ;;
  *R2J*) TRAIN=27_train.py; export MINIMAL_SETTINGS_FIT=joint ;;
  *R0*) TRAIN=02_model.py ;;
  *) echo "unknown run $RUN"; exit 1 ;;
esac
echo "=== train ($TRAIN) $(date)"
[ -f $R/model/step29/$RUN/model.pt ] || python $D/$TRAIN
if [ "${MINIMAL_TABLE}" = F29 ]; then
  echo "=== fake check $(date)"
  [ -f $R/step29/fake/$RUN.json ] || python $D/29_fake.py check
  echo "=== done $(date)"; exit 0
fi
echo "=== stack $(date)"
export STACK_VERSION=$V MINIMAL_STACK_LABEL=scf MINIMAL_ORDER=own_first MINIMAL_STACK_DIR=$R/gen \
       MINIMAL_STACK_FOREIGN=absent MINIMAL_COMPUTED_TOTAL=1 MINIMAL_STACK_CG_STRATIFIER=kg_lt
[ -f $R/gen/gen_meta_$V.json ] || python $D/03_stack.py
echo "=== done $(date)"
