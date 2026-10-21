#!/bin/bash -l
#SBATCH --job-name=min23
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --exclude=a1125u14n01,a1125u05n01
#SBATCH --time=03:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/23_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/23_%x_%j.err
# Step 23 (23_NONFILERS.md): v4c with crude non-filers (N1), then with
# decision 18's latent amounts for the SCF's interest and dividends (N2).
#   sbatch -J n1 research/rebuild/minimal/23_run.sbatch N1     train (02_model.py), stack, tilt
#   sbatch -J n2 research/rebuild/minimal/23_run.sbatch N2     train (23_train_latent.py), stack, tilt
# Each step skips itself when its output exists, so a resubmission resumes.
# Stack: v4c's rule (03_stack.py: own_first, net worth last under the SCF
# label); N2 with no source label and borrowed slots hidden, not absent.
# Tilt: 07_baseline.py as v4c (136 IRS targets, eight wealth targets on the
# SCF's own units) with MINIMAL_NONFILERS=1.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?N1 or N2, or a replicate such as N1s2}; V=$(echo $RUN | tr A-Z a-z)
# optional second argument: the training seed (MINIMAL_SEED; default 20261016)
[ -n "${2:-}" ] && export MINIMAL_SEED=$2
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
D=research/rebuild/minimal
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
export MINIMAL_TABLE=E4N MINIMAL_RUN=$RUN MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V4
export STACK_VERSION=$V MINIMAL_STACK_LABEL=scf MINIMAL_ORDER=own_first MINIMAL_STACK_DIR=$R/gen
# step 25 alternatives, read from the run name: "flat" = steepness fixed at 0,
# "cut" = steepness fixed at 8, "fac" = dollar factor fixed at 1
[[ "$RUN" == *flat* ]] && export MINIMAL_LATENT_BETA_FIXED=0
[[ "$RUN" == *cut* ]] && export MINIMAL_LATENT_BETA_FIXED=8
[[ "$RUN" == *fac* ]] && export MINIMAL_LATENT_FACTOR_FIXED=1
if [[ "$RUN" == N2* ]]; then
  export MINIMAL_NO_SOURCE=1 MINIMAL_BORROWED_MASKED=1 MINIMAL_STACK_FOREIGN=mask
else
  export MINIMAL_STACK_FOREIGN=absent
fi
echo "=== train $(date)"
if [ ! -f $R/model/$RUN/model.pt ]; then
  if [[ "$RUN" == N2* ]]; then python $D/23_train_latent.py; else python $D/02_model.py; fi
fi
echo "=== stack $(date)"
[ -f $R/gen/gen_meta_$V.json ] || python $D/03_stack.py
echo "=== tilt $(date)"
# two threads: N1's tilt missed the solver's tolerance on a 4-core node (23_NONFILERS.md)
export OMP_NUM_THREADS=2 OPENBLAS_NUM_THREADS=2 MKL_NUM_THREADS=2
export TILT_TAG=$V WEALTH_UNITS=all WEALTH_COUNT_BASE=scf_units MINIMAL_NONFILERS=1
# if the solver misses its tolerance by rounding (23_NONFILERS.md), retry with one thread
[ -f $R/tilt/$V/tables_$V.md ] || python $D/07_baseline.py || OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 python $D/07_baseline.py
echo "=== done $(date)"
