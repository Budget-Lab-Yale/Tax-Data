#!/bin/bash
#SBATCH --job-name=min09
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/09_run_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/09_run_%j.err
# One run of the bridge-variable grid (09_BRIDGE_EXPERIMENTS.md): train
# (02_model.py), score (02_evaluate.py), generate (03_generate.py).
#   sbatch --export=ALL[,MINIMAL_FLAG=1,...] research/rebuild/minimal/09_run_one.sbatch <T> [TABLE_TAG]
# <T> is the run tag (E0 ... E10): model in model/<T>/, files in gen/<T>/.
# TABLE_TAG, if given, trains on data/stacked_<TABLE_TAG>.parquet; without
# it, the v2 table. Flags (MINIMAL_NO_LABEL_SHARED, MINIMAL_COARSEN, ...)
# come in through the environment.
# On success touches markers/RUN_<T>_DONE; on failure writes
# markers/RUN_<T>_FAILED holding the step that failed and the log path.
set -uo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
T=${1:?run tag required}
export MINIMAL_RUN=$T
if [ -n "${2:-}" ]; then export MINIMAL_TABLE=$2; fi
CODE=${MINIMAL_CODE_DIR:-research/rebuild/minimal}   # another folder for tests only
LOG=$R/logs/09_run_${SLURM_JOB_ID}.out
mkdir -p $R/markers
rm -f $R/markers/RUN_${T}_DONE $R/markers/RUN_${T}_FAILED
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
echo "run $T  table ${MINIMAL_TABLE:-v2}  $(date)"
env | grep '^MINIMAL_' | sort
nvidia-smi --query-gpu=name --format=csv,noheader
for step in 02_model.py 02_evaluate.py 03_generate.py; do
  echo "== $step $(date)"
  if ! python $CODE/$step; then
    echo "$step failed; log $LOG (and ${LOG%.out}.err)" > $R/markers/RUN_${T}_FAILED
    echo "FAILED at $step"
    exit 1
  fi
done
touch $R/markers/RUN_${T}_DONE
echo "done $(date)"
