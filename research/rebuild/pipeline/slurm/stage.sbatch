#!/bin/bash -l
#SBATCH --job-name=rebuild
#SBATCH --partition=day
#SBATCH --gres=gpu:0
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=08:00:00
# One step of the pipeline as a Slurm job, with Bouchet's modules: run.py with the arguments given, and the array task
# id as --year. run.py --slurm submits these; to run a stage by hand on a GPU, for example:
#   sbatch --partition=gpu_h200 --gres=gpu:1 research/rebuild/pipeline/slurm/stage.sbatch stage <folder> draw top --run y2019
set -euo pipefail
cd "${REBUILD_ROOT:-$SLURM_SUBMIT_DIR}"
source research/rebuild/pipeline/slurm/bouchet_env.sh
python3 research/rebuild/pipeline/run.py "$@" ${SLURM_ARRAY_TASK_ID:+--year $SLURM_ARRAY_TASK_ID}
