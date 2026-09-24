#!/bin/bash
#SBATCH --job-name=phase3_fit
#SBATCH --partition=day
#SBATCH --cpus-per-task=8
#SBATCH --time=23:00:00
#------------------------------------------------------------------------------
# phase3_fit.R under Slurm. Arm A runs as an array over tax years (the task id
# is the year); B and C are single jobs. Memory is set per submission: a
# one-year fit follows the config-7 golden check (128G), arm C holds six years.
# Submit from the repo root with TAXDATA_SCRATCH exported, e.g.
#   sbatch --array=2017-2023 --mem=128G --output=$HOME/slurm-logs/%x-A-%A_%a.out \
#     research/state_weights/scripts/run_phase3_fit.sbatch A
#   sbatch --mem=128G --output=$HOME/slurm-logs/%x-B-%j.out \
#     research/state_weights/scripts/run_phase3_fit.sbatch B
#   sbatch --mem=240G --output=$HOME/slurm-logs/%x-C10-%j.out \
#     research/state_weights/scripts/run_phase3_fit.sbatch C 10
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"

module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
if [ "$1" = "A" ]; then
  Rscript research/state_weights/scripts/phase3_fit.R A "$SLURM_ARRAY_TASK_ID"
else
  Rscript research/state_weights/scripts/phase3_fit.R "$@"
fi

echo "PHASE3_FIT_SUCCESS"
