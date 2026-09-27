#!/bin/bash
#SBATCH --job-name=refit_score
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=48G
#SBATCH --time=02:00:00
#------------------------------------------------------------------------------
# 04_refit_score.R under Slurm, after the three run_refit_fit.sbatch jobs:
#   sbatch --dependency=afterany:<jobs> --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_refit_score.sbatch
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/filer_refit/04_refit_score.R
