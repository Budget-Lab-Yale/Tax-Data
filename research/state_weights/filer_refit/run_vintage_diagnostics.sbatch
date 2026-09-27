#!/bin/bash
#SBATCH --job-name=refit_vintage_diag
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=48G
#SBATCH --time=02:00:00
#------------------------------------------------------------------------------
# 05_vintage_diagnostics.R under Slurm, once the AFTER vintage is built:
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_vintage_diagnostics.sbatch
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/filer_refit/05_vintage_diagnostics.R
