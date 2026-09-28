#!/bin/bash
#SBATCH --job-name=table43
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=01:30:00
#------------------------------------------------------------------------------
# 08_table43.R under Slurm (reads the v2 output, TY2017-2023):
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_table43.sbatch
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/filer_refit/08_table43.R
