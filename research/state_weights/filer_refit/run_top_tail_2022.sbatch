#!/bin/bash
#SBATCH --job-name=top_tail_2022
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=24G
#SBATCH --time=01:00:00
#------------------------------------------------------------------------------
# 07_top_tail_2022.R under Slurm (reads the v2 TY2022 output):
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_top_tail_2022.sbatch
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/filer_refit/07_top_tail_2022.R
