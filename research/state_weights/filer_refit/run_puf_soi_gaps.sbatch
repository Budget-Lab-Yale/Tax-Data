#!/bin/bash
#SBATCH --job-name=puf_soi_gaps
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#------------------------------------------------------------------------------
# 06_puf_soi_gaps.R under Slurm (Phase 1 through create_2017_puf.R):
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_puf_soi_gaps.sbatch
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/filer_refit/06_puf_soi_gaps.R
