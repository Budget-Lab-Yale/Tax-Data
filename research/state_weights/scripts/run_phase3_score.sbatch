#!/bin/bash
#SBATCH --job-name=phase3_score
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=64G
#SBATCH --time=03:00:00
#------------------------------------------------------------------------------
# phase3_score.R under Slurm. Submit from the repo root with TAXDATA_SCRATCH
# exported:
#   sbatch --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/scripts/run_phase3_score.sbatch [arm ...]
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"

module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/scripts/phase3_score.R "$@"

echo "PHASE3_SCORE_SUCCESS"
