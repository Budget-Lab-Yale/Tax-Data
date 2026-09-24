#!/bin/bash
#SBATCH --job-name=prep_fit_inputs
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#------------------------------------------------------------------------------
# prep_fit_inputs.R, one tax year per array task (Phase 3 builds 2017-2023).
# Submit from the repo root with TAXDATA_SCRATCH exported:
#   sbatch --array=2017-2023 --output=$HOME/slurm-logs/%x-%A_%a.out \
#     research/state_weights/scripts/run_prep_fit_inputs.sbatch \
#     <tax_data_vintage> <baseline_detail_dir>
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"

module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/scripts/prep_fit_inputs.R "$SLURM_ARRAY_TASK_ID" "$1" "$2"

echo "PREP_FIT_INPUTS_SUCCESS"
