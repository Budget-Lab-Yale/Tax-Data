#!/bin/bash
#SBATCH --job-name=nf_anchor_bakeoff
#SBATCH --partition=day
#SBATCH --cpus-per-task=8
#SBATCH --mem=128G
#SBATCH --time=06:00:00
#------------------------------------------------------------------------------
# Phase 2 non-filer anchor bake-off (nonfiler_anchor_bakeoff.R), one tax year
# per array task. 128G and 8 CPUs are what the config-7 golden check used for
# the same filer fit (Slurm 27243481: 1h10m).
# Submit from the repo root with TAXDATA_SCRATCH exported:
#   sbatch --array=2022,2023 --output=$HOME/slurm-logs/%x-%A_%a.out \
#     research/state_weights/scripts/run_nonfiler_anchor_bakeoff.sbatch \
#     <tax_data_vintage> <baseline_detail_dir>
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"

module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript research/state_weights/scripts/nonfiler_anchor_bakeoff.R \
  "$SLURM_ARRAY_TASK_ID" "$1" "$2"

echo "NONFILER_ANCHOR_BAKEOFF_SUCCESS"
