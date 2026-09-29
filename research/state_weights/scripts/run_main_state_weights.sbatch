#!/bin/bash
#SBATCH --job-name=state_weights
#SBATCH --partition=day
#SBATCH --cpus-per-task=8
#SBATCH --time=23:00:00
#------------------------------------------------------------------------------
# src/main_state_weights.R under Slurm: the State-Weights producer. Memory per
# submission -- the C_10_warmB panel over six years took ~39 GB in Phase 3, a
# two-year arm-B dry run far less. TAXDATA_SCRATCH must be exported; the
# SW_* / TAXDATA_VINTAGE overrides pass through the environment.
#   the D4 dry run:
#     SW_ARM=B SW_FIT_YEARS=2022:2023 SW_EMIT_YEARS=2022:2025 \
#     TAXDATA_VINTAGE=dryrun_B_2022_2023 TAXDATA_SCRATCH=... \
#     sbatch --mem=128G --output=$HOME/slurm-logs/%x-%j.out \
#       research/state_weights/scripts/run_main_state_weights.sbatch
#   production (the runscript's arm and years; every year 2017-2097 emitted):
#     TAXDATA_VINTAGE=<name> TAXDATA_SCRATCH=... sbatch --mem=240G ... (same file)
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
echo "SW_ARM=${SW_ARM:-<runscript>} SW_FIT_YEARS=${SW_FIT_YEARS:-<runscript>} SW_EMIT_YEARS=${SW_EMIT_YEARS:-2017:2097} TAXDATA_VINTAGE=${TAXDATA_VINTAGE:-<hour stamp>}"
Rscript src/main_state_weights.R
echo "STATE_WEIGHTS_SUCCESS"
