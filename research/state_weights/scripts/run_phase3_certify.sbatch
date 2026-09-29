#!/bin/bash
#SBATCH --job-name=phase3_certify
#SBATCH --partition=day
#SBATCH --cpus-per-task=8
#SBATCH --time=23:00:00
#------------------------------------------------------------------------------
# Phase 4 P3 certification: run phase3_fit.R (now a driver over the chains in
# src/nonfilers/sw_fit.R) into {TAXDATA_SCRATCH}/phase3_cert/ and compare with
# the S39 fits in {TAXDATA_SCRATCH}/phase3/. Same arguments as
# run_phase3_fit.sbatch; the C arm reads its warm start from phase3_cert/B, so
# submit C with --dependency=afterok:<B job>.
#   sbatch --mem=128G --output=$HOME/slurm-logs/%x-B-%j.out research/state_weights/scripts/run_phase3_certify.sbatch B
#   sbatch --mem=240G --dependency=afterok:<B> --output=$HOME/slurm-logs/%x-C10B-%j.out research/state_weights/scripts/run_phase3_certify.sbatch C 10 B
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
export PHASE3_OUT=phase3_cert
Rscript research/state_weights/scripts/phase3_fit.R "$@"
arm_dir="$1"; [ "$1" = "C" ] && arm_dir="C_$2$([ "${3:-}" = "B" ] && echo _warmB)"
Rscript research/state_weights/scripts/phase3_compare_fits.R "$TAXDATA_SCRATCH/phase3/$arm_dir" "$TAXDATA_SCRATCH/phase3_cert/$arm_dir"
echo "PHASE3_CERTIFY_SUCCESS $*"
