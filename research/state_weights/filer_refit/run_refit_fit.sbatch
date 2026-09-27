#!/bin/bash
#SBATCH --job-name=refit_fit
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=48G
#SBATCH --time=12:00:00
#------------------------------------------------------------------------------
# One arm of the filer refit bake-off, TY2017 then TY2023 (whose prior is the
# arm's own TY2017 fit). Submit from the repo root with TAXDATA_SCRATCH exported:
#   sbatch --job-name=refit_fit-<arm> --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_refit_fit.sbatch <arm>
#------------------------------------------------------------------------------
set -euo pipefail
cd "$SLURM_SUBMIT_DIR"
: "${TAXDATA_SCRATCH:?TAXDATA_SCRATCH must be exported}"
arm="$1"

module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
for ty in 2017 2023; do
  Rscript research/state_weights/filer_refit/03_refit_fit.R "$arm" "$ty"
done
echo "REFIT_ARM_SUCCESS $arm"
