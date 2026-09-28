#!/bin/bash
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#------------------------------------------------------------------------------
# Run one research Rscript under Slurm. Job name, memory and time come from
# the command line (sbatch flags override these directives); each script's
# header carries its own line. Environment variables (e.g. BUCKET_MODE) pass
# through: sbatch exports the submitting shell's environment.
#   sbatch --job-name=<name> --mem=<G> --time=<HH:MM:SS> \
#     --output=$HOME/slurm-logs/%x-%j.out \
#     research/state_weights/filer_refit/run_rscript.sbatch <script.R>
#------------------------------------------------------------------------------
set -euo pipefail
[ $# -eq 1 ] || { echo "usage: sbatch ... run_rscript.sbatch <script.R>" >&2; exit 2; }
cd "$SLURM_SUBMIT_DIR"
module load R/4.4.2-gfbf-2024a
echo "commit: $(git rev-parse --short HEAD)  (working tree: $(git status --porcelain | wc -l) uncommitted)"
Rscript "$1"
