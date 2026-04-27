#!/bin/bash
#SBATCH --job-name=p12_diag
#SBATCH --partition=day
#SBATCH --time=12:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=200G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_p12_then_diagnose.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_p12_then_diagnose.err

# Single-shot pipeline:
#   1. Run Phase 1 + Phase 2 of main.R, save puf_2022 snapshot.
#   2. Run diagnose_age_income.R against the new snapshot.
# Output paths are propagated via .last_phase12_output_path.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

set -e

echo "===== Step 1: main_phase12_snapshot.R ====="
Rscript src/main_phase12_snapshot.R

OUT=$(cat .last_phase12_output_path)
echo ""
echo "===== Step 2: diagnose_age_income.R against $OUT ====="
Rscript src/eda/diagnose_age_income.R "$OUT"

echo ""
echo "===== ALL DONE ====="
