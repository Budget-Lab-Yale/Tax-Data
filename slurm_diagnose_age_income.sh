#!/bin/bash
#SBATCH --job-name=diag_age
#SBATCH --partition=day
#SBATCH --time=00:20:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_diagnose_age_income.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_diagnose_age_income.err

# Pass output dir as $1 (so we can chain after main_phase12).
# Defaults to the most recent vintage if not given.
OUTPUT_DIR=${1:-}
if [ -z "$OUTPUT_DIR" ]; then
  echo "ERROR: pass output_dir as arg 1"
  exit 1
fi

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/diagnose_age_income.R "$OUTPUT_DIR"
