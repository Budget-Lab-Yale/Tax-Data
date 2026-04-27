#!/bin/bash
#SBATCH --job-name=tilt_harness
#SBATCH --partition=day
#SBATCH --time=08:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=200G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_tilt_harness.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_tilt_harness.err

# End-to-end run of run_wealth_imputation under the new tilt-based Stage 3.
# Loads the cached per-cell DRFs (no retrain). Uses tax_units_2022.csv from
# a prior baseline as the PUF input -- wealth_harness.R nulls the wealth
# columns and reconstructs everything else.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline

# No "retrain" arg -> estimate_models = 0 -> reuses cached forests.
Rscript src/eda/wealth_harness.R "$OUTPUT_DIR"
