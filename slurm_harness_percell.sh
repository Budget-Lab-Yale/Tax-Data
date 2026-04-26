#!/bin/bash
#SBATCH --job-name=harness_pc
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_harness_percell.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_harness_percell.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline

# No "retrain" arg — reuses the 8 cached wealth_percell_* forests
# (identical hyperparameters to what wealth.R now trains).
Rscript src/eda/wealth_harness.R "$OUTPUT_DIR"
