#!/bin/bash
#SBATCH --job-name=hrn_idv100
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_harness_idv100.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_harness_idv100.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline

# NO retrain arg — wealth.R changes are PUF-side only (int_div threshold
# at $100 before has_/pctile_ construction). SCF features unchanged; the
# existing 8 cached per-cell forests apply directly at inference.
Rscript src/eda/wealth_harness.R "$OUTPUT_DIR"
