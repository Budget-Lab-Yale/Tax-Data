#!/bin/bash
#SBATCH --job-name=tilt_v4
#SBATCH --partition=day
#SBATCH --time=02:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=200G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_tilt_v4.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_tilt_v4.err

# v4: tilt with lambda_max=5 (capped), use_fallback_uniform=FALSE
# (no Step B uniform smear), against the new puf_2022 snapshot.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
OUTPUT_DIR=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026042712/baseline
Rscript src/eda/wealth_harness.R "$OUTPUT_DIR"
