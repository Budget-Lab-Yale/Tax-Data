#!/bin/bash
#SBATCH --job-name=tilt_smoke
#SBATCH --partition=day
#SBATCH --time=00:45:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_tilt_smoke.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_tilt_smoke.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline

# Default: pct99.9to100 (smallest cell). Override with TILT_SMOKE_CI env.
Rscript src/eda/tilt_single_cell_smoke.R "$OUTPUT_DIR"
