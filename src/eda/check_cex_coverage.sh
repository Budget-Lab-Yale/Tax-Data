#!/bin/bash
#SBATCH --job-name=check_cex_coverage
#SBATCH --partition=day
#SBATCH --time=00:15:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=24G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_cex_cov.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_cex_cov.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/check_cex_coverage.R
