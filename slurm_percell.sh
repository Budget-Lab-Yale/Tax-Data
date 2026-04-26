#!/bin/bash
#SBATCH --job-name=wealth_percell
#SBATCH --partition=day
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_percell.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_percell.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

Rscript src/eda/wealth_percell_diagnostic.R
