#!/bin/bash
#SBATCH --job-name=wealth_dim2
#SBATCH --partition=day
#SBATCH --time=00:45:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_dim2.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_dim2.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

Rscript src/eda/wealth_dim2_diagnostic.R
