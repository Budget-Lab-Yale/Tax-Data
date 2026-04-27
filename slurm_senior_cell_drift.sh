#!/bin/bash
#SBATCH --job-name=senior_cell_drift
#SBATCH --partition=day
#SBATCH --time=00:10:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=12G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_senior_cell_drift.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_senior_cell_drift.err
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/senior_cell_drift.R
