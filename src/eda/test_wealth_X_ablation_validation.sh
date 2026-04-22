#!/bin/bash
#SBATCH --job-name=wealth_Xabl_v
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_wealth_Xabl_validation.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_wealth_Xabl_validation.err

# Extended validation metrics aligned to Tax-Simulator integration goals.
# Depends on wealth_ablation.rds from postproc job.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/test_wealth_X_ablation_validation.R
