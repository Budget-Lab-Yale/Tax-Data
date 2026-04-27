#!/bin/bash
#SBATCH --job-name=tilt_syntax
#SBATCH --partition=day
#SBATCH --time=00:05:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_syntax_check.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_syntax_check.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript -e 'parse("src/imputations/wealth.R"); parse("src/imputations/tilt_solver.R"); parse("src/imputations/stage3_target_qc.R"); cat("syntax OK\n")'
