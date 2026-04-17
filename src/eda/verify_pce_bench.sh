#!/bin/bash
#SBATCH --job-name=verify_pce_bench
#SBATCH --partition=day
#SBATCH --time=00:20:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=24G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_verify_pce.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_verify_pce.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/verify_pce_bench.R
