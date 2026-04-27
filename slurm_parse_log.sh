#!/bin/bash
#SBATCH --job-name=parse_log
#SBATCH --partition=day
#SBATCH --time=00:02:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_parse_log.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_parse_log.err
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/parse_tilt_log.R slurm_tilt_harness.out
