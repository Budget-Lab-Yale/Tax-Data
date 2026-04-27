#!/bin/bash
#SBATCH --job-name=dina_ageprim
#SBATCH --partition=day
#SBATCH --time=00:08:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=12G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_dina_ageprim_probe.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_dina_ageprim_probe.err
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
Rscript src/eda/dina_ageprim_probe.R
