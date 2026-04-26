#!/bin/bash
#SBATCH --job-name=nf_idv
#SBATCH --partition=day
#SBATCH --time=00:15:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_nonfiler_intdiv.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_nonfiler_intdiv.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2026042312/baseline
Rscript src/eda/nonfiler_intdiv_probe.R "$OUTPUT_DIR"
