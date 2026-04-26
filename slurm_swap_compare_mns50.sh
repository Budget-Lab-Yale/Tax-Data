#!/bin/bash
#SBATCH --job-name=swap_mns50
#SBATCH --partition=day
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=16
#SBATCH --mem=96G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_swap_compare_mns50.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_swap_compare_mns50.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline
Rscript src/eda/swap_vs_tilt_compare.R "$OUTPUT_DIR" 50
