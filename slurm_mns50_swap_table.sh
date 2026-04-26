#!/bin/bash
#SBATCH --job-name=mns50_tbl
#SBATCH --partition=day
#SBATCH --time=00:25:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_mns50_swap_table.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_mns50_swap_table.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline
Rscript src/eda/mns50_swap_table.R "$OUTPUT_DIR"
