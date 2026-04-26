#!/bin/bash
#SBATCH --job-name=joint_sweep
#SBATCH --partition=day
#SBATCH --time=00:45:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_joint_sweep.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_joint_sweep.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=/nfs/roberts/scratch/pi_nrs36/jar335/jar335/model_data/Tax-Data/v1/2026042315/baseline
Rscript src/eda/joint_objective_sweep.R "$OUTPUT_DIR"
