#!/bin/bash
#SBATCH --job-name=tu_perms
#SBATCH --partition=day
#SBATCH --time=00:30:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_taxunit_permutations.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_taxunit_permutations.err

# Usage:
#   sbatch slurm_taxunit_permutations.sh [OUTPUT_DIR]
# OUTPUT_DIR defaults to the most recent shared baseline if not passed.

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

OUTPUT_DIR=${1:-/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Data/v1/2025060311/baseline}
Rscript src/eda/scf_taxunit_permutations.R "$OUTPUT_DIR"
