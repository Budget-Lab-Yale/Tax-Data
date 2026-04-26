#!/bin/bash
#SBATCH --job-name=tu_diag
#SBATCH --partition=day
#SBATCH --time=00:15:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --output=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_taxunit_diag.out
#SBATCH --error=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/slurm_taxunit_diag.err

module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data

Rscript src/eda/scf_taxunit_construction_diagnostic.R
