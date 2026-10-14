#!/bin/bash
#SBATCH --job-name=a_scfpdol
#SBATCH --partition=day
#SBATCH --time=00:20:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/a_scfp_dollars_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/a_scfp_dollars_%j.err
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
module load R/4.4.1-foss-2022b
Rscript research/rebuild/tests/scf_joint/a_scfp_dollars.R
