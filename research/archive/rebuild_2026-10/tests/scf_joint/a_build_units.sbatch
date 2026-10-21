#!/bin/bash
#SBATCH --job-name=a_scfunits
#SBATCH --partition=day
#SBATCH --time=01:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/a_build_units_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/a_build_units_%j.err
# Test 2 piece A: SCF tax units. Usage: sbatch a_build_units.sbatch [waves...]
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
module load R/4.4.1-foss-2022b
Rscript research/rebuild/tests/scf_joint/a_build_units.R "$@"
