#!/bin/bash
#SBATCH --job-name=s27scf13
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step27/scf2013_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step27/scf2013_%j.err
# Step 27 (27_MAPPING_SPEC.md): the 2013 SCF wave (tax year 2012), built the
# same way as the other waves by 01_build_table.R's wave mode. Writes new files
# only: data/scf2013_units.csv.gz, data/checks_scf2013.csv,
# data/wave2013_scfp_rawincome.csv.
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
D=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data
if [ -f $D/scf2013_units.csv.gz ]; then echo "already built"; exit 0; fi
module load R/4.4.1-foss-2022b
Rscript research/rebuild/minimal/01_build_table.R --scf-wave 2013
