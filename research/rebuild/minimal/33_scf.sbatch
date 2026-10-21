#!/bin/bash
#SBATCH --job-name=s33scf
#SBATCH --partition=day,week
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step33/scf_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step33/scf_%x_%j.err
# Step 33 (33_STATUS.md): four more SCF income items split into tax units for
# one wave (33_build_scf.R). Usage: sbatch -J scf2016 research/rebuild/minimal/33_scf.sbatch 2016
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
W=${1:?wave}
D=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data
if [ -f $D/scf${W}_units33.csv.gz ]; then echo "already built"; exit 0; fi
module load R/4.4.1-foss-2022b
Rscript research/rebuild/minimal/33_build_scf.R $W
