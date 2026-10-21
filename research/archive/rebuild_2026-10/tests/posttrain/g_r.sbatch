#!/bin/bash
# Test 3, piece G: run an R script on a CPU node. Usage: sbatch g_r.sbatch script.R [args]
#SBATCH --job-name=g_r
#SBATCH --partition=day
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/g_r_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/g_r_%j.err
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
module load R/4.4.1-foss-2022b
Rscript "$@"
