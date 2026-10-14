#!/bin/bash
#SBATCH --job-name=scfexpl
#SBATCH -p day -t 00:30:00 --cpus-per-task=2 --mem=24G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/logs/explore_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/logs/explore_%j.err
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/pooled_scf
Rscript 00_explore.R
