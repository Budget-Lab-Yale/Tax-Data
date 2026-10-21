#!/bin/bash
#SBATCH --job-name=f_wait
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --time=23:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/f_wait_%j.out
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
./f_wait.sh "$@"
