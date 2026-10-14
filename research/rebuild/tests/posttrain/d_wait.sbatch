#!/bin/bash
# Piece D: the waiter that launches the full-model jobs (d_full_chain.sh).
#SBATCH --job-name=d_wait
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --time=23:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/d_wait_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/d_wait_%j.err
S=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/d_full_chain_${SLURM_JOB_ID}.sh
cp /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain/d_full_chain.sh $S
bash $S
