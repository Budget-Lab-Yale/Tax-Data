#!/bin/bash
#SBATCH --job-name=pscf
#SBATCH -p day -t 04:00:00 --cpus-per-task=2 --mem=24G
#SBATCH --array=0-39
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/logs/test_%A_%a.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/logs/test_%A_%a.err
# Array index -> (target wave, split). 4 target waves x 10 splits.
module load R/4.4.1-foss-2022b
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/pooled_scf
TARGETS=(2022 2019 2016 2013)
T=${TARGETS[$((SLURM_ARRAY_TASK_ID / 10))]}
S=$((SLURM_ARRAY_TASK_ID % 10 + 1))
Rscript 01_pooled_scf_test.R $T $S
