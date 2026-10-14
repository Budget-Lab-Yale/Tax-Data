#!/bin/bash
# Probe S: income components by AGI group for S_REPORT.md (after s_collect.sbatch).
#SBATCH --job-name=s_final
#SBATCH --partition=day
#SBATCH --cpus-per-task=2
#SBATCH --mem=16G
#SBATCH --time=00:20:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_final_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_final_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 Arrow/17.0.0-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
C=/nfs/roberts/scratch/pi_nrs36/jar335/probes/s/compare_all.json
R=base,agi10,agi25,agi50,agi100,rnd50,rnd100,two50
for o in agifirst demolast givenagi; do echo "### $o"; python s_groups.py $C $o $R; echo; done
