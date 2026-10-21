#!/bin/bash
# Piece C: wait until every marker exists (each may be a glob pattern, e.g.
# DONE_*core*), then submit one Slurm job.
# Runs as a small CPU job itself, so the chain survives the driving session.
#   sbatch c_chain.sh "<marker1>,<marker2>,..." <sbatch arguments...>
# Example:
#   sbatch c_chain.sh "$T2/model/full/DONE_seed01,$P/full/a/DONE_P_core" \
#       -p gpu_b200,gpu_h200 --time=04:00:00 c_gpu.sbatch c_fit.py --arm P_core ...
#SBATCH --job-name=c_chain
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --time=23:59:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/c_chain_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/c_chain_%j.err
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
markers=$1; shift
IFS=',' read -ra M <<< "$markers"
echo "$(date) waiting for: ${M[*]}"
while true; do
  missing=0
  for m in "${M[@]}"; do compgen -G "$m" > /dev/null || missing=1; done
  [ $missing -eq 0 ] && break
  sleep 120
done
echo "$(date) all markers present; submitting: sbatch $*"
sbatch "$@"
