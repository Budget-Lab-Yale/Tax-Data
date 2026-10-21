#!/bin/bash
# Wait until piece A marks a dataset final ($S/data/<dataset>/DONE), then
# submit b_pipeline.sbatch for each config and training seeds 1-3.
# Usage: sbatch b_launch.sbatch <dataset> <config> [<config> ...]
# Job ids are appended to $S/logs/b_jobs.txt.
#SBATCH --job-name=b_launch
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --time=12:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_launch_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/logs/b_launch_%j.err
set -euo pipefail
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
DS=$1; shift
# Final = DONE exists and no token, record, meta or quantile file is newer.
AFTER=${AFTER:-0}   # epoch seconds; DONE must be newer than this
until [ -f "$S/data/$DS/DONE" ] && [ "$(stat -c %Y "$S/data/$DS/DONE")" -gt "$AFTER" ] && [ -z "$(find "$S/data/$DS" -newer "$S/data/$DS/DONE" -type f \( -name "*.npy" -o -name "records_*" -o -name meta.json -o -name quantiles.parquet \))" ]; do sleep 60; done
sleep 30   # let any final writes land
echo "$(date) $DS is final; submitting"
for CFG in "$@"; do
  for SEED in 1 2 3; do
    j=$(sbatch --parsable -J "b_${DS}_${CFG}_${SEED}" b_pipeline.sbatch "$DS" "$CFG" "$SEED")
    echo "$(date +%H:%M) $DS $CFG seed $SEED job $j" | tee -a "$S/logs/b_jobs.txt"
  done
done
