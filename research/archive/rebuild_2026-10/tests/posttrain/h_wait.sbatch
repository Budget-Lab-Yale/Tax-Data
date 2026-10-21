#!/bin/bash
# Piece H: wait until piece G marks the `full` dataset ready, then exit 0 so
# that the training jobs that depend on this job start.
# Ready = $DATA/DONE exists and no token, record, meta or quantile file is
# newer than it (b_launch.sbatch's rule from test 2).
# Usage: sbatch h_wait.sbatch    (env DATA overrides the dataset folder)
#SBATCH --job-name=h_wait
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --time=12:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/h_wait_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/h_wait_%j.err
set -euo pipefail
DATA=${DATA:-/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full}
echo "$(date) waiting for $DATA/DONE"
ready() {
  [ -f "$DATA/DONE" ] && [ -f "$DATA/meta.json" ] && [ -f "$DATA/tokens_train.npy" ] &&
  [ -z "$(find "$DATA" -maxdepth 1 -newer "$DATA/DONE" -type f \( -name "*.npy" -o -name "records_*" -o -name meta.json -o -name quantiles.parquet \))" ]
}
until ready; do sleep 30; done
sleep 60   # let any final writes land
ready || { echo "files changed after DONE; waiting again"; until ready; do sleep 30; done; sleep 60; }
echo "$(date) dataset ready: $(cat "$DATA/DONE" | head -3)"
ls -la "$DATA"
