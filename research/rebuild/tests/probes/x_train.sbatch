#!/bin/bash
# Probe X: copy of ../posttrain/h_train.sbatch (same settings) for full_v3x.
# Usage: sbatch h_train.sbatch <seed>
# Env: DATA (default $T2/data/full), OUT (default $T2/model/full), TAGBASE
#      (default full_w1), MAXH (wall-clock limit inside training, hours).
# Settings as test 2's w1 runs (b_pipeline.sbatch config joint_w1): base size
# (6 layers, width 256, 8 heads), any-order hiding of observed variables,
# source tag and numeric year, loss weighted by the population weight,
# every weight-kind variable dropped (never a model variable), early
# stopping after 15 epochs without improvement in validation loss.
# Writes $OUT/checkpoint_<TAGBASE>_seedNN.pt, train_log_*.csv, run_*.json,
# and the marker $OUT/DONE_seedNN when training finished without error.
# The dataset is first copied to $OUT/data (once; later seeds reuse it).
#SBATCH --job-name=x_train
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=05:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/x_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/x_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
POST=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
SEED=$1
NN=$(printf %02d "$SEED")
DATA=${DATA:-/nfs/roberts/scratch/pi_nrs36/jar335/probes/x/data/full_v3x}
OUT=${OUT:-/nfs/roberts/scratch/pi_nrs36/jar335/probes/x/model/full_v3x}
TAGBASE=${TAGBASE:-full_v3x_w1}
MAXH=${MAXH:-4.5}
mkdir -p "$OUT"
# Train from a copy of the dataset, so that the completions use exactly the
# data the model was trained on even if piece G rewrites $DATA later.
# h_complete.sbatch reads $OUT/data when it exists.
if [ ! -f "$OUT/data/DONE" ]; then
  mkdir -p "$OUT/data.tmp"
  cp -p "$DATA"/meta.json "$DATA"/tokens_*.npy "$DATA"/truth_*.npy "$DATA"/records_*.parquet \
        "$DATA"/quantiles.parquet "$DATA"/DONE "$OUT/data.tmp/" 2>/dev/null || true
  if [ ! -f "$OUT/data.tmp/DONE" ] || [ -n "$(find "$OUT/data.tmp" -newer "$OUT/data.tmp/DONE" -type f)" ]; then
    echo "dataset at $DATA is not final (DONE missing or older than a data file); stopping"; exit 1
  fi
  [ -d "$OUT/data" ] || mv "$OUT/data.tmp" "$OUT/data"
  rm -rf "$OUT/data.tmp"
fi
DATA=$OUT/data
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) seed $SEED data $DATA out $OUT"
test -f "$DATA/meta.json"
python -u $POST/h_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "${TAGBASE}_seed$NN" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours "$MAXH" "${@:2}"
echo "$(date) job ${SLURM_JOB_ID:-} data $DATA ($(cat "$DATA/DONE" 2>/dev/null | head -1))" > "$OUT/DONE_seed$NN"
echo "$(date) training done: $OUT/checkpoint_${TAGBASE}_seed$NN.pt"
