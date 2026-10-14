#!/bin/bash
# Train one model and write its completions (piece B, SPEC.md section B).
# Usage: sbatch b_pipeline.sbatch <dataset> <config> <training seed> [extra b_train.py args]
#   dataset: expA_clean, expA_noise, expA_recat, expA_unrep, expB (folder in $S/data)
#   config:  joint | half2 | scfonly, optionally with suffixes _small or _large
#            (_large: 8 layers, width 512, 8 heads), _w1, _wsqrt, _nowt
#            (4 layers, width 128, 4 heads instead of 6, 256, 8)
#   joint   trains on every source; half2 and scfonly train on source SCF only.
# Completions written (5 draws per record, plain and weight-corrected, K = 20):
#   expA_*: half 1 (the test split). joint: as its own source (tag <config>)
#           and as source SCF (tag <config>_asSCF). half2: as source SCF.
#   expB:   SCF test units with wealth and weight hidden (tag <config>_scf);
#           joint also completes the PUF test records at 2015, as source PUF
#           (<config>_puf) and presented as source SCF (<config>_puf_asSCF).
#SBATCH --job-name=y_pipe
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --time=08:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/y_pipe_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/y_pipe_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
DS=$1; CFG=$2; SEED=$3; shift 3
D=${DATA_ROOT:-$S/data}/$DS
M=${MODEL_ROOT:-$S/model}/$DS
NN=$(printf %02d "$SEED")
TAG=${CFG}_seed$NN
BASE=${CFG%%_*}
TRAIN_ARGS=()
case $BASE in
  joint) ;;
  half2|scfonly) TRAIN_ARGS+=(--sources SCF) ;;
  *) echo "unknown config $CFG"; exit 1 ;;
esac
if [[ $CFG == *_small* ]]; then TRAIN_ARGS+=(--d-model 128 --layers 4 --heads 4); fi
# _large: test 1's large size (8 layers, width 512, 8 heads).
if [[ $CFG == *_large* ]]; then TRAIN_ARGS+=(--d-model 512 --layers 8 --heads 8); fi
# Survey weight not a model variable (John, 9 October), handled in training:
#   _nowt  weight dropped, loss unweighted (control)
#   _w1    weight dropped, loss weighted by the population weight
#   _wsqrt weight dropped, loss weighted by its square root
# These complete with K = 1 (plain completion only; no weight correction).
COMP_EXTRA=()
if [[ $CFG == *_nowt* ]]; then TRAIN_ARGS+=(--drop-vars scf_weight); COMP_EXTRA+=(--k 1); fi
if [[ $CFG == *_w1* ]]; then TRAIN_ARGS+=(--drop-vars scf_weight --loss-weight survey); COMP_EXTRA+=(--k 1); fi
if [[ $CFG == *_wsqrt* ]]; then TRAIN_ARGS+=(--drop-vars scf_weight --loss-weight sqrt); COMP_EXTRA+=(--k 1); fi
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "dataset $DS config $CFG seed $SEED data $D out $M extra: $*"
test -f "$D/meta.json"
if [[ "${SKIP_TRAIN:-0}" == 1 ]]; then
  echo "SKIP_TRAIN=1: using the existing checkpoint"
else
python -u b_train.py --data-dir "$D" --out-dir "$M" --tag "$TAG" --seed "$SEED" \
    "${TRAIN_ARGS[@]}" "$@"
fi
CK=$M/checkpoint_$TAG.pt
CSEED=$((20261011 + SEED))
comp() { python -u b_complete.py --data-dir "$D" --out-dir "$M" --checkpoint "$CK" \
           --train-seed "$SEED" --seed "$CSEED" "${COMP_EXTRA[@]}" "$@"; }
case $DS in
  expA*)
    if [[ $BASE == joint ]]; then
      comp --tag "$CFG"
      comp --tag "${CFG}_asSCF" --complete-as-source SCF
    else
      comp --tag "$CFG" --complete-as-source SCF
    fi ;;
  expB_multi*)
    # SCF test units of 2013 and 2016 (the same units as expB), and of 2019
    # and 2022 separately; the joint model also completes the PUF test
    # records presented as SCF, decoded at 2015.
    comp --tag "${CFG}_scf" --target-sources SCF --target-years 2013,2016
    comp --tag "${CFG}_scf1922" --target-sources SCF --target-years 2019,2022
    if [[ $BASE == joint ]]; then
      comp --tag "${CFG}_puf_asSCF" --target-sources PUF --decode-year 2015 --complete-as-source SCF
    fi ;;
  expB*)
    comp --tag "${CFG}_scf" --target-sources SCF
    if [[ $BASE == joint ]]; then
      comp --tag "${CFG}_puf" --target-sources PUF --decode-year 2015
      comp --tag "${CFG}_puf_asSCF" --target-sources PUF --decode-year 2015 --complete-as-source SCF
    fi ;;
  *)  # fabricated or other datasets: complete the whole test split
    comp --tag "$CFG" ;;
esac
echo "pipeline done: $DS $CFG seed $SEED"
