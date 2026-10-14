#!/bin/bash
# Probe S: submit one training run and, after it, its evaluation and two
# generations (AGI first; demographics last).
# Usage: bash s_submit.sh <variant> <seed> [extra s_train.py args ...]
set -euo pipefail
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
cd "$HERE"
VAR=$1; SEED=$2; NN=$(printf %02d "$SEED")
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/s/$VAR
CK=$OUT/checkpoint_${VAR}_seed$NN.pt
J=$(sbatch --parsable s_train.sbatch "$@")
E=$(sbatch --parsable --dependency=afterok:$J s_eval.sbatch "$CK" "$OUT" "${VAR}_s$SEED" "$SEED")
G1=$(sbatch --parsable --dependency=afterok:$J s_gen.sbatch "$CK" "$OUT" "agifirst4x_${VAR}_s$SEED" agifirst "$SEED" $((20261150 + 100 * SEED)))
G2=$(sbatch --parsable --dependency=afterok:$J s_gen.sbatch "$CK" "$OUT" "demolast4x_${VAR}_s$SEED" demolast "$SEED" $((20261140 + 100 * SEED)))
echo "$VAR seed $SEED: train $J eval $E gen-agifirst $G1 gen-demolast $G2 | ${*:3}"
