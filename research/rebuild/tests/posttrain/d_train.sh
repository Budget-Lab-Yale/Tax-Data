#!/bin/bash
# Piece D: one training run. Usage: d_train.sh <output folder> <d_tune.py train arguments...>
set -euo pipefail
OUT=$1; shift
python -u d_tune.py train --out-dir "$OUT" "$@"
