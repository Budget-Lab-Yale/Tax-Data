#!/bin/bash
# Piece E development: rebuild the comparison from the dev outputs only.
set -euo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
export E_OUT=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e/out
python -u $CODE/e_compare.py
