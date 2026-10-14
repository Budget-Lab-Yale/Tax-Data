#!/bin/bash
# Piece E development: fabricated H files, then e_full.py on them.
set -euo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
export PYTHONPATH=$CODE/../scf_joint:${PYTHONPATH:-}
python -u $CODE/e_dev_fake_full.py
export E_OUT=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e/out
export E_FULL_MODEL=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e/full_model
touch -d '-5 minutes' $E_FULL_MODEL/*
rm -f $E_OUT/full/.attempted
python -u $CODE/e_full.py --auto
