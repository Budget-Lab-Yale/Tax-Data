#!/bin/bash
# Piece E development: e_full.py on the fabricated H files (no refabrication).
set -euo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
export E_OUT=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e/out
export E_FULL_MODEL=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/e/full_model
touch -d "-15 minutes" $E_FULL_MODEL/*
rm -f $E_OUT/full/.attempted
python -u -c "
import sys; sys.path.insert(0, '$CODE')
import e_full, glob, os, time
print(e_full.MODEL, glob.glob(os.path.join(e_full.MODEL, 'h_*_seed*.parquet')))
print([ (f, e_full.ready(f)) for f in glob.glob(os.path.join(e_full.MODEL, 'h_*_seed*.parquet'))])
print(e_full.plan())
"
python -u $CODE/e_full.py --auto
E_COMPARE_ONLY_FULL=1 python -u $CODE/e_compare.py
