#!/bin/bash
# Part C development check of c_evaluate_puf.py on expB (fabricated PUF completion).
set -euo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
D=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/dev/c
python -u $CODE/c_make_dev_puf.py
python -u $CODE/c_evaluate_puf.py --dataset expB --completed $D/completed_pufshuffle_seed01.parquet --out-root $D/eval
