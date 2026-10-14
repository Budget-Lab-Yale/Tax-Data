#!/bin/bash
# Piece E: evaluate the arm-P post-training files of one data version now
# (the watcher does the same every 10 minutes). Usage: sbatch e_sh.sbatch e_post.sh full|full_v2
set -uo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
E_FULL_VERSION=${1:-full} python -u $CODE/e_full.py --post
