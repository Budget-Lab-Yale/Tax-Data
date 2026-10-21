#!/bin/bash
set -euo pipefail
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
ROOT=$S/followup_20261009
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
for SEED in 01 02 03; do
    for FILE in "$S/model/expB/completed_joint_w1_scf_plain_seed${SEED}.parquet" \
                "$ROOT/model/expB/completed_joint_w1_nosource_random_scf_plain_seed${SEED}.parquet" \
                "$ROOT/model/expB/completed_joint_w1_nosource_orderless_scf_plain_seed${SEED}.parquet" \
                "$ROOT/model/expB/completed_joint_w1_nosource_ordered_scf_plain_seed${SEED}.parquet"; do
        python -u "$CODE/c_tilt_diagnostic.py" --dataset expB --completed "$FILE" --out-root "$ROOT/eval"
    done
done
