#!/bin/bash
# Part C development check on the fabricated dataset (run inside c_shell.sbatch).
set -uo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
D=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/dev/c
python -u $CODE/c_evaluate.py --dataset fake --data-root $D --out-root $D/eval --yardstick || exit 1
for f in $D/fake_completed/completed_*.parquet; do
  python -u $CODE/c_evaluate.py --dataset fake --data-root $D --out-root $D/eval --completed $f || echo "FAILED $f"
done
