#!/bin/bash
# Piece D: after training, K fresh draws from the tuned network, residual
# calibration (piece B's calibrate_matrix), report.json and curves, marker.
# Usage (through d_gpu_sh.sbatch):
#   d_finish.sh <output folder> <marker file> [d_tune.py common arguments...]
# Environment: DK = draws per record (default 32); BREP = piece B's report
# on the reference draws, for comparison (optional).
set -euo pipefail
OUT=$1; MARK=$2; shift 2
python -u d_tune.py draws --out-dir "$OUT" --k "${DK:-32}" "$@"
python -u d_tune.py resid --out-dir "$OUT" "$@"
python -u d_report.py --out-dir "$OUT" --b-report "${BREP:-none}"
date > "$MARK"
