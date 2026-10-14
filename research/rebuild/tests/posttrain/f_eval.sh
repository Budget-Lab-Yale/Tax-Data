#!/bin/bash
# Piece F: untargeted measures for the SCF from-scratch arms, then tables.
set -euo pipefail
cd "$(dirname "$0")"
python -u f_eval.py stats
python -u f_eval.py splitoff || true
python -u f_eval.py tables
