#!/bin/bash
# Code test of the oversampled PS1 on the pilot model: draws, then method 1.
set -euo pipefail
cd "$(dirname "$0")"
python -u f_full.py pilot_strat --mult 16
python -u f_puf.py pilot_ps1s --set core full
