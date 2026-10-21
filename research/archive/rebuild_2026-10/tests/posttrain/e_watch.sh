#!/bin/bash
# Piece E watcher: evaluate every input that is ready and new, then rebuild
# the comparison and the generated block of EVAL_REPORT.md; repeat every
# 10 minutes until the stop time.
#   sbatch --mem=100G --time=10:00:00 --job-name=e_watch e_sh.sbatch e_watch.sh 06:00
# Argument: stop time as HH:MM (default 06:00), or "once".
# Order each round:
#   1. e_full.py --auto for data version 2 (main) and version 1 (fallback):
#      piece H's files $T2/model/<version>/h_{gen_puf,puf_wealth,scf_train,scf_test}_seedNN.parquet
#      (each once its _info.json exists and it has been quiet 2 minutes), and
#      post-training files under $P/<version>/{a,b,c,d}/ (arm P: A's
#      ref_draws_P.npz with DONE_ref_P; every weights*.npz under b; every
#      draws.npz under c and d, with weights_resid.npz beside it if present).
#   2. The pilot on the current model: e_puf.py --auto and e_evaluate.py --auto.
#   3. e_compare.py --update-report.
# A file is evaluated once per modification time (lists in $P/e/**/.attempted;
# delete a stats.json to force a re-run).
set -uo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
STOP=${1:-06:00}
if [ "$STOP" = once ]; then end=0; else
  end=$(date -d "$STOP" +%s); [ $end -lt $(date +%s) ] && end=$(date -d "tomorrow $STOP" +%s); fi
while true; do
  echo "=== round $(date '+%H:%M')"
  E_FULL_VERSION=full_v2 python -u $CODE/e_full.py --auto
  E_FULL_VERSION=full python -u $CODE/e_full.py --auto
  python -u $CODE/e_puf.py --auto
  python -u $CODE/e_evaluate.py --auto
  python -u $CODE/e_compare.py --update-report
  [ $(date +%s) -ge $end ] && break
  sleep 600
done
echo "e_watch finished $(date '+%H:%M')"
