#!/bin/bash
# Evaluate expB_multi with piece C's scripts (C's watchers cover only the
# five original datasets). Every 10 minutes: one pass of C's c_watch.sh
# restricted to expB_multi (yardsticks, every completed file, compare.md),
# then C's PUF evaluation (c_evaluate_puf.py) for completed files with
# "puf" in the name, each once per modification time.
# Run with:  sbatch --time=12:00:00 -J b_eval_multi c_shell.sbatch b_eval_multi.sh 60
set -uo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
DS=expB_multi
ROUNDS=${1:-60}
ATT=$S/eval/.attempted_b_multi_puf
mkdir -p $S/eval; touch $ATT
for ((round=1; round<=ROUNDS; round++)); do
  echo "=== b_eval_multi round $round $(date '+%H:%M')"
  if [ -f $S/data/$DS/DONE ]; then
    bash $CODE/c_watch.sh 1 $DS
    for f in $S/model/$DS/completed_*puf*.parquet; do
      [ -f "$f" ] || continue
      fm=$(stat -c %Y "$f")
      [ $(( $(date +%s) - fm )) -lt 120 ] && continue
      key="$f:$fm"
      grep -qxF "$key" $ATT && continue
      echo "$key" >> $ATT
      stem=$(basename $f .parquet); stem=${stem#completed_}
      echo "--- PUF evaluation $DS model_$stem"
      python -u $CODE/c_evaluate_puf.py --dataset $DS --completed $f --label model_$stem
    done
  fi
  [ $round -lt $ROUNDS ] && sleep 600
done
echo "b_eval_multi finished $(date '+%H:%M')"
