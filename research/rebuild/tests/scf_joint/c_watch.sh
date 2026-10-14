#!/bin/bash
# Part C: evaluate everything that is ready, then (with a loop count > 1)
# keep checking every 10 minutes for new datasets and completed files.
# Run inside c_shell.sbatch:  sbatch --time=12:00:00 c_shell.sbatch c_watch.sh 70
#   (70 rounds of 10 minutes; "c_watch.sh 1" = one pass and stop).
# For each dataset folder in $S/data with meta.json and truth_test.npy:
#   - noise yardsticks (ref_sampling, ref_shuffled) if missing or older than the data;
#   - every $S/model/<ds>/completed_*.parquet and $S/comp/<ds>/completed_*.parquet
#     not yet evaluated (or changed since), label <model|comp>_<file stem>;
#   - for expB, PUF rows of the same files with c_evaluate_puf.py;
#   - then c_compare.py --update-report (EVAL_REPORT.md generated blocks).
# A file is attempted once per modification time (list in $S/eval/.attempted).
set -uo pipefail
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
S=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
ROUNDS=${1:-1}
ONLY=${2:-}          # optional: one dataset name
ATT=$S/eval/.attempted
mkdir -p $S/eval; touch $ATT
mtime() { stat -c %Y "$1" 2>/dev/null || echo 0; }
for ((round=1; round<=ROUNDS; round++)); do
  echo "=== round $round $(date '+%H:%M')"
  changed=0; [ $round = 1 ] && changed=1
  for dd in $S/data/*/; do
    ds=$(basename $dd)
    [ -n "$ONLY" ] && [ "$ds" != "$ONLY" ] && continue
    # only datasets piece A has marked final (DONE)
    [ -f $dd/DONE ] && [ -f $dd/meta.json ] && [ -f $dd/truth_test.npy ] && [ -f $dd/records_test.parquet ] || continue
    dm=$(mtime $dd/truth_test.npy)
    # wait until the data folder has been quiet for 3 minutes
    [ $(( $(date +%s) - dm )) -lt 180 ] && continue
    ys=$S/eval/$ds/ref_sampling/summary.json
    if [ ! -f $ys ] || [ $(mtime $ys) -lt $dm ]; then
      key="yardstick:$ds:$dm"
      if ! grep -qxF "$key" $ATT; then
        echo "$key" >> $ATT
        echo "--- yardsticks $ds"
        python -u $CODE/c_evaluate.py --dataset $ds --yardstick && changed=1
      fi
    fi
    for f in $S/model/$ds/completed_*.parquet $S/comp/$ds/completed_*.parquet; do
      [ -f "$f" ] || continue
      fm=$(mtime $f)
      [ $(( $(date +%s) - fm )) -lt 120 ] && continue
      origin=$(basename $(dirname $(dirname $f)))
      stem=$(basename $f .parquet); stem=${stem#completed_}
      label=${origin}_${stem}
      newest=$(( fm > dm ? fm : dm ))
      key="$f:$newest"
      grep -qxF "$key" $ATT && continue
      echo "$key" >> $ATT
      echo "--- $ds $label"
      python -u $CODE/c_evaluate.py --dataset $ds --completed $f --label $label && changed=1
      if [ "$ds" = "expB" ] && [[ "$stem" == *puf* ]]; then
        python -u $CODE/c_evaluate_puf.py --dataset $ds --completed $f --label $label && changed=1
      fi
    done
  done
  if [ $changed = 1 ]; then
    # several watchers may run (one per dataset); serialise report edits
    flock $S/eval/.report.lock python -u $CODE/c_compare.py --update-report ${ONLY:+--datasets $ONLY}
  fi
  [ $round -lt $ROUNDS ] && sleep 600
done
echo "c_watch finished $(date '+%H:%M')"
