#!/bin/bash
# Probe EV: wait for probe TR's files, score each as it appears (ev_score.py
# for wealth completions, v_score.py for generated files), and rerun the
# collector after each new score. Ends when probe TR has no job left in the
# queue and nothing new has appeared (not before 13:00 on 14 October), or at
# 08:00 on 15 October. Run by ev_cpush.sbatch with a long time limit.
set -uo pipefail
P=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
E=/nfs/roberts/scratch/pi_nrs36/jar335/probes/ev
TR=/nfs/roberts/scratch/pi_nrs36/jar335/probes/tr
cd $P
DEADLINE=$(date -d "2026-10-15 08:00" +%s)
MINEND=$(date -d "2026-10-14 13:00" +%s)
collect() {
  python -u v_collect.py $E/v $E/v_tables.md > $E/log_v_collect.txt 2>&1
  python -u ev_collect.py $E $P/EV_TABLES.md > $E/log_collect.txt 2>&1 && cp $P/EV_TABLES.md $E/EV_TABLES.md
  echo "$(date) collected" >> $E/wait.log
}
collect
while [ $(date +%s) -lt $DEADLINE ]; do
  new=0
  for f in $TR/comp/tr_*.parquet; do
    [ -f "$f" ] || continue
    b=$(basename $f .parquet)
    [ -f $TR/comp/${b}_info.json ] || continue
    case $b in
      tr_scf_test_nobr_*) part=scf; meth="trees ${b#tr_scf_test_nobr_}, bridge hidden";;
      tr_scf_test_*) part=scf; meth="trees ${b#tr_scf_test_}";;
      tr_puf_wealth_*) part=puf; meth="trees ${b#tr_puf_wealth_}";;
      *) continue;;
    esac
    out=$E/$part/$b.json
    [ -f $E/failed_$b ] && [ ! $f -nt $E/failed_$b ] && continue
    if [ ! -f $out ] || [ $f -nt $out ]; then
      echo "$(date) scoring $f" >> $E/wait.log
      python -u ev_score.py $part --label "$meth | file 1" --file $f --out $out > $E/log_$b.txt 2>&1 \
        && new=1 || { echo "$(date) FAILED $f (see $E/log_$b.txt)" >> $E/wait.log; rm -f $out; touch $E/failed_$b; }
    fi
  done
  for f in $TR/gen/tr_*_s*.parquet; do
    [ -f "$f" ] || continue
    b=$(basename $f .parquet)
    [ -f $TR/gen/${b}_info.json ] || continue
    out=$E/v/$b.json
    [ -f $out ] && [ ! $f -nt $out ] && continue
    [ -f $E/failed_$b ] && [ ! $f -nt $E/failed_$b ] && continue
    # totals computed from the parts (c3_totals.py) before scoring; work on a copy if the file lacks them
    g=$f
    if ! python -c "import pyarrow.parquet as pq,sys; sys.exit(0 if 'E00100' in pq.read_schema('$f').names else 1)"; then
      mkdir -p $E/gen; cp $f $E/gen/$b.parquet; g=$E/gen/$b.parquet
      python -u c3_totals.py $g >> $E/wait.log 2>&1
    fi
    tag=${b#tr_}; seed=${tag##*_s}; tag=${tag%_s*}
    echo "$(date) scorecard $g" >> $E/wait.log
    python -u v_score.py --gen $g --label "Trees $tag seed $seed" --out $out > $E/log_v_$b.txt 2>&1 \
      && new=1 || { echo "$(date) FAILED scorecard $g" >> $E/wait.log; rm -f $out; touch $E/failed_$b; }
  done
  [ $new -eq 1 ] && collect
  ntr=$(squeue -u $USER -h -o %j | grep -c '^tr_')
  if [ $(date +%s) -gt $MINEND ] && [ $ntr -eq 0 ] && [ $new -eq 0 ]; then
    echo "$(date) probe TR has no job left and nothing new; final collection" >> $E/wait.log
    collect
    break
  fi
  sleep 300
done
echo "$(date) waiter ends" >> $E/wait.log
