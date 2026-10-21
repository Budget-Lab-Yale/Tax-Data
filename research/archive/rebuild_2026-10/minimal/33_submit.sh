#!/bin/bash
# Step 33 (33_STATUS.md): submit the chain after the table job. Run from the repository root:
#   bash research/rebuild/minimal/33_submit.sh <table job id, or "none" if table E33 exists>
# Runs: Ns1-3, the base network (4 layers, width 128, as step 31), seeds
# 20261016-18 (step 31's); Bs1, a bigger network (6 layers, width 256, 8
# heads, feed-forward 512), seed 20261016. Each: train + stack (GPU), then the
# tilt on both wealth count bases. Ns1 also gets a tilt with tax-exempt
# interest targets added (SCF-units base). Last: the report (afterany).
# Every step skips itself when its output exists, so resubmitting resumes.
set -euo pipefail
S=research/rebuild/minimal
L=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step33/jobs.txt
TJ=${1:?table job id or none}
DEP=""; [ "$TJ" != none ] && DEP="--dependency=afterok:$TJ"
seed() { echo $((20261015 + $1)); }
ALL=""
echo "--- chain submitted $(date)" >> $L
BIG="--export=ALL,MINIMAL_LAYERS=6,MINIMAL_WIDTH=256,MINIMAL_HEADS=8,MINIMAL_FFN=512"
for spec in Ns1:1 Ns2:2 Ns3:3 Bs1:1; do
  r=${spec%%:*}; k=${spec##*:}
  X=""; [ "${r:0:1}" = B ] && X=$BIG
  j=$(sbatch --parsable -J $r $DEP $X $S/33_run.sbatch $r $(seed $k))
  t=$(sbatch --parsable -J t$r --dependency=afterok:$j $S/33_tilt.sbatch $r scf_units)
  f=$(sbatch --parsable -J t${r}f --dependency=afterok:$j $S/33_tilt.sbatch $r file_units)
  echo "$r seed $(seed $k) run $j tilt scf_units $t file_units $f" >> $L
  ALL="$ALL:$t:$f"
  if [ $r = Ns1 ]; then
    e=$(sbatch --parsable -J t${r}tei --dependency=afterok:$j $S/33_tilt.sbatch $r scf_units tei)
    echo "$r tilt scf_units with tax-exempt interest targets $e" >> $L
    ALL="$ALL:$e"
  fi
done
rep=$(sbatch --parsable -J report33 --dependency=afterany$ALL $S/33_cpu.sbatch report)
echo "report $rep (afterany every run and tilt)" >> $L
tail -8 $L
