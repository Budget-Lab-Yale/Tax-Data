#!/bin/bash
# Step 29 (29_STATUS.md): submit the whole chain. Run from the repository root:
#   bash research/rebuild/minimal/29_submit.sh
# Real runs (table E27): rules 1 and 3 first, then rule 2; each run job
# (train + stack) is followed by its tilt (afterok). Fake-survey runs (table
# F29): three rules x two seeds, plus step 27's R2J method and today's rule R0
# on F29, one seed each; each run job ends with its fake check. Last: the
# report (afterany every tilt and fake run, so it runs even if one fails).
# Job ids are appended to $R/logs/step29/jobs.txt. Every step skips itself when
# its output exists, so resubmitting resumes.
set -euo pipefail
S=research/rebuild/minimal
L=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step29/jobs.txt
seed() { echo $((20261015 + $1)); }
ALL=""
echo "--- chain submitted $(date)" >> $L
for rule in C1 C3 C2; do
  for k in 1 2 3; do
    r=${rule}s$k
    j=$(sbatch --parsable -J $r $S/29_run.sbatch $r $(seed $k))
    t=$(sbatch --parsable -J t$r --dependency=afterok:$j $S/29_tilt.sbatch $r)
    echo "$r seed $(seed $k) run $j tilt $t" >> $L
    ALL="$ALL:$t"
  done
done
for r in F29C1s1 F29C1s2 F29C3s1 F29C3s2 F29C2s1 F29C2s2 F29R2Js1 F29R0s1; do
  k=${r: -1}
  j=$(sbatch --parsable -J $r --export=ALL,MINIMAL_TABLE=F29 $S/29_run.sbatch $r $(seed $k))
  echo "$r seed $(seed $k) run+fake check $j" >> $L
  ALL="$ALL:$j"
done
rep=$(sbatch --parsable -J report29 --dependency=afterany$ALL $S/29_cpu.sbatch report)
echo "report $rep (afterany every tilt and fake run)" >> $L
tail -25 $L
