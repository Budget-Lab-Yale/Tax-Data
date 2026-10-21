#!/bin/bash
# Step 34 (34_STATUS.md): submit the chain after the table job. Run from the repository root:
#   bash research/rebuild/minimal/34_submit.sh <table job id, or "none" if table E34 exists>
# Runs: Ns1-3, the base network (4 layers, width 128, as steps 31 and 33),
# seeds 20261016-18. Each: train + stack (GPU), then the tilt on both wealth
# count bases. Last: the report (afterany). Every step skips itself when its
# output exists, so resubmitting resumes.
set -euo pipefail
S=research/rebuild/minimal
L=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step34/jobs.txt
TJ=${1:?table job id or none}
DEP=""; [ "$TJ" != none ] && DEP="--dependency=afterok:$TJ"
seed() { echo $((20261015 + $1)); }
ALL=""
echo "--- chain submitted $(date)" >> $L
for spec in Ns1:1 Ns2:2 Ns3:3; do
  r=${spec%%:*}; k=${spec##*:}
  j=$(sbatch --parsable -J $r $DEP $S/34_run.sbatch $r $(seed $k))
  t=$(sbatch --parsable -J t$r --dependency=afterok:$j $S/34_tilt.sbatch $r scf_units)
  f=$(sbatch --parsable -J t${r}f --dependency=afterok:$j $S/34_tilt.sbatch $r file_units)
  echo "$r seed $(seed $k) run $j tilt scf_units $t file_units $f" >> $L
  ALL="$ALL:$t:$f"
done
rep=$(sbatch --parsable -J report34 --dependency=afterany$ALL $S/34_cpu.sbatch report)
echo "report $rep (afterany every run and tilt)" >> $L
tail -6 $L
