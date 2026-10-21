#!/bin/bash
# Step 31 (31_STATUS.md): submit the whole chain. Run from the repository root:
#   bash research/rebuild/minimal/31_submit.sh
# Three seeds of the cell method (K): train + stack, then the tilt on both
# wealth count bases (afterok). The comparison runs (step 27 R0 and R2J, step
# 29 C4) already have stacks and SCF-units tilts; they get file-units tilts
# here. Last: the report (afterany everything). Job ids go to
# $R/logs/step31/jobs.txt. Every step skips itself when its output exists, so
# resubmitting resumes.
set -euo pipefail
S=research/rebuild/minimal
L=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step31/jobs.txt
seed() { echo $((20261015 + $1)); }
ALL=""
echo "--- chain submitted $(date)" >> $L
for k in 1 2 3; do
  r=Ks$k
  j=$(sbatch --parsable -J $r $S/31_run.sbatch $r $(seed $k))
  t=$(sbatch --parsable -J t$r --dependency=afterok:$j $S/31_tilt.sbatch step31 $r scf_units)
  f=$(sbatch --parsable -J t${r}f --dependency=afterok:$j $S/31_tilt.sbatch step31 $r file_units)
  echo "$r seed $(seed $k) run $j tilt scf_units $t file_units $f" >> $L
  ALL="$ALL:$t:$f"
done
for spec in step29:C4 step27:R2J step27:R0; do
  st=${spec%%:*}; code=${spec##*:}
  for k in 1 2 3; do
    f=$(sbatch --parsable -J t${code}s${k}f $S/31_tilt.sbatch $st ${code}s$k file_units)
    echo "$st ${code}s$k file_units tilt $f" >> $L
    ALL="$ALL:$f"
  done
done
rep=$(sbatch --parsable -J report31 --dependency=afterany$ALL $S/31_cpu.sbatch)
echo "report $rep (afterany every run and tilt)" >> $L
tail -16 $L
