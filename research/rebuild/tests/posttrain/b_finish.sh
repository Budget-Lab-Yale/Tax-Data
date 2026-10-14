#!/bin/bash
# Piece B: write Markdown summaries (aggregate tables only) of every method-1
# report that exists. Run through b_cpu.sbatch-like environment:
#   sbatch --dependency=afterany:<jobs> b_finish.sbatch
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
r=$(ls $P/b/report_ref_K8.json $P/b/report_ref_K16.json $P/b/report_ref.json $P/b/report_ref_K64.json $P/b/report_ref_K128.json 2>/dev/null)
[ -n "$r" ] && python -u b_summarize.py $r > $P/b/summary_scf.md && echo "wrote $P/b/summary_scf.md"
for V in full full_v2; do
  for S in P_core P_full; do
    r=$(ls $P/$V/b/$S/report_P_K8.json $P/$V/b/$S/report_P_K16.json $P/$V/b/$S/report_P.json 2>/dev/null)
    [ -n "$r" ] && python -u b_summarize.py $r > $P/$V/b/$S/summary.md && echo "wrote $P/$V/b/$S/summary.md"
  done
done
