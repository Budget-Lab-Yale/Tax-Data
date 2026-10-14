#!/bin/bash
# Piece H: queue the whole chain at once (run from a login shell; it only
# calls sbatch). Job ids are appended to $P/logs/h_jobs.txt.
#   wait (CPU) -> train seed 1 -> completions seed 1
#                              -> train seeds 2, 3 -> their completions
#   -> report (CPU, after everything, whatever the outcome)
# Usage: bash h_launch.sh [first dependency job id, if the wait job exists]
# Env passed through to every job (sbatch exports the environment): DATA,
# OUT, TAGBASE (see h_train.sbatch). Example, version 2 of the data:
#   DATA=$T2/data/full_v2 OUT=$T2/model/full_v2 TAGBASE=full_v2_w1 bash h_launch.sh
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
J=$P/logs/h_jobs.txt
W=${1:-$(sbatch --parsable h_wait.sbatch)}
T1=$(sbatch --parsable --dependency=afterok:$W -J h_train1 h_train.sbatch 1)
C1=$(sbatch --parsable --dependency=afterok:$T1 -J h_comp1 h_complete.sbatch 1)
T2=$(sbatch --parsable --dependency=afterok:$T1 -J h_train2 h_train.sbatch 2)
T3=$(sbatch --parsable --dependency=afterok:$T1 -J h_train3 h_train.sbatch 3)
C2=$(sbatch --parsable --dependency=afterok:$T2 -J h_comp2 h_complete.sbatch 2)
C3=$(sbatch --parsable --dependency=afterok:$T3 -J h_comp3 h_complete.sbatch 3)
R=$(sbatch --parsable --dependency=afterany:$C1:$C2:$C3 -J h_report h_cpu.sbatch h_report.py \
      --model-dir "${OUT:-/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full}" --tag "${TAGBASE:-full_w1}")
echo "$(date +%F' '%H:%M) ${DATA:-full} -> ${OUT:-model/full}: wait $W train1 $T1 comp1 $C1 train2 $T2 train3 $T3 comp2 $C2 comp3 $C3 report $R" | tee -a "$J"
