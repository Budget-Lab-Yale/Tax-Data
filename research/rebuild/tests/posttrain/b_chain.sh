#!/bin/bash
# Piece B chain for the full model's arm P: wait for piece A's reference
# draws and targets for one dataset version, then submit method 1 on the
# GPU, P_core (target set wcore) first, then P_full (wfull), then the K
# sensitivity (K = 8 and 16 subsets of the same draws, both sets).
# Usage (inside b_wait.sbatch): ./b_chain.sh <version>     (full or full_v2)
set -euo pipefail
V=$1
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
A=$P/$V/a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
for f in "$A/DONE_targets" "$A/DONE_ref_P"; do
  until [ -e "$f" ]; do sleep 120; done
  echo "$(date '+%H:%M:%S') found $f"
done
mkdir -p $P/$V/b
PARTS="-p gpu_b200,gpu_h100,gpu_h200,scavenge_gpu --requeue"
j1=$(sbatch --parsable $PARTS --time=03:00:00 --job-name=b_${V}_core b_gpu.sbatch b_full.py --version $V --set wcore)
j2=$(sbatch --parsable $PARTS --time=03:00:00 --job-name=b_${V}_full --dependency=afterany:$j1 b_gpu.sbatch b_full.py --version $V --set wfull)
j3=$(sbatch --parsable $PARTS --time=04:00:00 --job-name=b_${V}_ksens --dependency=afterany:$j2 b_gpu.sbatch b_full.py --version $V --set wcore --k 8 16 --marker DONE_P_ksens)
j4=$(sbatch --parsable $PARTS --time=04:00:00 --job-name=b_${V}_ksens --dependency=afterany:$j3 b_gpu.sbatch b_full.py --version $V --set wfull --k 8 16 --marker DONE_P_ksens)
echo "$(date '+%H:%M:%S') submitted $j1 (P_core) $j2 (P_full) $j3 $j4 (K sensitivity)" | tee -a $P/$V/b/chain_jobs.txt
