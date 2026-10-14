#!/bin/bash
# Piece F: queue arms PS0 and PS1 on the full model (SPEC.md, F 20:50).
#   ./f_chain.sh            # submit the whole chain; prints job ids
# Draws wait on G's data and H's checkpoint; calibrations wait on the draws
# and on A's full-model targets marker ($P/full/a/DONE_targets*).
set -euo pipefail
cd "$(dirname "$0")"
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
w1=$(sbatch --parsable f_wait.sbatch "$T2/data/full/DONE" "$T2/model/full/DONE_seed01")
d=()
for part in 1 2; do
  d+=($(sbatch --parsable --time=08:00:00 --dependency=afterok:$w1 f_gpu.sbatch f_full.py draw ps0 --part $part))
  d+=($(sbatch --parsable --time=08:00:00 --dependency=afterok:$w1 f_gpu.sbatch f_full.py draw ps1 --part $part))
done
w2=$(sbatch --parsable f_wait.sbatch "$P/full/a/DONE_targets*")
c0a=$(sbatch --parsable --time=10:00:00 -p gpu_b200,gpu_h100,gpu_h200 --dependency=afterok:$w2:${d[0]} f_gpu.sbatch f_full.py calib ps0 --k 8 --set core full)
c1a=$(sbatch --parsable --time=10:00:00 -p gpu_b200,gpu_h100,gpu_h200 --dependency=afterok:$w2:${d[1]} f_gpu.sbatch f_full.py calib ps1 --mult 4 --set core full)
c0b=$(sbatch --parsable --time=12:00:00 -p gpu_b200,gpu_h100,gpu_h200 --dependency=afterok:$w2:${d[0]}:${d[2]} f_gpu.sbatch f_full.py calib ps0 --k 16 --set core full)
c1b=$(sbatch --parsable --time=12:00:00 -p gpu_b200,gpu_h100,gpu_h200 --dependency=afterok:$w2:${d[1]}:${d[3]} f_gpu.sbatch f_full.py calib ps1 --mult 8 --set core full)
echo "wait_model=$w1 draws(ps0 p1, ps1 p1, ps0 p2, ps1 p2)=${d[*]} wait_targets=$w2 calib ps0 K8=$c0a ps1 4x=$c1a ps0 K16=$c0b ps1 8x=$c1b"
e1=$(sbatch --parsable --dependency=afterany:$c0a:$c1a:$c0b:$c1b f_cpu.sbatch f_full.py eval --k 16 --mult 8); echo "eval=$e1"
# Added 20:55: PS1 with the top oversampled (strata = income groups); see f_full.draw_strat.
ds=$(sbatch --parsable --time=08:00:00 -p gpu_b200,gpu_h100,gpu_h200 --dependency=afterok:$w1 f_gpu.sbatch f_full.py draw_strat --mult 16)
cs=$(sbatch --parsable --time=10:00:00 -p gpu_b200,gpu_h100,gpu_h200 --dependency=afterok:$w2:$ds f_gpu.sbatch f_full.py calib ps1s --set core full)
echo "ps1s draws=$ds calib=$cs (re-point the eval job's dependency at $cs too)"
