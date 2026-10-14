#!/bin/bash
# Piece F: queue PS0, PS1 and the oversampled PS1 on one full-model version.
#   ./f_chain_version.sh full_v2      (or full)
# Draws wait on G's data marker and H's checkpoint marker; calibrations wait
# on the draws and on A's targets marker ($P/<version>/a/DONE_targets*).
set -euo pipefail
cd "$(dirname "$0")"
V=${1:?version}
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
DATA=$T2/data/$V
CKPT=$T2/model/$V/checkpoint_${V}_w1_seed01.pt
OUT=$P/$V/f
AD=$P/$V/a
X="--data $DATA --ckpt $CKPT --out $OUT --adir $AD"
G="-p gpu_b200,gpu_h100,gpu_h200"
w1=$(sbatch --parsable f_wait.sbatch "$DATA/DONE" "$T2/model/$V/DONE_seed01")
d=()
for part in 1 2; do
  d+=($(sbatch --parsable --time=08:00:00 --dependency=afterok:$w1 f_gpu.sbatch f_full.py draw ps0 --part $part $X))
  d+=($(sbatch --parsable --time=08:00:00 --dependency=afterok:$w1 f_gpu.sbatch f_full.py draw ps1 --part $part $X))
done
ds=$(sbatch --parsable --time=08:00:00 $G --dependency=afterok:$w1 f_gpu.sbatch f_full.py draw_strat --mult 16 $X)
w2=$(sbatch --parsable f_wait.sbatch "$AD/DONE_targets*")
c0a=$(sbatch --parsable --time=10:00:00 $G --dependency=afterok:$w2:${d[0]} f_gpu.sbatch f_full.py calib ps0 --k 8 --set core full $X)
c1a=$(sbatch --parsable --time=10:00:00 $G --dependency=afterok:$w2:${d[1]} f_gpu.sbatch f_full.py calib ps1 --mult 4 --set core full $X)
cs=$(sbatch --parsable --time=10:00:00 $G --dependency=afterok:$w2:$ds f_gpu.sbatch f_full.py calib ps1s --set core full $X)
c0b=$(sbatch --parsable --time=12:00:00 $G --dependency=afterok:$w2:${d[0]}:${d[2]} f_gpu.sbatch f_full.py calib ps0 --k 16 --set core full $X)
c1b=$(sbatch --parsable --time=12:00:00 $G --dependency=afterok:$w2:${d[1]}:${d[3]} f_gpu.sbatch f_full.py calib ps1 --mult 8 --set core full $X)
e1=$(sbatch --parsable --dependency=afterany:$c0a:$c1a:$cs:$c0b:$c1b f_cpu.sbatch f_full.py eval --k 16 --mult 8 $X)
echo "$V: wait_model=$w1 draws(ps0 p1, ps1 p1, ps0 p2, ps1 p2)=${d[*]} ps1s_draws=$ds wait_targets=$w2"
echo "$V: calib ps0 K8=$c0a ps1 4x=$c1a ps1s=$cs ps0 K16=$c0b ps1 8x=$c1b eval=$e1"
