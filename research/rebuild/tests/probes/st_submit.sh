#!/bin/bash
# Probe ST: the self-finishing chain after training (submitted 11 October).
# Training jobs (seeds 1-3) and the base fits of the model trained with 2016
# were submitted separately; their job ids are passed in.
# Usage: bash st_submit.sh <train1> <train2> <train3> <fullfit1> <fullfit2> <fullfit3> <calib1> <calib2>
set -euo pipefail
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
TR=($1 $2 $3); FB="$4:$5:$6"; CB="$7:$8"
ST=/nfs/roberts/scratch/pi_nrs36/jar335/probes/st
FULL=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2
sb() { sbatch --parsable "$@"; }
HB=""; H2=""; H4=""
for s in 1 2 3; do
  ck=$ST/model/checkpoint_no2016_seed0$s.pt
  j=$(sb --job-name=st_fit --dependency=afterok:${TR[$((s-1))]} st_gpu.sbatch st_fit.py --name held_s$s --ckpt $ck); HB="$HB:$j"
  j=$(sb --job-name=st_fit --dependency=afterok:${TR[$((s-1))]} st_gpu.sbatch st_fit.py --name held_s${s}_top2 --variant top2 --ckpt $ck); H2="$H2:$j"
done
j=$(sb --job-name=st_fit --dependency=afterok:${TR[0]} st_gpu.sbatch st_fit.py --name held_s1_top4 --variant top4 --ckpt $ST/model/checkpoint_no2016_seed01.pt); H4="$H4:$j"
j=$(sb --job-name=st_fit st_gpu.sbatch st_fit.py --name full_s1_top2 --variant top2 --ckpt $FULL/checkpoint_full_v2_w1_seed01.pt); H2="$H2:$j"
j=$(sb --job-name=st_fit st_gpu.sbatch st_fit.py --name full_s1_top4 --variant top4 --ckpt $FULL/checkpoint_full_v2_w1_seed01.pt); H4="$H4:$j"
C2=""; C4=""
for rho in 1e-4 1e-6; do
  j=$(sb st_cpu.sbatch st_calib.py --variant top2 --rho $rho); C2="$C2:$j"
  j=$(sb st_cpu.sbatch st_calib.py --variant top4 --rho $rho); C4="$C4:$j"
done
E1=$(sb --job-name=st_eval --time=03:00:00 --dependency=afterany:$FB$HB:$CB st_cpu.sbatch st_eval.py base full_s1 full_s2 full_s3 held_s1 held_s2 held_s3)
E2=$(sb --job-name=st_eval --time=03:00:00 --dependency=afterany${H2}${C2} st_cpu.sbatch st_eval.py top2 held_s1_top2 held_s2_top2 held_s3_top2 full_s1_top2)
E4=$(sb --job-name=st_eval --time=03:00:00 --dependency=afterany${H4}${C4} st_cpu.sbatch st_eval.py top4 held_s1_top4 full_s1_top4)
T=$(sb --job-name=st_table --dependency=afterany:$E1:$E2:$E4 st_cpu.sbatch st_table.py)
echo "held base fits${HB}; top2 fits${H2}; top4 fits${H4}; calib top2${C2} top4${C4}; eval base $E1 top2 $E2 top4 $E4; table $T" | tee $ST/chain_jobs.txt
