#!/bin/bash
# Probe C: submit the self-finishing chains after the training jobs.
# Usage: bash c3_submit.sh <c3 train job seed1> <seed2> <seed3> <uwA train job seed1> <seed2>
set -euo pipefail
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
TR=("$1" "$2" "$3"); UW=("$4" "$5")
ALL=()
JOBS=$Q/jobs.txt
for i in 0 1 2; do
  s=$((i + 1)); T=${TR[$i]}
  g1=$(sbatch --parsable --dependency=afterok:$T c3_gen.sbatch $s 1)
  g2=$(sbatch --parsable --dependency=afterok:$T c3_gen.sbatch $s 2)
  c=$(sbatch --parsable --dependency=afterok:$T c3_comp.sbatch $s)
  v=$(sbatch --parsable --dependency=afterok:$g1 c3_vscore.sbatch $Q/gen/h_random_s1_seed0$s.parquet "C random seed $s" $Q/v/C_random_seed0$s.json)
  echo "combined seed $s: train $T gen $g1 $g2 completion $c scorecard $v" | tee -a $JOBS
  ALL+=($g1 $g2 $c $v)
done
for i in 0 1; do
  s=$((i + 1)); T=${UW[$i]}
  CKU=$Q/model_uwA/checkpoint_uwA_seed0$s.pt
  g1=$(CK=$CKU GDATA=$Q/data/full_v3cw PFX=uw_ sbatch --parsable --dependency=afterok:$T c3_gen.sbatch $s 1)
  g2=$(CK=$CKU GDATA=$Q/data/full_v3cw PFX=uw_ sbatch --parsable --dependency=afterok:$T c3_gen.sbatch $s 2)
  c=$(DATA=$Q/data/full_v3cw CK=$CKU OUT=$Q/model_uwA sbatch --parsable --dependency=afterok:$T c3_comp.sbatch $s)
  v=$(sbatch --parsable --dependency=afterok:$g1 c3_vscore.sbatch $Q/gen/h_uw_random_s1_seed0$s.parquet "C with weight variable random seed $s" $Q/v/CUW_random_seed0$s.json)
  echo "combined + weight variable seed $s: train $T gen $g1 $g2 completion $c scorecard $v" | tee -a $JOBS
  ALL+=($g1 $g2 $c $v)
done
DEP=$(IFS=:; echo "${ALL[*]}")
k=$(sbatch --parsable --dependency=afterany:$DEP c3_collect.sbatch)
echo "collector $k (afterany: $DEP) writes $Q/C_TABLES.md" | tee -a $JOBS
