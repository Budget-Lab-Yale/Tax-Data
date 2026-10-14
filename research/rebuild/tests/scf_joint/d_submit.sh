#!/bin/bash
# Queue piece D's two comparison methods for one dataset, seeds 1-3.
#   bash d_submit.sh <dataset> [--include-puf] [--after <jobid>]
# Submits (all on partition day): d_drf_prep.py, then d_drf.sbatch for each
# seed after it; d_seqtrees.py for each seed. Prints the job ids.
# Submitting jobs is fine from the login node; all computing runs in them.
set -e
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
ds=$1; shift
puf=""; dep=""
while [ $# -gt 0 ]; do
  case $1 in
    --include-puf) puf="--include-puf";;
    --after) dep="--dependency=afterok:$2"; shift;;
  esac
  shift
done
seeds=${SEEDS:-"1 2 3"}
prep=$(sbatch --parsable $dep -J d_prep_$ds -t 00:30:00 --cpus-per-task=2 --mem=32G d_py.sbatch d_drf_prep.py --dataset $ds $puf)
echo "prep $prep"
for s in $seeds; do
  j=$(sbatch --parsable --dependency=afterok:$prep -J d_drf_${ds}_$s d_drf.sbatch $ds $s 5)
  echo "drf seed $s: $j"
  k=$(sbatch --parsable $dep -J d_seq_${ds}_$s -t 06:00:00 --cpus-per-task=16 --mem=64G d_py.sbatch d_seqtrees.py --dataset $ds --seed $s $puf)
  echo "seqtrees seed $s: $k"
done
