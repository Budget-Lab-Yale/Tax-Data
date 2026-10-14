#!/bin/bash
# Piece D: method 3 on the full model (VER=full or full_v2), arm P (complete PUF records' wealth).
# Runs as a small CPU Slurm job (d_wait.sbatch) that waits for its inputs and
# submits the GPU jobs. Order:
#   1. when H's checkpoint marker and A's targets exist: variant 2 (direct
#      target loss with a KL penalty) on target set wcore (P_core), then its
#      fresh draws, residual calibration and report;
#   2. when B's method-1 weights for P_core and A's reference draws exist:
#      variant 1 (weighted distillation), then the same finish;
#   3. if B's P_full weights appear while waiting: variant 1 on wfull too.
# Outputs $P/$VER/d/<arm>/<variant>/, markers $P/$VER/d/DONE_<arm>_<variant>.
# Log of this waiter: $P/logs/d_wait_<job>.out.
set -uo pipefail
P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
cd $CODE
VER=${VER:-full}                 # full (v1) or full_v2
AD=$P/$VER/a
OUTD=$P/$VER/d
BD=$P/$VER/b
mkdir -p $OUTD
MARK=$T2/model/$VER/DONE_seed01
GPU="--partition=gpu_b200,gpu_h200,gpu_h100"   # not "gpu": its RTX A5000s are too slow in float32
C="--dataset $VER --arm puf --a-dir $AD"

first() { for f in "$@"; do [ -e "$f" ] && { echo "$f"; return 0; }; done; return 1; }
bweights() {   # $1 = core|full; B writes $BD/P_<set>/weights_P.npz then DONE_P
  [ -e $BD/P_$1/DONE_P ] && first $BD/P_$1/weights_P.npz && return 0
  first $BD/P_$1/weights_ref.npz $BD/P/$1/weights_ref.npz $BD/P/weights_$1.npz \
        $BD/P/weights_w$1.npz $BD/P/weights_ref_$1.npz $BD/P/weights_P_$1.npz \
        $BD/P_$1/weights.npz
}
submit_pair() {   # $1 arm dir, $2 variant, $3 target set, rest: train args
  local arm=$1 var=$2 ts=$3; shift 3
  local out=$OUTD/$arm/$var
  mkdir -p $out
  local j f
  j=$(sbatch --parsable $GPU --time=01:50:00 --job-name=d_${VER}_${arm}_$var d_gpu_sh.sbatch \
      d_train.sh $out --variant $var $C --target-set $ts "$@")
  f=$(sbatch --parsable $GPU --time=08:00:00 --mem=200G --dependency=afterok:$j \
      --job-name=d_fin_${VER}_${arm}_$var --export=ALL,DK=32,BREP=${BREP:-none} d_gpu_sh.sbatch \
      d_finish.sh $out $OUTD/DONE_${arm}_$var $C --target-set $ts)
  echo "$(date '+%F %T') submitted $arm/$var: train $j, finish $f" | tee -a $OUTD/chain_jobs.txt
}

echo "$(date '+%F %T') waiting for $MARK and $AD/DONE_targets"
until [ -e $MARK ] && [ -e $AD/DONE_targets ]; do sleep 120; done
[ "${SKIP_DIRECT:-0}" = 1 ] || submit_pair P_core direct wcore --beta 0.1 --lr 1e-4 --rl-batch 1024 --mix 0 \
  --minutes 80 --eval-min 10 --eval-k 1 --patience 4

done_core=0; done_full=0
echo "$(date '+%F %T') waiting for B's P_core weights and $AD/DONE_ref_P"
for i in $(seq 1 600); do
  if [ $done_core = 0 ] && [ -e $AD/DONE_ref_P ] && W=$(bweights core); then
    BREP=$(ls $(dirname $W)/report*.json 2>/dev/null | head -1)
    submit_pair P_core distill wcore --draws $AD/ref_draws_P.npz --weights $W \
      --lr 5e-5 --mix 0.2 --minutes 80 --eval-min 10 --eval-k 1 --patience 3
    done_core=1
  fi
  if [ $done_full = 0 ] && [ $done_core = 1 ] && [ -e $AD/DONE_ref_P ] && W=$(bweights full); then
    BREP=$(ls $(dirname $W)/report*.json 2>/dev/null | head -1)
    submit_pair P_full distill wfull --draws $AD/ref_draws_P.npz --weights $W \
      --lr 5e-5 --mix 0.2 --minutes 80 --eval-min 10 --eval-k 1 --patience 3
    done_full=1
  fi
  [ $done_core = 1 ] && [ $done_full = 1 ] && break
  sleep 120
done
echo "$(date '+%F %T') waiter finished (core $done_core, full $done_full)"
