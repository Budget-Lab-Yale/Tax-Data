#!/bin/bash
# Piece H: the completions the pretraining evaluation needs, one training seed.
# Usage: sbatch h_complete.sbatch <seed> [which ...]
#   which: any of gen_puf puf_wealth scf_train scf_test (default: all four)
#   gen_puf    (a) whole PUF records from nothing: as many as the PUF training
#              records, PUF tag, year 2015, the 39 PUF variables in test 1's
#              order; one draw.
#   puf_wealth (b) PUF training records, their 24 wealth variables drawn with
#              the SCF tag at year 2015 (decoded at 2015); age range left
#              unobserved where the PUF lacks it; five draws.
#   scf_train  (c) SCF training records: wealth set aside and redrawn from
#   scf_test       their observed variables, own tag and year; five draws.
# Outputs in $OUT (default $T2/model/full): h_<which>_seedNN.parquet,
# _tokens.npz, _info.json; marker DONE_comp_seedNN when all requested ran.
#SBATCH --job-name=h_comp
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/h_comp_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/logs/h_comp_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
SEED=$1; shift
NN=$(printf %02d "$SEED")
OUT=${OUT:-$T2/model/full}
if [ -f "$OUT/data/DONE" ]; then DATA=$OUT/data; else DATA=${DATA:-$T2/data/full}; fi
echo "data $DATA"
TAGBASE=${TAGBASE:-full_w1}
CK=$OUT/checkpoint_${TAGBASE}_seed$NN.pt
EXTRA=${EXTRA:-}
WHICH=("$@"); [ ${#WHICH[@]} -eq 0 ] && WHICH=(gen_puf puf_wealth scf_train scf_test)
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
test -f "$CK"
run() { python -u h_complete.py --data-dir "$DATA" --checkpoint "$CK" --out-dir "$OUT" \
          --train-seed "$SEED" $EXTRA "$@"; }
for w in "${WHICH[@]}"; do
  echo "$(date) $w"
  case $w in
    gen_puf)    run --tag gen_puf --mode generate --fill puf --gen-source PUF --gen-year 2015 \
                    --draws 1 --seed $((20261020 + 100 * SEED)) ;;
    puf_wealth) run --tag puf_wealth --mode complete --fill wealth --split train \
                    --target-sources PUF --as-source SCF --decode-year 2015 \
                    --draws 5 --seed $((20261021 + 100 * SEED)) --no-score ;;
    scf_train)  run --tag scf_train --mode complete --fill wealth --split train \
                    --target-sources SCF --draws 5 --seed $((20261022 + 100 * SEED)) ;;
    scf_test)   run --tag scf_test --mode complete --fill wealth --split test \
                    --target-sources SCF --draws 5 --seed $((20261023 + 100 * SEED)) ;;
    *) echo "unknown: $w"; exit 1 ;;
  esac
done
if [ ${#@} -eq 0 ]; then
  echo "$(date) job ${SLURM_JOB_ID:-}" > "$OUT/DONE_comp_seed$NN"
fi
echo "$(date) completions done for seed $SEED: ${WHICH[*]}"
