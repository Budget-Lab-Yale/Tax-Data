#!/bin/bash
# Probe DD: everything after training for one model, in one GPU job (as
# probe C's c3_chain.sbatch):
#   generation of 664,624 whole PUF records, a fresh random order per record
#   (dd_complete.py), totals from parts and non-itemizers' claimed deductions
#   zero (c3_totals.py); the paid variables are never drawn.
#     dd  (paid observed for PUF itemizers in training): order mode
#         random_paid (paid set equal to the claimed amount as soon as a record
#         draws "itemized"), sampling seed 1; for model seed 1 also plain random
#         (paid left hidden throughout), to measure what the hiding does
#     ddp (paid never observed on PUF rows): plain random, sampling seed 1
#   wealth completion of PUF training records presented as SCF records, five
#   draws, two inputs: full_v4dd (form of deduction and claimed amounts
#   observed, paid = claimed for itemizers, unknown for non-itemizers) and
#   full_v4dd_hide (the same with form of deduction and claimed amounts
#   hidden); SCF test records with the paid variables observed and hidden.
# Usage: sbatch dd_chain.sbatch <arm: dd | ddp> <model seed>
#SBATCH --job-name=dd_chain
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_chain_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/dd_chain_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/dd
ARM=$1; MS=$2; NN=$(printf %02d "$MS"); N=664624
DATA=$Q/data/full_v4$ARM; MD=$Q/model_$ARM; CK=$MD/checkpoint_full_v4${ARM}_seed$NN.pt
case $ARM in
  dd)  RUNS="random_paid:1"; [ "$MS" = 1 ] && RUNS="random_paid:1 random:1" ;;
  ddp) RUNS="random:1" ;;
  *) echo "unknown arm $ARM"; exit 1 ;;
esac
test -f "$CK"
mkdir -p $Q/gen
cd "$HERE"
PAID=MORTGAGE_INTEREST_PAID,REAL_ESTATE_TAX_PAID
ORDER=$(python -c "import json;m=json.load(open('$DATA/meta.json'));v={x['name']:x for x in m['variables']};p='$PAID'.split(',');print(','.join(n for n in m['generation_order'] if v[n].get('group')!='scf_only' and n not in p))")
for r in $RUNS; do
  MODE=${r%%:*}; SS=${r##*:}
  TAG=${ARM}_${MODE}_s${SS}
  F=$Q/gen/h_${TAG}_seed$NN.parquet
  if [ -f "$F" ] && [ -f "$Q/gen/totals_${TAG}_seed$NN.txt" ]; then
    echo "exists: $F"
  else
    echo "$(date) generate $TAG"
    python -u dd_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir $Q/gen --tag "$TAG" \
      --train-seed "$MS" --mode generate --fill "$ORDER" --gen-source PUF --gen-year 2015 --draws 1 \
      --n "$N" --seed $((20261014 + 1000 * MS + SS)) --order "$ORDER" --order-mode "$MODE" \
      --tokenizer $PT/g_tokenizer.py --batch-size 16384 > $Q/gen/log_${TAG}_seed$NN.txt
    python -u c3_totals.py "$F" | tee $Q/gen/totals_${TAG}_seed$NN.txt
  fi
done
cd "$PT"
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
run() { python -u h_complete.py --checkpoint "$CK" --out-dir "$MD" --train-seed "$MS" "$@"; }
echo "$(date) puf_wealth"
run --data-dir $Q/data/full_v4dd --tag puf_wealth --mode complete --fill "$WORD" --split train \
    --target-sources PUF --as-source SCF --decode-year 2015 --draws 5 --seed $((20261021 + 100 * MS)) \
    --no-score > $MD/log_puf_wealth_seed$NN.txt
echo "$(date) puf_wealth_hide"
run --data-dir $Q/data/full_v4dd_hide --tag puf_wealth_hide --mode complete --fill "$WORD" --split train \
    --target-sources PUF --as-source SCF --decode-year 2015 --draws 5 --seed $((20261021 + 100 * MS)) \
    --no-score > $MD/log_puf_wealth_hide_seed$NN.txt
echo "$(date) scf_test"
run --data-dir "$DATA" --tag scf_test --mode complete --fill "$WORD" --split test --target-sources SCF \
    --draws 5 --seed $((20261023 + 100 * MS)) > $MD/log_scf_test_seed$NN.txt
echo "$(date) scf_test_nobr"
run --data-dir "$DATA" --tag scf_test_nobr --mode complete --fill "$WORD,$PAID" --order "$WORD,$PAID" \
    --split test --target-sources SCF --draws 5 --seed $((20261023 + 100 * MS)) > $MD/log_scf_test_nobr_seed$NN.txt
echo "$(date) job ${SLURM_JOB_ID:-}" > "$MD/DONE_chain_seed$NN"
echo "$(date) done"
