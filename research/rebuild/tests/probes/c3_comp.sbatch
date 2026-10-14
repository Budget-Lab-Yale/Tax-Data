#!/bin/bash
# Probe C: completions from one checkpoint (runs ../posttrain/h_complete.py
# unchanged, as probes BR and X did). Usage: sbatch c3_comp.sbatch <seed> [which ...]
#   puf_wealth     PUF training records, 24 wealth values drawn with the SCF tag
#                  at 2015 (decoded at 2015); five draws
#   scf_test       SCF test records, wealth hidden and redrawn from everything
#                  else observed (bridge variables included); scored
#   scf_test_nobr  the same with the bridge variables (E19200, E18500) hidden
#                  too and drawn after the wealth, so the wealth score is the
#                  score without them
# Env: DATA, CK, OUT (defaults: probe C's full_v3c model of that seed)
#SBATCH --job-name=c3_comp
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_comp_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_comp_%j.err
set -euo pipefail
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3
SEED=$1; shift; NN=$(printf %02d "$SEED")
DATA=${DATA:-$Q/data/full_v3c}
OUT=${OUT:-$Q/model}
CK=${CK:-$OUT/checkpoint_full_v3c_seed$NN.pt}
BRV=${BRV:-E19200,E18500}
WHICH=("$@"); [ ${#WHICH[@]} -eq 0 ] && WHICH=(puf_wealth scf_test scf_test_nobr)
mkdir -p "$OUT"
test -f "$CK"
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
run() { python -u h_complete.py --data-dir "$DATA" --checkpoint "$CK" --out-dir "$OUT" --train-seed "$SEED" "$@"; }
for w in "${WHICH[@]}"; do
  echo "$(date) $w"
  case $w in
    puf_wealth) run --tag puf_wealth --mode complete --fill wealth --split train --target-sources PUF --as-source SCF \
                    --decode-year 2015 --draws 5 --seed $((20261021 + 100 * SEED)) --no-score ;;
    scf_test)   run --tag scf_test --mode complete --fill wealth --split test --target-sources SCF --draws 5 \
                    --seed $((20261023 + 100 * SEED)) ;;
    scf_test_nobr) run --tag scf_test_nobr --mode complete --fill "$WORD,$BRV" --order "$WORD,$BRV" --split test \
                    --target-sources SCF --draws 5 --seed $((20261023 + 100 * SEED)) ;;
    *) echo "unknown $w"; exit 1 ;;
  esac
done
echo "$(date) job ${SLURM_JOB_ID:-} ${WHICH[*]}" > "$OUT/DONE_comp_seed$NN"
echo "$(date) done"
