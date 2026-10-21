#!/bin/bash
# Probe C: everything after training for one model, in one GPU job (the
# per-user job limit is shared by all probes):
#   generation of 664,624 whole PUF records per file, a fresh random order per
#   record, sampling seeds 1 and 2 (c3_complete.py), totals from parts
#   (c3_totals.py); then wealth completion (c3_comp.sbatch's steps).
# Usage: sbatch c3_chain.sbatch <variant: c | cz | czw> <model seed>
#   c    full_v3c  (itemized deductions unknown for non-itemizers): one file
#        in plain random order and one with the deductions drawn after the
#        form of deduction (random_fded), sampling seed 1 each
#   cz   full_v3cz (deductions zero for non-itemizers): plain random, seeds 1, 2
#   czw  full_v3czw (cz plus the PUF weight as a model variable, probe UW's
#        weighting): plain random, seeds 1, 2; records weighted by drawn weight
#   czs  full_v3czs (cz with the SCF's Schedule E unobserved): completions only
#SBATCH --job-name=c3_chain
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_chain_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_chain_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3
V=$1; MS=$2; NN=$(printf %02d "$MS"); N=664624
case $V in
  c)   DATA=$Q/data/full_v3c;   MD=$Q/model;      CK=$MD/checkpoint_full_v3c_seed$NN.pt ;  RUNS="random:1 random_fded:1" ;;
  cz)  DATA=$Q/data/full_v3cz;  MD=$Q/model_cz;   CK=$MD/checkpoint_full_v3cz_seed$NN.pt ; RUNS="random:1 random:2" ;;
  czs) DATA=$Q/data/full_v3czs;  MD=$Q/model_czs;   CK=$MD/checkpoint_full_v3czs_seed$NN.pt ; RUNS="" ;;
  czw) DATA=$Q/data/full_v3czw; MD=$Q/model_czw;  CK=$MD/checkpoint_czw_seed$NN.pt ;       RUNS="random:1 random:2" ;;
  *) echo "unknown variant $V"; exit 1 ;;
esac
test -f "$CK"
mkdir -p $Q/gen
cd "$HERE"
ORDER=$(python -c "import json;m=json.load(open('$DATA/meta.json'));v={x['name']:x for x in m['variables']};print(','.join(n for n in m['generation_order'] if v[n].get('group')!='scf_only'))")
for r in $RUNS; do
  MODE=${r%%:*}; SS=${r##*:}
  TAG=${V}_${MODE}_s${SS}
  F=$Q/gen/h_${TAG}_seed$NN.parquet
  if [ -f "$F" ] && python -c "import pandas as pd,sys; sys.exit(0 if 'weight' in pd.read_parquet('$F', columns=['weight']).columns else 1)"; then
    echo "exists: $F"
  else
    echo "$(date) generate $TAG"
    python -u c3_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir $Q/gen --tag "$TAG" \
      --train-seed "$MS" --mode generate --fill puf --gen-source PUF --gen-year 2015 --draws 1 \
      --n "$N" --seed $((20261011 + 1000 * MS + SS)) --order "$ORDER" --order-mode "$MODE" \
      --tokenizer $PT/g_tokenizer.py --batch-size 16384 > $Q/gen/log_${TAG}_seed$NN.txt
    python -u c3_totals.py "$F"
  fi
done
cd "$PT"
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
run() { python -u h_complete.py --data-dir "$DATA" --checkpoint "$CK" --out-dir "$MD" --train-seed "$MS" "$@"; }
echo "$(date) puf_wealth"
run --tag puf_wealth --mode complete --fill wealth --split train --target-sources PUF --as-source SCF \
    --decode-year 2015 --draws 5 --seed $((20261021 + 100 * MS)) --no-score > $MD/log_puf_wealth_seed$NN.txt
echo "$(date) scf_test"
run --tag scf_test --mode complete --fill wealth --split test --target-sources SCF --draws 5 \
    --seed $((20261023 + 100 * MS)) > $MD/log_scf_test_seed$NN.txt
echo "$(date) scf_test_nobr"
run --tag scf_test_nobr --mode complete --fill "$WORD,E19200,E18500" --order "$WORD,E19200,E18500" --split test \
    --target-sources SCF --draws 5 --seed $((20261023 + 100 * MS)) > $MD/log_scf_test_nobr_seed$NN.txt
echo "$(date) job ${SLURM_JOB_ID:-}" > "$MD/DONE_chain_seed$NN"
echo "$(date) done"
