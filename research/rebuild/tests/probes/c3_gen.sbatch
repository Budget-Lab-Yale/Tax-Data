#!/bin/bash
# Probe C: generate whole PUF records from a full_v3c checkpoint, a fresh
# random order per record (as_complete.py, an unchanged copy of o_complete.py),
# then compute every total from its parts (c3_totals.py).
# Usage: sbatch c3_gen.sbatch <model seed> <sampling seed> [n]
# Env: CK (checkpoint), PFX (file prefix), GDATA (dataset), GOUT (output folder)
#SBATCH --job-name=c3_gen
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_gen_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/c3_gen_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
DATA=${GDATA:-/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/full_v3c}
MS=$1; SS=$2; N=${3:-664624}
NN=$(printf %02d "$MS")
CK=${CK:-/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/model/checkpoint_full_v3c_seed$NN.pt}
PFX=${PFX:-}
OUT=${GOUT:-/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/gen}
mkdir -p "$OUT"
test -f "$CK"
ORDER=$(python -c "import json;m=json.load(open('$DATA/meta.json'));v={x['name']:x for x in m['variables']};print(','.join(n for n in m['generation_order'] if v[n].get('group')!='scf_only'))")
echo "model seed $MS sampling seed $SS n $N; variables: $ORDER"
python -u c3_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" --tag "${PFX}random_s${SS}" \
  --train-seed "$MS" --mode generate --fill puf --gen-source PUF --gen-year 2015 --draws 1 \
  --n "$N" --seed $((20261011 + 1000 * MS + SS)) --order "$ORDER" --order-mode ${OMODE:-random_fded} \
  --tokenizer $PT/g_tokenizer.py --batch-size ${BS:-16384}
F=$OUT/h_${PFX}random_s${SS}_seed$NN.parquet
python -u c3_totals.py "$F"
echo "$(date) done"
