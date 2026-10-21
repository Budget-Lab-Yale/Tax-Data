#!/bin/bash
# Probe T: generate whole PUF records from a checkpoint in a chosen order.
# Usage: sbatch t_gen.sbatch <checkpoint> <out dir> <tag> <order> <seed> [extra h_complete args]
#   order: demolast (test 1's order, demographics after all PUF amounts) or
#          agifirst (AGI first, then the rest of demolast)
#SBATCH --job-name=t_gen
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_gen_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_gen_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data
CK=$1; OUT=$2; TAG=$3; KIND=$4; SEED=$5
mkdir -p "$OUT"
ORDER=$(python -c "
import json; m=json.load(open('$DATA/meta.json')); v={x['name']:x for x in m['variables']}
o=[n for n in m['generation_order'] if v[n].get('group')!='scf_only']
d=['AGERANGE','GENDER','EARNSPLIT']; o=[n for n in o if n not in d]+d
if '$KIND'=='agifirst': o=['E00100']+[n for n in o if n!='E00100']
print(','.join(o))")
echo "order $ORDER"
python -u $PT/h_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" --tag "$TAG" \
  --train-seed "$SEED" --mode generate --fill puf --gen-source PUF --gen-year 2015 --draws 1 \
  --seed $((20261140 + 100 * SEED)) --order "$ORDER" --tokenizer $PT/g_tokenizer.py "${@:6}"
echo "$(date) done"
