#!/bin/bash
# Probe UW: PUF training records completed given their true weight (PUFWT),
# true AGI and first six variables; every other PUF variable drawn (T's
# "given AGI" file).
# Usage: sbatch uw_given.sbatch <checkpoint> <out dir> <label> <train seed> [support npz]
# With a support table, uses uw_complete.py --support (draws restricted to tokens seen in the weight group).
#SBATCH --job-name=uw_given
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=01:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_given_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_given_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/full_v2w
CK=$1; OUT=$2; LAB=$3; SEED=$4
SCRIPT=$PT/h_complete.py; SUP=()
[ -n "${5:-}" ] && { SCRIPT=uw_complete.py; SUP=(--support "$5"); }
mkdir -p "$OUT"
REST=$(python -c "
import json; m=json.load(open('$DATA/meta.json')); v={x['name']:x for x in m['variables']}
o=[n for n in m['generation_order'] if v[n].get('group')!='scf_only']
d=['AGERANGE','GENDER','EARNSPLIT']; o=[n for n in o if n not in d]+d
print(','.join([x for x in o if x not in ['PUFWT','MARS','DSI','XTOT','N24','EIC','FDED','E00100']]))")
python -u $SCRIPT --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" --tag "givenagi_$LAB" \
  --train-seed "$SEED" --mode complete --split train --target-sources PUF --fill "$REST" --order "$REST" \
  --draws 1 --seed 20261130 --no-score --tokenizer $PT/g_tokenizer.py "${SUP[@]}"
echo "$(date) done"
