#!/bin/bash
# Probe UW: generate whole PUF records (664,624 = four times the PUF training
# count, unless N is given) from a checkpoint trained on `full_v2w`. Each
# record draws the PUF weight variable PUFWT.
# Usage: sbatch uw_gen.sbatch <checkpoint> <out dir> <tag> <order> <train seed> <sampling seed> [N] [support npz]
#   order: wfirst  PUFWT, then AGI, then S's AGI-first order (demographics last)
#          random  a fresh random order per record (o_complete.py --order-mode random)
#SBATCH --job-name=uw_gen
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=01:15:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_gen_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_gen_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/full_v2w
CK=$1; OUT=$2; TAG=$3; KIND=$4; SEED=$5; GSEED=$6; N=${7:-664624}
SUP=(); [ -n "${8:-}" ] && SUP=(--support "$8")
mkdir -p "$OUT"
ORDER=$(python -c "
import json; m=json.load(open('$DATA/meta.json')); v={x['name']:x for x in m['variables']}
o=[n for n in m['generation_order'] if v[n].get('group')!='scf_only']
d=['AGERANGE','GENDER','EARNSPLIT']; o=[n for n in o if n not in d]+d
o=['PUFWT','E00100']+[n for n in o if n not in ('PUFWT','E00100')]
print(','.join(o))")
MODE=fixed; [ "$KIND" = random ] && MODE=random
echo "order $ORDER; mode $MODE; n $N; sampling seed $GSEED"
python -u uw_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" --tag "$TAG" \
  --train-seed "$SEED" --mode generate --fill puf --gen-source PUF --gen-year 2015 --draws 1 \
  --n "$N" --seed "$GSEED" --order "$ORDER" --order-mode "$MODE" --tokenizer $PT/g_tokenizer.py "${SUP[@]}"
echo "$(date) done"
