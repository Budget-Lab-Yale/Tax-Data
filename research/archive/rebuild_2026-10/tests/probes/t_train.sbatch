#!/bin/bash
# Probe T: train one variant of the joint model on `full_v2`, then generate
# whole PUF records (demographics drawn last) from the kept checkpoint and
# from the PUF-best checkpoint.
# Usage: sbatch t_train.sbatch <variant> <seed> [extra t_train.py args ...]
# Settings otherwise as ../posttrain/h_train.sbatch (test 3, piece H).
#SBATCH --job-name=t_train
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=03:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_train_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
DATA=$T2/model/full_v2/data
VAR=$1; SEED=$2
NN=$(printf %02d "$SEED")
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/t/$VAR
MAXH=${MAXH:-1.6}
mkdir -p "$OUT"
nvidia-smi --query-gpu=name,memory.total --format=csv,noheader
echo "$(date) variant $VAR seed $SEED extra: ${*:3}"
TAG=${VAR}_seed$NN
python -u t_train.py --data-dir "$DATA" --out-dir "$OUT" --tag "$TAG" \
    --seed "$SEED" --drop-weight-vars --loss-weight survey --max-hours "$MAXH" "${@:3}"
ORDER=$(python -c "
import json; m=json.load(open('$DATA/meta.json')); v={x['name']:x for x in m['variables']}
o=[n for n in m['generation_order'] if v[n].get('group')!='scf_only']
d=['AGERANGE','GENDER','EARNSPLIT']; print(','.join([n for n in o if n not in d]+d))")
for CK in checkpoint_${TAG}.pt checkpoint_${TAG}_pufbest.pt; do
  GT=gen_${CK#checkpoint_}; GT=${GT%.pt}
  echo "$(date) generating from $CK"
  python -u $PT/h_complete.py --data-dir $DATA --checkpoint "$OUT/$CK" \
    --out-dir "$OUT" --tag "$GT" --train-seed "$SEED" --mode generate --fill puf \
    --gen-source PUF --gen-year 2015 --draws 1 --seed $((20261120 + 100 * SEED)) \
    --order "$ORDER" --tokenizer $PT/g_tokenizer.py
done
echo "$(date) done" > "$OUT/DONE_$TAG"
echo "$(date) variant $VAR seed $SEED done"
