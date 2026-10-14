#!/bin/bash
# Probe S: repeat generation from one checkpoint with further sampling seeds,
# to separate sampling noise from training-seed differences. Draws k = FROM..TO
# of 664,624 records each; sampling seed 30261150 + 1000k + train seed (AGI
# first) or 30261140 + 1000k + train seed (demographics last).
# Usage: sbatch s_gen_multi.sbatch <checkpoint> <label> <train seed> <order> <from> <to>
#SBATCH --job-name=s_genm
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_genm_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_genm_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
DATA=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data
OUT=/nfs/roberts/scratch/pi_nrs36/jar335/probes/s/noise
CK=$1; LAB=$2; SEED=$3; KIND=$4; FROM=$5; TO=$6
mkdir -p "$OUT"
ORDER=$(python -c "
import json; m=json.load(open('$DATA/meta.json')); v={x['name']:x for x in m['variables']}
o=[n for n in m['generation_order'] if v[n].get('group')!='scf_only']
d=['AGERANGE','GENDER','EARNSPLIT']; o=[n for n in o if n not in d]+d
if '$KIND'=='agifirst': o=['E00100']+[n for n in o if n!='E00100']
print(','.join(o))")
[ "$KIND" = agifirst ] && B=30261150 || B=30261140
for k in $(seq $FROM $TO); do
  python -u $PT/h_complete.py --data-dir $DATA --checkpoint "$CK" --out-dir "$OUT" \
    --tag "${KIND}4x_${LAB}_g$k" --train-seed "$SEED" --mode generate --fill puf \
    --gen-source PUF --gen-year 2015 --draws 1 --n 664624 --seed $((B + 1000 * k + SEED)) \
    --order "$ORDER" --tokenizer $PT/g_tokenizer.py
done
echo "$(date) done"
