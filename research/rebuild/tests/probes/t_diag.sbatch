#!/bin/bash
# Probe T, step 1 on test 3's version 2 models (seeds 1-3): t_diag.py; the
# components drawn given the true AGI and first six variables (seed 1);
# whole-record generation with demographics last for seeds 2 and 3; then the
# levels table (t_compare.py) for all of these and the tokenizer round trip.
#SBATCH --job-name=t_diag
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=02:00:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_diag_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_diag_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
cd "$HERE"
T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint
M=$T2/model/full_v2
DATA=$M/data
Q=/nfs/roberts/scratch/pi_nrs36/jar335/probes/t/base
mkdir -p $Q
DL=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/main/h_gen_puf_demoglast_seed01.parquet
python -u t_diag.py --out $Q/diag_step1.json --gen $DL \
  $M/checkpoint_full_v2_w1_seed01.pt $M/checkpoint_full_v2_w1_seed02.pt $M/checkpoint_full_v2_w1_seed03.pt
ORDER=$(python -c "
import json; m=json.load(open('$DATA/meta.json')); v={x['name']:x for x in m['variables']}
o=[n for n in m['generation_order'] if v[n].get('group')!='scf_only']
d=['AGERANGE','GENDER','EARNSPLIT']; print(','.join([n for n in o if n not in d]+d))")
REST=$(python -c "print(','.join([x for x in '$ORDER'.split(',') if x not in ['MARS','DSI','XTOT','N24','EIC','FDED','E00100']]))")
python -u $PT/h_complete.py --data-dir $DATA --checkpoint $M/checkpoint_full_v2_w1_seed01.pt \
  --out-dir $Q --tag given_agi --train-seed 1 --mode complete --split train --target-sources PUF \
  --fill "$REST" --order "$REST" --draws 1 --seed 20261130 --no-score --tokenizer $PT/g_tokenizer.py
for S in 2 3; do
  python -u $PT/h_complete.py --data-dir $DATA --checkpoint $M/checkpoint_full_v2_w1_seed0$S.pt \
    --out-dir $Q --tag gen_demoglast --train-seed $S --mode generate --fill puf --gen-source PUF \
    --gen-year 2015 --draws 1 --seed $((20261120 + 100 * S)) --order "$ORDER" --tokenizer $PT/g_tokenizer.py
done
python -u t_compare.py --out $Q/compare_base.json roundtrip=roundtrip \
  "agefirst_s1=$M/h_gen_puf_seed01.parquet" "demoglast_s1=$DL" \
  "demoglast_s2=$Q/h_gen_demoglast_seed02.parquet" "demoglast_s3=$Q/h_gen_demoglast_seed03.parquet" \
  "given_agi_s1=$Q/h_given_agi_seed01.parquet"
echo "$(date) done"
