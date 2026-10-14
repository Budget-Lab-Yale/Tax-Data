#!/bin/bash
# Probe WT (locate step): complete PUF training records' wealth with probe C's
# unknown-coded model (full_v3c), three ways, five draws each, as C's puf_wealth:
#   nobr     the bridge variables (E19200 interest paid, E18500 real estate tax)
#            hidden and redrawn after the wealth variables
#   shared   only variables the SCF also observes visible (wt_loc_shared.py)
#   sharednobr  both
# Usage: sbatch wt_loc_comp.sbatch <model seed>
#SBATCH --job-name=wt_loccomp
#SBATCH --partition=gpu_b200,gpu_h100,gpu_h200,gpu,scavenge_gpu
#SBATCH --requeue
#SBATCH --gres=gpu:1
#SBATCH --cpus-per-task=4
#SBATCH --mem=96G
#SBATCH --time=01:30:00
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_loccomp_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/wt_loccomp_%j.err
set -euo pipefail
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
            Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
export PYTHONDONTWRITEBYTECODE=1
HERE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/probes
PT=$HERE/../posttrain
C3=/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3
L=/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt/locate
MS=$1; NN=$(printf %02d "$MS")
CK=$C3/model/checkpoint_full_v3c_seed$NN.pt
DATA=$C3/data/full_v3c
SH=$L/data/full_v3c_sharedonly
mkdir -p $L/comp
cd "$HERE"
test -f $SH/tokens_train.npy || python -u wt_loc_shared.py
WORD=$(python -c "import json;m=json.load(open('$DATA/meta.json'));g=m['generation_order'];w=[v['name'] for v in m['variables'] if v['group']=='scf_only'];print(','.join([x for x in g if x in w]))")
cd "$PT"
run() { python -u h_complete.py --checkpoint "$CK" --out-dir $L/comp --train-seed "$MS" --mode complete --split train \
        --target-sources PUF --as-source SCF --decode-year 2015 --draws 5 --seed $((20261021 + 100 * MS)) --no-score "$@"; }
echo "$(date) nobr"
run --data-dir $DATA --tag puf_nobr --fill "$WORD,E19200,E18500" --order "$WORD,E19200,E18500" > $L/comp/log_puf_nobr_seed$NN.txt
echo "$(date) shared"
run --data-dir $SH --tag puf_shared --fill wealth > $L/comp/log_puf_shared_seed$NN.txt
echo "$(date) sharednobr"
run --data-dir $SH --tag puf_sharednobr --fill "$WORD,E19200,E18500" --order "$WORD,E19200,E18500" > $L/comp/log_puf_sharednobr_seed$NN.txt
echo "$(date) done"
