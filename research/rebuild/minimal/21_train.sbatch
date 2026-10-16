#!/bin/bash
#SBATCH --job-name=min21t
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/21_train_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/21_train_%j.err
# Step 21 (21_PRIORS.md): train the v4c model on several SCF waves.
#   sbatch research/rebuild/minimal/21_train.sbatch V5A Y1    PUF 2015 + SCF 2016, 2019, 2022 waves
#   sbatch research/rebuild/minimal/21_train.sbatch V5B Y1H   the same without the 2019 wave
# Table Y1H is built here from Y1 by dropping the SCF rows of tax year 2018
# (the 2019 survey) and their band-edge entries; nothing else changes.
# Same settings as v4 (MINIMAL_ALL_RECORDS=1: all records but a 2% held-back
# slice, 80 passes). On success touches markers/TRAIN21_<run>_DONE.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run tag}; TAB=${2:?table tag}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
if [ "$TAB" = Y1H ]; then
python - <<'PY'
import json, pandas as pd
D = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
t = pd.read_parquet(f'{D}/stacked_Y1.parquet')
drop = (t['source'] == 'scf') & (t['year'] == 2018)
print('dropping', int(drop.sum()), 'SCF rows of tax year 2018; keeping', int((~drop).sum()))
t[~drop].reset_index(drop=True).to_parquet(f'{D}/stacked_Y1H.parquet', index=False)
e = json.load(open(f'{D}/band_edges_Y1.json'))
e = {k: v for k, v in e.items() if not k.startswith('scf/2018/')}
json.dump(e, open(f'{D}/band_edges_Y1H.json', 'w'))
PY
fi
export MINIMAL_TABLE=$TAB MINIMAL_RUN=$RUN MINIMAL_ALL_RECORDS=1
python research/rebuild/minimal/02_model.py
touch $R/markers/TRAIN21_${RUN}_DONE
echo "done $(date)"
