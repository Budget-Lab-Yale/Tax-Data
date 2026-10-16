#!/bin/bash
#SBATCH --job-name=min21s
#SBATCH --partition=gpu_b200,gpu,gpu_h100,gpu_h200,scavenge_gpu
#SBATCH --gres=gpu:1
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/21_stack_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/21_stack_%j.err
# Step 21 (21_PRIORS.md): the v4c stack under one year token.
#   sbatch research/rebuild/minimal/21_stack.sbatch V5A Y1 2018
# v4c's rule (PUF variables first, net worth last under the SCF label with
# other income left out, four oversampled files of 100,000, plain files of
# 200,000 with seeds 3 and 35), with every record carrying year token <year>.
# Writes gen/<run>/gen_puf_stack_<run lower>_y<year>.parquet and its meta.
# On success touches markers/STACK21_<run>_y<year>_DONE.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
RUN=${1:?run}; TAB=${2:?table}; YEAR=${3:?year}
# second round (plan decision 3, 2026-10-16): with a 4th argument "nw", YEAR is
# net worth's year only and the PUF variables are drawn at 2015.
NW=${4:-}
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
export MINIMAL_RUN=$RUN MINIMAL_TABLE=$TAB MINIMAL_ALL_RECORDS=1
if [ "$NW" = nw ]; then
  export STACK_VERSION=$(echo $RUN | tr A-Z a-z)_nw$YEAR MINIMAL_STACK_YEAR=2015 MINIMAL_STACK_NW_YEAR=$YEAR
else
  export STACK_VERSION=$(echo $RUN | tr A-Z a-z)_y$YEAR MINIMAL_STACK_YEAR=$YEAR
fi
export MINIMAL_STACK_LABEL=scf MINIMAL_STACK_FOREIGN=absent MINIMAL_ORDER=own_first
export MINIMAL_STACK_DIR=$R/gen/$RUN
python research/rebuild/minimal/03_stack.py
touch $R/markers/STACK21_${STACK_VERSION}_DONE
echo "done $(date)"
