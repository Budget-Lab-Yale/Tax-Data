#!/bin/bash -l
#SBATCH --job-name=s32stack
#SBATCH --gres=gpu:1
#SBATCH --exclude=a1125u14n01,a1125u05n01
#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=64G
#SBATCH --output=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step32/stack_%x_%j.out
#SBATCH --error=/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/step32/stack_%x_%j.err
# Step 32 (32_STATUS.md): step 31's stack again from step 31's trained model
# (no retraining), with net worth drawn under the year token of another SCF
# wave. Everything else is step 31's stack command (31_run.sbatch): same
# model, same seeds, same settings; the PUF variables are drawn under
# "PUF, 2015" first, then net worth under the SCF label with the given year.
#   sbatch -J st32k1y18 -p gpu --gres=gpu:a5000:1 research/rebuild/minimal/32_stack.sbatch 1 2018
# Partition and GPU type are given on the command line: 32_submit.sh asks for
# the GPU type step 31's stack ran on (Ks1, Ks2: RTX A5000; Ks3: B200), since
# the same seed gives the same draws only on the same GPU type.
# Year 2018 = the 2019 wave (the run); year 2015 = the 2016 wave, through the
# new path (a check: must equal step 31's stack when the GPU type matches).
# Output: gen/step32/gen_puf_stack_s32_ks<k>_nw<year>.parquet and its meta; the
# plain comparison files under gen/step32/Ks<k>/. Skips itself if done.
set -euo pipefail
R=/nfs/roberts/scratch/pi_nrs36/jar335/minimal
K=${1:?seed number 1-3}; YEAR=${2:?net worth year token}
SEED=$((20261015 + K))
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data
D=research/rebuild/minimal
export PYTHONDONTWRITEBYTECODE=1
module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
nvidia-smi --query-gpu=name --format=csv,noheader
V=s32_ks${K}_nw${YEAR}
mkdir -p $R/gen/step32/Ks$K
export MINIMAL_TABLE=E27 MINIMAL_RUN=step31/Ks$K MINIMAL_SEED=$SEED MINIMAL_ALL_RECORDS=1 MINIMAL_MAP_DIR=$R/map/V27
export STACK_VERSION=$V MINIMAL_STACK_LABEL=scf MINIMAL_ORDER=own_first MINIMAL_STACK_DIR=$R/gen/step32 \
       MINIMAL_STACK_FOREIGN=absent MINIMAL_COMPUTED_TOTAL=1 MINIMAL_STACK_CG_STRATIFIER=kg_lt \
       MINIMAL_STACK_NW_YEAR=$YEAR MINIMAL_STACK_PLAIN=$R/gen/step32/Ks$K/plain_nw$YEAR.parquet
echo "=== stack $V $(date)"
[ -f $R/gen/step32/gen_meta_$V.json ] || python $D/03_stack.py
echo "=== done $(date)"
